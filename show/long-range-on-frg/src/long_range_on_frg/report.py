"""Render the standalone public HTML report from audited numerical data."""

from __future__ import annotations

import hashlib
import json
import re
from pathlib import Path

from jinja2 import Environment, FileSystemLoader, StrictUndefined, select_autoescape


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _fmt(value: float | None, digits: int = 8) -> str:
    if value is None:
        return "—"
    return f"{value:.{digits}g}"


def _restore_inline_math_delimiters(html: str) -> str:
    """Turn the template's parenthesized notation into valid MathJax markup.

    The report template uses ordinary parentheses as authoring shorthand for
    inline mathematics.  A balanced-parenthesis pass is more reliable than a
    regex cascade because formulas themselves contain parentheses, for example
    G(q) and uncertainty notation such as 0.0361(11).
    """

    head, body = html.split("<body>", 1)
    body = body.replace("(O(N))", "O(N)")
    protected: list[str] = []

    def stash(match: re.Match[str]) -> str:
        protected.append(match.group(0))
        return f"@@MATH{len(protected) - 1}@@"

    body = re.sub(r"\\\[.*?\\\]", stash, body, flags=re.DOTALL)
    # Inline math cannot cross an HTML tag.  This guard prevents a malformed
    # authoring delimiter in one table cell from swallowing later cells.
    body = re.sub(r"\\\([^<]*?\\\)", stash, body)

    # Anything left over is an unmatched authoring delimiter. Convert it back
    # to an ordinary parenthesis so the balanced parser can repair the formula.
    body = body.replace(r"\(", "(").replace(r"\)", ")")

    placeholder = re.compile(r"@@MATH(\d+)@@")
    scalar_symbols = {"d", "N", "M", "q", "k", "j", "t", "r", "u", "p", "c", "x", "y"}
    function_names = {"O", "G", "U", "u", "R", "H", "A", "ln", "exp"}

    def inline_contents(value: str) -> str:
        """Inline protected math without nesting a second delimiter pair."""

        def expand(match: re.Match[str]) -> str:
            item = protected[int(match.group(1))]
            if item.startswith(r"\(") and item.endswith(r"\)"):
                return item[2:-2]
            return item

        return placeholder.sub(expand, value)

    def is_mathematical(content: str) -> bool:
        stripped = content.strip()
        if stripped in scalar_symbols:
            return True
        if re.search(r"\\[A-Za-z]+|[_^=]|&lt;|&gt;|[<>]|\|", content):
            return True
        if "@@MATH" in content:
            return True
        return bool(
            re.fullmatch(r"[\d\s.,+\-*/]+", stripped)
            and re.search(r"[+\-*/]", stripped)
        )

    def wrap_text(text: str) -> str:
        pairs: list[tuple[int, int]] = []
        depth = 0
        start = -1
        for index, character in enumerate(text):
            if character == "(":
                if depth == 0:
                    start = index
                depth += 1
            elif character == ")" and depth:
                depth -= 1
                if depth == 0:
                    pairs.append((start, index))

        output: list[str] = []
        cursor = 0
        for start, end in pairs:
            content = text[start + 1 : end]
            prefix_match = re.search(r"(\\[A-Za-z]+|[A-Za-z][A-Za-z0-9_]*)$", text[cursor:start])
            expression_start = start
            if prefix_match:
                prefix = prefix_match.group(0)
                if prefix.startswith("\\") or prefix in function_names:
                    expression_start = cursor + prefix_match.start()
            mathematical = is_mathematical(content) or expression_start < start
            if not mathematical:
                continue
            output.append(text[cursor:expression_start])
            if expression_start < start:
                expression = text[expression_start : end + 1]
            else:
                expression = content
            output.append(r"\(" + inline_contents(expression) + r"\)")
            cursor = end + 1
        output.append(text[cursor:])
        return "".join(output)

    parts = re.split(r"(<[^>]+>)", body)
    skip_depth = 0
    for index, part in enumerate(parts):
        if part.startswith("<"):
            if re.match(r"<(?:pre|code)\b", part, flags=re.IGNORECASE):
                skip_depth += 1
            elif re.match(r"</(?:pre|code)>", part, flags=re.IGNORECASE):
                skip_depth = max(0, skip_depth - 1)
        elif not skip_depth:
            parts[index] = wrap_text(part)
    body = "".join(parts)

    # Newly recovered expressions may contain placeholders for expressions
    # stashed earlier.  Reverse order resolves those nested dependencies.
    for index in range(len(protected) - 1, -1, -1):
        value = protected[index]
        body = body.replace(f"@@MATH{index}@@", value)
    return f"{head}<body>{body}"


def build_report(project: Path) -> Path:
    data_directory = project / "data"
    results_path = data_directory / "results.json"
    paper_audit_path = data_directory / "paper_audit.json"
    reference_audit_path = data_directory / "reference_audit.json"
    results = json.loads(results_path.read_text())
    paper_audit = json.loads(paper_audit_path.read_text())
    reference_audit = json.loads(reference_audit_path.read_text())

    references = reference_audit["references"]
    reference_numbers = {entry["id"]: index for index, entry in enumerate(references, 1)}
    summaries = results["short_range"]["summaries"]
    summary = {
        f"d{row['dimension']:g}_n{row['components']}": row for row in summaries
    }
    benchmarks = {row["method"]: row for row in results["external_benchmarks"]}
    branches = {
        f"d{row['dimension']:g}_n{row['components']}": row
        for row in results["long_range"]["branches"]
    }
    formula_checks = paper_audit["formula_checks"]
    critical_checks = [
        row for row in formula_checks if row["severity"] in ("critical", "high")
    ]
    accepted_convergence = [
        row for row in results["short_range"]["convergence"] if row["accepted"]
    ]

    environment = Environment(
        loader=FileSystemLoader(project / "report"),
        autoescape=select_autoescape(["html", "xml"]),
        undefined=StrictUndefined,
        trim_blocks=True,
        lstrip_blocks=True,
    )
    environment.filters["num"] = _fmt
    template = environment.get_template("template.html.j2")
    output = project / "report" / "index.html"
    html = template.render(
        results=results,
        paper_audit=paper_audit,
        reference_audit=reference_audit,
        references=references,
        refnum=reference_numbers,
        summary=summary,
        benchmarks=benchmarks,
        branches=branches,
        critical_checks=critical_checks,
        accepted_convergence=accepted_convergence,
        hashes={
            "results": _sha256(results_path),
            "paper_audit": _sha256(paper_audit_path),
            "reference_audit": _sha256(reference_audit_path),
        },
    )
    html = _restore_inline_math_delimiters(html)
    output.write_text(html)
    return output


__all__ = ["build_report"]
