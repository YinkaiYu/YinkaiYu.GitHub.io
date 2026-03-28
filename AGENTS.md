# Repository Guidelines

## Project Structure & Module Organization
This repository is a static personal website. The root [`index.html`](/mnt/c/Users/Newton/Documents/yykspace.com/YinkaiYu.GitHub.io/index.html) redirects to the English site in `en/`; Chinese pages live in `cn/`. Shared styles and scripts are in `css/` and `js/`, and images/favicon assets are in `img/`. Content-heavy sections are organized by topic: `publications/`, `life/`, `scholar/`, and `share/`. A standalone research codebase with its own `Makefile` lives in `scholar/DQMC/code_DQMC/`.

## Build, Test, and Development Commands
There is no site build step, but the repo now includes a minimal npm setup for Playwright-based browser checks.

- `npm run serve`
  Serve the site locally from the repository root on port 8000 for browser testing.
- `npm run playwright:codegen`
  Record browser interactions against the local site when reproducing navigation or UI issues.
- `npm run playwright:test`
  Run Playwright tests when the repo gains browser checks.
- `python3 -m http.server 8000`
  Fallback local server if Node-based scripts are unavailable.
- `xdg-open http://localhost:8000/` or open the URL manually
  Verify navigation, asset paths, and language switching.
- `make` in `scholar/DQMC/code_DQMC/`
  Build the DQMC Fortran program only if you are working in that subdirectory and have its MPI/Intel toolchain.

## Coding Style & Naming Conventions
Preserve the existing hand-written static style: HTML/CSS/JS files use tabs heavily, simple relative paths, and minimal tooling. Keep file names lowercase and descriptive, matching current patterns such as `en/share.html`, `cn/nav.js`, and `img/portrait.png`. When updating shared navigation or mirrored content, keep `cn/` and `en/` pages aligned unless the difference is intentional.

## Frontend Workflow
For HTML/CSS/JS changes that affect layout, navigation, interaction, or language switching, prefer browser validation with Playwright rather than relying only on static inspection.

- Start the local site with `npm run serve` when possible, or `python3 -m http.server 8000` as a fallback.
- Use Playwright for visual checks, interaction checks, and quick regression checks on affected pages.
- When shared UI changes, verify both `en/` and `cn/` pages, plus language switching.
- Check at least one desktop width and one mobile width for visible layout changes.
- For homepage, landing page, and visually led redesign tasks, use `frontend-skill` if it is available in the current Codex environment; otherwise follow the same principles: strong visual anchor, restrained composition, minimal card use, and meaningful motion.

## Testing Guidelines
There is no committed automated test suite in this repository yet. After changes, load the affected pages in a browser or Playwright and check:

- internal links and language toggles
- image/PDF downloads and relative asset paths
- layout regressions in `css/main.css` consumers
- desktop and mobile layouts for pages with visible UI changes
- both language versions when shared navigation or shared styles are touched

For DQMC code changes, use the subproject’s own compile flow and any domain-specific validation you maintain outside this website repo.

## Commit & Pull Request Guidelines
Recent history uses short, direct commit subjects, often in Chinese, for example `更新publications`, `修复语言切换按钮`, and `更正doi`. Follow that style: one concise imperative summary per commit. Pull requests should state the affected section, describe user-visible changes, link related issues if any, and include screenshots for page/layout updates.
