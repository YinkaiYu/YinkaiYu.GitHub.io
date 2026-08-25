(() => {
  "use strict";

  const overviewRows = window.GRADE_DATA.overview;
  const detailRows = window.GRADE_DATA.details;
  const detailDisplayHeaders = ["课程", "Yu Index", "教师", "学年", "学期", "学分", "最终成绩", "绩点", "教学班排名", "类别"];
  const rows = detailRows.slice(1).map((row, index) => ({
    index,
    类别: row[0],
    课程: row[1],
    教师: row[2] ?? "",
    学年: row[3],
    学期: row[4],
    学分: Number(row[5]),
    最终成绩: row[6],
    绩点: Number(row[7]),
    教学班排名: row[8],
    "Yu Index": computeYuIndex(Number(row[7]), row[8]),
  }));

  const gradePalette = ["#244e61", "#386b81", "#5f8798", "#8da8b3", "#c5d2d7"];
  const categoryColors = {
    公必: "#2f6f8f",
    专必: "#c45d32",
    公选: "#a55f7a",
    专选: "#74813f",
  };
  const categorySymbols = { 公必: "circle", 专必: "square", 公选: "diamond", 专选: "triangle" };
  const scatterDefaultTitle = "课程绩点与排名分布图";
  const scatterExpandedTitle = "SYSU-Phys-Bench: 课程绩点与排名分布图";
  const yuScatterDefaultTitle = "Yu Index 与排名分布图";
  const yuScatterExpandedTitle = "SYSU-Phys-Bench: Yu Index 与排名分布图";
  const yearOrder = ["大一", "大二", "大三", "大四"];
  const termOrder = ["第一学期", "第二学期"];
  const semesterOrder = yearOrder.flatMap((year) => termOrder.map((term) => `${year}${term}`));

  let sortState = { key: "index", direction: "asc" };
  let pinnedTooltipTarget = null;
  const scatterEligibleRows = rows.filter((row) => Number.isFinite(row.绩点) && Number.isFinite(row["Yu Index"]));
  const yuIndexBenchmark = medianValue(scatterEligibleRows.map((row) => row["Yu Index"]));
  const scatterSelected = new Set(scatterEligibleRows.map((row) => row.index));
  const detailMultiFilters = {};
  const scatterMultiFilters = {};

  const $ = (selector) => document.querySelector(selector);
  const svgNS = "http://www.w3.org/2000/svg";

  function svgElement(name, attributes = {}, text = "") {
    const node = document.createElementNS(svgNS, name);
    Object.entries(attributes).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== "") node.textContent = text;
    return node;
  }

  function scatterSymbol(category, centerX, centerY, size, attributes = {}) {
    const common = { fill: categoryColors[category], ...attributes };
    if (categorySymbols[category] === "square") {
      const side = size * 1.7;
      return svgElement("rect", { x: centerX - side / 2, y: centerY - side / 2, width: side, height: side, rx: 1, ...common });
    }
    if (categorySymbols[category] === "diamond") {
      return svgElement("polygon", { points: `${centerX},${centerY - size} ${centerX + size},${centerY} ${centerX},${centerY + size} ${centerX - size},${centerY}`, ...common });
    }
    if (categorySymbols[category] === "triangle") {
      return svgElement("polygon", { points: `${centerX},${centerY - size} ${centerX + size * 0.92},${centerY + size * 0.72} ${centerX - size * 0.92},${centerY + size * 0.72}`, ...common });
    }
    return svgElement("circle", { cx: centerX, cy: centerY, r: size, ...common });
  }

  function formatValue(value, digits = 2) {
    return Number(value).toLocaleString("zh-CN", {
      minimumFractionDigits: 0,
      maximumFractionDigits: digits,
    });
  }

  function truncateText(value, maxLength = 28) {
    const characters = Array.from(String(value).trim());
    return characters.length > maxLength ? `${characters.slice(0, maxLength).join("")}…` : characters.join("");
  }

  function parseRank(value) {
    const match = String(value).match(/^(\d+)\/(\d+)$/);
    if (!match) return { rank: NaN, total: NaN, percentile: NaN };
    const rank = Number(match[1]);
    const total = Number(match[2]);
    return { rank, total, percentile: (rank / total) * 100 };
  }

  function inverseStandardNormal(probability) {
    if (!(probability > 0 && probability < 1)) return NaN;
    const a = [-39.69683028665376, 220.9460984245205, -275.9285104469687, 138.357751867269, -30.66479806614716, 2.506628277459239];
    const b = [-54.47609879822406, 161.5858368580409, -155.6989798598866, 66.80131188771972, -13.28068155288572];
    const c = [-0.007784894002430293, -0.3223964580411365, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783];
    const d = [0.007784695709041462, 0.3224671290700398, 2.445134137142996, 3.754408661907416];
    const lower = 0.02425;
    const upper = 1 - lower;
    let q;
    let r;

    if (probability < lower) {
      q = Math.sqrt(-2 * Math.log(probability));
      return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5])
        / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    }
    if (probability > upper) {
      q = Math.sqrt(-2 * Math.log(1 - probability));
      return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5])
        / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    }
    q = probability - 0.5;
    r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q
      / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
  }

  function computeYuIndex(gpa, rankValue) {
    const { rank, total } = parseRank(rankValue);
    if (!Number.isFinite(gpa) || !Number.isFinite(rank) || !Number.isFinite(total) || rank < 1 || rank > total) return NaN;
    const plottingPosition = (total - rank + 5 / 8) / (total + 1 / 4);
    return gpa - inverseStandardNormal(plottingPosition) / 3;
  }

  function medianValue(values) {
    const sorted = values.filter(Number.isFinite).sort((a, b) => a - b);
    if (!sorted.length) return NaN;
    const middle = Math.floor(sorted.length / 2);
    return sorted.length % 2 ? sorted[middle] : (sorted[middle - 1] + sorted[middle]) / 2;
  }

  function setupYuCalculator() {
    const form = $("#yu-calculator-form");
    const gpaInput = $("#yu-calculator-gpa");
    const rankInput = $("#yu-calculator-rank");
    const totalInput = $("#yu-calculator-total");
    const output = $("#yu-calculator-result");
    const inputs = [gpaInput, rankInput, totalInput];
    const resetOutput = () => {
      const label = document.createElement("span");
      label.textContent = "Yu Index";
      const placeholder = document.createElement("strong");
      placeholder.textContent = "—";
      output.replaceChildren(label, placeholder);
    };

    inputs.forEach((input) => input.addEventListener("input", resetOutput));

    form.addEventListener("submit", (event) => {
      event.preventDefault();
      const gpa = Number(gpaInput.value);
      const rank = Number(rankInput.value);
      const total = Number(totalInput.value);
      const valid = gpaInput.value !== "" && rankInput.value !== "" && totalInput.value !== ""
        && gpa >= 0 && gpa <= 5 && Number.isInteger(rank) && Number.isInteger(total)
        && total >= 2 && rank >= 1 && rank <= total;

      output.replaceChildren();
      if (!valid) {
        const message = document.createElement("span");
        message.className = "yu-calculator-error";
        message.textContent = "请输入有效的绩点、名次和人数";
        output.append(message);
        return;
      }

      const value = computeYuIndex(gpa, `${rank}/${total}`);
      const label = document.createElement("span");
      label.textContent = "Yu Index";
      const result = document.createElement("strong");
      result.textContent = value.toFixed(2);
      const interpretation = document.createElement("small");
      interpretation.textContent = value >= yuIndexBenchmark ? "给分较友好" : "给分较严格";
      output.append(label, result, interpretation);
    });
  }

  function weightedAverage(items, valueAccessor, weightAccessor = (item) => item.学分) {
    const valid = items.filter((item) => Number.isFinite(valueAccessor(item)) && weightAccessor(item) > 0);
    const weight = valid.reduce((sum, item) => sum + weightAccessor(item), 0);
    return weight ? valid.reduce((sum, item) => sum + valueAccessor(item) * weightAccessor(item), 0) / weight : 0;
  }

  function renderOverviewBenchmarks() {
    const benchmarks = $("#overview-benchmarks");
    benchmarks.replaceChildren();
    const totalRow = overviewRows.find((row) => row[0] === "合计");
    const specialRow = overviewRows.find((row) => typeof row[0] === "string" && row[0].includes("|"));
    [
      ["总绩点", totalRow?.[3]],
      ["总排名", specialRow?.[3]],
    ].forEach(([label, value]) => {
      const card = document.createElement("div");
      card.className = "overview-benchmark";
      const labelElement = document.createElement("span");
      labelElement.textContent = label;
      const valueElement = document.createElement("strong");
      valueElement.textContent = value ?? "";
      card.append(labelElement, valueElement);
      benchmarks.append(card);
    });
  }

  function setupMultiFilter(selector, items, allLabel, onChange) {
    const details = $(selector);
    const summary = details.querySelector("summary");
    const options = details.querySelector(".multi-filter-options");
    const normalizedItems = items.map((item) => typeof item === "string" ? { value: item, label: item } : item);
    const state = { details, summary, items: normalizedItems, selected: new Set(normalizedItems.map((item) => item.value)), allLabel };

    normalizedItems.forEach((item) => {
      const label = document.createElement("label");
      const input = document.createElement("input");
      input.type = "checkbox";
      input.value = item.value;
      input.checked = true;
      input.addEventListener("change", () => {
        if (input.checked) state.selected.add(item.value);
        else state.selected.delete(item.value);
        updateMultiFilterSummary(state);
        onChange();
      });
      const text = document.createElement("span");
      text.textContent = item.label;
      label.append(input, text);
      options.append(label);
    });
    updateMultiFilterSummary(state);
    return state;
  }

  function updateMultiFilterSummary(state) {
    const selectedItems = state.items.filter((item) => state.selected.has(item.value));
    if (selectedItems.length === state.items.length) state.summary.textContent = state.allLabel;
    else if (!selectedItems.length) state.summary.textContent = "未选择";
    else if (selectedItems.length === 1) state.summary.textContent = selectedItems[0].label;
    else state.summary.textContent = `已选 ${selectedItems.length} 项`;
  }

  function resetMultiFilter(state) {
    state.selected.clear();
    state.items.forEach((item) => state.selected.add(item.value));
    state.details.querySelectorAll('input[type="checkbox"]').forEach((input) => { input.checked = true; });
    updateMultiFilterSummary(state);
    state.details.open = false;
  }

  function setupFilters() {
    detailMultiFilters.category = setupMultiFilter("#category-filter", [...new Set(rows.map((row) => row.类别))], "全部类别", renderDetailsBody);
    detailMultiFilters.year = setupMultiFilter("#year-filter", yearOrder.filter((value) => rows.some((row) => row.学年 === value)), "全部学年", renderDetailsBody);
    detailMultiFilters.term = setupMultiFilter("#term-filter", termOrder.filter((value) => rows.some((row) => row.学期 === value)), "全部学期", renderDetailsBody);
    detailMultiFilters.credit = setupMultiFilter("#credit-filter", [
      { value: "0-1", label: "0.5–1 学分" },
      { value: "1.5-2", label: "1.5–2 学分" },
      { value: "3-plus", label: "3 学分以上" },
    ], "全部学分", renderDetailsBody);
    $("#search").addEventListener("input", renderDetailsBody);

    $("#reset-filters").addEventListener("click", () => {
      $("#search").value = "";
      Object.values(detailMultiFilters).forEach(resetMultiFilter);
      sortState = { key: "index", direction: "asc" };
      updateSortIndicators();
      renderDetailsBody();
    });
  }

  function scatterFilterRows() {
    const query = $("#scatter-search").value.trim().toLocaleLowerCase("zh-CN");
    return scatterEligibleRows.filter((row) => {
      const searchable = `${row.课程} ${row.教师}`.toLocaleLowerCase("zh-CN");
      return (
        (!query || searchable.includes(query)) &&
        scatterMultiFilters.category.selected.has(row.类别) &&
        scatterMultiFilters.year.selected.has(row.学年) &&
        scatterMultiFilters.term.selected.has(row.学期)
      );
    });
  }

  function renderScatterCourseOptions() {
    const visibleRows = scatterFilterRows();
    const options = $("#scatter-course-options");
    options.replaceChildren();
    visibleRows.forEach((row) => {
      const label = document.createElement("label");
      const input = document.createElement("input");
      input.type = "checkbox";
      input.value = row.index;
      input.checked = scatterSelected.has(row.index);
      input.addEventListener("change", () => {
        if (input.checked) scatterSelected.add(row.index);
        else scatterSelected.delete(row.index);
        refreshScatterSelection();
      });
      const text = document.createElement("span");
      text.textContent = row.课程;
      if (row.教师) {
        const teacher = document.createElement("small");
        teacher.textContent = truncateText(row.教师, 18);
        text.append(teacher);
      }
      label.append(input, text);
      options.append(label);
    });
    if (!visibleRows.length) {
      const empty = document.createElement("p");
      empty.className = "scatter-options-empty";
      empty.textContent = "无匹配课程";
      options.append(empty);
    }
    updateScatterSelectionCount();
  }

  function updateScatterSelectionCount() {
    const active = scatterFilterRows().filter((row) => scatterSelected.has(row.index)).length;
    $("#scatter-selection-count").textContent = `显示 ${active} / ${scatterFilterRows().length}`;
  }

  function refreshScatterSelection() {
    updateScatterSelectionCount();
    renderScatter();
    renderYuRankScatter();
    document.dispatchEvent(new CustomEvent("scatterfilterschange"));
  }

  function setupScatterFilters() {
    const onMultiFilterChange = () => {
      renderScatterCourseOptions();
      refreshScatterSelection();
    };
    scatterMultiFilters.category = setupMultiFilter("#scatter-category", [...new Set(scatterEligibleRows.map((row) => row.类别))], "全部类别", onMultiFilterChange);
    scatterMultiFilters.year = setupMultiFilter("#scatter-year", yearOrder.filter((value) => scatterEligibleRows.some((row) => row.学年 === value)), "全部学年", onMultiFilterChange);
    scatterMultiFilters.term = setupMultiFilter("#scatter-term", termOrder.filter((value) => scatterEligibleRows.some((row) => row.学期 === value)), "全部学期", onMultiFilterChange);
    $("#scatter-search").addEventListener("input", onMultiFilterChange);
    $("#scatter-select-visible").addEventListener("click", () => {
      scatterFilterRows().forEach((row) => scatterSelected.add(row.index));
      renderScatterCourseOptions();
      refreshScatterSelection();
    });
    $("#scatter-clear-visible").addEventListener("click", () => {
      scatterFilterRows().forEach((row) => scatterSelected.delete(row.index));
      renderScatterCourseOptions();
      refreshScatterSelection();
    });
    $("#scatter-reset").addEventListener("click", () => {
      $("#scatter-search").value = "";
      Object.values(scatterMultiFilters).forEach(resetMultiFilter);
      scatterSelected.clear();
      scatterEligibleRows.forEach((row) => scatterSelected.add(row.index));
      renderScatterCourseOptions();
      refreshScatterSelection();
    });
    renderScatterCourseOptions();
  }

  function creditBucket(credit) {
    if (credit >= 3) return "3-plus";
    if (credit >= 1.5) return "1.5-2";
    return "0-1";
  }

  function filteredRows() {
    const query = $("#search").value.trim().toLocaleLowerCase("zh-CN");
    const filtered = rows.filter((row) => {
      const searchable = `${row.课程} ${row.教师}`.toLocaleLowerCase("zh-CN");
      return (
        (!query || searchable.includes(query)) &&
        detailMultiFilters.category.selected.has(row.类别) &&
        detailMultiFilters.year.selected.has(row.学年) &&
        detailMultiFilters.term.selected.has(row.学期) &&
        detailMultiFilters.credit.selected.has(creditBucket(row.学分))
      );
    });

    return filtered.sort((a, b) => {
      const aValue = a[sortState.key];
      const bValue = b[sortState.key];
      let result;
      if (sortState.key === "学年") {
        result = yearOrder.indexOf(aValue) - yearOrder.indexOf(bValue);
      } else if (sortState.key === "学期") {
        result = termOrder.indexOf(aValue) - termOrder.indexOf(bValue);
      } else if (sortState.key === "教学班排名") {
        result = parseRank(aValue).percentile - parseRank(bValue).percentile;
      } else if (typeof aValue === "number" && typeof bValue === "number") {
        result = aValue - bValue;
      } else if (typeof aValue === "number") {
        result = -1;
      } else if (typeof bValue === "number") {
        result = 1;
      } else {
        result = String(aValue).localeCompare(String(bValue), "zh-CN");
      }
      if (result === 0) result = a.index - b.index;
      return sortState.direction === "asc" ? result : -result;
    });
  }

  function creditLevel(credit) {
    if (credit >= 3) return 3;
    if (credit >= 1.5) return 2;
    return 1;
  }

  function renderDetailsHead() {
    const tr = document.createElement("tr");
    detailDisplayHeaders.forEach((header) => {
      const th = document.createElement("th");
      th.scope = "col";
      if (header === "Yu Index") th.className = "yu-index-header";
      const button = document.createElement("button");
      button.className = "sort-button";
      button.type = "button";
      button.dataset.key = header;
      button.innerHTML = `<span>${header}</span><span class="sort-indicator" aria-hidden="true">↕</span>`;
      button.addEventListener("click", () => {
        if (sortState.key === header) {
          sortState.direction = sortState.direction === "asc" ? "desc" : "asc";
        } else {
          sortState = { key: header, direction: "asc" };
        }
        updateSortIndicators();
        renderDetailsBody();
      });
      th.append(button);
      tr.append(th);
    });
    $("#details-table thead").append(tr);
  }

  function updateSortIndicators() {
    document.querySelectorAll(".sort-button").forEach((button) => {
      const active = button.dataset.key === sortState.key;
      const indicator = button.querySelector(".sort-indicator");
      indicator.textContent = active ? (sortState.direction === "asc" ? "↑" : "↓") : "↕";
      button.setAttribute("aria-sort", active ? (sortState.direction === "asc" ? "ascending" : "descending") : "none");
    });
  }

  function renderDetailsBody() {
    const data = filteredRows();
    const tbody = $("#details-table tbody");
    tbody.replaceChildren();
    $("#result-count").textContent = `${data.length} / ${rows.length} 门课程`;

    if (!data.length) {
      const tr = document.createElement("tr");
      tr.className = "empty-row";
      const td = document.createElement("td");
      td.colSpan = detailDisplayHeaders.length;
      td.textContent = "无匹配课程";
      tr.append(td);
      tbody.append(tr);
      return;
    }

    data.forEach((row) => {
      const tr = document.createElement("tr");
      tr.dataset.creditLevel = creditLevel(row.学分);
      detailDisplayHeaders.forEach((header) => {
        const td = document.createElement("td");
        const value = row[header];
        if (header === "Yu Index") {
          td.className = "numeric yu-index-cell";
          const score = document.createElement("div");
          score.className = "yu-index-score";
          score.style.setProperty("--yu-score", `${Math.max(0, Math.min(100, (value / 5) * 100))}%`);
          score.innerHTML = `<strong>${value.toFixed(2)}</strong><i aria-hidden="true"></i>`;
          td.append(score);
        } else {
          td.textContent = value ?? "";
        }
        if (["学分", "绩点"].includes(header) || typeof value === "number") td.classList.add("numeric");
        if (header === "课程") td.classList.add("course-cell");
        if (header === "教师") td.classList.add("teacher-cell");
        tr.append(td);
      });
      tbody.append(tr);
    });
  }

  function showTooltip(event, title, lines, tooltipSelector = "#chart-tooltip") {
    const tooltip = $(tooltipSelector);
    tooltip.replaceChildren();
    const strong = document.createElement("strong");
    strong.textContent = title;
    tooltip.append(strong);
    lines.forEach((line) => {
      const span = document.createElement("span");
      span.textContent = line;
      tooltip.append(span);
    });
    tooltip.style.left = `${Math.min(event.clientX, window.innerWidth - 320)}px`;
    tooltip.style.top = `${Math.min(event.clientY, window.innerHeight - 120)}px`;
    tooltip.classList.add("is-visible");
    tooltip.setAttribute("aria-hidden", "false");
  }

  function hideTooltip(tooltipSelector = "#chart-tooltip") {
    const tooltip = $(tooltipSelector);
    tooltip.classList.remove("is-visible");
    tooltip.setAttribute("aria-hidden", "true");
  }

  function hideAllTooltips() {
    document.querySelectorAll(".chart-tooltip").forEach((tooltip) => {
      tooltip.classList.remove("is-visible");
      tooltip.setAttribute("aria-hidden", "true");
    });
  }

  function bindChartTooltip(target, title, lines, tooltipSelector = "#chart-tooltip") {
    const show = (event) => showTooltip(event, title, lines, tooltipSelector);
    target.setAttribute("data-chart-tooltip", "");

    target.addEventListener("pointermove", (event) => {
      if (event.pointerType !== "touch" && !pinnedTooltipTarget) show(event);
    });
    target.addEventListener("pointerleave", (event) => {
      if (event.pointerType !== "touch" && pinnedTooltipTarget !== target) hideTooltip(tooltipSelector);
    });
    target.addEventListener("click", (event) => {
      event.stopPropagation();
      if (pinnedTooltipTarget === target) {
        pinnedTooltipTarget = null;
        hideTooltip(tooltipSelector);
        return;
      }
      hideAllTooltips();
      pinnedTooltipTarget = target;
      show(event);
    });
  }

  function renderDonutChart(containerSelector, sourceRows, centerLabel, unit, ariaLabel) {
    const container = $(containerSelector);
    container.replaceChildren();
    const total = sourceRows.reduce((sum, item) => sum + item.value, 0);

    const layout = document.createElement("div");
    layout.className = "donut-layout";
    const wrap = document.createElement("div");
    wrap.className = "donut-wrap";
    const donut = svgElement("svg", { viewBox: "0 0 100 100" });
    donut.setAttribute("class", "donut");
    donut.setAttribute("role", "img");
    donut.setAttribute("aria-label", ariaLabel || sourceRows.map((item) => `${item.label} ${item.value}${unit}`).join("，"));
    let cursor = 0;
    sourceRows.forEach((item) => {
      const percent = (item.value / total) * 100;
      const tooltipLines = [`${formatValue(item.value, 1)}${unit}`, `占比 ${percent.toFixed(1)}%`];
      const segment = svgElement("circle", {
        cx: 50,
        cy: 50,
        r: 38,
        fill: "none",
        stroke: item.color,
        "stroke-width": 24,
        "stroke-dasharray": `${percent} ${100 - percent}`,
        "stroke-dashoffset": -cursor,
        "pathLength": 100,
        transform: "rotate(-90 50 50)",
        class: "donut-segment",
        tabindex: 0,
        "aria-label": `${item.label}，${tooltipLines.join("，")}`,
      });
      bindChartTooltip(segment, item.label, tooltipLines);
      donut.append(segment);
      cursor += percent;
    });
    const totalElement = document.createElement("div");
    totalElement.className = "donut-total";
    totalElement.innerHTML = `<strong>${formatValue(total, 1)}</strong><span>${centerLabel}</span>`;
    wrap.append(donut, totalElement);

    const legend = document.createElement("div");
    legend.className = "legend";
    sourceRows.forEach((item) => {
      const row = document.createElement("div");
      row.className = "legend-item";
      row.tabIndex = 0;
      row.innerHTML = `<i class="legend-swatch" style="background:${item.color}"></i><span>${item.label}</span><span class="legend-value">${formatValue(item.value, 1)}${unit}</span>`;
      const percent = (item.value / total) * 100;
      bindChartTooltip(row, item.label, [`${formatValue(item.value, 1)}${unit}`, `占比 ${percent.toFixed(1)}%`]);
      legend.append(row);
    });
    layout.append(wrap, legend);
    container.append(layout);
  }

  function renderCreditDonut() {
    const sourceRows = overviewRows.slice(1, 5).map((row) => ({
      label: row[0],
      value: Number(row[2]),
      color: categoryColors[row[0]],
    }));
    renderDonutChart("#credit-composition", sourceRows, "学分", "", "各类别学分构成");
  }

  function renderHorizontalBars(containerSelector, items, options = {}) {
    const container = $(containerSelector);
    container.replaceChildren();
    const width = Math.max(320, container.clientWidth || 480);
    const rowHeight = options.rowHeight || 34;
    const margin = { top: 12, right: 46, bottom: 28, left: options.left || 82 };
    const height = margin.top + margin.bottom + items.length * rowHeight;
    const maxValue = options.maxValue || Math.max(...items.map((item) => item.value), 1);
    const plotWidth = width - margin.left - margin.right;
    const svg = svgElement("svg", { viewBox: `0 0 ${width} ${height}`, role: "img", "aria-label": options.ariaLabel || "条形图" });

    for (let i = 0; i <= 4; i += 1) {
      const x = margin.left + (plotWidth * i) / 4;
      svg.append(svgElement("line", { x1: x, x2: x, y1: margin.top, y2: height - margin.bottom, class: "grid-line" }));
      svg.append(svgElement("text", { x, y: height - 8, "text-anchor": "middle", class: "tick-label" }, options.tickFormat ? options.tickFormat((maxValue * i) / 4) : formatValue((maxValue * i) / 4)));
    }

    items.forEach((item, index) => {
      const y = margin.top + index * rowHeight + 7;
      const barHeight = 18;
      const barWidth = (item.value / maxValue) * plotWidth;
      svg.append(svgElement("text", { x: margin.left - 10, y: y + 13, "text-anchor": "end", class: "tick-label" }, item.label));
      const rect = svgElement("rect", {
        x: margin.left,
        y,
        width: Math.max(barWidth, 1),
        height: barHeight,
        rx: 2,
        fill: item.color || "#386b81",
        stroke: item.stroke || "#244e61",
        "stroke-width": 0.7,
        tabindex: 0,
      });
      const tooltipLines = item.tooltip || [`${formatValue(item.value, options.valueDigits ?? 2)}${options.suffix || ""}`];
      bindChartTooltip(rect, item.label, tooltipLines);
      rect.addEventListener("focus", () => rect.setAttribute("opacity", "0.78"));
      rect.addEventListener("blur", () => rect.setAttribute("opacity", "1"));
      svg.append(rect);
      svg.append(svgElement("text", { x: Math.min(margin.left + barWidth + 7, width - 4), y: y + 13, class: "data-label" }, `${formatValue(item.value, options.valueDigits ?? 2)}${options.suffix || ""}`));
    });
    container.append(svg);
  }

  function renderGradeDistribution() {
    const bins = [
      { label: "4.5–5.0", count: 0 },
      { label: "4.0–4.4", count: 0 },
      { label: "3.5–3.9", count: 0 },
      { label: "3.0–3.4", count: 0 },
      { label: "<3.0", count: 0 },
    ];
    rows.forEach((row) => {
      const gpa = row.绩点;
      if (gpa >= 4.5) bins[0].count += 1;
      else if (gpa >= 4) bins[1].count += 1;
      else if (gpa >= 3.5) bins[2].count += 1;
      else if (gpa >= 3) bins[3].count += 1;
      else bins[4].count += 1;
    });
    renderDonutChart(
      "#grade-distribution",
      bins.map((bin, index) => ({ label: bin.label, value: bin.count, color: gradePalette[index] })),
      "课程",
      "",
      bins.map((bin) => `${bin.label} ${bin.count} 门课程`).join("，"),
    );
  }

  function buildTrendData(cumulative = false) {
    const accrued = [];
    return semesterOrder
      .map((semester) => {
        const year = yearOrder.find((value) => semester.startsWith(value));
        const term = termOrder.find((value) => semester.endsWith(value));
        const semesterRows = rows.filter((row) => row.学年 === year && row.学期 === term);
        accrued.push(...semesterRows);
        const items = cumulative ? accrued : semesterRows;
        return {
          label: `${year}${term === "第一学期" ? "上" : "下"}`,
          fullLabel: `${year} ${term}`,
          gpa: weightedAverage(items, (row) => row.绩点),
          rank: weightedAverage(items, (row) => parseRank(row.教学班排名).rank),
          percentile: weightedAverage(items, (row) => parseRank(row.教学班排名).percentile),
          credits: items.reduce((sum, row) => sum + row.学分, 0),
          count: items.length,
        };
      })
      .filter((item) => item.count);
  }

  function renderGpaTrend(containerSelector, semesterData, cumulativeData) {
    const container = $(containerSelector);
    container.replaceChildren();
    const legend = document.createElement("div");
    legend.className = "chart-legend-row";
    legend.innerHTML = `<span><i class="line-key"></i>各学期绩点</span><span><i class="line-key rank"></i>各学期排名</span><span><i class="line-key cumulative"></i>总绩点</span>`;
    container.append(legend);

    const width = Math.max(360, container.clientWidth || 900);
    const height = width < 520 ? 300 : 330;
    const margin = { top: 24, right: 54, bottom: 48, left: 52 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;

    const niceDomain = (values, absolutePadding) => {
      const rawMin = Math.min(...values);
      const rawMax = Math.max(...values);
      const rawSpan = Math.max(rawMax - rawMin, absolutePadding * 2);
      const paddedMin = rawMin - Math.max(rawSpan * 0.12, absolutePadding);
      const paddedMax = rawMax + Math.max(rawSpan * 0.12, absolutePadding);
      const roughStep = (paddedMax - paddedMin) / 4;
      const magnitude = 10 ** Math.floor(Math.log10(roughStep));
      const normalized = roughStep / magnitude;
      const niceNormalized = normalized <= 1 ? 1 : normalized <= 2 ? 2 : normalized <= 2.5 ? 2.5 : normalized <= 5 ? 5 : 10;
      const step = niceNormalized * magnitude;
      const min = Math.floor(paddedMin / step) * step;
      const max = Math.ceil(paddedMax / step) * step;
      const ticks = [];
      for (let value = min; value <= max + step / 2; value += step) ticks.push(Number(value.toFixed(8)));
      return { min, max, ticks, step };
    };

    const gpaScale = niceDomain([...semesterData, ...cumulativeData].map((item) => item.gpa), 0.05);
    const rankScale = niceDomain(semesterData.map((item) => item.rank), 1);
    rankScale.min = Math.max(1, rankScale.min);
    rankScale.ticks = rankScale.ticks.filter((value) => value >= rankScale.min);
    if (rankScale.ticks[0] !== rankScale.min) rankScale.ticks.unshift(rankScale.min);
    const gpaDigits = (String(gpaScale.step).split(".")[1] || "").length;
    const x = (index) => margin.left + (index / Math.max(semesterData.length - 1, 1)) * plotWidth;
    const gpaY = (value) => margin.top + ((gpaScale.max - value) / (gpaScale.max - gpaScale.min)) * plotHeight;
    const rankY = (value) => margin.top + ((value - rankScale.min) / (rankScale.max - rankScale.min)) * plotHeight;
    const svg = svgElement("svg", { viewBox: `0 0 ${width} ${height}`, role: "img", "aria-label": "各学期绩点、各学期平均教学班排名与总绩点折线图" });

    gpaScale.ticks.slice().reverse().forEach((gpaValue) => {
      const yPos = gpaY(gpaValue);
      svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: yPos, y2: yPos, class: "grid-line" }));
      svg.append(svgElement("text", { x: margin.left - 9, y: yPos + 4, "text-anchor": "end", class: "tick-label", fill: "#386b81" }, gpaValue.toFixed(gpaDigits)));
    });
    rankScale.ticks.forEach((rankValue) => {
      const yPos = rankY(rankValue);
      svg.append(svgElement("text", { x: width - margin.right + 9, y: yPos + 4, "text-anchor": "start", class: "tick-label", fill: "#748061" }, formatValue(rankValue, rankScale.step < 1 ? 1 : 0)));
    });
    svg.append(svgElement("text", { x: margin.left, y: 12, class: "tick-label", fill: "#386b81" }, "绩点"));
    svg.append(svgElement("text", { x: width - margin.right, y: 12, "text-anchor": "end", class: "tick-label", fill: "#748061" }, "排名"));

    const semesterPoints = semesterData.map((item, index) => [x(index), gpaY(item.gpa)]);
    const cumulativePoints = cumulativeData.map((item, index) => [x(index), gpaY(item.gpa)]);
    const rankPoints = semesterData.map((item, index) => [x(index), rankY(item.rank)]);
    const pathFor = (points) => points.map(([xPos, yPos], index) => `${index ? "L" : "M"}${xPos},${yPos}`).join(" ");
    svg.append(svgElement("path", { d: pathFor(rankPoints), class: "rank-line" }));
    svg.append(svgElement("path", { d: pathFor(semesterPoints), class: "gpa-line" }));
    svg.append(svgElement("path", { d: pathFor(cumulativePoints), class: "cumulative-line" }));

    semesterData.forEach((item, index) => {
      const cumulativeItem = cumulativeData[index];
      const xPos = x(index);
      const semesterYPos = semesterPoints[index][1];
      const cumulativeYPos = cumulativePoints[index][1];
      const rankYPos = rankPoints[index][1];
      const axisLabel = width < 520 ? item.label.replace("大", "") : item.label;
      svg.append(svgElement("text", { x: xPos, y: height - 17, "text-anchor": "middle", class: "tick-label" }, axisLabel));
      const tooltipLines = [
        `本学期绩点 ${item.gpa.toFixed(4)}`,
        `本学期排名 ${item.rank.toFixed(2)}`,
        `排名百分位 ${item.percentile.toFixed(2)}%`,
        `总绩点 ${cumulativeItem.gpa.toFixed(4)}`,
        `本学期 ${formatValue(item.credits, 1)} 学分 · ${item.count} 门课程`,
        `累计 ${formatValue(cumulativeItem.credits, 1)} 学分 · ${cumulativeItem.count} 门课程`,
      ];
      const semesterPoint = svgElement("circle", { cx: xPos, cy: semesterYPos, r: 4.5, class: "data-point", tabindex: 0 });
      const rankPoint = svgElement("circle", { cx: xPos, cy: rankYPos, r: 4.5, class: "rank-point", tabindex: 0 });
      const cumulativePoint = svgElement("circle", { cx: xPos, cy: cumulativeYPos, r: 4.5, class: "cumulative-point", tabindex: 0 });
      [semesterPoint, rankPoint, cumulativePoint].forEach((point) => {
        bindChartTooltip(point, item.fullLabel, tooltipLines);
      });
      svg.append(semesterPoint, rankPoint, cumulativePoint);
    });
    container.append(svg);
  }

  function renderTrendCharts() {
    renderGpaTrend("#gpa-trend", buildTrendData(false), buildTrendData(true));
  }

  function renderCategoryGpa() {
    const categories = [...new Set(rows.map((row) => row.类别))];
    const items = categories
      .map((category) => {
        const courses = rows.filter((row) => row.类别 === category);
        return {
          label: category,
          value: weightedAverage(courses, (row) => row.绩点),
          color: categoryColors[category],
          tooltip: [`绩点 ${weightedAverage(courses, (row) => row.绩点).toFixed(4)}`, `${courses.reduce((sum, row) => sum + row.学分, 0)} 学分`],
        };
      })
      .sort((a, b) => b.value - a.value);
    renderHorizontalBars("#category-gpa", items, {
      left: 56,
      rowHeight: 48,
      maxValue: 5,
      valueDigits: 4,
      ariaLabel: "各类别学分加权平均绩点",
    });
  }

  function isScatterExpanded() {
    const card = $("#score-rank-card");
    return document.fullscreenElement === card || card.classList.contains("is-expanded");
  }

  function isYuScatterExpanded() {
    const card = $("#yu-rank-card");
    return document.fullscreenElement === card || card.classList.contains("is-expanded");
  }

  function appendScatterCourseLabels(svg, data, x, y, valueAccessor, margin, width, height, compact = false) {
    const bounds = {
      left: margin.left + 3,
      right: width - margin.right - 3,
      top: margin.top + 3,
      bottom: height - margin.bottom - 3,
    };
    const labels = data.map((row) => ({
      row,
      pointX: x(row.rankPercentile),
      pointY: y(valueAccessor(row)),
      text: truncateText(row.课程, compact ? (width < 520 ? 5 : 9) : 18),
    }));

    const densityRadius = compact ? 42 : 58;
    labels.forEach((item) => {
      item.density = labels.filter((other) => other !== item && Math.hypot(other.pointX - item.pointX, other.pointY - item.pointY) < densityRadius).length;
    });
    labels.sort((a, b) => b.density - a.density || b.row.学分 - a.row.学分);

    const placed = [];
    const directions = [
      [1, 0], [-1, 0], [0.72, -0.72], [0.72, 0.72], [-0.72, -0.72], [-0.72, 0.72], [0, -1], [0, 1],
    ];
    const radii = compact ? [9, 14, 21, 30, 42] : [16, 24, 34, 48, 64, 84, 106];
    const overlapArea = (a, b) => Math.max(0, Math.min(a.right, b.right) - Math.max(a.left, b.left)) * Math.max(0, Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top));
    const clamp = (value, min, max) => Math.max(min, Math.min(max, value));

    labels.forEach((item) => {
      const textWidth = Array.from(item.text).reduce((sum, character) => sum + (/^[\x20-\x7e]$/.test(character) ? (compact ? 4.6 : 8) : (compact ? 8 : 14)), 4);
      const textHeight = compact ? 10 : 17;
      let bestCandidate = null;

      radii.forEach((radius) => {
        directions.forEach(([directionX, directionY]) => {
          const offsetX = directionX * radius;
          const offsetY = directionY * radius;
          const anchor = offsetX > 3 ? "start" : offsetX < -3 ? "end" : "middle";
          const labelX = item.pointX + offsetX;
          const labelY = item.pointY + offsetY + (compact ? 3 : 5);
          const left = anchor === "start" ? labelX : anchor === "end" ? labelX - textWidth : labelX - textWidth / 2;
          const box = { left, right: left + textWidth, top: labelY - textHeight + 2, bottom: labelY + 2 };
          if (box.left < bounds.left || box.right > bounds.right || box.top < bounds.top || box.bottom > bounds.bottom) return;

          const labelOverlap = placed.reduce((sum, placedBox) => sum + overlapArea(box, placedBox), 0);
          const coveredPoints = labels.filter((other) => other !== item && other.pointX >= box.left - 2 && other.pointX <= box.right + 2 && other.pointY >= box.top - 2 && other.pointY <= box.bottom + 2).length;
          const score = radius + labelOverlap * 180 + coveredPoints * 90;
          if (!bestCandidate || score < bestCandidate.score) bestCandidate = { anchor, box, labelX, labelY, score };
        });
      });

      if (!bestCandidate) {
        const labelX = clamp(item.pointX + 12, bounds.left, bounds.right - textWidth);
        const labelY = clamp(item.pointY + (compact ? 3 : 5), bounds.top + textHeight, bounds.bottom - 2);
        bestCandidate = {
          anchor: "start",
          box: { left: labelX, right: labelX + textWidth, top: labelY - textHeight + 2, bottom: labelY + 2 },
          labelX,
          labelY,
        };
      }

      placed.push(bestCandidate.box);
      const lineEndX = clamp(item.pointX, bestCandidate.box.left, bestCandidate.box.right);
      const lineEndY = clamp(item.pointY, bestCandidate.box.top, bestCandidate.box.bottom);
      svg.append(svgElement("line", {
        x1: item.pointX,
        y1: item.pointY,
        x2: lineEndX,
        y2: lineEndY,
        class: "scatter-label-line",
      }));
      svg.append(svgElement("text", {
        x: bestCandidate.labelX,
        y: bestCandidate.labelY,
        "text-anchor": bestCandidate.anchor,
        class: `scatter-course-label${compact ? " scatter-course-label-compact" : ""}`,
      }, item.text));
    });
  }

  function selectedScatterRows() {
    return scatterFilterRows()
      .filter((row) => scatterSelected.has(row.index))
      .map((row) => ({ ...row, rankPercentile: parseRank(row.教学班排名).percentile }));
  }

  function renderScatter() {
    const container = $("#score-rank-scatter");
    container.replaceChildren();
    const expanded = isScatterExpanded();
    const data = selectedScatterRows();
    if (!data.length) {
      const empty = document.createElement("div");
      empty.className = "scatter-empty";
      empty.textContent = "未选择课程";
      container.append(empty);
      return;
    }
    const width = Math.max(360, container.clientWidth || 720);
    const height = expanded ? Math.max(560, container.clientHeight || window.innerHeight - 84) : width < 520 ? 320 : 350;
    const margin = expanded ? { top: 76, right: 32, bottom: 72, left: 84 } : { top: 58, right: 20, bottom: 52, left: 64 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;
    const minScore = 2.5;
    const maxScore = 5;
    const minRank = 0.5;
    const maxRank = 100;
    const logMin = Math.log10(minRank);
    const logMax = Math.log10(maxRank);
    const x = (rank) => margin.left + ((logMax - Math.log10(rank)) / (logMax - logMin)) * plotWidth;
    const y = (score) => margin.top + ((maxScore - score) / (maxScore - minScore)) * plotHeight;
    const sortedRanks = data.map((row) => row.rankPercentile).sort((a, b) => a - b);
    const sortedScores = data.map((row) => row.绩点).sort((a, b) => a - b);
    const rankMedian = medianValue(sortedRanks);
    const scoreMedian = medianValue(sortedScores);
    const medianX = x(rankMedian);
    const medianY = y(scoreMedian);
    const svg = svgElement("svg", { viewBox: `0 0 ${width} ${height}`, role: "img", "aria-label": "横轴为对数刻度教学班排名百分位、纵轴为课程绩点的四象限散点图" });

    [
      [margin.left, margin.top, medianX - margin.left, medianY - margin.top, "rgba(56,107,129,0.045)"],
      [medianX, margin.top, width - margin.right - medianX, medianY - margin.top, "rgba(180,138,58,0.055)"],
      [margin.left, medianY, medianX - margin.left, height - margin.bottom - medianY, "rgba(116,128,97,0.035)"],
      [medianX, medianY, width - margin.right - medianX, height - margin.bottom - medianY, "rgba(200,117,66,0.035)"],
    ].forEach(([rectX, rectY, rectWidth, rectHeight, fill]) => {
      svg.append(svgElement("rect", { x: rectX, y: rectY, width: rectWidth, height: rectHeight, fill }));
    });

    [1, 2, 5, 10, 20, 50, 100].forEach((value) => {
      const xPos = x(value);
      svg.append(svgElement("line", { x1: xPos, x2: xPos, y1: margin.top, y2: height - margin.bottom, class: "grid-line" }));
      svg.append(svgElement("text", { x: xPos, y: height - (expanded ? 29 : 19), "text-anchor": "middle", class: "tick-label" }, `${value}%`));
    });
    [2.5, 3, 3.5, 4, 4.5, 5].forEach((value) => {
      const yPos = y(value);
      svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: yPos, y2: yPos, class: "grid-line" }));
      svg.append(svgElement("text", { x: margin.left - 8, y: yPos + 4, "text-anchor": "end", class: "tick-label" }, value.toFixed(1)));
    });
    svg.append(svgElement("line", { x1: medianX, x2: medianX, y1: margin.top, y2: height - margin.bottom, class: "quadrant-line" }));
    svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: medianY, y2: medianY, class: "quadrant-line" }));
    svg.append(svgElement("text", { x: margin.left + plotWidth / 2, y: height - (expanded ? 5 : 3), "text-anchor": "middle", class: "tick-label" }, "教学班排名百分位（对数刻度）"));
    const yAxisCenter = margin.top + plotHeight / 2;
    const yAxisLabelX = expanded ? 22 : 16;
    svg.append(svgElement("text", { x: yAxisLabelX, y: yAxisCenter, transform: `rotate(-90 ${yAxisLabelX} ${yAxisCenter})`, "text-anchor": "middle", class: "tick-label" }, "绩点"));

    if (width >= 620) {
      svg.append(svgElement("text", { x: margin.left + 8, y: margin.top + (expanded ? 22 : 16), class: "quadrant-label" }, "排名较低 · 绩点较高（课程给分高）"));
      svg.append(svgElement("text", { x: width - margin.right - 8, y: height - margin.bottom - (expanded ? 12 : 9), "text-anchor": "end", class: "quadrant-label" }, "排名较高 · 绩点较低（课程给分低）"));
    }

    const legendStep = expanded ? 92 : 70;
    const legendWidth = legendStep * Object.keys(categoryColors).length;
    const legend = svgElement("g", { transform: `translate(${Math.max(margin.left, width - margin.right - legendWidth)},${expanded ? 32 : 24})` });
    Object.entries(categoryColors).forEach(([label, color], index) => {
      const offset = index * legendStep;
      legend.append(scatterSymbol(label, offset + (expanded ? 7 : 5), 0, expanded ? 5.5 : 4, { fill: color }));
      legend.append(svgElement("text", { x: offset + (expanded ? 17 : 12), y: expanded ? 5 : 4, class: "tick-label" }, label));
    });
    svg.append(legend);

    appendScatterCourseLabels(svg, data, x, y, (row) => row.绩点, margin, width, height, !expanded);
    data.forEach((row) => {
      const marker = scatterSymbol(row.类别, x(row.rankPercentile), y(row.绩点), (3 + Math.sqrt(row.学分) * 1.15) * (expanded ? 1.8 : 1), {
        class: "scatter-point",
        stroke: "#ffffff",
        "stroke-width": expanded ? 1.5 : 1,
        opacity: 0.78,
        tabindex: 0,
      });
      const tooltipLines = [
        ...(row.教师 ? [`教师 ${truncateText(row.教师)}`] : []),
        `最终成绩 ${row.最终成绩}`,
        `绩点 ${row.绩点.toFixed(1)}`,
        `教学班排名 ${row.教学班排名}`,
        `排名百分位 ${row.rankPercentile.toFixed(2)}%`,
        `Yu Index ${row["Yu Index"].toFixed(2)}`,
        `${row.学分} 学分 · ${row.类别}`,
      ];
      bindChartTooltip(marker, row.课程, tooltipLines);
      marker.addEventListener("focus", () => marker.setAttribute("opacity", "1"));
      marker.addEventListener("blur", () => marker.setAttribute("opacity", "0.78"));
      svg.append(marker);
    });
    container.append(svg);
  }

  function renderYuRankScatter() {
    const container = $("#yu-rank-scatter");
    container.replaceChildren();
    const data = selectedScatterRows();
    if (!data.length) {
      const empty = document.createElement("div");
      empty.className = "scatter-empty";
      empty.textContent = "未选择课程";
      container.append(empty);
      return;
    }

    const expanded = isYuScatterExpanded();
    const width = Math.max(360, container.clientWidth || 720);
    const height = expanded ? Math.max(560, container.clientHeight || window.innerHeight - 84) : width < 520 ? 330 : 390;
    const margin = expanded ? { top: 76, right: 32, bottom: 72, left: 84 } : { top: 58, right: 20, bottom: 52, left: 64 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;
    const minRank = 0.5;
    const maxRank = 100;
    const logMin = Math.log10(minRank);
    const logMax = Math.log10(maxRank);
    const x = (rank) => margin.left + ((logMax - Math.log10(rank)) / (logMax - logMin)) * plotWidth;
    const minYuIndex = 2.5;
    const maxYuIndex = 5;
    const y = (value) => margin.top + ((maxYuIndex - value) / (maxYuIndex - minYuIndex)) * plotHeight;
    const svg = svgElement("svg", { viewBox: `0 0 ${width} ${height}`, role: "img", "aria-label": "横轴为对数刻度教学班排名百分位、纵轴为 Yu Index 估计平均绩点的课程散点图" });

    [1, 2, 5, 10, 20, 50, 100].forEach((value) => {
      const xPos = x(value);
      svg.append(svgElement("line", { x1: xPos, x2: xPos, y1: margin.top, y2: height - margin.bottom, class: "grid-line" }));
      svg.append(svgElement("text", { x: xPos, y: height - (expanded ? 29 : 19), "text-anchor": "middle", class: "tick-label" }, `${value}%`));
    });
    [2.5, 3, 3.5, 4, 4.5, 5].forEach((value) => {
      const yPos = y(value);
      svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: yPos, y2: yPos, class: "grid-line" }));
      svg.append(svgElement("text", { x: margin.left - 8, y: yPos + 4, "text-anchor": "end", class: "tick-label" }, value.toFixed(1)));
    });
    svg.append(svgElement("text", { x: margin.left + plotWidth / 2, y: height - (expanded ? 5 : 3), "text-anchor": "middle", class: "tick-label" }, "教学班排名百分位（对数刻度）"));
    const yAxisCenter = margin.top + plotHeight / 2;
    const yAxisLabelX = expanded ? 22 : 16;
    svg.append(svgElement("text", { x: yAxisLabelX, y: yAxisCenter, transform: `rotate(-90 ${yAxisLabelX} ${yAxisCenter})`, "text-anchor": "middle", class: "tick-label" }, "Yu Index"));

    const benchmarkY = y(yuIndexBenchmark);
    svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: benchmarkY, y2: benchmarkY, class: "yu-benchmark-line" }));
    svg.append(svgElement("text", { x: width - margin.right - 8, y: benchmarkY - (expanded ? 10 : 7), "text-anchor": "end", class: "yu-benchmark-label" }, "给分较友好"));
    svg.append(svgElement("text", { x: width - margin.right - 8, y: benchmarkY + (expanded ? 20 : 15), "text-anchor": "end", class: "yu-benchmark-label" }, "给分较严格"));
    svg.append(svgElement("text", { x: margin.left + 7, y: benchmarkY - (expanded ? 10 : 7), class: "yu-benchmark-value" }, `数据集中位数 ${yuIndexBenchmark.toFixed(2)}`));

    const legendStep = expanded ? 92 : 70;
    const legendWidth = legendStep * Object.keys(categoryColors).length;
    const legend = svgElement("g", { transform: `translate(${Math.max(margin.left, width - margin.right - legendWidth)},${expanded ? 32 : 24})` });
    Object.entries(categoryColors).forEach(([label, color], index) => {
      const offset = index * legendStep;
      legend.append(scatterSymbol(label, offset + (expanded ? 7 : 5), 0, expanded ? 5.5 : 4, { fill: color }));
      legend.append(svgElement("text", { x: offset + (expanded ? 17 : 12), y: expanded ? 5 : 4, class: "tick-label" }, label));
    });
    svg.append(legend);

    appendScatterCourseLabels(svg, data, x, y, (row) => row["Yu Index"], margin, width, height, !expanded);
    data.forEach((row) => {
      const marker = scatterSymbol(row.类别, x(row.rankPercentile), y(row["Yu Index"]), (3 + Math.sqrt(row.学分) * 1.15) * (expanded ? 1.8 : 1), {
        class: "scatter-point",
        stroke: "#ffffff",
        "stroke-width": expanded ? 1.5 : 1,
        opacity: 0.8,
        tabindex: 0,
      });
      bindChartTooltip(marker, row.课程, [
        ...(row.教师 ? [`教师 ${truncateText(row.教师)}`] : []),
        `Yu Index ${row["Yu Index"].toFixed(2)}`,
        `绩点 ${row.绩点.toFixed(1)}`,
        `教学班排名 ${row.教学班排名}`,
        `排名百分位 ${row.rankPercentile.toFixed(2)}%`,
        `${row.学分} 学分 · ${row.类别}`,
      ], "#yu-chart-tooltip");
      svg.append(marker);
    });
    container.append(svg);
  }

  function setupChartFullscreen({ cardSelector, titleSelector, buttonSelector, downloadSelector, chartSelector, defaultTitle, expandedTitle, ariaTitle, renderChart, isExpanded }) {
    const card = $(cardSelector);
    const title = $(titleSelector);
    const button = $(buttonSelector);
    const downloadButton = $(downloadSelector);
    let preparedPngUrl = "";

    const preparePng = async () => {
      downloadButton.setAttribute("aria-disabled", "true");
      downloadButton.textContent = "生成中";
      const source = $(`${chartSelector} svg`);
      if (!source) {
        preparedPngUrl = "";
        downloadButton.removeAttribute("href");
        downloadButton.textContent = "无可下载课程";
        return;
      }
      const clone = source.cloneNode(true);
      const viewBox = source.viewBox.baseVal;
      const titleHeight = 82;
      const exportHeight = viewBox.height + titleHeight;
      const exportScale = Math.max(2, Math.min(3, 3600 / viewBox.width));
      clone.setAttribute("width", viewBox.width);
      clone.setAttribute("height", exportHeight);
      clone.setAttribute("viewBox", `0 0 ${viewBox.width} ${exportHeight}`);
      clone.setAttribute("xmlns", svgNS);

      const chartGroup = svgElement("g", { transform: `translate(0 ${titleHeight})` });
      Array.from(clone.childNodes).forEach((child) => chartGroup.append(child));
      clone.replaceChildren();

      const style = svgElement("style", {}, `
        svg { font-family: "Microsoft YaHei", "Noto Sans CJK SC", sans-serif; }
        .tick-label { fill: #737c82; font-family: Consolas, monospace; font-size: 16px; }
        .grid-line { stroke: #e8ebe9; stroke-width: 1.2; }
        .quadrant-line { stroke: #7d878c; stroke-dasharray: 5 5; stroke-width: 1.3; }
        .quadrant-label { fill: #7b858a; stroke: #ffffff; stroke-width: 4px; paint-order: stroke; font-size: 14px; letter-spacing: 0.02em; }
        .yu-benchmark-line { stroke: #667278; stroke-dasharray: 7 5; stroke-width: 1.4; }
        .yu-benchmark-label, .yu-benchmark-value { fill: #59666c; stroke: #ffffff; stroke-width: 4px; paint-order: stroke; font-size: 14px; }
        .yu-benchmark-label { font-weight: 650; }
        .scatter-label-line { stroke: rgba(75, 87, 93, 0.42); stroke-width: 1.15; }
        .scatter-course-label { fill: #34434a; stroke: #ffffff; stroke-width: 5px; paint-order: stroke; font-size: 15px; }
        .scatter-point { stroke-width: 1.6px; }
        .export-title { fill: #1f3139; font-size: 30px; font-weight: 700; letter-spacing: -0.01em; }
      `);
      clone.append(style);
      clone.append(svgElement("text", { x: viewBox.width / 2, y: 52, "text-anchor": "middle", class: "export-title" }, expandedTitle));
      clone.append(chartGroup);

      const svgBlob = new Blob([new XMLSerializer().serializeToString(clone)], { type: "image/svg+xml;charset=utf-8" });
      const svgUrl = URL.createObjectURL(svgBlob);
      const image = new Image();
      image.src = svgUrl;
      await image.decode();

      const canvas = document.createElement("canvas");
      canvas.width = Math.round(viewBox.width * exportScale);
      canvas.height = Math.round(exportHeight * exportScale);
      const context = canvas.getContext("2d");
      context.fillStyle = "#ffffff";
      context.fillRect(0, 0, canvas.width, canvas.height);
      context.drawImage(image, 0, 0, canvas.width, canvas.height);
      URL.revokeObjectURL(svgUrl);

      preparedPngUrl = canvas.toDataURL("image/png");
      downloadButton.href = preparedPngUrl;
      downloadButton.removeAttribute("aria-disabled");
      downloadButton.textContent = "下载 PNG";
    };

    const update = () => {
      const expanded = isExpanded();
      title.textContent = expanded ? expandedTitle : defaultTitle;
      button.textContent = expanded ? "退出全屏" : "全屏";
      button.setAttribute("aria-label", expanded ? `退出${ariaTitle}全屏` : `全屏查看${ariaTitle}`);
      renderChart();
      if (expanded) {
        preparePng();
      } else {
        preparedPngUrl = "";
        downloadButton.removeAttribute("href");
        downloadButton.setAttribute("aria-disabled", "true");
        downloadButton.textContent = "下载 PNG";
      }
    };

    button.addEventListener("click", async () => {
      if (document.fullscreenElement === card) {
        await document.exitFullscreen();
        return;
      }
      if (card.classList.contains("is-expanded")) {
        card.classList.remove("is-expanded");
        document.body.classList.remove("chart-expanded");
        update();
        return;
      }
      if (card.requestFullscreen) {
        try {
          await card.requestFullscreen();
          return;
        } catch (_error) {
          // Continue with the in-page full-screen fallback.
        }
      }
      card.classList.add("is-expanded");
      document.body.classList.add("chart-expanded");
      update();
    });

    document.addEventListener("fullscreenchange", update);
    document.addEventListener("scatterfilterschange", () => {
      if (isExpanded()) preparePng();
    });
  }

  function setupScatterFullscreen() {
    setupChartFullscreen({
      cardSelector: "#score-rank-card",
      titleSelector: "#score-rank-title",
      buttonSelector: "#scatter-fullscreen",
      downloadSelector: "#scatter-download",
      chartSelector: "#score-rank-scatter",
      defaultTitle: scatterDefaultTitle,
      expandedTitle: scatterExpandedTitle,
      ariaTitle: "课程绩点与排名分布图",
      renderChart: renderScatter,
      isExpanded: isScatterExpanded,
    });
    setupChartFullscreen({
      cardSelector: "#yu-rank-card",
      titleSelector: "#yu-rank-title",
      buttonSelector: "#yu-scatter-fullscreen",
      downloadSelector: "#yu-scatter-download",
      chartSelector: "#yu-rank-scatter",
      defaultTitle: yuScatterDefaultTitle,
      expandedTitle: yuScatterExpandedTitle,
      ariaTitle: "Yu Index 与排名分布图",
      renderChart: renderYuRankScatter,
      isExpanded: isYuScatterExpanded,
    });
  }

  function renderCharts() {
    pinnedTooltipTarget = null;
    hideAllTooltips();
    renderCreditDonut();
    renderGradeDistribution();
    renderTrendCharts();
    renderCategoryGpa();
    renderScatter();
    renderYuRankScatter();
  }

  renderOverviewBenchmarks();
  setupYuCalculator();
  renderDetailsHead();
  setupFilters();
  setupScatterFilters();
  updateSortIndicators();
  renderDetailsBody();
  setupScatterFullscreen();
  renderCharts();

  document.addEventListener("pointerdown", (event) => {
    if (pinnedTooltipTarget && !event.target.closest("[data-chart-tooltip]")) {
      pinnedTooltipTarget = null;
      hideAllTooltips();
    }
  });

  let resizeTimer;
  let chartViewportWidth = window.innerWidth;
  window.addEventListener("resize", () => {
    if (Math.abs(window.innerWidth - chartViewportWidth) < 2) return;
    chartViewportWidth = window.innerWidth;
    window.clearTimeout(resizeTimer);
    resizeTimer = window.setTimeout(renderCharts, 140);
  });
})();
