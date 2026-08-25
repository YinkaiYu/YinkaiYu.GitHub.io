(() => {
  "use strict";

  const overviewRows = window.GRADE_DATA.overview;
  const detailRows = window.GRADE_DATA.details;
  const overviewHeaders = overviewRows[0];
  const detailHeaders = detailRows[0];
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
  }));

  const gradePalette = ["#244e61", "#386b81", "#5f8798", "#8da8b3", "#c5d2d7"];
  const categoryColors = {
    公必: "#386b81",
    专必: "#244e61",
    公选: "#b48a3a",
    专选: "#748061",
  };
  const yearOrder = ["大一", "大二", "大三", "大四"];
  const termOrder = ["第一学期", "第二学期"];
  const semesterOrder = yearOrder.flatMap((year) => termOrder.map((term) => `${year}${term}`));

  let sortState = { key: "index", direction: "asc" };
  let pinnedTooltipTarget = null;

  const $ = (selector) => document.querySelector(selector);
  const svgNS = "http://www.w3.org/2000/svg";

  function svgElement(name, attributes = {}, text = "") {
    const node = document.createElementNS(svgNS, name);
    Object.entries(attributes).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== "") node.textContent = text;
    return node;
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

  function weightedAverage(items, valueAccessor, weightAccessor = (item) => item.学分) {
    const valid = items.filter((item) => Number.isFinite(valueAccessor(item)) && weightAccessor(item) > 0);
    const weight = valid.reduce((sum, item) => sum + weightAccessor(item), 0);
    return weight ? valid.reduce((sum, item) => sum + valueAccessor(item) * weightAccessor(item), 0) / weight : 0;
  }

  function renderOverviewTable() {
    const table = $("#overview-table");
    const benchmarks = $("#overview-benchmarks");
    benchmarks.replaceChildren();
    const thead = document.createElement("thead");
    const headRow = document.createElement("tr");
    overviewHeaders.forEach((header) => {
      const th = document.createElement("th");
      th.scope = "col";
      th.textContent = header;
      headRow.append(th);
    });
    thead.append(headRow);

    const tbody = document.createElement("tbody");
    overviewRows.slice(1).forEach((row) => {
      if (typeof row[0] === "string" && row[0].includes("|")) {
        const labels = row[0].split("|");
        const values = row.slice(1).filter((value) => value !== null && value !== "");
        labels.forEach((label, index) => {
          const card = document.createElement("div");
          card.className = "overview-benchmark";
          const labelElement = document.createElement("span");
          labelElement.textContent = label;
          const valueElement = document.createElement("strong");
          valueElement.textContent = values[index] ?? "";
          card.append(labelElement, valueElement);
          benchmarks.append(card);
        });
        return;
      }

      const tr = document.createElement("tr");
      row.forEach((value, index) => {
        const td = document.createElement("td");
        if (typeof value === "number") td.className = "numeric";
        td.textContent = value ?? "";
        if (index > 0) td.dataset.label = overviewHeaders[index];
        tr.append(td);
      });
      tbody.append(tr);
    });
    table.append(thead, tbody);
  }

  function addOptions(select, values) {
    values.forEach((value) => {
      const option = document.createElement("option");
      option.value = value;
      option.textContent = value;
      select.append(option);
    });
  }

  function setupFilters() {
    addOptions($("#category-filter"), [...new Set(rows.map((row) => row.类别))]);
    addOptions($("#year-filter"), yearOrder.filter((value) => rows.some((row) => row.学年 === value)));
    addOptions($("#term-filter"), termOrder.filter((value) => rows.some((row) => row.学期 === value)));

    ["#search", "#category-filter", "#year-filter", "#term-filter", "#credit-filter"].forEach((selector) => {
      $(selector).addEventListener(selector === "#search" ? "input" : "change", renderDetailsBody);
    });

    $("#reset-filters").addEventListener("click", () => {
      $("#search").value = "";
      $("#category-filter").value = "";
      $("#year-filter").value = "";
      $("#term-filter").value = "";
      $("#credit-filter").value = "";
      sortState = { key: "index", direction: "asc" };
      updateSortIndicators();
      renderDetailsBody();
    });
  }

  function creditMatches(credit, filter) {
    if (!filter) return true;
    if (filter === "0-1") return credit <= 1;
    if (filter === "1.5-2") return credit >= 1.5 && credit <= 2;
    if (filter === "3-plus") return credit >= 3;
    return true;
  }

  function filteredRows() {
    const query = $("#search").value.trim().toLocaleLowerCase("zh-CN");
    const category = $("#category-filter").value;
    const year = $("#year-filter").value;
    const term = $("#term-filter").value;
    const credit = $("#credit-filter").value;

    const filtered = rows.filter((row) => {
      const searchable = `${row.课程} ${row.教师}`.toLocaleLowerCase("zh-CN");
      return (
        (!query || searchable.includes(query)) &&
        (!category || row.类别 === category) &&
        (!year || row.学年 === year) &&
        (!term || row.学期 === term) &&
        creditMatches(row.学分, credit)
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
    detailHeaders.forEach((header) => {
      const th = document.createElement("th");
      th.scope = "col";
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
      td.colSpan = detailHeaders.length;
      td.textContent = "无匹配课程";
      tr.append(td);
      tbody.append(tr);
      return;
    }

    data.forEach((row) => {
      const tr = document.createElement("tr");
      tr.dataset.creditLevel = creditLevel(row.学分);
      detailHeaders.forEach((header) => {
        const td = document.createElement("td");
        const value = row[header];
        td.textContent = value ?? "";
        if (["学分", "绩点"].includes(header) || typeof value === "number") td.classList.add("numeric");
        if (header === "课程") td.classList.add("course-cell");
        if (header === "教师") td.classList.add("teacher-cell");
        tr.append(td);
      });
      tbody.append(tr);
    });
  }

  function showTooltip(event, title, lines) {
    const tooltip = $("#chart-tooltip");
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

  function hideTooltip() {
    const tooltip = $("#chart-tooltip");
    tooltip.classList.remove("is-visible");
    tooltip.setAttribute("aria-hidden", "true");
  }

  function bindChartTooltip(target, title, lines) {
    const show = (event) => showTooltip(event, title, lines);
    target.setAttribute("data-chart-tooltip", "");

    target.addEventListener("pointermove", (event) => {
      if (event.pointerType !== "touch" && !pinnedTooltipTarget) show(event);
    });
    target.addEventListener("pointerleave", (event) => {
      if (event.pointerType !== "touch" && pinnedTooltipTarget !== target) hideTooltip();
    });
    target.addEventListener("click", (event) => {
      event.stopPropagation();
      if (pinnedTooltipTarget === target) {
        pinnedTooltipTarget = null;
        hideTooltip();
        return;
      }
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
      { label: "95–100", count: 0 },
      { label: "90–94.9", count: 0 },
      { label: "85–89.9", count: 0 },
      { label: "80–84.9", count: 0 },
      { label: "<80", count: 0 },
    ];
    rows.forEach((row) => {
      const score = row.最终成绩;
      if (typeof score === "number") {
        if (score >= 95) bins[0].count += 1;
        else if (score >= 90) bins[1].count += 1;
        else if (score >= 85) bins[2].count += 1;
        else if (score >= 80) bins[3].count += 1;
        else bins[4].count += 1;
      }
    });
    renderDonutChart(
      "#grade-distribution",
      bins.map((bin, index) => ({ label: bin.label, value: bin.count, color: gradePalette[index] })),
      "数值成绩课程",
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

  function appendScatterCourseLabels(svg, data, x, y, margin, width, height) {
    const bounds = {
      left: margin.left + 3,
      right: width - margin.right - 3,
      top: margin.top + 3,
      bottom: height - margin.bottom - 3,
    };
    const labels = data.map((row) => ({
      row,
      pointX: x(row.rankPercentile),
      pointY: y(row.最终成绩),
      text: truncateText(row.课程, 18),
    }));

    labels.forEach((item) => {
      item.density = labels.filter((other) => other !== item && Math.hypot(other.pointX - item.pointX, other.pointY - item.pointY) < 42).length;
    });
    labels.sort((a, b) => b.density - a.density || b.row.学分 - a.row.学分);

    const placed = [];
    const directions = [
      [1, 0], [-1, 0], [0.72, -0.72], [0.72, 0.72], [-0.72, -0.72], [-0.72, 0.72], [0, -1], [0, 1],
    ];
    const radii = [12, 19, 28, 40, 54, 70, 88];
    const overlapArea = (a, b) => Math.max(0, Math.min(a.right, b.right) - Math.max(a.left, b.left)) * Math.max(0, Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top));
    const clamp = (value, min, max) => Math.max(min, Math.min(max, value));

    labels.forEach((item) => {
      const textWidth = Array.from(item.text).reduce((sum, character) => sum + (/^[\x20-\x7e]$/.test(character) ? 5.8 : 10), 4);
      const textHeight = 12;
      let bestCandidate = null;

      radii.forEach((radius) => {
        directions.forEach(([directionX, directionY]) => {
          const offsetX = directionX * radius;
          const offsetY = directionY * radius;
          const anchor = offsetX > 3 ? "start" : offsetX < -3 ? "end" : "middle";
          const labelX = item.pointX + offsetX;
          const labelY = item.pointY + offsetY + 4;
          const left = anchor === "start" ? labelX : anchor === "end" ? labelX - textWidth : labelX - textWidth / 2;
          const box = { left, right: left + textWidth, top: labelY - 10, bottom: labelY + 2 };
          if (box.left < bounds.left || box.right > bounds.right || box.top < bounds.top || box.bottom > bounds.bottom) return;

          const labelOverlap = placed.reduce((sum, placedBox) => sum + overlapArea(box, placedBox), 0);
          const coveredPoints = labels.filter((other) => other !== item && other.pointX >= box.left - 2 && other.pointX <= box.right + 2 && other.pointY >= box.top - 2 && other.pointY <= box.bottom + 2).length;
          const score = radius + labelOverlap * 180 + coveredPoints * 90;
          if (!bestCandidate || score < bestCandidate.score) bestCandidate = { anchor, box, labelX, labelY, score };
        });
      });

      if (!bestCandidate) {
        const labelX = clamp(item.pointX + 12, bounds.left, bounds.right - textWidth);
        const labelY = clamp(item.pointY + 4, bounds.top + 10, bounds.bottom - 2);
        bestCandidate = {
          anchor: "start",
          box: { left: labelX, right: labelX + textWidth, top: labelY - 10, bottom: labelY + 2 },
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
        class: "scatter-course-label",
      }, item.text));
    });
  }

  function renderScatter() {
    const container = $("#score-rank-scatter");
    container.replaceChildren();
    const expanded = isScatterExpanded();
    const data = rows
      .filter((row) => typeof row.最终成绩 === "number")
      .map((row) => ({ ...row, rankPercentile: parseRank(row.教学班排名).percentile }));
    const width = Math.max(360, container.clientWidth || 720);
    const height = expanded ? Math.max(560, container.clientHeight || window.innerHeight - 84) : width < 520 ? 320 : 350;
    const margin = { top: 48, right: 20, bottom: 52, left: 52 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;
    const minScore = 75;
    const maxScore = 100;
    const minRank = 0.5;
    const maxRank = 100;
    const logMin = Math.log10(minRank);
    const logMax = Math.log10(maxRank);
    const x = (rank) => margin.left + ((logMax - Math.log10(rank)) / (logMax - logMin)) * plotWidth;
    const y = (score) => margin.top + ((maxScore - score) / (maxScore - minScore)) * plotHeight;
    const sortedRanks = data.map((row) => row.rankPercentile).sort((a, b) => a - b);
    const sortedScores = data.map((row) => row.最终成绩).sort((a, b) => a - b);
    const median = (values) => values.length % 2 ? values[(values.length - 1) / 2] : (values[values.length / 2 - 1] + values[values.length / 2]) / 2;
    const rankMedian = median(sortedRanks);
    const scoreMedian = median(sortedScores);
    const medianX = x(rankMedian);
    const medianY = y(scoreMedian);
    const svg = svgElement("svg", { viewBox: `0 0 ${width} ${height}`, role: "img", "aria-label": "横轴为对数刻度教学班排名百分位、纵轴为课程最终成绩的四象限散点图" });

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
      svg.append(svgElement("text", { x: xPos, y: height - 19, "text-anchor": "middle", class: "tick-label" }, `${value}%`));
    });
    [75, 80, 85, 90, 95, 100].forEach((value) => {
      const yPos = y(value);
      svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: yPos, y2: yPos, class: "grid-line" }));
      svg.append(svgElement("text", { x: margin.left - 8, y: yPos + 4, "text-anchor": "end", class: "tick-label" }, value));
    });
    svg.append(svgElement("line", { x1: medianX, x2: medianX, y1: margin.top, y2: height - margin.bottom, class: "quadrant-line" }));
    svg.append(svgElement("line", { x1: margin.left, x2: width - margin.right, y1: medianY, y2: medianY, class: "quadrant-line" }));
    svg.append(svgElement("text", { x: width - margin.right, y: height - 3, "text-anchor": "end", class: "tick-label" }, "教学班排名百分位（对数刻度）"));
    svg.append(svgElement("text", { x: margin.left, y: margin.top - 10, class: "tick-label" }, "最终成绩"));

    if (width >= 620) {
      svg.append(svgElement("text", { x: margin.left + 8, y: margin.top + 16, class: "quadrant-label" }, "排名较低 · 成绩较高（课程给分高）"));
      svg.append(svgElement("text", { x: width - margin.right - 8, y: height - margin.bottom - 9, "text-anchor": "end", class: "quadrant-label" }, "排名较高 · 成绩较低（课程给分低）"));
    }

    const legend = svgElement("g", { transform: `translate(${margin.left},20)` });
    Object.entries(categoryColors).forEach(([label, color], index) => {
      const offset = index * 64;
      legend.append(svgElement("circle", { cx: offset + 4, cy: 0, r: 4, fill: color }));
      legend.append(svgElement("text", { x: offset + 12, y: 4, class: "tick-label" }, label));
    });
    svg.append(legend);

    if (expanded) appendScatterCourseLabels(svg, data, x, y, margin, width, height);
    data.forEach((row) => {
      const circle = svgElement("circle", {
        cx: x(row.rankPercentile),
        cy: y(row.最终成绩),
        r: 3 + Math.sqrt(row.学分) * 1.15,
        fill: categoryColors[row.类别],
        stroke: "#ffffff",
        "stroke-width": 1,
        opacity: 0.78,
        tabindex: 0,
      });
      const tooltipLines = [
        ...(row.教师 ? [`教师 ${truncateText(row.教师)}`] : []),
        `成绩 ${row.最终成绩}`,
        `教学班排名 ${row.教学班排名}`,
        `排名百分位 ${row.rankPercentile.toFixed(2)}%`,
        `${row.学分} 学分 · ${row.类别}`,
      ];
      bindChartTooltip(circle, row.课程, tooltipLines);
      circle.addEventListener("focus", () => circle.setAttribute("opacity", "1"));
      circle.addEventListener("blur", () => circle.setAttribute("opacity", "0.78"));
      svg.append(circle);
    });
    container.append(svg);
  }

  function setupScatterFullscreen() {
    const card = $("#score-rank-card");
    const button = $("#scatter-fullscreen");
    const downloadButton = $("#scatter-download");
    let preparedPngUrl = "";

    const preparePng = async () => {
      downloadButton.setAttribute("aria-disabled", "true");
      downloadButton.textContent = "生成中";
      const source = $("#score-rank-scatter svg");
      const clone = source.cloneNode(true);
      const viewBox = source.viewBox.baseVal;
      const exportScale = Math.max(2, Math.min(3, 3600 / viewBox.width));
      clone.setAttribute("width", viewBox.width);
      clone.setAttribute("height", viewBox.height);
      clone.setAttribute("xmlns", svgNS);

      const style = svgElement("style", {}, `
        svg { font-family: "Microsoft YaHei", "Noto Sans CJK SC", sans-serif; }
        .tick-label { fill: #737c82; font-family: Consolas, monospace; font-size: 11px; }
        .grid-line { stroke: #e8ebe9; stroke-width: 1; }
        .quadrant-line { stroke: #7d878c; stroke-dasharray: 4 4; stroke-width: 1; }
        .quadrant-label { fill: #7b858a; font-size: 10px; letter-spacing: 0.02em; }
        .scatter-label-line { stroke: rgba(75, 87, 93, 0.34); stroke-width: 0.8; }
        .scatter-course-label { fill: #34434a; stroke: #ffffff; stroke-width: 3px; paint-order: stroke; font-size: 10px; }
      `);
      clone.prepend(style);

      const svgBlob = new Blob([new XMLSerializer().serializeToString(clone)], { type: "image/svg+xml;charset=utf-8" });
      const svgUrl = URL.createObjectURL(svgBlob);
      const image = new Image();
      image.src = svgUrl;
      await image.decode();

      const canvas = document.createElement("canvas");
      canvas.width = Math.round(viewBox.width * exportScale);
      canvas.height = Math.round(viewBox.height * exportScale);
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
      const expanded = isScatterExpanded();
      button.textContent = expanded ? "退出全屏" : "全屏";
      button.setAttribute("aria-label", expanded ? "退出课程成绩与教学班排名百分位全屏" : "全屏查看课程成绩与教学班排名百分位");
      renderScatter();
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
  }

  function renderCharts() {
    pinnedTooltipTarget = null;
    hideTooltip();
    renderCreditDonut();
    renderGradeDistribution();
    renderTrendCharts();
    renderCategoryGpa();
    renderScatter();
  }

  renderOverviewTable();
  renderDetailsHead();
  setupFilters();
  updateSortIndicators();
  renderDetailsBody();
  setupScatterFullscreen();
  renderCharts();

  document.addEventListener("pointerdown", (event) => {
    if (pinnedTooltipTarget && !event.target.closest("[data-chart-tooltip]")) {
      pinnedTooltipTarget = null;
      hideTooltip();
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
