# Repository Guidelines

## Project Structure & Module Organization
This repository is a static personal website. The root [`index.html`](/mnt/c/Users/Newton/Documents/yykspace.com/YinkaiYu.GitHub.io/index.html) redirects to the English site in `en/`; Chinese pages live in `cn/`. Shared styles and scripts are in `css/` and `js/`, and images/favicon assets are in `img/`. Content-heavy sections are organized by topic: `publications/`, `life/`, `scholar/`, and `share/`. A standalone research codebase with its own `Makefile` lives in `scholar/DQMC/code_DQMC/`.

## Build, Test, and Development Commands
There is no repo-wide build system or package manager.

- `python3 -m http.server 8000`
  Serve the site locally from the repository root for browser testing.
- `xdg-open http://localhost:8000/` or open the URL manually
  Verify navigation, asset paths, and language switching.
- `make` in `scholar/DQMC/code_DQMC/`
  Build the DQMC Fortran program only if you are working in that subdirectory and have its MPI/Intel toolchain.

## Coding Style & Naming Conventions
Preserve the existing hand-written static style: HTML/CSS/JS files use tabs heavily, simple relative paths, and minimal tooling. Keep file names lowercase and descriptive, matching current patterns such as `en/share.html`, `cn/nav.js`, and `img/portrait.png`. When updating shared navigation or mirrored content, keep `cn/` and `en/` pages aligned unless the difference is intentional.

## Testing Guidelines
There is no automated test suite in this repository. After changes, load the affected pages in a browser and check:

- internal links and language toggles
- image/PDF downloads and relative asset paths
- layout regressions in `css/main.css` consumers

For DQMC code changes, use the subproject’s own compile flow and any domain-specific validation you maintain outside this website repo.

## Commit & Pull Request Guidelines
Recent history uses short, direct commit subjects, often in Chinese, for example `更新publications`, `修复语言切换按钮`, and `更正doi`. Follow that style: one concise imperative summary per commit. Pull requests should state the affected section, describe user-visible changes, link related issues if any, and include screenshots for page/layout updates.
