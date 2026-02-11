# Font Refactor to Geist Mono

## TL;DR

> **Quick Summary**: Replace all existing fonts with Geist Mono to create a unified, modern monospace aesthetic for the entire site.
> 
> **Deliverables**:
> - Updated `templates/default.html` with Geist Mono Google Fonts import.
> - Refactored `css/default.css` replacing all `font-family` declarations.
> - Preserved KaTeX math font integrity.
> 
> **Estimated Effort**: Short
> **Parallel Execution**: NO - sequential updates to template then CSS.
> **Critical Path**: Template Update → CSS Refactor → Verification

---

## Context

### Original Request
"bro i think we need to change fonts in this project. i think for personal site of developer its much more appealing to use monospace fonts because it looks good it works good with tematics etc. can u plan on good refactor of fonts in this project to use good very good monospace font for programmers in this site"

### Interview Summary
**Key Discussions**:
- **Font Choice**: Geist Mono (modern, sleek, high legibility).
- **Scope**: Entire site (Headings, Body, Nav, Meta, Code).
- **Delivery**: Google Fonts for ease of maintenance.

**Research Findings**:
- Current site uses a mix: `Bricolage Grotesque` (Sans), `DM Serif Display` (Serif), and `JetBrains Mono` (Mono).
- Implementation is via Vanilla CSS and Hakyll templates.
- KaTeX is used for math, requiring specific font protection.

### Metis Review
**Identified Gaps** (addressed):
- **KaTeX Conflict**: Ensured global overrides do not break KaTeX math symbols.
- **Font Weights**: Importing full variable range (100..900) for flexibility.
- **Optimization**: Removing `JetBrains Mono` from Google Fonts import but keeping it as a local CSS fallback.

---

## Work Objectives

### Core Objective
Unify the site's typography using Geist Mono while maintaining readability and technical stability.

### Concrete Deliverables
- `templates/default.html`: Updated Google Fonts `<link>`.
- `css/default.css`: Unified `font-family` declarations.

### Definition of Done
- [ ] Geist Mono is the primary font for body, headings, and navigation.
- [ ] No references to `Bricolage Grotesque` or `DM Serif Display` remain.
- [ ] KaTeX math formulas still render correctly with their native fonts.
- [ ] All tests pass without visual regression (captured in screenshots).

### Must Have
- Geist Mono variable weight support (100-900).
- Fallback to `JetBrains Mono` then generic `monospace`.
- Protection for `.katex` elements.

### Must NOT Have (Guardrails)
- Do NOT remove KaTeX imports.
- Do NOT modify `css/syntax.css` logic (only its font inheritance).
- Avoid "font-slop": No orphaned font declarations.

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
> ALL verification is executed by the agent using tools. No exceptions.

### Test Decision
- **Infrastructure exists**: NO
- **Automated tests**: None (Agent QA only)
- **Framework**: none

### Agent-Executed QA Scenarios (MANDATORY)

Scenario: Verify Font Import Success
  Tool: Bash (curl)
  Preconditions: None
  Steps:
    1. curl -sI "https://fonts.googleapis.com/css2?family=Geist+Mono:wght@100..900&display=swap"
    2. Assert: HTTP status is 200
  Expected Result: Google Fonts API returns successful response
  Evidence: curl output

Scenario: Verify Global Font Application
  Tool: Playwright (playwright skill)
  Preconditions: Local dev server running (if possible) or static build inspection
  Steps:
    1. Open `_site/index.html` (or live server)
    2. Check computed style of `body`, `h1`, `nav a`
    3. Assert: `font-family` includes "Geist Mono"
    4. Screenshot: `.sisyphus/evidence/font-refactor-global-check.png`
  Expected Result: All elements use Geist Mono
  Evidence: .sisyphus/evidence/font-refactor-global-check.png

Scenario: Verify KaTeX Integrity
  Tool: Playwright (playwright skill)
  Preconditions: Page with math content loaded
  Steps:
    1. Navigate to a page with math (e.g., search for `.katex` element)
    2. Check computed style of `.katex`
    3. Assert: `font-family` is NOT Geist Mono (should be KaTeX fonts)
    4. Screenshot: `.sisyphus/evidence/font-refactor-katex-check.png`
  Expected Result: Math symbols render with correct math fonts
  Evidence: .sisyphus/evidence/font-refactor-katex-check.png

Scenario: Verify Cleanup
  Tool: Bash (grep)
  Preconditions: Changes applied
  Steps:
    1. grep -Ei "Bricolage|DM Serif" css/default.css
    2. Assert: Zero matches
  Expected Result: No old font references remain
  Evidence: grep output

---

## Execution Strategy

### Sequential Tasks
1. Update Template Imports
2. Refactor CSS Declarations
3. Final Verification

---

## TODOs

- [x] 1. Update Google Fonts Import in `templates/default.html`

  **What to do**:
  - Replace the existing `<link>` at line 10 with the new Geist Mono import.
  - New Import: `<link href="https://fonts.googleapis.com/css2?family=Geist+Mono:wght@100..900&display=swap" rel="stylesheet">`
  - Note: Removing `Bricolage Grotesque`, `DM Serif Display`, and `JetBrains Mono` from the import to reduce bundle size (JetBrains Mono will remain as a local fallback in CSS).

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Single line replacement in a template file.
  - **Skills**: [`git-master`]

  **Acceptance Criteria**:
  - [ ] `<link>` updated correctly.
  - [ ] `curl` check for new font URL returns 200.

- [x] 2. Refactor Font Declarations in `css/default.css`

  **What to do**:
  - Replace all occurrences of `'Bricolage Grotesque', sans-serif` and `'DM Serif Display', serif` with `'Geist Mono', 'JetBrains Mono', monospace`.
  - Update `font-weight` for `body` to `400` (from `500`) to better suit Geist Mono's weight distribution.
  - Ensure specific overrides (like `h5`, `h6`, `.post-date`) are also updated.
  - IMPORTANT: Do not add a global `font-family` to `*` to avoid breaking KaTeX. Keep the overrides on specific elements/classes as currently structured, but unify the font name.

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: Multiple string replacements across a single file.
  - **Skills**: []

  **Acceptance Criteria**:
  - [ ] `grep` check returns no old font names.
  - [ ] `body` has `font-family: 'Geist Mono', 'JetBrains Mono', monospace;`.

- [x] 3. Protect KaTeX Fonts (Safety Check)

  **What to do**:
  - Explicitly add a CSS rule to ensure KaTeX elements use their intended fonts, just in case of inheritance issues.
  - Rule: `.katex { font-family: KaTeX_Main, 'Times New Roman', serif !important; }` (and other KaTeX classes if needed).

  **Recommended Agent Profile**:
  - **Category**: `quick`

  **Acceptance Criteria**:
  - [ ] KaTeX elements render correctly in Playwright check.

- [ ] 4. Final Verification and Evidence Capture

  **What to do**:
  - Run all QA scenarios.
  - Capture screenshots and terminal outputs.

  **Recommended Agent Profile**:
  - **Category**: `visual-engineering`
    - Reason: Requires Playwright for visual verification.
  - **Skills**: [`playwright`]

  **Acceptance Criteria**:
  - [ ] All 4 QA Scenarios pass.
  - [ ] Evidence files saved to `.sisyphus/evidence/`.

---

## Success Criteria

### Verification Commands
```bash
grep -Ei "Bricolage|DM Serif" css/default.css # Expected: 0 matches
curl -sI "https://fonts.googleapis.com/css2?family=Geist+Mono:wght@100..900&display=swap" | grep "HTTP/2 200" # Expected: Match
```

### Final Checklist
- [ ] Geist Mono is primary across all site sections.
- [ ] Math formulas (KaTeX) are visually intact.
- [ ] Code syntax highlighting still works with mono font.
- [ ] Old font imports and CSS rules are removed.
