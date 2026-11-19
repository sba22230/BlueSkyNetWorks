---
title: 'Prevent graph rendering failure from invalid layout coordinates'
type: 'bugfix'
ticket: ''
created: '2026-11-16'
status: 'built'
route: 'oneshot'
route_source: 'auto'
review: 'quick'
review_source: 'auto'
lenses_ran: ['quick']
review_loop_iteration: 0
context: []
baseline_revision: 'deb10d04dcf9dbcc11f3dcc1f3d0a5749d835780'
---

<frozen-after-approval reason="human-owned intent — do not modify unless human renegotiates">

## Intent

**Problem:** `render_one_graph` passes layout coordinates directly to `plot.igraph`; a matrix with invalid dimensions or non-finite values can cause `need finite 'xlim' values` and abort the `rxExec` batch.

**Approach:** Validate each layout matrix before plotting. Recompute invalid coordinates with `igraph::layout_nicely`, and stop with a layout-specific diagnostic if valid coordinates still cannot be produced.

</frozen-after-approval>

## Implementation Notes

One-shot is appropriate because this is a focused, single-file guard and fallback under the 100-line threshold.

The planned layout validation and fallback are already present in `3_build_graph_and_metrics.R` at the resumed baseline revision; no source changes were needed. R parsing passed, and focused checks confirmed valid coordinates are retained, non-finite coordinates are replaced, and an empty graph produces the expected layout-specific error.

## Verification

**Commands:**
- `Rscript -e "parse(file = '3_build_graph_and_metrics.R')"` -- expected: parses without error.
- Run a focused R check with a small igraph graph and malformed coordinates -- expected: invalid coordinates are replaced by finite 2D coordinates, while valid coordinates are retained.
