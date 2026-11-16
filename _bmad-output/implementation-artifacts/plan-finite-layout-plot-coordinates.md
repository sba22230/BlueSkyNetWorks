---
title: 'Prevent graph rendering failure from invalid layout coordinates'
type: 'bugfix'
ticket: ''
created: '2026-11-16'
status: 'in-progress'
route: 'oneshot'
route_source: 'auto'
review: ''
review_source: 'auto'
lenses_ran: []
review_loop_iteration: 0
context: []
---

<frozen-after-approval reason="human-owned intent — do not modify unless human renegotiates">

## Intent

**Problem:** `render_one_graph` passes layout coordinates directly to `plot.igraph`; a matrix with invalid dimensions or non-finite values can cause `need finite 'xlim' values` and abort the `rxExec` batch.

**Approach:** Validate each layout matrix before plotting. Recompute invalid coordinates with `igraph::layout_nicely`, and stop with a layout-specific diagnostic if valid coordinates still cannot be produced.

</frozen-after-approval>

## Implementation Notes

One-shot is appropriate because this is a focused, single-file guard and fallback under the 100-line threshold.

## Verification

**Commands:**
- `Rscript -e "parse(file = '3_build_graph_and_metrics.R')"` -- expected: parses without error.
- Run a focused R check with a small igraph graph and malformed coordinates -- expected: invalid coordinates are replaced by finite 2D coordinates, while valid coordinates are retained.
