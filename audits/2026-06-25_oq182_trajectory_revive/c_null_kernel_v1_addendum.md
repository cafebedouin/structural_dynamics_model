# kernel_v1 C-null breadth leg — ATTEMPTED, did NOT complete (addendum, not a gate)

**Date:** 2026-06-26. Optional breadth addendum to the OQ-182 family-product close
(operator: "worth running but does NOT gate the close"). **No result claimed.**

## What happened
Ran the frozen C-null harness (`c_null_harness.pl`, seed 20260625, unchanged) with a
`corpus_path` overlay to `archives/datasets/kernel_v1` (1106 stories). It completed
**setup only**: 611,065 pair distances computed, 53 shift groups
(`c_null_kernel_v1.stderr.log`), then produced **no null distribution** — stdout ends
at the weights line, 0 result tokens (`c_null_kernel_v1.log`, 20 lines). Process exit
0 but `c_null_run` did not reach the RealSil / P95 / verdict output.

## Reading
The harness was **frozen for the testsets/ leg (N≈97)** and does not complete on the
~11× larger breadth corpus (N=1106) without adaptation (the per-component null draws /
silhouette stage over 1106 clustered points either failed silently or exceeded a
resource bound after the setup phase). The cosmetic stderr is only `discontiguous`
clause warnings from loading the kernel_v1 archive — not the cause.

## Disposition
**Not pursued further this session** — it is an addendum the operator de-prioritized,
and the family-product close does not depend on it (validated already by C-null on
testsets/ +5.01σ and the C-gen substrate TRACK=162/162=1.000). Adapting the frozen
harness to the breadth corpus (or sampling it) is a separate, optional task; recorded
here so a cold reader does not mistake the attempt for a passed breadth leg.
