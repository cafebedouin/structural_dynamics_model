# OQ-75(b) grain-sensitivity precursor probe — WRITEUP

Executed 2026-07-04, per the pre-registration (`PROPOSAL.md`, written before any run).
Instrument: `python/audits/oq75b_grain_probe.py`. Raw: `pairs_A*.tsv`, `run_*.log`,
`registry_A*.pl` (unratified overlay arms; the canonical registry was never edited).

## Headline: grain throw on this substrate is LARGE, and asymmetric by direction

```
arm            cells   conversions  C2 co-slot  verdicts
A0-baseline       47         10/10         3/3  {key_fragile: 26, robustly_undersampled: 16}
A1 coarsen-max    42         10/10         3/3  {key_fragile: 38, robustly_undersampled: 4}
A2 coarsen-2      41         10/10         3/3  {key_fragile: 34, robustly_undersampled: 8}
A3 refine-arb     21          9/10         0/3  {key_fragile: 12, robustly_undersampled: 30}
A4 refine-limit    0   (== exact_name; all-blind by prior corpus-wide witness, cited)
```

Cross-instrument check: A0's 47 cells == the OQ-72 Phase-5 sweep's 47 CELL rows
(independent instrument, same substrate) — the probe's baseline is consistent, not
self-normed.

- **Refinement direction (the fragile one):** ONE deterministic arbitrary split step
  (A3) halves the cells (47→21), kills a kernel's conversion entirely (tordesillas —
  its single 2-occupant cell separated), and **zeroes contradiction-pair co-slotting
  (3/3 → 0/3)**. The refinement limit (A4) is total collapse, known a priori.
- **Coarsening direction:** alignment MASS can only grow (theorem — merging atoms
  merges vantages; disclosed pre-run, not sold as finding). But the pre-registered
  CELL-COUNT observable is **non-monotone**: 47→42 under coarsen-max, by vantage
  consolidation. Witnessed row (moral_causation, dispositional×situational):
  A0 `1 agree + 1 disparity` (two vantages) → A1 `0 agree + 1 disparity` (one merged
  `__all` vantage; the pooled grounding sets differ). Fewer cells, MORE alignment.
- **Verdict distribution is grain-labile in both directions:** key_fragile 26→38
  (coarsen) and →12 (refine); robustly_undersampled 16→4→30.

## What this feeds the §7.1 correlation-statistic spec (the probe's purpose)

1. **Grain normalization is load-bearing** (the pre-registered interpretation grid's
   "large throw" branch). Expectation recorded for ruled Stage 1: the unratified arms
   will likely swing, so the staged escalation to ratified arms will likely be needed —
   an expectation, not a verdict.
2. **The invariance measure must not be a raw cell/vantage count** — witnessed
   non-monotone in grain (47→42 under a move that strictly increases alignment). Use
   pair-level alignment mass or a normalized ratio; a cell-count statistic would read
   coarsening as "less invariance," the wrong sign.
3. **Anything leaning on contradiction pairs inherits maximal refinement fragility**:
   co-slotting went 0/3 after one arbitrary split (the C2-style read is grain-brittle
   downward, grain-immune upward — coarsening can never separate a pair).
4. **`ax_stability_verdict` is not grain-stable** and should not be aggregated across
   registries of different grain without a grain stamp.

## Controls (all fired)

- Overlay-took-effect: per arm × leg, in-image fact count == arm-file count (runner
  halts otherwise; all runs passed). A1 atom-set == exactly the ten `<kernel>__all`
  atoms (checked, passed).
- Known-changer: A1 merged digital_money's two baseline slots (checked, passed) — the
  perturbation pipeline demonstrably changes what it should before any "stable" reading
  could be trusted.
- A0 external consistency: 47 == 47 vs the OQ-72 sweep (above).

## Scope limits (stated, not buried)

Pilot substrate only (10 kernels, 42 pairs, tranche-1 registry). A3 is the *maximally
unratified* refinement (alphabetical split); a ratified refinement would split by
subject-merit and could preserve more structure — which is exactly the Stage-1→Stage-2
disambiguation the ruling pre-registered. This probe does NOT discharge the ruled
Stage-1 check: that tests the correlation statistic, which remains unbuilt; this
measured the partition layer it will be built on.
