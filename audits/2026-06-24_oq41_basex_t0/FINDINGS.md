# OQ-41 rows 24–25 — the `BaseX=0.5` default at Time=0 is OFF-GRID PROBING, not absence

**Date:** 2026-06-24. **Corpus:** live `testsets/`, n=97 (manifest `pipeline_run_at`
2026-06-24). **Trigger:** Pass-1B of the OQ-37..41 census plan proposed replacing the
`BaseX = 0.5` impute in `drl_composition.pl:classify_at_time_with_supp` with a fail-closed
`unknown` per OQ-44. Operator approved adoption *conditional on* a per-kernel inspection
confirming the fix removes a fabrication of a genuinely-absent ε. The inspection **failed
that premise**. This audit records why, and reverts 1B.

## What the plan assumed

The plan (item B) characterised the `BaseX=0.5` site as "reachable only via the dormant
`constraint_history` path, so applying OQ-44 changes nothing live," and the OQ-41 source
characterised rows 24–25 as REACHABLE-BUT-LOCKED with a witness of "0 default-fires at t=0."

## What the substrate shows

1. **The site is LIVE, not dormant.** `cs_kernel_registry` calls `classify_at_time/4` at
   `Time=0` (lines 67, 68, 101) to compute kernel divergence, which feeds pipeline
   `validation.cs_kernel_*`. `/4 → /5 → classify_at_time_with_supp/7` reaches the
   `BaseX=0.5` branch directly — same predicate path, no dormant `constraint_history`
   in between. (Falsifier the operator named — a dual-arity that misses line 238 —
   resolved AGAINST: it is one path.)

2. **Fail-closing IS output-changing.** Pipeline diff (fail-closed vs baseline):
   `cs_kernel_divergence_count` 17→16, `cs_kernels_with_divergence` 9→8. Exactly one
   kernel flips: `jewish_sovereignty_palestine`, `diverging_pair_count` 1→0
   (`settler_colonial` vs `cultural_zionist`: jaccard 0.0/diverge-156 → jaccard
   1.0/agree-156, `robust_context_count` 0→156).

3. **The ε is NOT genuinely absent — it is OFF-GRID.** All 15 constraints that hit the
   `0.5` default at `Time=0` author `base_extractiveness` as a temporal series at real
   historical times, NONE at `Time=0`; **0 of 15 are genuinely ε-absent** (`gen15.pl`):
   - `settler_colonial`: [1917→0.6, 1948→0.8, 1967→0.85, 1993→0.88, 2024→0.9]
   - `cultural_zionist`: [1900→0.1, 1948→0.15, 1967→0.12, 2000→0.14, 2024→0.15]
   - others span 480 BC (Lycurgan), 1450 (press/Reformation), 1700 (treaty), etc.
   `cs_kernel_registry` probes `Time=0`, which is before every story's authored grid.

4. **The divergence the fix erases is REAL.** At every authored time, `settler_colonial`
   classifies `snare` and `cultural_zionist` classifies `scaffold` (`realtype.pl`, T=1948
   and T=2024). The kernel genuinely diverges. The `0.5` default happened to PRESERVE this
   (settler→snare via the fabricated 0.5, cultural→unknown); fail-closing makes BOTH
   `unknown` → spurious agreement → the kernel reads as fully-robust (156/156) when both
   readings are actually unmeasured-at-t0. **Fail-closing is strictly worse here: it
   replaces a true divergence with a false agreement.**

## Verdict

Neither value is correct at `Time=0` for these stories:
- `BaseX=0.5` impute → a fabrication (manufactures a χ from an unauthored-at-t0 value).
- fail-closed `unknown` → discards authored series data and ERASES real divergences.

The actual defect is **off-grid probing**: `cs_kernel_registry` (and any consumer that
queries `classify_at_time` at a fixed synthetic `Time=0`) asks for a snapshot at a time on
no temporal-series story's grid. The OQ-44 "fail-closed-on-absence" policy does not apply —
there is no absence. 1B is **REVERTED**; the disposition needs an operator ruling on the
real fix (resolve off-grid `Time` to an on-grid value — earliest/latest authored, or scalar
fallback — vs. have `cs_kernel_registry` probe at a story-valid time). This is also the
OQ-105 grid-misalignment family.

A SECOND defect is exposed regardless of the t0 fix: `cs_kernel_comparison` counts
`unknown==unknown` as agreement/robustness (Build-Discipline Pattern 6 — measured-empty
vs both-unmeasurable collapse at the aggregation boundary). Worth its own OQ.

## Artifacts (this dir)
- `probe_basex_t0.pl` — the 15 constraints hitting the t0 default (count + kernels).
- `gen15.pl` — proof all 15 are off-grid series, 0 genuinely absent.
- `confirm_jsp.pl` — the two JSP readings' full ε series + suppression.
- `realtype.pl` — real types at authored times (snare vs scaffold, persistent divergence).
- `control_basex.pl` — positive/negative control for the (reverted) fail-close arm.
