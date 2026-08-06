# PROPOSAL ADDENDUM — post-checkpoint operator review riders (pre-registered, zero spend)

Date: 2026-08-06, same session, AFTER `507e21ce`/`c5b7ca22` (Phase B complete, checkpoint
open) and BEFORE any Phase-C decision. Trigger: two operator reviews of PHASE0_REPORT.md.
This addendum registers three free computations and two reporting corrections. It is
committed BEFORE the computations run. Nothing here changes the committed gate verdict,
the committed calls, or the committed bands.

## 1. Denominator-convention sensitivity table — EXPLORATORY, NON-GATING

**Motivating observation (from the committed compute output, no new data):** TAG = 3, 3, 4
over D = 6, 4, 6 — the max-pairwise share Δ (0.500 → 0.750, the entire 0.25 range) falls
between two draws with IDENTICAL numerator (TAG = 3). The measured spread is produced by
the unit population churning at fixed judgment, not by the judged layer. The pooled
observable inherited per-reading churn through its denominator (D = kernel readings ∪
selected axes IS built from per-reading identity), with a sign flip: a draw that mints
fewer readings scores as MORE tag-idiomatic.

**Registered computation:** recompute the three Biopower draws (and the Cap triple as
contrast, zero-kernel exclusion unchanged) under alternative denominator conventions:

- A. per-draw D (the committed convention) — for the record;
- B. fixed baseline D (= 6);
- C. kernel-readings-only D;
- D. selected-axes-only D;
- E. raw TAG count (no denominator).

Report per-draw values and the range under each, which pair carries the max Δ, and
whether that pair is numerator-identical.

**Non-gating declaration (the point of pre-registering):** this table is exploratory.
The committed gate verdict is NOT recomputed under any alternative convention; no
convention is promoted because it "looks best"; the deliverable is the sensitivity table
itself — how much of the measured floor a future audit inherits from its own denominator
choice. Any convention change for future audits is a design decision for the minted
standard, made at closure, not a re-gate of this data.

## 2. Cap concordance check — free drift probe (weak, stated at its altitude)

Both 08-05 Biopower draws sit above the 08-03 baseline on share. Registered check: for
each mechanical observable (n_kernel_readings, n_selected_axes, n_deferred_axes) and for
share, tabulate the base→r1 and base→r2 direction for BOTH files. Interpretation rule,
fixed now: concordant cross-file direction on mechanical observables = cheapest available
evidence that the cross-day residue is real drift (which would also mean pooled k=6–8
anchors were never pooling same-input draws); discordant or mixed = no support for drift
beyond noise (drift NOT excluded — the check is weak either way at n=3 per file).

## 3. Reporting corrections (applied to PHASE0_REPORT.md as a dated correction block)

- **Dual-rule statement moves to the headline:** observed range 0.25 is PASS(sens1)
  under the rev-2 (recalibrated) modifier and INDETERMINATE under the plan's rev-1
  modifier; the modifier was recalibrated in Phase A on simulation grounds (CALIBRATION.txt),
  before any scoring and for reasons unrelated to this data — but the observed value
  landed exactly on the boundary the recalibration made passable, and that belongs in
  the headline, not the methods.
- **Duplicate agreement is a bound, not a zero:** 6/6 agreement gives a one-sided 95%
  binomial upper bound on the per-item disagreement rate of 1 − 0.05^(1/6) ≈ 0.393. The
  "measured-zero scorer variance" clause that lets a sens-1 PASS stand therefore rests
  on an interval consistent with an error rate up to ~39%, any of which is enough (at
  sensitivity 1) to have produced the pass. Reported as such wherever the clause is cited.
