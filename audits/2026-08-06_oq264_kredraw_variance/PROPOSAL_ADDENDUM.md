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

## 4. Phase C pre-registration — AT Fiat k=3, REPRODUCE-RATE ONLY (operator ruling 2026-08-06)

Committed BEFORE any call. Operator ruling (this session): no additional Biopower share
draws (the share observable is denominator-confounded per §1 — more draws measure the
confound more precisely); spend ONLY on AT Fiat reproduce-rate, closing the 2026-08-05
ruling's standing rider ("AT Fiat has NO Arm-0 measurement"); then standard-only closure.

**Runs (serial, ≤ 3 + 1 retry allowance for API failure only, never a re-roll;
≈ 34K input tok each, ~102K total):**

```bash
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/AT Fiat K - Michigan 2026 BCFP.md"
```

**Per-run HALT conditions (any one halts the arm):** input md5 ≠ `8d2224c8…` (the
`1bd57a84` baseline, re-checked immediately before each run); corpus write (listing-diff
on `prolog/testsets/` or `json/` non-empty vs the pre-run snapshot); any `*_brief.md`
appears beside the source; chunking/windowing in the log instead of whole-doc
single-prompt ingest.

**Observable (sole gate-relevant measurement):** per re-run reproduce-rate = (baseline
readings with a subject+stance match among the re-run's kernel readings + selected
axes) / 6, name-blind — the ADDENDUM §4 method verbatim. **Pinned AT Fiat baseline
reading set** (from the frozen `fiat_efficacy_kernel_2026_20260803_102258`; D = 6, all
kernel-linked; deferred `ethical_localism_trap` excluded):

1. fiat efficacious by EMPIRICAL PRECEDENT — small student/activist actions historically
   produced large-scale outcomes (BDS / Vietnam / Chicago School cases);
2. fiat efficacious as KNOWLEDGE-GENERATION — research + public academic engagement
   ("scholarship of consequence") reshape the conditions of social problems (Galea);
3. fiat efficacious as TRUTH-PROCEDURE — axiomatic prescription ("healthcare belongs to
   everyone") forces the State to rewrite its norms from outside its own logic
   (Badiou/McGee);
4. fiat efficacious as PREDICTIVE SYNTHESIS — disciplined hypothetical policy
   imagination as political theory's proper method (Bagg / Deweyan pragmatism);
5. fiat efficacious as EMPATHY SIMULATION — simulating/testing hypothetical policy
   builds real empathy and attitude change in participants (Mauri / HCI);
6. fiat efficacious as UTOPIAN FICTION / social criticism — debate as educational game,
   debaters as budding social critics working the public agenda (McGee & Romanelli).

A zero-kernel re-run is its own categorical outcome (kernel-minting churn), scored
against the selected-axes fallback for the match count but reported categorically —
same rule as §2 of PROPOSAL.md.

**Scope (firewalls, pre-committed):** AT Fiat feeds NO share number (the share
instrument is closed by the ruling; its D≈3–6 lattice is coarser regardless). It also
CANNOT serve OQ-259 item 3's genre-flag replication — item 3 requires a second
independent ARSENAL, and AT Fiat is a single-voice meta-layer answers file; any
genre-flag-shaped omega in the re-runs is recorded descriptively, non-gating. The
share-pooling retraction protocol (plan Phase C) is moot: no share gate is extended.
Results land in `READOUT_atfiat.md`; closure follows in WRITEUP.md + ISSUES.md.
