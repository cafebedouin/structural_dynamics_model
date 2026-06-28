# OQ-118 re-probe (2026-06-27) — witness behind the ruling object

Four read-only re-probes (no spend) over the 2026-06-12 cohort-zero replicate draws, run to
discharge two banked-but-unwitnessed claims before the OQ-118 ruling lands. The witness is the
re-runnable script, not its printed table:

    python3 audits/2026-06-27_oq118_reprobe/oq118_reprobe.py

Raw output frozen alongside in `oq118_reprobe.out`.

## What each probe settled

- **Probe 1 — `suppression` is authored, not computed.** Present as `0.12` in the raw
  pre-pipeline draw; required by the generation schema (`python/shared/schemas.py:184`). Positive
  control: computed fields (`chi`/`dr_type`) are absent from the raw draw's keys, so
  "present-in-raw-draw" discriminates authored from computed. → broad-A's scalar witness is real.

- **Probe 2 — `emerges_naturally` is a degenerate constant.** `True` in 18/18 draws across all 6
  stories: zero between-story variance, so its 6/6 "stability" witnesses nothing. `suppression`
  by contrast varies between stories (0.71/0.38/0.22/0.22/0.12) yet reproduces within-story 5/6
  (printing_press the lone escapee, matching the distance-metric signature).

- **Probe 3 — σ/seat is not the stability partition, and it INVERTS under decontamination.**
  Arm A (presence-hollow IN) reproduces the instrument's original `58/62 | 36/32`, 47.9%,
  p=0.6490 — validating the comparator/bucket map by reproduction. Arm B (presence-hollow OUT of
  both arms) drops to 39.7%, p=0.2348: the σ-predicted side decontaminates *toward* the unstable
  cast multisets. This inversion — not the bare p-value — is the narrow-A headline witness.

- **Probe 4 — between-story variance sweep (degeneracy positive-control on the stable side).**
  Caught three degenerate "stable" padders (`emerges_naturally`, `claimed_type`,
  `has_sunset_clause`) plus `omegas.count` (range 0.00) on the *seat* side. Confirmed
  `extractiveness` is **known-flagged** (input, no blind credit) → out of the generated witness
  set, same as claimed_type. The surviving **generated** authored-scalar witnesses are
  `suppression` (5/6), `accessibility_collapse` (4/6), `theater_ratio` (3/6), `resistance` (3/6) —
  all non-degenerate, all clearing the cast multisets' 0/6. This is a **floor contrast** (every
  authored scalar reproduces above the cast's zero), NOT a within-class gradient: stability and
  between-story spread do not co-vary across the four (accessibility_collapse has the smallest
  spread, 0.14, at middling stability 4/6).

## Bearing on the ruling

- **narrow-A discharges** on Probe 3B's inversion (σ/seat falsified as a stability predictor, not
  as theory; robust to removing presence-hollow contamination).
- **broad-A stays OPEN** as a hypothesis carried by the floor contrast (Probe 4). Its graduation
  owes a pre-registered construction-type partition whose positive-control burden **includes the
  variance/degeneracy sweep**, plus the verdict-class temperature sweep
  (`disappearance_verdict`/`founding_problem_status`, the OQ-75-load-bearing content class).
