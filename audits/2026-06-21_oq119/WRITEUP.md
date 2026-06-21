# OQ-119 — Does feeding move the join? WRITEUP

**Date:** 2026-06-21. **Spend:** 96 Sonnet generations (5 kernels × 16 readings × {withheld,fed} ×
3 draws), parties-fixed fed framing. **Frozen prediction:** `../2026-06-21_oq119_gate0/PREDICTION.md`
(committed pre-draw). **Machine output:** `RESULTS.md`. **Join records:** `join_{arm}_d{n}.json`.

## Answer: feeding moves the join — but on the DIAGNOSTIC verdict layer, NOT the committer structure

Applying the frozen per-axis rule (`median(D_A) > max(F_A)`; observer/rate-magnitude de-weighted as
SOFT; committer/temporal-sign/verdict HIGH-information):

| axis | moved (of 5 kernels) | class | reading |
|---|---|---|---|
| **verdict** (sig_grade / alerts / headline) | **4 / 5** | HIGH | feeding escalates the false-foundational signature |
| **committer** (obstruction_status / divergence) | **0 / 5** | HIGH | **Theorem-7 holds — feeding does not move it** |
| temporal sign-flip | 1 / 5 | HIGH | only waitangi |
| observer χ spread | 2 / 5 | SOFT | labile, as predicted |
| temporal rate magnitude | 3 / 5 | SOFT | labile, as predicted |

**Headline: JOIN MOVES** — carried entirely by HIGH-information axes (verdict 4/5, temporal-sign 1/5),
**not** by the SOFT observer axis (which moved in only 2/5 and, per the de-weighting, could not carry
the verdict). The observer de-weighting (Claude Web point 2) was load-bearing: a summed-scalar headline
would have mixed the labile observer wobble into the result; the per-axis rule isolates where the
movement actually lives.

## What moved, concretely (acceptable_risk_energy, witnessed)

- **withheld** (3 redraws): every reading `yellow / commentary / 1 alert` — stable.
- **fed** (3 redraws): the contested readings became `yellow / **correction** / **2 alerts**`.

Feeding the mountain claim — while holding parties fixed, so the authored ε/suppression stay high —
forces the `MOUNTAIN_METRIC_CONFLICT` seat divergence, which routes through the `false_natural_law`
signature and **escalates the verdict-join from commentary to correction grade**. The diagnostic
cross-examination *responds to the fed claim* even where the broad type holds (verdict_joined stays
`yellow` for most readings; the change is in grade + alert count, the OQ-98 join layer).

This is the OQ-119 question answered YES *for the verdict layer*: feeding moves the join (which alerts
fire, at what grade) without needing to move the type — exactly the sharper-than-OQ-117 effect the
question posited. **Honest caveat:** this movement is substantially the *claim-gated* path — the FNL
signature is gated on the authored mountain claim, so feeding the claim activating it is semi-expected.
The non-trivial, observer-blind result is the committer axis below.

## What did NOT move: the committer structure (Theorem-7 detection-independence)

`obstruction_status` / divergence-scopes did not move under feeding on any kernel (0/5). The honest
form of this is **"no effect beyond generation noise"**: the *withheld* redraws already flip
`real_closure ↔ licensed_plurality` (e.g. acceptable_risk withheld d1 = real_closure, d2/d3 =
licensed_plurality), so the committer floor `max(F)` is itself nonzero on 4/5 kernels — and the
fed-vs-withheld committer distance does not exceed it. On the one kernel where the committer floor is
clean (`ai_governance_legitimacy`, withheld committer F = [0,0,0]), feeding moves it **exactly zero**
(D_nonzero = 0/9) — a noise-free confirmation.

**This is the predicted Theorem-7 result, now MEASURED rather than assumed:** the authored
`cs_reading_relation` edges are observer-blind by construction, so feeding an observer-side
foundational claim does not re-author which sibling readings foreclose. No detection-independence
violation. (Had obstruction moved cleanly under feeding, the frozen prediction flagged it as a
top-line violation — it did not.)

A second-order finding worth its own thread: the committer axis is **generation-noisy** (withheld
obstruction flips across redraws) — consistent with OQ-149's observation that the committer/CS axis is
the most model-divergent layer. The fed effect is invisible against that noise; whether a larger draw
budget could resolve a sub-noise committer effect is open.

## Validity controls honored

- **Floor measured, not assumed:** every verdict above is `median(fed-vs-withheld) > max(withheld-vs-
  withheld)`, both pairwise — the generation-stochasticity floor is the comparator (OQ-26), no
  fabricated constant. `ai_governance` verdict did NOT move precisely because its withheld floor is
  noisy (F=[4,0,4]) — the floor correctly suppressed a noisy call.
- **Confound closed:** parties held fixed across arms (fed framing requires stakeholders), so fed-arm
  coverage loss is not the effect; full 6-draw coverage on 5/5 kernels.
- **k frozen pre-draw** (median-vs-max), **observer de-weighted**, **schema untouched** (OQ-149/OQ-83
  exemption is deliberate). All committed before any draw existed.

## Disposition

OQ-119 answered: **feeding moves the join at the diagnostic verdict layer (claim-gated false-
foundational escalation) and leaves the committer cross-examination structure invariant (Theorem-7
holds); observer and temporal-rate move softly.** The per-axis partition is the engine-characterization
— not a flat yes/no. Draws are probe artifacts (`prolog/testsets/oq119_*`), none joined the live corpus.
