# Edge is private truth: the forecasting record as the instrument's design brief

*Field note, 2026-06-09. Derived from the operator's Good Judgment supers-portal export
(635 scored questions, 135 non-imputed forecasts, 2018–2026; raw CSV local-only in
`outputs/`, not committed — public repo, personal performance data; aggregates quoted
here are the keep). Companion artifacts: the GJOpen 2020 self-review (114 questions,
Brier + accuracy-vs-median + lessons-learned format), `docs/repair_dynamics.md` (OQ-91),
and the not-yet-built prediction ledger this note is partly a spec for.*

## The shape of the record

On the typical question the operator is a **median-matcher**: median score_diff vs. the
supers median ≈ 0 (−0.0006), beat rate 52.6%. The aggregate cost lives entirely in the
tails, and the tails are asymmetric: **7 big wins vs 22 big losses** (|diff| ≥ 0.2),
loss mass (+10.99) nearly double win mass (−6.25), netting to +0.035 mean Brier paid per
question for the option on the home run. Swing-for-the-fences, quantified.

The tails are not random. They cluster into exactly two families:

- **Wins = narratives that were true and unpriced.** The top three are all COVID
  second-wave questions — the "respiratory epidemics come in waves, six months apart,
  second worse" model, bet at 100% while the crowd hedged. Plus UK 2019.
- **Loss family 1: macro-financial numerics** (Fed U3 ×2, EZ recession, global GDP
  2024, crypto) — already proscribed by the operator's own 2020 lessons; the U3 losses
  re-ran the unemployment logic that won in 2020, in a regime where it didn't.
- **Loss family 2: political repair capacity** (US–Taliban deal, US 2024 Republican
  field, TikTok ban, Italy and France elections) — underestimating institutional and
  incumbent capacity to reform around a shock. This is the **third independent
  appearance** of the same miss: the 2020 lessons-learned list (#6 institutional
  processes, #9 incumbents, the Bibi and Collins entries), the Colombia runoff call
  ("underestimated the right's ability to reform around a candidate"), and now the
  portal data. It is the operator's most persistent, most expensive bias — and the
  exact phenomenon `repair_dynamics.md` is trying to formalize. **The loss tail is the
  theory's calibration set.**

## The correction that matters (operator, this conversation)

A first read of the record suggested "swings pay when the edge is a mechanism and cost
when it's a narrative." The operator's correction: **structural arguments are also
narratives.** The COVID wave bet was not a different epistemic *category* from the Uber
bet — it was a narrative that happened to be true and unshared. Edge is not
mechanism-vs-story; **edge = true ∧ unpriced — private truth.** Every competent
forecaster's loss tail is narrative-over-base-rate, because for someone who knows the
base rates, that's the only place a loss tail *can* live.

What survives as an ex-ante discriminator: **checkability en route.** Some narratives
expose themselves to falsification before resolution (the wave model made dated
intermediate commitments — second wave, ~six months, worse — that could have failed
early and visibly); some cannot be wrong until the question resolves ("Uber's valuation
is crazy," "COVID must produce bankruptcies"). You cannot know ex ante whether your
narrative is true; you *can* know whether it is checkable en route. The wins cluster in
the checkable ones. This test is applicable at submission time, which is what makes it
worth anything.

## Era boundaries are confounds, witnessed

The yearly trend (2019–20 beating the median, 2021+ flipping positive) reads naturally
as edge-decay. The operator's testimony rewrites it as **attention reallocation**:
effort dropped April 2021 (crypto involvement); AI use began 2025-07; this project began
2026-01. The data alone cannot distinguish edge-decay from effort-decay — an era
confound real enough to fool a careful reader in this very conversation. Therefore the
prediction ledger carries **era as an explicit column**: pre-R / R / crypto-attention
(2021-04) / AI (2025-07) / DR (2026-01). A future writeup must not claim the engine's
delta when it measured the era's.

## The repeated behavior, and what it implies for the engine

This is the **second time** the operator has responded to a clustered loss family by
building a tool that mechanizes exactly the part where deep-interest intuition fails:

1. **Loss family 1 (market numerics) → R.** Payoff measured and recorded (2020 review,
   lesson 7: "R Script for generating probabilities from historical prices is better
   than almost everyone's stock price forecasts").
2. **Loss family 2 (repair capacity) → DR.** Same move, with a measured prior success
   as precedent.

Implication for the engine's forecasting role: **not to make forecasts, but to classify
the forecaster's reason for deviating from the median.** "Is this deviation backed by
something true-and-unpriced, or by a story that can't be wrong until resolution?" is
natively a constraint-classification question. An engine that did only that — flagged
"this swing's justification has repair-capacity-shaped holes" before submission — would
have been worth roughly 11 points of loss mass on this record.

## Ledger design notes (capture-at-ship-time; the rest can wait)

The scoring apparatus is staged behind getting the Prolog instrument stable (operator
ruling). What *cannot* be reconstructed later and must be captured at forecast time:

- **The unaided prior**, written down *before* consulting any engine output, alongside
  the submitted forecast — this is the only way each question yields an
  engine-attributable, sign-carrying delta (the platform preserves submissions, not the
  prior you didn't record). Finer split if friction allows: unaided prior /
  post-AI-conversation prior / post-engine forecast — separating "talking to a model
  helps" from "the engine helps."
- **Which engine artifacts the forecast leaned on** (one line).
- **Headline metric: accuracy-vs-median, not raw Brier** (question-mix confound; the
  operator's own 2020 lesson 13).
- **Post-mortem column: which engine layer the miss indicts** — the GJOpen lessons
  indicted heuristics; the DR version indicts theory components.

First DR-era scored forecast: the Colombia presidential runoff (proof-of-concept run on
the superforecasting platform; in runoff as of this note). The pre-AI personal baseline
(years of accuracy-vs-median) is the control arm almost no framework-validation effort
ever has.
