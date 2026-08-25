# Synthesis feed — Benter / structural-blindness essay

**Written:** 2026-08-25, at the end of the arc's Phase C topic run, before synthesis.
**For:** the session that takes this arc to a model for final essay synthesis.

The topic run (`c-orchestrator.py`) has already produced its own artifacts. This file lists
**everything else** that should go into the synthesis prompt with them, and why — the run's
decompose step saw only the topic string, so none of this reached it.

## 1. The run's own output

Topic run executed 2026-08-25, exit 0, 356.8s; `_step_commit` = `13cd510d2`; run manifest tag
`benter_hkjc_parimutuel_2026_20260825_125025`. Corpus delta read from the run's OWN
`pipeline_output.json` manifest (`2026-08-25T17:53:01Z`), not from intended adds:
**n_stories 258 → 263 (+5), n_constraints 285 → 291 (+6), axiom_contradiction 27 → 28,
n_unclassified 0.**

One kernel, `beatability_of_the_take`, with five readings. The typology in the topic string came
through the decompose step intact — each reading is one of the arc's positions:

| cid | arc position |
|---|---|
| `flow_extraction_reading` | flow extractor — take is outcome-invariant |
| `public_risk_reading` | risk-holder — carries model error unknowingly |
| `folk_mountain_reading` | blind seat — "you can't beat the races" |
| `meta_prediction_reading` | the seat that reclassified |
| `beatability_of_the_take_flat_control` | flat control |

Note the kernel is named for the **computed** side (`beatability_of_the_take`), not the felt side —
which is the naming rule OQ-382 records, arrived at independently by the generator.

- `outputs/constraint_reports/<cid>_report.md` — 5 reports, all present on disk (verified).
- `outputs/tensions_ledger.md` — 5 blocks, deterministic extraction, **not an essay** (OQ-101: the
  essay FORM collapses plurality; auto-essay was removed 2026-06-10). Synthesis is the operator's
  step, and the ledger is an input to it, not a draft of it.
- `json/<cid>.json` + `prolog/testsets/<cid>.pl` for the five (committed at `13cd510d2`).

## 2. The empirical result — **the arc's only one, and it must go in**

`audits/2026-08-25_gauge_fixed_prediction/WRITEUP.md`

This was Phase A, run before the essay. It is the arc's single empirical finding and the essay
should name what was observed rather than gesture at the restricted-classification gap. The
load-bearing pieces for an essay:

- The engine carries a surface (`classify_from_restricted/3`) that models exactly the essay's
  blind seat — *what a position can see, and what it therefore concludes* — and a pre-registered
  prediction about it sat unrun in a source comment for its whole life. Running it **refuted** it.
- **Why it failed is the essay-relevant part.** The restricted classifier's disagreement with the
  full one is dominated not by epistemic restriction but by a **vocabulary gap** — it cannot emit
  `scaffold`, `tangled_rope` or `naturalized` at all, so 343/1140 rows disagree or abstain for
  reasons that have nothing to do with what an observer can see. Meanwhile its mountain and snare
  thresholds are numerically identical to the real ones, so it agrees by construction at exactly
  the two types the comparison predicate fires on. The instrument was being compared against a
  near-copy of itself.
- **The one place restriction genuinely bites is the essay's own image.** At the powerless seat,
  suppression is not observable — only *felt* — so the engine substitutes experienced extraction
  as a proxy. A constraint whose true suppression is below the mountain ceiling can have a *proxy*
  suppression above it, and the seat reads `rope` where the full data says `mountain`. Two corpus
  rows do this. A constructed fire/decline pair differing in one authored number is in
  `fixtures/`. This is type misperception deleting an option, measured.
- **`dr_type = mountain ⟹ gauge_fixed = true` is a theorem of the canonical site**, not a corpus
  fact: two of the four canonical observer positions cannot perceive `mountain` at all. A
  "you can't beat the races" reading is not available from every seat — which is the essay's
  three-position claim showing up as a structural property of the machinery.
- Disagreement is **not monotone in the accessibility gradient** (institutional 54.0% > powerless
  49.2%). Caveat carried in the writeup: this is mostly the vocabulary artifact, not an epistemic
  result. **Do not let the essay promote it into one.**

## 3. The minted OQ bodies (`grep OQ-38 ISSUES.md`)

- **OQ-381** — the blindness×beneficiary join. Carries the **ruled beneficiary definition
  verbatim**; use that wording, it was decided rather than drafted. Also carries the
  flow-extractor / risk-holder / blind-seat typology in the operator's own terms.
- **OQ-382** — the false-wager signature: decline option × outcome-dependent settlement, and the
  compelled-volume-launders-variance mechanism.
- **OQ-383** — `feature_access/3` has no self-position feature. The open design question, and the
  one place the essay should say the framework does not yet have an answer.

## 4. Precedents — the repo's voice on adjacent ideas

- `blog/2026-06/the_vote_market/the_vote_market_draft3.md` — its **floor condition** (below the
  organizing floor, preferences are non-transactable because no counterparty can warrant them) is
  this arc's idea already in the repo's voice.
- `blog/2026-06/marked_to_market.md` — financial-register precedent.

## 5. Two constraints the essay inherits

- **The no-intent rail** (`docs/design/design_discipline.md:411-423`). The beneficiary definition
  was written to clear it: it quantifies over extraction and invariance, both computed from
  effect, and imputes no mental state to anyone. **The essay must not import intent** — the Jockey
  Club is not accused of designing the blindness, and does not need to be for the argument to work.
- **The correction to the task text, sourced and accepted:** Benter (1994) describes a logit
  technique for *combining* a fundamental model with public-implied probabilities, and reports the
  fundamental model as already carrying a significant advantage before the combination. The weaker
  accurate form is the only quotable one: his design fed both a fundamental estimate and the
  public-implied probability into a second-stage model.

## 6. Held, deliberately

GAP-30's falsifier design (a Benter-shaped predictive-validity test of authored ε) stays HELD per
the operator's ruling. Its only footprint is the dated note now in `docs/design/design_gaps.md`
under GAP-30 — which also records why a market price cannot serve as the out-of-corpus outcome
(a pooled price is other seats' gauges, so scoring ε against it is two seats disagreeing, which
GAP-30's standing falsifier already excludes). **The essay may use the parimutuel as an image;
it may not present it as a grounding test for ε.**
