# Redundancy diff — κ-track vs drift/trajectory track (the empirical half of ruling (b))

**Run 2026-06-10** (operator-commissioned: "run both tracks on the same inputs and compare the
distinction sets"). Inputs: the five committed probe stories + a series-bearing live corpus
story (`collapse_mechanism_ambiguity`, 18 measurement facts incl. `base_extractiveness` series).

## Direction 1 — drift-track on the grid-probe stories

`drift_events:metric_trend(C, base_extractiveness, _)` → **NO-SERIES on all five**;
`drift_event/3` → **[] on all five**. The drift track reads scalar measurement series
(`base_extractiveness`, `theater_ratio`, `coordination_effectiveness`); the grid stories author
only leveled metrics. Blind.

## Direction 2 — κ-track on a series-bearing corpus story

`collapse_mechanism_ambiguity`: DRIFT fires (trend=stable, events=[extraction_accumulation,
purity_drift]); κ-track G_sys=0.0 (no leveled grid → blind) — and **completeness = 312.5**,
exposing a latent defect: `coercion_vector/3` and `compute_completeness/2` read
`measurement(_, _, Metric(L), T, V)` with the INTERVAL ANONYMOUS — on a multi-story KB the
reads leak across constraints (8 slots × every story's matching time points). The probe runs
masked this because `load_and_run` consults one story alone. **The κ-track as built is
single-story-safe only**; interval-scoping is required work under any migrate ruling.

## Verdict

**Redundancy is ZERO — by disjointness, not by equivalence.** The two tracks read disjoint
input surfaces (leveled grid vs scalar series); neither can produce the other's distinctions
on any input. Consequences for the ruling:

- The κ-track's UNIQUE product, witnessed: **level-resolved coercion dynamics** — the
  `divergent` probe story (structural rises while individual falls, G_sys=+0.156 with
  per-level κ split) is representable by NO live subsystem; drift has no level axis. Retiring
  the κ-track loses that axis entirely; keeping drift loses nothing κ-shaped.
- "If the κ-track adds zero distinctions, retirement is evidence-settled" — it does NOT add
  zero; it adds exactly one axis (level resolution + weighted composite), currently fed by
  nothing.

## The priced question (b), as it returns to the operator

Keep-and-migrate buys: the level axis (who experiences the squeeze, per level, over time) +
the κ composite. It costs: (1) producer side — leveled-grid schema + prompt +
`stakes_inflation` resurrection, with the OQ-93 caveat that authoring moves invention from
category priors to LLM judgment; (2) repair side — interval-scoping `coercion_vector`/
`compute_completeness` (multi-story safety, witnessed broken); (3) the intent component is
retire-or-redesign REGARDLESS (ruling (a)). Labeled caution, per the operator: any prior
intuition about these constructs' yield was formed on the broken instrument (the gradient cut
+ the shim diet); pricing rests on post-fix output only — which is exactly the probe table
plus this diff.
