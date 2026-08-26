# PREREGISTRATION — OQ-342 §9 Arm B: are a model's hard seeds structurally different stories?

**Frozen:** 2026-08-25, before any arm was computed. Written by the executing session from the
plan `~/.claude/plans/review-oq-342-and-determine-stateful-hamster.md` Phase 2.

**Substrate:** the coherent 19-leg set at `a3966e7` (`code_dirty: False`), verified per-leg at
Phase 0 S1. Legs read: `pipeline_output.{nemotron,stealth,haiku,flash}.json`. Strata from the
legs' own `.pl` files via `audits.leg_diagnostic_table.PROV_RE` field 5 (`re.S`, multi-line
`story_provenance/8`) — never a leg-name-derived grep (TRIPWIRE A: Flash carries
`no_scope_rebuild_gemini*`; TRIPWIRE B: haiku's June originals carry bare `no_scope_rebuild`).

## Tag discipline (RULING-1, non-negotiable)

Arm B slices the **occupied `+rescue1`** stratum. `+rescue1` and `+seed_rescue1` are one token
apart and name two distinct generation events. Occupancy re-verified live 2026-08-25:

| leg | first-pass | `+rescue1` | `+seed_rescue1` | total |
|---|---|---|---|---|
| `testsets_nemotron` | `no_scope_rebuild_nemotron` 852 | **144** | 4 | 1000 |
| `testsets_nemotron_think` | `no_scope_rebuild_nemotron_think` 1003 | — | 2 | 1005 |
| `testsets_stealth` | `no_scope_rebuild_stealth` 969 | **36** | — | 1005 |
| `testsets_haiku` | `no_scope_rebuild` 505 | — | — (`+stakeholder_backfill` 455) | 960 |
| `testsets_flash` | `no_scope_rebuild_gemini` 754 | — | — (`+stakeholder_backfill` 206) | 960 |

Every stratum present per leg is enumerated above; no binary rescue-vs-rest cut is taken.
`+seed_rescue1` (nemotron 4, nemotron_think 2) is NOT part of any arm — too small to measure and
a different generation event.

## Pre-registered type vocabulary

**Union of seat types across ALL strata compared, zero-filled** — fixed at 8 before any arm runs:
`mountain, naturalized, piton, rope, scaffold, snare, tangled_rope, unknown`. A per-stratum
vocabulary would silently change the vector's length between arms. `untyped` does not occur on
any of these legs (checked); `unknown` is a real type value here and is counted, never dropped.
All 4 seats (`powerless, moderate, institutional, analytical`) are present on 100% of records on
all four legs (checked: one seat-key set, 0 records missing a seat).

## Primary statistic (the criterion) — one number

**L1 between two strata's per-seat type-share vectors**: 4 seats × 8 types, each seat's shares
summing to 1; L1 = Σ_seats Σ_types |share_A − share_B|. Range [0, 8] (each seat contributes ≤ 2).

The candidate readouts are **not independent** — `h1_band` and `verdict_join` are computed from
the seat-type vector, and ε drives χ drives type — so "≥2 independent statistics" would be a
criterion whose name is not true of what it counts. One primary; everything else descriptive.

**Secondary (reported, never criterial):** ε mean/sd/band-3 share; `h1_band` distribution
including null share; `verdict_join.verdict`; `signature`; `purity_band` with `n_scored`/`n_total`;
`claimed_type`. Nulls never coerced (OQ-51 `h1_band` nullable; OQ-60 purity's two absence tokens).

## Controls

Seeds land in `+rescue1` **because they failed the first pass**, so any structural difference is
confounded with selection. The controls separate *this model's hard seeds are structurally
different* from *regeneration under a different regime yields different output for any seed*.

- **C2 — one-sided null threshold, size-matched on BOTH sides.** At sample size m:
  - *Observed:* K=1000 seeded draws of an m-story sample `R` from the first-pass stratum;
    statistic = **mean** L1(target_m, R).
  - *Null:* K=1000 seeded draws of two **disjoint** m-story samples `S₁`,`S₂` from the first-pass
    stratum; null distribution = L1(S₁,S₂); **T95 = its 95th percentile** (single upper cutoff —
    L1 is non-negative, so only the upper tail is meaningful).
  - Both sides are m-vs-m, so they are structurally identical. Comparing the target against the
    *full* first-pass stratum while the null compared m against the remainder would inflate null
    distances, raise T95, and bias the verdict toward row 4.
- **C1 — regeneration-without-hardness, given its OWN null.** `+stakeholder_backfill` (haiku 455 /
  flash 206) is seeds regenerated under the new prompt that did **not** fail. Identical C2
  procedure on that leg → **T95_C1**. Arms are compared as **null-normalised exceedance ratios**,
  never raw L1: **R = observed mean L1 ÷ that leg's own T95**; R > 1 means the arm exceeds its own
  null. Raw L1 is not commensurable across legs (different type mixes, n, first-pass strata);
  each leg's own null supplies its own scale. **Residual, declared:** even normalised this is a
  cross-leg transfer, so a row-3 reading is marked **INFERRED** unless an in-leg witness supports
  it — analogy generates the hypothesis, it does not rule it.
- **C3 — is hardness a property of the seed or the model?** Intersect nemotron's `+rescue1` 144
  with stealth's 36. **One-sided hypergeometric over the shared 1005-seed pool** — the population
  both legs drew from, deliberately NOT the legs' landed `n_constraints` (nemotron 1000 / stealth
  1005), which count stories that landed, not seeds attempted. Expected overlap ≈ 144×36/1005 ≈
  **5.2 seeds**. **Declared low-powered up front:** only a large enrichment is detectable, so C3 is
  **directional only** — it may support a reading, never carry one alone, and a null here is
  `underpowered`, not `no enrichment`.

### Two declared deviations from the plan's Phase 2, forced by the data

1. **haiku's C1 runs at m=252, not 455.** C2's null needs two *disjoint* m-samples from the
   first-pass stratum, i.e. n_firstpass ≥ 2m. haiku is 505 first-pass vs 455 backfill, and
   2×455 = 910 > 505 — the matched-disjoint design is **not constructible at m=455**. Rule applied
   uniformly to every arm: **m = min(n_target, ⌊n_firstpass/2⌋)**. This binds only on haiku
   (nemotron 144 ≤ 426, stealth 36 ≤ 484, flash 206 ≤ 377 all run at their natural n; haiku
   subsamples the 455 backfill to 252 on each draw). Declared rather than silently rebalanced,
   because R is a ratio to the leg's own null at the same m and is therefore still internally
   valid — but haiku's arm is measured on a subsample and its n is reported everywhere.
2. **The MDE anchor is computed at the primary arm's own m.** "The L1 between two different
   models' first-pass strata" is computed **the same way** as T95: mean over K=1000 seeded draws
   of L1(nemotron_firstpass_m, stealth_firstpass_m) at m=144. Comparing a full-stratum L1 against
   an m=144 T95 would compare two different estimators.

## Power check — Row 1, computed BEFORE the arms are read

From the null distribution alone: the **minimum detectable effect** is the smallest L1 exceeding
T95. Anchored against a known-real, known-large difference computed the same way — the
**between-model first-pass L1** (free; the coherent set has 19 legs). If T95 exceeds *that*, the
instrument cannot see even a cross-model gap and certainly cannot see a within-leg one.

(The alternative criterion "T95 exceeds L1's observable maximum" could essentially never fire — a
permutation null never approaches the metric's maximum of 8. It looks like a power check and is
not one.)

## Pre-registered outcomes — evaluate in ORDER, first match wins

**The verdict is decided on the nemotron arm alone (n=144).** stealth (36) can corroborate
direction or fail to; it can never set, block, or downgrade the overall call.

| # | outcome | criterion (nemotron arm; R = observed ÷ own T95) | reading |
|---|---|---|---|
| 1 | `underpowered` | MDE anchor fails: T95_nemotron ≥ the between-model first-pass L1 | the instrument cannot discriminate at this n. Declare unanswerable and **name the n that would**; rows 2–4 are not evaluated |
| 2 | `regeneration_effect` | R_nemotron > 1 **and** R_C1 ≥ 1 | the move is regeneration, not hardness — C1 exceeds its own null too. NULL on §9's question; declared, not absorbed |
| 3 | `hard_seeds_differ` | R_nemotron > 1 **and** R_C1 < 1 | residues bias every leg; OQ-378's spend is a bias correction. Marked **INFERRED** per C1's cross-leg residual |
| 4 | `just_misauthored` | R_nemotron ≤ 1 | **the valuable null** — a *tested absence*, because C2 witnesses the instrument's ability to separate. Residues stop being a standing concern; OQ-378's spend becomes a composition nicety |

Row 1 is checked first because a non-discriminating instrument invalidates rows 2–4 rather than
competing with them. Rows 2–4 are mutually exclusive by construction on (R_nemotron, R_C1), so no
two can fire at once. **Both ratios are reported numerically whatever fires.** stealth's result is
reported alongside every outcome as corroboration — concordant, discordant, or uninterpretable at
36 — and never sets, blocks, or downgrades the call.

**Zero classification is mandatory** at write-up: every zero reported is typed *tested absence* /
*untested instrument* / *unrecheckable*. C3's likely null is `underpowered` by construction.

**Determinism:** `numpy.random.default_rng(20260825)`, K=1000, one generator threaded through all
arms in a fixed arm order (recorded in the artifact) so the run reproduces exactly.
