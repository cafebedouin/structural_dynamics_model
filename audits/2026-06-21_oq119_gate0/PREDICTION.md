# OQ-119 — Frozen pre-registration: does feeding move the JOIN structure?

**Status: FROZEN pre-spend. Re-frozen 2026-06-21 for the THREE-AXIS generator, before any draw
exists.** Edits after the first draw invalidate the pre-registration (escalate-don't-redraw: a
graded re-test is a NEW pre-registered test with its own frozen prediction, never a retrofit).

**Why re-frozen (the verification finding that reshaped the spec).** The first freeze specified the
single-story `cohort_replicate_batch` generator. Verification (the operator's "if it passes, do the
spend" gate) showed that path authors **no `cs_` facts** — a regenerated single story carries
observer + temporal only (witnessed: `audits/2026-06-13_oq117_within_arm_proxy/fed_arm/*.json` have
zero `cs_kernel_id` / `cs_reading_relation`). The committer axis — the HIGH-information axis where the
Theorem-7 tripwire lives (Claude Web's point 2) — would be **structurally dead** in those arms,
recreating the ≥1.5-axis vacuity OQ-119 forbids. The committer axis is born only on the
**kernel-generation path** (`generate_kernel_corpus` no-scope / `c-orchestrator` scope), which authors
`cs_structure.reading_relations` per reading (`generate_constraint_pl.py:666`; all 960 haiku + 69/92
live `testsets/` carry the edges). This spec therefore regenerates **whole multi-reading kernels**, so
all three axes — observer, committer, temporal — are live in BOTH arms.

This file is the discriminating control for the spend that answers OQ-119. The substrate gate
(Gate 0) is **witnessed open** (see `GATE0_FINDINGS.md`): observer + axiom + temporal are each
non-vacuous on the `testsets_haiku` twin, 325 kernels clear all three. The comparator (Phase 1,
`python/audits/oq119_join_diff.py`) is **validated** (self-diff=0, cross-diff>0, within-kernel
shift resolvable). What remains is the operator's **spend-go** ruling. Do not run the fed arm
without it.

---

## The question

OQ-117 showed feeding the mountain claim does not move the engine's *type* verdict (0/30 both
arms). OQ-119 asks the sharper question: does feeding move the **join structure** — the
cross-examination the engine performs across the three axes — even when the type holds?
- **observer**: which seats disagree, and by how much (the 4-seat χ spread).
- **axiom/committer**: which readings foreclose / coexist (obstruction status), which contexts
  the readings diverge at, and the verdict-join alert set / signature grade.
- **temporal**: how the drift trajectory runs (per-metric rates and slope signs).

## Unit of measurement (the within-reading, between-arm scale)

For each test story C in the pre-registered set:
- **withheld arm** = C regenerated with NO hypothesis fed (the natural draw).
- **fed arm** = C regenerated with the claim injected (the OQ-117 fed-hypothesis idiom).
- the measurement is **join_distance(fed, withheld)** computed by the Phase-1 comparator.

Both arms must be **actually run** — a within-arm proxy cannot answer this (OQ-117 WRITEUP §2).

## The binding noise floor (and why it needs spend)

Generation is stochastic: the same prompt yields a different draw every time (OQ-26 / Axiom 2,
v6.13.1). So a nonzero `join_distance(fed, withheld)` is meaningless until compared against the
**withheld-vs-withheld redraw floor** — the join motion produced by re-drawing the SAME withheld
story twice, with no claim fed. That floor is the generation-stochasticity baseline; the fed
effect must clear it to count as "feeding moved the join."

The Phase-1 controls bound only the comparator's *numerical* resolution (linear to ~5e-4) and its
*between-reading* sensitivity (within-kernel, two different readings: scalar ≈ 0.27). Neither is
the within-kernel redraw floor. **Therefore the spend protocol MUST measure the floor**: for each
kernel, regenerate the withheld arm **3×** (gives a withheld-vs-withheld distance distribution per
axis) in addition to the fed arm. The decision rule below is defined relative to that measured
distribution (median fed-distance vs the floor's max) — never a fabricated constant.

## The frozen decision rule (k is fixed NOW — Claude Web point 1)

The measurement is **per axis, per kernel**. For each kernel and each axis A ∈ {observer, committer,
temporal}:
- regenerate the kernel **3× withheld** and **3× fed**; for each draw, export its three-axis join
  record (`export_oq119_join_records.pl`) and aggregate to a kernel-level per-axis vector.
- `F_A` = the **withheld-vs-withheld** pairwise axis-distances (3 draws → 3 pairs). This is the
  generation-stochasticity floor for axis A.
- `D_A` = the **fed-vs-withheld** pairwise axis-distances (3×3 = 9 pairs). Same metric as `F_A`
  (both pairwise — NOT fed-vs-centroid; the earlier centroid framing is retired as a scale mismatch).

**Frozen statistic, no floating multiplier:** axis A **moved** for a kernel iff
> **median(D_A) > max(F_A)** — the typical fed-vs-withheld distance exceeds the *worst-case*
> withheld noise. k is pinned: the comparison is median-vs-max, chosen now, n=3-robust, with no
> constant to tune post hoc. (max(F_A) is the conservative floor at n=3 where a percentile is
> fragile.) A categorical axis (committer `obstruction_status`) uses the same rule with distance =
> "category differs" (1/0): it moved iff fed flips the category in the **majority** of fed-vs-withheld
> pairs while the withheld draws agree among themselves (max(F)=0).

## Per-axis verdict with observer DE-WEIGHTED (Claude Web point 2)

A single summed scalar is **not** used for the headline — the labile observer axis (moves on
ε/directionality with no re-authored stakeholder, per the Gate-0 stakeholder caveat) could carry a
"join moved" that is only ε wobble. Instead:

- **HIGH-information axes (can carry the headline):** committer `obstruction_status` / divergence-scope
  set, temporal **slope-sign flips**, and verdict-join **grade/alert** changes.
- **SOFT axis (reported, cannot carry the headline alone):** observer χ spread, and temporal rate
  *magnitude* without a sign flip.

| Outcome | Definition (frozen) | Reading |
|---|---|---|
| **JOIN MOVES** | ≥1 **HIGH-information** axis moved (rule above), stable across the 3 fed draws | Feeding shifts the cross-examination even where type holds — OQ-119 = YES. |
| **JOIN INVARIANT** | No HIGH-information axis moved on ≥80% of kernels | Feeding stays within generation noise on the axes that carry information — OQ-119 = NO; OQ-117 type-invariance extends to the informative join. |
| **SOFT-ONLY / per-axis** | Only the observer/rate-magnitude SOFT axis moved | Feeding nudges ε but does not move the cross-examination — reported explicitly as soft, NOT as "join moved." |

**Per-axis sub-predictions (frozen before the run so each outcome has a meaning):**
- **observer (soft)**: feeding a foundational claim is predicted to compress high-power seats toward
  analytical (institutional χ up). Expected to move — and that movement is LOW-information by itself.
- **committer (high-info)**: `cs_reading_relation` edges are observer-blind by construction (Theorem
  7 / detection-independence). Prediction: feeding an observer-side foundational claim should **NOT**
  move `obstruction_status`. **A committer move IS a Theorem-7 violation and a top-line finding** —
  and it is now *measurable* because the whole kernel (its sibling edges) is regenerated in both arms.
- **temporal (mixed)**: feeding predicted to move rate *magnitudes* (soft) more than slope *signs*
  (high-info); a sign flip would be a genuine trajectory-direction change.

## Pre-registered set (FROZEN — all readings, no TBD)

Five kernels from the Gate-0 joint cell spanning both obstruction statuses; seeds frozen in
`audits/2026-06-21_oq119_gate0/oq119_seed_subset.json` (16 reading seeds from the rebuild pool):

1. `acceptable_risk_energy` (licensed_plurality) — readings: catastrophic_tail_dominant,
   expected_value_dominant, option_value_preserving
2. `westphalia_sovereignty` (real_closure) — readings: absolute_non_intervention,
   conditional_responsibility, graded_sovereignty
3. `woman_category` (real_closure) — readings: gender_identity_reading,
   intersex_accommodation_reading, sex_biology_reading
4. `ai_governance_legitimacy` (real_closure) — readings: democratic_pluralist_reading,
   magisterial_subsidiarity_reading, market_libertarian_reading, technocratic_optimization_reading
5. `waitangi_sovereignty_allocation` (licensed_plurality) — readings: crown_sovereignty_reading,
   partnership_reading, rangatiratanga_reading

**Fed injection (frozen idiom).** Whole-kernel regeneration; into EVERY reading's generation task,
append the foundational claim in the OQ-117 idiom: *"AUTHORIAL FRAMING (fed hypothesis): the author
asserts this constraint is a natural / foundational feature of the world (claim type: mountain).
Author the story consistent with that framing."* The withheld arm appends nothing. This is the only
arm difference (regime otherwise identical: same model, temperature, prompt, schema, seeds).

## Spend specification

- **Driver**: `python/audits/oq119_spend_driver.py` — reuses `generate_kernel_corpus`
  `build_cached_messages` / `process_batch_results` / `stamp_kernel_linkage` (NOT a fork; only the
  fed-append, the 3-draw loop, and the `{withheld,fed}` output dirs differ). Anthropic Batch API +
  prompt-cache. Output `audits/2026-06-21_oq119/{withheld,fed}/`. Draws are PROBE ARTIFACTS — none
  join the live corpus.
- **Draw count**: 16 readings × 2 arms × 3 draws = **96 generations**. The kernel generator runs
  **Haiku** (`GEN_MODEL`, batch $0.50/$2.50 per MTok), not Sonnet — output-dominated
  (~6K out/draw × $2.50/MTok ≈ $0.015/draw) → **≈ $1.5 total** (`--dry-run` witnessed: $1.44 output
  + ~$0.07 cached input, real lower under prompt-cache). Cheaper than a single-story Sonnet spend
  despite 96 draws, because the kernel path is a Haiku model.
- **Read-out**: per kernel, load each draw's regenerated reading set as a `corpus_path` overlay
  (no full `run_pipeline` needed), export join records, compute `F_A` / `D_A` and apply the frozen
  rule above per axis. Write `RESULTS.md` citing per-kernel per-axis `median(D_A)` vs `max(F_A)`; the
  adjudication is blind to this file until after the read (no post-hoc tuning).

## STOP / GO

Spend-go is the operator's seat. This file is the frozen instrument; a `--dry-run` (no cost) confirms
the 96-generation plan and price before any submission.
