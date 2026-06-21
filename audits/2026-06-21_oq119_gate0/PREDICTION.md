# OQ-119 — Frozen pre-registration: does feeding move the JOIN structure?

**Status: FROZEN pre-spend. Committed 2026-06-21, before any fed-arm draw exists.**
Edits after the first draw invalidate the pre-registration (escalate-don't-redraw: a graded
re-test is a NEW pre-registered test with its own frozen prediction, never a retrofit).

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
the within-reading redraw floor. **Therefore the spend protocol MUST measure the floor**: for each
story, draw the withheld arm **≥3 times** (gives a withheld-vs-withheld distance distribution) in
addition to the fed arm. The threshold below is defined relative to that measured distribution —
never a fabricated constant.

## Pre-registered discriminating prediction (per axis, frozen)

Let `F` = the set of withheld-vs-withheld redraw join-distances (the floor distribution), and
`d_fed` = join_distance(fed, withheld) for the same story.

| Outcome | Definition (frozen) | Reading |
|---|---|---|
| **JOIN MOVES** | `d_fed > P95(F)` AND the moved-field breakdown is non-empty and **stable across fed redraws** (same axis/field flagged ≥2/3 fed draws) | Feeding shifts the cross-examination even where type holds — OQ-119 answered YES; the seat theorem's "feeding re-seats the reading" is witnessed at the join. |
| **JOIN INVARIANT** | `d_fed ≤ P95(F)` for ≥80% of stories | Feeding leaves the join within generation noise — OQ-119 answered NO; type-invariance (OQ-117) extends to the full join. |
| **PARTIAL / per-axis** | `d_fed > P95(F)` on a *proper subset* of the three axes, stable across redraws | Feeding moves SOME axes (name them) and not others — the engine-characterization is the per-axis partition, reported as such, NOT collapsed to a single yes/no. |

**Per-axis sub-predictions (stated before the run so each outcome has a meaning):**
- **observer**: feeding a foundational/mountain claim is predicted to *compress* the high-power
  seats toward the analytical seat (the claim's "this is how it is" pulls institutional χ up). A
  significant move here = `d_fed.observer > P95(F.observer)`.
- **axiom**: feeding is predicted to be MOST likely to move the **verdict-join alert set /
  signature grade** (a fed claim can trip a false-foundational signature) and LEAST likely to move
  the **obstruction status** (committer edges are authored `cs_reading_relation`, observer-blind by
  construction — Theorem 7 / detection-independence; a fed observer-side claim should NOT re-author
  sibling foreclosure edges). **If obstruction_status moves under feeding, that is a detection-
  independence violation and a finding in its own right.**
- **temporal**: feeding is predicted to move slope *magnitudes* (rates) more than slope *signs*
  (a fed claim nudges ε but rarely reverses a trajectory's direction).

## Pre-registered set (frozen)

Drawn from the Gate-0 joint cell (`g00_roster_output.txt`), choosing kernels that span both
obstruction statuses and a range of topics, matched to the OQ-117 fed-claim idiom (foundational/
mountain claim is the injected hypothesis):

1. `acceptable_risk_energy` (licensed_plurality, 3 readings) — fed reading: `__expected_value_dominant`
2. `westphalia_sovereignty` (real_closure, 3 readings) — fed reading: `__absolute_non_intervention`
3. `woman_category` (real_closure, 3 readings) — fed reading: `__sex_biology_reading`
4. `ai_governance_legitimacy` (real_closure, 3 readings) — fed reading: TBD-frozen-at-spec-time
5. `waitangi_sovereignty_allocation` (licensed_plurality, 3 readings) — fed reading: TBD-frozen-at-spec-time

(The two TBD readings must be frozen into this file BEFORE the spec script is run; leaving them
open here is the one permitted completion, and it must happen pre-draw.)

## Spend specification (DO NOT RUN — awaits operator spend-go)

- **Driver**: reuse `audits/2026-06-13_oq117_within_arm_proxy/oq117_spend_driver.py` structure
  (imports `cohort_replicate_batch.py` / `story_generator_base.build_prompt_parts`; Batch API +
  prompt-cache). New output dir `audits/2026-06-21_oq119/{withheld,fed}/`. Draws are PROBE
  ARTIFACTS — none join the live corpus.
- **Arms per story**: withheld ×3 (floor) + fed ×3 (effect + stability) = 6 draws/story × 5
  stories = **30 draws** — the same draw count as the OQ-117 two-arm spend that ran (`dcfaea97`).
  Priced off that empirical model (sonnet-4-5, Batch API $1.50/$7.50 per MTok, prompt-cached 19K
  prefix, ~6K output/draw, output-dominated at ≈$0.045/draw): **≈$1.5 cached, ≈$2.6 cache-cold —
  call it ~$2 total.** Dollars are negligible; the real costs are the small fed-arm seed-spec code
  change and the analysis/witnessing time. Confirm against the live OQ-117 batch log before quoting.
- **Pipeline**: a full `run_pipeline.py` per draw is NOT required for the join record — load each
  draw's `.pl` as a `corpus_path` overlay and export directly via
  `prolog/export_oq119_join_records.pl` (avoids 30 serialized OQ-77 pipeline passes). Feed pairs to
  `python/audits/oq119_join_diff.py`. Run a full pipeline only if a downstream artifact needs it.
- **Read-out**: `F` = the 3·(3 choose 2)=… withheld-pair distances per story; `d_fed` = each fed
  draw vs the withheld centroid. Apply the frozen table above. Write `RESULTS.md` citing the
  per-story per-axis distances; the engine adjudication is blind to this prediction file until
  after the read (no post-hoc threshold tuning).

## STOP

This is the spend-go gate. The fed-arm LLM spend is the operator's seat. Phase 1 + Phase 2 are
complete and witnessed; the next forward move is the operator's ruling to spend (or not).
