# OQ-109 Phase C — close-out (analytical tail + population correction)

**Date:** 2026-06-12 / 2026-06-13. **Branch:** `oq109-phasec-closeout`. **Substrate:** live cohort
zero, n=5 (`adjunctification_of_university_teaching_c0`, `demographic_skill_mismatch_c0`,
`institutional_trust_erosion_c0`, `organization_floor_c0`, `scale_ceiling_c0`). Pipeline manifest
`2026-06-13T03:01:15Z`, code `1f517a0`, clean.

This is the close of Phase C's analytical tail. The mechanical Phase C (archive → schema
removal → cohort-zero swap → battery) landed earlier (commits `c6d6880c`/`9a992459`/`a17e7461`/
`5f2a626c`/`7ca48e0b`). It began wire-only (instruments built + witnessed, replicate generation
gated); the operator then **authorized the gated spend**, so this writeup also records the replicate
batch and the σ/seat partition result (last two sections). Final status: **resolved**, σ/seat
residual discharged to OQ-118.

## Step 0 — population correction (RESOLVED, witnessed)

Two stories (`proxy_integration_narrative`, `strategic_victory_narrative`) from the
`strategic_communications_geopolitical_narrative` (Iran / Hormuz-Beirut) essay were sitting
**untracked** in the live `prolog/testsets/`, loading the corpus at **n=7, not n=5**. They carry the
post-removal provenance schema but a **different generation regime** than cohort zero:

| field | `_c0` batch | the Iran pair |
|---|---|---|
| model | `claude-sonnet-4-5-20250929` | `claude-sonnet-4-20250514` |
| sampling | `temperature=0.2` | `temperature=1.0` |
| source_essay | `cohort_zero_regen` | `strategic_communications_geopolitical_narrative` |
| seeded_from | archive id | `none` |

NOT cohort-zero-homogeneous → may not join the stability/σ-seat population (would break the
generation-era homogeneity the regen bought; OQ-109 items 4 & 6).

**Iran-count fork CLOSED (positive-controlled):** genuine 2-story essay, not an interrupted-run
fragment. `outputs/tensions_ledger.md` lists exactly these two; the positive control
`grep -l strategic_communications_geopolitical_narrative json/*.json` returns exactly these two and
no others (the search demonstrably finds essay members, so "only two" is a fact about the corpus,
not the search). ⇒ disposition = **separate cohort**, archived (not quarantine).

**Action taken:** archived the pair (pl/json/reports + schema-pinned manifest) to
`prolog/archives/datasets/iran_essay_2026-06-11/` (commit `d26d04a2`), proved byte-identity, removed
the live untracked copies. Corpus restored to a clean **n=5** (witnessed: `corpus_constraint` count
= 5; the `story_seed/3` redefinition warning the pair caused is gone). Revivable later as its own
homogeneous cohort; **never mix into cohort-zero denominators.**

## Step 1 — instruments (wired + witnessed, no spend)

**`python/cohort_stability.py`** — per-field draw-stability table over structured fields
(SIGMA_SEAT_PREDICTION scope) + within-vs-between distance.
- **Pattern-5 guard is the design point:** positive-agreement and agreement-in-absence are reported
  SEPARATELY. An all-absent/empty field (`[]`, null, missing) is agreement-IN-ABSENCE — never
  draw-stable evidence.
- **Witnessed on `organization_floor`×3** (the only existing replicate triple): 19 positive-stable,
  13 unstable, 6 agreement-in-absence (the genuinely-absent fields: `has_sunset_clause`,
  `vindicated_propositions`, `gain_flow`, `fixing_cost`, `cs_structure`, `coercion_grid` — correctly
  excluded), 2 seed-supplied flagged input-echo. Within-draw distances 0.387 / 0.281 / 0.226;
  between-story = none (n=1 story, correctly refused).
- **`--selftest` positive control PASS** (the instrument-before-data gate): a one-field change flips
  exactly that field's bit; an absent field scores absence not positive; identical pair distance 0.
- Artifacts: `stability_table.out`, `stability_table.json`.

**`python/cohort_sigma_seat_eval.py`** — σ/seat falsifier evaluator, two responsibilities split by
the spend gate.
- **Pre-spend PARSE-CHECK (PASS):** reproduces the frozen bucket assignment from
  `SIGMA_SEAT_PREDICTION.md` (all 26 rows, compound multi-backtick rows parsed) and confirms the live
  instrument (`cohort_stability.FIELDS`) has **zero drift** from it (`FIELD_TO_MD` map is code-visible
  so a drift would be a diff).
- **Population gate REFUSES a verdict below 3 stories × 2 draws.** At n=1 it returned
  **"NO TEST / gated"** — never a degenerate "insufficient power" number (operator ruling: that would
  be a counterfeit witness). Post-spend (6 stories) the gate passes and the partition test runs — see
  "The replicate spend" below. `run_verdict` adds a self-contained Fisher exact (validated vs scipy).
- Artifact: `sigma_seat_eval.out`.

## 1d — `reading_diff` re-point: RECLASSIFIED to a cohort-one item (out of Phase C close)

Witnessed: `constraint_stakeholder/7` is an **Unknown procedure** on the live corpus (zero
stakeholder seats), and the 5 `_c0` stories are perspectives-free. The re-point's positive control
("the operator fires on a story with stakeholder cells") therefore has **no live fireable target** —
claiming inert-by-corpus here would be inert-proving-inert. **Honest status:** re-point unwitnessed
until a stakeholder-cell-bearing story lands → cohort-one, not a Phase C close item. (Forward
dependency recorded in OQ-109.)

## The replicate spend + σ/seat partition test (RAN; commit `dcfaea97`)

Spend authorized 2026-06-12, run via batch: **15 draws = 5 contested kernels**
(`qwerty_path_naturalization`, `free_market_naturalization`, `total_war_unthinkability`,
`printing_press_reformation`, `zero_as_number`) **× 3**, batch `msgbatch_01UbfPq13BcHgJKxcsqK549i`,
15/15 ok, `claude-sonnet-4-5-20250929` @ temp 0.2, seeded from `prolog/kernel_seeds.json` through the
FROZEN seed-spec (title+domain+summary) so `SIGMA_SEAT_PREDICTION.md` (`5f2a626c`) applies. Runner
`agent/cohort_replicate_batch.py`; draws are probe artifacts (replicate dir; none join the corpus).
Fisher exact validated vs scipy to 6 sig figs before use.

**RESULT — the frozen prediction did NOT survive (pre-registered):** partition test over 6 stories
(5 kernels + organization_floor), 188 (field,story) cells — predicted-σ 58 stable / 62 unstable,
predicted-seat 36 / 32, 47.87% consistent, **Fisher two-sided p=0.649 → NO SEPARATION**. The noise
hypothesis the prediction pre-registered as its own falsifier was NOT rejected. Within-vs-between
distance separates cleanly (within ~0.23–0.54 mean ~0.37 < between ~0.50–0.67 mean ~0.59;
`printing_press` d1–d3=0.543 reproduces the signature-identity witness's "one draw escapes" on that
exact topic — the new metric passing the same natural experiment the old one did).

**Operator ruling (split; verdict-stability demoted to confounded-half; discharge-to-successor):**
- **Robust, recorded:** (1) apparatus-presence fields (`boltzmann`/`network`/`interval` 6/6 stable)
  are prompt/schema-determined but the prediction bucketed them **seat** — a mis-bucketing finding
  with NO naming confound (firmest result on the board); (2) the null partition, scoped to
  exact-match over these schema fields.
- **Held-confounded (two halves, named graduations):** (a) the cast/σ fields (stakeholder multisets,
  beneficiaries/victims/vindicated_propositions, all draw-unstable) — exact set-match conflates
  "fresh cast" with "renamed cast"; re-test with the already-built graded distance metric at field
  level; (b) verdict-stability (`disappearance_verdict` 4/2, `founding_problem_status` 3/3) — at n=6
  + temp 0.2 this is suggestive-confounded, not a finding (temp may over-determine the verdict);
  graduation = temp-sweep or accept-as-confounded.
- **Meta-finding (the yield):** draw-stability is an artifact of **field-construction-type**
  (free-authored cast vs schema-mandated/computed), not the σ/seat line — a property the
  meta-analysis corpus must carry before any cross-story claim trusts a field's stability. NOT a
  vindication of noise over seat (the confounded halves can't adjudicate). False-summit-is-the-
  product, done right: a finding about field construction, structured, more informative than a clean
  confirmation would have been.

## Close shape: RESOLVED, σ/seat residual discharged to OQ-118

OQ-109's Phase C **migration** is complete and witnessed; status → **resolved**. The σ/seat residual
was **discharged to a successor (OQ-118)**, not answered — the partition test ran, the frozen
prediction was falsified-as-tested, and the structured finding + its two graded re-test conditions
live in OQ-118. The escalate-don't-redraw discipline is inherited (a graded re-test is a new
pre-registered test, never a retrofit of the failed prediction; the frozen file is untouched). The
`reading_diff` re-point remains cohort-one-gated (carried into OQ-118's cohort-one scope). The
homogeneity falsifier (item 6) threads to cohort two.
