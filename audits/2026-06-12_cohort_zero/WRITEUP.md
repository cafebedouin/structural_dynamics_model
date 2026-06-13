# OQ-109 Phase C — close-out (analytical tail + population correction)

**Date:** 2026-06-12 / 2026-06-13. **Branch:** `oq109-phasec-closeout`. **Substrate:** live cohort
zero, n=5 (`adjunctification_of_university_teaching_c0`, `demographic_skill_mismatch_c0`,
`institutional_trust_erosion_c0`, `organization_floor_c0`, `scale_ceiling_c0`). Pipeline manifest
`2026-06-13T03:01:15Z`, code `1f517a0`, clean.

This is the wire-only close of Phase C's analytical tail. The mechanical Phase C (archive → schema
removal → cohort-zero swap → battery) landed earlier (commits `c6d6880c`/`9a992459`/`a17e7461`/
`5f2a626c`/`7ca48e0b`). Spend boundary per operator: **wire the instruments, gate the replicate-draw
API generation.**

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
- **Population gate REFUSES a verdict below 3 stories × 2 draws.** At n=1 it returns
  **"NO TEST / gated"** — never a degenerate "insufficient power" number (operator ruling: that would
  be a counterfeit witness). The σ/seat partition test is a post-spend item.
- Artifact: `sigma_seat_eval.out`.

## 1d — `reading_diff` re-point: RECLASSIFIED to a cohort-one item (out of Phase C close)

Witnessed: `constraint_stakeholder/7` is an **Unknown procedure** on the live corpus (zero
stakeholder seats), and the 5 `_c0` stories are perspectives-free. The re-point's positive control
("the operator fires on a story with stakeholder cells") therefore has **no live fireable target** —
claiming inert-by-corpus here would be inert-proving-inert. **Honest status:** re-point unwitnessed
until a stakeholder-cell-bearing story lands → cohort-one, not a Phase C close item. (Forward
dependency recorded in OQ-109.)

## Degenerate-population read (analysis substrate, NOT a verdict — n=1 story)

The `organization_floor`×3 table is data point one for the σ/seat partition, not a test. Recorded as
substrate only: `extractiveness` positive-stable at 0.42 across all 3 draws (consistent with the
earlier replicate datum, σ-side, but KNOWN-IN-ADVANCE = no blind credit); `claimed_type` stable
(`mountain`); `requires_active_enforcement` UNSTABLE despite being predicted-σ (a candidate
seat-boundary signal IF it survives at n≥3 — not citable now). None of this is a finding; it is the
shape the spend will populate.

## Close shape: PARTIAL, two named residuals

OQ-109 Phase C is **partial-with-named-residual**:
1. **σ/seat evaluation** — frozen prediction parse-checked; the partition test awaits the gated
   replicate spend (3–5 stories × 3 draws, set chosen against the prediction's seat-side fields).
2. **`reading_diff` re-point** — cohort-one, gated on a stakeholder-cell-bearing story existing.

The homogeneity falsifier (item 6) threads forward to cohort two. The gated spend (replicate-set
selection) remains the operator's decision; the runner (`agent/cohort_zero_regen.py`) and both
consuming instruments are wired and witnessed, so the spend is now a single launch + two re-runs.
