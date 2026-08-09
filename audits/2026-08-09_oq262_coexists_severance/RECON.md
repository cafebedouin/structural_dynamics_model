# RECON — OQ-262 severance/intrinsicness audit on authored `coexists_with` edges

**Phase:** A (read-only; no repo file modified; all probes in-memory)
**Date:** 2026-08-09
**Probe:** `recon_probe.pl` (this directory) — three entry goals, each its own process
with a per-process classifier self-check (4 resolution modes on constructed atoms,
cross-checked against `cs_kernel_registry:cs_edge_target_member/4`) and an
overlay-took-effect control (expected-present + expected-absent constraint) before any census.
**Logs:** `recon_live.log`, `recon_kernel_test.log`, `census_testsets_{haiku,flash,kimi,sonnet}.log`
— every claim below is pasted from one of these.

All edge enumeration goes through the mandated accessor
`cs_kernel_registry:cs_edge_target_member/4` / `kernel_pair_edge/5`; raw
`cs_reading_relation` target matching appears only inside the probe's mode
*classifier*, which mirrors the resolver's clause order exactly and was
self-checked against it in every process (`self_check: 4/4 modes classified as
constructed; resolver agrees` — all six logs).

## 1. Substrate counts (as-of these runs; corpus moves mid-session — Phase C re-fingerprints)

| leg | constraints loaded | kernel-owned edges | exact | prefixed_to_bare | bare_to_prefixed | unresolved | orphan-source edges |
|---|---|---|---|---|---|---|---|
| `testsets/` (live) | 235 | 336 | 14 | 168 | 0 | 154 | 48 |
| `archives/datasets/kernel_test/` | 229 | 190 | 145 | 0 | 0 | 45 | 20 |
| `testsets_haiku/` | 960 | 2004 | 1877 | 0 | 0 | 127 | 0 |
| `testsets_flash/` | 960 | 2008 | 1907 | 0 | 0 | 101 | 0 |
| `testsets_kimi/` | 1005 | 2111 | 2108 | 0 | 0 | 3 | 0 |
| `testsets_sonnet/` | 1001 | 2099 | 2085 | 0 | 0 | 14 | 0 |

Totality check (total = kernel-owned + orphan) passed on every leg (`totality CHECK OK`).
"Orphan" = a `cs_reading_relation` source UID belonging to no registered kernel member.

**The prefixed-target skew is confined to the live leg.** 168 prefixed_to_bare edges
across ~20 kernel families (per-kernel breakdown in `recon_live.log`), including
`fiat_efficacy_kernel` (30) and `visual_evidentiary_authority` (12) — confirming the
plan's "census extends beyond fiat + visual." The four twins and kernel_test carry
**zero** non-exact resolving edges. `bare_to_prefixed` fired **nowhere** (0 on all six legs).

**Phase-D movement surface (pre-derivation input).** The `cs_pattern_detection.pl:355`
raw-match consumer reads only `forecloses` edges; the edges that currently fail raw
target match but resolve via the resolver are exactly the `forecloses-prefixed_to_bare`
rows: **18 on the live leg** (adverse_effect_guarantee_kernel 1, authentic_preference_boundary 1,
cooperative_artifact_legitimacy 1, fetterley_transfer_kernel 1, **fiat_efficacy_kernel 2**,
generality_standard 1, genuine_relational_understanding 2, money_governance_coupling 1,
personhood_boundary_kernel 2, polaris_document_status 3, seat_gauge_orientation_kernel 1,
unsettled_claim_ontology 1, visual_evidentiary_authority 1), **0 on every other leg**.
Prediction to be finalized in Phase D: routing changes can move output only on the live
leg, and only for stories passing the predicate's other gates; twins and kernel_test
must be byte-identical. Same census bounds the `cs_corpus_analysis.pl:131–149` routing
(its raw matches cover coexists_with and forecloses; live-leg non-exact totals: 100
coexists_with + 18 forecloses prefixed_to_bare).

## 2. Fiat family (live leg) — edge table

6 registered readings; **30 directed edges, all resolving, all `prefixed_to_bare`**
(full table in `recon_live.log`). **13 unordered coexists_with pairs** — reconciling the
OQ entry's "20 edges": 20 *directed* coexists edges over 13 unordered pairs; both counts
correct, convention differs.

Per-pair directional profile (source → target relation | reverse):

| pair | dir 1 | dir 2 | shape |
|---|---|---|---|
| scholarship, empathy_simulation | coexists | coexists | mutual coexists |
| scholarship, empirical_precedent | coexists | coexists | mutual coexists |
| scholarship, truth_procedure | coexists | coexists | mutual coexists |
| empathy_simulation, empirical_precedent | coexists | coexists | mutual coexists **(contradiction pair)** |
| empathy_simulation, utopian_fiction | coexists | coexists | mutual coexists |
| empirical_precedent, predictive_synthesis | coexists | coexists | mutual coexists |
| truth_procedure, predictive_synthesis | coexists | coexists | mutual coexists |
| scholarship → utopian_fiction | influences | coexists | asymmetric (influences/coexists) |
| empathy_simulation → truth_procedure | coexists | influences | asymmetric |
| empathy_simulation → predictive_synthesis | influences | coexists | asymmetric |
| empirical_precedent → truth_procedure | influences | coexists | asymmetric |
| **empirical_precedent → utopian_fiction** | **forecloses** | coexists | **miscoded_asymmetry candidate** |
| **predictive_synthesis → utopian_fiction** | **forecloses** | coexists | **miscoded_asymmetry candidate** |

Non-coexists pairs (not in the 13): scholarship–predictive_synthesis
(influences/influences), truth_procedure–utopian_fiction (influences/influences).

Obstruction witness (live run): `cs_kernel_obstruction(fiat_efficacy_kernel): H1r=2
ClosureN=2 PluralityN=13 status=real_closure` — the two forecloses-asymmetry pairs are
counted in BOTH ClosureN and PluralityN (the predicate counts the two independently),
as the plan stated.

## 3. Fiat family — commitment inventory (the judged tier's raw material)

Each reading: 2 authored axioms (role, status, grounding) + 1 reference frame
(verbatim in `recon_live.log`):

| reading | foundational axiom (grounding) | second axiom (role, grounding) | reference frame |
|---|---|---|---|
| scholarship | efficacy_located_in_knowledge_production (instrumental) | public_discourse_shift_constitutes_political_action (secondary, empirically_contingent) | fiat_as_simulated_binding_action |
| empathy_simulation | efficacy_located_in_participant_psychology_not_enactment (empirically_contingent) | policy_realism_irrelevant_to_value_of_simulation (secondary, instrumental) | adversarial_perspective_taking_as_pedagogy |
| empirical_precedent | efficacy_requires_documented_causal_chain (empirically_contingent) | historical_analogy_licenses_present_action_warrant (secondary, instrumental) | policy_debate_stock_issues_tradition |
| truth_procedure | efficacy_located_in_subjective_ontological_break (deontological) | state_responsiveness_irrelevant_to_success_criterion (secondary, conventional) | policy_debate_pre_critical_turn |
| predictive_synthesis | efficacy_located_in_process_rigor_not_outcome_realism (instrumental) | reckless_experimentation_must_be_methodologically_guarded_against (foundational, conventional) | political_theory_as_distinct_disciplined_practice |
| utopian_fiction | value_located_in_practice_not_formal_power (conventional) | efficacy_question_bracketed_not_answered (foundational, instrumental) | debate_as_pedagogical_criticism_practice |

All axiom statuses are `holdable`.

**Contradiction joins** (`fiat_efficacy_kernel_contradictions.pl`, 4 directed facts = 2
unordered, each with prose rationale in the file):

1. `efficacy_located_in_participant_psychology_not_enactment` (empathy_simulation) ↔
   `efficacy_requires_documented_causal_chain` (empirical_precedent) — **this pair is
   mutual coexists_with**: the M1 must-fire target.
2. `efficacy_located_in_subjective_ontological_break` (truth_procedure) ↔
   `value_located_in_practice_not_formal_power` (utopian_fiction) — this pair is
   **influences/influences, no coexists edge**: the M1 must-NOT-fire negative control.

**A1 zero-mutation candidate pool** (recon evidence only; selection happens in the
prereg): mutual-coexists pairs with no declared contradiction between their axiom sets —
6 of the 7 mutual pairs (all but empathy_simulation–empirical_precedent):
scholarship–empathy_simulation, scholarship–empirical_precedent, scholarship–truth_procedure,
empathy_simulation–utopian_fiction, empirical_precedent–predictive_synthesis,
truth_procedure–predictive_synthesis. The axiom/grounding profiles above are the
selection substrate.

## 4. CP substrate (`archives/datasets/kernel_test/`, overlay confirmed 229 constraints)

**The planned control triplet — kernel `state_execution_authority`**
(abolition_reading, retributive_reading, deterrence_reading): 3 mutual coexists_with
pairs, all targets bare/`exact`. **Zero `cs_axiom` facts authored in all three files**
(`axioms: NONE AUTHORED` ×3), no `cs_axiom_contradiction` facts owned by this kernel,
and no contradictions file names it. Evidence base per reading: 1 `cs_reference_frame`
(classical_punitive_authority / natural_law_proportional_justice /
classical_deterrence_theory), `cs_drift_state`, and the story's own narrative/metric
content. **Operator rider 3's condition is CONFIRMED**: the blind judge works from a
strictly thinner authored-commitment base than the main instance has for fiat.

**Found during recon — a second, axiom-bearing CP family in the same archive:**
kernel `state_killing_authority` (retributive_desert, categorical_abolition,
deterrence_instrument; root-level kernel_test files), all bare targets, each reading
carrying 2 axioms + groundings + frame, with its own contradictions file
(`state_killing_authority_contradictions.pl`):

- categorical_abolition | deterrence_instrument: **mutual coexists_with AND declared
  contradiction** (`inalienable_life` ↔ `deterrence_empirical_efficacy`) — same
  structural shape as fiat's flagged pair.
- deterrence_instrument | retributive_desert: mutual coexists_with, no declared
  contradiction between their atoms.
- retributive_desert | categorical_abolition: mutual **forecloses** (+ contradiction
  `murderers_forfeit_rights` ↔ `inalienable_life`) — real closure, not a coexists pair.

This family offers a CP control arm whose evidence base matches fiat's (axioms +
contradictions + frames), which the planned triplet cannot. Whether to add it is an
R2 decision (see PROPOSAL.md); the plan's pinned control remains the triplet.

## 5. Un-routed consumer sites (verified against HEAD this session)

- `cs_pattern_detection.pl:348–357` `cs_displaced_beneficiary/1`: line 355 raw-matches
  `cs_reading_relation(UID, Sibling, forecloses)` then requires `cs_has_fields(Sibling)`
  — a prefixed target atom fails `cs_has_fields`, so the clause is silently dead on
  skewed families (the live leg's 18 prefixed forecloses edges).
- `cs_corpus_analysis.pl:123–156` (axiom-conflict closure/plurality split): lines
  131–132, 138–139, 146–149 raw-match `cs_reading_relation(UID, MemberName, Rel)` —
  dead on prefixed families; output is console-only (`format/2`), no pipeline artifact.
  Note the plan's line range 131–149 covers the raw matches; the enclosing computation
  is 123–156.
- `drl_composition.pl:120–126` `detect_necessity_inheritance/2`: line 122 raw-matches
  AND binds `Source` to a constraint id where `cs_reading_relation/3` is UID-keyed —
  the join can essentially never fire on real corpus data. Per plan: no edit; mint its
  own OQ (engine-design question: what should Source bind to). Allowlisted at
  `axis_boundary_allowlist.txt:29` (verify at Phase D).

## 6. Stale premises in the OQ-262 entry (to correct at close, per plan)

Recon-relevant confirmations: the live corpus is 235 constraints as of these runs (the
CLAUDE.md file-count 199 is dated 2026-07-24; cite the manifest, not either number, for
any pipeline run); the fiat edges are NOT in quarantine — all 30 resolve against the
live corpus (the quarantine JSON is a per-generation-run artifact). The remaining stale
premises (OQ-23 status, CP divergence 253/468 → 164, 2-vs-3 un-routed consumers) are
documented in the plan and carried to the writeup; recon adds: the un-routed consumer
count is 3 as the plan states (site list above).

## 7. What is answerable

- Mechanical tier M1 (contradiction-join) and M2 (relation-asymmetry) have exact,
  witnessed fire/no-fire targets on this substrate (§2–§3), including a genuine
  negative control for M1 (truth_procedure–utopian_fiction) and exactly 2 M2 targets.
- The judged tier has full raw material for 13 fiat pairs (axioms, groundings, frames,
  contradiction prose, directional profiles) and 3 CP-triplet pairs (frames + story
  content only — rider 3 asymmetry stands).
- The A1 zero-mutation control has a 6-pair candidate pool (§3).
- Phase-D routing diffs have a pre-derived movement surface (§1): live-leg-only, 18
  forecloses / 100 coexists prefixed edges; twins and kernel_test predict zero movement.
