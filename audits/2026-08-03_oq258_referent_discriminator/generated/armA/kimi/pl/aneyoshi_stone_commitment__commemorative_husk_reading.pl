% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Commitment — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone warns against building below its elevation. In
 *   the commemorative husk reading, this commitment system decayed across the
 *   twentieth century into pure symbolic observance: land-use decisions are
 *   made independently of the stone, the village's 2011 survival is
 *   attributed to luck and terrain rather than to the inscription's
 *   operational force, and the stone functions as a museum piece maintained
 *   by heritage administrators. The reading treats the constraint as an
 *   atrophied institutional form whose protective function has evaporated,
 *   leaving only theatrical maintenance.
 *
 * KEY AGENTS:
 *   - village_residents: Primary payers (moderate/constrained) — bear diffuse maintenance and ceremonial costs without receiving protective coordination.
 *   - heritage_administrators: Agenda-setter (moderate/constrained) — administer the stone as heritage but could redirect resources; inertia sustains their role.
 *   - disaster_anthropologists: Analytical observer (analytical/analytical) — sees the full decay trajectory and attributes survival to non-stone factors.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.76).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.2).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone Commitment — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '0d4399d6-f05a-4e0f-9b79-6c124cc22d1a').
narrative_ontology:cs_kernel_codification('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', fixed_text).
narrative_ontology:cs_authority_grounding('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', lineage).
narrative_ontology:cs_interpretation_layer_present('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a').
narrative_ontology:cs_reading_relation('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', aneyoshi_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', foundational, commemorative_function_primary).
narrative_ontology:cs_axiom_status(commemorative_function_primary, holdable).
narrative_ontology:cs_axiom_grounding('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', commemorative_function_primary, conventional).
narrative_ontology:cs_axiom('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', foundational, behavioral_compliance_unsustained).
narrative_ontology:cs_axiom_status(behavioral_compliance_unsustained, holdable).
narrative_ontology:cs_axiom_grounding('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', behavioral_compliance_unsustained, empirically_contingent).
narrative_ontology:cs_reference_frame('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', commemorative_continuity_framework).
narrative_ontology:cs_drift_state('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', post_2011_tsunami_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0d4399d6-f05a-4e0f-9b79-6c124cc22d1a', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, village_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the ongoing costs of stone maintenance, ceremonial observance, and heritage compliance. Their residential and land-use decisions are made independently of the stone's warning, yet they continue to supply labor and funds to sustain its symbolic presence out of cultural inertia and inherited obligation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, village_residents, payer,
    moderate, generational, constrained, local).

% Administer the stone as a formal heritage asset, organize commemorative rites, and allocate municipal resources to its upkeep. They possess the bureaucratic authority to reclassify or de-prioritize the site but lack incentive to do so; their role persists through ceremonial obligation and institutional inertia rather than demonstrated protective efficacy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, heritage_administrators, agenda_setter,
    moderate, biographical, constrained, local).

% Analyze the stone as a case study in commitment decay, comparing its current commemorative function against its original protective intent. They attribute the village's 2011 survival to terrain, modern education, and chance rather than to the stone's operational force.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated intergenerational avoidance of tsunami-vulnerable settlement zones by inscribing a durable, publicly visible warning; in its current form it coordinates no protective land-use behavior, only ceremonial remembrance.
% TRANSFER_FUNCTION: Moves labor and material resources from village residents to the maintenance of a memorial artifact; returns no protective land-use coordination to the settlement.
% ABSENT_VOICES: Modern disaster-risk engineers and hydrodynamic modelers who would advocate for probabilistic tsunami-risk zoning in place of symbolic commemoration; younger residents who regard the stone as historically interesting but functionally irrelevant to contemporary building decisions.
% DISAPPEARANCE_RATIONALE: While land-use patterns would remain unchanged because they are already independent of the stone, the village's ceremonial calendar, heritage identity, and intergenerational narrative would lose their central anchor. Social arrangements tied to the stone's symbolic presence would reorganize around alternative memorial practices or attrit.
% FOUNDING_PROBLEM: To prevent resettlement in low-lying areas devastated by the 1896 Meiji tsunami by transmitting catastrophic risk memory across generations through a fixed, physically durable warning.
% FOUNDING_PROBLEM_CORROBORATION: Historical geologists and disaster historians outside the village attest to the 1896 tsunami event and the stone's original protective intent. Anthropologists document the gradual shift from behavioral compliance to symbolic observance during the late twentieth century. Heritage administrators acknowledge the stone's current commemorative framing, though they do not characterize it as a failure of the original mandate.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.76) because the standing arrangement consumes ongoing resources—labor, funding, ceremonial time—while delivering zero protective land-use coordination. Theater ratio is very high (0.85) because nearly all remaining activity is performative: the stone is polished, photographed, and ritually acknowledged, but no builder consults it. Suppression is low (0.20) because the constraint persists without active enforcement; land-use planners ignore it with impunity. Resistance is minimal (0.10) because no actor fights a memorial. The claim is piton: the constraint is an inertial husk of a once-functional coordination mechanism. The metrics and claim are authored independently; the engine may compute divergent per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The heritage administrator seat experiences the constraint as legitimate cultural stewardship with low personal cost, while the resident seat experiences it as a diffuse, inherited tax on community resources. The anthropological observer seat sees the full structural gap between performed reverence and functional vacuity. The engine will compute different effective extraction across these positions due to power and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the constraint lacks concentrated rent capture: the heritage administrators do not personally profit, and the residents' costs are diffuse rather than extractive in the snare sense. Directionality therefore reverts to the canonical fallback per power atom. Moderate-power payers (residents) and agenda-setters (administrators) receive symmetric default directionality, while the analytical observer receives the analytical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—tsunami-risk avoidance through intergenerational warning—is dead, yet the arrangement persists. This prevents misclassifying the current form as a rope (it does not coordinate protective behavior) or as a snare (there is no active coercion or concentrated beneficiary). The piton classification captures that the constraint survives by institutional inertia and theatrical maintenance alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_2011,
    'Is the village''s 2011 survival attributable to the stone''s behavioral influence on settlement location, or to independent factors such as terrain, modern education, and chance?',
    'Archival land-use records and oral-history triangulation to establish whether building permits and residential decisions after 1933 were systematically constrained by the stone''s elevation marker.',
    'If survival is attributable to the stone, the constraint''s epsilon and type would shift toward tangled_rope or rope; if attributable to independent factors, the commemorative husk reading and its high-epsilon piton classification are corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_2011, empirical, 'Whether the stone causally influenced 2011 survival').

omega_variable(
    diffuse_cost_or_extraction,
    'Does the diffuse cost of maintaining a non-functional memorial constitute extractive overhead in the DR framework, or is it merely voluntary cultural expenditure that should register as near-zero epsilon?',
    'Comparative analysis of maintenance cost burden relative to household income and to the cost of modern disaster-preparedness alternatives; community survey of perceived obligation versus willingness to pay.',
    'If the cost is experienced as coerced inheritance rather than voluntary culture, epsilon stays high and the piton reading is reinforced; if voluntary, epsilon should be revised downward toward a low-extraction piton or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_cost_or_extraction, conceptual, 'Whether diffuse maintenance costs count as extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(aney_tr_t12, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(aney_tr_t24, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(aney_tr_t36, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 36, 0.45).
narrative_ontology:measurement(aney_tr_t48, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 48, 0.58).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.7).
narrative_ontology:measurement(aney_tr_t72, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 72, 0.78).
narrative_ontology:measurement(aney_tr_t88, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 88, 0.85).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(aney_be_t12, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(aney_be_t24, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(aney_be_t36, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 36, 0.45).
narrative_ontology:measurement(aney_be_t48, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 48, 0.55).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(aney_be_t72, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 72, 0.7).
narrative_ontology:measurement(aney_be_t88, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 88, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(aney_su_t12, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(aney_su_t24, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(aney_su_t36, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 36, 0.2).
narrative_ontology:measurement(aney_su_t48, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 48, 0.15).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(aney_su_t72, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 72, 0.18).
narrative_ontology:measurement(aney_su_t88, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 88, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint and behavioral_competence_reading are two readings of the aneyoshi_stone_commitment kernel. They share the same referent—the inscribed stone and its social embedding—but diverge on empirical claims about causal efficacy and therefore on epsilon. Per the ε-invariance principle, they are authored as separate constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
