% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree as Sufficient for Practice Displacement (Exogenous Override Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint describes the belief and practice that state decree
 *   authority is inherently sufficient to displace prior cultural or social
 *   practices, with compliance following directly from legal mandate,
 *   irrespective of whether the new practices are internalized by the
 *   population. It reflects a top-down, positivist view of state power in
 *   processes of modernization or cultural imposition. The
 *   'exogenous_override_reading' emphasizes the state's capacity to impose
 *   change from the outside, often through legal abolition of old practices
 *   and coercive enforcement of new ones, even if this leads to rural
 *   non-compliance and practical workarounds.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: Agenda setter (institutional/generational) — issues decrees, enforces compliance.
 *   - rural_populations: Payer (powerless/generational) — bears adjustment costs, faces coercive enforcement, may engage in non-compliance.
 *   - traditional_elites: Payer (powerful/generational) — loses status and influence as prior practices are undermined.
 *   - state_modernization_agenda: Beneficiary (analytical/civilizational) — the abstract goal that benefits from the perceived success of imposed practices.
 *   - sociological_observers: Observer (analytical/civilizational) — analyzes the actual dynamics of compliance and internalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.75).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree as Sufficient for Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'aaeaa613-d071-4dff-bc26-f45e09519244').
narrative_ontology:cs_kernel_codification('aaeaa613-d071-4dff-bc26-f45e09519244', formalized).
narrative_ontology:cs_authority_grounding('aaeaa613-d071-4dff-bc26-f45e09519244', extraction).
narrative_ontology:cs_interpretation_layer_present('aaeaa613-d071-4dff-bc26-f45e09519244').
narrative_ontology:cs_reading_relation('aaeaa613-d071-4dff-bc26-f45e09519244', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('aaeaa613-d071-4dff-bc26-f45e09519244', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('aaeaa613-d071-4dff-bc26-f45e09519244', foundational, state_decree_is_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(state_decree_is_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('aaeaa613-d071-4dff-bc26-f45e09519244', state_decree_is_sufficient_for_compliance, conventional).
narrative_ontology:cs_axiom('aaeaa613-d071-4dff-bc26-f45e09519244', secondary, internalization_is_irrelevant_to_legitimacy).
narrative_ontology:cs_axiom_status(internalization_is_irrelevant_to_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('aaeaa613-d071-4dff-bc26-f45e09519244', internalization_is_irrelevant_to_legitimacy, deontological).
narrative_ontology:cs_reference_frame('aaeaa613-d071-4dff-bc26-f45e09519244', legal_positivist_state_supremacy).
narrative_ontology:cs_drift_state('aaeaa613-d071-4dff-bc26-f45e09519244', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aaeaa613-d071-4dff-bc26-f45e09519244', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified legal and social order across diverse populations, replacing disparate local practices with a single, centrally mandated set of norms and institutions, thereby facilitating state-led modernization and national integration.
% TRANSFER_FUNCTION: Transfers authority over social and cultural practices from local communities and traditional elites to the central state. It also transfers the costs of adjusting to new norms and the burden of compliance onto rural populations, while accruing symbolic and material benefits (e.g., control, resources, legitimacy) to the state bureaucracy and its modernization agenda.
% ABSENT_VOICES: Local community leaders, cultural preservationists, and advocates for indigenous rights are often excluded. They would argue for the value of prior practices, the right to self-determination in cultural matters, and the social costs of imposed change. Their absence allows the state's narrative of progress to dominate without direct challenge.
% DISAPPEARANCE_RATIONALE: If the belief in state decree's sufficiency and its enforcement vanished, the state's capacity to impose new practices would collapse. Prior practices would likely re-emerge or adapt, local autonomy would increase, and the state's modernization agenda would face significant challenges, forcing a renegotiation of its relationship with diverse populations.
% FOUNDING_PROBLEM: The problem of establishing a unified national identity and administrative control over diverse, often fragmented, populations with disparate local customs and legal systems, which were perceived as obstacles to 'modern' state-building and economic development.
% FOUNDING_PROBLEM_CORROBORATION: State historians and official narratives attest that the problem of national unity and modernization remains live, justifying continued top-down imposition. However, postcolonial scholars, anthropologists, and local community leaders (from outside the benefiting parties) argue that the original problem has either been superseded by new challenges or was fundamentally misdiagnosed, and that the constraint now primarily serves to maintain state power and extract resources rather than genuinely solve societal problems.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate a new social order (modernization, national unity) but does so through asymmetric extraction and coercion. Extractiveness (0.65) is high due to the costs imposed on populations forced to abandon established practices and adopt new ones without consultation. Suppression (0.75) is also high, reflecting the active enforcement required to maintain compliance against resistance. The theater ratio (0.4) indicates that a significant portion of enforcement is performative, aimed at demonstrating state authority rather than achieving genuine internalization, especially when non-compliance persists through workarounds. Accessibility collapse is moderate (0.4) as alternatives (prior practices) are legally abolished but persist informally, and resistance is moderate (0.6) due to active, though often covert, non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   The state bureaucracy (agenda setter) perceives this as a legitimate and effective mechanism for progress, viewing any resistance as backwardness to be overcome. Rural populations (payers) experience it as an imposition, bearing the costs of adjustment and often finding ways to circumvent or resist the new norms. The state modernization agenda (beneficiary) is an abstract entity that 'benefits' from the appearance of compliance, regardless of its depth.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and the abstract 'state_modernization_agenda' are the primary beneficiaries (low directionality), as they gain from the perceived success of the imposed practices and the consolidation of state power. Rural populations and traditional elites are the primary targets (high directionality), bearing the direct costs of compliance, loss of autonomy, and cultural disruption. The active enforcement mechanism ensures that the costs are borne by the targets, while the benefits accrue to the state's objectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the coercive extraction) or a pure Snare (ignoring the genuine, if contested, coordination goal of state modernization). The 'tangled_rope' accurately captures the hybrid nature: a coordination function (modernization) is pursued through an extractive mechanism (imposed practices, coercive enforcement) that benefits the state while imposing costs on specific populations. Mandatrophy would occur if the 'modernization' goal became entirely theatrical, with enforcement serving only to maintain state power without any genuine societal benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of state authority''s capacity to override prior practice, or is it a specific reading of the ''legitimacy_of_imposed_practice'' kernel?',
    'Recognizing this as the ''exogenous_override_reading'' of the ''legitimacy_of_imposed_practice'' kernel, acknowledging sibling readings like ''endogenous_climb_reading'' and ''hybrid_scaffolding_reading''.',
    'Framing this as a specific reading clarifies that its claims about compliance and displacement are not universally accepted, but rather represent a particular theoretical or historical stance on state power and cultural change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''exogenous_override_reading'' of the ''legitimacy_of_imposed_practice'' kernel.').

omega_variable(
    displacement_completeness_ambiguity,
    'To what extent does legal abolition truly displace prior practice, given rural non-compliance and practical workarounds?',
    'Longitudinal ethnographic studies tracking the persistence of ''abolished'' practices in daily life, and quantitative analysis of enforcement efficacy versus actual behavioral change.',
    'If displacement is consistently incomplete, the constraint''s effective suppression and extractiveness are lower than claimed, and its ''tangled_rope'' classification leans towards ''piton'' due to performative enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_completeness_ambiguity, empirical, 'Ambiguity regarding the completeness of practice displacement by state decree.').

omega_variable(
    coercion_vs_internalization_balance,
    'What is the actual balance between coercive enforcement and any emergent internalization of new practices among the affected populations?',
    'Sociological surveys and qualitative interviews with affected populations to gauge acceptance, belief, and integration of new norms, contrasted with observed compliance under duress.',
    'If internalization is negligible and compliance is purely coercive, the constraint''s ''tangled_rope'' classification shifts closer to ''snare''; if some internalization is observed, the coordination function is partially vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_internalization_balance, empirical, 'Balance between coercive enforcement and internalization of new practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
