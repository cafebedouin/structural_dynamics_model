% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb of Norms (State Mandate Follows Acceptance)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'endogenous climb' reading of the
 *   imposition_mechanism_kernel, focusing on scenarios where new norms gain
 *   legitimacy through bottom-up popular adoption before being formalized by
 *   state mandate. This reading contrasts with 'exogenous override' (state
 *   coercion as primary) and 'hybrid legitimation' (mixed mechanisms). The
 *   constraint is classified as a Rope, reflecting its genuine coordination
 *   function and minimal extraction, as the state's role is primarily to
 *   recognize and coordinate existing social consensus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.15).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.1).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Climb of Norms (State Mandate Follows Acceptance)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, '96793bc8-d381-4c96-b305-806082a0d2db').
narrative_ontology:cs_kernel_codification('96793bc8-d381-4c96-b305-806082a0d2db', implicit).
narrative_ontology:cs_authority_grounding('96793bc8-d381-4c96-b305-806082a0d2db', practice).
narrative_ontology:cs_interpretation_layer_present('96793bc8-d381-4c96-b305-806082a0d2db').
narrative_ontology:cs_reading_relation('96793bc8-d381-4c96-b305-806082a0d2db', imposition_mechanism_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('96793bc8-d381-4c96-b305-806082a0d2db', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('96793bc8-d381-4c96-b305-806082a0d2db', foundational, legitimacy_from_popular_consent).
narrative_ontology:cs_axiom_status(legitimacy_from_popular_consent, holdable).
narrative_ontology:cs_axiom_grounding('96793bc8-d381-4c96-b305-806082a0d2db', legitimacy_from_popular_consent, conventional).
narrative_ontology:cs_axiom('96793bc8-d381-4c96-b305-806082a0d2db', secondary, state_mandate_as_recognition).
narrative_ontology:cs_axiom_status(state_mandate_as_recognition, holdable).
narrative_ontology:cs_axiom_grounding('96793bc8-d381-4c96-b305-806082a0d2db', state_mandate_as_recognition, conventional).
narrative_ontology:cs_reference_frame('96793bc8-d381-4c96-b305-806082a0d2db', spontaneous_social_order).
narrative_ontology:cs_drift_state('96793bc8-d381-4c96-b305-806082a0d2db', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('96793bc8-d381-4c96-b305-806082a0d2db', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, adopting_populace).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, state_apparatus).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily adopted the new norms due to perceived benefits in social coordination and identity formation. Experienced minimal friction and gained from shared understanding. Their acceptance is the primary driver of the norm's legitimacy.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, adopting_populace, beneficiary,
    moderate, biographical, mobile, regional).

% Formalized the already-accepted norms into law or policy, thereby gaining legitimacy and strengthening its own authority by aligning with popular sentiment. Its role was to codify and coordinate, not to coerce initial adoption.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Lost cultural authority and influence as the new norms superseded older traditions. While not directly 'extracted from' in a monetary sense, their social capital and power base diminished. Their resistance was largely ineffective against widespread popular adoption.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, traditional_elites, payer,
    powerful, biographical, constrained, regional).

% Analyze the historical processes of norm formation and state legitimation, seeking to understand the causal mechanisms behind cultural shifts and state power. They are external to the direct operation of the constraint.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a new, widely accepted framework of social conduct and cultural values, enabling more efficient and harmonious interaction across society by providing shared expectations and a common moral language.
% TRANSFER_FUNCTION: Transferred cultural authority and social capital from older, traditional elites and norms to the new, popularly adopted norms and, by extension, to the state apparatus that formalized them. Legitimacy flowed from the populace to the state.
% ABSENT_VOICES: Those deeply invested in the superseded traditional norms, who might have seen the new norms as a moral decline or an erosion of heritage, were marginalized by the widespread popular acceptance and the state's subsequent formalization.
% DISAPPEARANCE_RATIONALE: If these norms and their legitimation process vanished, the social fabric would lose its coherence, leading to fragmentation, conflict, and a crisis of state legitimacy. The historical trajectory of the society would be fundamentally altered, as the state's authority was built upon this 'endogenous climb' of norms.
% FOUNDING_PROBLEM: The society faced a period of cultural fragmentation or a perceived inadequacy of existing norms to address new social realities, leading to a need for a new, unifying framework of values and behaviors.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, sociological analyses, and contemporary accounts from the period (e.g., public intellectuals, religious leaders, chroniclers) corroborate the existence of social fragmentation and the perceived need for new norms, supporting the idea that the problem was genuinely live for the populace, not merely asserted by the state.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the norms are genuinely accepted and provide widespread benefits, with minimal costs imposed. Suppression is low (0.10) as the state's mandate follows, rather than precedes, popular acceptance, meaning coercion is not the primary mechanism of persistence. Theater ratio is negligible (0.05) because the constraint's function is authentic and widely embraced. Accessibility collapse is moderate (0.60) as alternative norms existed but were naturally superseded by the new, more widely adopted ones. Resistance is low (0.08) due to the bottom-up nature of adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the adopting populace and the state, this process is a beneficial evolution of social order. From the perspective of traditional elites, it represents a loss of their established authority and a decline of older values. The engine's per-seat classification would reflect this divergence, with beneficiaries experiencing a Rope-like structure and traditional elites experiencing a more Snare-like or Piton-like structure due to their loss of power.
 *
 * DIRECTIONALITY LOGIC:
 *   The adopting populace is a clear beneficiary (d near 0.0) as they gain from enhanced social coordination and shared identity. The state apparatus is also a beneficiary (d near 0.0-0.1) as it gains legitimacy and stability by formalizing popular norms. Traditional elites are the primary 'payers' (d near 0.8-0.9) as they lose cultural authority and influence, even if not directly extracted from financially.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subtle_coercion_ambiguity,
    'To what extent was the ''bottom-up adoption'' influenced by subtle, non-state forms of coercion or social pressure that are not captured by the low suppression metric?',
    'Detailed micro-historical analysis of social networks, opinion leader influence, and economic incentives at the local level, distinguishing genuine voluntary adoption from diffuse social pressure.',
    'If significant subtle coercion is found, the effective suppression and extractiveness of the constraint would be higher, potentially shifting its classification towards a Tangled Rope or even Snare, as the ''endogenous'' nature would be compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subtle_coercion_ambiguity, empirical, 'Distinguishing genuine popular acceptance from subtle social coercion.').

omega_variable(
    measurement_of_popular_acceptance,
    'How reliably can ''popular acceptance'' be measured in historical contexts, and are the indicators used truly reflective of widespread voluntary adoption rather than mere compliance or lack of organized dissent?',
    'Comparative historical studies using diverse sources (diaries, local records, folklore, material culture) to triangulate evidence of genuine behavioral change and expressed sentiment, rather than relying solely on official decrees or elite narratives.',
    'If ''acceptance'' is found to be more akin to passive compliance, the constraint''s extractiveness and suppression would be re-evaluated upwards, challenging the Rope classification and potentially aligning it more with a Tangled Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_popular_acceptance, empirical, 'Reliability of historical evidence for ''popular acceptance''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 1700, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1700, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1700, 0.03).
narrative_ontology:measurement(impo_tr_t1710, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1710, 0.04).
narrative_ontology:measurement(impo_tr_t1720, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1720, 0.04).
narrative_ontology:measurement(impo_tr_t1730, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1730, 0.05).
narrative_ontology:measurement(impo_tr_t1740, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1740, 0.05).
narrative_ontology:measurement(impo_tr_t1750, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1750, 0.05).

% Extraction over time
narrative_ontology:measurement(impo_be_t1700, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(impo_be_t1710, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1710, 0.12).
narrative_ontology:measurement(impo_be_t1720, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1720, 0.13).
narrative_ontology:measurement(impo_be_t1730, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1730, 0.14).
narrative_ontology:measurement(impo_be_t1740, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1740, 0.15).
narrative_ontology:measurement(impo_be_t1750, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1750, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1700, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(impo_su_t1710, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1710, 0.07).
narrative_ontology:measurement(impo_su_t1720, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1720, 0.08).
narrative_ontology:measurement(impo_su_t1730, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1730, 0.09).
narrative_ontology:measurement(impo_su_t1740, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1740, 0.1).
narrative_ontology:measurement(impo_su_t1750, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1750, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'imposition_mechanism_kernel', each representing a different primary mechanism by which new norms achieve legitimacy and are formalized by the state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
