% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Norms (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process of state-imposed normativity, where
 *   new social and legal norms are established through the coercive power of
 *   a central state, rather than through organic cultural acceptance.
 *   Legitimacy is derived from the state's monopoly on violence, and
 *   compliance is primarily a function of active monitoring and enforcement.
 *   This is the 'exogenous override' reading of the
 *   imposition_mechanism_kernel, emphasizing top-down coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.85).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norms (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '49600f70-03df-4b00-9195-d8945fe5ef26').
narrative_ontology:cs_kernel_codification('49600f70-03df-4b00-9195-d8945fe5ef26', formalized).
narrative_ontology:cs_authority_grounding('49600f70-03df-4b00-9195-d8945fe5ef26', extraction).
narrative_ontology:cs_interpretation_layer_present('49600f70-03df-4b00-9195-d8945fe5ef26').
narrative_ontology:cs_reading_relation('49600f70-03df-4b00-9195-d8945fe5ef26', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('49600f70-03df-4b00-9195-d8945fe5ef26', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('49600f70-03df-4b00-9195-d8945fe5ef26', foundational, state_monopoly_on_violence_is_primary_legitimator).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_primary_legitimator, holdable).
narrative_ontology:cs_axiom_grounding('49600f70-03df-4b00-9195-d8945fe5ef26', state_monopoly_on_violence_is_primary_legitimator, conventional).
narrative_ontology:cs_axiom('49600f70-03df-4b00-9195-d8945fe5ef26', secondary, cultural_acceptance_is_secondary_to_coercion).
narrative_ontology:cs_axiom_status(cultural_acceptance_is_secondary_to_coercion, holdable).
narrative_ontology:cs_axiom_grounding('49600f70-03df-4b00-9195-d8945fe5ef26', cultural_acceptance_is_secondary_to_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('49600f70-03df-4b00-9195-d8945fe5ef26', rational_state_consolidation).
narrative_ontology:cs_drift_state('49600f70-03df-4b00-9195-d8945fe5ef26', post_enlightenment_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49600f70-03df-4b00-9195-d8945fe5ef26', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, local_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that promulgates new norms, enforces them through its monopoly on violence (military, police, courts), and benefits from the consolidation of power and revenue that these norms enable. It actively suppresses dissent and alternative forms of social organization.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The political and economic beneficiaries of the new state-imposed norms. They gain privileged access to resources, legal protections, and social status under the new order, and actively support the state's enforcement efforts.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite, beneficiary,
    powerful, biographical, mobile, national).

% The primary targets of the new norms. They are forced to abandon traditional practices, pay new taxes, or conform to unfamiliar legal codes, often under threat of violence. Their resistance is met with state suppression, and their options for exit are severely limited.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Local or regional leaders whose authority and status are undermined by the new state-imposed norms. They lose influence, resources, and the ability to adjudicate disputes according to customary law. They may attempt to resist or negotiate, but their power is dwarfed by the state.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites, excluded).

% Historians and sociologists who analyze the process of state formation and norm imposition. They can discern the mechanisms of coercion and the contested nature of legitimacy, often long after the events have transpired.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates social behavior under a unified legal and administrative framework, replacing diverse local customs with a single, centrally enforced standard. This reduces internal friction for the state's administrative and extractive functions.
% TRANSFER_FUNCTION: Transfers authority, resources, and social control from local, customary systems to the central state apparatus and its allied elites. It extracts compliance and material wealth from local communities.
% ABSENT_VOICES: The voices of local communities and traditional elites, whose customary norms are being overridden, are actively suppressed or ignored in the state's legitimation narrative. They would articulate the costs of forced compliance and the loss of self-governance.
% DISAPPEARANCE_RATIONALE: If the state's coercive power and its imposed norms vanished overnight, local communities would likely revert to customary practices or develop new, more organically derived norms. The centralized administrative and extractive structures would collapse, leading to a rapid decentralization of authority and a reorganization of social life.
% FOUNDING_PROBLEM: The state sought to consolidate its power, rationalize administration, and extract resources more efficiently across a diverse territory, which was hindered by fragmented local norms and competing authorities.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from state archives and accounts from the ruling elite consistently frame the founding problem as one of 'order' and 'unity.' However, ethnographic studies and local histories (from outside the benefiting parties) reveal that for local communities, the 'problem' was the state's encroachment itself, not the prior diversity of norms.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the norms are designed to benefit the state and ruling elite, often at the expense of local communities. Suppression is very high (0.90) as the state actively uses force to overcome resistance and enforce compliance. Theater ratio is low (0.20) because the state's claims of 'order' and 'progress' are largely cover for direct power consolidation; the enforcement is quite real and functional for its extractive purpose. Accessibility collapse is moderate (0.70) as alternatives are suppressed but not entirely eliminated, leading to ongoing resistance (0.75).
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, these norms are necessary for 'civilization' and 'order,' justifying the use of force. From the perspective of local communities, they are an alien imposition, a source of extraction and oppression. The engine's per-seat classification will reflect this divergence, with the state as a beneficiary and local communities as victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and ruling elite are clear beneficiaries, as the norms consolidate their power and wealth (low directionality). Local communities and traditional elites are clear targets, bearing the costs of compliance and loss of autonomy (high directionality). Historical observers maintain an analytical distance, aiming for symmetric directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare, not a rope or scaffold. Its persistence is not due to a genuine coordination problem for all parties, nor is it temporary. The 'founding problem' of fragmentation is framed by the state to justify its extractive and suppressive actions. The high and sustained extractiveness and suppression, coupled with ongoing resistance, indicate that the mandate is a cover for a coercive structure, not a decaying coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'To what extent did the state-imposed norms eventually gain genuine cultural acceptance, shifting their legitimacy source from coercion to internalized consent?',
    'Longitudinal ethnographic studies and analysis of legal compliance rates after the initial period of intense enforcement. If compliance persists without active monitoring, it suggests internalized legitimacy.',
    'If significant cultural acceptance occurred, the constraint might transition from a Snare to a Tangled Rope or even a Rope over time, as the coercive overhead decreases and the coordination function becomes more salient. If not, it remains a Snare sustained by force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Ambiguity regarding the long-term source of legitimacy for state-imposed norms.').

omega_variable(
    resistance_effectiveness_ambiguity,
    'Was the observed resistance primarily symbolic, or did it impose significant costs on the state, potentially altering the norms or their enforcement?',
    'Detailed historical analysis of state expenditures on enforcement, records of successful local rebellions or negotiations, and changes in norm content over time that reflect concessions to local demands.',
    'If resistance was highly effective, the state''s extractiveness might have been capped or even reduced, and the constraint''s classification might lean more towards a contested Tangled Rope. If resistance was largely ineffective, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_effectiveness_ambiguity, empirical, 'Uncertainty about the actual impact and effectiveness of local resistance against state-imposed norms.').

omega_variable(
    framing_underdetermination_coercion_vs_coordination,
    'Is the state''s ''coordination'' narrative a genuine, if secondary, function, or purely a cover story for extraction?',
    'Comparative analysis with states that achieved similar coordination outcomes with significantly lower coercive overhead. If such cases exist, it suggests the ''coordination'' here is largely a cover.',
    'If the coordination function is genuinely secondary, the Snare classification is robust. If it''s found to be a significant, albeit coercive, coordination, it might lean towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_underdetermination_coercion_vs_coordination, conceptual, 'Ambiguity in distinguishing genuine coordination from a cover story for coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 1600, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1600, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement(impo_tr_t1640, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1640, 0.22).
narrative_ontology:measurement(impo_tr_t1680, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1680, 0.2).
narrative_ontology:measurement(impo_tr_t1720, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1720, 0.18).
narrative_ontology:measurement(impo_tr_t1760, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1760, 0.19).
narrative_ontology:measurement(impo_tr_t1800, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1800, 0.2).

% Extraction over time
narrative_ontology:measurement(impo_be_t1600, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1600, 0.75).
narrative_ontology:measurement(impo_be_t1640, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1640, 0.8).
narrative_ontology:measurement(impo_be_t1680, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1680, 0.85).
narrative_ontology:measurement(impo_be_t1720, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1720, 0.87).
narrative_ontology:measurement(impo_be_t1760, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1760, 0.88).
narrative_ontology:measurement(impo_be_t1800, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1800, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1600, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(impo_su_t1640, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1640, 0.85).
narrative_ontology:measurement(impo_su_t1680, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1680, 0.9).
narrative_ontology:measurement(impo_su_t1720, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1720, 0.92).
narrative_ontology:measurement(impo_su_t1760, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1760, 0.9).
narrative_ontology:measurement(impo_su_t1800, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1800, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
