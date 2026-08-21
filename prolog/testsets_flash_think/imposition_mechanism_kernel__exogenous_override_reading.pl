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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: State-Imposed Norms via Coercion (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'exogenous override' reading of
 *   the imposition_mechanism_kernel. It describes a scenario where new norms
 *   are primarily established and maintained through the coercive power of a
 *   state, rather than through prior cultural acceptance. Legitimacy, in this
 *   reading, is derived from the state's monopoly on violence and its
 *   capacity to enforce compliance, with cultural acceptance being a
 *   secondary or non-existent factor. The metrics reflect high extraction and
 *   suppression, consistent with a Snare, as the state actively overrides
 *   existing social structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.85).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norms via Coercion (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'e592e6cc-cc7f-470d-8ab8-87ee288d3454').
narrative_ontology:cs_kernel_codification('e592e6cc-cc7f-470d-8ab8-87ee288d3454', formalized).
narrative_ontology:cs_authority_grounding('e592e6cc-cc7f-470d-8ab8-87ee288d3454', extraction).
narrative_ontology:cs_interpretation_layer_present('e592e6cc-cc7f-470d-8ab8-87ee288d3454').
narrative_ontology:cs_reading_relation('e592e6cc-cc7f-470d-8ab8-87ee288d3454', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('e592e6cc-cc7f-470d-8ab8-87ee288d3454', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('e592e6cc-cc7f-470d-8ab8-87ee288d3454', foundational, state_monopoly_on_violence_is_legitimacy).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e592e6cc-cc7f-470d-8ab8-87ee288d3454', state_monopoly_on_violence_is_legitimacy, instrumental).
narrative_ontology:cs_axiom('e592e6cc-cc7f-470d-8ab8-87ee288d3454', foundational, cultural_acceptance_is_secondary_to_force).
narrative_ontology:cs_axiom_status(cultural_acceptance_is_secondary_to_force, holdable).
narrative_ontology:cs_axiom_grounding('e592e6cc-cc7f-470d-8ab8-87ee288d3454', cultural_acceptance_is_secondary_to_force, empirically_contingent).
narrative_ontology:cs_reference_frame('e592e6cc-cc7f-470d-8ab8-87ee288d3454', state_monopoly_on_violence_as_legitimacy_source).
narrative_ontology:cs_drift_state('e592e6cc-cc7f-470d-8ab8-87ee288d3454', initial_imposition_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e592e6cc-cc7f-470d-8ab8-87ee288d3454', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body (military, bureaucracy, legal system) that directly implements and enforces the new norms, deriving its power and resources from this imposition. It benefits from the consolidation of central authority and the suppression of alternative power centers.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The political and economic class whose power and interests are directly served by the new norms. They benefit from the stability and predictability that state-enforced conformity brings, even if it's achieved through coercion rather than consent.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite, beneficiary,
    powerful, generational, mobile, national).

% The diverse groups within the territory who are forced to comply with the new norms. They bear the costs of cultural disruption, loss of traditional autonomy, and direct penalties for non-compliance. Their compliance is conditional on constant state monitoring and enforcement.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_population, payer,
    powerless, immediate, trapped, local).

% Local leaders, religious figures, or clan elders whose authority is undermined or directly suppressed by the imposition of state norms. They represent the pre-existing cultural acceptance that is being overridden, and their resistance is met with state force.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_authorities, excluded,
    organized, biographical, constrained, local).

% Academics who analyze the historical processes of state formation and norm imposition, seeking to understand the mechanisms of legitimation and coercion. They observe the dynamics without direct participation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the state's perspective, it establishes a unified legal and cultural framework across a diverse territory, enabling centralized governance, resource mobilization, and internal order. From the population's perspective, it primarily coordinates submission.
% TRANSFER_FUNCTION: Transfers obedience, cultural conformity, and often material resources (taxes, labor) from the subject population to the state apparatus and ruling elite. It also transfers the right to define legitimate behavior from traditional authorities to the state.
% ABSENT_VOICES: Traditional authorities and dissenting cultural groups are actively suppressed or marginalized. They would articulate the value of pre-existing norms, the costs of forced assimilation, and the illegitimacy of coercion-based rule.
% DISAPPEARANCE_RATIONALE: If the state's coercive power and its imposed norms vanished overnight, the central authority would collapse. Pre-existing local and traditional norms would likely resurface, leading to a fragmentation of governance and a re-negotiation of social order, potentially with significant conflict.
% FOUNDING_PROBLEM: The state sought to consolidate its power, unify diverse populations under a single, central authority, and extract resources more efficiently from its territory, overcoming the fragmentation and resistance of local power structures.
% FOUNDING_PROBLEM_CORROBORATION: Official state histories and legal doctrines consistently assert the necessity of central authority for order and progress. However, historical accounts of resistance, ethnographic studies of local communities, and critical historical sociology from outside the state's narrative contest the 'live' status of the problem, arguing it was a problem of state ambition, not societal dysfunction.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the state directly benefits from the imposed norms (e.g., taxation, conscription, unified legal system) at the expense of the subject population's autonomy and resources. Suppression is very high (0.92) as the state actively uses its monopoly on violence to crush resistance and enforce compliance, making alternatives virtually inaccessible. Theater ratio is low (0.1) because the enforcement is direct and functional, not performative; the state's power is openly asserted. Resistance is high (0.7) because the norms lack cultural acceptance and are met with active, though often suppressed, opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state apparatus and ruling elite, these norms are essential for order and progress, and their imposition is a necessary act of governance. From the perspective of the subject population and traditional authorities, the norms are an alien imposition, a source of extraction and oppression, and fundamentally illegitimate. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and ruling elite are clear beneficiaries, directly gaining power, resources, and control. The subject population and traditional authorities are victims, bearing the costs of forced compliance and cultural disruption. The directionality for the subject population is near 1.0 (full target) due to their trapped exit options and direct extraction. The state apparatus, as the agenda-setter, sits near 0.0 (full beneficiary).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'To what extent did the imposed norms eventually gain some degree of cultural acceptance, even if initially coerced, thereby shifting the source of their persistence from pure coercion to a hybrid form?',
    'Longitudinal ethnographic studies and historical analysis of cultural practices, legal adoption rates, and popular narratives over several generations post-imposition. If compliance becomes habitual and internalized without overt enforcement, the source of legitimacy has shifted.',
    'If significant cultural acceptance is found, the constraint''s classification might drift towards a Tangled Rope or even a Rope over time, as the ''extraction'' component becomes less reliant on active suppression and more on internalized norms. If not, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Whether coercion-based legitimacy can evolve into cultural acceptance.').

omega_variable(
    resistance_measurement_bias,
    'Is the measured resistance an accurate reflection of popular dissent, or is it systematically underestimated due to the state''s effective suppression of visible opposition and historical record manipulation?',
    'Analysis of non-official sources (oral histories, dissident archives, archaeological evidence of non-compliance) and comparative studies with similar regimes where suppression was less effective. If hidden resistance is significantly higher, the ''resistance'' metric is understated.',
    'If resistance is significantly higher than measured, the constraint''s effective suppression is even more critical to its persistence, reinforcing its Snare classification and highlighting the fragility of its ''legitimacy''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_measurement_bias, empirical, 'Accuracy of resistance measurement under high suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 100, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(impo_tr_t120, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement(impo_tr_t140, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 140, 0.1).
narrative_ontology:measurement(impo_tr_t160, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 160, 0.1).
narrative_ontology:measurement(impo_tr_t180, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 180, 0.1).
narrative_ontology:measurement(impo_tr_t200, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 100, 0.75).
narrative_ontology:measurement(impo_be_t120, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(impo_be_t140, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 140, 0.83).
narrative_ontology:measurement(impo_be_t160, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 160, 0.85).
narrative_ontology:measurement(impo_be_t180, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 180, 0.85).
narrative_ontology:measurement(impo_be_t200, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(impo_su_t120, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 120, 0.88).
narrative_ontology:measurement(impo_su_t140, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 140, 0.9).
narrative_ontology:measurement(impo_su_t160, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 160, 0.92).
narrative_ontology:measurement(impo_su_t180, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 180, 0.92).
narrative_ontology:measurement(impo_su_t200, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 200, 0.92).


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
