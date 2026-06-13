% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Killing Authority: Retributive Desert
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'retributive desert' reading of state
 *   killing authority, where capital punishment is justified by the principle
 *   of lex talionis (death for death) and the forfeiture of the murderer's
 *   right to life. It is distinct from readings based on deterrence or
 *   categorical abolition. The constraint is framed as a snare due to its
 *   high extraction (life itself) and suppression (the state's ultimate
 *   coercive power), with identifiable victims (condemned persons) and
 *   beneficiaries (victims' families, society's moral order).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.85).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.95).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, snare).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Killing Authority: Retributive Desert").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '2c2cb4b8-a7a0-4610-b309-e47797c8ddc0').
narrative_ontology:cs_kernel_codification('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', formalized).
narrative_ontology:cs_authority_grounding('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', lineage).
narrative_ontology:cs_interpretation_layer_present('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0').
narrative_ontology:cs_reading_relation('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', foundational, right_to_life_is_forfeitable).
narrative_ontology:cs_axiom_status(right_to_life_is_forfeitable, holdable).
narrative_ontology:cs_axiom_grounding('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', right_to_life_is_forfeitable, deontological).
narrative_ontology:cs_axiom('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', foundational, lex_talionis_is_just).
narrative_ontology:cs_axiom_status(lex_talionis_is_just, holdable).
narrative_ontology:cs_axiom_grounding('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', lex_talionis_is_just, deontological).
narrative_ontology:cs_reference_frame('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', classical_retributive_justice).
narrative_ontology:cs_drift_state('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2c2cb4b8-a7a0-4610-b309-e47797c8ddc0', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, society_at_large).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) as it involves the ultimate cost: life. Suppression is also very high (0.95) because the state's authority to execute is backed by its monopoly on legitimate force, and there are no exit options for the condemned. Theater ratio is low (0.1) because the act of execution is a direct, non-performative application of the constraint's core function. The metrics reflect the severe, direct, and irreversible nature of this form of punishment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims' families and retributive proponents, this constraint is a necessary act of justice, restoring balance. From the perspective of the condemned and abolitionists, it is an unjust act of state violence. The engine's classification as a snare reflects the structural reality of extraction and suppression, regardless of the moral justification offered by its proponents.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system acts as the agenda-setter, enforcing the constraint. Murder victims' families and society at large are beneficiaries, receiving symbolic justice and moral vindication. Condemned persons are the direct victims, losing their lives. Human rights advocates are excluded, as their foundational premises are incompatible with this reading's justification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_forfeiture_ambiguity,
    'Is the right to life truly forfeitable by criminal act, or is it an inalienable right?',
    'Philosophical consensus on the nature of rights, or a shift in societal moral intuitions regarding state power over life.',
    'If the right to life is inalienable, the foundational premise of this reading collapses, reclassifying the constraint as pure extraction (snare) without moral justification, potentially shifting it towards a ''categorical_abolition'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_forfeiture_ambiguity, conceptual, 'Ambiguity regarding the moral basis of life forfeiture.').

omega_variable(
    proportionality_measurement,
    'How is ''death for death'' proportionality objectively measured and applied, given the unique value of each human life?',
    'Development of a universally accepted metric for moral proportionality in punishment, or a legal framework that consistently applies lex talionis without arbitrary distinctions.',
    'If proportionality cannot be objectively measured, the claim of ''justice'' becomes subjective, weakening the constraint''s legitimacy and potentially exposing it as arbitrary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement, conceptual, 'The challenge of objectively measuring proportional punishment.').

omega_variable(
    sibling_reading_impact,
    'How would the adoption of the ''categorical_abolition'' reading structurally alter this constraint?',
    'Analysis of legal and societal changes in jurisdictions that have adopted abolitionist stances.',
    'The ''categorical_abolition'' reading would directly foreclose the ''retributive_desert'' reading by denying the state''s authority to take life, rendering this constraint null and void within that framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of categorical abolition on retributive desert.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__retributive_desert, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1990, state_killing_authority__retributive_desert, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__retributive_desert, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__retributive_desert, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__retributive_desert, base_extractiveness, 1976, 0.8).
narrative_ontology:measurement(stat_be_t1990, state_killing_authority__retributive_desert, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__retributive_desert, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__retributive_desert, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__retributive_desert, suppression_requirement, 1976, 0.9).
narrative_ontology:measurement(stat_su_t1990, state_killing_authority__retributive_desert, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__retributive_desert, suppression_requirement, 2005, 0.92).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__retributive_desert, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_authority' kernel, focusing on retributive desert. It is linked to the deterrence and abolitionist readings as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
