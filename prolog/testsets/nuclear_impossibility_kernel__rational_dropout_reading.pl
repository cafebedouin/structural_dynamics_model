% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational-Choice Dropout: War Remains Reachable but Irrational
 *   domain: strategic/military/international_relations
 *
 * SUMMARY:
 *   The rational-dropout reading instantiates one interpretation of the
 *   nuclear-weapons kernel. It claims that nuclear weapons created a
 *   constraint on war by making it rationally irrational: victory remains
 *   structurally possible (rational agents retain agency; war is in the
 *   feasible set), but costs exceed benefits by such a margin that rational
 *   choice theory predicts war will be abandoned even by actors with the
 *   power to wage it. This reading vindicates the rational-actor framework
 *   and expected-utility theory. It is distinct from the
 *   structural-impossibility reading (which claims war is
 *   physically/logically impossible) and the credibility-paradox reading
 *   (which claims deterrence is logically incoherent). This story models the
 *   rational-dropout reading as a mountain—a constraint that emerges from the
 *   structure of rational choice under conditions of mutual
 *   annihilation—while acknowledging beneficiaries (deterrence theorists,
 *   strategic-stability doctrine) who benefit from its persistence. The
 *   beneficiary listing triggers FSM evaluation; the omegas document the
 *   natural-law vs. constructed ambiguity.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: institutional power, trapped exit, generational horizon. Maintain deterrence doctrine and arsenals despite rational calculation that use is irrational.
 *   - deterrence_theorists: analytical power, analytical exit. Benefit from the rational-dropout reading because it resolves the paradox and vindicates their analytical framework.
 *   - nuclear_strategists_and_planners: powerful institutional actors, identity_locked into strategic roles. Face the dual bind of maintaining credible deterrence while knowing rational choice predicts non-use.
 *   - civilian_populations: powerless, trapped, generational horizon. Benefit from the constraint insofar as it stabilizes deterrence without logical paradox.
 *   - disarmament_advocates: moderate power, constrained exit, generational horizon. Excluded from strategic-stability consensus; would argue the constraint should lead to disarmament.
 *   - analytical_observer: sees the full structure and can measure the reading's persistence relative to its siblings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.15).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.22).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational-Choice Dropout: War Remains Reachable but Irrational").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic/military/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9').
narrative_ontology:cs_kernel_codification('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', formalized).
narrative_ontology:cs_authority_grounding('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', expertise).
narrative_ontology:cs_interpretation_layer_present('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9').
narrative_ontology:cs_reading_relation('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', foundational, rational_actors_maximize_expected_utility).
narrative_ontology:cs_axiom_status(rational_actors_maximize_expected_utility, holdable).
narrative_ontology:cs_axiom_grounding('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', rational_actors_maximize_expected_utility, empirically_contingent).
narrative_ontology:cs_axiom('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', foundational, war_remains_in_feasible_choice_set).
narrative_ontology:cs_axiom_status(war_remains_in_feasible_choice_set, holdable).
narrative_ontology:cs_axiom_grounding('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', war_remains_in_feasible_choice_set, empirically_contingent).
narrative_ontology:cs_reference_frame('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', game_theoretic_rationality_framework).
narrative_ontology:cs_drift_state('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', post_cold_war_nuclear_erosion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce3b8fe2-d9c2-4535-ad6e-ba45d01572a9', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, strategic_stability_doctrine).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_theorists).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint does not transfer resources or rents—it constrains choices based on rational calculation. Suppression is low (0.22) because the constraint relies on shared agreement about rationality rather than coercion; nuclear-armed states voluntarily accept the rational-choice framework. Theater_ratio is moderate-rising (0.41 at interval end, up from 0.25) because deterrence doctrine requires continuous performative maintenance—posturing, signaling, readiness displays—even though rational calculation predicts non-use. Accessibility_collapse is high (0.78) because once the rational-actor assumption is accepted, alternatives to the rational-dropout framing (paradox, structural impossibility, disarmament) collapse as less coherent explanations. Resistance is moderate (0.58) because disarmament advocates and paradox theorists maintain ongoing resistance; the rational-dropout reading does not silence its competitors, though it dominates strategic-policy consensus. The measurement series tracks rising theater_ratio over the interval, indicating increasing decoupling between rational-choice explanation and performative deterrence activity—a piton-drift signal. This divergence is noted in omega_id rational_choice_performance_decoupling.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (deterrence theorists, strategic-stability doctrine advocates) perceive the constraint as a genuine natural law of rational choice—a discovery about how rational agents must behave. The payer seats (nuclear-armed states, strategic planners) perceive it as both natural law and performative burden—they must maintain credibility while knowing use is irrational, which creates cognitive dissonance. Disarmament advocates and paradox theorists perceive the constraint as a constructed cover story for deterrence-by-inertia. The engine's per-seat classification will diverge: from the analytical seat the constraint is a clean mountain; from the strategic-policy seat it will show mixed signals as theater rises; from the disarmament seat it may compute as snare or piton depending on how theater is weighted. These divergences are predictions the framework exists to make.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic-stability theorists are beneficiaries (d near 0.0): the constraint vindicates their analytical framework and their institutional position. Nuclear-armed states are payers (d near 0.9): they bear the cost of maintaining doctrines and arsenals for weapons rationality forbids using. However, they are also beneficiaries in a secondary sense: the rational-choice framework allows them to pursue deterrence without accepting either paradox (incoherence) or structural impossibility (fatalism). The directionality is therefore asymmetric: theorists collect the pure benefit; armed states pay to maintain the framework while also benefiting from the stability it provides. This asymmetry is structural, not a matter of debate, and does not require override.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem—how to make deterrence credible when mutual destruction is guaranteed—was live at t=0 (immediately post-WWII). At t=30 (present), the problem is contested: strategic theorists say rational-choice framework solved it; disarmament advocates say it merely reframed it; some strategists say the problem has degraded into pure theater as nuclear weapons have become less usable (constraints from public opinion, treaty limitations, cyber/drone alternatives). The rising theater_ratio supports the contested verdict: the constraint is not dead (deterrence doctrine remains active) but its founding problem's status is shifting from 'live' toward 'zombie function maintained by inertia.' This is a mandatrophy trajectory. It does not require a base_properties.mandatrophy_resolved flag yet, but the measurement series anticipates the possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_actor_assumption_contingency,
    'Is the rational-dropout reading''s explanatory power grounded in a universal feature of how rational agents must behave, or does it depend on culturally contingent assumptions about how strategic actors decide (expected-utility maximization, narrow self-interest, state-level agency)?',
    'Comparative analysis of non-Western strategic traditions, behavioral economics evidence on actual decision-making under existential uncertainty, and historical case studies of near-use incidents where actors violated expected-utility logic.',
    'If the rational-actor framework is culturally contingent, the rational-dropout reading is a reading of the kernel specific to Western strategic theory, not a feature of the nuclear constraint itself. If universal, the reading grounds deterrence in a structural feature of rational choice. This distinction determines whether the constraint is a natural law or a theoretical construct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rational_actor_assumption_contingency, conceptual, 'Whether the rational-dropout reading rests on universal rationality or culturally specific assumptions.').

omega_variable(
    reachability_vs_rationality_boundary,
    'When strategists declare war is ''irrational,'' do they mean it is removed from the feasible choice set (actors cannot choose it) or merely from the optimal choice set (actors can choose it but should not)? The rational-dropout reading requires the latter; the structural-impossibility reading requires the former.',
    'Formal analysis of how actors treat ''reachable but irrational'' options in decision models, and empirical study of near-use incidents to determine whether restraint reflected rational calculation or constraint.',
    'If war is truly reachable (in the feasible set), then deterrence depends on rationality persisting; if war is structurally unreachable, the constraint is physical/logical, not rational. This distinguishes the rational-dropout reading from its structural-impossibility sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_vs_rationality_boundary, empirical, 'Whether nuclear war remains in the feasible choice set for rational actors or has been structurally removed.').

omega_variable(
    strategic_performance_vs_underlying_truth,
    'The theater_ratio is 0.41 and rising. If deterrence performance becomes decoupled from rational calculation (actors maintain postures they themselves believe are irrational), does the constraint persist as a rational constraint or does it become pure theater?',
    'Archival and interview evidence on whether nuclear planners genuinely believe their own rationality frameworks, and temporal measurement of whether theater_ratio rises as the coherence of rational-choice explanations erodes.',
    'Rising theater with stable extraction would indicate the rational-dropout reading is becoming a cover story for deterrence-by-inertia; falling theater would indicate genuine belief in rational constraint is persisting. Classification would shift toward piton if theater comes to dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_performance_vs_underlying_truth, empirical, 'Whether the rational-dropout reading''s explanatory power persists or is becoming theatrical maintenance.').

omega_variable(
    natural_law_vs_beneficiary_construction,
    'This constraint is authored as a mountain with declared beneficiaries (deterrence theorists, strategic-stability doctrine). Is rational-choice constraint on war a feature of reality independent of who benefits from the framing, or is it constructed to benefit the strategic-stability consensus?',
    'Genealogical analysis of when and why the rational-choice framing emerged in strategic theory (post-WWII game theory adoption); comparison with pre-1945 strategic reasoning about existential weapons; and analysis of who benefits if this framing persists versus who benefits if it collapses.',
    'If constructed, the constraint is a false summit and should reclassify as tangled rope or snare (the deterrence establishment extracts legitimacy and career value from the rational-choice frame). If genuinely natural, the beneficiary listing reflects vindication of true propositions rather than extraction. This is the FSM candidate: mountain with beneficiaries requires omega documentation of the ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_beneficiary_construction, conceptual, 'Whether the rational-choice constraint is a feature of nuclear reality or a theoretical construct that serves the interests of the strategic-stability establishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(nucl_tr_t30, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(nucl_be_t30, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(nucl_su_t30, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 30, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nuclear_impossibility_kernel. The sibling readings (credibility_paradox and structural_contraction) instantiate different interpretations of what nuclear weapons created. The rational_dropout_reading claims war is reachable but irrational (rational-choice constraint); the credibility_paradox_reading claims the deterrence threat is logically incredible; the structural_contraction_reading claims war is physically impossible. These readings have different ε values, different beneficiary structures, and different implications for deterrence policy. They are linked via network.affects_constraints to enable contamination-propagation analysis: if one reading's coherence degrades, the sibling readings will be affected differently (the paradox reading would gain credence if rationality assumptions collapse; the structural reading would be vindicated if irrational-actor behavior becomes empirically prevalent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
