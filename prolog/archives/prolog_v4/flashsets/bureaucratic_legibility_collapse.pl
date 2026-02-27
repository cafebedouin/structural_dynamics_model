% ============================================================================
% CONSTRAINT STORY: bureaucratic_legibility_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_legibility_collapse, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bureaucratic_legibility_collapse
 *   human_readable: The Administrative Whiteout
 *   domain: political/organizational/informational
 *
 * SUMMARY:
 *   The Administrative Whiteout describes a scenario where the metrics used
 *   by an institution to "see" and manage its domain become so decoupled from
 *   reality that the institution's actions produce the opposite of their
 *   intended effects. This can occur due to metric substitution, regulatory
 *   capture, or simply the inherent complexity of the system.
 *
 * KEY AGENTS:
 *   - Incumbent Bureaucracy: The institution whose metrics have become decoupled from reality (institutional/constrained).
 *   - Intended Beneficiaries of Policy: Those whom the policy was designed to help (powerless/trapped).
 *   - Regulatory Capture Lobbyists: Those who benefit from the complexity and opacity of the system (powerful/arbitrage).
 *   - Operational Transparency:  Undermined by the useless metrics generated. (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_legibility_collapse, 0.6).
domain_priors:suppression_score(bureaucratic_legibility_collapse, 0.7).
domain_priors:theater_ratio(bureaucratic_legibility_collapse, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, extractiveness, 0.6).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_legibility_collapse, snare).
narrative_ontology:human_readable(bureaucratic_legibility_collapse, "The Administrative Whiteout").
narrative_ontology:topic_domain(bureaucratic_legibility_collapse, "political/organizational/informational").

domain_priors:requires_active_enforcement(bureaucratic_legibility_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, incumbent_bureaucracy).
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, regulatory_capture_lobbyists).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, intended_beneficiaries_of_policy).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, operational_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Those whom the policy was designed to help are trapped by its unintended consequences and suffer the most from the legibility collapse.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The bureaucracy, while nominally the 'enforcer', becomes a piton as its metrics fail to reflect reality. It is trapped in a cycle of enforcing ineffective rules and generating meaningless data.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Lobbyists benefit from the complexity and opacity, finding arbitrage opportunities within the system while contributing to its dysfunction. They extract rents from the confusion.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An outside observer sees the tangled web of unintended consequences and the system's overall extractive nature.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The possibility of achieving actual transparency is undermined by the overwhelming amount of meaningless metrics.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_legibility_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_legibility_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_legibility_collapse, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_legibility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the system actively harms those it is supposed to help. The suppression is high because the system is resistant to change and suppresses alternative approaches.  The theater ratio is high because the institution focuses on generating metrics rather than achieving its goals.
 *
 * PERSPECTIVAL GAP:
 *   Those who are supposed to benefit from the policy are harmed by it (snare).  The institution itself is trapped in a cycle of ineffectiveness (piton).  Lobbyists benefit from the system's dysfunction (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are those who profit from the system's dysfunction, while the victims are those who are harmed by it.  The institution is caught in the middle, unable to correct course due to the flawed metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a snare because the system actively harms those it is supposed to help. It is not simply a case of ineffectiveness, but rather of actively producing negative outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_drift_threshold,
    'At what point does metric substitution become so severe that the original purpose of the institution is completely undermined?',
    'Longitudinal study comparing stated goals with actual outcomes.',
    'If the threshold is low, then even small metric drifts can cause significant harm. If high, then the institution is more resilient to manipulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_drift_threshold, empirical, 'The point at which metric substitution undermines the institution''s purpose.').

omega_variable(
    data_feedback_sensitivity,
    'How sensitive is the bureaucratic system to feedback from the real world, and how quickly can it adapt its metrics?',
    'Analysis of policy revision cycles and stakeholder engagement.',
    'If the system is highly sensitive, then it can correct course quickly. If it is insensitive, then it is doomed to repeat its mistakes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_feedback_sensitivity, empirical, 'Sensitivity of bureaucratic system to real-world feedback.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_legibility_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bure_tr_t0, bureaucratic_legibility_collapse, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bure_tr_t5, bureaucratic_legibility_collapse, theater_ratio, 5, 0.6).
narrative_ontology:measurement(bure_tr_t10, bureaucratic_legibility_collapse, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(bure_be_t0, bureaucratic_legibility_collapse, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bure_be_t5, bureaucratic_legibility_collapse, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(bure_be_t10, bureaucratic_legibility_collapse, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_legibility_collapse, enforcement_mechanism).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, goodhart_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
