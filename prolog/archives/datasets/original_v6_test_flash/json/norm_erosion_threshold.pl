% ============================================================================
% CONSTRAINT STORY: norm_erosion_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_norm_erosion_threshold, []).

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
 *   constraint_id: norm_erosion_threshold
 *   human_readable: The Social Fabric Breach
 *   domain: social/political
 *
 * SUMMARY:
 *   This constraint models the non-linear breakdown of social cooperation
 *   when the perceived rate of "rule-breaking" or "defection" crosses a
 *   critical threshold. The erosion of adherence to shared norms can
 *   destabilize society.
 *
 * KEY AGENTS:
 *   - Rule Followers: Primary victim (powerless/trapped) — experience the cost of others breaking norms.
 *   - Opportunistic Actors: Primary beneficiary (powerful/arbitrage) — benefit from defying norms.
 *   - Average Citizen: Moderate position between following rules and taking advantage (moderate/constrained).
 *   - Social Cohesion: Abstract entity that suffers from norm erosion (victim).
 *   - Analytical Observer: Civilizational perspective on the process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(norm_erosion_threshold, 0.55).
domain_priors:suppression_score(norm_erosion_threshold, 0.4).
domain_priors:theater_ratio(norm_erosion_threshold, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(norm_erosion_threshold, extractiveness, 0.55).
narrative_ontology:constraint_metric(norm_erosion_threshold, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(norm_erosion_threshold, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(norm_erosion_threshold, tangled_rope).
narrative_ontology:human_readable(norm_erosion_threshold, "The Social Fabric Breach").
narrative_ontology:topic_domain(norm_erosion_threshold, "social/political").

domain_priors:requires_active_enforcement(norm_erosion_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(norm_erosion_threshold, opportunistic_actors).
narrative_ontology:constraint_victim(norm_erosion_threshold, rule_followers).
narrative_ontology:constraint_victim(norm_erosion_threshold, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual who consistently adheres to social norms feels increasingly exploited as others defect. They have limited exit options from their social environment and experience direct negative consequences.
constraint_indexing:constraint_classification(norm_erosion_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Most citizens find themselves in a position where they benefit from social cooperation but are also tempted to break norms when it's personally advantageous. They are somewhat constrained due to societal expectations but still have the option to be opportunistic. The erosion of norms affects the citizen moderately.
constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Those willing to defy social norms for personal gain benefit from the exploitation of trust. Immediate benefits exist within the local scope, where they can leverage these breaches.
constraint_indexing:constraint_classification(norm_erosion_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Analyzes social contract and its fragility. Norm erosion is a process with extractiveness and some coordination. It can be seen as a mechanism of social selection.
constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(norm_erosion_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(norm_erosion_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(norm_erosion_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(norm_erosion_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(norm_erosion_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate, as the system benefits from cooperation but is easily undermined by defection. Suppression is likewise moderate, since actors are not forced to participate and have options to defect. The theater ratio is moderate, reflecting the performative aspects of norm enforcement (e.g., public shaming, virtue signaling) that don't necessarily translate into effective deterrence.
 *
 * PERSPECTIVAL GAP:
 *   The rule follower perceives a snare as their trust is exploited by others who are willing to defect for personal benefit. The opportunistic actor perceives a rope, since they are benefiting from the norm erosion. The analytical observer recognizes a tangled rope, as social cooperation provides a coordination benefit, but that is extracted from by opportunistic actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality determined by the capacity for actors to benefit from defection, or be hurt by it. The rule follower is powerless and trapped, so they bear full cost. Opportunistic actors are powerful and arbitrage the social contract for their benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_determination,
    'What specific percentage of norm violations constitutes the ''tipping point'' for widespread social breakdown?',
    'Historical analysis of societies experiencing collapse, correlation between norm violations and social trust levels',
    'Inaccurate thresholds may lead to premature or delayed interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_determination, empirical, 'Determination of the critical threshold for norm erosion').

omega_variable(
    norm_definition_vagueness,
    'How much does ambiguity in social norm definition contribute to perceived violation rates?',
    'Survey research assessing varying interpretations of common social norms; legal analysis of ambiguous laws',
    'Vague norms may lead to overestimation of violations, accelerating breakdown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_definition_vagueness, conceptual, 'Impact of norm definition ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(norm_erosion_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norm_tr_t0, norm_erosion_threshold, theater_ratio, 0, 0.55).
narrative_ontology:measurement(norm_tr_t5, norm_erosion_threshold, theater_ratio, 5, 0.65).
narrative_ontology:measurement(norm_tr_t10, norm_erosion_threshold, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(norm_be_t0, norm_erosion_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(norm_be_t5, norm_erosion_threshold, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(norm_be_t10, norm_erosion_threshold, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(norm_erosion_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(norm_erosion_threshold, political_polarization).
narrative_ontology:affects_constraint(norm_erosion_threshold, economic_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
