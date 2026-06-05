% ============================================================================
% CONSTRAINT STORY: israel_electoral_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_electoral_threshold, []).

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
 *   constraint_id: israel_electoral_threshold
 *   human_readable: The 3.25% Knesset Electoral Threshold
 *   domain: political
 *
 * SUMMARY:
 *   The 3.25% Knesset electoral threshold requires political parties to
 *   secure at least 3.25% of the national vote to gain seats. It aims to
 *   promote government stability by reducing the number of small parties, but
 *   it also suppresses representation for smaller groups and minorities. The
 *   threshold's function as both a coordination mechanism and an extraction
 *   mechanism makes it a tangled rope.
 *
 * KEY AGENTS:
 *   - Smaller Political Parties: Primary target (powerless/trapped) – Unable to reach the threshold, suppressed votes.
 *   - Larger Political Parties: Primary beneficiary (institutional/arbitrage) – Consolidate power.
 *   - Coalition Stability: Secondary beneficiary (institutional/constrained) – Potentially more stable governments.
 *   - Underrepresented Minorities: Secondary target (powerless/constrained) – Reduced voice in government.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_electoral_threshold, 0.55).
domain_priors:suppression_score(israel_electoral_threshold, 0.7).
domain_priors:theater_ratio(israel_electoral_threshold, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_electoral_threshold, extractiveness, 0.55).
narrative_ontology:constraint_metric(israel_electoral_threshold, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(israel_electoral_threshold, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_electoral_threshold, tangled_rope).
narrative_ontology:human_readable(israel_electoral_threshold, "The 3.25% Knesset Electoral Threshold").
narrative_ontology:topic_domain(israel_electoral_threshold, "political").

domain_priors:requires_active_enforcement(israel_electoral_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, larger_political_parties).
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, coalition_stability).
narrative_ontology:constraint_victim(israel_electoral_threshold, smaller_political_parties).
narrative_ontology:constraint_victim(israel_electoral_threshold, underrepresented_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Smaller parties are trapped by the threshold, unable to gain representation and influence policy. Their supporters' votes are effectively suppressed.
constraint_indexing:constraint_classification(israel_electoral_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Voters of smaller parties are constrained, as their preferred choice may not reach the threshold, leading to wasted votes. However, they have limited mobility by potentially voting strategically for larger parties or influencing party mergers.
constraint_indexing:constraint_classification(israel_electoral_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Larger parties benefit from the threshold as it consolidates power, reducing the influence of smaller factions and potentially stabilizing coalition governments. They can arbitrage by absorbing smaller parties or benefiting from their exclusion.
constraint_indexing:constraint_classification(israel_electoral_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% A political scientist sees the threshold as a tangled rope. It provides some stability to the government (coordination) but extracts representation from smaller parties and potentially underrepresented groups (extraction).
constraint_indexing:constraint_classification(israel_electoral_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_electoral_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_electoral_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(israel_electoral_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to high. The threshold directly prevents smaller parties from gaining representation, extracting their potential influence. Suppression (0.70): High. The threshold actively suppresses the political power of smaller parties and their supporters. Theater Ratio (0.20): Low. The threshold's impact is structural and direct, not performative. While there is some debate about its true effects, the mechanism of denying seats to parties below the threshold is explicit and not primarily theatrical.
 *
 * PERSPECTIVAL GAP:
 *   Smaller parties view the threshold as a snare, as it traps them and prevents them from participating in government. Larger parties and those prioritizing coalition stability see it as a rope, helping to create more stable and functional governments. From an analytical viewpoint, it's a tangled rope, balancing stability and representation.
 *
 * DIRECTIONALITY LOGIC:
 *   The threshold benefits larger parties by consolidating power and making coalition formation easier (low d). It harms smaller parties by preventing them from gaining representation (high d). The analytical perspective acknowledges both the coordinating benefits for larger parties and the extractive effects on smaller parties, resulting in a classification as a tangled rope. The voters of smaller parties are victims, but they have the option to shift their votes, giving them a constrained exit, so their d is lower than the smaller parties themselves.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_threshold_percentage,
    'What is the optimal electoral threshold percentage that balances government stability and representation of diverse interests?',
    'Comparative analysis of electoral systems in other countries, statistical modeling of coalition formation, and surveys of voter preferences.',
    'Higher threshold: increased government stability but reduced representation of minorities. Lower threshold: increased representation but potentially unstable coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_threshold_percentage, empirical, 'Determine the best threshold % balancing stability vs representation.').

omega_variable(
    coalition_formation_effect,
    'To what extent does the threshold genuinely simplify coalition formation versus simply suppressing dissent?',
    'In-depth case studies of coalition negotiations before and after changes to the threshold, analysis of government longevity, and qualitative assessment of political discourse.',
    'If simplification is the primary effect: electoral system is more ''rope''-like. If suppression is the primary effect: electoral system is more ''snare''-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_effect, conceptual, 'Examine if the threshold simplifies coalition or suppresses dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_electoral_threshold, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isra_tr_t0, israel_electoral_threshold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(isra_tr_t10, israel_electoral_threshold, theater_ratio, 10, 0.15).
narrative_ontology:measurement(isra_tr_t20, israel_electoral_threshold, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(isra_be_t0, israel_electoral_threshold, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(isra_be_t10, israel_electoral_threshold, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(isra_be_t20, israel_electoral_threshold, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_electoral_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_electoral_threshold, coalition_bargaining_dynamics).
narrative_ontology:affects_constraint(israel_electoral_threshold, minority_group_representation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
