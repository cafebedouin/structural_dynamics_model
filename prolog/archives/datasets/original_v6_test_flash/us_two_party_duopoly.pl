% ============================================================================
% CONSTRAINT STORY: us_two_party_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_two_party_duopoly, []).

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
 *   constraint_id: us_two_party_duopoly
 *   human_readable: The U.S. Two-Party Political Duopoly
 *   domain: political
 *
 * SUMMARY:
 *   The U.S. two-party system, dominated by the Democratic and Republican
 *   parties, creates a political landscape where third parties and
 *   independent voters face significant challenges. While the duopoly
 *   provides stability and a framework for governance, it also limits voter
 *   choice and suppresses alternative viewpoints. This system exhibits a
 *   tangled_rope dynamic, with both coordinating and extractive elements.
 *
 * KEY AGENTS:
 *   - Democratic Party: Institutional beneficiary (institutional/arbitrage) - Benefits from established structures and resources.
 *   - Republican Party: Institutional beneficiary (institutional/arbitrage) - Benefits from established structures and resources.
 *   - Third Parties: Primary victim (powerless/trapped) - Faces significant barriers to entry and limited opportunities for success.
 *   - Independent Voters: Primary victim (powerless/trapped) - Feels limited choices and preferences not adequately represented.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_two_party_duopoly, 0.6).
domain_priors:suppression_score(us_two_party_duopoly, 0.7).
domain_priors:theater_ratio(us_two_party_duopoly, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_two_party_duopoly, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_two_party_duopoly, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_two_party_duopoly, tangled_rope).
narrative_ontology:human_readable(us_two_party_duopoly, "The U.S. Two-Party Political Duopoly").
narrative_ontology:topic_domain(us_two_party_duopoly, "political").

domain_priors:requires_active_enforcement(us_two_party_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, democratic_party).
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, republican_party).
narrative_ontology:constraint_victim(us_two_party_duopoly, third_parties).
narrative_ontology:constraint_victim(us_two_party_duopoly, independent_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Third parties are trapped in a system designed to exclude them, facing significant barriers to entry and limited opportunities for success.
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Independent voters feel trapped with limited choices, often forced to choose the lesser of two evils, their preferences not adequately represented.
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Democratic Party benefits from the duopoly, maintaining its power and influence through established structures and resources. It has the ability to adapt and adjust its platform to maintain relevance.
constraint_indexing:constraint_classification(us_two_party_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Republican Party benefits from the duopoly, maintaining its power and influence through established structures and resources. It has the ability to adapt and adjust its platform to maintain relevance.
constraint_indexing:constraint_classification(us_two_party_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Political scientists understand the complex dynamics of the duopoly, recognizing both its coordinating function and its extractive elements.
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_two_party_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_two_party_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_two_party_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The duopoly extracts from the third parties and independent voters by suppressing their influence, while the Democratic and Republican parties benefit from maintaining their dominance. The theater_ratio reflects that while the parties engage in performative politics, they also perform real governance functions.
 *
 * PERSPECTIVAL GAP:
 *   Third parties and independent voters experience the duopoly as a snare, limiting their ability to participate effectively in the political process. The Democratic and Republican parties see it as a rope, providing a stable framework for governance and enabling them to maintain their power. Political scientists recognize both the coordination and extraction functions of the system, classifying it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The Democratic and Republican parties benefit by maintaining their power and influence, while the third parties and independent voters are suppressed and limited in their choices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reform_viability,
    'How viable are electoral reforms (e.g., ranked-choice voting, open primaries) in dismantling the duopoly?',
    'Analysis of reform efforts'' success in different states; modeling the impact of reforms on electoral outcomes',
    'If viable: Duopoly weakens, system shifts towards greater representation. If not viable: Duopoly persists, limiting voter choice and political innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_viability, empirical, 'Viability of electoral reforms in dismantling the duopoly').

omega_variable(
    partisan_polarization_driver,
    'To what extent does partisan polarization drive and reinforce the duopoly, and vice versa?',
    'Statistical analysis of polarization trends and party alignment; comparative studies of political systems with different party structures.',
    'If polarization is a primary driver: Overcoming the duopoly requires addressing underlying social divisions. If the duopoly drives polarization: Breaking up the duopoly may reduce political animosity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partisan_polarization_driver, conceptual, 'Role of partisan polarization in driving the duopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_two_party_duopoly, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_t_tr_t0, us_two_party_duopoly, theater_ratio, 0, 0.3).
narrative_ontology:measurement(us_t_tr_t20, us_two_party_duopoly, theater_ratio, 20, 0.35).
narrative_ontology:measurement(us_t_tr_t40, us_two_party_duopoly, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(us_t_be_t0, us_two_party_duopoly, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_t_be_t20, us_two_party_duopoly, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(us_t_be_t40, us_two_party_duopoly, base_extractiveness, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_two_party_duopoly, enforcement_mechanism).
narrative_ontology:affects_constraint(us_two_party_duopoly, campaign_finance_regulation).
narrative_ontology:affects_constraint(us_two_party_duopoly, media_bias).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
