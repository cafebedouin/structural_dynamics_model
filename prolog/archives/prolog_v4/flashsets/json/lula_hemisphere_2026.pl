% ============================================================================
% CONSTRAINT STORY: lula_hemisphere_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lula_hemisphere_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lula_hemisphere_2026
 *   human_readable: The Monroe Doctrine Revival (Unilateral US Hegemony)
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the revival of unilateral hegemonic power, as
 *   described by Brazilian President Lula in response to a hypothetical 2026
 *   US military intervention in Venezuela. The intervention extracts
 *   sovereignty from Venezuela and suppresses regional alternatives, while
 *   primarily benefitting the US executive branch and defense contractors.
 *
 * KEY AGENTS:
 *   - United States Executive Branch: Primary beneficiary (institutional/arbitrage)
 *   - US Defense Contractors: Secondary beneficiary (powerful/mobile)
 *   - Venezuelan Sovereignty: Primary victim (powerless/trapped)
 *   - Latin American Alliances: Secondary victim (moderate/constrained)
 *   - Global Multilateralism: Affected institution (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lula_hemisphere_2026, 0.6).
domain_priors:suppression_score(lula_hemisphere_2026, 0.7).
domain_priors:theater_ratio(lula_hemisphere_2026, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lula_hemisphere_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(lula_hemisphere_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(lula_hemisphere_2026, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lula_hemisphere_2026, tangled_rope).
narrative_ontology:human_readable(lula_hemisphere_2026, "The Monroe Doctrine Revival (Unilateral US Hegemony)").
narrative_ontology:topic_domain(lula_hemisphere_2026, "political").

domain_priors:requires_active_enforcement(lula_hemisphere_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lula_hemisphere_2026, united_states_executive_branch).
narrative_ontology:constraint_beneficiary(lula_hemisphere_2026, us_defense_contractors).
narrative_ontology:constraint_victim(lula_hemisphere_2026, venezuelan_sovereignty).
narrative_ontology:constraint_victim(lula_hemisphere_2026, latin_american_alliances).
narrative_ontology:constraint_victim(lula_hemisphere_2026, global_multilateralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Venezuelan sovereignty is trapped with no exit from US intervention.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Latin American alliances are constrained, having some power but bearing costs.
constraint_indexing:constraint_classification(lula_hemisphere_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The US executive branch benefits from the action, experiencing the constraint as coordination.
constraint_indexing:constraint_classification(lula_hemisphere_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Global multilateralism is degraded; the US action demonstrates the limits of the institution.
constraint_indexing:constraint_classification(lula_hemisphere_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees a tangled rope: mixed coordination and extraction, high suppression of alternatives.
constraint_indexing:constraint_classification(lula_hemisphere_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lula_hemisphere_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lula_hemisphere_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lula_hemisphere_2026, TR),
    TR >= 0.70.

:- end_tests(lula_hemisphere_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Significant extraction of sovereignty and resources from Venezuela. Suppression (0.7): High suppression of alternative regional governance structures and multilateral solutions. Theater ratio (0.7): Relatively high performative content; the intervention involves a degree of performative action to demonstrate power and resolve.
 *
 * PERSPECTIVAL GAP:
 *   Venezuelan sovereignty views the intervention as a pure snare, while the US executive branch sees it as a rope enabling its foreign policy objectives. Latin American alliances experience it as a tangled rope due to the complex interplay of costs and benefits. Global multilateralism sees a degraded institution due to the US bypassing international norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Beneficiaries (US executive branch, defense contractors) have lower directionality values, while victims (Venezuelan sovereignty, Latin American alliances, global multilateralism) have higher directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by identifying distinct perspectives and structural relationships. It prevents mislabeling coordination as pure extraction by recognizing the power dynamics and differentiated experiences of various actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_intervention_threshold,
    'What specific event or condition would trigger a US military intervention in Venezuela?',
    'Analysis of US foreign policy documents and historical intervention patterns.',
    'Clarifies the scope and limits of the Monroe Doctrine revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_intervention_threshold, empirical, 'Defines the intervention trigger.').

omega_variable(
    latin_american_response,
    'How unified and effective would the Latin American response be to a US intervention?',
    'Game-theoretic modeling of alliance dynamics and economic leverage.',
    'Determines the degree of constraint on US unilateralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_american_response, empirical, 'Models the response of Latin American alliances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lula_hemisphere_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lula_tr_t0, lula_hemisphere_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lula_tr_t5, lula_hemisphere_2026, theater_ratio, 5, 0.5).
narrative_ontology:measurement(lula_tr_t10, lula_hemisphere_2026, theater_ratio, 10, 0.7).

% Extraction over time
narrative_ontology:measurement(lula_be_t0, lula_hemisphere_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lula_be_t5, lula_hemisphere_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(lula_be_t10, lula_hemisphere_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lula_hemisphere_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
