% ============================================================================
% CONSTRAINT STORY: paris_municipal_reform_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_municipal_reform_2026, []).

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
 *   constraint_id: paris_municipal_reform_2026
 *   human_readable: Paris Municipal Reform (Loi Maillard/PLM Reform)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The 2026 reform (Loi du 11 août 2025) fundamentally shifts the "Majority
 *   Premium" (Prime Majoritaire) for the Council of Paris from 50% down to
 *   25% of seats. This reform aims to promote greater political pluralism
 *   within the Council of Paris by reducing the advantage traditionally held
 *   by the dominant political party. While intended to enhance representation
 *   and inclusivity, the reform also entails a redistribution of political
 *   power, potentially impacting governance dynamics and policy outcomes. The
 *   reform is a shift away from the old system towards what's supposed to be
 *   greater representation but also a loss of guaranteed power for the
 *   formerly dominant.
 *
 * KEY AGENTS:
 *   - Dominant Political Party: Loses guaranteed majority, affected by reduction in seat allocation
 *   - Smaller Political Parties: Beneficiaries of increased representation and potential for coalition building
 *   - Opposition Parties: Gain opportunities for increased influence and policy input.
 *   - Analytical Observer: Assesses the overall impact on governance, inclusivity, and policy outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_municipal_reform_2026, 0.5).
domain_priors:suppression_score(paris_municipal_reform_2026, 0.4).
domain_priors:theater_ratio(paris_municipal_reform_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_municipal_reform_2026, extractiveness, 0.5).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_municipal_reform_2026, tangled_rope).
narrative_ontology:human_readable(paris_municipal_reform_2026, "Paris Municipal Reform (Loi Maillard/PLM Reform)").
narrative_ontology:topic_domain(paris_municipal_reform_2026, "political/legal").

domain_priors:requires_active_enforcement(paris_municipal_reform_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, smaller_political_parties).
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, opposition_parties).
narrative_ontology:constraint_victim(paris_municipal_reform_2026, dominant_political_party).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the dominant political party, which sees its guaranteed majority significantly reduced. They are trapped in the reformed system in the immediate aftermath, reducing their influence.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of smaller parties who benefit from increased representation. They can arbitrage the new system to gain influence, leading to better coordination.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% A balanced perspective recognizing both the coordination (increased representation) and extraction (reduced power for the previously dominant party) aspects of the reform.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% Opposition parties benefit from increased representation, but are constrained by the existing political landscape and may still face challenges in gaining significant power.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_municipal_reform_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_municipal_reform_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_municipal_reform_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. The dominant party experiences a tangible reduction in guaranteed seats (extraction of power). Smaller parties experience an increase in potential influence. Suppression (0.40): Moderate. While the reform increases representation, it does not fully eliminate barriers to entry or influence for smaller parties. Established political dynamics and resources still play a significant role. Theater ratio (0.30): Relatively low. The reform represents a substantive change in the electoral system, not merely a performative adjustment.
 *
 * PERSPECTIVAL GAP:
 *   The dominant political party perceives the reform as an extractive measure, diminishing their established power (Snare). Smaller parties view it as a coordinating mechanism, enhancing their opportunities for representation and influence (Rope). The analytical observer recognizes the mixed nature of the reform, balancing coordination and extraction elements (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (smaller parties) gain influence and opportunities for representation, yielding low d and negative chi. The dominant party, which is a victim, sees its guaranteed power and influence diminish, yielding high d and high chi.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_impact_on_governance,
    'Will the reform lead to more inclusive and responsive governance in Paris, or will it create political gridlock and instability?',
    'Longitudinal analysis of policy outcomes, coalition formation, and public opinion in Paris after the reform.',
    'If positive: The reform is a successful coordination mechanism (Rope). If negative: The reform is a destabilizing extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_governance, empirical, 'The long-term impact of the reform on the quality of governance in Paris.').

omega_variable(
    coalition_dynamics,
    'How will the new electoral system affect coalition formation and political alignment in the Paris City Council?',
    'Analysis of voting patterns, alliance building, and policy negotiations in the City Council after the reform.',
    'Impacts relative influence of parties and policy outputs',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_dynamics, empirical, 'Dynamics of new political landscape').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_municipal_reform_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_municipal_reform_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pari_tr_t3, paris_municipal_reform_2026, theater_ratio, 3, 0.3).
narrative_ontology:measurement(pari_tr_t6, paris_municipal_reform_2026, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_municipal_reform_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pari_be_t3, paris_municipal_reform_2026, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(pari_be_t6, paris_municipal_reform_2026, base_extractiveness, 6, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_municipal_reform_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
