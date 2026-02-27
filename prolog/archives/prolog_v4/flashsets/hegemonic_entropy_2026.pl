% ============================================================================
% CONSTRAINT STORY: hegemonic_entropy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hegemonic_entropy_2026, []).

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
 *   constraint_id: hegemonic_entropy_2026
 *   human_readable: The Asymmetry of Hegemonic Decay
 *   domain: economic/political
 *
 * SUMMARY:
 *   The global order, once a tool for broad coordination, has shifted toward
 *   asymmetric extraction via Cantillon effects and tax havens. Peripheral
 *   nations and domestic taxpayers are increasingly bearing the costs of the
 *   system, while core financial institutions and multinational corporations
 *   disproportionately benefit. The system is enforced through a combination
 *   of economic coercion and military power.
 *
 * KEY AGENTS:
 *   - Core Financial Institutions: Primary beneficiary (institutional/arbitrage) - benefits from arbitrage.
 *   - Multinational Corporations: Primary beneficiary (institutional/arbitrage) - benefits from tax havens.
 *   - Peripheral Nations: Primary target (powerless/trapped) - subject to capital flight.
 *   - Domestic Taxpayers: Secondary target (moderate/constrained) - bears costs of bailouts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hegemonic_entropy_2026, 0.65).
domain_priors:suppression_score(hegemonic_entropy_2026, 0.7).
domain_priors:theater_ratio(hegemonic_entropy_2026, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hegemonic_entropy_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hegemonic_entropy_2026, tangled_rope).
narrative_ontology:human_readable(hegemonic_entropy_2026, "The Asymmetry of Hegemonic Decay").
narrative_ontology:topic_domain(hegemonic_entropy_2026, "economic/political").

domain_priors:requires_active_enforcement(hegemonic_entropy_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, core_financial_institutions).
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, multinational_corporations).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, peripheral_nations).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, domestic_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Peripheral nations are trapped in the global financial system, subject to capital flight and limited policy autonomy.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Domestic taxpayers in hegemonic nations face increasing tax burdens to support global commitments and bailouts, while benefiting somewhat from the stability the system provides.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Core financial institutions benefit from the global financial system's arbitrage opportunities and implicit guarantees, experiencing it as a coordination mechanism.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Multinational corporations benefit from tax havens and regulatory arbitrage, while relying on the hegemonic power for market access and protection.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees the system as a whole, recognizing the intertwined coordination and extraction dynamics that characterize hegemonic decay.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hegemonic_entropy_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hegemonic_entropy_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hegemonic_entropy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The system increasingly extracts resources from peripheral nations and domestic taxpayers to benefit core financial institutions and multinational corporations. Suppression (0.70): High. Exit options are limited for peripheral nations, who are pressured to conform to the system. Domestic taxpayers have some exit through political action, but the suppression is significant due to the concentration of power. Theater ratio (0.40): Moderate. The system still performs some coordination functions, but the performative aspects are increasing, with more resources devoted to maintaining the system than to solving global problems.
 *
 * PERSPECTIVAL GAP:
 *   Peripheral nations see a snare, trapped in a system that extracts resources from them. Core financial institutions see a rope, a coordination mechanism that facilitates global finance. Domestic taxpayers see a tangled rope, bearing the costs of the system while still benefiting from its stability. The analytical observer recognizes the asymmetry and the increasing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Core financial institutions and multinational corporations have high exit options and are beneficiaries, so they experience low effective extraction. Peripheral nations have low exit options and are victims, so they experience high effective extraction. Domestic taxpayers are victims, but have some exit, so they experience moderate extraction. The analytical observer recognizes the asymmetry and the increasing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is not a pure snare or a pure rope, but a tangled rope, combining coordination and extraction. The analytical observer sees the system as a whole, recognizing the intertwined dynamics. The mandatrophy is resolved by recognizing that the system serves multiple functions and affects different actors differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deglobalization_threshold,
    'At what point does deglobalization reverse the extraction asymmetry?',
    'Tracking trade flows, capital flows, and political alignments',
    'Changes the nature of the hegemonic system',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deglobalization_threshold, empirical, 'Threshold for deglobalization reversing extraction').

omega_variable(
    multipolarity_transition,
    'How will the transition to a multipolar world impact the extraction dynamics?',
    'Game-theoretic models and geopolitical analysis',
    'Changes the structure of power and extraction',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multipolarity_transition, conceptual, 'Impact of multipolarity transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hegemonic_entropy_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hege_tr_t0, hegemonic_entropy_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hege_tr_t5, hegemonic_entropy_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(hege_tr_t10, hegemonic_entropy_2026, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(hege_be_t0, hegemonic_entropy_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hege_be_t5, hegemonic_entropy_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(hege_be_t10, hegemonic_entropy_2026, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hegemonic_entropy_2026, global_infrastructure).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, financial_contagion).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, regulatory_arbitrage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
