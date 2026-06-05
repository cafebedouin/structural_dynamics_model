% ============================================================================
% CONSTRAINT STORY: kidney_exchange_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kidney_exchange_market, []).

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
 *   constraint_id: kidney_exchange_market
 *   human_readable: Kidney Exchange Cycles/Chains
 *   domain: social/technological/biological
 *
 * SUMMARY:
 *   Kidney exchange programs are designed to facilitate transplants when a
 *   living donor is incompatible with their intended recipient. The system
 *   matches donor/recipient pairs to create cycles or chains, increasing the
 *   overall number of successful transplants. However, some patients may be
 *   disadvantaged by the exchange. The program aims to alleviate the
 *   incompatible donor problem.
 *
 * KEY AGENTS:
 *   - Transplant Recipients: Beneficiaries (moderate/constrained)
 *   - Organ Donation Networks: Beneficiaries (institutional/arbitrage)
 *   - Potential Donors without Compatible Recipients: Victims (powerless/trapped)
 *   - Patients with Rare Blood Types: Victims (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kidney_exchange_market, 0.55).
domain_priors:suppression_score(kidney_exchange_market, 0.4).
domain_priors:theater_ratio(kidney_exchange_market, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kidney_exchange_market, extractiveness, 0.55).
narrative_ontology:constraint_metric(kidney_exchange_market, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(kidney_exchange_market, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kidney_exchange_market, tangled_rope).
narrative_ontology:human_readable(kidney_exchange_market, "Kidney Exchange Cycles/Chains").
narrative_ontology:topic_domain(kidney_exchange_market, "social/technological/biological").

domain_priors:requires_active_enforcement(kidney_exchange_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kidney_exchange_market, transplant_recipients).
narrative_ontology:constraint_beneficiary(kidney_exchange_market, organ_donation_networks).
narrative_ontology:constraint_victim(kidney_exchange_market, potential_donors_without_compatible_recipients).
narrative_ontology:constraint_victim(kidney_exchange_market, patients_with_rare_blood_types).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients with rare blood types may find it harder to find a match, even within the exchange. Their exit options are limited. High extraction.
constraint_indexing:constraint_classification(kidney_exchange_market, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% Organ donation networks benefit from increased efficiency and access to a broader pool of donors and recipients. They can leverage arbitrage by optimizing matching algorithms.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Transplant recipients benefit from increased access to compatible kidneys, but are constrained by the availability of donors and the complexity of the matching process. Extraction includes the inherent risk and stress associated with transplantation and the exchange process.
constraint_indexing:constraint_classification(kidney_exchange_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The kidney exchange market is a sophisticated allocation system solving a collective action problem, yet with inherent limitations for certain individuals. Viewing from a civilizational perspective, it creates an asymmetric benefit.
constraint_indexing:constraint_classification(kidney_exchange_market, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kidney_exchange_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kidney_exchange_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kidney_exchange_market, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kidney_exchange_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kidney_exchange_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts from those who might not otherwise receive a transplant opportunity. Suppression (0.40): Moderate. Alternatives include deceased donor lists. Theater ratio (0.20): Low. The primary goal is functional: increasing transplant rates. The extraction is inherent to the allocation of scarce resources. Certain patients may experience extraction due to their blood types.
 *
 * PERSPECTIVAL GAP:
 *   The exchange is seen as a rope by the organ donation networks. The transplant recipients experience a tangled rope because it allows for better allocation but may still impose risk to the process. Patients with rare blood types see a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by whether an agent benefits from the constraints or has limited exit options. Beneficiaries and victims determine directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    matching_algorithm_fairness,
    'To what extent do matching algorithms prioritize specific groups (e.g., based on blood type or genetic markers), potentially disadvantaging others?',
    'Analysis of algorithm design and outcomes, comparing transplant rates across different patient subgroups.',
    'If algorithms are biased, the constraint may shift towards a pure snare for disadvantaged groups. Otherwise, it remains a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(matching_algorithm_fairness, empirical, 'Fairness of matching algorithms').

omega_variable(
    donor_altruism_vs_coercion,
    'Is the exchange truly altruistic, or are there subtle forms of coercion (social pressure, emotional obligation) that influence donor participation?',
    'Surveys and interviews with donors and recipients, exploring their motivations and experiences.',
    'If coercion is significant, the constraint may become more extractive for donors. If altruism prevails, it remains a mixed coordination/extraction system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_altruism_vs_coercion, conceptual, 'Degree of altruism vs. coercion in donor participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kidney_exchange_market, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kidn_tr_t0, kidney_exchange_market, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kidn_tr_t5, kidney_exchange_market, theater_ratio, 5, 0.15).
narrative_ontology:measurement(kidn_tr_t10, kidney_exchange_market, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(kidn_be_t0, kidney_exchange_market, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(kidn_be_t5, kidney_exchange_market, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(kidn_be_t10, kidney_exchange_market, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kidney_exchange_market, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
