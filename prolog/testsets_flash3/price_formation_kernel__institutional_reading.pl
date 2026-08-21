% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__institutional_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Housing Price Formation (Institutional Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes housing price formation as a product of
 *   specific institutional arrangements: zoning laws, lending standards, tax
 *   policies, and the structure of intermediary platforms. It argues that
 *   prices are not merely a reflection of natural scarcity or individual
 *   preferences, but are actively shaped by these human-designed rules and
 *   systems. This is one reading of the 'price_formation_kernel', focusing on
 *   the constructed nature of market outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.75).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Housing Price Formation (Institutional Reading)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '69ce24bf-44d0-4d2d-993d-96457ef466a4').
narrative_ontology:cs_kernel_codification('69ce24bf-44d0-4d2d-993d-96457ef466a4', formalized).
narrative_ontology:cs_authority_grounding('69ce24bf-44d0-4d2d-993d-96457ef466a4', practice).
narrative_ontology:cs_interpretation_layer_present('69ce24bf-44d0-4d2d-993d-96457ef466a4').
narrative_ontology:cs_reading_relation('69ce24bf-44d0-4d2d-993d-96457ef466a4', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('69ce24bf-44d0-4d2d-993d-96457ef466a4', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('69ce24bf-44d0-4d2d-993d-96457ef466a4', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('69ce24bf-44d0-4d2d-993d-96457ef466a4', foundational, housing_prices_are_socially_constructed).
narrative_ontology:cs_axiom_status(housing_prices_are_socially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('69ce24bf-44d0-4d2d-993d-96457ef466a4', housing_prices_are_socially_constructed, empirically_contingent).
narrative_ontology:cs_axiom('69ce24bf-44d0-4d2d-993d-96457ef466a4', foundational, institutions_mediate_scarcity_and_value).
narrative_ontology:cs_axiom_status(institutions_mediate_scarcity_and_value, holdable).
narrative_ontology:cs_axiom_grounding('69ce24bf-44d0-4d2d-993d-96457ef466a4', institutions_mediate_scarcity_and_value, empirically_contingent).
narrative_ontology:cs_reference_frame('69ce24bf-44d0-4d2d-993d-96457ef466a4', post_new_deal_regulatory_state).
narrative_ontology:cs_drift_state('69ce24bf-44d0-4d2d-993d-96457ef466a4', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69ce24bf-44d0-4d2d-993d-96457ef466a4', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_platforms).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, low_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from rising property values due to restrictive zoning and tax policies that favor existing ownership. They actively lobby for policies that maintain or increase their property's value, often resisting new development.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, biographical, mobile, local).

% Profit from increased loan volumes and values driven by higher housing prices and specific lending standards. They influence policy to maintain a robust, if sometimes inflated, housing market.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from transaction fees and advertising revenue tied to high property values and frequent market activity. Their business models are optimized for the existing institutional framework of housing transactions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_platforms, beneficiary,
    powerful, biographical, arbitrage, national).

% Bear the costs of high housing prices through increased rents, with limited options for affordable housing due to restrictive zoning and market dynamics. Their ability to exit is severely constrained by economic realities.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Struggle to enter the housing market due to high prices, stringent lending standards, and competition from investors. They face significant barriers to accumulating down payments and qualifying for loans.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_homebuyers, payer,
    moderate, biographical, constrained, local).

% Are most severely impacted by high housing costs, often facing housing insecurity and displacement. Their options are extremely limited, making them effectively trapped within the existing market structure.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, low_income_households, payer,
    powerless, immediate, trapped, local).

% Administer and enforce zoning laws, building codes, and other regulations that directly shape housing supply and density. They operate within existing legal frameworks but have some agency to propose reforms.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, urban_planners_and_regulators, agenda_setter,
    institutional, generational, constrained, local).

% Analyze the impact of institutional factors on housing affordability and advocate for policy changes, such as zoning reform, tenant protections, and expanded public housing. They aim to shift the institutional framework.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates urban development, property rights, and financial flows within the housing market, providing a framework for investment, ownership, and tenancy.
% TRANSFER_FUNCTION: Transfers wealth from renters and first-time buyers to incumbent homeowners, mortgage lenders, and real estate platforms through inflated property values and transaction costs, mediated by institutional rules.
% ABSENT_VOICES: Future generations and potential residents are structurally excluded from current zoning and development decisions that limit housing supply and affordability. Their interests are not directly represented in the institutional mechanisms that shape price formation.
% DISAPPEARANCE_RATIONALE: If zoning, lending standards, tax treatments, and intermediary platforms vanished overnight, the housing market would collapse into chaos. Property values would plummet, lending would cease, and the entire system of housing provision and ownership would need to be rebuilt from first principles, likely leading to a more fluid, but initially highly unstable, market.
% FOUNDING_PROBLEM: To establish stable property rights, facilitate orderly urban development, and enable access to housing finance, preventing speculative chaos and ensuring quality standards.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent homeowners and lenders argue the system still provides stability and access to finance. Housing advocates and economists, from outside the benefiting parties, argue that while some stability remains, the system has ossified into a mechanism for wealth extraction and exclusion, failing its original goals for broad access and orderly development.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because these institutional structures systematically favor existing property owners and financial intermediaries at the expense of new entrants and renters. Suppression (0.75) is high due to the difficulty of challenging or changing deeply entrenched zoning laws, financial regulations, and tax codes. Theater ratio is low (0.20) as the institutions are largely functional in their stated purpose of regulating the market, even if their outcomes are extractive. The metrics reflect a system that, while providing some coordination, primarily serves to extract value for specific beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries (homeowners, lenders) perceive the system as a stable, fair market reflecting value. Victims (renters, first-time buyers) experience it as an exclusionary, extractive system. Regulators may see it as a necessary framework for orderly markets. The engine's per-seat classification will reflect these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, mortgage lenders, and real estate platforms are clear beneficiaries, as the institutional framework directly inflates their assets or revenue. Renters, first-time homebuyers, and low-income households are victims, bearing the costs of high prices and limited access. Urban planners and regulators act as agenda-setters, administering the rules. Housing advocates serve as observers, analyzing and challenging the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_natural_scarcity,
    'To what extent do housing prices reflect genuine natural scarcity (e.g., limited desirable land) versus institutionally constructed scarcity (e.g., restrictive zoning)?',
    'Comparative analysis of housing markets with similar natural endowments but vastly different institutional frameworks (e.g., Houston vs. San Francisco zoning).',
    'If institutional scarcity dominates, the extractiveness and suppression metrics are more robust; if natural scarcity is primary, the constraint leans more towards a Mountain, and its extractiveness is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_natural_scarcity, empirical, 'Distinguishing natural vs. institutional drivers of scarcity in housing markets.').

omega_variable(
    institutional_vs_financialization_drivers,
    'What is the relative causal weight of institutional factors (zoning, tax) versus financialization factors (credit expansion, asset-price feedback) in driving housing price inflation?',
    'Econometric modeling that isolates the impact of policy changes (e.g., zoning reform, interest rate shifts) on price dynamics, controlling for other variables.',
    'If financialization is the dominant driver, this ''institutional_reading'' might be a downstream influence rather than the primary constraint, potentially reclassifying to a Rope or Piton if its direct extractive power is less than the financial system''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_financialization_drivers, empirical, 'Disentangling institutional vs. financial drivers of housing prices.').

omega_variable(
    mandatrophy_of_zoning,
    'Has the original mandate of zoning (e.g., public health, orderly development) been superseded by its function as a mechanism for property value protection and exclusion?',
    'Historical analysis of zoning ordinance evolution, legal challenges, and stated justifications over time, compared with observed outcomes on affordability and equity.',
    'If the mandate has atrophied, the constraint''s theater_ratio would be higher, and its classification would lean more towards a Piton or Snare, as its stated purpose becomes cover for its actual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_zoning, conceptual, 'Assessing the functional shift of zoning from public good to exclusionary mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__institutional_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__institutional_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__institutional_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__institutional_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__institutional_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__institutional_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__institutional_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__institutional_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__institutional_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__institutional_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__institutional_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__institutional_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, wealth_inequality_amplification).

% DUAL FORMULATION NOTE:
% This constraint is the 'institutional_reading' of the 'price_formation_kernel', focusing on the constructed nature of housing prices through policy and market structures. It is linked to other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
