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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Housing Price Formation (Institutional Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes housing price formation as a constructed
 *   outcome of institutional rules, including zoning regulations, mortgage
 *   lending standards, property tax policies, and the structure of real
 *   estate intermediary platforms. It is one reading of the
 *   'price_formation_kernel', emphasizing that prices are not purely natural
 *   or financial, but are shaped by human-designed systems. This reading
 *   identifies clear beneficiaries (incumbent owners, lenders) and victims
 *   (renters, first-time buyers) of these institutional arrangements.
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
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Housing Price Formation (Institutional Reading)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '65008d28-d791-4764-bcdb-46c6c572bf1e').
narrative_ontology:cs_kernel_codification('65008d28-d791-4764-bcdb-46c6c572bf1e', formalized).
narrative_ontology:cs_authority_grounding('65008d28-d791-4764-bcdb-46c6c572bf1e', lineage).
narrative_ontology:cs_interpretation_layer_present('65008d28-d791-4764-bcdb-46c6c572bf1e').
narrative_ontology:cs_reading_relation('65008d28-d791-4764-bcdb-46c6c572bf1e', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('65008d28-d791-4764-bcdb-46c6c572bf1e', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('65008d28-d791-4764-bcdb-46c6c572bf1e', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('65008d28-d791-4764-bcdb-46c6c572bf1e', foundational, housing_prices_are_institutionally_constructed).
narrative_ontology:cs_axiom_status(housing_prices_are_institutionally_constructed, holdable).
narrative_ontology:cs_axiom_grounding('65008d28-d791-4764-bcdb-46c6c572bf1e', housing_prices_are_institutionally_constructed, empirically_contingent).
narrative_ontology:cs_axiom('65008d28-d791-4764-bcdb-46c6c572bf1e', secondary, institutional_rules_create_distributive_outcomes).
narrative_ontology:cs_axiom_status(institutional_rules_create_distributive_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('65008d28-d791-4764-bcdb-46c6c572bf1e', institutional_rules_create_distributive_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('65008d28-d791-4764-bcdb-46c6c572bf1e', post_war_regulatory_state).
narrative_ontology:cs_drift_state('65008d28-d791-4764-bcdb-46c6c572bf1e', contemporary_neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('65008d28-d791-4764-bcdb-46c6c572bf1e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_brokers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, property_developers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, low_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from rising property values due to restricted supply and favorable tax treatment. They often advocate for maintaining restrictive zoning and other policies that protect their asset values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, generational, mobile, local).

% Profit from the volume and value of mortgage debt, which is inflated by high housing prices. They influence lending standards and benefit from a stable, appreciating asset class.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from high transaction volumes and values, as their commissions are a percentage of sale price. They often lobby against policies that would reduce housing prices or transaction costs.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_brokers, beneficiary,
    organized, biographical, mobile, local).

% Benefit from the scarcity created by zoning, which drives up the value of developable land and new construction. They navigate complex regulatory environments to maximize profits.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, property_developers, beneficiary,
    powerful, biographical, constrained, local).

% Bear the burden of high housing prices and stringent lending standards, making homeownership increasingly inaccessible. Their options are limited to saving for longer, taking on more debt, or relocating.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_homebuyers, payer,
    powerless, biographical, constrained, local).

% Face rising rents driven by overall housing market dynamics and limited supply. They have few options for stable, affordable housing and are often at the mercy of landlords and market fluctuations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Set zoning laws, approve development projects, and collect property taxes. They balance revenue needs with resident demands, often prioritizing existing homeowners' interests and maintaining property values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_governments, agenda_setter,
    institutional, generational, constrained, local).

% Establish and enforce mortgage lending standards, which influence credit availability and housing demand. They aim to ensure financial stability but can inadvertently contribute to housing affordability crises.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, financial_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Are most severely impacted by high housing costs, often facing displacement, homelessness, or living in substandard conditions. Their options are extremely limited by income and systemic barriers.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, low_income_households, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).

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
 *   The extractiveness (0.68) is high because these institutional arrangements create artificial scarcity and confer advantages to existing property owners and financial intermediaries, allowing them to capture a significant portion of housing value. Suppression (0.75) is also high, as these rules are actively enforced through legal and financial systems, limiting alternatives for those seeking affordable housing. The theater ratio (0.20) is relatively low, as the institutions generally perform their stated functions, even if those functions have extractive outcomes. Accessibility collapse (0.60) is moderate, as alternatives (e.g., building more housing, alternative financing) are constrained but not entirely absent. Resistance (0.55) is also moderate, reflecting ongoing political and social movements for housing reform.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent homeowners and mortgage lenders perceive these institutional arrangements as stable, legitimate mechanisms for wealth creation and market function. First-time homebuyers and renters, however, experience them as barriers to access and sources of escalating costs. The agenda-setters (local governments, financial regulators) often frame these rules as necessary for stability and public welfare, while their effects are demonstrably extractive for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, mortgage lenders, and real estate brokers are beneficiaries (d near 0.0) as they directly profit from rising property values and transaction volumes enabled by these rules. First-time homebuyers, renters, and low-income households are victims (d near 1.0) as they bear the costs of inflated prices and limited supply. Local governments and financial regulators act as agenda-setters, enforcing the rules that create this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (e.g., 'preserving neighborhood character' via zoning, 'ensuring financial stability' via lending standards) often masks its extractive function. The classification as a Tangled Rope prevents mislabeling it as a pure coordination mechanism (Rope) or a natural market outcome (Mountain). The persistence of these rules, despite their clear costs to victims, indicates that the coordination story serves as cover for the extraction benefiting powerful stakeholders. Mandatrophy is not fully resolved, as the original problems (e.g., orderly development, financial stability) are still cited, but the mechanisms have drifted to become primarily extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_institutional_vs_naturalist,
    'Is housing price formation a constructed outcome of institutional rules, or a natural equilibrium reflecting objective scarcity and preference?',
    'Comparative analysis of housing markets with radically different institutional structures (e.g., Vienna vs. San Francisco): if price patterns diverge systematically, it supports the institutional reading; if they converge despite different rules, it supports the naturalist reading.',
    'If the institutional reading is correct, policy interventions (zoning reform, tax changes) can directly alter price outcomes. If the naturalist reading is correct, such interventions are largely futile or counterproductive, and price is a ''mountain''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_institutional_vs_naturalist, conceptual, 'Distinguishes institutional construction from natural market forces in price formation.').

omega_variable(
    kernel_reading_institutional_vs_financialization,
    'To what extent is housing price formation primarily driven by institutional factors (zoning, lending, tax) versus financialization dynamics (credit expansion, asset-price feedback loops)?',
    'Econometric modeling disentangling the relative contributions of regulatory changes, credit supply shocks, and investor demand to price movements over time.',
    'If institutional factors dominate, policy levers are primarily regulatory. If financialization dominates, macro-prudential and monetary policy are the primary levers. This reading emphasizes the former, while the financialization reading emphasizes the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_institutional_vs_financialization, empirical, 'Clarifies the primary drivers of housing price formation between institutional and financialization perspectives.').

omega_variable(
    kernel_reading_institutional_vs_georgist,
    'Does the institutional framework primarily construct land value (unearned rent) or improvement value (earned capital/labor)?',
    'Detailed property tax assessments that disaggregate land value from improvement value across different zoning and tax regimes. If institutional rules disproportionately inflate land value, it supports the Georgist distinction.',
    'If land value is primarily constructed, a land value tax (Georgist solution) becomes a more potent and equitable policy tool. This reading acknowledges the distinction but focuses on the institutional construction of both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_institutional_vs_georgist, conceptual, 'Examines whether institutional factors primarily construct land value or improvement value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__institutional_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__institutional_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__institutional_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__institutional_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__institutional_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__institutional_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__institutional_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__institutional_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__institutional_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'price_formation_kernel', focusing on institutional construction. Other readings (naturalist, georgist, financialization) are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
