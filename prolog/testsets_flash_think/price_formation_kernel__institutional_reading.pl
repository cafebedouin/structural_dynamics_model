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
 *   This constraint story represents the 'institutional reading' of the
 *   broader 'price_formation_kernel'. It posits that housing prices are not
 *   merely a result of natural market forces but are actively constructed and
 *   shaped by a complex web of institutional rules, including zoning laws,
 *   mortgage lending standards, property tax policies, and the operational
 *   logic of real estate intermediary platforms. This reading emphasizes the
 *   role of human-designed systems in determining housing affordability and
 *   wealth distribution, contrasting with views that prioritize natural
 *   scarcity or financial dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.65).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.55).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Housing Price Formation (Institutional Reading)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '6dbf5e14-aebc-4c7b-bba9-c974869569e4').
narrative_ontology:cs_kernel_codification('6dbf5e14-aebc-4c7b-bba9-c974869569e4', formalized).
narrative_ontology:cs_authority_grounding('6dbf5e14-aebc-4c7b-bba9-c974869569e4', lineage).
narrative_ontology:cs_interpretation_layer_present('6dbf5e14-aebc-4c7b-bba9-c974869569e4').
narrative_ontology:cs_reading_relation('6dbf5e14-aebc-4c7b-bba9-c974869569e4', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6dbf5e14-aebc-4c7b-bba9-c974869569e4', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6dbf5e14-aebc-4c7b-bba9-c974869569e4', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('6dbf5e14-aebc-4c7b-bba9-c974869569e4', foundational, housing_market_is_social_construct).
narrative_ontology:cs_axiom_status(housing_market_is_social_construct, holdable).
narrative_ontology:cs_axiom_grounding('6dbf5e14-aebc-4c7b-bba9-c974869569e4', housing_market_is_social_construct, conventional).
narrative_ontology:cs_axiom('6dbf5e14-aebc-4c7b-bba9-c974869569e4', foundational, policy_shapes_distribution).
narrative_ontology:cs_axiom_status(policy_shapes_distribution, holdable).
narrative_ontology:cs_axiom_grounding('6dbf5e14-aebc-4c7b-bba9-c974869569e4', policy_shapes_distribution, instrumental).
narrative_ontology:cs_reference_frame('6dbf5e14-aebc-4c7b-bba9-c974869569e4', post_war_regulatory_state).
narrative_ontology:cs_drift_state('6dbf5e14-aebc-4c7b-bba9-c974869569e4', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6dbf5e14-aebc-4c7b-bba9-c974869569e4', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_developers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_agents).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, low_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from rising property values and the stability provided by existing zoning and lending standards, which protect their asset appreciation. Their ability to exit the market is constrained by transaction costs and the need for alternative housing.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    powerful, biographical, constrained, local).

% Profit from the volume and value of mortgages, which are directly influenced by lending standards and property prices. They have significant influence over policy and can arbitrage regulatory differences.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, lenders, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from the scarcity created by restrictive zoning, which drives up the value of developable land and new construction. They can shift projects between localities with more favorable regulations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_developers, beneficiary,
    powerful, biographical, mobile, regional).

% Earn commissions based on property values, directly benefiting from higher prices. Their exit options are limited by the local market conditions and licensing requirements.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_agents, beneficiary,
    moderate, biographical, constrained, local).

% Bear the direct cost of high housing prices through rent, with limited options for affordable housing due to market conditions shaped by the constraint. Often trapped by economic circumstances and lack of alternatives.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, trapped, local).

% Struggle to enter the housing market due to high prices and stringent lending standards. Their options are limited to saving for longer, seeking assistance, or relocating.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_homebuyers, payer,
    powerless, immediate, constrained, local).

% Disproportionately affected by high housing costs, often facing housing insecurity and displacement. Their options are severely limited by income and lack of affordable housing stock.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, low_income_households, payer,
    powerless, immediate, trapped, local).

% Administer and enforce zoning laws, property taxes, and building codes, which directly shape housing supply and costs. They are constrained by state laws and local political pressures.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_governments, agenda_setter,
    institutional, generational, constrained, local).

% Set national lending standards and influence tax policy, impacting the broader housing market. They have an analytical perspective on the system but are subject to political cycles.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, federal_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the impact of housing policies on affordability and equity, advocating for reforms. They observe the system and attempt to influence it through public pressure and lobbying.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_advocates, observer,
    organized, biographical, analytical, national).

% Adhere to a view that housing prices are primarily determined by natural market forces of supply and demand, rather than institutional constructs. Their perspective is often excluded from policy debates focused on regulatory reform.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, naturalist_economists, excluded,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, lenders).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for land use, property development, and financial transactions in housing markets, providing predictability for investors, developers, and homeowners.
% TRANSFER_FUNCTION: Transfers wealth from those seeking housing (renters, first-time buyers) to existing property owners, lenders, and developers, by artificially restricting supply and increasing the cost of access.
% ABSENT_VOICES: Future generations and those permanently priced out of housing are structurally absent from the policy-making process that shapes these institutional arrangements. Their interests are not directly represented.
% DISAPPEARANCE_RATIONALE: If zoning, lending standards, tax treatment, and intermediary platforms vanished overnight, the housing market would undergo a radical, chaotic reorganization. Land use would be unregulated, lending would be unstandardized, and property transactions would lack formal structure, leading to immense disruption and a complete re-evaluation of property values and access.
% FOUNDING_PROBLEM: To ensure orderly urban development, protect property values, manage financial risk in lending, and generate local tax revenue.
% FOUNDING_PROBLEM_CORROBORATION: Local governments and lenders argue the founding problems (orderly development, financial stability) are still live. Housing advocates and some economists argue that while these problems persist, the current institutional arrangements have exacerbated affordability crises, indicating a shift in function. Independent urban planning studies and economic analyses corroborate the contested status.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is moderate to high because these institutional arrangements, while providing some coordination, systematically transfer wealth from those seeking housing to those who already own or profit from its provision. Suppression (0.55) is moderate, as alternatives to the established housing market are limited by these very rules, though not entirely absent. The theater ratio (0.15) is low, indicating that the institutions largely perform their stated functions, even if those functions have extractive outcomes. Accessibility collapse (0.45) is moderate, as while the system is complex, its rules are generally understood, but navigating them to find affordable alternatives is difficult. Resistance (0.5) is moderate, reflecting ongoing advocacy and political movements for housing reform.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent homeowners, lenders, and developers experience this constraint as a stable, beneficial framework that protects their investments and facilitates profit. Renters and first-time homebuyers, however, experience the same structure as a barrier to access and a source of significant financial burden. Local governments and federal regulators, as agenda-setters, navigate the conflicting demands of these groups, often balancing revenue generation and stability with calls for affordability.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, lenders, and developers are clear beneficiaries, as the system's design favors asset appreciation and profit generation. Renters and first-time homebuyers are victims, bearing the costs of inflated prices and restricted supply. Local and federal governments act as agenda-setters, creating and enforcing the rules. Housing advocates observe and challenge the system, while naturalist economists are largely excluded from this institutional framing of the problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to ensure orderly development and financial stability. While these problems persist, the institutional reading suggests that the mechanisms designed to solve them have, over time, become tools for wealth transfer and rent extraction, leading to a contested status for the founding problem. The persistence of the constraint, despite its perceived shift in function, indicates a potential for mandatrophy, where the original coordination function is overshadowed by extractive outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_price,
    'To what extent are housing prices determined by ''natural'' factors (e.g., objective scarcity, inherent desirability) versus ''constructed'' institutional factors (zoning, lending, tax)?',
    'Comparative analysis of housing markets with vastly different institutional frameworks (e.g., highly regulated vs. deregulated, different tax regimes) to isolate the impact of institutional variables.',
    'If natural factors dominate, the constraint''s extractiveness is lower, and its classification shifts towards a Mountain or Rope. If constructed factors dominate, the Tangled Rope classification is reinforced, highlighting policy as the primary lever for change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_price, empirical, 'Ambiguity between natural market forces and institutional construction in price formation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by renters and first-time buyers primarily structural (e.g., lack of affordable housing stock, restrictive zoning) or internalized (e.g., belief that homeownership is an unattainable dream, resignation to high rents)?',
    'Longitudinal studies tracking housing outcomes and psychological states of individuals in areas undergoing significant policy reforms (e.g., upzoning, rent control) to see if perceived options and agency increase with structural changes.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, requiring interventions beyond policy changes. If primarily structural, policy reforms are more likely to directly alleviate the burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for housing access.').

omega_variable(
    institutional_vs_financialization_drivers,
    'Is the primary driver of housing price inflation the institutional framework (zoning, lending, tax) or the financialization of housing as an asset class (credit expansion, speculative investment)?',
    'Econometric modeling that disentangles the causal pathways and relative contributions of institutional policy variables versus financial market variables to housing price movements over time.',
    'If financialization is the dominant driver, the ''financialization_reading'' gains explanatory power, potentially shifting the focus of policy interventions from zoning reform to financial regulation. If institutional factors are primary, the ''institutional_reading'' is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_financialization_drivers, empirical, 'Distinguishing institutional vs. financialization drivers of housing prices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__institutional_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(pric_tr_t1965, price_formation_kernel__institutional_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__institutional_reading, theater_ratio, 1980, 0.13).
narrative_ontology:measurement(pric_tr_t1995, price_formation_kernel__institutional_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__institutional_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__institutional_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(pric_be_t1965, price_formation_kernel__institutional_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__institutional_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(pric_be_t1995, price_formation_kernel__institutional_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__institutional_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__institutional_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(pric_su_t1965, price_formation_kernel__institutional_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__institutional_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(pric_su_t1995, price_formation_kernel__institutional_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__institutional_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, wealth_inequality_dynamics).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, naturalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'price_formation_kernel'. It focuses on the constructed nature of housing prices through institutional mechanisms, contrasting with naturalist, georgist, and financialization readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
