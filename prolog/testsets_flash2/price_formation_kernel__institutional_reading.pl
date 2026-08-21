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
 *   institutional choices, including zoning regulations, mortgage lending
 *   standards, property tax structures, and the role of real estate
 *   platforms. It is an 'institutional reading' of the broader
 *   'price_formation_kernel', contrasting with naturalist, georgist, and
 *   financialization readings. This reading emphasizes that prices are not
 *   merely market outcomes but are actively shaped by policy and regulatory
 *   frameworks, creating identifiable beneficiaries (incumbent homeowners,
 *   lenders) and victims (renters, first-time buyers). The claimed type is
 *   'tangled_rope' because it performs a coordination function (orderly
 *   development, financial stability) but also enables significant asymmetric
 *   extraction.
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
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '09ac1eb1-da5d-4741-b5d4-358d240b926b').
narrative_ontology:cs_kernel_codification('09ac1eb1-da5d-4741-b5d4-358d240b926b', formalized).
narrative_ontology:cs_authority_grounding('09ac1eb1-da5d-4741-b5d4-358d240b926b', practice).
narrative_ontology:cs_interpretation_layer_present('09ac1eb1-da5d-4741-b5d4-358d240b926b').
narrative_ontology:cs_reading_relation('09ac1eb1-da5d-4741-b5d4-358d240b926b', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('09ac1eb1-da5d-4741-b5d4-358d240b926b', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('09ac1eb1-da5d-4741-b5d4-358d240b926b', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('09ac1eb1-da5d-4741-b5d4-358d240b926b', foundational, housing_is_a_regulated_market).
narrative_ontology:cs_axiom_status(housing_is_a_regulated_market, holdable).
narrative_ontology:cs_axiom_grounding('09ac1eb1-da5d-4741-b5d4-358d240b926b', housing_is_a_regulated_market, conventional).
narrative_ontology:cs_axiom('09ac1eb1-da5d-4741-b5d4-358d240b926b', foundational, policy_shapes_market_outcomes).
narrative_ontology:cs_axiom_status(policy_shapes_market_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('09ac1eb1-da5d-4741-b5d4-358d240b926b', policy_shapes_market_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('09ac1eb1-da5d-4741-b5d4-358d240b926b', post_war_regulatory_consensus).
narrative_ontology:cs_drift_state('09ac1eb1-da5d-4741-b5d4-358d240b926b', contemporary_unaffordability_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09ac1eb1-da5d-4741-b5d4-358d240b926b', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_developers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, local_governments).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, low_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from rising property values and the protection of their investment through restrictive zoning and tax policies that favor existing ownership. They actively lobby local governments to maintain these conditions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, biographical, mobile, local).

% Profit from the volume and value of housing transactions, with lending standards and government-backed programs creating a stable, profitable market. They influence policy to ensure continued demand for mortgage products.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from high demand and limited supply, which drives up prices for new construction. While they face regulatory hurdles, these often serve to limit competition and increase the value of their projects. They lobby for favorable zoning changes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_developers, beneficiary,
    powerful, biographical, constrained, regional).

% Administer and enforce zoning laws, property taxes, and building codes. They benefit from increased property tax revenues and often respond to the political pressure of incumbent homeowners to maintain restrictive policies, which can limit housing supply.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_governments, agenda_setter,
    institutional, generational, constrained, local).

% Bear the direct cost of high housing prices through increased rents, with limited options for affordable housing due to supply restrictions. Their ability to influence policy is often diffuse and unorganized.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Struggle to enter the housing market due to high prices, strict lending standards, and competition. They face significant barriers to accumulating down payments and qualifying for mortgages, often delaying homeownership or forcing them into less desirable areas.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_homebuyers, payer,
    moderate, biographical, constrained, local).

% Are most severely impacted by high housing costs, often leading to housing insecurity, displacement, and increased financial strain. Their options are extremely limited, with few affordable alternatives.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, low_income_households, payer,
    powerless, immediate, trapped, local).

% Analyze the structural causes of housing unaffordability and advocate for policy changes, such as zoning reform, increased public housing, and tenant protections. They work to raise awareness and influence public opinion and legislative action.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development, financing, and allocation of housing by establishing rules for land use, construction quality, and financial access, aiming to ensure orderly growth and property value stability.
% TRANSFER_FUNCTION: Transfers wealth from renters and first-time homebuyers to incumbent homeowners, mortgage lenders, and developers through inflated property values and rents, enabled by regulatory and financial structures.
% ABSENT_VOICES: Future generations and potential residents are structurally absent from current zoning and land-use decisions, which prioritize existing residents' property values over future housing needs. Their interests are represented only indirectly by advocates.
% DISAPPEARANCE_RATIONALE: If the institutional framework (zoning, lending, tax treatment) vanished, the housing market would undergo a radical transformation. Land use would deregulate, construction would likely increase, property values would re-align with more fundamental costs, and the distribution of housing wealth would shift dramatically, leading to a complete reorganization of urban and suburban landscapes.
% FOUNDING_PROBLEM: To ensure stable property values, orderly urban development, and financial security for homeowners, preventing speculative booms and busts, and protecting neighborhood character.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent homeowners and local governments often claim the problem is live, citing the need for stability and local control. Housing advocates and economists, from outside the benefiting parties, argue that while some aspects of the problem (e.g., preventing chaos) are addressed, the current institutional structure has shifted to primarily serve rent extraction and wealth accumulation, making the original problem 'dead' in its current form.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because the institutional framework systematically limits supply and inflates demand, leading to prices that exceed fundamental costs for many. Suppression (0.75) is also high, as the system actively suppresses alternative housing models (e.g., dense multi-family housing in single-family zones) and exit options for those trapped by high costs. Theater ratio is moderate (0.20); while some regulatory functions are genuine, a portion of the 'planning' and 'stability' rhetoric serves to mask the extractive effects. The metrics show a clear trend of increasing extractiveness and suppression over time, reflecting the hardening of these institutional structures.
 *
 * PERSPECTIVAL GAP:
 *   The institutional reading highlights a significant perspectival gap: for beneficiaries, the system appears as a stable, well-ordered market that protects investments and ensures quality. For victims, it is a rigged game that systematically transfers wealth and limits opportunity. The engine's per-seat classification should reflect this divergence, with beneficiaries experiencing a 'rope'-like coordination and victims experiencing a 'snare'-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, mortgage lenders, and developers are beneficiaries, as the system inflates their assets and revenue streams. Local governments, as agenda-setters, also benefit from increased property tax bases, aligning their interests with maintaining the status quo. Renters and first-time homebuyers are clear victims, bearing the costs of inflated prices and restricted access. Housing advocates act as observers, analyzing and challenging the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted significantly. While initially intended to ensure orderly development and financial stability (a genuine coordination problem), the institutional framework has increasingly become a mechanism for wealth transfer and asset protection for existing owners. The 'contested' status of the founding problem reflects this drift: the coordination function persists, but the extractive function has grown disproportionately, preventing it from being a pure rope or a piton. It is a tangled rope because the coordination and extraction are intertwined and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_natural_scarcity,
    'To what extent do high housing prices reflect genuine natural scarcity of land versus institutionally constructed scarcity (e.g., restrictive zoning)?',
    'Comparative analysis of housing markets with similar natural endowments but different regulatory regimes; economic modeling of price elasticity under zoning reform.',
    'If institutional scarcity dominates, the extractiveness and suppression metrics are more accurate, and the constraint is more amenable to policy intervention. If natural scarcity dominates, the constraint leans more towards a mountain, and policy interventions would have limited impact on price.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_natural_scarcity, empirical, 'Distinguishing between natural and constructed scarcity in housing markets.').

omega_variable(
    policy_intent_vs_outcome,
    'Are the current institutional arrangements (zoning, tax, lending) primarily designed to achieve their stated coordination goals (e.g., neighborhood stability, financial security) or have their primary effects shifted to wealth transfer and rent extraction?',
    'Historical analysis of legislative intent vs. observed outcomes; analysis of lobbying efforts by beneficiary groups; public opinion surveys on perceived fairness and effectiveness.',
    'If outcomes diverge significantly from stated intent, the ''tangled_rope'' classification is strongly reinforced, potentially shifting towards ''snare'' if the coordination function is found to be largely theatrical. If intent and outcome align, it would support a ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome, conceptual, 'Assessing the alignment of institutional policy intent with actual market outcomes.').

omega_variable(
    reading_interdependence,
    'How does the ''institutional_reading'' interact with the ''financialization_reading''? Does institutional structure enable financialization, or are they independent drivers of price formation?',
    'Econometric analysis of the causal pathways between regulatory changes, credit expansion, and asset price inflation; case studies of markets where one factor is dominant.',
    'If institutional structures are found to be a primary enabler of financialization, the combined extractiveness of the system is higher than either reading alone suggests. If they are largely independent, the two readings describe distinct, though interacting, constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_interdependence, empirical, 'Causal relationship between institutional structures and financialization in housing markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__institutional_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(pric_tr_t1970, price_formation_kernel__institutional_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__institutional_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__institutional_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(pric_be_t1970, price_formation_kernel__institutional_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__institutional_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__institutional_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(pric_su_t1970, price_formation_kernel__institutional_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__institutional_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, wealth_inequality_amplification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
