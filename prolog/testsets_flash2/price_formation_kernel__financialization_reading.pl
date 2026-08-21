% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__financialization_reading, []).

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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Housing Price Formation (Financialization Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes housing price formation through the lens of
 *   financialization, where prices are primarily driven by credit expansion,
 *   asset-price feedback loops, and demand for housing as a financial asset,
 *   rather than its utility as shelter. This reading posits a Tangled Rope,
 *   as the financial system coordinates capital but extracts heavily from
 *   households through debt and inflated prices. The price level is decoupled
 *   from shelter value, driven by leverage availability and speculative
 *   demand.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.85).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.78).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '1c0cdb9a-5901-49be-93ef-9b6a750bcb12').
narrative_ontology:cs_kernel_codification('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', distributed).
narrative_ontology:cs_authority_grounding('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', extraction).
narrative_ontology:cs_interpretation_layer_present('1c0cdb9a-5901-49be-93ef-9b6a750bcb12').
narrative_ontology:cs_reading_relation('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', foundational, housing_as_financial_asset).
narrative_ontology:cs_axiom_status(housing_as_financial_asset, holdable).
narrative_ontology:cs_axiom_grounding('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', housing_as_financial_asset, conventional).
narrative_ontology:cs_axiom('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', foundational, credit_drives_price_formation).
narrative_ontology:cs_axiom_status(credit_drives_price_formation, holdable).
narrative_ontology:cs_axiom_grounding('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', credit_drives_price_formation, empirically_contingent).
narrative_ontology:cs_reference_frame('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', unfettered_capital_markets).
narrative_ontology:cs_drift_state('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1c0cdb9a-5901-49be-93ef-9b6a750bcb12', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, asset_owners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, indebted_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from credit expansion, mortgage origination fees, securitization, and transaction volume. Actively lobbies for policies that favor housing as a financial asset and resists regulations that would curb credit growth or speculative investment. Sets lending standards and influences monetary policy.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% See their wealth grow through rising asset prices, often leveraging existing equity to acquire more property. They benefit from the feedback loops that decouple housing prices from underlying economic fundamentals, and often resist policies that would deflate asset values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, asset_owners, beneficiary,
    powerful, biographical, mobile, national).

% Face increasingly unaffordable housing prices driven by financial speculation and credit availability, requiring larger down payments and higher debt burdens. Their options are to delay homeownership, move to less desirable areas, or take on significant financial risk.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_homebuyers, payer,
    powerless, immediate, constrained, local).

% Experience rising rents as landlords pass on increased property values and debt service costs. They are often trapped in a cycle where saving for a down payment becomes impossible due to high rental costs, perpetuating their exclusion from ownership.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters, payer,
    moderate, immediate, constrained, local).

% Are highly exposed to interest rate fluctuations and economic downturns, with their primary asset (their home) also being their largest liability. Their identity as homeowners is tied to the financial system, making exit from the debt-driven market difficult without significant loss.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, indebted_households, payer,
    moderate, biographical, identity_locked, national).

% Analyze the systemic drivers of housing unaffordability, documenting the role of financialization and advocating for policy changes that prioritize housing as shelter over investment. They collect data and propose alternative regulatory frameworks.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation towards housing development and ownership, enabling large-scale investment and facilitating transactions through a standardized credit system.
% TRANSFER_FUNCTION: Transfers wealth from households (via debt service, inflated prices, and rent) to the financial sector and existing asset owners, driven by the expansion of credit and asset-price appreciation.
% ABSENT_VOICES: Future generations and those permanently priced out of housing are structurally excluded from the current market's price-setting mechanisms; they would advocate for policies that prioritize affordability and de-financialize housing.
% DISAPPEARANCE_RATIONALE: If credit expansion and asset-price feedback loops vanished, housing prices would likely crash, the financial sector would face massive defaults, and the entire economy would undergo a profound restructuring as housing reverted to a primary function of shelter rather than investment.
% FOUNDING_PROBLEM: The need to efficiently allocate capital for housing construction and enable widespread homeownership, while providing a stable asset class for investment.
% FOUNDING_PROBLEM_CORROBORATION: The financial sector and asset owners claim the system still serves its founding purpose by providing liquidity and investment opportunities. Housing advocates and economists outside the benefiting parties argue the problem of affordability has been exacerbated, and the system now primarily serves rent extraction, not efficient capital allocation for shelter.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__financialization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__financialization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the financial sector captures significant value through interest, fees, and asset appreciation, while households bear the costs of inflated prices and debt. Suppression (0.78) is high due to the systemic nature of financialization, where alternative housing models or non-speculative markets are actively marginalized or suppressed by policy and market structure. Theater ratio is low (0.20) because the system is highly functional in its extractive capacity, with minimal performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   The financial sector perceives this system as efficient capital allocation and wealth creation, while households experience it as an extractive trap. The engine's classification will reflect this divergence, showing a beneficial outcome for the financial sector and an extractive one for households, despite the shared underlying constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial sector and asset owners are clear beneficiaries, with arbitrage-grade exit options and institutional power, allowing them to shape the market to their advantage. First-time homebuyers, renters, and indebted households are targets, facing constrained or identity-locked exit options due to the systemic nature of the market and their dependence on it for shelter. Housing advocates act as observers, analyzing the system's dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_financialization,
    'To what extent is financialization the primary driver of housing price inflation, versus other factors like supply constraints, population growth, or regulatory burdens?',
    'Comparative econometric studies across diverse housing markets with varying degrees of financialization and regulatory environments, controlling for other variables.',
    'If financialization is less dominant, the extractiveness and suppression attributed to this constraint would decrease, potentially shifting its classification towards a more neutral coordination type or even a mountain (if scarcity is primary). If more dominant, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_financialization, empirical, 'Determines the relative causal weight of financial factors in housing price formation.').

omega_variable(
    decoupling_of_price_from_value,
    'At what point does the market price of housing decouple from its fundamental value as shelter, and what metrics best capture this divergence?',
    'Development of robust, cross-jurisdictional ''shelter value'' indices (e.g., cost of construction + maintenance + land rent for non-speculative use) and comparison with market prices over time.',
    'A clear and persistent decoupling would strengthen the ''extraction'' component of this constraint, supporting the Tangled Rope classification. If prices largely track shelter value, the constraint would appear less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_of_price_from_value, conceptual, 'Defines the boundary between housing as shelter and housing as financial asset.').

omega_variable(
    identity_lock_mechanism,
    'For indebted households, is the ''identity_locked'' exit option primarily due to financial entanglement (e.g., underwater mortgages, high transaction costs) or psychological/social factors (e.g., cultural value of homeownership, fear of social stigma)?',
    'Qualitative sociological studies and behavioral economics research on homeowner decision-making under financial stress, combined with quantitative analysis of financial barriers to exit.',
    'If psychological/social factors are dominant, the ''suppression'' metric for these households might be higher than structural measures suggest, as the constraint is internalized. If financial entanglement is primary, the structural suppression is accurately captured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Distinguishes between structural and internalized components of identity-locked exit for homeowners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__financialization_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__financialization_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__financialization_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__financialization_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__financialization_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__financialization_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__financialization_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__financialization_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__financialization_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, wealth_inequality_amplification).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'price_formation_kernel'. Each reading offers a distinct structural explanation for housing price dynamics, leading to different classifications and policy implications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
