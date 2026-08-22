% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Financialized Price Formation in Housing Markets
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   This constraint describes the financialization reading of price formation
 *   in housing markets: prices are driven not by shelter scarcity or
 *   construction cost, but by the availability and terms of credit, the
 *   feedback loops between collateral values and lending capacity, and the
 *   reclassification of housing as a financial asset class yielding returns
 *   to global capital. The constraint is a TANGLED ROPE: it performs a
 *   genuine coordination function (channeling savings into housing via
 *   standardized mortgage markets) while simultaneously extracting
 *   asymmetrically — the financial sector captures origination fees,
 *   securitization spreads, yield compression gains, and asset appreciation,
 *   while households bear debt service, crash risk, displacement, and the
 *   macroeconomic instability of credit cycles. The engine computes per-seat
 *   classification from structural data; the divergence between the financial
 *   sector's 'coordination' experience and households' 'extraction'
 *   experience is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.72).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Financialized Price Formation in Housing Markets").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "economic/political/social").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '0e8960d1-6256-4d6b-8bbc-a2dfc16b6280').
narrative_ontology:cs_kernel_codification('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', distributed).
narrative_ontology:cs_authority_grounding('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', extraction).
narrative_ontology:cs_interpretation_layer_present('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280').
narrative_ontology:cs_reading_relation('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_axiom('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', foundational, credit_availability_is_primary_price_driver).
narrative_ontology:cs_axiom_status(credit_availability_is_primary_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', credit_availability_is_primary_price_driver, empirically_contingent).
narrative_ontology:cs_axiom('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', foundational, housing_financialization_benefits_financial_sector_at_household_expense).
narrative_ontology:cs_axiom_status(housing_financialization_benefits_financial_sector_at_household_expense, holdable).
narrative_ontology:cs_axiom_grounding('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', housing_financialization_benefits_financial_sector_at_household_expense, empirically_contingent).
narrative_ontology:cs_reference_frame('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', post_bretton_woods_credit_expansion_era).
narrative_ontology:cs_drift_state('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', post_2008_financial_crisis_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e8960d1-6256-4d6b-8bbc-a2dfc16b6280', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, investment_banks).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, asset_managers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, private_equity_landlords).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, construction_finance_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters_priced_out).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, leveraged_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, low_income_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, leveraged_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, municipal_governments).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, credit_drives_asset_prices).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, housing_as_financial_asset_paradigm).
narrative_ontology:constraint_vindicates(price_formation_kernel__financialization_reading, leverage_availability_sets_price_floor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originate and securitize mortgage debt at scale. Profit from origination fees, servicing spreads, and securitization margins. Lobby for expanded credit access, favorable capital treatment of mortgage assets, and regulatory forbearance. Can shift to other asset classes if housing credit contracts.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_lenders, agenda_setter).

% Structure, underwrite, and trade mortgage-backed securities and structured credit products. Capture fees at each layer of the securitization chain. Design products that amplify leverage and tranche risk. Exit to other structured credit markets if housing securitization volume declines.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, investment_banks, beneficiary,
    institutional, biographical, arbitrage, global).

% Allocate institutional capital to residential mortgage-backed securities, REITs, and single-family rental portfolios. Benefit from yield compression and asset appreciation driven by credit expansion. Redeploy capital across asset classes based on risk-adjusted returns.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, asset_managers, beneficiary,
    organized, biographical, mobile, global).

% Acquire distressed or bulk housing portfolios using cheap leverage. Convert owner-occupied stock to rental, extracting yield from rent growth funded by credit-driven price appreciation. Exit constrained by portfolio illiquidity and regulatory scrutiny of corporate landlordism.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, private_equity_landlords, beneficiary,
    powerful, biographical, constrained, national).

% Control development finance pipelines — determine what gets built, where, and at what price point. Capture fees and equity participation. Shape supply response to credit-driven demand signals. Exit constrained by project-specific commitments and local market entrenchment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, construction_finance_intermediaries, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, construction_finance_intermediaries, beneficiary).

% Face prices decoupled from local incomes, driven by credit availability and investor demand. Must stretch leverage to dangerous levels or exit to rental markets. Exit constrained by employment location, family formation needs, and lack of affordable alternatives.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyers, payer,
    moderate, biographical, constrained, local).

% Absorb rent increases driven by asset-price inflation and investor yield requirements. No equity accumulation, no tax advantages, no leverage access. Exit trapped by geographic immobility, deposit barriers, and absence of social housing alternatives.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters_priced_out, payer,
    powerless, biographical, trapped, local).

% Hold mortgage debt at high loan-to-value ratios against inflated collateral. Benefit from paper appreciation but bear crash risk and debt service burden. Exit identity-locked: homeownership is constitutive of middle-class identity, retirement planning, and social standing; selling realizes losses and breaks the identity frame.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, leveraged_homeowners, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, leveraged_homeowners, beneficiary).

% Priced out of both ownership and formal rental markets. Pushed into informal, overcrowded, or substandard housing. No voice in credit policy, zoning, or investment allocation decisions that determine housing supply and cost. Would object to financialization if structurally included.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, low_income_households, excluded,
    powerless, immediate, trapped, local).

% Set monetary policy and prudential rules that govern credit creation and allocation. Mandate price stability and financial stability — often in tension when credit-driven asset inflation is the transmission mechanism. Analyze but rarely intervene directly in housing credit channels.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks_financial_regulators, observer,
    institutional, generational, analytical, national).

% Bear fiscal costs of displacement, homelessness, and infrastructure strain from financialized development patterns. Depend on property tax base inflated by the same financialization. Constrained by higher-level policy frameworks and capital mobility.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, municipal_governments, observer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, municipal_governments, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels global savings into residential housing production and allocation through standardized mortgage credit and securitization infrastructure. Solves the maturity transformation and risk-pooling problem that would otherwise make long-term housing finance unavailable at scale.
% TRANSFER_FUNCTION: Moves interest payments, principal amortization, and transaction fees from households (buyers and renters) to financial intermediaries (lenders, banks, asset managers, landlords). Moves capital gains from leveraged appreciation to asset holders. Moves crash losses to leveraged households and, via bailout channels, to taxpayers.
% ABSENT_VOICES: Low-income households, renters in informal tenure, future generations who inherit the debt-overhang and degraded housing stock, and non-financial firms crowded out by credit misallocation. They are excluded by lack of financial assets, political representation, and epistemic access to credit-policy discourse.
% DISAPPEARANCE_RATIONALE: If credit-driven price formation vanished overnight, housing prices would collapse to shelter-value fundamentals, mortgage markets would contract sharply, financial sector balance sheets would shrink, construction would reorient to affordable tenure models, and millions of leveraged households would face negative equity — a fundamental reorganization of the housing-finance-economy nexus.
% FOUNDING_PROBLEM: Post-WWII housing shortage and the inability of traditional building-society / thrift models to scale mortgage credit to mass homeownership. The financialization reading traces to the 1970s-80s securitization innovation that promised to solve credit rationing by connecting housing to global capital markets.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis by Perry Mehrling (money view), Katharina Pistor (legal coding of capital), and Adair Turner (Between Debt and the Devil) attest the original credit-rationing problem was solved by the 1990s; subsequent expansion served financial sector profitability, not housing access. Central bank speeches (e.g., Borio, Disyatat) acknowledge credit growth has decoupled from real economy needs. No major institutional voice outside the financial sector claims current credit volumes are necessary for housing provision.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the wedge between shelter-value fundamentals and credit-driven price levels represents a massive, recurring transfer. Suppression (0.72) is high because the constraint persists through active policy choices: subsidized mortgage credit, favorable tax treatment of leveraged ownership, bailout expectations, zoning that restricts non-financialized supply, and suppression of tenant protections — not through participant preference. Theater ratio (0.45) is substantial: the 'homeownership society' and 'housing ladder' narratives perform coordination while the actual mechanism is leverage-enabled extraction. Accessibility collapse (0.68) is high because alternatives (social housing, non-speculative tenure, credit guidance) have been ideologically and institutionally marginalized. Resistance (0.55) is moderate: political pushback exists (rent control, eviction moratoria, public banking proposals) but is fragmented and lacks structural power.
 *
 * PERSPECTIVAL GAP:
 *   From the financial sector's seat, this is a rope — a coordination mechanism that solves maturity transformation and risk pooling. From the leveraged homeowner's seat, it is a tangled rope — they get coordination (access to credit) but pay extraction (debt service, crash risk). From the renter's seat, it is a snare — pure extraction with no coordination benefit. The engine computes this divergence from the declared roles, power, and exit options; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector agents (lenders, banks, asset managers, PE landlords) are structural beneficiaries: they collect the extraction, set the agenda, and hold arbitrage-grade exit. Households (first-time buyers, renters, leveraged owners) are structural targets: they pay the transfer, face constrained or identity-locked exit, and bear crash risk. Central banks and municipalities are observers who experience the constraint's macroeconomic effects but do not directly collect or pay the core transfer — though municipalities bear fiscal externalities. Low-income households are excluded entirely: they would object if present but have no seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credit rationing for mass homeownership) was solved by the 1990s. The arrangement persists because it now serves financial sector profitability — credit expansion, securitization volume, and asset management fees. The constraint has undergone mandatrophy: its mandate (housing access) is dead, but the structure persists and has inverted into extraction. The 'coordination' function is now the cover story for the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (mortgage credit intermediation) be separated from the extractive structure (leverage-driven price inflation, securitization complexity, investor yield extraction)?',
    'Counterfactual policy simulation: a public mortgage utility providing standardized credit at cost, with strict loan-to-value and debt-to-income caps, no securitization, and no investor yield channel. If housing access is maintained at lower price levels, the functions are separable.',
    'If separable, the current extractive layer is removable without losing coordination — supporting a rope or scaffold classification for the coordination core and snare for the financialization overlay. If inseparable, the tangled rope classification holds for the unified structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components of financialized housing finance are structurally separable.').

omega_variable(
    crash_loss_socialization_mechanism,
    'To what extent are the crash losses of the financialized price formation constraint socialized onto taxpayers vs. borne by the financial sector?',
    'Historical accounting of fiscal costs of mortgage market interventions (2008 TARP, GSE conservatorship, 2020-21 forbearance programs, deposit insurance backstops) vs. financial sector losses absorbed without public support.',
    'High socialization increases effective extraction on the taxpayer seat (not currently modeled as a stakeholder) and strengthens the snare/tangled rope classification. Low socialization would imply the financial sector bears its own risk, reducing net extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crash_loss_socialization_mechanism, empirical, 'Degree of loss socialization in financialized housing credit cycles.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the financialization reading''s framing of price formation as ''credit-driven'' foreclose the institutional reading''s claim that ''zoning and lending standards construct prices,'' or do they operate at different causal levels?',
    'Causal decomposition: does credit expansion operate *through* zoning/lending standards (institutional reading as mechanism) or *alongside* them as an independent driver? Empirical work on credit supply shocks (e.g., Di Maggio & Kermani, Favara & Imbs) vs. zoning elasticity studies (Glaeser, Gyourko, Saiz).',
    'If financialization forecloses institutional reading, reading_relation = forecloses. If they are complementary causal layers, relation = coexists_with or influences. This determines the cs_structure.reading_relations topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Causal relationship between financialization and institutional readings of price formation.').

omega_variable(
    identity_lock_mechanism_homeownership,
    'What specific mechanism binds leveraged_homeowners to the identity_locked exit state — professional identity, relational identity, ideological identity, or institutional identity?',
    'Qualitative longitudinal studies of underwater homeowners'' decision-making; comparison with jurisdictions where strategic default is normalized vs. stigmatized.',
    'If ideological (homeownership as moral/civic virtue), the lock is deeper and more resistant to policy change. If institutional (retirement planning, tax-advantaged savings vehicle), the lock is policy-contingent and could be redesigned. Affects the persistence of extraction on this seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_homeownership, empirical, 'Mechanism of identity lock for leveraged homeowners in financialized housing markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__financialization_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__financialization_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(pric_tr_t2007, price_formation_kernel__financialization_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__financialization_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(pric_tr_t2015, price_formation_kernel__financialization_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__financialization_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__financialization_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__financialization_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__financialization_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(pric_be_t2007, price_formation_kernel__financialization_reading, base_extractiveness, 2007, 0.75).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__financialization_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(pric_be_t2015, price_formation_kernel__financialization_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__financialization_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__financialization_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__financialization_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__financialization_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(pric_su_t2007, price_formation_kernel__financialization_reading, suppression_requirement, 2007, 0.7).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__financialization_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(pric_su_t2015, price_formation_kernel__financialization_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__financialization_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__financialization_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.12).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, mortgage_securitization_infrastructure).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, zoning_density_constraints).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, property_tax_assessment_systems).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, rental_market_regulation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, construction_labor_market).

% DUAL FORMULATION NOTE:
% This is the financialization_reading of the price_formation_kernel. It decomposes the kernel's natural-language concept into a structurally precise claim with its own ε, stakeholders, and classification. Linked to sibling readings via shared kernel_id. The naturalist_reading would be a mountain candidate; institutional_reading a tangled_rope with different beneficiaries; georgist_reading a rope/scaffold depending on land-value-tax adoption status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, moderate, 0.85).
constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, powerless, 0.95).
constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
