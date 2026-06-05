% ============================================================================
% CONSTRAINT STORY: financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financialization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: financialization_reading
 *   human_readable: Financialization of Housing: Credit Expansion and Asset-Price Feedback
 *   domain: political_economy/housing_markets/financial_system
 *
 * SUMMARY:
 *   The financialization reading of housing price formation asserts that
 *   price levels are decoupled from shelter value and driven primarily by
 *   credit expansion, leverage availability, and asset-price feedback loops.
 *   Under this reading, housing functions as financial asset for existing
 *   holders and investors, while households purchasing for shelter are
 *   systematically extracted through debt service, refinancing cycles, and
 *   exposure to crash risk. The financial sector benefits from transaction
 *   volume, spreads, and leverage creation; existing asset holders benefit
 *   from price appreciation; debt-service households and future entrants bear
 *   the cost through extended amortization, payment-to-income ratios, and
 *   generational lock-in. The constraint exhibits tangled rope structure:
 *   coordination function (credit allocation enabling housing transactions)
 *   coexists with asymmetric extraction (financial sector and asset holders
 *   capture upside; households bear downside). The teatrical component
 *   (housing policy rhetoric of affordability) masks the underlying
 *   credit-driven mechanism. This reading is one of four distinct causal
 *   theories of housing price formation (financialization,
 *   naturalist/scarcity, institutional/regulatory-capture,
 *   georgist/land-monopoly), each producing different beneficiary structures
 *   and policy implications. The readings are not reconcilable within a
 *   single theoretical framework — they instantiate structurally different
 *   constraints with different ε values.
 *
 * KEY AGENTS:
 *   - Financial Sector: Institutional beneficiary (institutional/arbitrage) — captures leverage creation, transaction fees, spreads, refinancing volume; net beneficiary with high optionality
 *   - Existing Asset Holders: Mixed beneficiary-constrained (institutional/constrained) — benefits from price appreciation but entangled in system dependencies; cannot exit without accepting loss or reallocating within same system
 *   - Debt-Service Households: Primary victim (powerless/trapped) — locked into decades of debt service, mortgage commitment, and exposure to refinancing cycles; no structural exit option
 *   - Future Housing Users: Generational victim (powerless/trapped) — born into inflated price regime; intergenerational lock-in; prices ratchet upward each leverage cycle
 *   - Renters: Structural victim (powerless/constrained) — excluded from asset appreciation; bear cost of financialization in rental markets (investor consolidation, yield-seeking behavior, conversion to financialized ownership)
 *   - Central Bank / Monetary Authority: Constrained coordinator (powerful/constrained) — enforces low rates and credit availability; coordination function genuine but extraction embedded in mechanism
 *   - Housing Policy Apparatus: Theatrical maintainer (institutional/constrained) — declares affordability mission while structurally maintaining financialization through zoning, tax incentives, lending regulation; piton classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financialization_reading, 0.68).
domain_priors:suppression_score(financialization_reading, 0.62).
domain_priors:theater_ratio(financialization_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financialization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(financialization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(financialization_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financialization_reading, tangled_rope).
narrative_ontology:human_readable(financialization_reading, "Financialization of Housing: Credit Expansion and Asset-Price Feedback").
narrative_ontology:topic_domain(financialization_reading, "political_economy/housing_markets/financial_system").

domain_priors:requires_active_enforcement(financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(financialization_reading, 'b6e84306-e1f5-4624-a7ab-801193d8c10f').
narrative_ontology:cs_kernel_codification('b6e84306-e1f5-4624-a7ab-801193d8c10f', distributed).
narrative_ontology:cs_authority_grounding('b6e84306-e1f5-4624-a7ab-801193d8c10f', extraction).
narrative_ontology:cs_interpretation_layer_present('b6e84306-e1f5-4624-a7ab-801193d8c10f').
narrative_ontology:cs_reading_relation('b6e84306-e1f5-4624-a7ab-801193d8c10f', financialization_reading__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6e84306-e1f5-4624-a7ab-801193d8c10f', financialization_reading__institutional_reading, influences).
narrative_ontology:cs_reading_relation('b6e84306-e1f5-4624-a7ab-801193d8c10f', financialization_reading__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('b6e84306-e1f5-4624-a7ab-801193d8c10f', foundational, credit_expansion_price_driver).
narrative_ontology:cs_axiom_status(credit_expansion_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('b6e84306-e1f5-4624-a7ab-801193d8c10f', credit_expansion_price_driver, empirically_contingent).
narrative_ontology:cs_axiom('b6e84306-e1f5-4624-a7ab-801193d8c10f', foundational, financial_sector_primary_beneficiary).
narrative_ontology:cs_axiom_status(financial_sector_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('b6e84306-e1f5-4624-a7ab-801193d8c10f', financial_sector_primary_beneficiary, deontological).
narrative_ontology:cs_reference_frame('b6e84306-e1f5-4624-a7ab-801193d8c10f', credit_expansion_driven_equilibrium).
narrative_ontology:cs_drift_state('b6e84306-e1f5-4624-a7ab-801193d8c10f', contemporary_institutional_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6e84306-e1f5-4624-a7ab-801193d8c10f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(financialization_reading, existing_asset_holders).
narrative_ontology:constraint_beneficiary(financialization_reading, construction_capital).
narrative_ontology:constraint_victim(financialization_reading, future_housing_users).
narrative_ontology:constraint_victim(financialization_reading, debt_service_households).
narrative_ontology:constraint_victim(financialization_reading, renters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-SERVICE HOUSEHOLD (SNARE) — Trapped by housing necessity and financing availability. Mortgage commitment locks the household into decades of extraction via interest, fees, and refinancing cycles. Price level driven by credit availability, not shelter value, forces overextension. No exit without losing housing or defaulting. Maximum extraction experience.
constraint_indexing:constraint_classification(financialization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE HOUSING USERS (SNARE) — Generational cohort locked into purchasing at prices inflated by financial demand and leverage. Prices ratched upward each cycle; credit standards reset. Users born into a housing cost regime they did not create and cannot exit. Trapped by demographic necessity and pre-set price floor.
constraint_indexing:constraint_classification(financialization_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SECTOR (ROPE) — Benefits from credit expansion volume, transaction fees, spreads, and leverage creation. Experiences constraint as pure coordination: extending credit enables transaction volume that would not exist without financialization. Net beneficiary. High arbitrage options — capital can reallocate across markets if housing credit becomes constrained.
constraint_indexing:constraint_classification(financialization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXISTING ASSET HOLDERS (TANGLED ROPE) — Benefit from rising prices driven by credit expansion and leverage demand. But also constrained by the system they benefit from: cannot exit the rising-price regime without accepting loss or reallocating capital within the same system. Coordination function (asset protection via price stability) and extraction (upward ratchet prevents downward correction) coexist. Moderate extraction because beneficiaries are also entangled in system risks.
constraint_indexing:constraint_classification(financialization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CENTRAL BANK (TANGLED ROPE) — Coordinates credit expansion necessary for financial stability and employment. But enforcement of low rates and credit availability extracts from savers and future inflation costs. Coordination function genuine (macro stability) but extraction embedded in the mechanism (financial repression of savers, future inflation tax). Constrained: cannot raise rates without triggering system stress or political pressure. Powerful but operationally locked by the system it maintains.
constraint_indexing:constraint_classification(financialization_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HOUSING POLICY APPARATUS (PITON) — Declares mission of 'housing affordability' while systematically maintaining the financialization mechanism. Zoning restrictions, tax incentives, and lending-friendly regulations present as affordability policy but function as price-support theater. Real mechanism buried in credit supply and asset-price feedback. Policy persists through institutional inertia despite internal contradiction between stated mission and structural enforcement.
constraint_indexing:constraint_classification(financialization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk naturalization: 'Housing prices must rise with economic development and population growth; credit expansion is how modern economies coordinate investment; leverage is inherent to capital allocation.' This perspective treats financialization as immutable law rather than contingent institutional arrangement. Engine false-summit detection will flag this as naturalization of extractive institutional mechanisms.
constraint_indexing:constraint_classification(financialization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financialization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financialization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financialization_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financialization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financialization_reading, TR),
    TR >= 0.70.

:- end_tests(financialization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, rising over interval. Credit expansion enables price elevation above replacement/shelter value. Rising leverage ratios and falling loan-to-value buffers over time indicate increasing extraction intensity as households over-extend. The baseline (t=0, value 0.35) reflects moderate extraction from early financialization stages (1990s); the t=20 value (0.68) reflects mature system with high payment-to-income ratios and institutional investor consolidation. Suppression (0.62): Moderately high, rising. Suppression mechanisms include: dependency on credit access (cannot purchase without leverage), information asymmetry (household pricing power limited by institutional investor consolidation and algorithmic price-setting), regulatory entrenchment (zoning prevents alternative housing supply responses), cultural normalization of 30-year debt commitment. Suppression rises as credit standards tighten and consolidation reduces buyer bargaining power. Theater ratio (0.55): Moderate, rising. 'Affordability' rhetoric in housing policy (inclusionary zoning, first-time homebuyer programs, community land trusts) presents as housing supply solutions but functions as price-support theater — they do not address the underlying credit-availability driver. Rising over time as policy apparatus elaborates affordability theater without touching credit mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The financialization reading produces maximal perspectival divergence. The financial sector sees coordination and opportunity (rope/arbitrage). Existing asset holders see mixed benefit-and-entanglement (tangled rope/constrained). Debt-service households see pure extraction and trap (snare/trapped). Future users see generational lock-in (snare/trapped across generations). The policy apparatus performs affordability while maintaining the extraction mechanism (piton/theatrical). The central bank experiences genuine coordination tension with extraction embedded in the mechanism (tangled rope/powerful-constrained). The analytical observer risks naturalizing the constraint as immutable law of economic development. This perspectival spread is diagnostic evidence that the financialization reading instantiates a genuine structural claim — single-perspective analysis would miss the asymmetry entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Financial sector: beneficiary status + arbitrage exit = d ≈ 0.1 (beneficiary end). Existing asset holders: beneficiary status + constrained exit = d ≈ 0.35 (mixed, leaning beneficiary but entangled). Debt-service households: victim status + trapped exit = d ≈ 0.95 (target end). Central bank: coordination function (beneficiary status) but constrained exit (politically locked) = d ≈ 0.40 (mixed, coordination dominates but extraction visible). The directional derivation chain flows from the constraint's structural mechanics: credit expansion benefits those controlling credit creation and asset appreciation; it extracts from those dependent on credit access for shelter. Power levels modulate the experienced intensity: powerless agents experience extraction more acutely than powerful ones with equivalent d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved in this reading by rejecting the naturalist interpretation: housing prices are NOT immutably tied to economic development or population growth. The financialization reading shows that price elevation is contingent on credit policy, leverage standards, and institutional investor participation. Different credit regimes (strict loan-to-value caps, transaction taxes, leverage limits) would produce different price trajectories while preserving housing transactions and shelter function. The mandate (housing policy declares affordability) has outlived its authentic function — early housing policy aimed at enabling homeownership and shelter access; contemporary policy maintains financialization rhetoric while obscuring the credit-driven mechanism. The piton classification captures this: housing policy apparatus maintains theater (affordability rhetoric, supply-side solutions, first-time homebuyer programs) that masks the underlying extraction mechanism. Mandatrophy is resolved by naming the policy apparatus's true function: price-support and extraction enablement, not affordability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_necessity_threshold,
    'What portion of price elevation above replacement cost is driven by credit expansion vs. scarcity value (land constraints, zoning restrictions)?',
    'Counterfactual analysis: modeling housing prices under alternative credit regimes (fixed lending standards, loan-to-value caps, geographic credit rationing); comparison of price trends in markets with different zoning elasticity',
    'If credit expansion > 50% of premium: financialization reading confirmed as primary driver. If scarcity mechanisms > 50%: financialization is secondary to zoning, and reading reclassifies toward institutional_reading. If mixed parity: both readings coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_necessity_threshold, empirical, 'Relative contribution of credit expansion vs scarcity mechanisms to price elevation').

omega_variable(
    feedback_loop_closure,
    'Does the asset-price feedback loop (higher prices → more collateral → more leverage → more demand → higher prices) self-sustain or require continuous credit supply acceleration?',
    'Time-series analysis of leverage ratios and price growth rates; identification of periods where price growth decoupled from credit growth; stress-test scenarios with exogenous credit constraints',
    'If self-sustaining: financialization constraint is structurally endogenous (snare for households). If requires acceleration: constraint depends on policy choice (tangled rope with central bank agency visible). Classification and beneficiary structure shift accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_closure, empirical, 'Whether asset-price feedback is self-sustaining or externally driven').

omega_variable(
    shelter_vs_asset_split,
    'What fraction of housing demand is shelter-motivated vs. financial-asset demand (investment, speculation, foreign capital, REIT/institutional holdings)?',
    'Microdata on investor vs owner-occupant purchase patterns; vacancy rates in investor-heavy markets; capital flows analysis (foreign investment, financial institution purchases); price differential between owner-occupied and investor property in same market',
    'If shelter-motivated > 80%: price formation is primarily scarcity-driven (institutional_reading dominates). If asset-demand > 40%: financialization reading confirmed. Distribution drives victim classification: high asset-demand markets show stronger extraction from future users and renters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shelter_vs_asset_split, empirical, 'Relative proportion of shelter vs financial-asset demand in housing market').

omega_variable(
    reading_kernel_contest,
    'Which reading of the price_formation_kernel is correct: financialization (credit + leverage driver), naturalist (scarcity + demographic driver), institutional (zoning + regulatory capture driver), or georgist (land monopoly driver)?',
    'This omega documents the kernel-level contest. The sibling readings (naturalist, institutional, georgist) are separate constraints in separate files. Each reading instantiates a different ε, different beneficiary structure, and different policy implications. Resolution requires comparative analysis across all four readings'' empirical predictions and structural decomposability.',
    'If financialization reading is correct: high extraction from households, primary beneficiary is financial sector. If naturalist reading is correct: constraint is coordination problem, no primary beneficiary. If institutional reading is correct: primary beneficiary is regulatory-capture coalition. If georgist reading is correct: primary beneficiary is land monopolists. The four readings are not reconcilable within a single framework — they represent structurally distinct causal chains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Which causal reading of price formation kernel is structurally correct').

omega_variable(
    extraction_vector_stability,
    'Does the financialization extraction vector persist in low-growth or negative-growth regimes, or does it depend on continuous price escalation?',
    'Historical cases of near-zero or negative price growth (Japan 1990-2010, some US markets 2008-2012); analysis of debt-service burden, household balance sheets, and financial sector returns in low-growth periods',
    'If extraction persists in low growth: constraint is structurally snare (households cannot escape debt service even if prices stagnate). If extraction collapses without growth: constraint is temporary entanglement that requires sustained credit growth (tangled rope, not snare). Victim classification and type certainty shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vector_stability, empirical, 'Extraction persistence under growth regime variation').

omega_variable(
    policy_agency_bounds,
    'Can policymakers disable the financialization constraint through regulatory action (loan-to-value caps, leverage limits, transaction taxes) without triggering systemic financial stress?',
    'Comparative policy analysis (countries with strict LTV limits, China''s administrative restrictions, hypothetical stress tests); identification of political feasibility barriers vs structural barriers',
    'If regulable without stress: constraint is tangled rope with policy agency visible (central bank perspective is genuinely constrained but not trapped). If regulation triggers cascading defaults: constraint has snare characteristics for the regime (exit is disabled by financial system dependencies). If regulation is politically impossible: constraint is snare due to capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_agency_bounds, empirical, 'Whether financialization constraint is regulable without systemic consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financialization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(financ_theater_t0, financialization_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(financ_theater_t10, financialization_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(financ_theater_t20, financialization_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(financ_extractiveness_t0, financialization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(financ_extractiveness_t10, financialization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(financ_extractiveness_t20, financialization_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(financ_suppression_t0, financialization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(financ_suppression_t10, financialization_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(financ_suppression_t20, financialization_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(financialization_reading, 0.18).
narrative_ontology:affects_constraint(financialization_reading, naturalist_reading).
narrative_ontology:affects_constraint(financialization_reading, institutional_reading).
narrative_ontology:affects_constraint(financialization_reading, georgist_reading).
narrative_ontology:affects_constraint(financialization_reading, household_debt_accumulation).
narrative_ontology:affects_constraint(financialization_reading, financial_system_stability).

% DUAL FORMULATION NOTE:
% The price_formation_kernel decomposes into four structurally distinct constraint stories representing competing causal theories of housing prices. Financialization_reading attributes price elevation to credit expansion and leverage; it influences (constrains policy options for) institutional_reading and georgist_reading by establishing empirical facts about leverage and credit cycles. The four readings are not averageable or reconcilable — each represents a complete causal chain with its own beneficiary structure and policy implications. Sibling readings: naturalist_reading (scarcity driver), institutional_reading (regulatory capture driver), georgist_reading (land monopoly driver).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financialization_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
