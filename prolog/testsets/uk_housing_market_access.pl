% ============================================================================
% CONSTRAINT STORY: uk_housing_market_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_housing_market_access, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_housing_market_access
 *   human_readable: UK Housing Market Access Constraint
 *   domain: economic/housing/financial
 *
 * SUMMARY:
 *   The UK housing market access constraint creates structural barriers to
 *   property ownership for younger cohorts and low-income households, while
 *   extracting wealth from these populations through elevated rental costs
 *   and speculative capital appreciation benefiting existing property owners
 *   and financial institutions. The constraint has intensified over the
 *   interval (2014-2024) as house price to income ratios have risen from
 *   4.5:1 nationally to 8:1+ in major urban centers, while deposit
 *   requirements remain fixed at 10-20% of purchase price. Suppression has
 *   increased through multiple mechanisms: planning restrictions maintaining
 *   artificial scarcity, building standards increasing construction costs,
 *   credit qualification criteria requiring higher incomes, and speculative
 *   capital from international and domestic investors capturing demand.
 *   Theater ratio has remained low (0.42) because the constraint's extractive
 *   mechanisms operate structurally rather than performatively — property
 *   prices rise due to supply/demand mechanics and capital flows, not ritual
 *   justification. The constraint exhibits all six classification types from
 *   different perspectives, revealing the fundamental structural tension
 *   between housing as shelter (coordination problem requiring stable supply)
 *   and housing as investment asset (extraction opportunity for capital
 *   holders).
 *
 * KEY AGENTS:
 *   - First-Time Buyers (Cohort 25-40): Primary victims (powerless/trapped) — face deposit barriers, income qualification thresholds, and rising prices outpacing wage growth. No viable exit within biographical horizon.
 *   - Young Families: Secondary victims (powerless/trapped) — dependent on Bank of Mum and Dad for deposit capital; intergenerational wealth becomes gating mechanism for housing access.
 *   - Low-Income Renters: Systemic victims (powerless/trapped) — trapped in private rental sector with weak tenure security, rising rents consuming >40% of income, no pathway to ownership.
 *   - Existing Property Owners: Primary beneficiaries (institutional/arbitrage) — benefit from capital appreciation averaging 5-7% annually, capital gains tax exemption on primary residences, rental income streams.
 *   - Financial Institutions (Banks, BTLs, REITs): Institutional beneficiaries (institutional/arbitrage) — extract mortgage interest, arrangement fees, and capital gains from property appreciation; benefit from credit expansion increasing access to leveraged buyers.
 *   - International Capital / Overseas Investors: Secondary beneficiaries (powerful/mobile) — access UK market as UK-pound denomination hedge and capital appreciation play; no local constraint on remittance or exit.
 *   - Planning System / Local Authorities: Institutional actors (institutional/constrained) — maintain green belt and development restrictions; subject to NIMBY political pressure; function degraded to theater protecting existing property values.
 *   - Community Land Trust / Co-housing Movements: Organized alternative providers (organized/constrained) — building parallel pathways with lower extractiveness; constrained by capital availability and regulatory barriers but represent visible sunset mechanism for snare.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_housing_market_access, 0.58).
domain_priors:suppression_score(uk_housing_market_access, 0.68).
domain_priors:theater_ratio(uk_housing_market_access, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_housing_market_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_housing_market_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_housing_market_access, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_housing_market_access, snare).
narrative_ontology:human_readable(uk_housing_market_access, "UK Housing Market Access Constraint").
narrative_ontology:topic_domain(uk_housing_market_access, "economic/housing/financial").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_housing_market_access, existing_property_owners).
narrative_ontology:constraint_beneficiary(uk_housing_market_access, financial_institutions).
narrative_ontology:constraint_beneficiary(uk_housing_market_access, landlord_class).
narrative_ontology:constraint_victim(uk_housing_market_access, first_time_buyers).
narrative_ontology:constraint_victim(uk_housing_market_access, young_families).
narrative_ontology:constraint_victim(uk_housing_market_access, low_income_renters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIRST-TIME BUYER (SNARE) — Trapped by capital requirements, deposit thresholds, and income qualification criteria. No exit mechanism available within biographical horizon. Median house price to median income ratio of 8:1+ in major cities creates structural immobility. Suppression is high: savings accumulation against rising prices is mathematically impossible for most cohorts, and alternative housing routes (social housing, co-ops) are severely constrained. Maximum experienced extraction.
constraint_indexing:constraint_classification(uk_housing_market_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: YOUNG FAMILIES (SNARE) — Trapped across generational horizons. Parental wealth becomes primary determinant of housing access (Bank of Mum and Dad phenomenon). Structural immobility transmits across cohorts. Suppression increases over generations as property as asset class crowds out housing as shelter function.
constraint_indexing:constraint_classification(uk_housing_market_access, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: EXISTING PROPERTY OWNERS (ROPE) — Benefit from capital appreciation and rental arbitrage. Experience constraint as coordination mechanism: stable property values depend on functioning market, regulation ensures contractual enforcement. Low experienced extraction because beneficiaries gain from the mechanism. Arbitrage exit ensures institutional players can reallocate capital.
constraint_indexing:constraint_classification(uk_housing_market_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL CAPITAL (TANGLED ROPE) — Mix of coordination (housing requires credit mechanisms) and extraction (capital gains extraction, overseas buyer demand inflating local prices). Mobile enough to reallocate globally but benefits from UK market inefficiency (strict planning constraints + high demand = price appreciation). Experiences constraint as profit opportunity.
constraint_indexing:constraint_classification(uk_housing_market_access, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMUNITY LAND TRUSTS (SCAFFOLD) — Organized alternative pathways (CLTs, co-housing, shared equity models) bypass traditional mortgage structures. Sunset logic: if CLTs mature and scale, they create alternative access routes that reduce snare extraction. Currently constrained by capital availability and regulatory barriers, but exit mechanism is visible (policy change, funding model innovation).
constraint_indexing:constraint_classification(uk_housing_market_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PLANNING SYSTEM (PITON) — Originated as coordination mechanism for land use and community stability. Now largely performative: planning decisions function as theater protecting existing property values rather than enabling efficient housing allocation. Theater ratio high (extensive public consultation with predetermined outcomes; environmental impact assessments that marginally slow but do not reverse development). Primary function (housing coordination) has atrophied; maintained through institutional inertia and political capture by homeowner groups.
constraint_indexing:constraint_classification(uk_housing_market_access, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, UK housing supply is constrained by immutable factors: land scarcity in desirable areas, geological/infrastructure constraints, climate costs of sprawl. High demand + fixed supply = prices always rise. This perspective naturalizes the constraint as inherent to geography and physics. However, structural data reveals this as false naturalization: housing scarcity is partly contingent policy (planning restrictions, green belt enforcement, building regulations) and partly institutional (capital allocation toward existing assets rather than new supply).
constraint_indexing:constraint_classification(uk_housing_market_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_housing_market_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_housing_market_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_housing_market_access, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_housing_market_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_housing_market_access, TR),
    TR >= 0.70.

:- end_tests(uk_housing_market_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts wealth from trapped cohorts in multiple forms: (1) rental premiums due to ownership barriers, (2) forced savings for deposit accumulation while prices rise, (3) wage suppression in high-cost regions, (4) lost intergenerational wealth accumulation. Extractiveness increased over the interval from 0.32 (2014: post-crisis, recovery phase, lower capital flows) to 0.58 (2024: speculative capital dominance, international interest, price runaway). Suppression (0.68): High. Multiple non-compensable barriers: deposit requirements (20% of purchase price = £40,000-100,000+ for median property), income qualification (mortgage stress tests require 5.5x annual income), planning scarcity (greenfield restrictions, lengthy approvals), credit rationing (banks preferring second-property investors to first-time buyers), behavioral barriers (family dependence normalized, perceived impossibility). Theater ratio (0.42): Moderate-low. The constraint operates through structural price mechanics rather than performative justification. Planning inquiries and environmental assessments provide some theater, but the constraint's power derives from actual supply scarcity and capital flows, not from narrative legitimation. This contrasts with many extractive constraints that require ideological cover — housing extraction occurs transparently through market price signaling.
 *
 * PERSPECTIVAL GAP:
 *   The most revealing gap is between trapped first-time buyers and institutional property owners. The same constraint that appears as an absolute barrier to the former (snare) appears as a functioning market mechanism to the latter (rope). This gap reveals that the constraint's classification is not observer-invariant — it genuinely IS a snare from the trapped cohort's perspective and genuinely IS rope from the beneficiary's perspective. This is not measurement ambiguity but structural reality: the constraint has opposite effects depending on which side of the wealth threshold you occupy. The planning system's piton classification reveals institutional degradation: it performs the theater of public deliberation while delivering predetermined outcomes protecting existing values. The analytical observer's risk of mountain classification reveals the danger of naturalizing contingent institutional arrangements (planning restrictions, building standards, credit allocation rules) as immutable geography.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from power position, exit options, and structural relationship to the extraction flow. First-time buyers (powerless/trapped) have d ≈ 0.95: they are targets with no exit. Experienced property owners (institutional/arbitrage) have d ≈ 0.10: they are beneficiaries with full mobility. Young families (powerless/trapped) have d ≈ 0.90: similar to first-time buyers but slightly elevated due to multi-person household negotiation complexity. Planning system (institutional/constrained) has d ≈ 0.65: subject to political capture and NIMBY pressure; not a free beneficiary but not a victim either. Community land trusts (organized/constrained) have d ≈ 0.45: they are targets of capital constraints but benefit from policy support. International capital (powerful/mobile) has d ≈ 0.25: beneficiaries with complete exit optionality. The disparity in d values across perspectives confirms the snare classification from trapped perspectives — high f(d) values on victims mean maximum experienced extractiveness. Beneficiaries see lower effective extraction because f(d) is dampened for low d.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy collapse through clear structural differentiation. The snare classification (from trapped perspectives) is robust because actual suppression (capital requirements, planning scarcity) is material and non-negotiable within institutional frames. The rope classification (from beneficiary perspectives) is legitimate because beneficiaries genuinely experience coordination benefits (stable values, functioning credit markets). The scaffold classification (from organized alternatives) is accurate because CLTs and co-housing represent real structural exits with visible scaling pathways. The piton classification (from planning system) is precise because the planning apparatus has undergone real functional atrophy — it retains form but lost coordination function. The mountain risk (from analytical observer naturalizing scarcity) is flagged by the structural data: if the constraint were truly immutable natural law, we would not see such radically different classifications from different power positions. The fact that beneficiaries and victims experience it as opposite types is diagnostic evidence that it is policy-contingent rather than law-like. The constraint does not collapse into paradox because the indexical tuple (P, T, E, S) legitimately produces different types — this is perspectival realism, not logical contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    planning_constraint_exogeneity,
    'How much of UK housing supply constraint is exogenous (geography, infrastructure, climate) vs. endogenous (policy choices in planning, building standards, greenfield restrictions)?',
    'Comparative analysis: supply elasticity in high-constraint regions (Southeast, London) vs. low-constraint regions; modeling of counterfactual supply under alternative planning regimes (e.g., relaxed greenfield restrictions, reduced building standards, faster permitting)',
    'If >70% exogenous: mountain classification partially justified; snare extraction is partially coordination cost. If >70% endogenous: mountain is false summit; constraint is policy-contingent snare with alternative exit (deregulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planning_constraint_exogeneity, empirical, 'Exogenous vs endogenous determinants of housing supply constraint').

omega_variable(
    speculative_capital_necessity,
    'Is capital investment in UK housing primarily speculative (seeking price appreciation) or functional (housing capital formation), and what would be the supply/demand equilibrium if speculative component were removed?',
    'Decomposition of buyer cohorts: owner-occupants vs investors vs overseas capital; modeling of price elasticity and supply response under different speculative tax regimes (land value tax, vacant property tax, foreign buyer restrictions); international comparison with lower-speculation markets',
    'If speculative >40% of demand: extraction mechanism is capital hoarding; tangible exit exists (taxing speculation). If speculative <20%: constraint is primarily coordination problem (mismatch of supply to demographics); exit requires production not capital control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_capital_necessity, empirical, 'Speculative vs functional capital in housing demand').

omega_variable(
    intergenerational_wealth_transfer_lock,
    'To what degree does Bank of Mum and Dad capital determine access, and would primary access mechanisms shift if intergenerational wealth transfer were constrained (via inheritance tax, lifetime transfer tax)?',
    'Cohort analysis of first-time buyer characteristics; correlation between parental wealth and buyer age; modeling of access patterns under alternative inheritance regimes; comparison with jurisdictions having higher intergenerational transfer taxes',
    'If BOMAD >60% of first-time buyers: wealth lock is primary extraction mechanism; alternate mechanism (income-based) would reconfigure who is trapped vs mobile. If BOMAD <30%: other factors dominate; wealth transfer is secondary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_wealth_transfer_lock, empirical, 'Intergenerational wealth transfer as primary access determinant').

omega_variable(
    clt_scalability_ceiling,
    'What is the structural scalability ceiling for Community Land Trust and co-housing models given current capital constraints and regulatory barriers? Would scaling to 10%+ of market materially reduce snare extraction?',
    'Modeling of CLT capital requirements and sources; comparison of CLT housing costs vs traditional market costs; analysis of regulatory barriers to rapid scaling; case studies of jurisdictions with higher CLT penetration (Denmark, Germany, Switzerland)',
    'If scalable to >20% of market: scaffold sunset logic is credible; snare is not inherent but contingent. If ceiling <5%: alternative models remain marginal; snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clt_scalability_ceiling, empirical, 'Scalability of alternative housing models as snare exit path').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_housing_market_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukhousing_tr_t0, uk_housing_market_access, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ukhousing_tr_t5, uk_housing_market_access, theater_ratio, 5, 0.33).
narrative_ontology:measurement(ukhousing_tr_t10, uk_housing_market_access, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(ukhousing_be_t0, uk_housing_market_access, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ukhousing_be_t5, uk_housing_market_access, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ukhousing_be_t10, uk_housing_market_access, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_housing_market_access, resource_allocation).
narrative_ontology:affects_constraint(uk_housing_market_access, uk_intergenerational_wealth_transfer).
narrative_ontology:affects_constraint(uk_housing_market_access, planning_restriction_supply_ceiling).
narrative_ontology:affects_constraint(uk_housing_market_access, mortgage_credit_rationing).

% DUAL FORMULATION NOTE:
% UK housing access decomposes into three structurally distinct constraints: (1) planning-driven supply scarcity (ε≈0.35, primarily policy-contingent), (2) credit rationing and income qualification barriers (ε≈0.42, primarily institutional), (3) intergenerational wealth transfer via BOMAD (ε≈0.48, primarily social reproduction). Each has different ε values reflecting different measurement bases. The aggregate constraint story combines all three; separate stories should isolate policy, credit, and wealth mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_housing_market_access, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
