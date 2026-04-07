% ============================================================================
% CONSTRAINT STORY: carbon_credit_markets_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carbon_credit_markets_2026, []).

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
 *   constraint_id: carbon_credit_markets_2026
 *   human_readable: International Carbon Credit Trading Schemes (2026)
 *   domain: economic/political
 *
 * SUMMARY:
 *   International carbon credit trading schemes represent a market-based
 *   approach to climate mitigation that distributes climate responsibility
 *   through tradeable permits and offset mechanisms. The constraint exhibits
 *   a fundamental tension between its coordination function (enabling global
 *   mitigation at lower cost) and its extraction mechanism (allowing wealthy
 *   nations and financial actors to externalize emissions reduction costs to
 *   poorer nations and vulnerable communities). From 2015 onwards, carbon
 *   markets have expanded significantly, with the EU ETS, Article 6 of the
 *   Paris Agreement, and corporate voluntary carbon markets creating a
 *   multi-billion-dollar ecosystem. However, empirical analysis reveals
 *   persistent problems: additionality failures (offset projects that would
 *   have happened anyway), permanence risks (climate impacts reversing offset
 *   gains), leakage (emissions displaced to unregulated jurisdictions), and
 *   gross benefit asymmetry (financial intermediaries capturing 60-80% of
 *   credit value while indigenous communities receive 5-10%). The
 *   constraint's theater ratio has increased as compliance verification has
 *   become more complex and difficult to audit, creating space for accounting
 *   games and credit quality degradation. The 2026 moment is critical: carbon
 *   markets are supposed to transition from temporary coordination mechanism
 *   to permanent baseline as renewable energy costs fall, but empirical
 *   evidence suggests the mechanism is degrading toward pure financial
 *   redistribution. This story models carbon markets as tangled_rope from the
 *   analytical perspective (coordination function genuine, but asymmetric
 *   extraction severe), while from victim and beneficiary perspectives the
 *   classification diverges toward snare and rope respectively.
 *
 * KEY AGENTS:
 *   - Indigenous Land Holders and Global South Communities: Primary victims (powerless/trapped) — dispossessed of land control; forest commons treated as extraction sites; minimal benefit from carbon credit revenue despite bearing ecological and livelihood risk
 *   - Developing Nation Policy Makers: Secondary victims (moderate/constrained) — depend on climate finance but have limited negotiating power; forced to choose between expensive domestic reductions or low-value offset exports
 *   - Financial Intermediaries and Carbon Brokers: Primary beneficiaries (institutional/arbitrage) — capture spread on credit transactions; monetize compliance premium; can arbitrage between regulated and voluntary markets
 *   - High-Emission Industrialized Nations: Secondary beneficiaries (powerful/mobile) — can meet climate targets cheaply by purchasing offsets rather than transitioning energy systems; maintain high consumption through carbon market access
 *   - Climate-Committed Corporations and NGOs: Organized reform agents (organized/constrained) — work to build integrity mechanisms; see carbon markets as temporary transition tool with sunset toward emissions reduction
 *   - Traditional Carbon Pricing Regulators: Institutional maintainers (institutional/arbitrage) — maintain performative compliance apparatus (EU ETS, CDM mechanisms); see own regulatory function as degraded but persist through inertia
 *   - Analytical Observer (Atmospheric Physics): Sees irreducible collective action problem requiring global coordination mechanism; risks naturalizing contingent market design as inherent to climate physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carbon_credit_markets_2026, 0.52).
domain_priors:suppression_score(carbon_credit_markets_2026, 0.58).
domain_priors:theater_ratio(carbon_credit_markets_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carbon_credit_markets_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(carbon_credit_markets_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(carbon_credit_markets_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carbon_credit_markets_2026, tangled_rope).
narrative_ontology:human_readable(carbon_credit_markets_2026, "International Carbon Credit Trading Schemes (2026)").
narrative_ontology:topic_domain(carbon_credit_markets_2026, "economic/political").

domain_priors:requires_active_enforcement(carbon_credit_markets_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carbon_credit_markets_2026, high_emission_industrialized_nations).
narrative_ontology:constraint_beneficiary(carbon_credit_markets_2026, financial_intermediaries).
narrative_ontology:constraint_beneficiary(carbon_credit_markets_2026, offset_project_developers).
narrative_ontology:constraint_victim(carbon_credit_markets_2026, climate_mitigation_effectiveness).
narrative_ontology:constraint_victim(carbon_credit_markets_2026, global_south_communities).
narrative_ontology:constraint_victim(carbon_credit_markets_2026, indigenous_land_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS LAND HOLDERS (SNARE) — Trapped by land dispossession and carbon accounting schemes that treat their forests as extraction sites without consent or benefit-sharing. Suppression is high: lack of legal recourse, inability to exit carbon markets, and dependence on land-based livelihoods. Extraction is severe: carbon credit value flows to external developers while communities bear ecological and livelihood risks.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION POLICY MAKERS (TANGLED ROPE) — Constrained exit: countries depend on carbon finance to fund adaptation and renewable energy but have limited negotiating power. Coordination function exists (climate mitigation requires global participation), but extraction is asymmetric: wealthy nations can meet targets cheaply by buying credits rather than transitioning; developing nations must undertake costly domestic reductions or export low-value offsets. Active enforcement through climate agreements creates both coordination benefit and extraction mechanism.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCIAL INTERMEDIARIES (ROPE) — Institutional actors with arbitrage options (can move capital between markets, jurisdictions, and assets). Experience the constraint as pure coordination: carbon credits are a standardized asset class that solves the collective action problem of pricing emissions globally. Net beneficiary through spread capture, compliance premium monetization, and portfolio diversification. Suppression is minimal for this actor; extraction runs toward them, not away.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE-COMMITTED CORPORATIONS AND NGOs (SCAFFOLD) — Organized actors working to build integrity mechanisms: Article 6 transparency rules, independent verification standards, community benefit-sharing requirements. Sees carbon markets as temporary coordination tool with sunset: as renewable energy costs fall and carbon pricing rises, reliance on offsets diminishes. Theater ratio remains high (compliance theater), but sunset clause is structural — carbon markets are explicitly designed as transition mechanism toward net-zero through emissions reduction, not permanent offset infrastructure.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL REGULATORY CARBON PRICING (PITON) — Legacy carbon trading (EU ETS, Kyoto Protocol mechanisms) persists through institutional inertia despite known leakage, additionality failures, and fraud. The regulatory apparatus maintains theater: compliance paperwork, credit verification, offset audits — but effectiveness in reducing emissions has degraded as financial sophistication has outpaced oversight capacity. Theater ratio (0.68) reflects that much carbon credit activity is performative accounting rather than real mitigation.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-EMISSION INDUSTRIALIZED NATIONS (TANGLED ROPE) — Powerful actors with mobile exit options (can invest in domestic renewables, carbon capture, or other compliance pathways). Coordination function: carbon markets enable global mitigation targets and reduce compliance costs. But extraction is asymmetric: wealthy nations can maintain high consumption by purchasing cheap offsets rather than undertaking costly domestic reductions. This perspective experiences the constraint as coordination benefit plus structural advantage — the market does solve collective action, but the solution is asymmetric.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ATMOSPHERIC PHYSICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, atmospheric carbon is a global commons problem: any nation's emissions warm the entire planet, and no individual actor can solve climate change alone. This perspective sees carbon markets as a natural law response to an irreducible physics constraint. However, the structural data contradicts this — carbon markets are institutional designs with clear beneficiaries and victims, not immutable natural laws. This is a false summit: naturalizing a contingent market mechanism as inherent to climate physics.
constraint_indexing:constraint_classification(carbon_credit_markets_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carbon_credit_markets_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carbon_credit_markets_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carbon_credit_markets_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carbon_credit_markets_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carbon_credit_markets_2026, TR),
    TR >= 0.70.

:- end_tests(carbon_credit_markets_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Carbon markets do solve the legitimate coordination problem of pricing emissions globally and enabling lower-cost mitigation pathways. But the extraction is real: wealthy nations capture ~70% of mitigation benefit by purchasing cheap offsets while avoiding domestic transitions; financial intermediaries capture 60-80% of credit value; indigenous communities and global south actors experience net extraction. The value increased from 0.35 (2015, early implementation) to 0.52 (2026) as financial sophistication outpaced oversight and additionality/permanence failures became apparent. Suppression (0.58): High. Multiple barriers to exit and resistance exist: (a) legal barriers — indigenous communities lack title to carbon in their forests under most frameworks, limiting their ability to negotiate; (b) structural barriers — developing nations are locked into offset export dependency once they adopt carbon-market-dependent climate finance; (c) information barriers — verification of offset quality is technically complex and dominated by financial actors and developed-nation consultants; (d) coercive barriers — climate finance conditionality forces developing nations into market participation. Theater ratio (0.68): High and increasing. Compliance verification has become increasingly performative as projects have scaled: auditing additionality and permanence for 10,000+ projects globally is infeasible; many offset credits trade as financial assets divorced from physical carbon accounting; corporate net-zero commitments often rely on purchased offsets rather than real emissions reductions. Theater increased from 0.55 (early period) to 0.68 as financial mechanisms and accounting complexity became dominant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. Indigenous land holders trapped in offset zones see pure snare: they cannot exit, face high suppression, and experience severe extraction with no coordination benefit. Developing nation policy makers see tangled_rope: coordination function is real (they need global climate finance) but extraction is asymmetric (they do costly mitigation while wealthy nations buy cheap credits). Financial intermediaries see pure rope: the market solves coordination elegantly; extraction runs toward them; suppression is minimal. Wealthy industrialized nations see tangled_rope but from the beneficiary side: coordination works (global emissions pricing) and they benefit (lower compliance costs). Climate-committed actors see scaffold: the markets are temporary tool with sunset toward real emissions reduction. Traditional regulators see piton: their own compliance machinery is performative and degraded but persists through institutional inertia. The analytical observer risks seeing mountain (irreducible global commons problem requiring tradeable permits) but the structural data reveals this as false summit — carbon markets are institutional designs, not laws of physics. The perspectival gap between the indigenous community (snare) and the financial intermediary (rope) using the same carbon credit reveals the constraint's true structure: the architecture creates winners and losers, not merely coordinates a neutral global action.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position relative to extraction flow. Indigenous land holders have d ≈ 0.95 (full target, no exit, no benefits) → high f(d) → high experienced extraction. Developing nation policy makers have d ≈ 0.68 (net target, but with some coordination benefit and partial agency) → medium f(d) → moderate extraction. Financial intermediaries have d ≈ 0.08 (full beneficiaries with arbitrage exit) → negative f(d) → negative/negligible extraction. High-emission industrialized nations have d ≈ 0.40 (beneficiaries, but with some cost to maintain market legitimacy and some domestic pressure for real reductions) → f(d) ≈ 0.4 → low-moderate extraction. Climate-committed actors have d ≈ 0.55 (symmetric, but with exit via alternative mitigation pathways and organized power) → f(d) ≈ 0.75 → moderate extraction. The engine derives d from these structural facts (beneficiary/victim status + exit options + power level) automatically; the directionality logic notes that beneficiaries with arbitrage exit experience minimal effective extraction while victims with trapped exit experience maximum extraction, even from the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY TENSION: The constraint is classified as tangled_rope from the analytical perspective (has both coordination function and asymmetric extraction), but mandatrophy_resolved is false because the question of 'is this mechanism primarily coordinating climate action or primarily redistributing wealth?' remains unresolved. If carbon offsets achieve genuine additionality and permanence at scale, the tangled_rope classification holds — markets coordinate global mitigation while enabling some wealthy-nation advantage. If additionality is systematic fiction and permanence is illusory under climate change, the classification degrades toward pure snare or piton (performance mechanism without real mitigation). The four omegas identify the key empirical tests: (1) additionality verification determines whether credits represent real mitigation or financial accounting, (2) permanence assessment determines whether offsets are durable or illusory, (3) leakage analysis determines whether emissions are actually reduced or displaced, (4) benefit distribution reveals the asymmetry magnitude. Resolution of these omegas would determine mandatrophy: if all four resolve toward 'extraction/theater,' the mechanism is a snare; if toward 'real coordination with moderate asymmetry,' the tangled_rope holds; if toward 'temporary mechanism being phased out,' the scaffold classification becomes dominant. The theater ratio (0.68) and increasing trajectory indicate the mechanism is drifting toward piton (degraded, performative) rather than toward rope (genuine coordination) or scaffold (deliberate transition tool). This story is unresolved mandatrophy because the fundamental question — whether carbon markets are a legitimate market design for climate coordination or an elaborate extraction mechanism — depends on empirical facts still being determined in 2026.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    additionality_verification_problem,
    'What baseline conditions actually define ''additionality'' — would the offset project have occurred anyway without carbon finance?',
    'Longitudinal tracking of project initiation timing relative to carbon credit revenues; comparison of baseline assumptions across projects in same sector/region; analysis of project failure rates post-credit earning',
    'If additionality is systematically unverifiable: carbon credits represent financial redistribution without emissions reduction (χ increases toward pure snare). If additionality can be reliably established: coordination function is real and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(additionality_verification_problem, empirical, 'Whether carbon offset projects are truly additional vs. would-have-happened-anyway').

omega_variable(
    permanence_and_reversal_risk,
    'Can forests and soil carbon offset project commitments hold permanently, or do climate impacts (wildfire, drought, pest outbreaks) inevitably reverse offset gains?',
    'Monitoring of reforestation and soil carbon projects over 20+ year timescales; assessment of climate change impact on project success rates; analysis of insurance and permanence guarantee mechanisms',
    'If permanence < 50 years: offset credits represent short-term accounting fraud (effective χ becomes snare-like). If permanence > 80 years: coordination function is more credible and tangled_rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanence_and_reversal_risk, empirical, 'Whether carbon offset permanence is achievable under climate change').

omega_variable(
    leakage_and_displacement,
    'Do carbon offset projects simply displace emissions to unregulated jurisdictions, creating zero net mitigation?',
    'Cross-border emissions tracking; analysis of commodity production shifts following offset project initiation; modeling of indirect economic effects (demand shift, price changes, supply-chain relocation)',
    'If leakage > 60%: effective emissions reduction is minimal and markets are primarily financial redistribution (pure extraction). If leakage < 20%: carbon markets achieve meaningful mitigation and coordination function is substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leakage_and_displacement, empirical, 'Whether offset projects displace emissions rather than reduce them').

omega_variable(
    benefit_distribution_across_contexts,
    'What proportion of carbon credit value reaches frontline communities vs. financial intermediaries and developed-nation corporations?',
    'Price tracking through full carbon credit supply chain (developer to final buyer); benefit-sharing analysis across project types; audit of payment flows to indigenous and local communities',
    'If developer share < 20%: extraction asymmetry is extreme and snare classification dominates (powerless victims see χ >> institutional beneficiaries). If developer share > 50%: tangled_rope asymmetry is moderate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(benefit_distribution_across_contexts, empirical, 'How carbon credit value distributes across market participants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carbon_credit_markets_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carbon_tr_t0, carbon_credit_markets_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(carbon_tr_t3, carbon_credit_markets_2026, theater_ratio, 3, 0.62).
narrative_ontology:measurement(carbon_tr_t6, carbon_credit_markets_2026, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(carbon_be_t0, carbon_credit_markets_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carbon_be_t3, carbon_credit_markets_2026, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(carbon_be_t6, carbon_credit_markets_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carbon_credit_markets_2026, resource_allocation).
narrative_ontology:affects_constraint(carbon_credit_markets_2026, renewable_energy_deployment_incentives).
narrative_ontology:affects_constraint(carbon_credit_markets_2026, global_south_adaptation_financing).
narrative_ontology:affects_constraint(carbon_credit_markets_2026, indigenous_land_rights_enforcement).

% DUAL FORMULATION NOTE:
% Carbon credit markets decompose into distinct structural constraints: (1) the coordination problem of global emissions pricing (rope-like), (2) the extraction mechanism of permit allocation to wealthy nations (snare-like), (3) the offset verification infrastructure (piton-like, performative). This story treats the integrated market mechanism; upstream constraints address the specific empirical questions (additionality, permanence, leakage) that determine which component dominates. Downstream constraints address how carbon markets interact with renewable energy deployment and indigenous land rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carbon_credit_markets_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
