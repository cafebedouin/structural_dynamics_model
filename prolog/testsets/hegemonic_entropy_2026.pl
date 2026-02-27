% ============================================================================
% CONSTRAINT STORY: hegemonic_entropy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hegemonic_entropy_2026, []).

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
 *   constraint_id: hegemonic_entropy_2026
 *   human_readable: The Asymmetry of Hegemonic Decay
 *   domain: economic/political
 *
 * SUMMARY:
 *   The post-WWII international order was designed as a coordination
 *   mechanism: fixed exchange rates (later floating), capital mobility
 *   controls, and trade agreements enabled economic recovery and growth.
 *   However, the structural asymmetry embedded in reserve currency issuance —
 *   the ability of the hegemon to export inflation and debt to periphery —
 *   has gradually transformed the system from coordination into asymmetric
 *   extraction. Cantillon effects (the wealth creation that accrues to those
 *   nearest to credit expansion) and tax haven networks (profit shifting by
 *   multinationals and capital flight by elites) have decoupled the order's
 *   nominal function (coordination) from its actual effect (extraction). The
 *   hegemonic system persists through institutional inertia (Bretton Woods
 *   bodies, dollar settlement rails, military enforcement of capital flow
 *   regimes) despite increasing delegitimacy, making this a degraded Piton in
 *   some perspectives while remaining an extractive Snare in others. The
 *   constraint's theater_ratio (0.64) reflects that development narratives,
 *   financial inclusion rhetoric, and sustainability goals mask the core
 *   extraction mechanism — Cantillon effects and capital mobility that
 *   concentrate wealth at the core regardless of policy framing.
 *
 * KEY AGENTS:
 *   - Financial Centers (New York, London, Singapore): Primary beneficiary (institutional/arbitrage) — control credit creation, payment rails, and asset pricing. Capture spread on currency intermediation and benefit first from monetary expansion.
 *   - Multinational Corporations: Secondary beneficiary (institutional/arbitrage) — access to global supply chains, tax havens via transfer pricing, regulatory arbitrage, intellectual property enforcement through trade law.
 *   - Reserve Currency Issuers (US Treasury, Federal Reserve): Hegemon (institutional/arbitrage) — exogenous control of monetary policy, ability to run persistent deficits, seigniorage capture, geopolitical leverage via financial coercion.
 *   - Peripheral Economies (emerging markets, developing nations): Primary victim (powerless/trapped) — debt denominated in foreign currency, capital flight, Cantillon effect erosion of purchasing power, forced fiscal adjustment via IMF conditions.
 *   - Labor Classes (core and periphery): Secondary victim (powerless/trapped) — wage suppression through capital arbitrage and financialization, extraction of economic surplus via asset inflation (housing, healthcare, education), geographic immobility.
 *   - Middle-Income States: Mixed (moderate/constrained) — benefit from trade access and FDI but constrained by capital mobility and profit-shifting pressures. Some agency through industrial policy or regional blocs.
 *   - Bretton Woods Institutions (IMF, World Bank, UN): Institutional actors (institutional/constrained) — nominally coordinating bodies that have degraded into vehicles for hegemonic enforcement. Constrained by dependence on reserve currency and lack of alternative mandate.
 *   - Labor Coalitions (unions, social democracies, progressive governments): Organized resistance (organized/constrained) — possess enforcement power through strike and regulation but face structural constraints from capital mobility. See sunset potential through coordinated policy reform.
 *   - Analytical Observer: Universal vantage (analytical/analytical) — risks naturalizing reserve currency asymmetry as inevitable structural feature rather than contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hegemonic_entropy_2026, 0.58).
domain_priors:suppression_score(hegemonic_entropy_2026, 0.67).
domain_priors:theater_ratio(hegemonic_entropy_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hegemonic_entropy_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hegemonic_entropy_2026, tangled_rope).
narrative_ontology:human_readable(hegemonic_entropy_2026, "The Asymmetry of Hegemonic Decay").
narrative_ontology:topic_domain(hegemonic_entropy_2026, "economic/political").

domain_priors:requires_active_enforcement(hegemonic_entropy_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, financial_centers).
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, multinational_corporations).
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, reserve_currency_issuers).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, peripheral_economies).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, labor_classes).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, fiscal_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Emerging market or developing nation locked into debt obligations denominated in reserve currency, unable to exit capital controls or regional banking dependencies. Bears maximum extraction through Cantillon effects — currency creation benefits those nearest the money tap (financial centers) and inflates costs for peripheral producers. No meaningful exit: dollarization of debt, capital flight, or IMF structural adjustment all enforce the same extraction vector.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Workers in both developed and developing economies face suppressed wage growth despite productivity gains. Trapped by: (1) geographic immobility of jobs, (2) capital's arbitrage ability to seek lower-wage jurisdictions, (3) financialization that extracts rent from necessities (housing, healthcare, education). No exit without massive coordination. Maximum experienced extraction.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Middle-income country experiences both coordination benefits (access to global supply chains, trade agreements, capital markets) and asymmetric extraction (capital flight, profit shifting via transfer pricing, currency speculation). Constrained by dependence on foreign direct investment and export markets, but possesses some agency through industrial policy or regional bloc membership. Significant experienced extraction but not total.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Primary beneficiary. Experiences the global order as pure coordination: capital flows, currency exchanges, and financial regulation enable unimpeded arbitrage and asset pricing. Net benefit flows toward financial centers through Cantillon effects — they issue credit, control payment rails, and capture spread. Can arbitrage jurisdictions, exit unfavorable regulations, migrate capital instantly. Minimum experienced extraction.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Secondary beneficiary. Benefits from: (1) transfer pricing and tax havens (base erosion and profit shifting), (2) regulatory arbitrage between jurisdictions, (3) labor cost arbitrage across borders, (4) intellectual property enforcement via trade agreements. Experiences the constraint as coordination infrastructure that enables global supply chains. Arbitrage exits available across all dimensions.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized groups (unions, social democracies, regulatory bodies) see the global order as a mixed system: welfare state coordination (wage floors, healthcare, public goods) is being undermined by capital mobility and financialization. They have some enforcement power (strike, regulation, cross-border coordination) but face structural constraints from capital's superior mobility. Significant suppression of their preferred policies, but active resistance creates visible friction and generates organizational power through coalition. This perspective sees the sunset: if labor coordination achieves sufficient international organizing (ILO enforcement, capital controls, progressive taxation treaties), the extraction mechanism could be curbed.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% IMF, World Bank, and UN multilateral bodies were designed as coordination mechanisms (post-WWII cooperative order) but have degraded into vehicles for hegemonic enforcement. Theater_ratio high: development programs, structural adjustment, and capacity-building narratives mask capital extraction and fiscal dominance. These institutions persist through inertia and lack of alternatives, not because they function as intended. Constrained by inability to challenge the reserve currency regime upon which they depend.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational vantage, the asymmetry could be naturalized as an immutable feature of any currency-based international system: there must be a numeraire, and the issuer of the numeraire holds structural advantage. This perspective risks seeing hegemonic decay as a law of economics. However, the structural data contradicts the mountain classification: the Bretton Woods institutions were deliberately constructed, can be reformed or replaced, and the Cantillon extraction is not inherent to currency systems themselves but to specific policy choices (unregulated capital flows, tax havens, currency monopoly enforcement via military). The analytical observer should detect this as a false summit — naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hegemonic_entropy_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hegemonic_entropy_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hegemonic_entropy_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hegemonic_entropy_2026, TR),
    TR >= 0.70.

:- end_tests(hegemonic_entropy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.58): High-moderate. The global order extracts significant surplus from peripheral economies and labor through Cantillon effects, tax havens, currency speculation, and capital mobility. However, it is not maximal (0.70+) because: (1) some coordination benefits persist (trade, FDI, technology transfer), (2) extraction is mediated through market mechanisms rather than outright coercion, and (3) peripheral actors retain some agency through industrial policy, regional blocs, and labor organizing. The value of 0.58 reflects that the system is primarily extractive but retains mixed properties. Suppression (0.67): High. Barriers to exit include: (1) currency regime lock-in (dollar debt obligations), (2) capital controls and financial sanctions, (3) geographic/skill immobility of labor, (4) military backing of hegemonic financial order, (5) absence of viable alternative systems. But suppression is not total — peripheral states can pursue import substitution, labor can strike, coalitions can form. Theater Ratio (0.64): Moderate-high. The system generates significant performative activity — development rhetoric, financial inclusion narratives, sustainability commitments, institutional capacity-building — that masks the core extraction mechanism. Bretton Woods institutions maintain theater around development goals while enforcing fiscal discipline. Financial markets maintain theater around efficient pricing while conducting high-frequency extraction. The theater has increased over the interval as the order's legitimacy has eroded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates substantial perspectival divergence from the same structural base. The financial center perceives Rope — pure coordination that enables arbitrage and asset pricing. The multinational perceives Rope — the system facilitates global supply chains and profit optimization. The peripheral economy perceives Snare — no exit from debt, currency dependence, or capital extraction. Labor perceives Snare — capital mobility prevents organizing, wage suppression is structural. The middle-income state perceives Tangled Rope — mixed benefit from trade access and mixed cost from profit-shifting and capital controls. The labor coalition perceives Tangled Rope with sunset — active enforcement (strikes, regulations) generates friction and political organization; multi-polar reform could shift the order. The Bretton Woods institutions perceive Piton — their core functions (coordination) have atrophied while performative functions (development theater) have intensified. The analytical observer risks perceiving Mountain — reserve currency hegemony as inherent to any monetary order — but structural analysis reveals this as false naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by structural position within the extraction flow. Financial centers and multinationals occupy the proximate position to money creation and regulatory arbitrage — they are beneficiaries (low d, negative χ from their perspective). Peripheral economies and labor classes are targets of the extraction — they face high d values (0.80+) producing high χ. The middle-income state experiences mixed extraction and coordination (d ≈ 0.55, moderate χ). The Bretton Woods institutions are constrained agents nominally enforcing the order (d ≈ 0.60, constrained exit). The labor coalition has organized agency but limited exit options (d ≈ 0.65, constrained/analytical). The analytical observer sees the system from a civilizational vantage and risks collapsing hegemonic structure into natural law (false summit detection triggered by examining whether the system is designed or inherited).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids classification collapse by recognizing that hegemonic entropy is fundamentally about extraction at high institutional entropy (theater_ratio 0.64 indicates degraded function). The tangled_rope classification captures the core paradox: the system originated as genuine coordination (post-WWII recovery, trade boom), has been progressively parasitized by extraction mechanisms (Cantillon effects, tax havens, capital controls favoring the center), and persists through institutional inertia and the absence of viable alternatives. The Piton perspective correctly identifies that the Bretton Woods bodies maintain performative function (development narrative) while their real function (global coordination) has degraded. The Snare perspectives correctly identify that trappers (peripheral economies, labor) have no exit. The Rope perspectives correctly identify that beneficiaries (financial centers, multinationals) experience genuine coordination benefits. The constraint avoids collapse into false Mountain by recognizing that the hegemon itself is an institutional choice, not a law of nature — alternatives exist (commodity-backed systems, multi-polar currency regimes, capital controls) and have historical precedent. The system persists because the extraction is valuable to those conducting it and because organized alternatives lack critical mass, not because the system is inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_currency_exit_cost,
    'What economic and political cost would a coordinated exit from dollar hegemony impose on the exiting coalition, and is that cost lower than the extraction they currently bear?',
    'Quantitative: estimate transition cost (currency volatility, trade disruption, capital flight) vs. annual extraction via Cantillon effects and financial dominance. Qualitative: historical comparison to prior currency regime shifts (post-WWI pound decline, Bretton Woods collapse).',
    'If exit cost < extraction: coalition coalition formation becomes rational and snare perspective shifts to scaffold (sunset via multi-polar currency system). If exit cost > extraction: peripheral economies remain trapped; snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_currency_exit_cost, empirical, 'Cost-benefit threshold for coordinated exit from dollar hegemony').

omega_variable(
    cantillon_effect_magnitude,
    'How much of observed income inequality within developed economies is attributable to Cantillon effects (proximity to money creation) versus skill-biased technological change, globalization, or other factors?',
    'Decomposition of inequality trends using: (1) wealth distribution of financial sector workers vs. real economy workers, (2) correlation between credit expansion cycles and asset price inflation, (3) cross-country comparison of inequality in economies with different monetary policy regimes.',
    'If Cantillon effects explain > 40% of inequality: extraction mechanism confirmed as primary driver. If < 20%: constraint classification must shift toward Rope (coordination with incidental inequality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cantillon_effect_magnitude, empirical, 'Quantification of Cantillon effects on income distribution').

omega_variable(
    tax_haven_functional_necessity,
    'Are tax havens and transfer pricing mechanisms functionally necessary for efficient global capital allocation, or are they purely extractive rent-seeking?',
    'Comparison of capital efficiency and innovation rates in: (1) high-openness, low-tax-haven regimes vs. (2) closed, haven-dependent regimes; historical analysis of FDI patterns pre/post tax-haven proliferation.',
    'If functional: constraint is Rope with incidental extraction (reclassify toward lower extractiveness). If purely extractive: Snare/Tangled Rope classification confirmed; extraction channel should be disrupted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_haven_functional_necessity, empirical, 'Whether tax havens provide coordination or pure extraction').

omega_variable(
    labor_coalition_critical_mass,
    'What fraction of global labor force must coordinate (cross-border wages, capital controls, progressive taxation) before they achieve sufficient countervailing power to shift the extraction equilibrium?',
    'Network analysis of labor-organizing capacity; historical case studies of successful cross-border labor coordination (Scandinavian model, EU labor standards); simulation of coalition defection costs.',
    'If critical mass < 30%: scaffold perspective is realistic — labor coalition can reshape the order. If > 60%: coordination barriers are insurmountable; snare perspective locked in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_coalition_critical_mass, empirical, 'Threshold for labor coalition countervailing power').

omega_variable(
    alternative_hegemonic_extractiveness,
    'If the current dollar-hegemonic order were replaced by a multi-polar or commodity-backed system, would total extractiveness decrease or merely shift to a different beneficiary (e.g., energy exporters, China)?',
    'Counterfactual analysis using historical transitions (Bretton Woods to floating rates, oil standard, prior empires'' decline). Network analysis of alternative bases for hegemony (energy, digital, manufacturing).',
    'If extractiveness persists under alternatives: constraint is about hegemony itself, not specific institutions (reshape as civilization-level snare). If extractiveness decreases: reform via multi-polarity is genuine exit path (scaffold confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_hegemonic_extractiveness, conceptual, 'Whether alternative hegemonic systems would reduce total extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hegemonic_entropy_2026, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heg_tr_t0, hegemonic_entropy_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(heg_tr_t20, hegemonic_entropy_2026, theater_ratio, 20, 0.51).
narrative_ontology:measurement(heg_tr_t40, hegemonic_entropy_2026, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(heg_be_t0, hegemonic_entropy_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(heg_be_t20, hegemonic_entropy_2026, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(heg_be_t40, hegemonic_entropy_2026, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hegemonic_entropy_2026, global_infrastructure).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, cantillon_effect_distribution).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, tax_haven_capital_flight).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, currency_regime_lock_in).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, labor_arbitrage_suppression).

% DUAL FORMULATION NOTE:
% The hegemonic entropy constraint is the civilizational-level parent structure encompassing multiple derivative mechanisms: Cantillon effects (monetary policy extraction), tax havens (profit-shifting extraction), currency lock-in (debt obligation extraction), and labor arbitrage (wage suppression extraction). Each downstream constraint has distinct ε but shared beneficiary (financial centers) and victim (periphery) structure. The network links establish causality: hegemonic order enables each extraction mechanism. Upstream: post-WWII institutional design (Bretton Woods conjecture). Downstream: specific mechanisms through which extraction flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hegemonic_entropy_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
