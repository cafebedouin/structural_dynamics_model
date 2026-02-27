% ============================================================================
% CONSTRAINT STORY: hegemonic_entropy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The global hegemonic order established after World War II functioned
 *   initially as a genuine coordination mechanism: the Bretton Woods system,
 *   the postwar trade regime, and US currency centrality solved collective
 *   action problems (reduced transaction costs, price stability, enforceable
 *   contracts across borders). Over the past 35 years, this coordination
 *   mechanism has been progressively overlaid with asymmetric extraction
 *   mechanisms: Cantillon effects (monetary expansion benefits capital
 *   holders first, wage earners last), tax arbitrage (capital flight to
 *   minimally-taxed jurisdictions undermines fiscal capacity of non-haven
 *   states), and the real bills doctrine (central bank support for financial
 *   assets over productive investment). The constraint now exhibits all the
 *   characteristics of a Tangled Rope at the systemic level — genuine
 *   coordination function persists (global trade, contract law, currency
 *   stability) alongside severe asymmetric extraction (wealth concentration,
 *   periphery debt traps, wage stagnation despite productivity growth). The
 *   theater ratio has risen from 0.35 (Bretton Woods institutions genuinely
 *   enforced rules) to 0.64 (IMF conditionality and World Bank structural
 *   adjustment are increasingly performative as real power migrates to
 *   unilateral monetary policy and tax competition). The extractiveness has
 *   tripled from 0.28 to 0.58, reflecting the layering of rent-extraction
 *   mechanisms onto coordination infrastructure. This is the defining
 *   constraint of global political economy in 2026: the hegemon maintains the
 *   coordination fiction while capturing extraction rents, peripheral
 *   economies are trapped in the regime, and organized reform coalitions are
 *   testing alternative pathways (BRICS de-dollarization, OECD minimum tax
 *   agreements, debt relief).
 *
 * KEY AGENTS:
 *   - Currency Hegemon (US): Institutional/arbitrage — primary beneficiary via seigniorage and Cantillon priority. Experiences regime as coordination.
 *   - Tax Haven Operators (Cayman Islands, Singapore, Luxembourg, etc.): Institutional/arbitrage — secondary beneficiary. Profitable coordination facilitators.
 *   - Multinational Capital (Fortune 500 firms): Institutional/arbitrage — tertiary beneficiary via profit shifting. Captures extracted rents.
 *   - Peripheral Economies (Global South, Eastern Europe, smaller eurozone states): Moderate/constrained — trapped in currency regime, experience capital flight, cannot compete in tax rates without sacrificing social spending.
 *   - Wage Earners (Global North and South): Powerless/trapped — unable to exit nominal wage contracts; experience erosion of real purchasing power via Cantillon effects.
 *   - Non-Haven Fiscal Authorities (Germany, Japan, Scandinavia, Canada, Australia): Moderate/constrained to organized — benefit from trade access and capital markets, simultaneously constrained by regulatory capture and tax competition.
 *   - Reform Coalition (OECD, BRICS, unions, progressive economists): Organized/constrained — building alternative pathways: minimum tax agreements, de-dollarization initiatives, capital control experiments.
 *   - Bretton Woods Institutions (IMF, World Bank, GATT/WTO successors): Institutional/arbitrage — maintain ritual legitimacy with declining functional enforcement; increasingly piton-like.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hegemonic_entropy_2026, 0.58).
domain_priors:suppression_score(hegemonic_entropy_2026, 0.68).
domain_priors:theater_ratio(hegemonic_entropy_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hegemonic_entropy_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hegemonic_entropy_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hegemonic_entropy_2026, tangled_rope).
narrative_ontology:human_readable(hegemonic_entropy_2026, "The Asymmetry of Hegemonic Decay").
narrative_ontology:topic_domain(hegemonic_entropy_2026, "economic/political").

domain_priors:requires_active_enforcement(hegemonic_entropy_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, currency_issuers).
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, multinational_capital).
narrative_ontology:constraint_beneficiary(hegemonic_entropy_2026, tax_haven_operators).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, peripheral_economies).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, labor_wage_earners).
narrative_ontology:constraint_victim(hegemonic_entropy_2026, fiscal_capacity_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL ECONOMY (SNARE) — Trapped in currency regime and capital flight patterns. Cannot exit without catastrophic currency revaluation or capital controls (which carry their own costs). Bears full cost of Cantillon effects: monetary expansion enriches capital centers first, peripheral economies see inflation without corresponding wage growth. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WAGE EARNER (SNARE) — Trapped in nominal wage contracts while real purchasing power erodes via monetary expansion and asset inflation. Exit options limited: changing employment doesn't change underlying monetary constraint. d≈0.88, f(d)≈1.28, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: NON-HAVEN FISCAL AUTHORITY (TANGLED ROPE) — Benefits from access to global trade, currency stability, and capital markets. Simultaneously constrained by: (a) inability to tax mobile capital without flight, (b) regulatory capture by multinational firms that threaten exit, (c) pressure to compete in tax rates for investment. Dual function: maintains macroeconomic coordination (currency, contract law, infrastructure) while experiencing asymmetric extraction via tax competition. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CURRENCY HEGEMON (ROPE) — Primary beneficiary of Cantillon mechanism: early access to newly created currency gives capital accumulation advantage. Experiences the constraint as coordination: global use of hegemonic currency enables reserve status, pricing power, and seigniorage. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Negative effective extraction: net beneficiary. The regime is genuinely coordinating (reduced transaction costs, price stability for trade) AND extractive (hegemonic rent).
constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TAX HAVEN OPERATOR (ROPE) — Experiences the constraint as pure coordination mechanism for capital efficiency. Tax arbitrage is their coordinating function: enabling capital to flow to highest-return uses by reducing friction. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.03. The arbitrage itself provides genuine efficiency (lower transaction costs for mobile capital). From this perspective, the global order is coordinating, not extracting.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REFORM COALITION (TANGLED ROPE) — Organized agents (OECD, BRICS, labor unions, progressive tax advocates) see the constraint as hybrid: the global order does coordinate trade and finance (coordination function), but extraction mechanisms (tax avoidance, capital flight, Cantillon effects) are layered onto coordination. They experience significant coercion (need to compete in tax rates, cannot unilaterally repatriate capital without capital controls), but also benefit from access to global supply chains and capital markets. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48. Moderate effective extraction, but organized agents are building alternative pathways (OECD minimum tax, BRICS de-dollarization).
constraint_indexing:constraint_classification(hegemonic_entropy_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: BRETTON WOODS RITUAL (PITON) — Traditional international institutions (IMF, World Bank, GATT/WTO framework) maintain coordinating function in narrative but increasingly theatrical in implementation. Their actual enforcement capacity over tax policy and capital controls has eroded; they perform institutional legitimacy while power shifts to unilateral monetary policy and tax arbitrage. Theater_ratio=0.64 reflects this degradation: substantial performative element (institutional review missions, structural adjustment theater) alongside declining functional verification of compliance. d≈0.12, f(d)≈0.00, σ=1.2 → χ≈0.00.
constraint_indexing:constraint_classification(hegemonic_entropy_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risk of naturalizing hegemonic extraction as an immutable law of economics: 'large economies always have monetary advantages' or 'capital always flows to highest returns' or 'tax competition is inherent to sovereign states.' These statements appear natural-law-like. However, the structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts the mountain classification. The engine will flag this as a false summit: the apparent inevitability is contingent on institutional arrangements (fiat currency regime, weak capital controls, tax haven tolerance) that are human-made and reversible.
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
 *   Extractiveness (0.58): The constraint extracts significantly via multiple channels: (1) Cantillon mechanism (monetary expansion benefits capital first; peripheral economies absorb inflation without corresponding wage growth), (2) tax arbitrage (capital flight reduces fiscal revenue for social spending), (3) debt trap dynamics (peripheral economies must service debt in hegemon currency), (4) wage compression (labor cannot compete with mobile capital). However, extractiveness is not >0.70 because genuine coordination benefits persist: lower transaction costs for trade, price stability, enforceable contracts. The 0.58 reflects the hybrid nature: coordination infrastructure is being used as an extraction mechanism but remains partially functional. Suppression (0.68): High but not maximal. Barriers to exit include: currency regime lock-in (switching currency costs are enormous), capital controls (socially and politically costly), coordinating function dependency (leaving the regime means forgoing trade and investment benefits). Peripheral economies have some limited exit options (BRICS, trade agreements, regional monetary cooperation) but they are expensive and still incomplete. Labor has almost no exit options (cannot emigrate en masse, cannot change macroeconomic regimes unilaterally). Theater ratio (0.64): Moderate-high and rising. Bretton Woods institutions (IMF, World Bank) perform conditionality and structural adjustment theater with declining actual enforcement. Tax transparency agreements are performed internationally while capital continues to flow to haven jurisdictions. The ritual of international coordination persists while real power migrates to unilateral central bank policy and de facto tax competition. The theater has risen from 0.35 (Bretton Woods was genuinely enforced) to 0.64 (institutional performativity now dominates). Claimed type (tangled_rope): The regime has both genuine coordination (trade, finance, contracts) and asymmetric extraction (monetary, fiscal, wage) overlaid on the same institutional infrastructure. Requires active enforcement: central banks and tax authorities must actively maintain the extraction mechanisms (capital flow tolerance, interest rate policy that favors asset holders).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival disagreement. The currency hegemon and tax haven operators see coordination (Rope) — they experience the regime as enabling and profitable. The wage earner and peripheral economy see pure extraction (Snare) — they experience only the costs and constraints. The reform coalition and non-haven fiscal authorities see Tangled Rope — they benefit from the coordination function but are increasingly constrained by the extraction mechanisms. The Bretton Woods institutional system sees itself as coordinating (rope perspective from their own view) but analysts see it as degraded/piton from the civilizational view. The false summit perspective risks naturalizing the extraction as inevitable economics. This spread from Rope to Snare (across the same infrastructure) is diagnostic of hegemonic decay: the regime is still formally coordinating but the asymmetric extraction has become the dominant feature for most agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Currency hegemon: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Seigniorage, monetary policy autonomy, reserve status. Tax haven operators: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Capital attraction via arbitrage. Peripheral economies: Victim + trapped → d≈0.92, f(d)≈1.38. Currency lock-in, capital flight, inflation exposure. Wage earners: Victim + trapped → d≈0.88, f(d)≈1.28. Nominal wage contracts, purchasing power erosion, no exit options. Non-haven fiscal authorities: Victim + constrained → d≈0.68, f(d)≈1.05. Tax competition pressure, regulatory capture risk, but not fully trapped (can cooperate, can leverage trade access). Reform coalition: Victim/Beneficiary hybrid + constrained → d≈0.55, f(d)≈0.75. Mixed experience (benefit from coordination, constrained by extraction mechanisms) but organized so not fully trapped. The spread of d values (0.08 to 0.92) across a single institutional infrastructure is the signature of Tangled Rope at the system level.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the regime is genuinely Tangled Rope: it was originally pure Rope (postwar coordination), has accumulated Snare mechanisms (Cantillon, tax arbitrage, debt traps) overlaid on the Rope infrastructure, and now exhibits both coordination and extraction simultaneously from the same institutions. The regime cannot be correctly classified as either pure Rope or pure Snare because it IS both: the trade system coordinates, the monetary system extracts; the capital markets coordinate, the currency regime extracts. The false summit perspective (Mountain) naturalizes this as inevitable economics ('capital always seeks highest returns,' 'large economies always have monetary advantages'). The mandatrophy is resolved by rejecting the false summit and accepting the Tangled Rope classification: the extraction is not inevitable, it is layered institutional design that can be reformed (multilateral tax agreements, alternative currency baskets, capital controls with openness, wage indexation). The reform coalition's Scaffold-like perspective (seeing this as a temporary institutional phase with a sunset toward more symmetric arrangements) is neither naive nor inevitable — it depends on whether organizing agents can overcome the coordination problems of collective action against entrenched extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cantillon_mechanism_reversibility,
    'Is the Cantillon effect (monetary expansion benefits capital-holders first, wage-earners last) an irreducible feature of fiat currency or a reversible institutional arrangement?',
    'Empirical comparison of monetary expansion effects under different institutional regimes (direct wage indexation, helicopter money to workers first, universal basic income funded by monetary expansion, historical comparison with indexation schemes). Economic simulation of alternative transmission mechanisms.',
    'If irreversible: the asymmetry is mountain-like and cannot be removed without abandoning fiat currency. If reversible: the asymmetry is snare/tangled_rope (institutional extraction) and policy can redirect. This determines whether hegemonic decay is structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cantillon_mechanism_reversibility, empirical, 'Whether Cantillon effects are irreducible or institutional').

omega_variable(
    tax_haven_coordination_efficiency,
    'Does tax arbitrage (capital flowing to tax-minimized jurisdictions) increase genuine economic efficiency or merely reallocate extracted rents?',
    'Productivity analysis of capital in tax havens vs operational headquarters. Measurement of real economic output generated per unit of capital in tax-minimized vs non-minimized regimes. Cross-border capital flow tracking and ultimate investment outcomes.',
    'If efficiency-enhancing: tax havens are legitimately coordinating (Rope perspective is correct). If rent-reallocation: they are pure extraction (Snare escalation). This determines whether the tax haven operator perspective is correctly classified as Rope or misclassified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_haven_coordination_efficiency, empirical, 'Whether tax havens increase efficiency or reallocate rents').

omega_variable(
    hegemon_decay_threshold,
    'At what point does hegemonic monetary power threshold from coordination mechanism to pure extraction regime? What are the measurable indicators?',
    'Historical analysis of previous hegemonic transitions (British pound → US dollar, comparison with euro adoption). Measurement of: (a) seigniorage rates, (b) capital flight elasticity to rate differentials, (c) dollarization ratios in peripheral economies, (d) real wage divergence across core vs periphery.',
    'Identifies whether the current regime is still primarily coordinating (Rope dominant) or has crossed into extraction dominance (Snare/Tangled Rope dominant). Determines urgency of reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemon_decay_threshold, empirical, 'Threshold indicators for hegemonic decay from coordination to extraction').

omega_variable(
    alternative_monetary_regime_feasibility,
    'Is a multi-polar or decentralized monetary regime (BRICS, crypto-backed baskets, SDR expansion) structurally capable of solving the Cantillon asymmetry or would it recreate the same extraction patterns?',
    'Comparative institutional analysis of proposed alternatives. Simulation of monetary dynamics under multi-currency standards. Historical precedent: gold standard decentralization, Bretton Woods decentralization failures, current euro experience.',
    'If alternatives are feasible: the tangled rope constraint is solvable via reform and the reform coalition perspective is correct. If alternatives recreate asymmetries: the problem is deeper (Snare/Mountain) and requires structural economic change, not monetary regime change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_monetary_regime_feasibility, conceptual, 'Feasibility of alternative monetary regimes to reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hegemonic_entropy_2026, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heg_entropy_tr_t0, hegemonic_entropy_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(heg_entropy_tr_t20, hegemonic_entropy_2026, theater_ratio, 20, 0.48).
narrative_ontology:measurement(heg_entropy_tr_t35, hegemonic_entropy_2026, theater_ratio, 35, 0.64).

% Extraction over time
narrative_ontology:measurement(heg_entropy_be_t0, hegemonic_entropy_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(heg_entropy_be_t20, hegemonic_entropy_2026, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(heg_entropy_be_t35, hegemonic_entropy_2026, base_extractiveness, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hegemonic_entropy_2026, global_infrastructure).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, capital_flight_debt_trap).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, wage_deflation_asset_inflation).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, tax_competition_race_to_bottom).
narrative_ontology:affects_constraint(hegemonic_entropy_2026, fiat_monetary_regime_asymmetry).

% DUAL FORMULATION NOTE:
% Hegemonic decay is upstream of multiple specific constraints (capital flight, wage deflation, tax competition) that decompose its extraction mechanisms. The hegemonic entropy constraint has ε=0.58 (Tangled Rope) reflecting the mixed coordination-extraction nature at the system level. Downstream constraints have higher ε values reflecting specific extraction mechanisms: capital flight (ε=0.72), wage deflation (ε=0.65), tax competition (ε=0.68). These are not separate constraints but specific instantiations of the general hegemonic asymmetry. They are linked as family members via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hegemonic_entropy_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
