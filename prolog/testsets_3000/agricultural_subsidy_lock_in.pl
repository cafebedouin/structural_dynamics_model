% ============================================================================
% CONSTRAINT STORY: agricultural_subsidy_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agricultural_subsidy_lock_in, []).

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
 *   constraint_id: agricultural_subsidy_lock_in
 *   human_readable: Agricultural Subsidy Lock-In and Political Economy Entrenchment
 *   domain: political_economy/agricultural_policy
 *
 * SUMMARY:
 *   Agricultural subsidies originated as a genuine coordination mechanism to
 *   stabilize prices, prevent farm debt defaults, and ensure food security
 *   during economic crisis (U.S. Agricultural Adjustment Act of 1933,
 *   European Common Agricultural Policy post-WWII). Over 80+ years, subsidies
 *   have evolved into a tangled extraction mechanism that locks producers and
 *   nations into commodity monoculture while simultaneously creating its own
 *   perpetuation system through path dependency, political constituencies,
 *   and international competitive dynamics. The constraint exemplifies how
 *   coordination mechanisms degrade into extraction through institutional
 *   inertia, beneficiary capture, and multi-level lock-in (individual farmer
 *   dependence, rural political power, international trade dynamics). The
 *   measurement trajectory shows extractiveness and theater increasing
 *   together — as genuine coordination functions attenuated, performative
 *   justifications intensified. The spread across all six constraint types
 *   reveals that the subsidy system IS multiple constraints simultaneously:
 *   coordination for large producers, extraction for small producers,
 *   temporary support for some regions, degraded ritual for policy
 *   institutions, snare for developing nations, and false natural law when
 *   viewed civilizationally.
 *
 * KEY AGENTS:
 *   - Small Farmers: Primary victims (powerless/trapped) — locked into subsidy-dependent land values and commodity monoculture; trapped exit due to debt structure and alternative employment scarcity
 *   - Large Commodity Producers: Primary beneficiaries (institutional/arbitrage) — capture subsidy rents, stable markets, and infrastructure investment; mobile exit options enable arbitrage
 *   - Agricultural Input Suppliers: Beneficiaries (institutional/arbitrage) — subsidies fund demand for seeds, fertilizers, equipment; benefit from scale lock-in
 *   - Mid-Scale Producers: Mixed (moderate/constrained) — benefit from some programs but trapped in commodity model; cannot innovate toward diversification within subsidy constraints
 *   - Farming Cooperatives: Organized beneficiaries (organized/constrained) — leverage subsidy pooling for collective infrastructure but locked into commodity coordination
 *   - Developing-Nation Farmers: Victims (powerless/trapped at global scope) — destroyed by dumped commodity exports; trapped in dependent agricultural sectors unable to compete
 *   - Agricultural Policy Institutions: Institutional maintenance (institutional/arbitrage) — preserve subsidy apparatus through performative justification despite recognition of degradation
 *   - Trading Nations: Powerful victims (powerful/mobile) — experience subsidy dumping but locked into retaliatory subsidy escalation; escape constrained by trade agreement interdependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agricultural_subsidy_lock_in, 0.58).
domain_priors:suppression_score(agricultural_subsidy_lock_in, 0.65).
domain_priors:theater_ratio(agricultural_subsidy_lock_in, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agricultural_subsidy_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(agricultural_subsidy_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(agricultural_subsidy_lock_in, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agricultural_subsidy_lock_in, tangled_rope).
narrative_ontology:human_readable(agricultural_subsidy_lock_in, "Agricultural Subsidy Lock-In and Political Economy Entrenchment").
narrative_ontology:topic_domain(agricultural_subsidy_lock_in, "political_economy/agricultural_policy").

domain_priors:requires_active_enforcement(agricultural_subsidy_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agricultural_subsidy_lock_in, large_commodity_producers).
narrative_ontology:constraint_beneficiary(agricultural_subsidy_lock_in, agricultural_input_suppliers).
narrative_ontology:constraint_beneficiary(agricultural_subsidy_lock_in, political_constituencies).
narrative_ontology:constraint_victim(agricultural_subsidy_lock_in, small_farmers).
narrative_ontology:constraint_victim(agricultural_subsidy_lock_in, taxpayers).
narrative_ontology:constraint_victim(agricultural_subsidy_lock_in, developing_agriculture_nations).
narrative_ontology:constraint_victim(agricultural_subsidy_lock_in, environmental_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL FARMER (SNARE) — Trapped by subsidy structure that rewards scale and commodity production. Cannot compete with subsidized industrial producers; cannot exit farming without catastrophic loss of land and livelihood. Suppression is structural: debt dependency, land obligations, and the subsidy-inflated land prices that subsidies themselves created. Maximum extraction experienced by the structurally immobilized agent.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVELOPING-NATION FARMERS (SNARE) — Trapped by dumped commodity exports from subsidized producers. Cannot compete on price; their domestic agricultural sectors are destroyed by the spillover extraction. No exit from the global subsidy regime without domestic agriculture collapse. Generational scope reveals the long-term structural damage.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-SCALE PRODUCERS (TANGLED ROPE) — Constrained by subsidy-induced land price inflation and input cost spirals, but also benefit from some subsidy programs and infrastructure investment. Experience both extraction (locked into commodity production model) and coordination benefit (guaranteed market access). High suppression but not total — some agency remains.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE COMMODITY PRODUCERS AND INPUT SUPPLIERS (ROPE) — Primary beneficiaries experiencing the constraint as coordination: subsidies stabilize demand, create reliable markets, and fund infrastructure (irrigation, research, transportation). Exit options abundant (can shift markets, diversify crops, relocate operations). Net benefit from the subsidy system. The constraint appears as pure coordination from their position.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FARMING COOPERATIVES AND REGIONAL ACTORS (TANGLED ROPE) — Organized agents see both coordination function (subsidy pooling enables collective infrastructure) and extraction (locked into commodity model that prevents diversification and innovation). Some agency and negotiating power, but constrained by path dependency and political economy of rural voting blocs.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AGRICULTURAL POLICY APPARATUS (PITON) — The subsidy system has become substantially performative. Originally created (1930s New Deal) to coordinate genuine coordination problems (price volatility, farm debt, market access), the subsidy structure now maintains itself through institutional inertia and political theater. Agricultural bureaucracies, commodity groups, and rural political constituencies preserve the system despite recognition that subsidy efficiency is degraded. Theater ratio 0.68 reflects the gap between stated policy goals (farm income stability, food security) and actual outcomes (extraction, environmental damage, market distortion).
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: TRADING NATIONS (TANGLED ROPE) — Powerful agents experience both coordination (global market access) and extraction (subsidized commodities depress their agricultural sectors' competitiveness). Mobile but constrained by retaliatory subsidy escalation and trade agreement lock-in. See the system as hybrid: partially beneficial market framework, partially extractive dumping mechanism.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — The risk of false mountain classification: viewing subsidy lock-in as an inevitable consequence of agriculture's sector characteristics (weather dependency, price volatility, long production cycles). This naturalizes what is actually a contingent political-institutional arrangement. True structural limits exist (crop failure risk), but the subsidy lock-in itself is a constructed constraint, not a law of nature.
constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agricultural_subsidy_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agricultural_subsidy_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agricultural_subsidy_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agricultural_subsidy_lock_in, TR),
    TR >= 0.70.

:- end_tests(agricultural_subsidy_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The subsidy system demonstrates both genuine coordination functions (market stabilization, infrastructure investment enabling food production at scale) and significant extraction (wealth concentration to large producers, small farmer trap, developing nation agricultural destruction, environmental cost shifting). The measured value reflects that coordination benefits are real but have been substantially overtaken by extraction dynamics. The measurement trajectory (0.25 → 0.58 over 80 years) shows degradation from primarily coordination (early subsidies) to primarily extraction (contemporary subsidy bloat). Suppression (0.65): High. Multiple suppression mechanisms operate simultaneously: (1) Structural: land debt service, lack of alternative rural employment, commodity infrastructure lock-in make exit materially difficult. (2) Political: rural voting blocs and agricultural lobbies suppress policy reform. (3) Cognitive/Identity-locked (for some agents): farming identity has become constitutionally linked to subsidy dependency; farmers perceive subsidies as necessary to 'real farming.' Theater ratio (0.68): High and rising. Contemporary subsidies are substantially decoupled from their original stated functions (price stabilization, food security, farm income). Modern subsidy justifications invoke environmental stewardship, rural community preservation, and food system resilience — all of which are secondary to the primary function of maintaining producer rents. The gap between stated purpose and actual outcome is the theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless small farmer's snare classification and the institutional large producer's rope classification is a diagnostic signal: the same constraint produces opposite experienced extractiveness based on structural position. The piton classification at civilizational scope reveals institutional degradation — the policy apparatus maintains subsidy theater despite reduced functional capacity. The false mountain classification at analytical scope is the most revealing: viewing subsidy dependency as inherent to agriculture naturalizes what is actually a constructed political-economic lock-in. The presence of genuine coordination functions (price stabilization, infrastructure investment) explains why this is Tangled Rope rather than pure Snare — but the rope component is increasingly theater while the extraction component is materially locked in.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation prioritizes beneficiary/victim structural relationships over nominal power. Large commodity producers are beneficiaries with arbitrage exit options, producing low d (beneficiary + mobile exit = ~0.15, producing negative χ). Small farmers are victims with trapped exit, producing high d (~0.95, producing maximum f(d) ≈ 1.42). Mid-scale producers are ambiguous — they receive some subsidy benefits but bear costs of commodity lock-in and land price inflation. Their directionality reflects constrained exit and mixed beneficiary/victim status (d ≈ 0.50). Developing-nation farmers are victims of subsidy spillover with trapped exit at global scope (d ≈ 0.95). Organized agents (cooperatives) are nominally beneficiaries but constrained by path dependency rather than truly arbitrage-capable, producing mid-range d (≈ 0.45). The differing d values across perspectives are stable — they reflect structural differences in who benefits and who bears costs from THIS specific constraint, not observer perspective alone.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that subsidy lock-in contains genuinely distinct structural mechanisms: (1) A coordination mechanism (price stabilization, market access, rural infrastructure) that was genuine in origin and remains partially functional. (2) An extraction mechanism (rent concentration, small producer entrapment, developing-nation agricultural destruction) that has grown to dominate. The constraint is NOT purely coordinated (that would be pure Rope and misses the small farmer snare). It is NOT purely extractive (that would be pure Snare and misses the large producer rope and genuine infrastructure coordination). Tangled Rope is the correct type because both mechanisms operate simultaneously: active enforcement (farm bills, subsidy distribution) is required, beneficiaries are identifiable (large producers, input suppliers), victims are identifiable (small farmers, taxpayers, developing nations), and the coordination function is genuine (market stabilization) but insufficient to justify the extraction it enables. The mandatrophy resolves in favor of Tangled Rope precisely because misclassifying as pure Rope would suppress recognition of extraction, while misclassifying as pure Snare would ignore genuine coordination. Tangled Rope forces acknowledgment of both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_dependency_exit_cost,
    'What proportion of the measured suppression is structural (external barriers to exiting subsidy-dependent agriculture) versus identity-locked (farmers who have internalized the subsidy as essential to farming identity)?',
    'Post-subsidy removal behavioral analysis; tracking farmers in regions where subsidies have been reduced or eliminated; measuring whether exit barriers persist after external constraints are removed; farmer interviews on identity formation and subsidy dependency',
    'If predominantly structural: reclassify suppression as barrier-removal problem (policy change can reduce extraction). If predominantly identity-locked: recognition that subsidy dependency has become fused with farming identity itself — policy change alone insufficient without identity reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_dependency_exit_cost, empirical, 'Structural versus identity-locked components of suppression').

omega_variable(
    optimal_subsidy_floor,
    'Is there a genuinely beneficial subsidy level that addresses coordination problems (price stabilization, food security) without triggering extraction dynamics and lock-in? Where is that threshold?',
    'Comparative policy analysis across jurisdictions with different subsidy levels; economic modeling of extraction-free coordination costs; historical analysis of subsidy design intent versus actual outcomes; analysis of decoupled vs. coupled subsidy mechanisms',
    'If threshold exists and is lower than current subsidies: tangled rope classification confirmed — system has degraded from coordination to extraction. If no threshold exists (any subsidy triggers lock-in): snare classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_subsidy_floor, empirical, 'Existence and location of benign subsidy threshold').

omega_variable(
    international_subsidy_escalation_lock,
    'Is the subsidy lock-in driven primarily by domestic political economy or by an iterated prisoner''s dilemma dynamic where nations subsidize agriculture to match trading partners'' subsidies?',
    'Historical sequence analysis of subsidy adoption across trading blocs; game-theoretic modeling of subsidy reduction vs. unilateral vulnerability; analysis of subsidy reduction attempts and retaliatory escalation patterns; empirical testing of whether trade agreements that constrain all parties show subsidy reductions',
    'If primarily domestic: subsidy lock-in is a constraint within one political system (tangled rope from institutional perspective). If primarily international coordination failure: subsidy lock-in is an inter-institutional snare where all nations are victims of mutual extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_subsidy_escalation_lock, empirical, 'Domestic versus international drivers of subsidy lock-in').

omega_variable(
    environmental_cost_externalization,
    'How much of the measured extractiveness comes from environmental cost shifting (subsidies enable unsustainable practices) versus direct economic extraction from small producers?',
    'Life-cycle analysis of subsidy-enabled agricultural practices; quantification of environmental externality costs not captured in commodity prices; measurement of soil degradation, water depletion, and pesticide/fertilizer runoff tied to subsidy-incentivized practices; comparison of environmental outcomes in subsidy vs. non-subsidy regions',
    'If environmental externalization is major component: true extractiveness is higher than measured (environmental costs borne by future generations and non-farmer populations). Reclassifies constraint as intergenerational and global extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_cost_externalization, empirical, 'Environmental cost externalization in subsidy lock-in').

omega_variable(
    path_dependency_irreversibility,
    'If subsidies were removed, could the agricultural system return to pre-subsidy equilibrium, or has path dependency created irreversible lock-in (infrastructure, crop genetics, land values, knowledge systems)?',
    'Historical analysis of agricultural systems pre-subsidy vs. post-subsidy; testing of agricultural reconversion programs; analysis of whether subsidy withdrawal leads to system recovery or collapse; modeling of feedback loops that increase subsidy dependency over time',
    'If reversible: subsidy lock-in is a tangled rope that could be unwound with political will. If irreversible: it has become a snare where even removing the explicit subsidy constraint leaves agents trapped in dependent systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_irreversibility, empirical, 'Path dependency and reversibility of subsidy lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agricultural_subsidy_lock_in, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agsubsidy_tr_t0, agricultural_subsidy_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(agsubsidy_tr_t20, agricultural_subsidy_lock_in, theater_ratio, 20, 0.48).
narrative_ontology:measurement(agsubsidy_tr_t40, agricultural_subsidy_lock_in, theater_ratio, 40, 0.58).
narrative_ontology:measurement(agsubsidy_tr_t60, agricultural_subsidy_lock_in, theater_ratio, 60, 0.68).
narrative_ontology:measurement(agsubsidy_tr_t80, agricultural_subsidy_lock_in, theater_ratio, 80, 0.66).

% Extraction over time
narrative_ontology:measurement(agsubsidy_be_t0, agricultural_subsidy_lock_in, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(agsubsidy_be_t20, agricultural_subsidy_lock_in, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(agsubsidy_be_t40, agricultural_subsidy_lock_in, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(agsubsidy_be_t60, agricultural_subsidy_lock_in, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(agsubsidy_be_t80, agricultural_subsidy_lock_in, base_extractiveness, 80, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agricultural_subsidy_lock_in, resource_allocation).
narrative_ontology:affects_constraint(agricultural_subsidy_lock_in, commodity_price_volatility).
narrative_ontology:affects_constraint(agricultural_subsidy_lock_in, food_system_concentration).
narrative_ontology:affects_constraint(agricultural_subsidy_lock_in, rural_political_entrenchment).
narrative_ontology:affects_constraint(agricultural_subsidy_lock_in, developing_nation_agricultural_collapse).

% DUAL FORMULATION NOTE:
% Agricultural subsidy lock-in decomposes into multiple structurally distinct constraints along domain and temporal lines. The price stabilization coordination function (ε ≈ 0.15, pure Rope) is distinct from the rent extraction mechanism (ε ≈ 0.72, pure Snare). The measured tangled rope classification reflects their simultaneous operation. Decomposition into separate stories would enable more precise measurement of genuine coordination cost versus extractive bloat.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agricultural_subsidy_lock_in, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
