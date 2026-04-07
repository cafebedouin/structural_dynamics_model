% ============================================================================
% CONSTRAINT STORY: sotu_1950_truman_reciprocal_trade_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1950_truman_reciprocal_trade_agreements, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1950_truman_reciprocal_trade_agreements
 *   human_readable: Reciprocal Trade Agreement Framework (Truman, 1950)
 *   domain: international_trade/political_economy
 *
 * SUMMARY:
 *   The reciprocal trade agreement framework, articulated by President Truman
 *   in 1950 as the mechanism for post-war economic reconstruction, represents
 *   a core institutional constraint shaping international commerce and
 *   geopolitical alignment during the Cold War. The framework operates by
 *   negotiating bilateral and multilateral tariff reductions that nominally
 *   benefit all parties through market access, but embed asymmetric
 *   structural advantages favoring the U.S. and capital-exporting allied
 *   nations (Western Europe, Japan) while constraining developing nations and
 *   extracting costs from domestic import-competing sectors. The constraint
 *   exhibits tangled rope character at the analytical level: it coordinates
 *   post-war alliance formation and economic recovery while systematically
 *   extracting from powerless workers in protected sectors and weak nations
 *   in commodity-exporting positions. The framework's theater ratio has
 *   increased over time (0.38 to 0.58 across the interval) as successive
 *   negotiation rounds become increasingly performative — the substantive
 *   tariff reductions decline in magnitude while the institutional apparatus
 *   of trade governance expands. By the 1960s, the framework shows signs of
 *   piton classification: the institutional form (GATT negotiation rounds)
 *   persists through inertia despite declining functional effectiveness at
 *   tariff reduction, as capital account openness and non-tariff barriers
 *   (quotas, voluntary export restraints, regulatory standards) increasingly
 *   replace tariff protection as the primary extraction mechanism.
 *
 * KEY AGENTS:
 *   - American export-oriented manufacturers and agricultural producers: Primary beneficiary (institutional/arbitrage) — gain market access and capital export opportunities
 *   - Domestic import-competing workers (textiles, steel, coal): Primary victim (powerless/trapped) — face wage suppression and unemployment with no negotiated compensation
 *   - U.S. executive and Congress: Secondary beneficiary (powerful/mobile) — coordinate post-war order and geopolitical influence, but absorb domestic political cost from tariff reductions
 *   - Allied nations (Western Europe, Japan): Secondary beneficiary with constrained exit (institutional/constrained) — benefit from U.S. market access and capital but face security linkage that limits negotiating autonomy
 *   - Developing nations agricultural exporters: Secondary victim (moderate/constrained) — face persistent U.S. agricultural protection despite formal reciprocity framework
 *   - Institutional trade governance (GATT/WTO): Organized actor (organized/constrained) — coordinates multilateral tariff reductions with sunset logic embedded in successive rounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1950_truman_reciprocal_trade_agreements, 0.48).
domain_priors:suppression_score(sotu_1950_truman_reciprocal_trade_agreements, 0.62).
domain_priors:theater_ratio(sotu_1950_truman_reciprocal_trade_agreements, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1950_truman_reciprocal_trade_agreements, extractiveness, 0.48).
narrative_ontology:constraint_metric(sotu_1950_truman_reciprocal_trade_agreements, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1950_truman_reciprocal_trade_agreements, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1950_truman_reciprocal_trade_agreements, tangled_rope).
narrative_ontology:human_readable(sotu_1950_truman_reciprocal_trade_agreements, "Reciprocal Trade Agreement Framework (Truman, 1950)").
narrative_ontology:topic_domain(sotu_1950_truman_reciprocal_trade_agreements, "international_trade/political_economy").

domain_priors:requires_active_enforcement(sotu_1950_truman_reciprocal_trade_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1950_truman_reciprocal_trade_agreements, american_exporters).
narrative_ontology:constraint_beneficiary(sotu_1950_truman_reciprocal_trade_agreements, allied_nations_seeking_market_access).
narrative_ontology:constraint_beneficiary(sotu_1950_truman_reciprocal_trade_agreements, us_capital_markets).
narrative_ontology:constraint_victim(sotu_1950_truman_reciprocal_trade_agreements, domestic_import_competing_sectors).
narrative_ontology:constraint_victim(sotu_1950_truman_reciprocal_trade_agreements, developing_nations_agricultural_exporters).
narrative_ontology:constraint_victim(sotu_1950_truman_reciprocal_trade_agreements, small_scale_domestic_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC IMPORT-COMPETING WORKER (SNARE) — Textile, steel, and agricultural workers in non-export sectors face job displacement with no exit option. Tariff reductions negotiated by the U.S. government benefit exporters but expose workers to import competition. They cannot negotiate, cannot exit their industry without severe cost, and bear full extraction through wage suppression and unemployment. Suppression is structural: retraining programs are insufficient, geographic mobility is costly, and alternative employment in export sectors is limited.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AMERICAN EXPORT-ORIENTED FIRMS (ROPE) — Manufacturers and agricultural exporters benefit from reciprocal tariff reductions that open foreign markets. They experience the constraint as pure coordination: the negotiated framework creates market access that would not otherwise exist. They have arbitrage options (can relocate, can diversify across markets) and benefit directly from the constraint. Extraction runs toward them, not away.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED NATIONS / RECONSTRUCTION ECONOMIES (TANGLED ROPE) — Western European and Japanese governments benefit from U.S. market access and capital inflows while negotiating tariff reductions that expose their own domestic industries. They have constrained exit (cannot fully reject the framework without risking U.S. economic support and security guarantees during Cold War). The constraint coordinates post-war economic recovery while embedding asymmetric extraction: the U.S. negotiating position is stronger because of the implicit security guarantee linkage. Both coordination and extraction present.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPING NATIONS / AGRICULTURAL EXPORTERS (SNARE) — Latin American, African, and Asian agricultural producers face tariff reductions in manufactured goods (which they import) but find U.S. agricultural tariffs persist despite reciprocal trade rhetoric. Exit options are severely constrained: developing nations have weak negotiating power, limited capital to diversify into manufacturing, and face commodity price volatility. They bear extraction through unequal terms of trade while receiving minimal market access gains.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: U.S. EXECUTIVE AND CONGRESSIONAL LEADERSHIP (TANGLED ROPE) — Truman and Congress coordinate post-war economic architecture and gain geopolitical influence through trade negotiations. They also face domestic extraction pressure: tariff reductions face organized opposition from import-competing sectors, creating political cost. The executive must enforce the constraint against domestic pressure, making it active enforcement. They have mobile exit (can modify negotiations) but choose constrained implementation to maintain Cold War alliance structure. Both coordination (establishing post-war order) and extraction (political cost of tariff reductions) present.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL TRADE GOVERNANCE (GATT/WTO) (SCAFFOLD) — The reciprocal trade framework is institutionalized through GATT (established 1947, formalized through reciprocal negotiations). The institution coordinates multilateral tariff reductions with a sunset logic embedded in successive rounds (Kennedy Round, Tokyo Round, Uruguay Round): each round ratchets down tariffs further, with escapes and adjustment assistance provisions that decline over time. Institutional actors see temporary coordination problems being solved through negotiation rounds. Theater ratio rises as the institution becomes performative: dispute settlement procedures and rounds become more about process than substantive tariff changes.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (COMPARATIVE INSTITUTIONAL VIEW) — From a civilizational scale, reciprocal trade agreements are a contingent institutional choice among alternatives (autarky, imperial preference blocs, planned trade blocs). The post-war framework embedded U.S. institutional power into trade rules, creating apparent 'reciprocity' while maintaining structural advantages (dollar hegemony, capital market depth, manufacturing dominance in 1950, agricultural subsidies that persist despite tariff reductions). The constraint naturalizes institutional asymmetry as neutral rules.
constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, analytical,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1950_truman_reciprocal_trade_agreements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1950_truman_reciprocal_trade_agreements, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1950_truman_reciprocal_trade_agreements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1950_truman_reciprocal_trade_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The framework systematically benefits export-oriented sectors and capital-exporting nations while imposing costs on import-competing sectors and capital-importing nations. The growth from 0.32 (1950) to 0.48 (1960) reflects increasing gap between nominal reciprocity and actual asymmetric gains. By 1960, the framework is being used to justify capital account openness and non-tariff barriers that amplify extraction. Suppression (0.62): High. Import-competing workers face structural barriers: geographic immobility, industry-specific human capital, slow retraining programs, and lack of political representation in trade negotiations. Developing nations face weak negotiating power and commodity dependence. The framework explicitly suppresses exit options through tariff binding schedules that constrain future unilateral protection. Theater ratio (0.58): Moderate-high, rising. Initial implementation (1950) focused on substantive tariff reductions with measurable market access gains. By 1960, successive GATT rounds become increasingly performative: the Dillon Round and early Kennedy Round negotiations focus on negotiating procedures and escape clause mechanics rather than substantive tariff level changes. The institutional apparatus expands (dispute settlement, transparency rules) while actual tariff reduction rates decline.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full range of classification from tangled rope at the analytical level to snare at the powerless/trapped level. American exporters genuinely experience rope (pure coordination) because the framework solves their collective action problem of accessing foreign markets. Institutional trade governance actors experience scaffold (temporary coordination with sunset logic) because each negotiation round has an implicit sunset — tariffs can only go down so far before the incentive to negotiate disappears. Import-competing workers experience snare because they bear costs with no exit and no voice in the negotiation process. Allied nations experience tangled rope because they coordinate on recovery while facing extraction through the Cold War linkage. Developing nations experience snare (formal asymmetry in 'reciprocal' tariff schedules) or tangled rope (benefits from manufactured goods access alongside agricultural protection costs). The perspectival gap reveals that the constraint's classification depends entirely on which structural position the observer occupies — the same framework is rope for exporters and snare for workers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. American exporters (beneficiary + arbitrage) derive low d, experiencing negative effective extraction (the constraint subsidizes them). Import-competing workers (victim + trapped) derive high d (~0.95), experiencing maximum extraction. Allied nations (nominal beneficiary + constrained by Cold War security linkage) derive moderate d (~0.55), experiencing moderate extraction because while they gain market access, the security linkage prevents genuine arbitrage exit. Developing nations (victim + constrained by weak negotiating power) derive high d (~0.85), experiencing high extraction despite formal 'reciprocity' because the reciprocity is formal (equal percentage reductions) rather than substantive (equal welfare gains). The U.S. executive/Congress (beneficiary + mobile) derive low d because they can modify negotiations, but they also bear domestic political cost (victim of organized labor opposition), which partially offsets their beneficiary status. Directionality overrides are not needed; the structural derivation captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three gates for tangled rope classification. (1) Beneficiaries exist: american_exporters, allied_nations_seeking_market_access, us_capital_markets. (2) Victims exist: domestic_import_competing_sectors, developing_nations_agricultural_exporters, small_scale_domestic_manufacturers. (3) Active enforcement is required: the framework requires executive negotiating authority, congressional authorization of tariff modifications, and ongoing compliance monitoring through GATT dispute settlement. The constraint is neither pure coordination (rope) because asymmetric extraction is substantial, nor pure extraction (snare) because genuine coordination of post-war alliance and trade recovery occurs. The mandatrophy is resolved by recognizing that the constraint simultaneously accomplishes two distinct functions: (a) coordinating post-war economic reconstruction that all parties benefit from (genuine rope function), and (b) embedding structural asymmetries that allow capital-exporting nations and export-oriented sectors to extract rents from import-competing sectors and capital-importing nations (extraction function). The tangled rope classification identifies this duality: the constraint's coordination function is real and necessary (solving the post-war collective action problem of restoring international trade), but it is inextricably intertwined with asymmetric extraction that could not be separated without fundamentally changing the constraint's form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_definition_ambiguity,
    'Do ''reciprocal'' tariff reductions mean equal percentage reductions (formal reciprocity) or equal welfare gains (substantive reciprocity)? The U.S. gains more market access from the same percentage reduction due to greater export capacity.',
    'Comparative analysis of tariff reduction schedules and subsequent trade flow changes; measurement of actual market access gains vs. nominal tariff symmetry across negotiation rounds',
    'If formal reciprocity only: constraint is tangled rope with high extraction for developing nations. If substantive reciprocity required: constraint would need to include capacity-building transfers and asymmetric tariff reductions favoring weaker parties. Current framework assumes formal reciprocity, embedding structural asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_definition_ambiguity, conceptual, 'Whether reciprocity means equal percentage reductions or equal welfare outcomes').

omega_variable(
    agricultural_carve_out_persistence,
    'Why do U.S. agricultural tariffs and subsidies persist despite reciprocal trade agreements? Is this a temporary political compromise or a structural exception proving that the reciprocal framework is theater?',
    'Historical analysis of agricultural tariff schedules across negotiation rounds; comparison of manufacturing tariff reduction rates vs. agricultural protection levels; identification of political coalitions maintaining agricultural exclusions',
    'If temporary: scaffold perspective confirmed — agricultural protections will sunset as political power shifts. If structural: constraint is snare for agricultural exporters and tangled rope becomes more salient (coordination for manufactures, extraction for agriculture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_carve_out_persistence, empirical, 'Whether agricultural protections are temporary or structural exceptions to reciprocal trade').

omega_variable(
    capital_account_asymmetry,
    'How much of the U.S. benefit comes from tariff reductions on goods vs. from capital account openness (allowing U.S. firms and capital to operate freely in allied nations)?',
    'Accounting decomposition of U.S. export gains vs. foreign direct investment gains; measurement of capital flows and profit repatriation vs. merchandise trade benefits',
    'If capital account gains dominate: constraint is primarily extraction mechanism for U.S. capital accumulation, not reciprocal goods trade. Tangled rope classification depends critically on this ratio — higher capital extraction tilts toward snare for capital-importing nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_account_asymmetry, empirical, 'Decomposition of benefits between goods trade access and capital account openness').

omega_variable(
    escape_clause_effectiveness,
    'Do escape clauses (safeguard provisions allowing temporary tariff increases when imports surge) provide meaningful relief for import-competing sectors, or are they theater?',
    'Analysis of escape clause invocations, approval rates, duration of protection granted, and evidence of adjustment assistance uptake; comparison of sectors using escape clauses vs. sectors undergoing unprotected decline',
    'If effective: suppression is moderate (actors have genuine escape routes). If theater: suppression is high (escape clauses exist but are rarely approved or provide insufficient protection), tangled rope classification is at risk of reclassifying to snare for import-competing sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escape_clause_effectiveness, empirical, 'Whether escape clauses provide meaningful protection or are performative').

omega_variable(
    cold_war_linkage_counterfactual,
    'How much of allied nations'' participation in reciprocal trade agreements is voluntary coordination vs. coerced by the implicit security guarantee linkage? Would the same trade outcomes occur without the Cold War context?',
    'Counterfactual analysis using historical trade data from non-aligned nations with different Cold War postures; assessment of trade agreement compliance correlation with military alliance status; study of allied nations'' negotiating positions and documented preferences',
    'If security linkage is dominant: constraint is snare for allied nations, not tangled rope — exit options are not genuinely constrained by trade agreement terms but by geopolitical dependence. If trade benefits are sufficient: tangled rope classification is correct and security is incidental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cold_war_linkage_counterfactual, conceptual, 'Whether allied participation is trade-motivated or security-coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1950_truman_reciprocal_trade_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rta_theater_1950, sotu_1950_truman_reciprocal_trade_agreements, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rta_theater_1955, sotu_1950_truman_reciprocal_trade_agreements, theater_ratio, 5, 0.5).
narrative_ontology:measurement(rta_theater_1960, sotu_1950_truman_reciprocal_trade_agreements, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(rta_extractiveness_1950, sotu_1950_truman_reciprocal_trade_agreements, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rta_extractiveness_1955, sotu_1950_truman_reciprocal_trade_agreements, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(rta_extractiveness_1960, sotu_1950_truman_reciprocal_trade_agreements, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1950_truman_reciprocal_trade_agreements, resource_allocation).
narrative_ontology:boltzmann_floor_override(sotu_1950_truman_reciprocal_trade_agreements, 0.18).
narrative_ontology:affects_constraint(sotu_1950_truman_reciprocal_trade_agreements, gatt_dispute_settlement_procedures).
narrative_ontology:affects_constraint(sotu_1950_truman_reciprocal_trade_agreements, agricultural_subsidy_protection_carve_out).
narrative_ontology:affects_constraint(sotu_1950_truman_reciprocal_trade_agreements, developing_nation_technology_access_barriers).

% DUAL FORMULATION NOTE:
% Reciprocal trade agreements form a constraint family with three distinct structural components: (1) tariff_coordination_mechanism (ε≈0.15, Rope) — the pure coordination of reducing tariff barriers, (2) capital_account_integration (ε≈0.62, Snare) — the extraction mechanism allowing unrestricted capital flows, and (3) agricultural_exception_regime (ε≈0.55, Tangled Rope) — the hybrid coordination of food security alongside asymmetric protection. The story presented here integrates all three, but decomposition into separate stories would show how tariff reductions alone would classify as Rope, while capital openness is closer to Snare, and agricultural protection is Tangled Rope. The upstream constraint is post_war_bipolar_alliance_structure (the Cold War security linkage that makes 'reciprocal' trade agreements necessary); the downstream constraints are the specific sectoral and institutional mechanisms through which asymmetry is maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1950_truman_reciprocal_trade_agreements, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
