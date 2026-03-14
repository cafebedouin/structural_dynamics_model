% ============================================================================
% CONSTRAINT STORY: developing_economy_trade_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developing_economy_trade_asymmetry, []).

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
 *   constraint_id: developing_economy_trade_asymmetry
 *   human_readable: Developing Economy Trade Asymmetry
 *   domain: economic_policy/international_trade
 *
 * SUMMARY:
 *   Developing economy trade asymmetry is a global structural constraint that
 *   has intensified since the 1990 Washington Consensus wave of trade
 *   liberalization. The constraint exhibits genuine coordination functions —
 *   trade access enables technology transfer, FDI, market scale, and
 *   specialized production that are unavailable under autarky.
 *   Simultaneously, it exhibits severe asymmetric extraction: trade terms
 *   deteriorate over time, intellectual property regimes lock developing
 *   economies into technology dependency, labor costs suppress through
 *   competitive bidding, and capital extraction through repatriation and
 *   profit taking overwhelms domestic reinvestment. The constraint's
 *   extractiveness (0.58) and suppression (0.68) reflect that while
 *   developing economies have not returned to pre-liberalization autarky
 *   (implying trapped-level extraction), their negotiating power remains
 *   severely constrained by capital mobility, debt dependency, and
 *   conditional lending. The theater ratio (0.65) reflects that the
 *   justificatory frame (comparative advantage, mutual benefit, convergence
 *   thesis) persists as institutional narrative despite empirical evidence of
 *   divergence and subordination.
 *
 * KEY AGENTS:
 *   - Developing Economy Workers: Primary victims (powerless/trapped) — bear extraction through suppressed wages, unsafe conditions, lack of collective bargaining; comprise >80% of populations in nations bound by constraint
 *   - Multinational Corporations: Primary beneficiaries (institutional/arbitrage) — capture labor cost arbitrage, supply chain efficiency, market access; can exit to alternative sites if terms shift
 *   - Advanced Economy Governments: Powerful beneficiaries with constrained exit (powerful/mobile) — gain trade surplus, low input costs, industrial specialization; politically constrained by domestic deindustrialization constituencies
 *   - Developing Economy Governments: Mixed victims/coordinators (moderate/constrained) — depend on trade access for tax revenue and FDI inflow; lack negotiating power to improve terms; constrained by debt and conditional lending
 *   - Multinational Financial Institutions (IMF/World Bank): Institutional enforcers (institutional/arbitrage) — impose structural adjustment conditions requiring liberalization; have arbitrage exit options and institutional incentives to expand trade regime
 *   - Developed Economy Consumers: Secondary beneficiaries (moderate/mobile) — gain from low-cost imports; exit option available through domestic production but economically irrational
 *   - Developing Economy Coalition (BRICS, G-77): Organized resistance (organized/constrained) — attempt to increase exit options through preferential access and regional blocs; constrained by scale disadvantages and capital dependency
 *   - Comparative Advantage Narrative: Institutional frame (institutional/arbitrage) — justifies asymmetry through economic theory; maintained by development economics profession and trade institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developing_economy_trade_asymmetry, 0.58).
domain_priors:suppression_score(developing_economy_trade_asymmetry, 0.68).
domain_priors:theater_ratio(developing_economy_trade_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developing_economy_trade_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(developing_economy_trade_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(developing_economy_trade_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developing_economy_trade_asymmetry, tangled_rope).
narrative_ontology:human_readable(developing_economy_trade_asymmetry, "Developing Economy Trade Asymmetry").
narrative_ontology:topic_domain(developing_economy_trade_asymmetry, "economic_policy/international_trade").

domain_priors:requires_active_enforcement(developing_economy_trade_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developing_economy_trade_asymmetry, advanced_economies).
narrative_ontology:constraint_beneficiary(developing_economy_trade_asymmetry, multinational_corporations).
narrative_ontology:constraint_beneficiary(developing_economy_trade_asymmetry, developed_economy_consumers).
narrative_ontology:constraint_victim(developing_economy_trade_asymmetry, developing_economy_workers).
narrative_ontology:constraint_victim(developing_economy_trade_asymmetry, developing_economy_domestic_industries).
narrative_ontology:constraint_victim(developing_economy_trade_asymmetry, global_labor_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMY WORKER (SNARE) — Trapped by economic necessity, limited labor mobility, and absence of alternative employment at livable wages. Bears extraction through suppressed wages, unsafe conditions, and lack of collective bargaining power. Cannot exit without catastrophic cost to survival. Maximum extractive experience.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMY GOVERNMENT (TANGLED ROPE) — Faces genuine coordination problem: trade access enables development pathways, technology transfer, and FDI that genuinely improve infrastructure and employment. Simultaneously experiences asymmetric extraction: trade terms deteriorate, capital flows extract profits, and structural adjustment terms prevent domestic industry protection. High suppression (lack of negotiating power) but real coordination benefits. Active enforcement occurs through conditional lending, bilateral pressure, and WTO rules.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL CORPORATION (ROPE) — Experiences the constraint as pure coordination: access to labor cost arbitrage, supply chain optimization, and market entry are all enabled by asymmetric trade terms. Benefits flow directly. Exit options abundant — can relocate to alternative sites if trade terms deteriorate. Low experienced extraction because benefits exceed costs by large margin.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVANCED ECONOMY GOVERNMENT (TANGLED ROPE) — Genuine coordination benefit: trade access expands consumer choice, reduces input costs for domestic industries, and creates interdependence reducing geopolitical conflict risk. Simultaneously extracts through trade surplus accumulation, industrial deindustrialization externalities, and wage suppression in domestic low-skill sectors. Mobile exit option (can shift trade partners, regions) but politically constrained by domestic constituencies. Active enforcement via tariffs, non-tariff barriers, rules of origin requirements.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPING ECONOMY COALITION (TANGLED ROPE) — Organized agents (regional trade blocs, BRICS, G-77) recognize both coordination function (preferential access, technology sharing) and extraction mechanism (structural subordination, terms of trade). Constrained by lack of alternative markets at comparable scale and by capital dependency. Coalition formation itself is an attempt to increase exit options through negotiating power, but power asymmetry remains severe.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COMPARATIVE ADVANTAGE NARRATIVE (PITON) — The justificatory frame (Ricardian comparative advantage, mutually beneficial trade) persists as institutional inertia despite empirical degradation. Historical evidence shows trade liberalization in developing economies often correlates with deindustrialization and wage stagnation rather than convergence predicted by theory. The narrative is largely performative — maintained by development institutions and trade economists because alternatives haven't fully replaced it, not because evidence supports it. Theater ratio high because the ritual of neoclassical justification continues despite contradictory outcomes.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: FAIR TRADE AND LABOR STANDARDS MOVEMENT (SCAFFOLD) — Organized agents (labor unions, NGOs, consumer movements) see the asymmetry as temporary and solvable through certification standards, enforcement of labor rights, and consumer pressure. Treat the constraint as having a sunset: rising wages and standards adoption reduce extraction mechanisms over time. Theater moderately high (certification ritual can become performative) but genuine functional pathway visible. Constrained because enforcement depends on consumer willingness to pay premiums and corporate voluntary compliance.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, wage differentials between economies are inherent to labor cost gradients and capital mobility — structural features of global capitalism. Comparative advantage is a natural consequence of resource distribution and technology gaps. This perspective risks naturalizing what is actually contingent institutional arrangement (IP regimes, capital controls, labor mobility restrictions, historical accumulation). Engine will classify this as false summit — false naturalization of political choices.
constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developing_economy_trade_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developing_economy_trade_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developing_economy_trade_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developing_economy_trade_asymmetry, TR),
    TR >= 0.70.

:- end_tests(developing_economy_trade_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts through multiple channels: labor cost suppression (workers cannot command scarcity rents because competing developing economies bid wages down), technology dependency (IP regimes prevent domestic innovation), terms of trade deterioration (commodity exports have declining value relative to manufactured imports), and capital repatriation (profits extracted faster than reinvestment). However, extractiveness is not maximal (>0.70) because genuine coordination benefits exist — trade access does provide development pathways, technology spillovers, and market scale that developing economies value sufficiently to accept the terms. Base extractiveness increased from 0.42 to 0.58 over the 30-year interval as financialization deepened capital extraction and as labor arbitrage intensified through supply chain globalization. Suppression (0.68): High. Suppression mechanisms include: capital mobility creating competitive bidding for investment (states race to the bottom on labor standards and taxes), debt dependency making exit costly (IMF/World Bank conditionality), technology dependency creating lock-in (reverse-engineering prohibited by IP law), political sovereignty constraints (sanctions, exclusion from markets for non-compliance), and labor mobility restrictions preventing exit to advanced economies. Suppression is high but not total (≥0.90) because some developing economies retain partial policy autonomy and because alternative regional trade pathways are theoretically available (though costly). Theater ratio (0.65): Moderately high. The comparative advantage narrative persists in development economics textbooks and IMF policy prescriptions despite empirical evidence of divergence and deindustrialization in many liberalized developing economies. The ritual of trade negotiations, WTO dispute resolution, and tariff reduction ceremonies continues with the performance of 'mutual benefit' despite asymmetric outcomes. Theater increased over the interval as the gap between predicted (convergence) and observed (divergence) outcomes widened — the narrative required more institutional effort to maintain.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the core mandatrophy diagnostic: beneficiaries misclassifying extraction as coordination. Multinationals and developed governments experience genuine coordination benefits (rope-level logic) and thus classify the constraint as Rope from their perspective. Developing economy governments see mixed function and classify as Tangled Rope. Developing economy workers see pure extraction and classify as Snare. The gap is not ambiguity about the constraint itself — the structural data is identical across perspectives. The gap is that beneficiaries perceive coordination that victims perceive as cover story for extraction. The developed economy government's Tangled Rope classification is also instructive: they do experience real coordination (trade benefits) but this coordination has externalized costs (deindustrialization, wage suppression for low-skill workers) that aren't visible in the GDP gains. The comparative advantage narrative as Piton reflects that the justificatory frame has become increasingly performative — the theory continues to be taught despite mounting evidence that development outcomes don't match predictions.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality pattern is critical for preventing misclassification. Beneficiaries with arbitrage options (multinationals: d≈0.10) experience low or negative χ — extraction flows toward them, not away. Powerful beneficiaries with constrained exit (developed governments: d≈0.40) experience moderate χ. Moderate victims with constrained exit (developing governments: d≈0.60) experience higher χ. Powerless victims with trapped exit (developing workers: d≈0.92) experience maximum χ. This pattern explains why the constraint appears as Snare from the powerless perspective even though base extractiveness is only moderate — the directionality multiplier amplifies the experienced extraction for the trapped agent. The phi derivation (power + exit + structural relationship) feeds into f(d), which scales extractiveness appropriately for each perspective. Without explicit beneficiary/victim declarations and exit option differentiation, the engine would default to symmetrical perspectives and miss the perspectival gap entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSED TANGLED ROPE: The constraint resolves mandatrophy by demonstrating genuine coordination function (trade access enables development) combined with asymmetric extraction (capital extraction, labor suppression, terms of trade deterioration) requiring active enforcement (IMF conditionality, trade agreements, IP enforcement). The beneficiary institutions benefit from labeling this as pure coordination (Rope) — this mislabeling is itself part of the extraction mechanism. The analytical observer must identify: (1) genuine coordination elements exist and are not fabricated; (2) asymmetric extraction is simultaneously real and substantial; (3) the constraint requires enforcement to prevent escape (developing economies cannot simply 'choose' autarky if they have debt obligations); (4) the theater ratio reflects that the 'mutually beneficial' framing has become increasingly performative as evidence mounts of divergence rather than convergence. The Tangled Rope classification prevents false summits (the mountain perspective is correct that trade dynamics follow consistent patterns, but those patterns are contingent on institutional rules, not natural law) and prevents misdiagnosis as pure extraction (developing economies genuinely value trade access, suggesting Snare classification would be too extreme for the governmental level). The constraint is unstable: rising suppression and extraction will eventually make Rope (beneficiary perception) unsustainable, either forcing shift to Snare (reversion to autarky/revolutionary change) or triggering Scaffold dynamics (fair trade standards, labor rights enforcement, terms of trade improvement that sunset the asymmetry). The 30-year measurement trajectory (extractiveness 0.42→0.58) suggests the constraint is degrading toward extraction dominance — if extractiveness reaches 0.70+, mandatrophy resolution requires either demonstrating that coordination benefits increase proportionally or shifting classification toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convergence_vs_divergence_empirics,
    'Do trade-liberalized developing economies converge to advanced economy wage and productivity levels, or does structural subordination in global value chains prevent convergence?',
    'Long-term wage and productivity data for trade-liberalizing developing economies (1990-2025); comparison of convergence trajectories for East Asian NICs (higher autonomy) vs post-liberalization Latin America and SSA (lower autonomy); identification of structural barriers to value-chain upgrading',
    'If convergence: trade asymmetry is temporary coordination problem (Scaffold/Rope outcomes more likely). If divergence: asymmetry is structural extraction mechanism (Snare/Tangled Rope outcomes confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convergence_vs_divergence_empirics, empirical, 'Whether trade liberalization produces convergence or deepens subordination').

omega_variable(
    autonomous_development_feasibility,
    'Is alternative development pathway (local industrial base, regional trade, capital controls, import substitution) structurally feasible, or does global capital mobility make autonomy impossible?',
    'Analysis of China, Vietnam, India industrial development trajectories under capital controls; current feasibility of de-dollarization and alternative payment systems; simulation models of closed vs open economy development paths',
    'If feasible: exit options upgrade from trapped→constrained→mobile for developing economies; classification shifts toward Rope/Scaffold. If infeasible: trapped classification holds; asymmetry appears immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_development_feasibility, empirical, 'Whether autonomous development is structurally feasible').

omega_variable(
    labor_standards_certification_effectiveness,
    'Do fair trade certification and labor standards enforcement (SA8000, Better Work, etc.) actually reduce exploitation at meaningful scale, or are they performative theater that legitimizes extraction?',
    'Audit data: correlation between certification and actual wage/working condition improvements; worker testimony on enforcement effectiveness vs compliance avoidance; cost-benefit analysis of certification overhead vs extraction reduction',
    'If effective: Scaffold sunset is real, extraction mechanisms degradable. If performative: certification is theater masking continuation of snare; Piton classification gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_standards_certification_effectiveness, empirical, 'Whether labor standards certification reduces exploitation').

omega_variable(
    intellectual_property_regime_contingency,
    'Are IP regimes (WTO TRIPS) natural consequences of knowledge economics, or contingent political choices that could be reformed to enable developing economy technology access?',
    'Historical analysis of alternative IP regimes (compulsory licensing, technology transfer requirements, patent pools); comparison of pharmaceutical/vaccine outcomes under different IP regimes; modeling of generic drug access under reformed TRIPS',
    'If contingent: major structural parameter of trade asymmetry is reform-amenable; classification shifts toward Tangled Rope with clearer sunset. If natural law: IP subordination appears immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_property_regime_contingency, conceptual, 'Whether IP regimes are contingent political choices').

omega_variable(
    multinational_profit_extraction_quantification,
    'What proportion of trade-based value creation is captured by developing economy agents (workers, governments, domestic firms) vs extracted by multinationals and advanced economy shareholders?',
    'Value chain mapping for commodity exports (apparel, electronics, agriculture); measurement of value-added at each stage; profit/wage ratio analysis; transfer pricing investigation to quantify profit repatriation',
    'High extraction fraction (>60%) confirms Snare perspective for workers. Low extraction fraction suggests Rope/Tangled Rope misclassification. Quantification enables directionality precision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multinational_profit_extraction_quantification, empirical, 'What proportion of value is extracted vs retained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developing_economy_trade_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deta_tr_t0, developing_economy_trade_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(deta_tr_t15, developing_economy_trade_asymmetry, theater_ratio, 15, 0.62).
narrative_ontology:measurement(deta_tr_t30, developing_economy_trade_asymmetry, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(deta_be_t0, developing_economy_trade_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(deta_be_t15, developing_economy_trade_asymmetry, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(deta_be_t30, developing_economy_trade_asymmetry, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developing_economy_trade_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(developing_economy_trade_asymmetry, intellectual_property_enforcement).
narrative_ontology:affects_constraint(developing_economy_trade_asymmetry, capital_mobility_regime).
narrative_ontology:affects_constraint(developing_economy_trade_asymmetry, labor_standards_and_enforcement).
narrative_ontology:affects_constraint(developing_economy_trade_asymmetry, terms_of_trade_dynamics).

% DUAL FORMULATION NOTE:
% Developing economy trade asymmetry is composed of multiple structurally distinct constraints: intellectual property dependency (ε≈0.72, Snare-class), labor standards arbitrage (ε≈0.65, Tangled Rope), capital mobility extraction (ε≈0.68, Tangled Rope), and terms of trade deterioration (ε≈0.55, Tangled Rope). This story aggregates at the systemic level (ε≈0.58) but decomposes at the mechanism level. Each sub-constraint has its own sunset/persistence timeline and enforcement structure. Network links show this aggregate story's structural dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developing_economy_trade_asymmetry, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
