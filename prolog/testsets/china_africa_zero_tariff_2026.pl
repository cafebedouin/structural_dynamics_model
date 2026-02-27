% ============================================================================
% CONSTRAINT STORY: china_africa_zero_tariff_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_africa_zero_tariff_2026, []).

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
 *   constraint_id: china_africa_zero_tariff_2026
 *   human_readable: China-Africa Zero-Tariff Trade Framework
 *   domain: economic/trade_policy
 *
 * SUMMARY:
 *   The China-Africa Zero-Tariff Trade Framework represents a structural
 *   constraint combining genuine coordination benefits (market access,
 *   reduced transaction costs, consumer price reductions) with significant
 *   extraction mechanisms (fiscal revenue loss, industrial
 *   deindustrialization, debt leverage through infrastructure financing). The
 *   framework operates simultaneously as a rope for Chinese exporters, a
 *   snare for African domestic manufacturers, a tangled hybrid for African
 *   governments and the Chinese state, a piton for the neoliberal trade
 *   regime, and a scaffold for emerging African industrial policy
 *   alternatives. The theater ratio (0.58) reflects moderate performative
 *   content: the framework is justified through comparative advantage
 *   doctrine that produces observable aggregate gains (consumer prices, trade
 *   volume) while obscuring distributional extraction (who loses fiscal
 *   capacity, who loses industrial capability). The constraint's temporal
 *   trajectory shows extractiveness increasing from 0.28 to 0.52 over six
 *   years as tariff-revenue losses accumulate and deindustrialization
 *   dynamics deepen. Theater ratio rises from 0.35 to 0.58 as the discrepancy
 *   between neoliberal theory (free trade optimality) and observable outcomes
 *   (fiscal squeeze, job losses) widens, requiring increasingly performative
 *   justification.
 *
 * KEY AGENTS:
 *   - Chinese Manufacturing Exporters: Primary beneficiary (institutional/arbitrage) — gain unmediated market access; receive state subsidies; experience framework as pure coordination mechanism
 *   - African Domestic Manufacturers: Primary victim (powerless/trapped) — face elimination from own domestic markets; immobile due to scale and logistics barriers; bear maximum extraction from tariff asymmetry
 *   - African Government Treasuries: Secondary victim (moderate/constrained) — benefit from reduced consumer prices but lose tariff revenue; constrained by debt obligations to Chinese lenders (Belt-and-Road infrastructure financing creates framework compliance incentive)
 *   - Chinese State Apparatus: Institutional coordinator (powerful/mobile) — uses framework for geopolitical influence and industrial hierarchy; maintains negotiating flexibility; manages extraction through state capital and subsidy apparatus
 *   - Pan-African Trade Coalition: Organized resistance (organized/constrained) — can coordinate policy responses but lack exit option (cannot reject framework without isolation); experience constrained extraction
 *   - Neoliberal Trade Regime: Institutional supporter (institutional/arbitrage) — legitimizes framework through comparative advantage narrative; maintains performative verification (aggregate growth metrics obscure distribution)
 *   - African Industrial Policy Insurgency: Organized alternative (organized/constrained) — building domestic manufacturing pathways (Ethiopia, Rwanda, South Africa) as exit mechanisms; represent genuine scaffold with sunset potential
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent power asymmetry as immutable economic law; false summit detection reveals how naturalization functions as suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_africa_zero_tariff_2026, 0.52).
domain_priors:suppression_score(china_africa_zero_tariff_2026, 0.65).
domain_priors:theater_ratio(china_africa_zero_tariff_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_africa_zero_tariff_2026, tangled_rope).
narrative_ontology:human_readable(china_africa_zero_tariff_2026, "China-Africa Zero-Tariff Trade Framework").
narrative_ontology:topic_domain(china_africa_zero_tariff_2026, "economic/trade_policy").

domain_priors:requires_active_enforcement(china_africa_zero_tariff_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, chinese_manufacturing_exporters).
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, african_consumer_markets).
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, chinese_state_infrastructure_lenders).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, african_domestic_manufacturers).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, african_fiscal_revenues).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, african_industrial_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFRICAN DOMESTIC MANUFACTURER (SNARE) — Cannot exit the zero-tariff framework without losing market access; faces unmediated competition from subsidized Chinese manufactures. Small enterprises lack scale, logistics infrastructure, and state support to compete. Maximum experienced extraction — immobile, facing elimination from own domestic market.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AFRICAN GOVERNMENT TREASURY (TANGLED ROPE) — Benefits from reduced import costs for consumer goods and infrastructure components; faces reduced tariff revenue and loss of fiscal capacity for industrial policy. Constrained by debt obligations to Chinese lenders (infrastructure financing creates obligation to support trade framework). Mixed coordination-extraction: achieves consumer welfare gains but loses fiscal autonomy and development capacity.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHINESE MANUFACTURING EXPORTERS (ROPE) — Primary beneficiaries with full arbitrage exit (can redirect exports elsewhere; negotiate terms through state apparatus). Experience the framework as pure coordination mechanism — access to market that would otherwise require tariff navigation. Positive extraction for this agent; net subsidizer of the constraint through domestic Chinese state policy.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PAN-AFRICAN TRADE COALITION (TANGLED ROPE) — Organized resistance (African Union, ECOWAS, regional trade bodies) can negotiate framework terms but lack exit option (collective sanction would isolate Africa from trade entirely). Framework provides some coordination benefit (market access, reduced transaction costs) alongside extraction (lost policy autonomy, deindustrialization risk). Organized power and constrained exit produce moderate experienced extraction.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: NEOLIBERAL TRADE REGIME (PITON) — The broader institutional apparatus (WTO norms, structural adjustment doctrine, development finance conditionality) presents the zero-tariff framework as optimal policy and inevitable outcome. The regime's verification mechanism (comparative advantage theory, aggregate growth metrics) is substantially performative — high theater because actual outcomes (deindustrialization, fiscal squeeze, debt accumulation) contradict the theoretical narrative. The regime persists through institutional inertia and ideological investment, not functional verification of its claims.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINESE STATE APPARATUS (TANGLED ROPE) — Uses zero-tariff framework as tool for geopolitical influence and market expansion. Maintains exit optionality (can modulate framework, shift trade flows, or reorient development focus). Simultaneously constrained by WTO MFN obligations and need for stable supply chains. Framework achieves both coordination (regional market integration) and extraction (industrial hierarchy, debt leverage through infrastructure financing). Asymmetric power and mobile exit produce moderate effective extraction from Beijing's perspective — high agency in determining terms.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: AFRICAN INDUSTRIAL POLICY INSURGENCY (SCAFFOLD) — Organized movements (Ethiopia's manufacturing corridor, Rwanda's tech investment, South Africa's renewables policy) are building alternatives that bypass dependence on zero-tariff imports. These efforts are temporary scaffolding: they lack the scale and state backing of coordinated African industrial policy but represent pathways to exit extraction. Theater ratio for this perspective is lower — direct manufacturing investment has immediate functional verification (jobs, tax revenue, skills) rather than relying on comparative-advantage narrative.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / IMMUTABLE LOGIC VIEW (MOUNTAIN) — From a civilizational, universal perspective, zero-tariff trade is presented as an immutable law of optimal economics: comparative advantage dictates specialization; tariffs distort efficiency; free trade maximizes aggregate welfare. This perspective naturalizes the framework as a law of economics rather than a contingent policy choice. However, the structural data contradicts the mountain classification — the constraint clearly exhibits institutional enforcement, beneficiary/victim asymmetry, and contingent implementation. The mountain reading is a false summit revealing how economic naturalization functions as suppression mechanism.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_africa_zero_tariff_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_africa_zero_tariff_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_africa_zero_tariff_2026, TR),
    TR >= 0.70.

:- end_tests(china_africa_zero_tariff_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The framework's base extraction reflects significant asymmetry: Chinese state apparatus subsidizes exporters, African governments lose fiscal autonomy, African manufacturers face unmediated competition. The trajectory from 0.28 to 0.52 reflects accumulating extraction as tariff revenue losses compound and industrial deindustrialization deepens. Not extreme (0.70+) because meaningful coordination benefits exist (consumer access, transaction cost reduction), and alternative pathways (industrial policy insurgency) represent partial exits. Suppression (0.65): High. Multiple barriers prevent African exit: WTO MFN obligations eliminate selective tariff options; debt-to-China creates compliance incentives; diffuse global supply chains make domestic substitution difficult; neoliberal regime ideology naturalizes framework as inevitable. Theater ratio (0.58): Moderate-high. The framework is legitimized through comparative advantage theory that produces observable aggregate metrics (trade volume, consumer prices) while obscuring distribution (who loses fiscal capacity, whose jobs disappear). As outcomes diverge from theory, performative content rises — more narrative emphasis on long-term growth potential, consumer benefits, and regional stability to offset visible fiscal squeeze.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival variance. Chinese exporters and the state apparatus see pure coordination and asymmetric benefit (Rope/Tangled Rope) — market access they couldn't achieve unilaterally. African manufacturers see elimination (Snare) — they experience the constraint as structurally inescapable. African governments see mixed benefit-cost (Tangled Rope) — consumer welfare gains but fiscal/industrial losses. The neoliberal regime sees inevitable natural law (Mountain) — free trade as optimization principle. The African industrial policy insurgency sees temporary problem with exit (Scaffold) — manufacturing investment is building alternative pathways. The pan-African coalition sees organized but constrained extraction (Tangled Rope) — significant agency in policy negotiation but no realistic exit from framework without isolation. This perspectival divergence is the core diagnostic: it reveals that a single 'free trade' framework operates as coordination for the powerful, extraction for the powerless, and mixed hybrid for middling institutional actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) ranges from 0.05 (Chinese exporters: full beneficiary + arbitrage) to 0.95 (African manufacturers: full victim + trapped). The engine computes d from beneficiary/victim status and exit options per the sigmoid derivation chain. Chinese exporters: beneficiary status + arbitrage exit = low d (≈0.05) → negative f(d) → negative effective extraction (they benefit from the constraint). African manufacturers: victim status + trapped exit = high d (≈0.95) → high f(d) (≈1.42) → maximum experienced extraction. African governments: victim status (fiscal loss) + beneficiary status (consumer gains) simultaneously, constrained exit → mid-range d (≈0.55-0.65) → moderate f(d) (≈0.75-1.00) → moderate experienced extraction. The analytical observer at civilizational scope sees d ≈ 0.72 (analytical positioning) → f(d) ≈ 1.15 → analytical-level extraction magnitude, but mountain classification attempt is false summit (constraint is clearly institutional/policy, not natural law).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine coordination (rope-like access for Chinese exporters, some consumer benefit for African populations) from embedded extraction (fiscal loss, deindustrialization, debt leverage). The tangled rope classification applies to: (1) Chinese state apparatus (coordination function: regional market integration + extraction function: industrial hierarchy and geopolitical leverage), (2) African governments (coordination: consumer access + extraction: fiscal/industrial capacity loss + debt constraint), (3) Pan-African coalition (coordination: negotiation framework + extraction: constrained outcomes). The framework is NOT a pure rope because structural enforcement is required (debt coercion, WTO MFN lock-in, neoliberal regime legitimization) and asymmetric benefits flow away from African domestic producers. It is NOT pure snare because measurable coordination benefits exist (consumer prices, transaction costs, trade volume) and organized agents (AU, ECOWAS) maintain some negotiation agency. The tangled rope classification is mandatrophy-resolving because it captures that the same institutional arrangement simultaneously coordinates access and extracts capacity — the moral/political question (is this fair exchange?) is separated from the structural question (does this mechanism exhibit both coordination and extraction?). The answer to both is yes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chinese_debt_leverage_binding,
    'To what extent does Chinese infrastructure financing create binding obligation for African governments to maintain zero-tariff compliance?',
    'Analysis of loan covenants, debt-restructuring negotiations, and instances where African countries attempted policy deviation despite Chinese lending exposure. Historical comparison with IMF structural adjustment leverage.',
    'If binding: framework operates as debt-snare hybrid (extraction mechanism is financial coercion, not tariff mechanics). If loose: framework is more rope-like (coordination benefit dominates extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_debt_leverage_binding, empirical, 'Strength of debt coercion mechanism in framework enforcement').

omega_variable(
    african_industrial_policy_alternative_viability,
    'Can African nations build domestic manufacturing capacity (textiles, consumer goods, simple electronics) that can compete with zero-tariff Chinese imports within a 15-year horizon?',
    'Tracking of manufacturing employment, value-added production, export competitiveness, and skill development in early-mover African industrial policy jurisdictions (Ethiopia, Rwanda, Kenya). Comparative analysis with East Asian industrialization timelines and support infrastructure.',
    'If viable: scaffold perspective is real structural feature — industrial policy insurgency is genuine exit path. If not viable: framework is durable snare for African manufacturers with no realistic alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(african_industrial_policy_alternative_viability, empirical, 'Viability of African domestic manufacturing alternatives to Chinese imports').

omega_variable(
    consumer_welfare_gains_genuine,
    'Do aggregate consumer welfare gains from reduced import prices in Africa actually exceed lost fiscal revenue and foregone industrial development value?',
    'Comprehensive accounting: comparison of consumer price reductions vs. lost tariff revenue, public health/education spending reductions, and long-term income effects from deindustrialization. Temporal welfare accounting over 20-30 year horizon.',
    'If genuine net gain: framework justifies moderate tangled_rope classification from African government perspective. If false: extraction mechanism is masked by consumption gains in short term while long-term industrial atrophy creates multi-generational extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_welfare_gains_genuine, empirical, 'Whether consumer welfare gains genuinely exceed lost fiscal and industrial capacity').

omega_variable(
    chinese_state_subsidy_transparency,
    'What proportion of competitive advantage for Chinese manufactures derives from explicit state subsidies (SOE financing, export credits, industrial policy) vs. legitimate productivity/cost advantage?',
    'Forensic analysis of Chinese export finance mechanisms, SOE cost structure, value-chain subsidies, and comparative factor productivity studies. Contrast with African manufacturing cost structures adjusted for subsidy equivalents.',
    'If heavily subsidized: framework extraction is asymmetric state power (China''s institutional resources create unmediated advantage). If productivity-based: framework approaches legitimate comparative advantage coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_state_subsidy_transparency, empirical, 'Extent of Chinese state subsidies embedded in export competitiveness').

omega_variable(
    wto_mfn_enforcement_constraint,
    'Could African nations negotiate selective tariff exemptions (under WTO safeguard clauses, infant-industry exceptions) without triggering MFN retaliation from China or other major trading partners?',
    'Legal analysis of WTO dispute mechanisms, precedent cases (India/Pakistan infant-industry claims, SAPTA safeguards). Modeling of negotiation dynamics if Africa attempted coordinated safeguard claim.',
    'If enforceable exemptions exist: African negotiating position is stronger (exit via WTO rules is possible). If MFN enforcement is tight: African nations are structurally trapped (any selective exemption triggers retaliation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wto_mfn_enforcement_constraint, empirical, 'Feasibility of WTO-compliant African tariff exemptions without retaliation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_africa_zero_tariff_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cazt_theater_t0, china_africa_zero_tariff_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cazt_theater_t3, china_africa_zero_tariff_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cazt_theater_t6, china_africa_zero_tariff_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(cazt_ext_t0, china_africa_zero_tariff_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cazt_ext_t3, china_africa_zero_tariff_2026, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cazt_ext_t6, china_africa_zero_tariff_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_africa_zero_tariff_2026, resource_allocation).
narrative_ontology:affects_constraint(china_africa_zero_tariff_2026, african_fiscal_squeeze_constraint).
narrative_ontology:affects_constraint(china_africa_zero_tariff_2026, chinese_overcapacity_offloading).
narrative_ontology:affects_constraint(china_africa_zero_tariff_2026, belt_and_road_debt_leverage).

% DUAL FORMULATION NOTE:
% The zero-tariff framework decomposes into three structurally distinct constraints: (1) tariff mechanics (the trade policy itself, ε≈0.35, primarily coordination with consumer benefits), (2) fiscal extraction (revenue loss mechanism, ε≈0.55, primarily extraction from treasuries), and (3) debt leverage (Belt-and-Road financing as framework enforcement tool, ε≈0.62, primarily institutional coercion). These are linked causally — the tariff framework creates fiscal gaps that borrowing fills, and borrowing creates compliance incentives — but have distinct ε values reflecting different empirical status. The aggregate story (this JSON) uses ε=0.52 as a flux estimate. Full precision requires separate stories for each mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_africa_zero_tariff_2026, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
