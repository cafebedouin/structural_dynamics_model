% ============================================================================
% CONSTRAINT STORY: us_manufacturing_tariff_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_manufacturing_tariff_regime, []).

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
 *   constraint_id: us_manufacturing_tariff_regime
 *   human_readable: US Manufacturing Tariff Regime
 *   domain: economic_policy/trade
 *
 * SUMMARY:
 *   The US manufacturing tariff regime is a hybrid coordination-extraction
 *   constraint operating between domestic protected manufacturers
 *   (beneficiaries), consumers and downstream manufacturers (victims), and
 *   trading partner economies (victims). The regime exhibits genuine
 *   industrial policy coordination (maintaining domestic manufacturing
 *   capacity and negotiating leverage) alongside extraction (price floors,
 *   input cost inflation, lost consumer surplus). Extractiveness has
 *   increased from 0.35 to 0.58 over the measurement interval as tariff
 *   schedules have expanded and enforcement has intensified. Theater ratio
 *   has increased from 0.52 to 0.68 as security justifications (national
 *   security, critical supply chains) have been invoked with increasing
 *   frequency to justify protectionist schedules. The constraint is
 *   characterized by: (1) Active enforcement through Customs and Border
 *   Protection; (2) Multiple institutional actors (US government, protected
 *   manufacturers, downstream industries, trading partners) with conflicting
 *   structural positions; (3) Retaliatory dynamics that create cascading
 *   suppression; (4) An aspirational free trade alternative (bilateral
 *   agreements, sectoral liberalization) that suggests Scaffold logic at the
 *   organized coalition level; (5) A false summit risk where comparative
 *   advantage is naturalized as economic law rather than understood as a
 *   contestable theoretical framework dependent on assumptions about capital
 *   mobility and adjustment costs.
 *
 * KEY AGENTS:
 *   - Protected Domestic Manufacturers: Primary beneficiary (institutional/arbitrage) — Steel, automotive, agricultural sectors protected by tariff schedules; benefit from price floors and reduced import competition; can arbitrage tariff complexity
 *   - Wage-Earning Consumers: Primary victim (powerless/trapped) — Bear tariff costs through higher prices on imported goods, clothing, electronics; no viable exit; trapped by geography and income constraints
 *   - Downstream Manufacturers: Secondary victim (moderate/constrained) — Automotive parts suppliers, apparel manufacturers, electronics assemblers depending on imported inputs; face cost increases and capital relocation barriers
 *   - US Government: Institutional actor (institutional/constrained) — Extracts tariff revenue; coordinates industrial policy; constrained by WTO obligations, retaliation, and political economy of protected regions
 *   - Trading Partner Economies: Secondary victim (moderate/trapped) — Face tariff barriers on exports; subject to retaliation risk; trapped in bilateral negotiation asymmetry
 *   - Free Trade Coalition: Organized agents (organized/mobile) — Import-dependent industry groups, consumer advocates, academic economists; pushing alternative arrangements with sunset vision
 *   - Multinational Corporations: Powerful actor (powerful/mobile) — Can arbitrage tariff regimes through supply chain optimization and transfer pricing; experience mixed coordination benefit and constraint
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — Risks naturalizing comparative advantage theory as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_manufacturing_tariff_regime, 0.58).
domain_priors:suppression_score(us_manufacturing_tariff_regime, 0.65).
domain_priors:theater_ratio(us_manufacturing_tariff_regime, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_manufacturing_tariff_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_manufacturing_tariff_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_manufacturing_tariff_regime, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_manufacturing_tariff_regime, tangled_rope).
narrative_ontology:human_readable(us_manufacturing_tariff_regime, "US Manufacturing Tariff Regime").
narrative_ontology:topic_domain(us_manufacturing_tariff_regime, "economic_policy/trade").

domain_priors:requires_active_enforcement(us_manufacturing_tariff_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_manufacturing_tariff_regime, protected_domestic_manufacturers).
narrative_ontology:constraint_beneficiary(us_manufacturing_tariff_regime, tariff_revenue_government).
narrative_ontology:constraint_victim(us_manufacturing_tariff_regime, import_dependent_consumers).
narrative_ontology:constraint_victim(us_manufacturing_tariff_regime, downstream_manufacturers).
narrative_ontology:constraint_victim(us_manufacturing_tariff_regime, trading_partner_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-EARNING CONSUMER (SNARE) — Bears tariff costs through higher prices on imported goods and downstream products with no viable exit. Cannot arbitrage or relocate consumption patterns at scale. Suppression is structural: price floors created by tariff enforcement, no accessible substitute supply chains. Maximum extraction experienced.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOWNSTREAM MANUFACTURER (SNARE) — Depends on imported intermediate goods and raw materials; tariffs increase input costs and production overhead. Exit requires relocation to tariff-advantaged jurisdictions (constrained by capital, workforce retraining, supply chain reestablishment). No coordination benefit — constraint is pure extraction from this position.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTECTED MANUFACTURER (ROPE) — Benefits from tariff-created price floor and reduced import competition. Experiences the constraint as coordination: tariff regime enforces an orderly market that enables pricing power and capital investment certainty. Net beneficiary. Can arbitrage: relocate production, adjust supply chains, leverage tariff schedule complexity. Low experienced extraction.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATION (TANGLED ROPE) — Can arbitrage tariff regimes through transfer pricing, offshoring, and global supply chain optimization. Also benefits from tariff protection in US market segments. Extraction mixed with coordination benefit — the regime constrains but also creates profitable market structures. Mobile exit options but leveraging tariff complexity.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: US GOVERNMENT (TANGLED ROPE) — Extracts tariff revenue and coordinates industrial policy through tariff schedules (coordination function). But constrained by: WTO obligations, retaliatory tariffs from trading partners, domestic political economy requiring constant tariff renegotiation. Extraction from tariff revenue; coordination from industrial policy leverage. Suppression comes from retaliation cycles and institutional rigidity.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADING PARTNER ECONOMY (SNARE) — Faces US tariff barriers on export goods; retaliatory tariffs trigger counter-sanctions; trapped in bilateral negotiation asymmetry favoring the larger US market. No exit except through costly trade disputes or market substitution. Suppression via US market access dependence.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 7: WTO TRADING SYSTEM (PITON) — Tariff regimes nominally constrained by WTO rules, but enforcement is weak and member states routinely circumvent via exceptions (national security, infant industry, etc.). The WTO architecture persists through institutional inertia despite limited enforcement capacity. Theater ratio high: tariff justifications invoke WTO compliance while actual behavior violates GATT principles. System has degraded from original coordination function.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: FREE TRADE COALITION (SCAFFOLD) — Organized non-state actors (import-dependent industry groups, consumer advocates, academic economists) pushing alternative arrangements (bilateral free trade agreements, sectoral liberalization, supply chain diversification). See tariff regime as temporary barrier to eventual liberalization. Has exit vision: progressive tariff reduction, global supply chain reintegration. Sunset logic applies: trade agreements can unwind tariffs over negotiated timescales.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, tariff regimes contradict comparative advantage and produce aggregate welfare loss (Ricardian economics). The argument: tariffs are inherently distortionary, creating deadweight loss that exceeds any localized benefit. However, this perspective naturalizes a contestable economic theory (comparative advantage assumes perfect capital mobility, perfect information, zero adjustment costs) as a law of nature. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_manufacturing_tariff_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_manufacturing_tariff_regime, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_manufacturing_tariff_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_manufacturing_tariff_regime, TR),
    TR >= 0.70.

:- end_tests(us_manufacturing_tariff_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increased from 0.35 over interval. The regime extracts from consumers and downstream manufacturers through price floors and input cost inflation. But extraction is not maximal because: (1) protected manufacturers genuinely benefit from orderly markets (coordination value exists), (2) some downstream manufacturers can absorb costs through productivity improvements, (3) some consumers can arbitrage through imports or substitutes (though limited). The increase over time reflects expanded tariff schedules and tightened enforcement. Suppression (0.65): Moderate-high. Consumers face structural price floors with no exit; downstream manufacturers face supply chain lock-in; trading partners face market access dependence. Suppression is not maximal because tariff schedules have some transparency (published rates, predictable implementation) and political contestability (lobby pressure, electoral consequences). Theater ratio (0.68): Moderate-high. National security and infant industry justifications are invoked to legitimize protectionist tariff schedules whose economic rationale is general protectionism. The performative element increased over the interval as security framing expanded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The protected manufacturer sees coordination (Rope) — the tariff regime solves the problem of market ordering and capital investment certainty. The consumer sees pure extraction (Snare) — they bear costs with no benefit and no exit. The downstream manufacturer sees mixed extraction-coordination (Tangled Rope) — they both benefit from supplier stability and suffer from input cost increases. The trading partner sees extraction (Snare) — they are trapped in bilateral asymmetry. The government sees mixed revenue extraction and industrial policy coordination (Tangled Rope) — they extract tariff revenue and coordinate strategic capacity. The free trade coalition sees a temporary barrier with a sunset (Scaffold) — progressive liberalization is the identified exit path. The WTO system sees its own degraded function (Piton) — tariff regimes nominally constrained by WTO rules but routinely circumvented. The analytical observer risks seeing natural economic law (Mountain) but this is a false summit: the regime is a contingent institutional arrangement, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent derives from their power level, exit options, and relationship to the extraction flow. Protected manufacturers (institutional/arbitrage) experience low d: they are beneficiaries with exit capacity (can relocate, leverage tariff complexity). Consumers (powerless/trapped) experience high d: they bear costs with no exit. Downstream manufacturers (moderate/constrained) experience intermediate d: they bear costs but can exit at capital cost. Trading partners (moderate/trapped) experience high d: structurally dependent on US market access. Government (institutional/constrained) experiences moderate d: extracts revenue but constrained by WTO obligations and retaliation. The analytical observer (analytical/analytical) risks low d through false naturalization: if comparative advantage is seen as natural law, the observer positions themselves as outside the constraint's structure, failing to see how tariff regime distributes power asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the tariff regime is genuinely a hybrid coordination-extraction system, not a misclassification. The coordination function is real: the regime does maintain domestic manufacturing capacity, does create stable investment environments for protected sectors, and does provide negotiating leverage on geopolitical supply chains. The extraction is equally real: consumers bear definite costs without compensation, downstream manufacturers face input cost inflation, and trading partners bear bilateral asymmetry. Both functions coexist. The classification as Tangled Rope is robust because: (1) Beneficiaries exist (protected manufacturers) who benefit from genuine coordination (market ordering), (2) Victims exist (consumers, downstream manufacturers) who bear extraction costs, (3) Active enforcement is required to maintain tariff schedules against liberalization pressure, (4) Suppression is structural (price floors, market access barriers), (5) Theater is moderate-high (security justifications expand). The constraint is neither pure coordination (Rope) nor pure extraction (Snare) — it is an intentionally mixed system where protected sectors are privileged to maintain strategic capacity at the cost of broad consumer and downstream manufacturer extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infant_industry_legitimacy,
    'Do tariff protections for emerging industries actually enable industrial development and eventual competitiveness, or do they create permanent dependency and rent-seeking?',
    'Historical analysis of tariff-protected industries: track whether protected sectors achieved sustained competitiveness after tariff removal (South Korea steel, semiconductor infant industry protection); compare to sectors that never achieved competitiveness despite long protection (certain agricultural sectors)',
    'If protection enables competitiveness: tariff regime has genuine coordination function (Rope or Tangled Rope at industry level). If protection creates dependency: regime is extraction mechanism with theater (Snare or Piton). Classification shifts substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_industry_legitimacy, empirical, 'Whether infant industry protection enables eventual competitiveness').

omega_variable(
    national_security_scope_creep,
    'How much of current tariff enforcement is justified by genuine national security concerns (energy, semiconductors, rare earth metals) versus pretext for general protectionism?',
    'Distinguish tariff schedule items by: (a) dual-use technology classification, (b) supply chain concentration metrics, (c) geopolitical dependency risk assessments. Compare security-justified tariff rates to general manufacturing tariff rates.',
    'If genuine security concern: tariff regime has legitimate coordination function for strategic industrial capacity. If mostly pretext: theater_ratio should be higher (~0.80+), regime reclassifies toward Piton or Snare. Affects suppression characterization and victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_scope_creep, empirical, 'Extent of security justification versus protectionist pretext in tariff enforcement').

omega_variable(
    retaliation_asymmetry,
    'Does US market size and diversification create structural asymmetry in tariff retaliation capacity, where trading partners cannot match US tariff impact?',
    'Quantitative analysis of retaliatory tariff impact: measure price inflation in US imports vs equivalent tariff rate applied by trading partner; assess relative welfare loss as percentage of GDP',
    'If asymmetry confirmed: US government has institutional power advantage, trading partners trapped (higher d for trading partner victims). If symmetric: trading partners have more exit capacity. Affects whether trading partner perspective is Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retaliation_asymmetry, empirical, 'Whether US market size creates asymmetric retaliation capacity').

omega_variable(
    tariff_incidence_empirical,
    'What is the empirical incidence of tariffs: do protected manufacturers pass through costs to consumers, absorb in margins, or invest in productivity?',
    'Econometric analysis of producer and consumer price indices by tariff schedule; measure cost pass-through rates; compare productivity growth in protected vs unprotected sectors',
    'If high pass-through: consumer extraction is high (suppression gate confirmed). If manufacturers absorb: distributes costs toward capital holders. If productivity investment: coordination benefit emerges. Affects whether extractiveness should be 0.58 or lower/higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tariff_incidence_empirical, empirical, 'Empirical incidence of tariff costs across manufacturer, consumer, and productivity').

omega_variable(
    supply_chain_lock_in,
    'Do downstream manufacturers face lock-in costs (switching suppliers, tooling, inventory adjustments) that exceed exit costs of relocation, making tariff-driven cost increases effectively irreversible?',
    'Case studies of downstream manufacturers responding to tariff changes: measure time and capital required to source alternative inputs, relocate production, or accept tariff cost burden',
    'If lock-in is high: exit_options should be ''trapped'' not ''constrained''; suppression higher. Downstream manufacturers move from Snare(constrained) to Snare(trapped), increasing experienced extraction. If low: exit is genuinely available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_lock_in, empirical, 'Supply chain lock-in costs relative to relocation costs').

omega_variable(
    comparative_institutional_advantage,
    'Does US political system''s protectionist bias (geographically concentrated manufacturing lobbies in swing states) represent a genuine institutional constraint on liberalization, or a contingent policy choice?',
    'Political economy analysis: track tariff schedule changes under administrations with different trade philosophies; assess whether tariff patterns correlate with electoral college significance of manufacturing regions',
    'If institutional constraint: regime is more rigid (higher suppression). If contingent choice: policy is reversible faster (lower suppression, higher exit options for government actor). Affects government''s exit classification and scenic scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_institutional_advantage, conceptual, 'Whether protectionist bias is structural institutional constraint or contingent policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_manufacturing_tariff_regime, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_tr_t0, us_manufacturing_tariff_regime, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tariff_tr_t10, us_manufacturing_tariff_regime, theater_ratio, 10, 0.62).
narrative_ontology:measurement(tariff_tr_t20, us_manufacturing_tariff_regime, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(tariff_be_t0, us_manufacturing_tariff_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tariff_be_t10, us_manufacturing_tariff_regime, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(tariff_be_t20, us_manufacturing_tariff_regime, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_manufacturing_tariff_regime, resource_allocation).
narrative_ontology:affects_constraint(us_manufacturing_tariff_regime, supply_chain_concentration).
narrative_ontology:affects_constraint(us_manufacturing_tariff_regime, semiconductor_manufacturing_capacity).
narrative_ontology:affects_constraint(us_manufacturing_tariff_regime, agricultural_subsidy_regime).
narrative_ontology:affects_constraint(us_manufacturing_tariff_regime, retaliatory_tariff_cycles).

% DUAL FORMULATION NOTE:
% The US tariff regime decomposes into multiple structurally distinct constraints: sector-specific tariff schedules (steel, automotive, agriculture) each have different ε values and different beneficiary/victim structures. National security-justified tariffs (semiconductors, rare earth metals) have different suppression mechanisms than general manufacturing tariffs. The regime-level story captures the aggregate structure; sector-level stories would reveal that protection is selectively distributed rather than uniform. Link upstream to specific supply chain constraints (semiconductor capacity, rare earth dependency) that the tariff regime claims to protect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_manufacturing_tariff_regime, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
