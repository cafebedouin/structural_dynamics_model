% ============================================================================
% CONSTRAINT STORY: eu_mercosur_trade_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_mercosur_trade_agreement, []).

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
 *   constraint_id: eu_mercosur_trade_agreement
 *   human_readable: EU-Mercosur Free Trade Agreement
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU-Mercosur Free Trade Agreement represents a complex institutional
 *   arrangement combining genuine coordination benefits (supply chain
 *   integration, rules harmonization, reduced transaction costs) with
 *   systematic extraction mechanisms targeting small-scale farmers,
 *   industrial workers, and environmental commons. Negotiations began in 1999
 *   and reached provisional agreement in 2019, but ratification remains
 *   blocked due to divergent concerns: European environmental coalitions
 *   oppose deforestation linkage to beef exports; Mercosur countries demand
 *   agricultural access reciprocity; small farmers and unions in both blocs
 *   view the agreement as asymmetric. The constraint exhibits all six
 *   classification types across different structural positions, making it a
 *   diagnostic exemplar for how free trade agreements simultaneously create
 *   coordination and enable extraction. The key structural tension: tariff
 *   harmonization solves legitimate coordination problems (supply chain
 *   efficiency, regulatory transaction costs) while simultaneously removing
 *   the policy tools (agricultural tariffs, environmental regulations) that
 *   protect smaller, less-capitalized actors from competition with
 *   large-scale agribusiness and multinational corporations. The theater
 *   ratio (0.68) reflects that labor and environmental provisions are largely
 *   performative — they are negotiated with great fanfare but embedded within
 *   a WTO framework where enforcement mechanisms are weak and trade rules
 *   systematically dominate.
 *
 * KEY AGENTS:
 *   - European Small-Scale Farmers: Primary victims (powerless/trapped) — face price competition from Mercosur agricultural exports without reciprocal access to Mercosur markets; exit only through farm abandonment
 *   - Mercosur Industrial Workers: Primary victims (moderate/constrained) — manufacturing sectors face automation and capital flight pressure; constrained exit in rural regions
 *   - Amazon Ecosystem and Climate Systems: Victim (no agency/trapped) — agreement incentivizes deforestation through beef export market expansion; systematic environmental degradation with no enforcement mechanism
 *   - European Manufacturing Exporters: Primary beneficiary (institutional/arbitrage) — gain market access for high-value exports; can relocate supply chains; experience constraint as coordination
 *   - Mercosur Agricultural Exporters: Primary beneficiary (institutional/arbitrage) — gain EU market access for beef, sugar, citrus, ethanol; can shift production across regional borders
 *   - Multinational Corporations: Secondary beneficiary (powerful/arbitrage) — enable supply chain restructuring; benefit from harmonized regulatory standards; lock in intellectual property regimes
 *   - Environmental and Climate Coalitions: Organized victim (organized/constrained) — attempt to enforce environmental standards but lack enforcement mechanisms; constrained by agreement structure that subordinates environmental rules to trade rules
 *   - Progressive Political Coalitions: Organized advocate (organized/constrained) — view agreement as temporary and seek to embed labor/environmental enforcement, but constrained by weak side-agreement mechanisms
 *   - WTO System: Institutional context (institutional/constrained) — MFN principle requires uniform treatment, making negotiated asymmetry performative rather than enforceable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_mercosur_trade_agreement, 0.58).
domain_priors:suppression_score(eu_mercosur_trade_agreement, 0.62).
domain_priors:theater_ratio(eu_mercosur_trade_agreement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_mercosur_trade_agreement, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_mercosur_trade_agreement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_mercosur_trade_agreement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_mercosur_trade_agreement, tangled_rope).
narrative_ontology:human_readable(eu_mercosur_trade_agreement, "EU-Mercosur Free Trade Agreement").
narrative_ontology:topic_domain(eu_mercosur_trade_agreement, "economic/political").

domain_priors:requires_active_enforcement(eu_mercosur_trade_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, european_manufacturing_exporters).
narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, mercosur_agricultural_exporters).
narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, multinational_corporations).
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, european_small_farmers).
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, mercosur_industrial_workers).
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, amazon_ecosystem).
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, eu_environmental_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EUROPEAN SMALL FARMERS (SNARE) — Trapped within the EU's trade policy regime. Competition from large-scale Mercosur agricultural exports (especially beef, sugar, citrus) undercuts domestic pricing without reciprocal market access. No exit option except farm abandonment. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MERCOSUR INDUSTRIAL WORKERS (SNARE) — Manufacturing sectors (automotive, machinery) face automation and offshoring pressure as trade agreement enables capital mobility. Exit constrained by lack of alternative employment in rural areas. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ENVIRONMENTAL COALITIONS (TANGLED ROPE) — Organized but constrained by treaty enforcement mechanisms that subordinate environmental standards to trade liberalization. The agreement creates incentive structure favoring Amazon deforestation (cattle ranching, soy) over conservation. Coordination function (integrating climate action into trade): weak. Extraction function (rules constrain regulatory autonomy): strong. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN MANUFACTURING EXPORTERS (ROPE) — Institutional beneficiary with arbitrage exit (can shift supply chains). Mercosur agreement reduces tariffs on high-value exports (machinery, chemicals, autos). Experiences the constraint as coordination: unified trade rules reduce transaction costs and enable supply chain integration. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MERCOSUR AGRICULTURAL EXPORTERS (ROPE) — Institutional beneficiary. Agreement removes EU tariffs on beef, sugar, citrus, ethanol, opening markets worth billions. Experiences constraint as coordination of market access rules. d≈0.10, f(d)≈-0.07, σ=1.1 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: MULTINATIONAL CORPORATIONS (TANGLED ROPE) — Powerful actors with arbitrage exit. The agreement enables capital mobility and supply chain restructuring (win: coordination). It also locks both blocs into regulatory harmonization toward corporate-friendly standards, extracting from smaller competitors and locking in intellectual property regimes. d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.23. Low extraction but genuine coordination + asymmetric benefit.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PROGRESSIVE POLITICAL COALITIONS (SCAFFOLD) — Organized opposition viewing the agreement as temporary: labor, environmental, and small-farmer movements frame it as a transitional regime that will be superseded by climate-aligned trade rules. Conditional ratification with enforcement mechanisms for labor/environmental standards represents scaffold logic: coordination with sunset tied to climate compliance. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.52. Moderate extraction because mechanism to enforce sunset exists but is weak.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: WTO REGIME (PITON) — The EU-Mercosur agreement operates within WTO MFN constraints (most-favored-nation), which require uniform tariff treatment across trading partners. This creates theater: the agreement's negotiated complexity (labor chapters, environmental side agreements) is performative within WTO framework because any real asymmetry must be open to all WTO members. theater_ratio=0.68 reflects that negotiated enforcement mechanisms are largely aspirational — actual WTO litigation proceeds via dispute settlement, not via agreement-specific enforcement. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.32.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global scope, the agreement is a hybrid institutional structure with genuine coordination function (integrated supply chains, reduced transaction costs, rules of origin harmonization) AND genuine extraction mechanics (environmental deregulation, labor standard suppression, intellectual property lock-in). The suppression (0.62) reflects that exit alternatives (bilateral deals, regional integration, carbon tariffs) are being foreclosed by the agreement's structural scope. d≈0.70, f(d)≈1.10, σ=1.2 → χ≈0.77.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_mercosur_trade_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_mercosur_trade_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_mercosur_trade_agreement, TR),
    TR >= 0.70.

:- end_tests(eu_mercosur_trade_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The agreement creates systematic extraction mechanisms: (1) tariff elimination on Mercosur agricultural exports undercuts EU small farmers without reciprocal manufacturing market access; (2) trade rules subordinate environmental standards, enabling deforestation externality; (3) capital mobility enables wage suppression in Mercosur manufacturing through automation threat; (4) intellectual property harmonization locks both regions into corporate-favorable regimes. However, extraction is not total (χ < 0.66) because multinational corporations and large institutional exporters also experience genuine coordination benefits, and exit mechanisms exist for powerful actors (arbitrage). The extractiveness measurement trajectory (0.32 → 0.58 over 20 years) shows accumulation as initial market access gains fade and long-term structural effects (deforestation acceleration, farm consolidation, industrial displacement) compound. Suppression (0.62): Moderate-high. Significant barriers prevent exit: small farmers cannot relocate production; Mercosur workers lack alternative employment; environmental constituencies cannot opt out of trade rule supremacy over environmental law. The WTO MFN principle constrains policy autonomy even for countries not party to the agreement. But suppression is not total — political coalitions can organize side-agreement enforcement, and countries retain some regulatory autonomy within WTO bounds. Theater ratio (0.68): Moderate-high. Labor and environmental chapters perform legitimacy without strong enforcement: dispute resolution mechanisms exist but operate slowly and with low remedies; side agreements are subordinate to trade rules; WTO dispute settlement proceeds separately from agreement-specific enforcement, creating performative dualism where negotiated commitments appear binding but actual enforcement relies on trade retaliation (which is economically costly for smaller actors).
 *
 * PERSPECTIVAL GAP:
 *   European small farmers see pure extraction (Snare) — trapped by agricultural import competition with no reciprocal manufacturing market, no exit option except farm abandonment, no mechanism to enforce agricultural protections within the agreement structure. Mercosur industrial workers see Snare for similar reasons — capital mobility threatens jobs, wage suppression follows from trade liberalization without labor standard enforcement. Environmental coalitions see Tangled Rope — genuine coordination function (integrated climate accounting, harmonized standards) embedded within extraction mechanism (rules subordinating environmental standards to trade rules). European manufacturers see Rope (pure coordination) — tariff elimination solves legitimate supply chain coordination problems; they experience no extraction because they have arbitrage exit and market access benefits. Mercosur agricultural exporters see Rope — genuine coordination benefit from unified market access and harmonized rules of origin. Multinational corporations see Tangled Rope with low extraction — they benefit from supply chain coordination and regulatory harmonization but also face constraints from labor organizing and environmental enforcement. Progressive coalitions see Scaffold — viewing the agreement as temporary, embeddable with stronger labor/environmental enforcement, supersedable by climate-aligned trade rules. The WTO system sees Piton — MFN principle constrains asymmetric enforcement, making labor/environmental chapters performative rather than binding; the regime persists through inertia despite weak functional verification of commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   European small farmers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction victim. Mercosur industrial workers: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction. Environmental coalitions: Victim + constrained (organized but constrained by agreement structure) → d≈0.72, f(d)≈1.15. Significant extraction despite organized status. European manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Mercosur agricultural exporters: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.07. Net beneficiary. Multinational corporations: Mixed (beneficiary + arbitrage but constrained by labor organizing) → d≈0.35, f(d)≈0.33. Low extraction beneficiary. Progressive coalitions: Victim + constrained (organized but constrained by weak enforcement) → d≈0.50, f(d)≈0.65. Moderate extraction despite organized status. WTO system: Institutional + constrained (unable to differentiate asymmetric commitments from MFN principle) → d≈0.40, f(d)≈0.40. Piton classification from theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the genuinely coordination-productive elements (supply chain integration, rules harmonization, transaction cost reduction) from the extraction-productive elements (environmental deregulation, labor standard suppression, capital mobility enabling). The agreement is NOT pure extraction disguised as coordination (Snare misclassification). It IS a hybrid where both functions operate simultaneously and structurally reinforce each other. The Tangled Rope classification reflects this: the coordination function is real (benefits flow to large agricultural exporters and manufacturers), AND the extraction function is real (costs flow to small farmers, workers, and environmental commons). The confusion arises from false natural law framing ('trade liberalization is inherent efficiency that benefits all participants') — the engine's analytical perspective catches this by showing that extractiveness > 0.46 (Tangled Rope minimum). The extraction would be preventable if the agreement embedded binding environmental/labor enforcement with trade retaliation capacity (moving toward Rope or Scaffold). The presence of performative labor/environmental chapters without enforcement mechanism is the theater signal (ratio 0.68). If chapters were enforceable, theater would drop to 0.35-0.45 and the constraint would remain Tangled Rope but with reduced χ from both the victims' perspective (higher exit options through enforcement) and the institutional beneficiaries' perspective (constrained arbitrage). If chapters were strengthened with climate conditionality and automatic sunset if emissions targets unmet, the constraint would shift toward Scaffold (χ ≤ 0.30 from progressive coalition perspective; sunset mechanism visible).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    environmental_standard_supremacy,
    'Will trade liberalization rules systematically override environmental/climate protections, or can environmental standards be enforced as valid constraints on trade?',
    'Historical analysis of WTO dispute cases involving environmental measures; implementation pattern of side agreements in prior trade deals (USMCA, CPTPP); legal status of climate commitments within agreement text',
    'If trade rules dominate: constraint classifies as Snare from environmental perspective (victims are trapped). If environmental standards enforceable: constraint shifts to Tangled Rope or Scaffold (escape mechanism exists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(environmental_standard_supremacy, empirical, 'Whether environmental standards override trade liberalization rules').

omega_variable(
    deforestation_causality,
    'Does the agreement causally increase Amazon deforestation, or does deforestation proceed independently of tariff changes?',
    'Econometric analysis of pastureland expansion rates pre/post-agreement; comparison with counterfactual scenarios (no tariff change); attribution of deforestation to cattle vs soy vs other drivers',
    'If causal: suppression increases (victims cannot opt out of environmental damage); extraction parameter rises. If coincidental: environmental victim classification weakens; agreement reclassifies toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deforestation_causality, empirical, 'Causality between agreement and Amazon deforestation').

omega_variable(
    labor_standard_enforcement,
    'Can labor standards chapters be enforced against countries (especially Brazil) with weak labor inspectorates, or are they unenforceable commitments?',
    'Review of prior trade agreement labor chapter enforcement (NAFTA, USMCA, CPTPP); case studies of disputes; comparison of labor practices pre/post ratification in comparable agreements',
    'If enforceable: labor rights constitute real coordination mechanism (Tangled Rope confirmed). If unenforceable: labor chapters are theater (Piton elements dominate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_standard_enforcement, empirical, 'Whether labor standard chapters are enforceable').

omega_variable(
    small_farmer_displacement_threshold,
    'What tariff reduction rate on agricultural imports triggers farm abandonment and rural depopulation in EU regions dependent on commodity production?',
    'Regional agricultural economics: comparison of farm closure rates in prior EU trade liberalization events (Doha negotiations, Eastern European accession); threshold identification from historical data',
    'If threshold < 20% tariff reduction: small farmers immediately trapped (high suppression). If threshold > 50%: gradual adjustment possible (lower suppression, mobile exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_farmer_displacement_threshold, empirical, 'Tariff threshold triggering small farmer displacement').

omega_variable(
    mercosur_industrial_competitiveness,
    'Can Mercosur manufacturing (especially autos, machinery) compete with EU exports, or is the agreement merely a vehicle for EU access to agricultural markets without reciprocal manufactured goods demand?',
    'Trade flow analysis pre/post-agreement; price competitiveness comparisons (quality-adjusted); market share shifts in manufacturing sectors',
    'If Mercosur manufacturing uncompetitive: agreement is asymmetric extraction (manufacturing workers trapped, farmers benefit; snare dominates). If competitive: genuinely reciprocal coordination (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mercosur_industrial_competitiveness, empirical, 'Whether Mercosur manufacturing can compete with EU exports').

omega_variable(
    capital_mobility_labor_impact,
    'Does trade liberalization enable capital flight from Mercosur (FDI outflows to EU), or does it increase manufacturing investment and employment?',
    'FDI flow analysis post-agreement; sectoral employment trends in Mercosur manufacturing; comparison with pre-liberalization baseline',
    'If capital flight: workers face both job displacement and wage suppression (high extraction). If investment inflow: workers experience mixed coordination benefits and wage competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_labor_impact, empirical, 'Whether liberalization triggers capital flight or investment inflow').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_mercosur_trade_agreement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eumercosur_tr_t0, eu_mercosur_trade_agreement, theater_ratio, 0, 0.52).
narrative_ontology:measurement(eumercosur_tr_t10, eu_mercosur_trade_agreement, theater_ratio, 10, 0.6).
narrative_ontology:measurement(eumercosur_tr_t20, eu_mercosur_trade_agreement, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(eumercosur_be_t0, eu_mercosur_trade_agreement, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eumercosur_be_t10, eu_mercosur_trade_agreement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(eumercosur_be_t20, eu_mercosur_trade_agreement, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_mercosur_trade_agreement, resource_allocation).
narrative_ontology:affects_constraint(eu_mercosur_trade_agreement, amazon_deforestation_subsidy).
narrative_ontology:affects_constraint(eu_mercosur_trade_agreement, agricultural_trade_asymmetry).
narrative_ontology:affects_constraint(eu_mercosur_trade_agreement, mercosur_industrial_competitiveness).
narrative_ontology:affects_constraint(eu_mercosur_trade_agreement, eu_environmental_standards_race_to_bottom).

% DUAL FORMULATION NOTE:
% The EU-Mercosur agreement represents a constraint family with several structurally distinct components. The base constraint (this story) models the full institutional arrangement. Upstream constraints include: (1) WTO MFN rules that constrain asymmetric enforcement (creates theater); (2) Amazon deforestation subsidy embedded in cattle export pricing (creates environmental externality). Downstream constraints include: (1) EU environmental standard erosion (agreement prevents unilateral climate tariffs); (2) Mercosur industrial competitiveness crisis (capital flight and job displacement). The ε values differ: WTO MFN rules show low ε (0.20, Mountain view) — they are structural constraints on trade policy architecture. Amazon deforestation subsidy shows higher ε (0.65, Snare) — it is an extractive mechanism hidden in commodity pricing. EU environmental standard suppression shows high ε (0.72, Tangled Rope) — the agreement's rules subordinate environmental regulations to trade rules. All are linked: the agreement is the institutional mechanism binding them together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_mercosur_trade_agreement, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
