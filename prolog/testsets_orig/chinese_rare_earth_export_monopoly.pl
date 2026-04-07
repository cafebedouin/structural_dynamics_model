% ============================================================================
% CONSTRAINT STORY: chinese_rare_earth_export_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chinese_rare_earth_export_monopoly, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: chinese_rare_earth_export_monopoly
 *   human_readable: Chinese Rare Earth Export Monopoly and Supply Dependency
 *   domain: economic/geopolitical/resources
 *
 * SUMMARY:
 *   China's dominance of rare earth supply chains represents a classic
 *   resource monopoly constraint with dual mechanism: geological
 *   concentration of mineable deposits and Chinese control of downstream
 *   processing infrastructure. Beginning in the 1990s as an accident of
 *   industrial investment and geographic proximity to deposits, the monopoly
 *   hardened through deliberate state policy (export quotas, licensing
 *   restrictions, vertical integration of mining-to-processing) into a
 *   geopolitical extraction lever. Dependent technology sectors
 *   (semiconductors, defense, renewable energy) face suppression: high cost
 *   to exit (retooling, alternative sourcing, 5-10 year timelines), no
 *   intermediate options, and vulnerability to supply shocks and political
 *   leverage. The constraint shows increasing extractiveness over two decades
 *   (0.45→0.68) as Chinese policymakers learned to weaponize the monopoly.
 *   Theater ratio remains low (0.25-0.35) because the extraction operates
 *   through material scarcity and legal export controls, not through
 *   performative mechanisms. The constraint exhibits Snare classification
 *   from powerless and moderate perspectives (technology sectors, allied
 *   militaries) but Rope or Scaffold from organized coalition perspectives
 *   (alternative supply investments creating exit pathways) and Mountain from
 *   the false thermodynamic view (naturalizing what is actually political
 *   extraction).
 *
 * KEY AGENTS:
 *   - Chinese State Apparatus: Primary beneficiary (institutional/arbitrage) — captures geopolitical leverage and economic rent from monopoly control; uses export restrictions as policy tool
 *   - Chinese Mining Corporations: Primary beneficiary (institutional/arbitrage) — extract economic surplus through pricing power and preferential supply access; state-owned vertically integrated firms
 *   - Dependent Technology Sectors: Primary victim (powerless/trapped) — semiconductor makers, defense contractors, renewable energy producers trapped by supply dependency; face price volatility and supply shocks
 *   - Allied Military Industrial Complex: Secondary victim (moderate/constrained) — defense and aerospace unable to diversify rare earth sources quickly; strategic vulnerability in supply chains
 *   - Competing Rare Earth Producers: Secondary victim (powerful/mobile) — Australia, USA, Myanmar producers face price undercutting and China's processing monopoly; technically mobile but squeezed through downstream control
 *   - Coalition of Allied Governments: Organized actor (organized/mobile) — investing in alternative mining/processing; developing exit strategies through government-backed diversification
 *   - Alternative Supply Ecosystems: Emerging actors (organized/constrained) — USA (Mountain Pass), EU (Greenland), India rare earth projects building parallel supply chains with 15-25 year timelines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chinese_rare_earth_export_monopoly, 0.68).
domain_priors:suppression_score(chinese_rare_earth_export_monopoly, 0.72).
domain_priors:theater_ratio(chinese_rare_earth_export_monopoly, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chinese_rare_earth_export_monopoly, extractiveness, 0.68).
narrative_ontology:constraint_metric(chinese_rare_earth_export_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(chinese_rare_earth_export_monopoly, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chinese_rare_earth_export_monopoly, snare).
narrative_ontology:human_readable(chinese_rare_earth_export_monopoly, "Chinese Rare Earth Export Monopoly and Supply Dependency").
narrative_ontology:topic_domain(chinese_rare_earth_export_monopoly, "economic/geopolitical/resources").

domain_priors:requires_active_enforcement(chinese_rare_earth_export_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chinese_rare_earth_export_monopoly, chinese_state_apparatus).
narrative_ontology:constraint_beneficiary(chinese_rare_earth_export_monopoly, chinese_mining_corporations).
narrative_ontology:constraint_victim(chinese_rare_earth_export_monopoly, dependent_technology_sectors).
narrative_ontology:constraint_victim(chinese_rare_earth_export_monopoly, allied_military_industrial_complex).
narrative_ontology:constraint_victim(chinese_rare_earth_export_monopoly, competing_rare_earth_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT TECHNOLOGY SECTOR (SNARE) — Electronics manufacturers, defense contractors, and renewable energy producers cannot exit the rare earth supply chain without massive retooling costs and 5-10 year diversification timelines. Trapped within Chinese supply dependency for critical inputs (neodymium, dysprosium, terbium). Maximum extraction experienced: subject to price shocks, export restrictions, and geopolitical leverage.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED MILITARY INDUSTRIAL COMPLEX (SNARE) — Military supply chains depend on rare earths for precision guidance, radar systems, and communications. High cost to develop alternative sources (10-20 year timelines for mine development and processing infrastructure). Career risk for procurement officials who flag the vulnerability. Constrained by strategic necessity and institutional inertia.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETING RARE EARTH PRODUCERS (SNARE) — Alternative producers (USA, Myanmar, Greenland, Australia) have structural capacity to exit the Chinese monopoly but face price undercutting, environmental barriers, and China's control of processing infrastructure. Suppression operates through processing monopoly (China controls 90%+ of rare earth processing globally). Mobile technically but squeezed through downstream monopoly.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE STATE APPARATUS (ROPE) — Experiences the export control as a coordination mechanism for strategic resource management and geopolitical leverage. Net beneficiary. Can arbitrage supply restrictions against competitor states and lock in dependent relationships. Low suppression for the beneficiary — enforcement is legal (export licensing) and carries no domestic constraint.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COALITION OF ALLIED GOVERNMENTS (TANGLED ROPE) — NATO, US, EU governments see both coordination (pooling of resources to develop alternative suppliers) and extraction (China's leverage over their technology sectors). Organized actors beginning to invest in rare earth mining, processing, and substitution. Moderate-high extraction but declining due to invested exit pathways. Theater is low — this constraint operates through material scarcity, not performative mechanisms.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL TRADE REGIME (PITON) — WTO free-trade norms formally prohibit export cartels on raw materials, but these norms are largely performative in rare earth context. China's export quotas and licensing restrictions violate the letter of WTO law but persist through institutional inertia, weak enforcement, and Chinese arbitrage of geopolitical leverage against prosecution mechanisms. Theater ratio high (formal compliance claims) masking actual restrictions.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE SUPPLY ECOSYSTEMS (SCAFFOLD) — USA (Mountain Pass), EU (Greenland/Estonia), India, and Vietnam are building alternative rare earth mining and processing capacity. These represent Scaffold logic: temporary coordination of government investment with sunset of Chinese monopoly benefit over 15-25 year horizon. High suppression presently (China's processing dominance + initial capital costs) but declining through installed sunset clause (alternative capacity coming online).
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — Rare earth elements are geologically concentrated: most economically viable deposits exist in China, Myanmar, and Australia. Processing requires specialized knowledge and infrastructure concentrated in East Asia. From a civilizational timescale, concentration of resource access is 'natural' — reflecting geological distribution and incumbent investment. However, the false summit detector will flag this: geological concentration is immutable, but China's extraction of monopoly rent is not. The constraint is a snare, not a mountain.
constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chinese_rare_earth_export_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chinese_rare_earth_export_monopoly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chinese_rare_earth_export_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chinese_rare_earth_export_monopoly, TR),
    TR >= 0.70.

:- end_tests(chinese_rare_earth_export_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. China captures economic rent through pricing power (rare earths sold at 3-5x estimated production cost during peak extraction periods) and geopolitical leverage (explicit use of export restrictions as foreign policy tool — 2010 Japan dispute, ongoing US-China technology wars). The extractiveness reflects both the magnitude of economic transfer and the structural weaponization. Suppression (0.72): Very high. Barriers to exit include: (1) geological reality — mineable rare earth deposits are geographically concentrated; (2) processing monopoly — China controls 90%+ of global rare earth processing capacity; (3) time-to-capability — alternative mining takes 5-10 years to develop, processing infrastructure 8-15 years; (4) sunk costs — existing rare earth-dependent technology architectures cannot be quickly redesigned. Theater ratio (0.35): Low. The extraction operates through material mechanisms (supply constraints, pricing), not performative ones. Export licensing is legally transparent but functionally extractive. Theater is above zero only because alternative producers perform 'strategic autonomy' rhetoric while remaining dependent. Base extractiveness increase over 20 years reflects learning: early Chinese monopoly (1990s) was unintentional; by 2010s it was deliberately weaponized through export quotas and strategic licensing.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between beneficiary (Rope) and victim (Snare) frames. The beneficiary legitimately coordinates resource management; the victim legitimately experiences extraction they cannot exit. The Scaffold and piton perspectives represent emerging/degrading intermediate positions. The false mountain represents the risk of naturalizing contingent power asymmetries into laws of nature. Resolving the perspectival gap requires decomposing: (1) What is genuinely immutable (geology)? Rare earth elements are concentrated; mineable deposits are geographically sparse. (2) What is contingent monopoly (policy)? Processing capacity concentration, export quotas, pricing strategies. The constraint is a Snare on the contingent layer, not a Mountain on the immutable layer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the extraction flow. Chinese state apparatus: beneficiary + arbitrage exit → d ≈ 0.05 (full beneficiary, can arbitrage globally). Technology sectors: victim + trapped exit → d ≈ 0.95 (full target, no escape). Allied governments: victim with organizing capacity + mobile exit → d ≈ 0.55 (partial target, building exit). Competing producers: victim + mobile exit but facing processing monopoly squeeze → d ≈ 0.70 (primary target, exit constrained by downstream control). The pipeline computes f(d) from these d values: beneficiaries experience negative chi (subsidy), trapped victims experience high chi (maximum extraction), constrained actors with partial agency experience moderate chi. The overrides are not needed here — structural derivation captures the genuine relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The extractiveness (0.68) is high enough to require mandatrophy resolution. The analysis reveals that this is NOT a false positive classification (mislabeling extraction as coordination). The Chinese beneficiary legitimately experiences coordination (managing strategic resources) but this coordination function coexists with asymmetric extraction (dependent sectors paying monopoly rent without consent). Tangled Rope classification was considered but rejected because: (1) the victims (powerless and moderate agents) experience minimal coordination benefit — they are subjects of extraction, not participants in coordination; (2) the suppression (0.72) and extraction magnitude exceed Tangled Rope thresholds; (3) the constraint's primary function is resource allocation via monopoly control, not genuine collective action problem-solving. Snare is correct. The Scaffold perspective (alternative supplies) creates a real sunset mechanism, but the current state is Snare. The mandatrophy resolves by confirming that high extractiveness reflects structural extraction, not measurement error or misframing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    processing_monopoly_vs_mining_concentration,
    'Is the extraction mechanism driven by China''s control of processing infrastructure (achievable alternative monopoly) or by geological rarity of mineable deposits (immutable scarcity)?',
    'Cost analysis of processing capacity development in non-Chinese jurisdictions; timeline for alternative processing scale-up; data on rare earth ore availability outside China vs. processing feasibility',
    'If processing-dominant: the constraint is a snare with achievable exit (10-20 year timescale). If geology-dominant: the constraint approaches mountain (immutable) and allied technology sectors face permanent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(processing_monopoly_vs_mining_concentration, empirical, 'Whether extraction derives from processing monopoly or geological scarcity').

omega_variable(
    substitution_feasibility,
    'Can downstream technology industries (semiconductors, magnets, batteries) develop functional substitutes for critical rare earths (neodymium, dysprosium) or are these irreplaceable for specific applications?',
    'Materials science literature review; successful substitution timelines in existing technologies; R&D investment outcomes in rare-earth-free motor and magnet designs',
    'If substitution viable: exit cost drops by 40-60%, classification shifts toward Tangled Rope from dependent sectors. If irreplaceable: permanent Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_feasibility, empirical, 'Technical feasibility of rare earth substitution in critical applications').

omega_variable(
    chinese_political_stability_dependency,
    'Does the sustainability of China''s rare earth export monopoly depend on political continuity in Beijing, or would any rational actor controlling the resource base extract similarly?',
    'Comparative study of resource monopoly behavior across regimes; analysis of whether extraction persists under leadership transitions; counterfactual modeling of alternative political scenarios',
    'If regime-dependent: the constraint''s suppression could collapse under political transition. If universal to monopoly: the extraction is structural and regime-invariant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chinese_political_stability_dependency, conceptual, 'Whether extraction is regime-dependent or universal to monopoly').

omega_variable(
    coalition_sunset_credibility,
    'Are the alternative supply investments (US, EU, India rare earth projects) credible exit mechanisms with genuine sunset logic, or are they performative ''strategic autonomy'' theater that will be abandoned when investment becomes costly?',
    'Capital commitment tracking; cost curves for alternative processing; government budget persistence through fiscal cycles; corporate R&D allocation to substitution',
    'If credible: Scaffold perspective valid, suppression declining on measured trajectory. If performative: alternative supply is theater, Snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_sunset_credibility, empirical, 'Credibility of alternative supply development as genuine exit mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chinese_rare_earth_export_monopoly, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crem_tr_t0, chinese_rare_earth_export_monopoly, theater_ratio, 0, 0.25).
narrative_ontology:measurement(crem_tr_t10, chinese_rare_earth_export_monopoly, theater_ratio, 10, 0.3).
narrative_ontology:measurement(crem_tr_t20, chinese_rare_earth_export_monopoly, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(crem_be_t0, chinese_rare_earth_export_monopoly, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(crem_be_t10, chinese_rare_earth_export_monopoly, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(crem_be_t20, chinese_rare_earth_export_monopoly, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chinese_rare_earth_export_monopoly, resource_allocation).
narrative_ontology:affects_constraint(chinese_rare_earth_export_monopoly, semiconductor_supply_chain_fragility).
narrative_ontology:affects_constraint(chinese_rare_earth_export_monopoly, alliance_strategic_mineral_dependency).
narrative_ontology:affects_constraint(chinese_rare_earth_export_monopoly, geopolitical_technology_competition).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific technology sector vulnerabilities (semiconductors, defense, renewables). Each downstream constraint has its own extractiveness reflecting sector-specific mitigation capacity; the monopoly constraint provides the structural commons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
