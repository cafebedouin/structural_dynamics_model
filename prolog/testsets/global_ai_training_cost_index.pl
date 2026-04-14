% ============================================================================
% CONSTRAINT STORY: global_ai_training_cost_index
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_ai_training_cost_index, []).

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
 *   constraint_id: global_ai_training_cost_index
 *   human_readable: Global AI Training Cost Index — Coordination and Asymmetric Extraction
 *   domain: artificial_intelligence/computational_economics/infrastructure
 *
 * SUMMARY:
 *   The Global AI Training Cost Index tracks the computational and financial
 *   requirements for training frontier large language models, serving as both
 *   a coordination mechanism for capital allocation and an extraction
 *   mechanism that locks in technological dependency. The index exhibits
 *   characteristics of a tangled rope constraint: it solves genuine
 *   coordination problems (capital allocation, strategic planning, research
 *   benchmarking) while simultaneously creating asymmetric extraction
 *   dynamics that concentrate capability in wealthy compute providers and
 *   frontier labs. The constraint operates across nine perspectival contexts,
 *   from powerless research commons (trapped/snare) to institutional
 *   beneficiaries (rope) to analytical observers at risk of naturalizing
 *   contingent institutional arrangements as laws of AI development. The
 *   theater ratio has increased over time as the index has shifted from a
 *   research metric to a policy and market signal, gaining performative
 *   functions beyond its original information role. Base extractiveness has
 *   increased from 0.35 to 0.58 over a six-year interval, reflecting both
 *   genuine cost escalation (driven by capability scaling) and institutional
 *   concentration (oligopolistic compute supply, network effects favoring
 *   frontier labs). Multiple omega variables identify structural ambiguities:
 *   efficiency improvement trajectories that could reduce or accelerate
 *   extraction, open-source capability gaps that could democratize or deepen
 *   dependency, and the reversibility of compute concentration itself.
 *
 * KEY AGENTS:
 *   - Research Commons: Powerless/trapped victim (institutional/academic groups, non-wealthy universities, developers in low-income regions) — faces absolute barriers to frontier model access, dependent on external compute provision
 *   - AI Development Periphery: Moderate/constrained victim (regional companies, middle-income nation labs) — face high cost barriers and capacity constraints; exit possible but costly
 *   - Open-Source AI Community: Organized/constrained victim-beneficiary (distributed researchers, open model contributors) — benefits from shared resources and coordination, extracted via cost escalation and compute competition
 *   - Frontier Model Laboratories: Institutional/arbitrage beneficiary (OpenAI, Anthropic, Google DeepMind, Meta) — primary beneficiaries of index coordination and cost regime; capture coordination gains
 *   - Compute Provider Oligopoly: Institutional/arbitrage beneficiary (NVIDIA, cloud providers AWS/GCP/Azure, custom silicon players) — benefits from price justification and capacity coordination enabled by index
 *   - National AI Strategies: Powerful/mobile actor (state actors, governments pursuing AI capability) — see coordinated benefits (planning) and extraction (dependency), moderate suppression through political options
 *   - AI Safety and Evaluation: Organized/constrained actor (alignment researchers, evaluation communities) — original function degrading as index becomes policy theater; piton classification reflects institutional inertia
 *   - Efficiency-Scaling Research: Organized/constrained beneficiary-actor (scaling law researchers, hardware optimization teams) — see sunset dynamics; research progress creating exit path from high-cost regime
 *   - Analytical Observer: Civilizational analytical perspective — risks naturalizing contingent institutional arrangements (architectural inefficiency, concentration, suboptimal hardware utilization) as fundamental limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_ai_training_cost_index, 0.58).
domain_priors:suppression_score(global_ai_training_cost_index, 0.62).
domain_priors:theater_ratio(global_ai_training_cost_index, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_ai_training_cost_index, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_ai_training_cost_index, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_ai_training_cost_index, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_ai_training_cost_index, tangled_rope).
narrative_ontology:human_readable(global_ai_training_cost_index, "Global AI Training Cost Index — Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(global_ai_training_cost_index, "artificial_intelligence/computational_economics/infrastructure").

domain_priors:requires_active_enforcement(global_ai_training_cost_index).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_ai_training_cost_index, compute_provider_oligopoly).
narrative_ontology:constraint_beneficiary(global_ai_training_cost_index, frontier_model_laboratories).
narrative_ontology:constraint_victim(global_ai_training_cost_index, research_commons_access).
narrative_ontology:constraint_victim(global_ai_training_cost_index, ai_development_periphery).
narrative_ontology:constraint_victim(global_ai_training_cost_index, energy_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCH COMMONS (SNARE) — Cannot exit the training cost regime. Small research groups, non-wealthy universities, and developers in low-income regions face absolute barriers to frontier model access. Trapped by economic dependency and structural inability to marshal capital. Bears full extraction cost with no viable alternatives.
constraint_indexing:constraint_classification(global_ai_training_cost_index, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AI DEVELOPMENT PERIPHERY (SNARE) — Regional AI companies and middle-income nation laboratories face high cost barriers and capacity constraints. Exit is possible but bears significant economic and institutional costs: retooling toward smaller models, accepting capability loss, or accepting dependency on compute rental. Suppression is high — alternatives are technically and economically inferior.
constraint_indexing:constraint_classification(global_ai_training_cost_index, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPEN-SOURCE AI COMMUNITY (TANGLED ROPE) — Organized but constrained. Benefits from shared model weights, pooled resources, and distributed training infrastructure. Simultaneously extracted via training cost escalation and compute resource competition. Face barriers to training state-of-the-art models (billions in capital required) but benefit from coordination mechanisms (shared architectures, distributed training frameworks, open model repos). Asymmetric: frontier labs capture coordination gains while periphery bears cost increases.
constraint_indexing:constraint_classification(global_ai_training_cost_index, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: FRONTIER MODEL LABORATORIES (ROPE) — Experience the index as pure coordination. Training cost transparency enables: (a) capital allocation efficiency, (b) competitive benchmarking, (c) justification for AI capability scaling budgets to boards/governments. The index solves a collective action problem: coordinating investment decisions across labs. Net beneficiary — extraction runs toward this agent. Exit options include arbitrage (training proprietary models with captured talent and data).
constraint_indexing:constraint_classification(global_ai_training_cost_index, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPUTE PROVIDER OLIGOPOLY (ROPE) — Pure beneficiary. The index provides: (a) market transparency that justifies pricing power, (b) coordination mechanism for capacity allocation, (c) legitimacy for sustained price growth by demonstrating it as 'market necessity.' Index existence enables collusion-adjacent coordination without explicit coordination. Exit via arbitrage (deploying proprietary hardware, locking in customers via ecosystem effects).
constraint_indexing:constraint_classification(global_ai_training_cost_index, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATIONAL AI STRATEGIES (TANGLED ROPE) — State actors see coordinated benefits (strategic planning, public investment justification) alongside extraction (cost escalation drives dependency on frontier models, creates technology sovereignty vulnerabilities). Powerful but not institutional arbitrage — governments face political constraints (public opinion on AI spending, competing budget priorities) and geopolitical lock-in. Suppression is moderate: exits exist (sovereign compute capacity, subsidized local training) but bear political and technical costs.
constraint_indexing:constraint_classification(global_ai_training_cost_index, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: AI SAFETY AND EVALUATION (PITON) — Original function was measuring computational requirements for alignment research. Theater has risen as the index has become a proxy for 'AI capability progress' in policy discussions. Actual safety-relevant signal is obscured by theater: total training cost ≠ alignment difficulty, large parameter counts ≠ safety-solved. The original coordination function (tracking compute allocation for research) persists through institutional inertia but is increasingly performative.
constraint_indexing:constraint_classification(global_ai_training_cost_index, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: EFFICIENCY-SCALING RESEARCH PROGRAMS (SCAFFOLD) — Organized actors (scaling law researchers, inference optimization teams, distributed training frameworks) see the high training cost regime as a temporary problem with a sunset. Structural improvements in compute efficiency (better architectures, training algorithms, hardware utilization) are reducing cost per unit capability. Exit path is clear: efficiency progress bypasses the index's extraction mechanism. Suppression decreases over generational time as efficiency breakthroughs compound. Has sunset logic embedded in research trajectories.
constraint_indexing:constraint_classification(global_ai_training_cost_index, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, training cost follows necessarily from information-theoretic requirements: teaching a system to model world complexity requires data processing proportional to task complexity. The index becomes naturalized as a law of AI development. However, this risks false summit: actual training costs are contingent on architectural inefficiency, suboptimal hardware utilization, and institutional concentration choices — not fundamental laws. Engine's false summit detector should flag this.
constraint_indexing:constraint_classification(global_ai_training_cost_index, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_ai_training_cost_index_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_ai_training_cost_index, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_ai_training_cost_index, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_ai_training_cost_index, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_ai_training_cost_index, TR),
    TR >= 0.70.

:- end_tests(global_ai_training_cost_index_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, increasing trend. Base extractiveness reflects genuine computational cost escalation driven by capability scaling, combined with institutional concentration in compute provision and frontier labs. The index tracks this truthfully, but its tracking serves as a coordination mechanism that facilitates and legitimizes the extraction. Initial value (0.35) represents early-phase cost-benefit ambiguity; final value (0.58) reflects matured rent-extraction dynamics where concentration enables pricing power. Suppression (0.62): High. Multiple suppression mechanisms operate: (1) technical barriers (training large models requires capital and expertise concentration), (2) institutional barriers (frontier labs control model weights and training recipes), (3) economic barriers (compute costs exclude most actors), (4) information barriers (training details obscured by proprietary secrecy). Suppression is not total — open-source alternatives exist, efficiency improvements accumulate, but barriers are substantial. Theater ratio (0.48): Moderate. The index serves genuine coordination functions (capital allocation, benchmarking, strategic planning) alongside emerging theater functions (legitimizing price growth, justifying AI capability spending to boards and governments). Theater has increased over the interval as the index expanded from research metric to policy signal. Current 0.48 reflects mixed function, not pure performance.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between institutional beneficiaries (frontier labs and compute providers, who see rope/pure coordination) and powerless victims (research commons, who see snare/pure extraction). The same coordination mechanism — transparent training cost tracking — appears as beneficial to capital-holders and extractive to the capital-poor. A secondary gap appears between the piton perspective (safety evaluation communities, whose original function has degraded to theater) and the scaffold perspective (efficiency researchers, who see genuine progress toward sunset). The analytical observer's mountain classification is revealed as a false summit when structural data shows that concentration, proprietary control, and suboptimal architectural choices drive costs — not fundamental information-theoretic limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural relationship to cost extraction flows. Powerless/trapped victims (research commons) derive d ≈ 0.95: maximum victim status, no exit, full extraction. Moderate constrained actors (development periphery) derive d ≈ 0.70: victim status with partial mobility (arbitrage to smaller models possible, but costly). Organized constrained actors (open-source community) derive d ≈ 0.60: mixed status — some victim (extraction via compute cost escalation), some beneficiary (coordination benefits from shared weights). Institutional arbitrage actors (frontier labs, compute providers) derive d ≈ 0.15-0.25: beneficiary status with high exit mobility, extraction flows toward them. Powerful mobile actors (national strategies) derive d ≈ 0.50: symmetric mixed — both cost and benefits, but with political constraints limiting exit. These d values feed the sigmoid f(d) to produce experienced extractiveness chi, scaled by scope modifier σ(S). At global scope, σ(S)=1.2, amplifying chi slightly — the index's coordination function and extraction mechanism both operate at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION PATHWAY: This constraint resolves mandatrophy through perspectival decomposition rather than single-type resolution. The question 'is the index coordinating or extracting?' has no univocal answer — it is both simultaneously, with the balance varying by observer position. The index genuinely solves coordination problems for capital-holders (frontier labs, compute providers) and genuinely enables extraction from the capital-poor (research commons, periphery). The mandatrophy is resolved by recognizing that 'coordination' and 'extraction' are not mutually exclusive categories — tangled rope is the legitimate classification precisely because both functions operate simultaneously with asymmetric distribution of benefits and costs. The false summit (analytical/mountain view) is rejected because structural data reveals contingency: cost escalation depends on architectural choices, compute concentration, and institutional decisions, not on inevitable information-theoretic necessity. The piton classification (safety evaluation degradation) identifies institutional drift — the original function has atrophied under pressure from the index's growing policy role. The scaffold classification (efficiency-scaling sunset) identifies a real structural exit path — efficiency improvements compound over generational timescales, reducing the cost-extraction mechanism's force. Mandatrophy is resolved when the engine recognizes that the index is a legitimate tangled rope (both coordination and extraction), not a misclassified rope (pure coordination) or snare (pure extraction), and identifies the false summit risk in naturalizing costs as unavoidable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_progress_rate,
    'What is the actual rate of compute-efficiency improvement? Does it exceed cost index growth, creating sunset dynamics?',
    'Longitudinal tracking of training cost per unit capability gain; comparison of published scaling laws against realized hardware/algorithmic improvements over 5-year intervals',
    'If efficiency progress > cost growth: scaffold sunset is real, classification shifts toward rope/mountain. If efficiency progress < cost growth: index remains extractive snare/tangled rope, no sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_progress_rate, empirical, 'Whether efficiency improvements outpace cost index growth').

omega_variable(
    open_model_capability_gap,
    'Can open-source models approach frontier capability without frontier-scale compute budgets? Does gap narrow or widen?',
    'Benchmark capability (MMLU, coding tasks, reasoning) of open models vs frontier models; cost-efficiency ratio comparison over time; adoption rates in production systems',
    'If gap narrows: open-source research commons gains viability, snare perspective weakens. If gap widens: periphery becomes more trapped, snare extraction accelerates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_model_capability_gap, empirical, 'Whether open-source models narrow frontier capability gap without proportional compute').

omega_variable(
    compute_concentration_reversibility,
    'Is the oligopolistic compute supply concentration reversible? Can distributed training infrastructure or alternative hardware break the monopoly?',
    'Growth of non-major-provider compute ecosystems; successful alternatives to NVIDIA/cloud providers; distributed training framework adoption; custom silicon competitiveness',
    'If reversible: rope beneficiary classification breaks, extract mechanism weakens. If irreversible: institutional lock-in becomes mountain-adjacent, suppression remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compute_concentration_reversibility, empirical, 'Whether compute concentration is reversible or path-dependent lock-in').

omega_variable(
    index_coordination_function_authenticity,
    'Does the index genuinely coordinate investment and resource allocation, or does it primarily serve as legitimacy theater for price justification?',
    'Analysis of decision patterns before/after index publication; interviews with capital allocation decision-makers; correlation between index announcements and market behavior; counterfactual modeling of capital flows without index',
    'If genuine coordination: rope/tangled rope classification appropriate. If primarily theater: piton classification becomes more central, index is maintained by inertia rather than function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_coordination_function_authenticity, conceptual, 'Whether the index genuinely coordinates or primarily legitimizes extraction').

omega_variable(
    energy_infrastructure_externality_scope,
    'Are energy grid impacts (carbon, peak demand) actually distributed externalities or are they concentrated in specific regions/nations?',
    'Mapping of training facilities to power grid characteristics; carbon intensity analysis by region; correlation between training compute and grid strain; energy cost differentials by geography',
    'If distributed: energy_infrastructure_victims are abstract/global. If concentrated: specific regions/nations are targeted victims, extraction becomes geopolitically legible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_infrastructure_externality_scope, empirical, 'Whether energy externalities are distributed globally or concentrated regionally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_ai_training_cost_index, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaitci_tr_t0, global_ai_training_cost_index, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gaitci_tr_t2, global_ai_training_cost_index, theater_ratio, 2, 0.38).
narrative_ontology:measurement(gaitci_tr_t4, global_ai_training_cost_index, theater_ratio, 4, 0.45).
narrative_ontology:measurement(gaitci_tr_t6, global_ai_training_cost_index, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(gaitci_be_t0, global_ai_training_cost_index, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gaitci_be_t2, global_ai_training_cost_index, base_extractiveness, 2, 0.43).
narrative_ontology:measurement(gaitci_be_t4, global_ai_training_cost_index, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(gaitci_be_t6, global_ai_training_cost_index, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_ai_training_cost_index, resource_allocation).
narrative_ontology:affects_constraint(global_ai_training_cost_index, frontier_model_capability_scaling).
narrative_ontology:affects_constraint(global_ai_training_cost_index, compute_supply_concentration).
narrative_ontology:affects_constraint(global_ai_training_cost_index, ai_energy_carbon_externality).
narrative_ontology:affects_constraint(global_ai_training_cost_index, research_commons_access_inequality).

% DUAL FORMULATION NOTE:
% The global AI training cost index is downstream of multiple structural constraints with distinct ε values: compute supply concentration (ε≈0.52, tangled rope), frontier model scaling pressures (ε≈0.48, tangled rope), energy infrastructure constraints (ε≈0.35, rope), and research access inequality (ε≈0.68, snare). The index itself (ε≈0.58, tangled rope) represents the coordinated effect of these upstream constraints, enabling both capital allocation efficiency and rent extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_ai_training_cost_index, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
