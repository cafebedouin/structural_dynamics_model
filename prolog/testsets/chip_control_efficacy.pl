% ============================================================================
% CONSTRAINT STORY: chip_control_efficacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chip_control_efficacy, []).

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
 *   constraint_id: chip_control_efficacy
 *   human_readable: Semiconductor Export Control Efficacy in Strategic Competition
 *   domain: international_relations/technology_governance/strategic_competition
 *
 * SUMMARY:
 *   US semiconductor export controls targeting China's AI capabilities
 *   (NVIDIA A100/H100 GPUs, advanced AI accelerators) represent a technology
 *   governance coordination mechanism among allied nations to manage dual-use
 *   technology diffusion in strategic competition. The constraint exhibits
 *   low extractiveness (0.18) because the primary function is genuine
 *   coordination around technology governance and strategic parity
 *   maintenance rather than asymmetric rent extraction. Observable evidence
 *   shows the controls create measurable but surmountable barriers: Chinese
 *   AI labs report 10x-30x compute disadvantages in hardware access, yet
 *   model performance approaches frontier systems through algorithmic
 *   optimization, open-source architectures, and training efficiency gains.
 *   The regime classifies as Rope from the allied coalition perspective
 *   (genuine coordination benefit) but as Tangled Rope from Chinese
 *   perspective (mixed coordination via published research + extraction via
 *   access denial). Emerging-market AI startups face Snare classification:
 *   trapped between pressure to join the coalition (losing geopolitical
 *   autonomy) and exclusion from advanced hardware without political voice.
 *   The constraint exhibits low theater ratio (0.42) because semiconductor
 *   export controls rely primarily on technical enforcement (supply chain
 *   tracking, chip design verification) rather than performative legitimacy,
 *   distinguishing it from multilateral regimes that depend on consensus
 *   theater.
 *
 * KEY AGENTS:
 *   - US-Led Allied Coalition (US, Taiwan, Japan, South Korea, Netherlands): Institutional beneficiaries (arbitrage exit) — coordinate technology governance standards, maintain strategic technology advantage, prevent unilateral defection
 *   - Chinese AI Research Laboratories: Moderate victims (constrained exit) — bear hardware access denial but benefit from published research and algorithmic innovation pathways
 *   - Chinese Domestic Chip Development Programs (SMIC, Huawei HiSilicon): Organized state actors (constrained exit) — experience constraint as temporary coordination failure solvable through domestic capability development (scaffold sunset logic)
 *   - Emerging Market AI Startups (India, Brazil, Vietnam, Indonesia): Powerless victims (trapped exit) — excluded from both hardware access and coalition governance, experience pure extraction without compensation
 *   - Legacy Multilateral Export Control Regimes (Wassenaar, NSG): Institutional actors (arbitrage exit) — bypassed by bilateral US-led controls, persist through institutional inertia with high theater ratio
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing technology diffusion as inevitable law of nature, obscuring the contingent strategic choice to accept temporary advantage costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chip_control_efficacy, 0.18).
domain_priors:suppression_score(chip_control_efficacy, 0.35).
domain_priors:theater_ratio(chip_control_efficacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chip_control_efficacy, extractiveness, 0.18).
narrative_ontology:constraint_metric(chip_control_efficacy, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(chip_control_efficacy, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chip_control_efficacy, rope).
narrative_ontology:human_readable(chip_control_efficacy, "Semiconductor Export Control Efficacy in Strategic Competition").
narrative_ontology:topic_domain(chip_control_efficacy, "international_relations/technology_governance/strategic_competition").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chip_control_efficacy, allied_semiconductor_exporters).
narrative_ontology:constraint_beneficiary(chip_control_efficacy, us_led_ai_research_coalition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALLIED SEMICONDUCTOR EXPORTERS (ROPE) — US, Taiwan, Netherlands, South Korea benefit from coordinated control regime that preserves market segmentation and pricing power. Experience the constraint as pure coordination: establish common standards on dual-use technology, protect legitimate high-performance chip sales to allies, maintain technology leadership advantage. Arbitrage options available (compliance with allied standards vs. unilateral export relationships). Low experienced extraction — the constraint coordinates their interests rather than extracting from them.
constraint_indexing:constraint_classification(chip_control_efficacy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: CHINESE AI RESEARCH LABORATORIES (TANGLED ROPE) — Primary targets of the control regime; face 10x-30x compute disadvantages in advanced GPU/TPU access. However, the constraint also enables some coordination benefits: open-source model architectures, published research methodologies, and algorithmic innovation provide partial substitution pathways. Constrained exit (could pursue domestic chip development but at massive capital cost and timeline delay). Experience significant extraction (access denial) alongside genuine constraint coordination (published research enables algorithmic innovation). Mixed extraction and coordination distinguish tangled rope from pure snare.
constraint_indexing:constraint_classification(chip_control_efficacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE DOMESTIC CHIP DEVELOPMENT (SCAFFOLD) — Organized state response to export controls. SMIC, Huawei HiSilicon, and government-backed programs treat the control regime as a temporary coordination failure solvable through domestic capability development. The sunset logic is explicit in policy documents: investment in domestic semiconductors (RISC-V, 7nm+ fabs) aims to make the control regime irrelevant within 10-15 years. Low effective extraction because the organized agent has structured exit pathway. Theater component (demonstrating commitment to self-sufficiency) is moderate.
constraint_indexing:constraint_classification(chip_control_efficacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE US-LED ALLIED COALITION (ROPE) — Genuine coordination of technology governance among NATO, Japan, South Korea, Taiwan. Coalition benefits from coordinated standards that prevent unilateral defection (e.g., Netherlands or Taiwan selling advanced chips to China at full capability levels). Pure coordination function: establish common control framework, share intelligence on diversion, enforce reciprocal compliance. Arbitrage available (maintain coalition commitment vs. pursue unilateral market expansion). Low extraction experienced because all major members have similar strategic interests in technology preservation.
constraint_indexing:constraint_classification(chip_control_efficacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EMERGING MARKET AI STARTUPS (SNARE) — India, Brazil, Vietnam, Indonesia AI labs face indirect but severe constraints: they cannot access the most advanced chips without joining the allied coalition, yet lack political leverage to shape the coalition's terms. Trapped by geopolitical alignment decisions (coalition membership requires explicit political alignment). No arbitrage options. Experience pure extraction: deprived of hardware access without compensation or voice in governance. This is the unintended victim class of the control regime.
constraint_indexing:constraint_classification(chip_control_efficacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY MULTILATERAL REGIMES (PITON) — The Wassenaar Arrangement, Nuclear Suppliers Group, and other multilateral export control frameworks predate semiconductor export controls. These institutions have been substantially bypassed by bilateral US-led controls (BIS Entity List, unilateral sanctions). The legacy regimes persist through institutional inertia but are not the primary mechanism. Theater ratio is high: formal multilateral process for consensus-building is conducted in parallel with unilateral US enforcement, creating redundancy and performative legitimacy. Low functional control because primary mechanism is bilateral, not multilateral.
constraint_indexing:constraint_classification(chip_control_efficacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, technology diffusion is structurally inevitable: Moore's Law extends to all economies eventually, supply chains optimized toward decentralization make enforcement costly, knowledge published in open literature cannot be 'unlearned'. This perspective frames export controls as attempts to slow inevitable diffusion rather than prevent it. Empirical support: Chinese AI labs achieved competitive performance on transformer architectures with limited GPU access (using engineering optimization, algorithmic innovation). However, the mountain classification is vulnerable to false-summit detection: the 'inevitability' framing naturalizes what is actually a contingent strategic choice by the allied coalition to accept temporary technological advantage costs.
constraint_indexing:constraint_classification(chip_control_efficacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chip_control_efficacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chip_control_efficacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chip_control_efficacy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(chip_control_efficacy, TR),
    TR >= 0.70.

:- end_tests(chip_control_efficacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint's primary function is coordination of technology governance standards among allied nations, not asymmetric resource extraction. The US and allies derive benefit from coordinated standards that prevent unilateral defection and maintain technology leadership, but this is not extraction in the sense of one party taking resources from another without compensation. Chinese labs suffer access denial, but they also benefit from published research, open-source model architectures, and algorithmic innovation pathways that substitute partially for hardware access. The measured extractiveness reflects the net balance: coordination benefit (tempo of technology diffusion, maintained strategic advantage) outweighs pure extraction. The 10x-30x compute disadvantage is real but does not constitute maximum extraction because it is not irreversible — domestic chip development provides an exit pathway on 10-15 year timescales. Suppression (0.35): Moderate. Supply chain controls, licensing regimes, and technical enforcement create significant barriers to advanced semiconductor access but are not insurmountable. Chinese labs achieve workarounds through algorithmic optimization, older-generation hardware, and cloud computing partnerships. Coalition enforcement is real but faces defection incentives from individual member states (Netherlands, Taiwan, South Korea) facing economic costs of export denial. Theater ratio (0.42): Moderate-low. Unlike multilateral regimes that depend on consensus theater and formal processes, semiconductor export controls rely primarily on technical enforcement (chip verification, supply chain tracking) and bilateral relationships. This creates lower theatrical content than legacy regimes like Wassenaar, which conduct formal consensus processes in parallel with actual enforcement. The theater component (regulatory legitimacy, international law framing) is present but subordinate to technical control mechanisms. Rising theater over the interval reflects increased diplomatic justification and formal process layering as the regime matures and faces coalition criticism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across the indexed positions. The allied coalition (institutional/arbitrage) experiences pure Rope — coordination of their mutual interests with low experienced extraction. Chinese domestic chip programs (organized/constrained) experience Scaffold — a temporary coordination failure with structured exit pathway and sunset logic. Chinese AI labs (moderate/constrained) experience Tangled Rope — genuine access coordination via published research alongside extraction via hardware denial. Emerging-market actors (powerless/trapped) experience Snare — pure extraction with no voice in governance and no exit pathway. Legacy multilateral regimes (institutional/arbitrage) experience Piton — degraded institutional function persisting through inertia while bilateral mechanisms provide real enforcement. The analytical observer at civilizational scope risks Mountain — naturalizing technology diffusion as inevitable, obscuring the coalition's active strategic choice. This perspectival range demonstrates that no single type captures the constraint's structure; the presheaf over observation positions is required.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality values (d) map to power level, exit options, and beneficiary/victim status. Allied institutional actors with arbitrage exits (US, Taiwan, TSMC, SK Hynix) derive d ≈ 0.05-0.15: beneficiaries with escape options. Chinese labs with constrained exits and victim status (hardware denial) derive d ≈ 0.70-0.85: high extraction experienced. Emerging-market powerless actors with trapped exits derive d ≈ 0.95: maximum extraction. The sigmoid f(d) scales these into effective experienced extractiveness chi, with scope modifiers amplifying global-level constraints (σ=1.2). The tangled rope classification reflects the moderate midpoint: Chinese labs are victims (high d) but also benefit from coordination mechanisms (published research), creating mixed extraction-coordination dynamics. The coalition's low extraction despite high global scope reflects f(d) approaching -0.20 at d=0.05 — beneficiaries experience negative extraction (subsidy) because the constraint serves their interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that Rope (allied coalition perspective) and Tangled Rope (Chinese labs perspective) are both correct descriptions of the same structural mechanism. The mandatrophy is not 'which classification is true?' but 'which structural relationship are you observing from?' The allied coalition genuinely experiences coordination (Rope): establishing common standards solves their collective action problem around technology governance. Chinese labs genuinely experience mixed extraction + coordination (Tangled Rope): access denial extracts, but published research and algorithmic innovation pathways enable partial substitution. Both perspectives are structural facts, not observational artifacts. The resolution requires recognizing that the constraint serves different functions for different agents simultaneously. The Rope function (coordination of allied interests) is the primary intended function; the Tangled Rope function (mixing extraction with coordination) is the structural consequence for targets. This is not a classification ambiguity but a structural asymmetry that the indexical framework captures precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_pathway_velocity,
    'How quickly can Chinese domestic chip development (SMIC, HiSilicon) achieve capability parity with controlled US-origin semiconductors?',
    'Technical roadmap analysis; cross-reference SMIC fab yields and node maturity against TSMC/Samsung timelines; track architectural optimization advances in open-source frameworks (vLLM, Hugging Face) that reduce hardware dependence',
    'If < 5 years: scaffold sunset is real and extraction diminishes rapidly. If > 15 years: extraction persists; scaffold classification becomes aspirational rather than structural. If indefinite: reframe constraint as structural snare, not temporary scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_pathway_velocity, empirical, 'Timeline for Chinese domestic chip capability to reach parity with controlled exports').

omega_variable(
    algorithmic_substitution_ceiling,
    'Do algorithmic innovations and training efficiency gains provide sufficient compensation for hardware access denial, or is there an irreducible hardware-constrained frontier beyond which algorithmic optimization cannot substitute?',
    'Comparative analysis of model performance vs. compute consumption: track FLOPs-to-performance ratio for equivalent models trained with full hardware access vs. algorithmic optimization under constraints. Compare frontier models (GPT-4 class) achievable with and without advanced GPU access.',
    'If high substitution ceiling (>80% of frontier capability achievable via algorithms): extraction is partially mitigated, tangled rope classification robust. If low ceiling (<50%): hardware denial is irreducible bottleneck, snare classification strengthens, escape pathway requires domestic chips.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_substitution_ceiling, empirical, 'Degree to which algorithmic optimization substitutes for hardware access').

omega_variable(
    coalition_cohesion_sustainability,
    'Can the US-led allied coalition maintain unified semiconductor export controls when individual members (Netherlands, Taiwan, South Korea) face economic incentives to unilaterally expand sales to China?',
    'Monitor defection signals: unilateral license approvals, circumvention via subsidiaries, formal negotiation of carve-outs. Track historical precedents (CoCom collapse 1994, Encryption Wars defection patterns). Model coalition stability under Chinese counter-offers (purchasing commitments, rare-earth trade concessions).',
    'If coalition cohesion holds: rope classification remains robust, extraction on Chinese targets persists. If defection accelerates: rope degrades to rope with mounting enforcement cost, effective extraction declines, constraint approaches piton (performative enforcement with limited real suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_cohesion_sustainability, empirical, 'Sustainability of allied coalition against defection incentives').

omega_variable(
    unintended_victim_constituency_visibility,
    'Will emerging-market AI startups (non-aligned states) mobilize politically around being trapped victims of the control regime, creating pressure for alternative governance structures?',
    'Monitor policy advocacy from India, Brazil, Vietnam, Indonesia AI communities. Track UN statements on technology equity. Measure coalition pressure from middle-power states to participate in governance design.',
    'If low visibility: snare classification remains uncontested, extractive asymmetry persists without international friction. If high visibility: coalition faces legitimacy pressure, may need to formalize victim compensation (technology transfer programs, preferential access tiers) or risk fracturing. This routes toward rope with explicit victim accommodation (scaling up beneficiaries) rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unintended_victim_constituency_visibility, empirical, 'Degree of political mobilization by trapped emerging-market actors').

omega_variable(
    natural_law_false_summit_candidate,
    'Is the constraint truly a natural law of technology diffusion (mountain), or does declaring the diffusion inevitable naturalize what is actually a contingent strategic choice by the allied coalition?',
    'Counterfactual analysis: what would AI capability distribution look like in 2035 with vs. without export controls? Examine whether ''inevitable diffusion'' assumption is grounded in physics/economics or in geopolitical inevitabilism. Cross-reference with historical cases (nuclear technology, encryption) where controls achieved multi-decade delays despite ''inevitability'' narratives.',
    'If mountain is false summit: constraint should reclassify as rope (coordination) or tangled rope (mixed extraction + coordination). The ''naturalization'' framing obscures the coalition''s active strategic choice and its distributive consequences. If mountain is genuine: extraction on Chinese targets is inevitable cost of slower diffusion, not extractive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_candidate, conceptual, 'Whether technology diffusion inevitability is natural law or strategic narrative').

omega_variable(
    us_domestic_extraction_within_allied_coalition,
    'Does the US extract disproportionate benefits from the coalition through asymmetric control-setting power and enforcement authority (BIS Entity List unilateralism), making the coalition itself a tangled rope from allied perspective?',
    'Compare governance structures: do allied members have veto power over control additions, or does US unilateral authority dominate? Track grievances from Netherlands (ASML export restrictions) and Taiwan (subordinated to US strategy). Assess whether coalition members could exit and pursue independent semiconductor strategies.',
    'If US extracts significant coalition benefit: the coalition-level classification may degrade from rope to tangled rope (coordination function for technology governance + extraction of governance authority by hegemon). This would create a nested constraint: allies experience rope (technology coordination) while collectively experiencing tangled rope (US hegemonic extraction of control authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_domestic_extraction_within_allied_coalition, empirical, 'Asymmetric extraction of governance authority within allied coalition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chip_control_efficacy, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chip_tr_t0, chip_control_efficacy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(chip_tr_t3, chip_control_efficacy, theater_ratio, 3, 0.35).
narrative_ontology:measurement(chip_tr_t6, chip_control_efficacy, theater_ratio, 6, 0.42).

% Extraction over time
narrative_ontology:measurement(chip_be_t0, chip_control_efficacy, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(chip_be_t3, chip_control_efficacy, base_extractiveness, 3, 0.14).
narrative_ontology:measurement(chip_be_t6, chip_control_efficacy, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chip_control_efficacy, global_infrastructure).
narrative_ontology:boltzmann_floor_override(chip_control_efficacy, 0.12).
narrative_ontology:affects_constraint(chip_control_efficacy, chinese_ai_capability_frontier).
narrative_ontology:affects_constraint(chip_control_efficacy, semiconductor_supply_chain_fragmentation).
narrative_ontology:affects_constraint(chip_control_efficacy, allied_technology_coalition_cohesion).

% DUAL FORMULATION NOTE:
% Semiconductor export controls decompose into three structurally distinct constraints: (1) the coordination mechanism among allied exporters (rope, ε≈0.08), (2) the extraction mechanism targeting Chinese labs (tangled_rope/snare, ε≈0.55-0.70 depending on substitution pathway velocity), and (3) the political-economy constraint on coalition defection (rope with embedded snare, ε≈0.35). This story models the aggregate constraint. Sibling stories decompose by primary victim/beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chip_control_efficacy, institutional, 0.09).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
