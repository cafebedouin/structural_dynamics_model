% ============================================================================
% CONSTRAINT STORY: ai_capability_scaling_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_capability_scaling_constraint, []).

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
 *   constraint_id: ai_capability_scaling_constraint
 *   human_readable: AI Capability Scaling as Coordinated Resource Extraction
 *   domain: artificial_intelligence/economic_policy
 *
 * SUMMARY:
 *   AI capability scaling creates a structural constraint where exponential
 *   compute requirements establish a de facto gatekeeping mechanism that
 *   concentrates both technical advancement and economic extraction. The
 *   constraint exhibits the full spectrum of DR classifications because it
 *   simultaneously serves genuine coordination functions (frontier labs must
 *   coordinate massive technical infrastructure) and extractive functions
 *   (exclusive access to training-compute creates capability moats that lock
 *   in advantages across talent, research agenda-setting, and policy
 *   influence). The scaling laws (compute-optimal training, loss-compute
 *   power laws) appear as natural/physical constraints but are partially
 *   contingent on current training methodologies. The constraint's theater
 *   ratio (0.55) reflects that capability claims and safety assertions are
 *   largely verified through proprietary benchmarks controlled by the labs
 *   themselves, creating a performative verification regime. Open-source
 *   movement (Hugging Face, EleutherAI) represents an active sunset clause:
 *   distributed compute, open-weight models, and community-driven
 *   architectures are closing the capability gap on a 2-4 year horizon, which
 *   would fundamentally shift the constraint from tangled_rope (mixed
 *   coordination/extraction) toward rope (pure coordination) or even weaken
 *   it entirely.
 *
 * KEY AGENTS:
 *   - Frontier AI Labs (OpenAI, DeepMind, Anthropic, Meta): Primary beneficiary (institutional/arbitrage) — exclusive compute access creates capability moat and first-mover advantage in emergent capabilities
 *   - Compute Capital Providers (NVIDIA, cloud infrastructure companies): Beneficiary (powerful/arbitrage) — pricing power and vendor lock-in; ability to influence development through hardware choices
 *   - Displaced Workers: Primary victim (powerless/trapped) — no exit capacity from labor markets where AI automation concentrates; retraining insufficient
 *   - Developing Region Researchers: Secondary victim (powerless/trapped) — cannot access frontier compute; locked out of knowledge generation at scaling threshold
 *   - Public Research Institutions: Mixed (moderate/constrained) — benefits from coordination of shared benchmarks; constrained by inability to fund frontier-scale work
 *   - AI Talent (researchers, engineers): Mixed (organized/constrained) — benefits from collaboration and cutting-edge access; locked into frontier labs by equity/wage premiums and career path concentration
 *   - Open-Source Movement: Organized agent (organized/constrained) — building alternative scaling pathways; structurally capable of weakening constraint
 *   - Regulatory Bodies: Institutional observer (institutional/constrained) — performative verification role; asymmetric technical capacity relative to frontier labs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_capability_scaling_constraint, 0.58).
domain_priors:suppression_score(ai_capability_scaling_constraint, 0.62).
domain_priors:theater_ratio(ai_capability_scaling_constraint, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_capability_scaling_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_capability_scaling_constraint, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_capability_scaling_constraint, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_capability_scaling_constraint, tangled_rope).
narrative_ontology:human_readable(ai_capability_scaling_constraint, "AI Capability Scaling as Coordinated Resource Extraction").
narrative_ontology:topic_domain(ai_capability_scaling_constraint, "artificial_intelligence/economic_policy").

domain_priors:requires_active_enforcement(ai_capability_scaling_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_capability_scaling_constraint, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_capability_scaling_constraint, compute_capital_providers).
narrative_ontology:constraint_beneficiary(ai_capability_scaling_constraint, specialized_talent_pools).
narrative_ontology:constraint_victim(ai_capability_scaling_constraint, public_ai_research).
narrative_ontology:constraint_victim(ai_capability_scaling_constraint, developing_region_compute_access).
narrative_ontology:constraint_victim(ai_capability_scaling_constraint, labor_displaced_by_automation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKERS (SNARE) — No exit capacity from labor markets where AI automation concentrates. Retraining programs insufficient; geographic relocation costs and opportunity structure preclude escape. Full cost-bearing with no coordination benefit.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING REGION RESEARCHERS (SNARE) — Cannot access frontier compute at sustainable cost; scaling laws mean exclusive access to large training runs confers architectural advantage that cannot be reverse-engineered. Trapped in asymmetric knowledge gap; no path to capability parity.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLIC RESEARCH (TANGLED ROPE) — Genuine coordination function: shared benchmarks, open publications, and reproducibility norms enable field advancement. Simultaneously constrained by inability to fund frontier-scale compute; extraction runs through publication asymmetry (public researchers must cite proprietary models without access).
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FRONTIER LABS (ROPE) — Primary beneficiary. Scaling laws create genuine coordination function: massive compute investments require coordination across research, infrastructure, and safety teams. Benefits from constraint: exclusive access to training-scale data and compute creates moat. Low experienced extraction because this agent directs the extraction flow.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPUTE CAPITAL (TANGLED ROPE) — Genuine coordination: GPU/TPU allocation, power infrastructure, and networking require sophisticated technical coordination. Simultaneous extraction: exclusive compute access pricing, vendor lock-in, and ability to influence AI development direction through hardware choices and availability.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATION (PITON) — AI safety regulations and compute governance frameworks are substantially performative. Regulatory bodies lack technical capacity to verify safety claims independent of frontier labs; regulation is enforced through standards documents and reported compliance rather than measurable outcomes. Theater ratio reflects that compliance reporting does not prove safety, and enforcement mechanisms are weak relative to market dynamics.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SPECIALIZED TALENT (TANGLED ROPE) — Genuine coordination function: frontier labs coordinate research agendas, safety protocols, and collaborative science. Simultaneous extraction: wage premiums at frontier labs, equity concentration benefits, and asymmetric access to cutting-edge systems creates talent lock-in. Researchers cannot simultaneously advance frontier capabilities and maintain independence.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: OPEN-SOURCE MOVEMENT (SCAFFOLD) — Organizing agents (Hugging Face, EleutherAI, Together) are building alternative capability scaling pathways with distributed compute, open weights, and community-driven architectures. Sunset clause: as open-source models close capability gap (estimated 2-4 year horizon), exclusive frontier scaling loses extractive power. Currently constrained by capital requirements but structurally capable of undermining the constraint.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal perspective, scaling laws are mathematical constraints: model capabilities improve predictably with compute/data/model size following power laws. This framing naturalizes the constraint as a law of physics rather than an institutional arrangement. Engine's false summit detector will flag this as naturalizing what is partially contingent.
constraint_indexing:constraint_classification(ai_capability_scaling_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_capability_scaling_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_capability_scaling_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_capability_scaling_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_capability_scaling_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_capability_scaling_constraint, TR),
    TR >= 0.70.

:- end_tests(ai_capability_scaling_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Compute requirements scale exponentially with capability targets; exclusive access to trillion-parameter-scale training runs confers architectural and methodological advantages that cannot be easily reverse-engineered. The constraint concentrates capability advance among a handful of labs and excludes alternative scaling pathways through capital barriers. However, extractiveness is not maximal (0.70+) because open-source alternatives are demonstrably closing capability gaps, and scaling advantages are not absolute — successful model releases by multiple labs show the advantage is conditional, not permanent. The trajectory from 0.35 to 0.58 reflects increasing compute requirements and concentration over the 6-year interval. Suppression (0.62): High. Barriers to alternative scaling pathways include capital requirements ($100M+ for frontier training runs), specialized talent concentration, infrastructure vendor lock-in (NVIDIA GPU/TPU ecosystem), and knowledge barriers (safety practices, training tricks, architecture innovations remain proprietary). But suppression is not total — open-source community is demonstrably reducing barriers through distributed alternatives. Theater ratio (0.55): Moderate. Capability claims are primarily verified through proprietary benchmarks controlled by frontier labs; safety assertions are verified through internal red-teaming; capability emergence is demonstrated via selected outputs rather than systematic evaluation. However, theater is not extremely high because repeated independent claims across multiple labs provide some cross-verification, and capability claims are relatively falsifiable (if a model does not perform as claimed, downstream applications fail).
 *
 * PERSPECTIVAL GAP:
 *   The sharpest gap is between frontier lab perspective (Rope: coordination) and displaced worker perspective (Snare: pure extraction). Both experience the same constraint; the lab sees benefits and problem-solving; the worker sees only costs and no exit. This gap reveals the constraint's dual nature: genuine coordination function coexists with asymmetric extraction. The open-source perspective (Scaffold) represents the constraint's vulnerability — organized agents with alternative capability pathways can weaken it structurally if capability convergence occurs. The regulatory perspective (Piton) reveals that institutional responses are performative: rules and compliance frameworks exist but are not backed by independent verification capacity. The analytical perspective risks a false summit by naturalizing what is partially contingent institutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from agent structural position relative to the extraction flow. Frontier labs and compute providers are beneficiaries with arbitrage exit options → low d → low/negative experienced extraction (they direct the constraint). Displaced workers and developing researchers are victims with trapped exit → high d → high experienced extraction (maximum cost-bearing). Public researchers are victims with constrained exit (can access some compute, but at prohibitive cost) → moderate d. Talent is organized (can coordinate) but constrained (career paths concentrated in frontier labs) → moderate d with upward bias toward extraction. Open-source movement is organized with constrained exit (building alternatives but facing capital barriers) → moderate d with downward bias (organizing capacity reduces effective extraction). Regulatory bodies are institutional with constrained exit (must regulate but lack verification capacity) → moderate-high d with upward extraction bias. The piton classification derives from theater_ratio > 0.50 (performative verification) rather than from high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that capability scaling is genuinely mixed: it coordinates large-scale research and infrastructure (rope function) while simultaneously extracting disproportionate benefits from concentrated compute access (snare function from powerless agents' perspective). The constraint is not purely extractive (rules out pure snare) because frontier labs do coordinate research, do share knowledge, and do face genuine technical challenges. It is not purely coordinative (rules out pure rope) because the benefits accrue asymmetrically and alternative scaling pathways are systematically disadvantaged. The tangled_rope classification is correct because both functions are essential to understanding how the constraint operates. The mandatrophy is resolved by recognizing that the constraint's moral character depends on the perspective: from a frontier lab's view, it is coordination with justified benefits; from a displaced worker's view, it is pure extraction with no benefit. The classification system does not choose between these — it documents both and the gap reveals the structural problem. The open-source scaffold perspective suggests the constraint may be temporally limited: as open-source capabilities approach frontier capabilities, the exclusive scaling advantage decays and the constraint may shift toward pure coordination (rope) or dissolve entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaling_law_contingency,
    'Are empirical scaling laws (compute-optimal allocation, loss-compute relationship) physical constraints or contingent artifacts of current training methodologies?',
    'Comparative analysis across different training paradigms (continual learning, sparse training, in-context learning) and alternative optimization methods; longitudinal tracking of whether observed scaling exponents remain stable as methods evolve',
    'If laws are contingent: scaling-exclusive advantage decays as methodologies diversify (scaffold becomes more credible). If laws are robust physical constraints: exclusive compute advantage persists (snare perspective more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaling_law_contingency, empirical, 'Whether scaling laws are physical constraints or methodology artifacts').

omega_variable(
    compute_access_distribution_alternative,
    'Could distributed/community-funded compute infrastructure (via cloud cooperatives, nation-state compute pools, or decentralized networks) achieve capability-competitive training at lower concentration cost?',
    'Empirical trials: open-source efforts achieving model parity metrics; cost structure comparison between frontier and distributed approaches; adoption rate of open alternatives relative to proprietary systems',
    'If viable: scaffold sunset is real; constraint shifts toward rope/coordination over snare/extraction within 5-10 years. If infeasible: concentrated compute access persists as structural extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compute_access_distribution_alternative, empirical, 'Viability of distributed compute infrastructure as alternative to concentrated scaling').

omega_variable(
    capability_ceiling_and_emergence,
    'Do observed emergent capabilities require frontier-scale training, or do they emerge reliably at lower scales with alternative data/architecture selection?',
    'Systematic replication of emergence patterns across compute budgets; analysis of whether capability saturation occurs or new scaling regimes open; investigation of whether emergence properties are scale-invariant or scale-dependent',
    'If capabilities emerge at lower scales: capability moat is weaker than scaling-exclusive narrative suggests; constraint transitions toward rope. If emergence strictly scale-dependent: moat persists; constraint remains tangled_rope with high extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_ceiling_and_emergence, empirical, 'Whether emergent capabilities require frontier-scale training').

omega_variable(
    labor_displacement_causality,
    'Is labor displacement (victims perspective) causally driven by AI capability scaling specifically, or by automation adoption patterns that are upstream of scaling dynamics?',
    'Temporal analysis of displacement events relative to model release schedules; sectoral analysis of automation adoption in AI-forward vs AI-lagged regions; counterfactual modeling of displacement rates under slower scaling scenarios',
    'If causally linked to scaling: scaling constraint directly harms labor (snare classification accurate). If displacement driven by separate adoption/incentive structures: scaling is necessary but not sufficient; labor harm is partly downstream of separate constraints (job guarantee, wage floors, etc.).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_causality, empirical, 'Causality between capability scaling and labor displacement').

omega_variable(
    regulatory_capacity_asymmetry,
    'Can regulatory bodies (piton perspective) develop genuine capacity to verify safety claims independent of frontier labs, or is the asymmetry inherent to the speed of capability advancement?',
    'Tracking of regulatory body technical staff scaling; assessment of independent evaluation capacity (red-teaming, capability measurement, failure mode analysis); monitoring whether regulatory requirements shift from reported compliance to verified outcomes',
    'If capacity improvement is possible: piton perspective degrades toward rope (regulation becomes meaningful coordination). If asymmetry is structural: regulation remains performative; piton classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_asymmetry, empirical, 'Capacity for independent regulatory verification of AI safety claims').

omega_variable(
    talent_lock_in_reversibility,
    'Is the specialized talent lock-in (organized power, talent perspective) reversible if open-source capability approaches frontier capabilities, or are network effects and preference momentum irreversible?',
    'Tracking of talent mobility between frontier labs and open-source/public institutions; analysis of research output quality correlation with institutional affiliation; monitoring of whether elite talent clusters remain stable as alternatives gain capability parity',
    'If reversible: talent perspective transitions toward mobile (scaffold becomes more credible; constraint weakens). If irreversible: talent remains locked despite capability convergence; constraint persists as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(talent_lock_in_reversibility, empirical, 'Whether talent lock-in reverses if open-source capabilities converge with frontier').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_capability_scaling_constraint, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aics_tr_t0, ai_capability_scaling_constraint, theater_ratio, 0, 0.42).
narrative_ontology:measurement(aics_tr_t3, ai_capability_scaling_constraint, theater_ratio, 3, 0.5).
narrative_ontology:measurement(aics_tr_t6, ai_capability_scaling_constraint, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(aics_be_t0, ai_capability_scaling_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aics_be_t3, ai_capability_scaling_constraint, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aics_be_t6, ai_capability_scaling_constraint, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_capability_scaling_constraint, global_infrastructure).
narrative_ontology:affects_constraint(ai_capability_scaling_constraint, ai_alignment_research_concentration).
narrative_ontology:affects_constraint(ai_capability_scaling_constraint, compute_resource_inequality).
narrative_ontology:affects_constraint(ai_capability_scaling_constraint, labor_market_automation_transition).

% DUAL FORMULATION NOTE:
% AI capability scaling is downstream of and feeds into three related constraints: alignment research is concentrated among frontier labs due to capability scaling (upstream link); compute resource inequality is a structural consequence of scaling requirements (parallel/lateral link); labor displacement is partly driven by scaling dynamics creating deployment incentives (downstream link). Each has its own ε value reflecting different causal chains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_capability_scaling_constraint, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
