% ============================================================================
% CONSTRAINT STORY: ml_research_resource_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ml_research_resource_concentration, []).

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
 *   constraint_id: ml_research_resource_concentration
 *   human_readable: ML Research Resource Concentration
 *   domain: artificial_intelligence/research_infrastructure
 *
 * SUMMARY:
 *   ML research has undergone rapid resource concentration in the past
 *   decade. Access to compute infrastructure (GPUs, TPUs, large training
 *   clusters) is increasingly concentrated in a small number of
 *   well-resourced institutions: major tech companies (OpenAI, DeepMind,
 *   Meta, Google), wealthy academic labs, and national AI initiatives. This
 *   concentration creates a structural constraint with contradictory
 *   properties: it enables genuine scientific advances (coordinated research
 *   on large models), simultaneously enables asymmetric extraction (resource
 *   holders capture epistemic priority and field direction), and produces
 *   identity-fusion effects (the field's values become aligned with 'scale'
 *   as a proxy for progress). The constraint exhibits high extractiveness
 *   (0.58) driven by career incentives asymmetry and suppression of
 *   resource-efficient research paths. It is being partially contested by the
 *   open-source ML movement (Hugging Face, community-driven models,
 *   efficiency improvements), which represents a scaffold structure with a
 *   civilizational-scale sunset — as training efficiency improves and
 *   open-weights models mature, the extraction mechanism weakens. Theater is
 *   moderate (0.55): research evaluation systems claim to assess merit
 *   independently of resources, but implicitly privilege findings achievable
 *   only at scale, creating performative review layers that do not verify
 *   computational claims.
 *
 * KEY AGENTS:
 *   - Resource-Poor Researchers: Primary victim (powerless/trapped) — locked out of large-scale experiment participation; career advancement requires scale access
 *   - Mid-Tier Research Institutions: Secondary victim (moderate/constrained) — benefit from benchmarks and collaboration but forced into arms-race dynamics; resource contributions extract value
 *   - Well-Resourced Laboratories: Primary beneficiary (institutional/arbitrage) — enjoy scale advantages, publication prestige, talent recruitment; see constraint as pure coordination
 *   - Compute Capital Holders (Cloud/Semiconductor): Extractive beneficiary (institutional/constrained) — capture value through pricing, lock-in, usage data; genuinely coordinate resource provisioning but asymmetrically
 *   - Open-Source ML Movement: Organized challenger (organized/constrained) — building alternative pathways with sunset logic; see concentration as temporary
 *   - Field's Epistemic Diversity: Primary victim (powerless/identity_locked) — research directions requiring efficiency, interpretability, or sparse data are deprioritized; professional identity fused with 'bigger is better'
 *   - Scientific Merit Evaluation System: Institutional actor (institutional/arbitrage) — maintains performative review; sees own function as degraded (Piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ml_research_resource_concentration, 0.58).
domain_priors:suppression_score(ml_research_resource_concentration, 0.62).
domain_priors:theater_ratio(ml_research_resource_concentration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ml_research_resource_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(ml_research_resource_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ml_research_resource_concentration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ml_research_resource_concentration, tangled_rope).
narrative_ontology:human_readable(ml_research_resource_concentration, "ML Research Resource Concentration").
narrative_ontology:topic_domain(ml_research_resource_concentration, "artificial_intelligence/research_infrastructure").

domain_priors:requires_active_enforcement(ml_research_resource_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ml_research_resource_concentration, well_resourced_institutions).
narrative_ontology:constraint_beneficiary(ml_research_resource_concentration, compute_capital_holders).
narrative_ontology:constraint_victim(ml_research_resource_concentration, resource_poor_researchers).
narrative_ontology:constraint_victim(ml_research_resource_concentration, field_epistemic_diversity).
narrative_ontology:constraint_victim(ml_research_resource_concentration, alternative_research_directions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-POOR RESEARCHER (SNARE) — Structurally locked out of high-compute ML research. Cannot access GPU clusters or training infrastructure without affiliation to well-resourced institutions. Career trajectory in ML increasingly requires demonstration of scale — but scale requires resources only concentrated actors control. No meaningful exit option.
constraint_indexing:constraint_classification(ml_research_resource_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER RESEARCH INSTITUTION (TANGLED ROPE) — Experiences both genuine coordination and extraction. Benefits from shared benchmarks, open models, and collaborative infrastructure. Simultaneously faces extraction: must contribute computing resources to consortia, train students on tools designed for scale, participate in arms-race dynamics around model size. Constrained: could exit but at significant cost to competitiveness and graduate recruitment.
constraint_indexing:constraint_classification(ml_research_resource_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELL-RESOURCED LABORATORY (ROPE) — Net beneficiary. Experiences the constraint as pure coordination: resource concentration enables scale, which generates data and insights for the field. Can exit anytime without penalty — has alternatives (consulting, industry, closed-source development). Sees research coordination as advantageous.
constraint_indexing:constraint_classification(ml_research_resource_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPUTE CAPITAL HOLDER (TANGLED ROPE) — Genuine coordination function: provisioning compute infrastructure enables research at scale. Asymmetric extraction: capture value through pricing tiers, usage data, training-on-user-workloads, and lock-in to proprietary frameworks. Constrained: cannot exit without ceding market leadership, but also benefits from ecosystem lock-in. Different institutional position than well-resourced lab — benefits from concentration but through market power rather than epistemic merit.
constraint_indexing:constraint_classification(ml_research_resource_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE ML MOVEMENT (SCAFFOLD) — Organized response to concentration. Hugging Face, PyTorch, open weights models (Llama, Mistral), reduced-scale training techniques. Building alternative pathways with sunset logic: as training efficiency improves and open models mature, resource concentration becomes less extractive. Agents see temporary constraint with technological sunset — efficiency gains reduce compute barrier. Suppression is real but declining.
constraint_indexing:constraint_classification(ml_research_resource_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SCIENTIFIC MERIT EVALUATION SYSTEM (PITON) — Conference and journal review claims to evaluate ideas on merit independent of resources. Theater is high: peer review reads papers, assesses novelty and rigor, but the evaluation system implicitly privileges findings achievable only at scale. Reviewers cannot run 7-billion-parameter models to verify claims. The system persists through institutional inertia — academic reputation, citation prestige — but its function (evaluation independent of resource asymmetry) has largely atrophied. Alternative: preprints and community scrutiny.
constraint_indexing:constraint_classification(ml_research_resource_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: FIELD EPISTEMIC DIVERSITY (SNARE / IDENTITY-LOCKED) — Research directions that require small models, sparse data, interpretability over scale, or resource-efficient methods are structurally deprioritized. The field's identity has become fused with 'bigger is better.' Researchers pursuing alternative paradigms (mechanistic interpretability, sparse training, efficient architectures) face identity lock: being 'inefficient' or 'not scalable' becomes a professional liability even when their research directions are epistemically valuable. No exit option that preserves professional identity in ML.
constraint_indexing:constraint_classification(ml_research_resource_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes genuine coordination function: scaling laws are real, large models reveal emergent capabilities that inform research. Simultaneously recognizes asymmetric extraction: resource concentration filters which questions get answered, which researchers can participate, which failure modes get studied. Civilization-scale perspective shows that concentration is neither immutable (open-source movement, efficiency improvements) nor pure coordination (epistemic diversity cost).
constraint_indexing:constraint_classification(ml_research_resource_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ml_research_resource_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ml_research_resource_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ml_research_resource_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ml_research_resource_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ml_research_resource_concentration, TR),
    TR >= 0.70.

:- end_tests(ml_research_resource_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and increasing. The constraint extracts career opportunities, epistemic priority, and research agenda-setting from resource-poor actors toward resource-rich ones. Extraction is growing (0.32 → 0.58 across interval) because: (a) scaling laws have proven empirically real, making scale increasingly valuable for capability gains; (b) training costs for frontier models have risen exponentially; (c) compute capacity is concentrated in fewer hands. The metric is not at snare-level (≥0.66) because genuine coordination benefits exist (benchmarks, open models, shared techniques) and some mobility remains for well-executed research at lower resource levels. Suppression (0.62): High. Barriers to participation include: access to compute clusters (technical barrier), expertise requirements for scale optimization (skills barrier), career risk of negative results in resource-intensive work (social barrier), and identity lock (cognitive barrier). These are multiple, compound, and enforced by institutional structures. Theater (0.55): Moderate. Peer review claims resource-independence; conference committees evaluate 'ideas' not 'compute'. But the system implicitly privileges work demonstrating scale because larger models have generated most high-impact findings in recent years. Reviewers cannot run models to verify claims. The theater is not as high as traditional peer review (which claims complete independence from resource constraints), but higher than direct outcome observation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from beneficiary-to-victim gradient. The well-resourced lab (institutional/arbitrage) sees Rope because they have genuine coordination benefits and exit options. The resource-poor researcher (powerless/trapped) sees Snare because they face extraction without exit. The scaffold perspective (organized/constrained) is crucial: it shows that the constraint is not immutable. Open-source models, efficiency improvements, and community infrastructure are building alternatives with declining extraction. The piton perspective reveals that the scientific merit system's capacity to evaluate resource-independent merit has largely disappeared — replaced by performative review. The identity-locked perspective (field epistemic diversity) is the most subtle: even if resource barriers were removed, the field's self-concept ('bigger is better') would suppress alternative research. This suggests the constraint operates at multiple levels: material (compute access), epistemic (agenda-setting), and psychological (identity fusion).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base extractiveness (0.58) × f(d) × σ(global=1.2). The resource-poor researcher with d ≈ 0.95 (full target) experiences χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (nearly maximal extraction). The well-resourced lab with d ≈ 0.10 (full beneficiary) experiences χ ≈ 0.58 × -0.10 × 1.2 ≈ -0.07 (subsidy). The mid-tier institution with d ≈ 0.55 (symmetric) experiences χ ≈ 0.58 × 0.65 × 1.2 ≈ 0.45 (moderate extraction). This explains why perspectives diverge so dramatically: the same constraint produces near-total extraction for one agent and near-zero extraction for another, and the difference is entirely structural (d), not perspective error.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through structural clarity of the mixed coordination-extraction hybrid. Genuine coordination exists: scaling laws are real, large-scale experiments generate insights, shared infrastructure enables research that would be impossible in isolation. Asymmetric extraction simultaneously exists: resource concentration filters research agendas, excludes participants, and embeds career incentives that reward scale-chasing even when scientifically suboptimal. Neither can be decomposed away. The constraint is Tangled Rope, not pure extraction (Snare) because the coordination function is real, not pure coordination (Rope) because the extraction is asymmetric and the field has identity-locked with scale as a value proxy. The scaffold perspective resolves temporal mandatrophy: at biographical horizons, resource concentration appears permanent (Snare from powerless view, Rope from beneficiary view). At generational horizons, open-source and efficiency improvements are building alternatives — the constraint is being reshaped. At civilizational horizons, the mechanism becomes visible: the field's identity-fusion with scale is contingent, and epistemic diversity may be recoverable if identity frames shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaling_law_necessity,
    'Are large-scale experiments epistemically necessary for fundamental ML insights, or is scale a sufficient-but-not-necessary condition that dominates the field through resource concentration?',
    'Comparative historical analysis: high-impact insights produced without massive resources (attention mechanisms, layer normalization, prompt engineering); vs insights requiring scale (emergent abilities in language models). Decomposition of causality: does scale reveal phenomena or does it merely enable demonstration?',
    'If necessary: resource concentration is a legitimate structural feature (Rope dominates). If sufficient-but-not-necessary: concentration is extractive (Snare/Tangled Rope dominates), and field is missing alternative research paths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaling_law_necessity, empirical, 'Whether large scale is epistemically necessary or contingently dominant').

omega_variable(
    open_weights_threshold,
    'At what point does open-weights model maturity (Llama, Mistral, etc.) reduce the effective extraction from resource concentration? What capability gap remains closed?',
    'Temporal tracking of open-weights model capabilities vs proprietary SOTA; resource requirements for fine-tuning vs pretraining; publication lag for insights achievable on open models vs proprietary frontier.',
    'If gap narrows to zero: scaffold sunset is real, concentration becomes temporary (Scaffold classification strengthens). If gap persists: open-weights are substitute goods with limits (Scaffold is aspirational, concentration persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_weights_threshold, empirical, 'Maturity gap between open-weights and proprietary frontier models').

omega_variable(
    identity_lock_persistence,
    'If resource barriers to efficient-model research were removed, would the field''s identity fusion with ''scale'' persist as an internalized suppression mechanism?',
    'Longitudinal study of researcher career outcomes in resource-efficient subfields (mechanistic interpretability, sparse training, small-model research) before and after major efficiency breakthroughs. Do barriers persist after removal of material constraints?',
    'If identity lock persists: even with resource access, alternative research directions face cognitive capture and institutional gatekeeping (Snare/identity_locked classification stands). If identity lock dissolves: suppression was primarily structural, concentration is Tangled Rope (mixed coordination and extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether field identity fusion with scale persists after resource barriers decline').

omega_variable(
    compute_price_elasticity,
    'How sensitive is research accessibility to compute pricing? Would 10x efficiency gain or 50% price reduction substantially alter the distribution of who can run large-scale experiments?',
    'Price-sensitivity analysis from survey data; simulation of research participation with different compute costs; historical precedent from GPU cost evolution.',
    'If highly elastic: price/efficiency improvements rapidly enable broader participation (scaffold sunset accelerates). If inelastic: other barriers (expertise, time, integration costs) persist independent of price (suppression remains high).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compute_price_elasticity, empirical, 'Price elasticity of research participation with respect to compute cost').

omega_variable(
    proprietary_moat_permanence,
    'Are the competitive advantages held by well-resourced institutions (proprietary training techniques, dataset advantages, accumulated engineering) sustainable moats, or do they erode as techniques become open and optimized?',
    'Historical analysis of technical advantages: how quickly do closed techniques get replicated once published? Timeline for open-source alternatives. Comparison with historical IT industry concentration patterns.',
    'If moats are permanent: concentration persists indefinitely (Snare classification stands, no sunset). If moats erode: concentration is temporary, open-source movement will succeed (Scaffold sunset is real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_moat_permanence, empirical, 'Durability of competitive moats from resource concentration').

omega_variable(
    funding_bottleneck_circularity,
    'Does funding concentration (NSF, DARPA, corporate research labs control compute budgets) reinforce research concentration, or would decentralized funding structures create alternative research directions at lower budgets?',
    'Analysis of funding source correlation with research direction and resource intensity. Comparison of research agendas in publicly-funded vs corporate-funded vs bootstrapped ML work. Historical precedent from other fields (biology, physics).',
    'If reinforcing: two-level extraction (resource + funding concentration). If decoupled: funding diversification could create epistemic diversity even without compute access (Tangled Rope structure becomes clearer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_bottleneck_circularity, empirical, 'Whether funding concentration reinforces or decouples from research resource concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ml_research_resource_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mlrc_tr_t0, ml_research_resource_concentration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mlrc_tr_t5, ml_research_resource_concentration, theater_ratio, 5, 0.47).
narrative_ontology:measurement(mlrc_tr_t10, ml_research_resource_concentration, theater_ratio, 10, 0.55).
narrative_ontology:measurement(mlrc_tr_t15, ml_research_resource_concentration, theater_ratio, 15, 0.6).

% Extraction over time
narrative_ontology:measurement(mlrc_be_t0, ml_research_resource_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mlrc_be_t5, ml_research_resource_concentration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mlrc_be_t10, ml_research_resource_concentration, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mlrc_be_t15, ml_research_resource_concentration, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ml_research_resource_concentration, resource_allocation).
narrative_ontology:affects_constraint(ml_research_resource_concentration, large_model_capability_convergence).
narrative_ontology:affects_constraint(ml_research_resource_concentration, ml_research_agenda_concentration).

% DUAL FORMULATION NOTE:
% ML research resource concentration decomposes into three structurally distinct constraints with different ε values: (1) compute_access_inequality (ε=0.72, Snare) — material barriers to participation; (2) research_agenda_concentration (ε=0.48, Tangled Rope) — filtering of which questions get prioritized; (3) field_identity_fusion_with_scale (ε=0.42, Tangled Rope / identity_locked) — cognitive lock-in with 'bigger as better'. Each has different beneficiaries, different suppression mechanisms, and different sunset dynamics. This story represents the aggregate constraint across all three decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ml_research_resource_concentration, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
