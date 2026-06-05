% ============================================================================
% CONSTRAINT STORY: transformer_self_attention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transformer_self_attention, []).

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
 *   constraint_id: transformer_self_attention
 *   human_readable: The Transformer Self-Attention Architecture
 *   domain: technological/machine_learning
 *
 * SUMMARY:
 *   The transformer self-attention architecture, introduced by Vaswani et al.
 *   (2017), fundamentally restructured deep learning by enabling
 *   parallelizable sequence processing with learned long-range dependencies.
 *   This architectural innovation solves a genuine coordination problem — how
 *   to process variable-length sequences efficiently on modern parallel
 *   hardware — but its dominance in the AI ecosystem has created both
 *   positive coordination mechanisms and extractive lock-in dynamics. The
 *   constraint exhibits multiple indexical readings: pure coordination (Rope)
 *   from the research community's perspective, mixed coordination-extraction
 *   (Tangled Rope) from downstream developers and end users, a false summit
 *   (Mountain) from the analytical perspective that naturalizes attention as
 *   necessary, and a temporary scaffold (Scaffold) from organized
 *   alternatives building state-space and hybrid competitors. The
 *   extractiveness has increased over the interval (0.12 → 0.28) as
 *   commercial actors have concentrated control over billion-parameter models
 *   and created ecosystem dependencies, while theater ratio has risen (0.25 →
 *   0.42) as the architecture's dominance has become increasingly ritualized
 *   — the assumption that 'transformer = the solution' rather than
 *   'transformer = a solution' is now deeply embedded in research practice
 *   and commercial development.
 *
 * KEY AGENTS:
 *   - LLM Research Community: Primary beneficiary (institutional/arbitrage) — solution to parallelization and clarity problem
 *   - Commercial AI Companies: Primary beneficiary (powerful/arbitrage) — scaling advantage, infrastructure efficiency, ecosystem control
 *   - Downstream Application Developers: Secondary victim (moderate/constrained) — trapped in ecosystem, face computational overhead and dependency on released models
 *   - End Users / Society: Secondary victim (powerless/constrained) — constrained exit from services, bear environmental and concentration costs
 *   - Open-Source Alternative Coalition: Organized actors (organized/constrained) — building state-space models and hybrid architectures as sunset path
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent architectural design as mathematical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transformer_self_attention, 0.28).
domain_priors:suppression_score(transformer_self_attention, 0.35).
domain_priors:theater_ratio(transformer_self_attention, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transformer_self_attention, extractiveness, 0.28).
narrative_ontology:constraint_metric(transformer_self_attention, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(transformer_self_attention, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transformer_self_attention, rope).
narrative_ontology:human_readable(transformer_self_attention, "The Transformer Self-Attention Architecture").
narrative_ontology:topic_domain(transformer_self_attention, "technological/machine_learning").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transformer_self_attention, llm_research_community).
narrative_ontology:constraint_beneficiary(transformer_self_attention, commercial_ai_companies).
narrative_ontology:constraint_beneficiary(transformer_self_attention, downstream_application_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCH COMMUNITY (ROPE) — The transformer architecture solves a genuine coordination problem: how to process variable-length sequences in parallel while maintaining long-range dependencies. From the research community's perspective, this is a coordination mechanism enabling progress. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary through parallelization gains and architectural clarity.
constraint_indexing:constraint_classification(transformer_self_attention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL AI COMPANY (ROPE) — Transformer architecture enables scaling and commodity hardware utilization (GPUs/TPUs), reducing infrastructure costs relative to RNNs. Exit via custom hardware or alternative architectures exists but carries switching costs. d≈0.25, f(d)≈0.08, σ=1.2 → χ≈0.03. Low extraction — architecture is genuinely coordinative for the commercial layer.
constraint_indexing:constraint_classification(transformer_self_attention, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DOWNSTREAM APPLICATION DEVELOPER (TANGLED ROPE) — Developers benefit from transformer tooling (PyTorch, Hugging Face) and model availability but face hidden costs: quadratic attention complexity O(n²), massive pretraining/fine-tuning overhead, and dependence on released model weights. Switching to alternatives (hybrid RNN-transformer, state-space models) faces both technical and ecosystem lock-in barriers. d≈0.60, f(d)≈0.85, σ=1.2 → χ≈0.29. Mixed coordination (access to capabilities) and extraction (computational cost externalization, ecosystem dependency).
constraint_indexing:constraint_classification(transformer_self_attention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: END USER / SOCIETY (TANGLED ROPE) — Citizens benefit from transformer-powered services (translation, writing assistance, information access) but bear hidden costs: concentration of AI power in a few corporations, opaque decision-making in language model outputs, environmental cost of large-scale pretraining (hundreds of thousands of GPU-hours), and training data bias/privacy extraction. Exit option (abstain from services) is increasingly constrained as transformers become infrastructure. d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.37. Significant extraction masked by consumer-facing benefits.
constraint_indexing:constraint_classification(transformer_self_attention, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep mathematical perspective, the transformer's self-attention mechanism emerges from a fundamental principle: solving sequence processing with parallelizable, long-range dependency modeling requires a mechanism that computes pairwise similarity across all tokens. This is not contingent institutional design — it is a mathematical inevitability. Attention is the natural solution to the problem. However, the structural data (ε=0.28, suppression=0.35, theater=0.42) contradicts this. The engine will detect this as a false summit: the architecture's 'inevitability' is being naturalized when in fact attention is one solution among many possible parallel sequence models. State-space models (Mamba), hybrid architectures, and recurrent variants show the design is contingent, not necessary.
constraint_indexing:constraint_classification(transformer_self_attention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: OPEN-SOURCE ALTERNATIVE COALITION (SCAFFOLD) — Organized actors (open-source communities, research labs working on Mamba, state-space models, and hybrid architectures) are building alternative sequence processing mechanisms that avoid transformer quadratic complexity. This perspective sees the transformer hegemony as temporary — a necessary coordination point while scaling via attention was the only well-understood path, but with a sunset as alternatives mature (5-10 years). d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.13. Low effective extraction because organized actors see a genuine path forward and agency to build it.
constraint_indexing:constraint_classification(transformer_self_attention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transformer_self_attention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transformer_self_attention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transformer_self_attention, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(transformer_self_attention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate. The transformer solves a real problem (parallelizable sequence processing) but has created commercial dependency and computational overhead that was not inevitable. The extractiveness reflects that downstream developers and end users bear costs (quadratic complexity, pretraining overhead, environmental impact, model access dependency) that accrue to commercial actors. The moderate value (not high) reflects that the architecture has genuine coordination benefits and that alternatives are emerging. Suppression (0.35): Moderate. Barriers to alternatives include: (1) network effects — ecosystem tooling (PyTorch, Hugging Face, vLLM) is transformer-optimized; (2) knowledge sunk cost — researcher expertise is concentrated in transformer techniques; (3) benchmark inertia — evaluation standards favor transformer-scale models. But suppression is not total — state-space models, sparse attention variants, and hybrid architectures are emerging with real backing. Theater ratio (0.42): Moderate. Some performative activity is present: the assumption that 'bigger transformer = better solution' has motivated wasteful scaling practices (training on 5+ trillion tokens, creating trillion-parameter models) when smaller, task-specific models might be more efficient. However, the architecture itself is not purely performative — attention is functional for its intended problem. The theater reflects overextension and hype-driven scaling, not the core mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival disagreement. The research community and commercial actors see primarily Rope (genuine coordination enabling parallelization and scaling). Downstream developers and end users see Tangled Rope (benefiting from capability access but bearing computational and dependency costs). The analytical observer's Mountain classification is a false summit — the claim that 'attention is necessary' naturalizes what is actually a contingent architectural choice. The open-source coalition sees Scaffold with a real sunset — alternatives like Mamba and state-space models are viable in 5-10 years, creating a genuine exit path. The perspectival gap reflects genuine structural disagreement about whether transformer dominance is inevitable (Mountain/Rope from research perspective) or temporary (Scaffold from organized alternative perspective) or extractive (Tangled Rope from powerless end-user perspective). The engine's perspectival gap detection will highlight that the beneficiary's confidence ('this is the solution') and the victim's experience ('I'm locked in') are incommensurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Research community: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Genuine coordination benefit with exit option (can publish on alternatives). Commercial AI companies: Beneficiary + arbitrage → d≈0.25, f(d)≈0.08. Moderate benefit but substantial lock-in. Downstream developers: Victim + constrained → d≈0.60, f(d)≈0.85. Significant extraction. Benefits from access but trapped in ecosystem, faces switching costs to alternatives. End users: Victim + constrained → d≈0.75, f(d)≈1.10. High extraction. Benefits from services but increasingly cannot exit; bears environmental and privacy costs. Open-source coalition: Organized + constrained → d≈0.35, f(d)≈0.32. Low effective extraction because this agent has agency and real alternatives. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is rejected by structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that transformer dominance is neither pure coordination (Rope) nor pure necessity (Mountain), but a mixture of coordination and extraction with a viable sunset. The research community's Rope perspective is legitimate — attention solves a real problem. The commercial actor's benefit is also legitimate — scaling works. But the Tangled Rope perspectives (developers, end users) reveal that the architecture's dominance has created extraction beyond the core coordination function: computational cost externalization, ecosystem lock-in, and concentration of AI power. The Scaffold perspective (organized alternatives) shows that this is not permanent — alternatives exist and are advancing. The false summit (Mountain) is the dangerous classification here: naturalizing 'transformer must be the solution' prevents exploration of alternatives and locks in the current power structure. The mandatrophy is resolved by accepting that the architecture is simultaneously a real coordination solution AND an extractive lock-in mechanism AND a temporary scaffold being superseded by alternatives. All perspectives are partially correct. The classifier's job is not to pick one but to measure the gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_mechanistic_necessity,
    'Is self-attention a mathematically necessary solution for parallel sequence processing with long-range dependencies, or merely the first scaled solution that worked at scale?',
    'Characterization of solution space: enumerate all known architectures solving long-range dependency modeling; analyze their computational properties and scalability constraints; determine whether attention emerges as the unique optimal solution or as one path among many',
    'If necessary (Mountain): the constraint is immutable, architecture optimization is the only lever. If contingent (Rope/Scaffold): alternatives like state-space models and selective attention variants could be competitive, reducing lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_mechanistic_necessity, conceptual, 'Whether attention is a mathematical necessity or a scaled contingency').

omega_variable(
    quadratic_complexity_hard_limit,
    'Is the O(n²) complexity of full attention a fundamental barrier that persists even with optimal hardware, or is it a barrier that future algorithmic work (sparse attention, hierarchical attention, other variants) can overcome?',
    'Theoretical analysis of lower bounds for long-range dependency computation; empirical comparison of attention variants'' actual complexity vs claimed complexity; benchmarking at extreme sequence lengths (>1M tokens) to identify genuine barriers',
    'If hard barrier: transformer applicability is structurally limited to problems with manageable sequence length, creating suppression via architectural ceiling. If surmountable: quadratic complexity is a transient problem, and suppression declines as variants mature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quadratic_complexity_hard_limit, empirical, 'Whether O(n²) attention complexity is surmountable').

omega_variable(
    commercial_moat_sustainability,
    'How durable is the current commercial moat created by transformer pretraining at billion-parameter scale? Do open-source alternatives (Llama 3.1, others) or alternative architectures erode it faster than proprietary capabilities expand?',
    'Time-series analysis of model performance parity: track when open-source models match closed commercial models in capabilities; measure training cost reduction in open-source; monitor adoption rate of alternative architectures in production systems',
    'If moat erodes quickly (2-3 years): extraction by commercial actors is temporary (Scaffold perspective validated). If moat sustains (5+ years): commercial concentration persists and extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_moat_sustainability, empirical, 'Durability of commercial transformer advantage').

omega_variable(
    environmental_cost_externalization,
    'Is the environmental cost of large-scale transformer pretraining (hundreds of thousands of GPU-hours, gigawatts of electricity) a necessary part of the architecture or an artifact of current scaling practices and commercial incentives?',
    'Lifecycle assessment of pretraining energy cost vs output utility; comparison of pretrain-then-finetune vs from-scratch training for specific tasks; analysis of whether smaller, task-specific transformers achieve comparable performance to giant foundation models with fraction of energy',
    'If necessary: environmental suppression is intrinsic to the architecture (contributes to base suppression score). If artifact: suppression comes from commercial scaling choices, not from the architecture itself, and could be reduced by alternative training regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_cost_externalization, empirical, 'Whether environmental cost is architectural or commercial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transformer_self_attention, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transformer_self_attention, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tran_tr_t5, transformer_self_attention, theater_ratio, 5, 0.33).
narrative_ontology:measurement(tran_tr_t10, transformer_self_attention, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transformer_self_attention, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tran_be_t5, transformer_self_attention, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(tran_be_t10, transformer_self_attention, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transformer_self_attention, information_standard).
narrative_ontology:affects_constraint(transformer_self_attention, large_language_model_scaling).
narrative_ontology:affects_constraint(transformer_self_attention, gpu_semiconductor_dependency).
narrative_ontology:affects_constraint(transformer_self_attention, ai_training_cost_externalization).

% DUAL FORMULATION NOTE:
% The transformer self-attention architecture is a canonical example of a successful coordination mechanism (solving the parallelization problem) that has become extractive through market concentration. This story is upstream of constraints on LLM scaling economics and GPU dependency because transformer's architectural properties (parallelizability, scaling efficiency) drive demand for massive computational resources. As alternatives like state-space models mature, downstream dependencies on transformer-specific infrastructure will weaken.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
