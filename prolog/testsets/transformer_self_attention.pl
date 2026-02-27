% ============================================================================
% CONSTRAINT STORY: transformer_self_attention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   The transformer self-attention architecture represents a fundamental
 *   computational constraint that exhibits tangled rope dynamics: it provides
 *   genuine coordination benefits across the AI ecosystem while
 *   simultaneously concentrating capability and excluding interpretability
 *   researchers. The architecture solved the parallelization problem that
 *   made RNNs intractable at scale, enabling the emergence of large language
 *   models. Yet the mechanism by which it achieves this — full pairwise
 *   attention over all positions in a sequence — creates quadratic complexity
 *   that concentrates computational resources at scale-rich institutions and
 *   produces opaque learned representations that resist mechanistic
 *   interpretation. The constraint operates differently across the
 *   observation site: frontier labs experience pure coordination (Rope),
 *   smaller labs face mixed extraction and genuine empowerment (Tangled
 *   Rope), interpretability researchers face a trap (Snare),
 *   efficiency-focused coalitions see a temporary architectural necessity
 *   with a sunset (Scaffold), academic attention theory maintains
 *   increasingly performative justifications (Piton), and the analytical
 *   observer risks naturalizing a choice about inductive bias as an inherent
 *   property of sequence processing (false Mountain). The theater ratio
 *   (0.58) reflects that the academic literature on attention mechanisms is
 *   increasingly decoupled from the engineering decisions that drive actual
 *   architectural development — papers proliferate on attention variants but
 *   model scaling and capability emerge primarily from increases in parameter
 *   count and dataset size, not from theoretical innovations in attention
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Frontier AI Labs (OpenAI, Anthropic, Google DeepMind, Meta): Primary beneficiaries (institutional/arbitrage) — capture scaling advantage and capability concentration; can afford to compute and optimize full-attention mechanisms
 *   - Smaller AI Labs and Academic Groups: Mixed position (moderate/constrained) — benefit from architectural standardization and transfer learning but face resource barriers; cannot afford to train large transformers from scratch
 *   - Interpretability Researchers: Primary victims (powerless/trapped) — face opacity of multi-head attention; forced to work within opaque architecture with no practical exit; bear full cost of black-box models
 *   - Resource-Constrained Practitioners: Secondary victims (moderate/constrained) — need to run models on edge devices or with limited compute; face trade-off between using standard transformers (expensive) or accepting performance degradation from alternatives
 *   - Efficiency-Focused Coalition: Organized agents (organized/constrained) — developing linear attention, sparse attention, knowledge distillation, quantization; building alternative pathways with genuine functional benefits
 *   - Academic Attention Theory Community: Institutional actor (institutional/arbitrage) — publishes attention variants; maintains theoretical framing; experiences own field as increasingly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the choice to avoid strong inductive bias as a law of computation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transformer_self_attention, 0.38).
domain_priors:suppression_score(transformer_self_attention, 0.42).
domain_priors:theater_ratio(transformer_self_attention, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transformer_self_attention, extractiveness, 0.38).
narrative_ontology:constraint_metric(transformer_self_attention, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transformer_self_attention, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transformer_self_attention, tangled_rope).
narrative_ontology:human_readable(transformer_self_attention, "The Transformer Self-Attention Architecture").
narrative_ontology:topic_domain(transformer_self_attention, "technological/machine_learning").

domain_priors:requires_active_enforcement(transformer_self_attention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transformer_self_attention, large_model_labs).
narrative_ontology:constraint_beneficiary(transformer_self_attention, frontier_capability_developers).
narrative_ontology:constraint_victim(transformer_self_attention, interpretability_researchers).
narrative_ontology:constraint_victim(transformer_self_attention, computational_efficiency_seekers).
narrative_ontology:constraint_victim(transformer_self_attention, resource_constrained_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERPRETABILITY RESEARCHER (SNARE) — Trapped in quadratic attention complexity and opacity. Self-attention mechanisms produce outputs that are structurally resistant to interpretation — the multi-head weighting distributions are high-dimensional and causally entangled. No practical exit from the requirement to work within the architecture; must accept that mechanistic understanding of learned attention patterns remains elusive. Maximum experienced extraction: forced to work within an opaque system with no alternative path to influence LLM development.
constraint_indexing:constraint_classification(transformer_self_attention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALLER AI LAB (TANGLED ROPE) — Constrained by computational cost of training transformer models. Self-attention's quadratic complexity in sequence length creates a resource barrier that concentrates capability at scale-rich institutions. Yet the architecture also provides genuine coordination benefit: attention mechanisms enable transfer learning, few-shot adaptation, and composable representations that benefit smaller labs. Mixed extraction and coordination — forced to work within the paradigm but also genuinely empowered by the shared architectural standard.
constraint_indexing:constraint_classification(transformer_self_attention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER AI LAB (ROPE) — Benefits from the self-attention standard as a coordination mechanism. Attention mechanisms enable scaling laws, emergent capabilities, and architectural innovations (sparse attention, Flash Attention) that are their competitive advantage. The architecture is experienced as a pure coordination tool: establishing the shared standard means that incremental improvements in attention mechanisms (better kernels, lower-rank approximations) propagate value to the entire ecosystem while concentrating frontier capability in the hands of organizations that can exploit architectural asymmetries.
constraint_indexing:constraint_classification(transformer_self_attention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EFFICIENCY-FOCUSED COALITION (SCAFFOLD) — Organized efforts (linear attention variants, sparse transformers, knowledge distillation, quantization frameworks) are building alternative pathways that reduce quadratic complexity and resource requirements. These alternatives are genuinely functional — Flash Attention, linear transformers, and compression techniques lower the computational barrier without sacrificing core coordination benefits. The standard self-attention architecture is transitioning from mandatory to one option among several. Sunset clause: estimated 5-10 years as linear-attention variants mature and prove competitive on benchmark tasks.
constraint_indexing:constraint_classification(transformer_self_attention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC ATTENTION THEORY COMMUNITY (PITON) — The formal study of attention mechanisms as a computation primitive has largely become performative. Papers proposing novel attention variants proliferate (multihead, multi-query, grouped-query, sparse, linear, causal, bidirectional, cross, self-cross hybrids) but the core architecture persists unchanged in practice. The academic attention theory community maintains elaborate theoretical justifications and empirical comparisons, but the selection of which variant to use is driven by scaling laws and empirical loss curves, not by theoretical understanding. Theater ratio high (0.58): extensive ablation studies and theoretical framing accompany what is fundamentally an engineering decision. The academic rituals persist through institutional inertia — attention theory remains academically prestigious despite losing functional centrality to the architecture's role.
constraint_indexing:constraint_classification(transformer_self_attention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPUTATION THEORY VIEW (MOUNTAIN) — From a universal computational perspective, some form of attention mechanism (variable-length history contextualization) is a fundamental requirement for processing unbounded sequences. The quadratic complexity emerges from the mathematical requirement to compare all pairs of positions — this is inherent to the problem of 'attend to all relevant history' without strong inductive bias. However, the structural data contradicts the mountain classification: the engine will compute this as a false summit, revealing that 'attention is inherent to sequence processing' naturalizes what is actually a choice to avoid strong inductive bias (recurrence, locality, hierarchy) in favor of learned pairwise interaction.
constraint_indexing:constraint_classification(transformer_self_attention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transformer_self_attention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transformer_self_attention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transformer_self_attention, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transformer_self_attention, TR),
    TR >= 0.70.

:- end_tests(transformer_self_attention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The architecture concentrates capability at well-resourced institutions through computational cost barriers, but this is not maximal extraction because the coordination benefits are genuine and widely distributed. Smaller labs benefit from the standardization, transfer learning, and architectural maturity. The extraction flows primarily in one direction (capability concentration) but is offset by ecosystem-level coordination gains. Suppression (0.42): Moderate. Barriers exist: computational cost excludes many practitioners, interpretability remains intractable, quadratic complexity creates hard scaling limits. Yet suppression is not total — alternatives exist (linear attention, distillation, quantization), open-source implementations lower adoption barriers, and research continues on efficiency variants. Theater ratio (0.58): Moderate-high. The academic attention literature has become increasingly detached from architectural necessity. Hundreds of papers propose attention variants with marginal improvements, yet the core quadratic mechanism persists. The evaluation of which variant to use is driven by scaling laws and empirical loss curves, not by theoretical understanding or architectural insights. The academic framing (various justifications for attention heads, theoretical analyses) is increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The frontier lab and interpretability researcher perspectives exhibit maximum divergence. The frontier lab sees coordination and empowerment (Rope) — attention mechanisms enable the scaling laws that establish their capability advantage and the architectural flexibility that allows innovations like sparse attention and Flash Attention to propagate. The interpretability researcher sees extraction and trapping (Snare) — forced to work within opaque high-dimensional representations with no practical mechanism to understand learned attention patterns. The smaller lab perspective is genuinely mixed (Tangled Rope) — they benefit from standardization and transfer learning but face resource barriers that exclude them from frontier-scale training. The efficiency coalition sees the architecture as temporary (Scaffold) — linear variants and sparse mechanisms are real alternatives that work and are improving. The academic community sees their field as degraded (Piton) — attention theory generates publications but does not drive architectural development. The analytical observer risks false Mountain classification: attention mechanisms solve the genuine problem of sequence modeling without strong inductive bias, but the choice to favor learned representations over built-in structure is contingent, not inherent to computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is determined by the agent's structural position. Frontier labs with arbitrage options and institutional power experience low or negative effective extraction — the architecture enables their competitive advantage. Interpretability researchers with no exit and trapped status experience maximum extraction — they cannot escape the opacity. Smaller labs with constrained exits but also benefits from standardization experience moderate mixed extraction. Efficiency-focused organized agents with constrained exits but agency in building alternatives experience lower extraction than their power level alone would suggest. The academic attention theory community experiences degradation (Piton) because their field's theoretical outputs are not selected by the architecture's development — capability emerges from scaling and empirical optimization, not from theoretical innovations.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by showing that the architecture simultaneously solves a genuine coordination problem (enabling parallel scaling of sequence modeling) while creating asymmetric extraction (concentrating capability and excluding interpretability). The classification as Tangled Rope correctly captures both: (1) beneficiaries (frontier labs, model capability) experience it as coordination, (2) victims (interpretability researchers, resource-constrained practitioners) experience extraction, (3) the architecture requires active enforcement (continued development, optimization, and institutional resource commitment to maintain its dominance against alternatives). The Scaffold perspective is not an alternative classification — it is a real structural feature (efficiency alternatives are genuine and improving). The false Mountain perspective is correctly identified as naturalization: the claim that 'attention is inherent to unbounded sequence processing' confuses mathematical necessity (you need to handle variable-length history) with architectural choice (full pairwise comparison vs. alternatives). The Piton academic theory perspective correctly captures degradation: attention theory has become increasingly performative because capability emerges from scaling and empirical optimization, not from theoretical understanding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quadratic_complexity_necessity,
    'Is the quadratic complexity in sequence length a fundamental feature of attention mechanisms or a contingent implementation choice?',
    'Empirical comparison of linear-attention and sparse-attention variants on diverse sequence tasks; theoretical analysis of what information patterns require full pairwise comparison vs. compressible approximations',
    'If quadratic is necessary: resource concentration is inherent to the architecture (Snare from resource-constrained perspective confirmed). If contingent: alternatives can scale, and Scaffold perspective''s sunset is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quadratic_complexity_necessity, empirical, 'Whether quadratic complexity is fundamental to attention or contingent').

omega_variable(
    interpretability_versus_capability_tradeoff,
    'Does the opacity of multi-head attention weights reflect an information-theoretic tradeoff between interpretability and learned capacity, or is it an artifact of how we train and evaluate models?',
    'Mechanistic interpretability research on attention patterns; comparison of interpretability metrics across models trained with different transparency constraints vs. standard black-box training; analysis of whether architectural simplifications (single-head, gated attention) reduce capacity more than interpretability improves',
    'If tradeoff is fundamental: Interpretability Researcher remains trapped (Snare confirmed). If artifact: alternative architectures could offer both interpretability and capacity, changing classification to Tangled Rope or Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_versus_capability_tradeoff, empirical, 'Whether attention opacity reflects a necessary capability-interpretability tradeoff').

omega_variable(
    alternative_sequence_architecture_viability,
    'Can recurrent, convolutional, or hybrid architectures with strong inductive biases achieve comparable performance to full-attention transformers at scale, or is the absence of built-in structure actually necessary for emergence of general reasoning?',
    'Large-scale training comparisons: transformers vs. gated recurrent units, state-space models, hybrid architectures with learned locality; evaluation on generalization, few-shot reasoning, and transfer learning benchmarks',
    'If alternatives achieve parity: architecture choice is engineering rather than capability requirement (Rope becomes standard perspective). If transformers uniquely enable scaling: organizational concentration persists (Tangled Rope and Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_sequence_architecture_viability, empirical, 'Whether alternative architectures can match transformer scaling performance').

omega_variable(
    efficiency_variant_performance_parity,
    'Will linear-attention variants (Flash Attention, Mamba, linear transformers) achieve genuine performance parity with full quadratic attention, or do they trade efficiency for capability loss?',
    'Longitudinal tracking of efficiency-variant performance on standard benchmarks; measurement of convergence speed, final loss, and downstream task performance relative to full transformers; analysis of whether capability gaps narrow over successive model scales',
    'If parity achieved: Efficiency-Focused Coalition''s Scaffold sunset is real — resource barrier dissolves (Rope becomes viable for more perspectives). If persistent gaps: quadratic attention remains dominant despite alternatives (Snare and Tangled Rope remain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_variant_performance_parity, empirical, 'Whether linear-attention variants achieve performance parity with full transformers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transformer_self_attention, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, transformer_self_attention, theater_ratio, 0, 0.35).
narrative_ontology:measurement(attn_tr_t4, transformer_self_attention, theater_ratio, 4, 0.48).
narrative_ontology:measurement(attn_tr_t8, transformer_self_attention, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, transformer_self_attention, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(attn_be_t4, transformer_self_attention, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(attn_be_t8, transformer_self_attention, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transformer_self_attention, information_standard).
narrative_ontology:affects_constraint(transformer_self_attention, model_scaling_laws).
narrative_ontology:affects_constraint(transformer_self_attention, emergent_llm_capabilities).
narrative_ontology:affects_constraint(transformer_self_attention, ai_resource_concentration).

% DUAL FORMULATION NOTE:
% The transformer self-attention architecture is upstream of specific LLM capability claims. The architecture enables scaling and emergent reasoning, but the specific capabilities (in-context learning, chain-of-thought reasoning, knowledge retention) depend on both the architecture and training data/compute. This story focuses on the architectural constraint; downstream stories decompose specific capability claims from their own structural evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transformer_self_attention, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
