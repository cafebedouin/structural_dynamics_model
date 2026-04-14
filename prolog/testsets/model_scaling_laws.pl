% ============================================================================
% CONSTRAINT STORY: model_scaling_laws
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_scaling_laws, []).

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
 *   constraint_id: model_scaling_laws
 *   human_readable: Model Scaling Laws as Coordination-Extraction Hybrid
 *   domain: machine_learning/computational_science
 *
 * SUMMARY:
 *   Scaling laws in machine learning describe the empirical relationship
 *   between model parameters, training data, and compute, and the resulting
 *   performance improvements. Originating from Kaplan et al. (2020) and
 *   systematized by Hoffmann et al. (2022), these laws have become the
 *   dominant framework for predicting AI capability development and
 *   justifying compute investment. This constraint exhibits the full spectrum
 *   of DR classification because scaling laws simultaneously serve as a
 *   genuine coordination mechanism (shared prediction framework enabling
 *   reproducible research), a contingent institutional arrangement (creating
 *   compute concentration and exclude resource-constrained researchers), and
 *   a potential false summit (naturalizing what may be regime-specific
 *   artifacts as universal laws). The constraint's extractiveness (0.38)
 *   reflects that scaling laws encode both real coordination value and
 *   asymmetric extraction through compute concentration. Theater ratio (0.65)
 *   indicates that scaling laws are increasingly cited as law rather than as
 *   empirical hypothesis subject to continuous falsification — the
 *   performative invocation has grown as architectural variations and
 *   instruction tuning have introduced complications the original framework
 *   did not predict.
 *
 * KEY AGENTS:
 *   - Frontier AI Labs: Primary beneficiaries (institutional/arbitrage) — directly control compute resources needed to validate scaling laws and extract research priority; can exit scaling pursuit without career cost
 *   - Resource-Constrained Researchers: Primary victims (powerless/trapped) — cannot access compute budgets needed for scaling experiments; trapped in credibility gap where non-frontier work is discounted as less advanced
 *   - Funded Research Groups: Secondary victims (moderate/constrained) — face increasing pressure to engage in scaling competitions with unequal resource access; benefit from scaling framework but at unsustainable cost
 *   - Field Knowledge Diversity: Victim collective (powerless/trapped) — areas of AI research not amenable to large-scale compute (cognitive science connections, human-aligned methods, domain-specific efficiency) are devalued as research focus shifts toward scaling
 *   - Open-Science and Efficiency Coalition: Organized beneficiary-victims (organized/constrained) — building alternatives; see sunset mechanism in algorithmic improvement and sparse methods
 *   - Analytical Observer: Risk of false summit (analytical/analytical) — may naturalize contingent architectural regime (transformer + dense training) as universal law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_scaling_laws, 0.38).
domain_priors:suppression_score(model_scaling_laws, 0.48).
domain_priors:theater_ratio(model_scaling_laws, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_scaling_laws, extractiveness, 0.38).
narrative_ontology:constraint_metric(model_scaling_laws, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(model_scaling_laws, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_scaling_laws, tangled_rope).
narrative_ontology:human_readable(model_scaling_laws, "Model Scaling Laws as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(model_scaling_laws, "machine_learning/computational_science").

domain_priors:requires_active_enforcement(model_scaling_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_scaling_laws, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(model_scaling_laws, compute_wealthy_institutions).
narrative_ontology:constraint_victim(model_scaling_laws, resource_constrained_researchers).
narrative_ontology:constraint_victim(model_scaling_laws, field_knowledge_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED RESEARCHER (SNARE) — Trapped by the empirical reality that achieving state-of-the-art performance requires massive compute budgets. Cannot exit without abandoning relevance. Scaling laws became predictive law of the field, forcing participation in the arms race. Maximum extraction: locked into dependence on compute-wealthy institutions for any credible work.
constraint_indexing:constraint_classification(model_scaling_laws, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUNDED RESEARCH GROUP (TANGLED ROPE) — Moderate power with constrained exit. Benefits from scaling laws as a research strategy (clear roadmap for improvement, predictable returns on compute investment). But also bears extraction: must continuously scale to remain competitive; funding flows concentrate at frontier labs; mid-tier groups face choice between abandoning scaling pursuit or competing with unequal resources. Genuine coordination (shared understanding of performance curves) embedded with asymmetric extraction.
constraint_indexing:constraint_classification(model_scaling_laws, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER AI LAB (ROPE) — Institutional actor with arbitrage options (can exit scaling pursuit and switch domains without career cost). Experiences scaling laws as pure coordination: the laws provide transparent prediction of performance gains, enable reproducible research, and create a shared epistemic framework. Net beneficiary with ability to shape the terms of engagement. Low experienced extraction.
constraint_indexing:constraint_classification(model_scaling_laws, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SCIENCE COALITION (SCAFFOLD) — Organized agents (efficient scaling research, parameter-sharing protocols, federated learning) building alternative pathways around the scaling arms race. See the current scaling regime as temporary: better algorithms, sparsity, mixture-of-experts, and distributed training will eventually decouple capability from raw compute. Sunset mechanism: as efficiency techniques mature, the raw-compute extraction mechanism loses force. Structured sunset clause in terms of technological trajectory.
constraint_indexing:constraint_classification(model_scaling_laws, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SCALING LAW PREDICTION FRAMEWORK (PITON) — The original Kaplan et al. (2020) and Hoffmann et al. (2022) scaling laws have become largely performative in current usage. Used as ritual justification for ever-larger models, but the framework's predictive power has degraded: contradictions accumulate (instruction tuning breaks predicted laws, emergence breaks monotonicity, multimodal scaling differs from language-only), yet the framework persists through institutional inertia. The scaling law invocation maintains its force through theater rather than empirical precision. Theater ratio reflects this — scaling laws are cited as law rather than subjected to continuous empirical questioning.
constraint_indexing:constraint_classification(model_scaling_laws, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPUTATIONAL LIMIT VIEW (MOUNTAIN) — From a computational complexity perspective, some scaling relationship between parameters, data, and compute is fundamental: no architecture can escape the information-theoretic constraint that capacity requires dimensionality. Viewing scaling laws as discoverable natural structure of the universe of functions. This perspective risks naturalizing what is actually a contingent regime (the specific power-law form discovered in the 2020s for transformer architectures at specific scales).
constraint_indexing:constraint_classification(model_scaling_laws, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_scaling_laws_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_scaling_laws, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_scaling_laws, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_scaling_laws, TR),
    TR >= 0.70.

:- end_tests(model_scaling_laws_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Scaling laws provide genuine predictive value — performance improvements follow measured curves with high empirical support across a range of conditions. But extractiveness is not negligible because the framework justifies unlimited compute spending and creates a coordination problem (arms race) in which resource-constrained agents cannot participate. The moderate value reflects the balance: real coordination value (~0.15 at t=0) plus growing extraction through compute concentration (~0.13 added by t=4). Suppression (0.48): Moderate-high. Significant barriers include capital requirements for compute, specialized infrastructure knowledge, and the framing of scaling as 'the' path to capability (reducing perceived legitimacy of alternative approaches). But suppression is not total — some groups can collaborate with compute-wealthy institutions, and alternative methods are emerging. Theater ratio (0.65): Moderate-high and increasing. Early scaling law papers (2020-2021) were carefully empirical, testing predictions against new data. Current usage increasingly invokes 'scaling laws' as justification without empirical specificity — the theater has grown as complications (instruction tuning, multimodal scaling, emergent capabilities) have accumulated but been rhetorically managed rather than forcing framework revision. The trajectory from 0.35 to 0.65 captures this drift toward performative usage.
 *
 * PERSPECTIVAL GAP:
 *   Resource-constrained researchers and frontier labs occupy opposite positions in the scaling law extraction mechanism. The frontier lab's low-extraction rope view and the powerless researcher's high-extraction snare view are not arbitrary perspectives on the same invariant phenomenon — they are readings of a structurally asymmetric arrangement where beneficiaries control the terms of engagement. The piton perspective (scaling law framework as degraded ritual) emerges because the framework's predictive precision has declined (instruction tuning, emergence, multimodal regimes break the original laws) but its institutional force has increased (cited as law rather than hypothesis). This is diagnostic of piton dynamics: the mechanism persists through inertia and performative invocation rather than through empirical accuracy. The scaffold perspective (efficiency coalition building alternatives) is not aspirational but structural — sparse models, mixture-of-experts, and algorithmic improvements are showing 2-10x efficiency gains, establishing a real alternative pathway. The mountain perspective risks false summit: computational limits are real, but the specific power-law form discovered in the 2020s is regime-specific, not universal. If the analytical observer naturalizes this regime as law, the framework becomes an ideological lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from the agent's structural position in the extraction flow. Frontier labs (beneficiary + arbitrage) have low d ~0.10 — they experience negative or zero effective extraction because scaling laws align with their interests and resources. Resource-constrained researchers (victim + trapped) have high d ~0.92 — they bear maximum extraction because they are excluded from the mechanism that defines progress. Funded research groups (mixed + constrained) have moderate d ~0.65 — they participate in scaling but at unsustainable cost relative to frontier labs. The efficiency coalition (organized + constrained) has lower d ~0.45 because they have agency and a perceived exit path (algorithmic improvement trajectory). These d values feed into f(d) sigmoid, producing the effective extractiveness chi experienced by each agent. The tangled_rope classification follows because the constraint has genuine coordination value (d-independent) and asymmetric extraction (d-dependent distribution of costs).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is dissolved by recognizing that scaling laws satisfy both Rope and Snare criteria from different observation sites. For frontier actors, scaling laws are Rope (coordination without excessive coercion); for resource-constrained researchers, they are Snare (extraction without coordination). The constraint is Tangled Rope at the field level because (a) genuine coordination value exists (shared prediction framework, reproducible research trajectories), (b) asymmetric extraction exists (compute-wealthy institutions extract capability primacy from resource-constrained ones), and (c) active enforcement exists (the field's consensus that scaling is the primary path to progress). The piton observation shows degradation: the framework is increasingly cited as law rather than continually empirically tested. The scaffold trajectory shows structural sunset: efficiency-based alternatives are maturing. No single type is 'correct' — the presheaf of observations reveals that scaling laws are a hybrid mechanism with different extraction profiles depending on institutional position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaling_law_universality,
    'Are empirically discovered scaling laws universal properties of neural computation or regime-specific artifacts of transformer architectures and current training datasets?',
    'Comparative analysis across architectures (CNNs, RNNs, mixture-of-experts), training regimes (reinforcement learning, unsupervised), and scales (from parameters=10^6 to 10^13). If laws hold across all settings, universality claim is supported; if they break at architectural or scale boundaries, they are regime-specific.',
    'If universal: constraint is closer to mountain (immutable). If regime-specific: constraint is contingent institutional arrangement that will shift as architectures evolve; classification is tangled_rope or snare depending on distribution of exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scaling_law_universality, empirical, 'Whether scaling laws are universal or regime-specific').

omega_variable(
    emergence_and_predictability,
    'Do emergent capabilities (in-context learning, reasoning, code generation) follow predictable scaling curves, or do they appear discontinuously, breaking the monotonic scaling hypothesis?',
    'High-resolution scaling measurements tracking performance on tasks with known emergence phenomena (few-shot learning, arithmetic). Statistical tests for discontinuities. Replication across model families.',
    'If monotonic and predictable: scaling laws provide genuine coordination framework. If discontinuous: predictability breaks at critical points, reducing extraction-enabling power of the framework. Scaling law perspective shifts from mountain toward rope (less deterministic, more sensitive to specific choices).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_and_predictability, empirical, 'Whether emergent capabilities follow predictable scaling').

omega_variable(
    efficiency_alternative_viability,
    'Can scaling law alternatives (sparse models, mixture-of-experts, algorithmic improvements, training efficiency) achieve equivalent frontier capability at 10-100x lower compute cost within 5 years?',
    'Longitudinal tracking of capability-per-unit-compute for sparse and efficient methods. Comparison of resource requirements for equivalent task performance. Cost trajectory analysis.',
    'If viable: scaffold perspective confirmed — sunset is real and structural. Scaling law extraction mechanism loses force as alternatives mature. If not viable: scaling laws remain lock-in constraint for 10+ years. Extract mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_alternative_viability, empirical, 'Viability of efficiency-based alternatives to raw scaling').

omega_variable(
    compute_concentration_causality,
    'Do scaling laws cause compute concentration (because frontier performance requires frontier compute), or do institutional incentives cause both scaling pursuit and compute concentration independently?',
    'Historical analysis of funding flows, institutional incentives, and capability distribution. Counterfactual: would compute have concentrated without scaling laws? Mechanistic analysis of alternative research strategies in absence of scaling prediction.',
    'If scaling laws cause concentration: they are the primary extraction mechanism (snare from resource-constrained view). If coincident with institutional incentives: scaling laws are a rationalization layer on top of pre-existing extraction mechanisms. Affects whether solving scaling law problem would solve extraction problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compute_concentration_causality, conceptual, 'Whether scaling laws cause or rationalize compute concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_scaling_laws, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msl_tr_t0, model_scaling_laws, theater_ratio, 0, 0.35).
narrative_ontology:measurement(msl_tr_t2, model_scaling_laws, theater_ratio, 2, 0.52).
narrative_ontology:measurement(msl_tr_t4, model_scaling_laws, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(msl_be_t0, model_scaling_laws, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(msl_be_t2, model_scaling_laws, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(msl_be_t4, model_scaling_laws, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_scaling_laws, information_standard).
narrative_ontology:affects_constraint(model_scaling_laws, compute_concentration_in_ai).
narrative_ontology:affects_constraint(model_scaling_laws, research_accessibility_barriers).

% DUAL FORMULATION NOTE:
% Scaling laws can be decomposed into two structurally distinct constraints: (1) scaling_law_as_empirical_discovery (ε≈0.08, mountain from scientific perspective) and (2) scaling_law_as_institutional_enforcement (ε≈0.48, tangled_rope from field perspective). The current story models the institutional enforcement regime; the empirical discovery claim should be treated as upstream constraint. Decomposition follows ε-invariance principle: different observables (whether to measure scalings laws as mathematical properties or as field-enforcing institutional mechanisms) yield different ε values, indicating different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_scaling_laws, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
