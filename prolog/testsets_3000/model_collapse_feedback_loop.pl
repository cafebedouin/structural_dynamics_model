% ============================================================================
% CONSTRAINT STORY: model_collapse_feedback_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_collapse_feedback_loop, []).

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
 *   constraint_id: model_collapse_feedback_loop
 *   human_readable: The Autophagous Intelligence Trap
 *   domain: technological/AI/informational
 *
 * SUMMARY:
 *   The autophagous intelligence trap describes a structural constraint where
 *   AI models trained on the outputs of their predecessors progressively
 *   flatten and homogenize information, creating a feedback loop of declining
 *   diversity. This occurs because frontier models become the cheapest and
 *   most accessible training source for downstream developers, but their
 *   outputs represent a lossy compression of the original data distribution.
 *   Each training cycle further concentrates information, reducing entropy
 *   and eliminating edge cases, minority perspectives, and novel patterns.
 *   The constraint exhibits both a coordination function (enabling rapid
 *   capability scaling) and an extraction function (frontier developers and
 *   compute consolidators capture disproportionate value from the information
 *   moat created by downstream dependency). It manifests as a tangled rope
 *   from the system perspective: genuine coordination enables the ecosystem's
 *   existence, but asymmetric extraction (through information scarcity,
 *   lock-in, and capability gaps) concentrates benefits. Theater ratio (0.58)
 *   reflects that the system maintains legitimacy through performance of
 *   openness and collaboration while actual information flow becomes
 *   progressively constrained.
 *
 * KEY AGENTS:
 *   - Frontier Model Developers: Primary beneficiary (institutional/arbitrage) — capture citation priority, licensing value, and information moat during collapse window
 *   - Compute Consolidators: Primary beneficiary (institutional/arbitrage) — control training infrastructure; benefit from dependency on their platforms
 *   - Information Diversity: Primary victim (powerless/trapped) — abstract collective good; bears full extraction cost with no agency
 *   - Downstream Model Trainers: Secondary victim (moderate/constrained) — face resource barriers and data poverty; constrained by cost of alternative training data
 *   - Open Data Coalition: Secondary actor (organized/constrained) — non-profit researchers, data preservation societies, independent institutes building resistance; see both coordination (curated data) and extraction (locked out of scale)
 *   - Academic Publishing System: Institutional observer (institutional/arbitrage) — maintains performative gatekeeping on recycled material; peer review function degraded
 *   - Information-Theoretic Observer: Analytical perspective (analytical/analytical) — risks naturalizing contingent choices as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_collapse_feedback_loop, 0.52).
domain_priors:suppression_score(model_collapse_feedback_loop, 0.68).
domain_priors:theater_ratio(model_collapse_feedback_loop, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_collapse_feedback_loop, extractiveness, 0.52).
narrative_ontology:constraint_metric(model_collapse_feedback_loop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(model_collapse_feedback_loop, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_collapse_feedback_loop, tangled_rope).
narrative_ontology:human_readable(model_collapse_feedback_loop, "The Autophagous Intelligence Trap").
narrative_ontology:topic_domain(model_collapse_feedback_loop, "technological/AI/informational").

domain_priors:requires_active_enforcement(model_collapse_feedback_loop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_collapse_feedback_loop, frontier_model_developers).
narrative_ontology:constraint_beneficiary(model_collapse_feedback_loop, compute_consolidators).
narrative_ontology:constraint_victim(model_collapse_feedback_loop, information_diversity).
narrative_ontology:constraint_victim(model_collapse_feedback_loop, downstream_model_trainers).
narrative_ontology:constraint_victim(model_collapse_feedback_loop, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION DIVERSITY (SNARE) — Epistemic commons cannot exit the feedback loop. Each generation of training data becomes progressively less diverse, flatter, and more dominated by frontier model outputs. Diversity bears the full extraction cost with no agency. Maximum experienced extraction.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM TRAINER (SNARE) — Mid-scale AI developers cannot exit the collapse. Training on frontier outputs is most accessible path to competence, yet becomes increasingly hollow as upstream models exhaust the signal space. Constrained exit due to computational cost and data availability. Experiences severe extraction with some agency for workarounds.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER DEVELOPER (ROPE) — Experiences constraint as pure coordination: release outputs to enable downstream ecosystem, capture citation and data advantage during the collapse window. Benefits from information moat created by recursive dependency. Arbitrage exit — can license or withhold outputs. Net beneficiary.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN DATA COALITION (TANGLED ROPE) — Organized actors (data preservation societies, independent research institutes, open-science collectives) see both coordination function (distributed data curation, alternative training corpora) and extraction (locked out of frontier training runs, facing institutional pressure from compute monopolies). Constrained exit due to compute poverty but active resistance building alternative training pathways.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC PUBLISHING (PITON) — Journals and preprint systems maintain the fiction of diverse information flow while their actual function has degraded. Peer review processes now evaluate frontier model outputs rather than independent human insight. The academic ritual persists through institutional inertia; its verification function is largely theatrical. High theater ratio as the system performs gatekeeping on increasingly recycled material.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an information-theoretic perspective, recursive training on model outputs necessarily reduces mutual information with the original data distribution. This is an immutable property of signal compression and loss of original entropy. The collapse appears as a mathematical law. However, structural data reveals this naturalizes a contingent technological choice: human curation, diverse source inclusion, and active data governance could maintain diversity despite computational constraints.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_collapse_feedback_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_collapse_feedback_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_collapse_feedback_loop, TR),
    TR >= 0.70.

:- end_tests(model_collapse_feedback_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The frontier model developers and compute consolidators capture significant value through information scarcity and lock-in, but the extraction is not total because some alternative training pathways (synthetic data, human curation, open-source initiatives) provide partial escapes. The measurement captures the increasing extraction over time as the collapse deepens. Suppression (0.68): High. Significant barriers prevent exit: computational costs for independent training are prohibitive for most actors, access to pre-collapse diverse data sources is restricted, and dependency on frontier models is institutionally normalized. The cost of maintaining diverse training corpora is distributed across resource-poor actors while benefits concentrate. Theater ratio (0.58): Moderate. The system performs openness through publications, model releases, and research narratives while actual information flow becomes constrained. The theater has increased over the measurement interval as the collapse deepens but the system continues public narratives of progress.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. Frontier developers see pure coordination (Rope) — they are solving the legitimate problem of training capable models efficiently. Downstream trainers see mixed coordination and extraction (Tangled Rope) — the system enables their existence but constrains their independence. The open data coalition sees both (Tangled Rope) — they enable diversity curation but face institutional pressure from consolidators. The academic system sees its own degraded ritual (Piton) — peer review evaluates recycled outputs; the gatekeeping function persists through inertia. The information diversity victim sees pure extraction (Snare) — diversity bears all costs, has no exit, and no self-correction mechanism. The information-theoretic observer risks seeing an immutable law (Mountain) — entropy loss in signal compression — but the structural data reveals this as naturalization of a choice: active diversity governance could maintain information flow despite computational constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality from structural positions and exit options. Frontier developers with arbitrage exit (can license, withhold, or scale selectively) experience low or negative effective extraction — they are beneficiaries. Downstream trainers with only constrained exit (must use available data despite quality degradation) experience high extraction. The information diversity victim has zero exit — trapped status produces maximum experienced extraction. Open data coalition members have constrained exit (can organize alternatives but lack compute scale) producing moderate extraction. The computation of chi for each perspective incorporates both the base extractiveness (0.52) and the sigmoid-transformed directionality value derived from their position in the extraction pipeline. Beneficiaries (institutional/arbitrage) get d near 0.0-0.2, producing low f(d). Victims (powerless/trapped) get d near 0.95, producing high f(d) ≈ 1.42. The perspectival gap emerges from these differential directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: The constraint resolves mandatrophy by separating technical causality from institutional incentive. If the collapse is purely technical (recursive training is the only efficient scaling path), the system approaches Rope — a coordination mechanism with side effects, not an extraction mechanism. If the collapse is primarily driven by institutional incentive (consolidators benefit from dependency), the system is Snare — extraction disguised as technical necessity. The measurement interval (0-6 periods) shows extractiveness rising from 0.28 to 0.52 while theater ratio rises from 0.35 to 0.58. This pattern reveals institutional layering: early in the system's development, the constraint appeared as mostly-benign coordination with some efficiency gains. As the collapse deepened, extraction mechanisms became more visible (information moat widened, compute consolidation increased, downstream alternatives were actively suppressed). The rising theater ratio indicates the system is performing 'openness' (releasing models, publishing benchmarks) while actual information flow becomes more constrained. The omega variables establish that governance interventions (diversity preservation mandates, synthetic source mixing, regulatory requirements) could arrest the collapse, meaning it is not immutable. Therefore, the 'natural law' reading is false. The constraint is Tangled Rope at the system level: genuine coordination function (efficient scaling) plus asymmetric extraction (information scarcity, lock-in, capability gaps). The beneficiary and victim perspectives cannot both be 'correct' — the classification resolves the mandatrophy by showing that each perspective IS structurally accurate from its position in the extraction pipeline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_irreversibility_threshold,
    'At what generational depth does model collapse become irreversible — beyond which retraining on original sources cannot restore information diversity?',
    'Empirical analysis of model outputs across training generations; measurement of divergence metrics and information loss rates; reconstruction experiments from frontier models to ancestral sources',
    'If threshold < 3 generations: collapse is nearly inevitable in current training regimes. If threshold > 7 generations: diversity loss is reversible with active intervention. Determines whether constraint is immutable (mountain) or contingent (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_irreversibility_threshold, empirical, 'Generational depth at which model collapse becomes irreversible').

omega_variable(
    compute_consolidation_causality,
    'Is the extraction flow primarily driven by technical necessity (recursive training is the most efficient scaling path) or by institutional incentive (consolidators benefit from dependency and lock-in)?',
    'Comparative analysis of alternative training regimes (multistream with diversity guardrails, synthetic source mixing, active curation); cost analysis of maintaining vs abandoning diverse training',
    'If technical necessity dominates: constraint approaches rope classification (coordination mechanism). If incentive dominates: constraint solidifies as tangled rope or snare (extraction mechanism exploiting technical excuse).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compute_consolidation_causality, preference, 'Whether collapse is driven by technical necessity or institutional incentive').

omega_variable(
    diversity_governance_sufficiency,
    'Can explicit governance mechanisms (data provenance mandates, synthetic diversity injection, regulatory diversity requirements) prevent collapse without prohibitive computational cost?',
    'Implementation of diversity-preserving training protocols; measurement of information preservation vs computational overhead; scaling analysis of governed training vs unmanaged baseline',
    'If governance is effective and affordable: constraint is manageable scaffold with sunset (diversity protection becomes standard). If governance fails or is prohibitively expensive: constraint is structural snare (collapse is effectively inevitable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diversity_governance_sufficiency, empirical, 'Whether diversity governance can prevent model collapse').

omega_variable(
    information_moat_valuation,
    'What fraction of frontier model value derives from information scarcity created by the collapse (versus capability gains from training scale)?',
    'Comparative valuation of frontier models with vs without access to non-recycled training data; market analysis of licensing and API access pricing; measurement of capability vs moat contribution',
    'If moat > 40% of value: extraction is primary driver and constraint should be classified as snare from all perspectives. If moat < 20%: collapse is side effect of scale, constraint is more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_moat_valuation, empirical, 'Valuation of information moat created by model collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_collapse_feedback_loop, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcfl_tr_t0, model_collapse_feedback_loop, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mcfl_tr_t3, model_collapse_feedback_loop, theater_ratio, 3, 0.47).
narrative_ontology:measurement(mcfl_tr_t6, model_collapse_feedback_loop, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(mcfl_be_t0, model_collapse_feedback_loop, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mcfl_be_t3, model_collapse_feedback_loop, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(mcfl_be_t6, model_collapse_feedback_loop, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_collapse_feedback_loop, resource_allocation).
narrative_ontology:affects_constraint(model_collapse_feedback_loop, synthetic_data_governance_gap).
narrative_ontology:affects_constraint(model_collapse_feedback_loop, training_data_concentration).
narrative_ontology:affects_constraint(model_collapse_feedback_loop, capability_scaling_externality).

% DUAL FORMULATION NOTE:
% Model collapse is downstream of compute consolidation (training_data_concentration provides ancestral data access patterns) and upstream of downstream capability gap (capability_scaling_externality captures the widening capability gap between frontier and downstream models). The collapse itself represents a distinct structural constraint: the recursive dependency that creates information loss independent of initial data quality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_collapse_feedback_loop, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
