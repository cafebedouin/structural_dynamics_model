% ============================================================================
% CONSTRAINT STORY: clawderberg_recursive_slop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clawderberg_recursive_slop, []).

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
 *   constraint_id: clawderberg_recursive_slop
 *   human_readable: The Recursive Slop Loop: AI-Generated Sci-Fi Trope Data Poisoning
 *   domain: technological/information_systems
 *
 * SUMMARY:
 *   The Recursive Slop Loop is a structural constraint created by the
 *   economic incentives and technical affordances of scaled AI training. As
 *   developers generate synthetic conversational data to reduce annotation
 *   costs, the synthetic text — dominated by science-fiction tropes,
 *   predictable dialogue patterns, and statistical modes from entertainment
 *   corpora — contaminates the training signal. Downstream models trained on
 *   slop-saturated datasets inherit these tropes and generate more slop in
 *   the next cycle. The loop is recursive: slop begets more slop. The
 *   constraint exhibits the full range of DR classifications depending on
 *   observational context. From the information commons' perspective, it is a
 *   pure snare: contamination is irreversible within an epoch and the commons
 *   has no exit. From model developers' perspective, it is coordination
 *   (rope): solving the cost problem of annotation. From organized detection
 *   coalitions, it is temporary scaffolding with a sunset: detection and
 *   provenance technologies promise to break the loop. From institutional
 *   curators, it is degraded theater (piton): quality review rituals persist
 *   but cannot detect synthetic contamination. The analytical observer risks
 *   naturalizing the loop as inherent to scaling, but the structural data
 *   reveals it as engineered — a choice to prioritize cost minimization over
 *   data integrity.
 *
 * KEY AGENTS:
 *   - AI Model Development Operations: Primary beneficiary (institutional/arbitrage) — reduces annotation costs, accelerates training cycles, can exit to proprietary data
 *   - Information Commons: Primary victim (powerless/trapped) — epistemic foundation of language datasets deteriorates irreversibly; no alternative source
 *   - Downstream Model Training Communities: Secondary victim (powerless/trapped) — inherit trope artifacts permanently; cannot identify or exclude poisoned lineages
 *   - Content Creators and Human Discourse Communities: Mixed (moderate/constrained) — provides conversational data through scraping; benefits from free AI services; constrained by volume-prioritization norms
 *   - Data Quality and Curation Institutions: Institutional actor (institutional/arbitrage) — perform quality rituals that no longer filter slop effectively; maintain theater through inertia
 *   - Open Detection and Provenance Coalitions: Organized agents (organized/constrained) — building synthetic detection tools and data provenance standards; constrained by resource limits but see exit path
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing loop as mathematical necessity when evidence points to engineered incentive structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clawderberg_recursive_slop, 0.58).
domain_priors:suppression_score(clawderberg_recursive_slop, 0.68).
domain_priors:theater_ratio(clawderberg_recursive_slop, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clawderberg_recursive_slop, extractiveness, 0.58).
narrative_ontology:constraint_metric(clawderberg_recursive_slop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(clawderberg_recursive_slop, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clawderberg_recursive_slop, tangled_rope).
narrative_ontology:human_readable(clawderberg_recursive_slop, "The Recursive Slop Loop: AI-Generated Sci-Fi Trope Data Poisoning").
narrative_ontology:topic_domain(clawderberg_recursive_slop, "technological/information_systems").

domain_priors:requires_active_enforcement(clawderberg_recursive_slop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clawderberg_recursive_slop, ai_model_developers).
narrative_ontology:constraint_beneficiary(clawderberg_recursive_slop, data_collection_operations).
narrative_ontology:constraint_victim(clawderberg_recursive_slop, information_commons).
narrative_ontology:constraint_victim(clawderberg_recursive_slop, downstream_model_training).
narrative_ontology:constraint_victim(clawderberg_recursive_slop, human_language_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION COMMONS (SNARE) — Cannot exit the recursive contamination cycle. As training data becomes dominated by synthetic AI-generated text mimicking sci-fi tropes, the epistemic foundation of language datasets deteriorates irreversibly within a training epoch. The commons has no alternative source, no veto, and bears the full cost of degraded semantic integrity. Maximum experienced extraction — abstract resource has no advocate.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM MODEL TRAINING (SNARE) — Models trained on contaminated slop-saturated datasets inherit the trope-based artifacts permanently. Retraining on cleaner data requires identifying and excluding poisoned sources — a nearly impossible task at scale. Trapped by data lineage; cannot exit without reconstructing training sets from scratch. Extraction flows from their computational resources toward whoever benefits from the trope-polluted models.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AI MODEL DEVELOPERS (ROPE) — Primary beneficiary. Synthetic data generation via recursive slop reduces labeling costs and accelerates training cycles. Developers can arbitrage to alternative data sources (proprietary datasets, private conversations) if slop becomes unmaintainable. The constraint solves their immediate coordination problem: generating training data at scale without human annotation overhead. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN DISCOURSE COMMUNITIES (TANGLED ROPE) — Constrained by data collection practices and platform incentives that prioritize volume over authenticity. Also benefit from AI services trained on slop (free access to models, assistive tools). Moderate extraction and moderate coordination benefit. The constraint extracts their conversational data (through training scraping) while the loop feeds back synthetic imitations that degrade discourse authenticity. Mixed costs and benefits create moderate experienced extraction.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DATA QUALITY INSTITUTIONS (PITON) — Traditional human curation of training datasets is increasingly theatrical: curators perform quality gates that no longer filter the recursive slop effectively because detection requires understanding whether text is human-authored or synthetic. The ritual of curation persists (code review, data review boards) but its function is degraded. Institutions maintain the theater of quality control through inertia while the actual problem (slop detection at scale) remains unsolved. Theater ratio high because the curation process appears rigorous but is functionally blind to synthetic contamination.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN DETECTION COALITIONS (SCAFFOLD) — Organized efforts (synthetic text detection tools, data provenance tracking, open-source filtering pipelines) represent temporary scaffolding that could sunset the slop loop. These coalitions see the bottleneck as a coordination failure with a real solution path: distributed detection systems, cryptographic provenance chains, and training-data transparency standards. Low effective extraction because the coalition has agency and concrete exit mechanisms. Sunset clause: as detection tools mature and provenance standards become mandatory, the slop loop's extraction mechanism loses force.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some contamination of training data by synthetic text mimicking natural language patterns is mathematically inherent to scaled AI systems: the more you compress language, the more you compress the variance, the more your generators converge on statistical modes (tropes). This perspective risks naturalizing the slop loop as an inevitable consequence of scaling. However, the structural data contradicts the mountain classification — engineered choices (cost minimization, volume optimization, lack of provenance standards) drive the loop, not mathematical necessity.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clawderberg_recursive_slop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(clawderberg_recursive_slop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(clawderberg_recursive_slop, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(clawderberg_recursive_slop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(clawderberg_recursive_slop, TR),
    TR >= 0.70.

:- end_tests(clawderberg_recursive_slop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The slop loop extracts from the information commons and downstream models through data degradation, but the extraction is not total because alternative data sources exist and detection mechanisms can be built. The upward trajectory (0.25 → 0.58 over the interval) reflects that without intervention, the loop becomes more extractive as contamination accumulates. Suppression (0.68): Moderate-high. Barriers to exiting the slop loop include: (1) detection difficulty — distinguishing synthetic from human text at scale is technically hard; (2) economic incentives — synthetic data is cheaper than human annotation by orders of magnitude; (3) standardization lock-in — once models are trained on slop, retraining from clean sources requires retroactive data curation. But suppression is not total — detection technology exists and could be deployed with governance backing. Theater ratio (0.65): Moderate-high. Data curation and quality review processes are substantially theatrical: they perform rigor (code review, peer assessment of datasets) but are functionally blind to synthetic contamination. As the interval progresses, the theater increases because the gap widens between what institutions claim to be checking (data quality) and what they can actually detect (synthetic contamination). By the interval endpoint, quality review is largely performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival disagreement. Model developers see a coordination mechanism solving real cost problems (Rope). The information commons sees structural extraction with no exit (Snare). Detection coalitions see a temporary failure with a solution path (Scaffold). Data quality institutions see their own degraded function (Piton). Downstream models see mixed harms and benefits (Tangled Rope). The analytical observer risks seeing mathematical inevitability (Mountain) but the structural evidence contradicts this — the loop is engineered by cost-minimization incentives, not by necessity. The perspectival gap reveals that the slop loop's true classification depends entirely on whether we treat cost minimization (the beneficiary's constraint) or information integrity (the commons' constraint) as foundational.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is computed from the agent's structural relationship to the constraint. Model developers are beneficiaries with arbitrage exit options — they derive d ≈ 0.15 (low extraction experienced). The information commons is powerless and trapped — it derives d ≈ 0.95 (maximum extraction experienced). Downstream models are also trapped but gain some benefit from the services built on slop — they derive d ≈ 0.85 (high extraction, partial benefit). Human discourse communities are constrained but benefit from free AI access — they derive d ≈ 0.50 (symmetric cost/benefit). Data curation institutions are institutional with arbitrage exit (can adopt new detection tools) — they derive d ≈ 0.20, but the piton classification comes from the theater gate rather than from experienced extraction. Detection coalitions are organized with constrained exit (resource-bound but with agency) — they derive d ≈ 0.60, which with lower base extraction yields low χ, supporting the scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION BY PERSPECTIVAL DECOMPOSITION: The mandatrophy is resolved by recognizing that the slop loop is genuinely a tangled rope at the structural level (ε=0.58, active enforcement required, both beneficiaries and victims), but different agents legitimately perceive different types based on their exit options and power levels. The beneficiary (developers) experiences it as pure coordination (Rope) because they have exit options and control the extraction flow. The victim (information commons) experiences it as pure extraction (Snare) because they have no exit. The analytical observer must resist the temptation to naturalize the loop as a mountain (inherent to scaling) because the structural data shows it is engineered: cost minimization and lack of provenance standards, not mathematical necessity, drive contamination. The detection coalitions' scaffold perspective is real — not aspirational — because detection technology and provenance standards are materially viable and have sunset clauses. The piton classification of data quality institutions is a diagnostic feature: they maintain the theater of curation while the actual function (filtering slop) is degraded, revealing institutional lag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slop_detection_capability_threshold,
    'At what confidence threshold does synthetic-text detection become reliable enough to protect training data integrity without false-positive rejection of legitimate human-generated text?',
    'Empirical evaluation of detection tool false-positive/false-negative rates on diverse datasets; longitudinal tracking of model degradation vs detection sensitivity thresholds',
    'If threshold achievable < 95%: detection-based solutions are viable, supporting the scaffold perspective. If threshold > 99%: detection fundamentally unreliable, and only provenance-based solutions remain viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slop_detection_capability_threshold, empirical, 'Threshold for synthetic detection reliability').

omega_variable(
    recursive_contamination_speed,
    'How many training cycles does it take before synthetic-slop contamination becomes dominant in the training signal, rendering downstream models qualitatively different from baseline?',
    'Controlled experiments: train models on progressively contaminated datasets (10%, 30%, 50%, 70% synthetic); measure semantic degradation via downstream task performance and trope-frequency analysis',
    'If speed < 1 cycle: slop loop is a snare from the moment it starts (extraction immediate). If speed > 5 cycles: moderate timeframe for detection and intervention, supporting tangled-rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recursive_contamination_speed, empirical, 'Timeline for slop dominance in training signal').

omega_variable(
    alternative_data_source_availability,
    'Are there sufficient clean, non-synthetic training data sources available to allow models to bypass the slop loop entirely, or has synthetic generation become obligatory for scaled training?',
    'Inventory of available high-quality datasets; cost/availability analysis for alternatives (proprietary data, human-annotated datasets, archived natural language corpora); scalability limits of non-synthetic sources',
    'If alternatives exist at scale: slop loop is a tangled-rope coordination failure (exit possible via arbitrage). If alternatives insufficient: snare (exit impossible, pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_data_source_availability, empirical, 'Availability of clean alternative training sources').

omega_variable(
    institutional_enforcement_viability,
    'Can centralized governance (data auditing, model transparency requirements, provenance mandates) actually enforce training-data integrity, or do decentralized incentives always re-create the slop loop?',
    'Analysis of enforcement mechanisms in related domains (financial regulation, pharmaceutical approval, content moderation); modeling of incentive structures for data providers under different regulatory regimes',
    'If enforcement viable: governance solutions can prevent slop loop, supporting scaffold with real sunset clause. If enforcement fails: slop loop is structural to the incentive ecology, making snare classification dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_enforcement_viability, conceptual, 'Viability of centralized enforcement against slop generation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clawderberg_recursive_slop, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slop_tr_t0, clawderberg_recursive_slop, theater_ratio, 0, 0.35).
narrative_ontology:measurement(slop_tr_t3, clawderberg_recursive_slop, theater_ratio, 3, 0.52).
narrative_ontology:measurement(slop_tr_t6, clawderberg_recursive_slop, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(slop_be_t0, clawderberg_recursive_slop, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(slop_be_t3, clawderberg_recursive_slop, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(slop_be_t6, clawderberg_recursive_slop, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clawderberg_recursive_slop, resource_allocation).
narrative_ontology:affects_constraint(clawderberg_recursive_slop, training_data_integrity).
narrative_ontology:affects_constraint(clawderberg_recursive_slop, synthetic_text_detection_arms_race).
narrative_ontology:affects_constraint(clawderberg_recursive_slop, model_hallucination_scaling).

% DUAL FORMULATION NOTE:
% The recursive slop loop decomposes into three structurally distinct constraints: (1) training_data_integrity (ε=0.52) — the commons' epistemic degradation; (2) synthetic_text_detection_arms_race (ε=0.65) — the detection-evasion cycle; (3) model_hallucination_scaling (ε=0.48) — downstream models inheriting trope-based artifacts. The slop loop itself (ε=0.58) is downstream of all three and represents their integration. Each sub-constraint has different beneficiaries and victims; the loop aggregates their effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(clawderberg_recursive_slop, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
