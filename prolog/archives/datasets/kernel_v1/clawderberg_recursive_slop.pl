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
 *   constraint_id: clawderberg_recursive_slop
 *   human_readable: Recursive Slop Loop: AI-Generated Sci-Fi Trope Data Poisoning
 *   domain: technological/information_systems
 *
 * SUMMARY:
 *   The recursive slop loop is a structural constraint emerging from the
 *   confluence of three factors: (1) economic incentives to reduce annotation
 *   costs through synthetic data generation, (2) technical affordances that
 *   enable large-scale synthetic text creation, and (3) statistical
 *   properties of entertainment corpora that dominate public training
 *   datasets. As AI developers generate synthetic conversational data to cut
 *   human annotation costs, this synthetic text — shaped by sci-fi tropes,
 *   predictable dialogue patterns, and statistical modes from entertainment
 *   sources — contaminates downstream model training. Models trained on
 *   slop-contaminated data then generate more slop when used for data
 *   synthesis, creating a feedback loop. The constraint exhibits mixed
 *   coordination (synthetic data solves the annotation bottleneck) and
 *   asymmetric extraction (linguistic diversity and specialized knowledge are
 *   systematically degraded). The constraint's extractiveness has risen from
 *   0.32 to 0.58 over four years as synthetic-data-based training has become
 *   standard practice, and theater ratio (0.68) reflects the gap between data
 *   quality certification systems and actual corpus composition in production
 *   models.
 *
 * KEY AGENTS:
 *   - Annotation Cost Reduction Holders: Institutional beneficiaries (institutional/arbitrage) — vendors, labs, and companies prioritizing speed and cost efficiency over linguistic fidelity. Benefit from the extraction window during synthetic-data acceleration phase.
 *   - Linguistic Diversity and Knowledge Commons: Primary victim (powerless/trapped) — minority languages, specialized domains, long-tail knowledge patterns. Cannot be recovered once slop-contaminated corpora become the training standard.
 *   - Individual Researchers and Domain Specialists: Secondary victims (moderate/constrained) — researchers working with specialized corpora, low-resource languages, and non-entertainment domains. Face degraded model performance and high cost of mitigation.
 *   - Model Developers Using Pre-Trained Foundations: Mixed-role agents (institutional/constrained) — face both benefit (cheaper training through synthetic data) and cost (degraded downstream performance on specialized tasks).
 *   - Regulatory and Open-Source Initiatives: Organized agents (organized/constrained) — building alternative training pathways, data governance standards, and domain-curated datasets that represent sunset mechanisms.
 *   - Data Quality Certification Systems: Institutional theater (institutional/arbitrage) — annotation provenance tracking, data cards, synthetic disclosure requirements that document slop without preventing it.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constraint as a statistical inevitability rather than recognizing it as a contingent economic choice to prioritize speed over data quality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clawderberg_recursive_slop, 0.58).
domain_priors:suppression_score(clawderberg_recursive_slop, 0.62).
domain_priors:theater_ratio(clawderberg_recursive_slop, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clawderberg_recursive_slop, extractiveness, 0.58).
narrative_ontology:constraint_metric(clawderberg_recursive_slop, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(clawderberg_recursive_slop, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clawderberg_recursive_slop, tangled_rope).
narrative_ontology:human_readable(clawderberg_recursive_slop, "Recursive Slop Loop: AI-Generated Sci-Fi Trope Data Poisoning").
narrative_ontology:topic_domain(clawderberg_recursive_slop, "technological/information_systems").

domain_priors:requires_active_enforcement(clawderberg_recursive_slop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clawderberg_recursive_slop, annotation_cost_reduction_holders).
narrative_ontology:constraint_beneficiary(clawderberg_recursive_slop, rapid_ai_deployment_vendors).
narrative_ontology:constraint_victim(clawderberg_recursive_slop, downstream_model_performance).
narrative_ontology:constraint_victim(clawderberg_recursive_slop, linguistic_diversity).
narrative_ontology:constraint_victim(clawderberg_recursive_slop, long_tail_knowledge_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINGUISTIC DIVERSITY (SNARE) — The constraint traps linguistic diversity, minority language patterns, specialized discourse (scientific nomenclature, technical jargon, domain-specific registers), and long-tail knowledge. These cannot be recovered from synthetic data dominated by sci-fi statistical modes. No exit: once slop-contaminated corpora become the training standard, recovery requires expensive human re-annotation or domain-expert intervention — a barrier most low-resource languages and specialized domains cannot clear. Maximum extraction from the epistemic commons.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCHERS AND DOMAIN SPECIALISTS (TANGLED ROPE) — Constrained by dependency on pre-trained models (switching costs, retraining infrastructure, institutional momentum) but also benefit from fast model iteration and public baseline availability. They perceive the constraint as a mixed burden: synthetic data enables rapid prototyping, but slop contamination degrades downstream task performance. Extraction is asymmetric — the cost of degraded language models falls more heavily on those working with specialized domains than on general-purpose chatbot developers.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANNOTATION COST REDUCTION HOLDERS (ROPE) — Primary beneficiaries (cost reduction vendors, rapid-deployment labs, companies prioritizing speed over linguistic fidelity). Experience the constraint as pure coordination: synthetic data solves the collective action problem of expensive human annotation. High exit capacity — they can always revert to human annotation or pay for higher-quality synthetic data. Net beneficiaries during the extraction window.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND OPEN-SOURCE COUNTER-MOVEMENTS (SCAFFOLD) — Organized agents (data governance initiatives, open-science coalitions, domain-specific research communities) are building alternative training pathways: human-annotated corpora, domain-curated datasets, linguistic diversity preservation projects, federated training frameworks that bypass centralized synthetic-data pipelines. These represent sunset mechanisms — as governance standards tighten and open alternatives mature, the economic pressure to use slop-contaminated synthetic data diminishes. Estimated sunset: 5-10 years for regulatory frameworks and alternative infrastructure to establish domain-specific standards.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANNOTATION QUALITY CERTIFICATION SYSTEMS (PITON) — Formal quality assurance and data lineage tracking (data cards, annotation provenance, synthetic-data disclosure requirements) persist as institutional theater. These systems document slop contamination without preventing it, because the economic incentives and technical automation still favor synthetic data generation at scale. The certification ritual is maintained to preserve the appearance of governance while the underlying extraction mechanism operates unimpeded. Theater ratio reflects the gap between documented quality standards and actual dataset composition in production models.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MODEL DEVELOPERS (INSTITUTIONAL/CONSTRAINED) — Face high switching costs (retraining on alternative corpora, fine-tuning infrastructure, institutional momentum toward particular training pipelines) but also benefit from the efficiency gains of synthetic-data-accelerated development cycles. They are both beneficiaries (cost reduction) and victims (degraded downstream performance on specialized tasks). Classification reflects constrained exit: they could invest in cleaner training data, but the organizational and financial barriers make this an expensive alternative.
constraint_indexing:constraint_classification(clawderberg_recursive_slop, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STATISTICAL INEVITABILITY VIEW (MOUNTAIN) — From a mathematical perspective, synthetic data generated from existing corpora must statistically regress toward the training distribution's modes. Sci-fi tropes dominate entertainment corpora (high data density); minority languages and specialized domains have low representation. The constraint appears as an immutable property of statistics: synthetic data cannot generate what is not densely represented in the source. However, this perspective risks naturalizing what is actually a contingent economic choice — to use synthetic data rather than investing in human annotation or curated datasets. The engine will flag this as a false summit.
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
    constraint_indexing:constraint_classification(clawderberg_recursive_slop, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high, rising trajectory. The constraint begins as a modest extraction (0.32) when synthetic data is a marginal acceleration tool, but becomes severe (0.58) as synthetic-data-based training becomes dominant practice. The rise reflects accumulating contamination and diminishing availability of clean training corpora. Suppression (0.62): High and rising. Barriers to exit include: institutional momentum toward particular training pipelines, high retraining cost, lack of alternatives at comparable scale, and increasing dependency on contaminated pre-trained models. Switching to human annotation or curated data requires substantial investment — most organizations cannot clear this barrier. Theater ratio (0.68): High. Data quality certification systems (data cards, annotation provenance, synthetic disclosure requirements) are extensively documented and formally required by governance frameworks, yet they do not prevent slop contamination. The certification process is performative — it creates the appearance of quality control while the underlying economic incentives ensure synthetic-data dominance. The theater ratio rises as certification requirements become more elaborate while actual corpus quality degrades.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by beneficiary/victim status. Cost-reduction holders see pure coordination (Rope) — synthetic data solves a real collective action problem of expensive annotation. Linguistic communities and researchers see extraction (Snare, Tangled Rope) — their specialized knowledge and language diversity are systematically degraded with no recovery mechanism. Organized counter-movements see a temporary problem with sunset mechanisms (Scaffold) — regulatory frameworks and alternative infrastructure are being built. The data quality certification system sees its own performative ritual (Piton) — extensive documentation of quality standards that do not prevent the underlying extraction. The civilizational analytical observer risks seeing an immutable statistical law (Mountain) — synthetic data must regress toward training distribution modes — but this naturalizes what is actually a contingent economic choice to prioritize speed over data quality. The false summit detector will identify this as naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d) captures each agent's structural position relative to the extraction flow. Cost-reduction beneficiaries (institutional/arbitrage) derive low d values — they exit easily and capture benefits, so experienced extractiveness is negative (they gain utility). Trapped agents (linguistic diversity, minority communities) derive high d values approaching 1.0 — they cannot exit, have no alternatives, and bear full cost of contamination. Organized counter-movements (regulatory bodies, open-science coalitions) have intermediate d values reflecting constrained but non-zero exit capacity — they can mobilize resources and build alternatives, though at significant organizational cost. The piton perspective (data quality certification) reflects institutional position (low d from arbitrage capacity) paired with high theater ratio — the institution maintains performative quality assurance while the extraction mechanism operates beneath the ceremonial layer.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating how the same structural data (slop contamination, cost reduction, synthetic data adoption) produces legitimate but distinct classifications across observation sites. Cost-reduction holders genuinely perceive Rope (coordination solving annotation bottleneck). Linguistic communities genuinely perceive Snare (extraction with no exit). Regulatory bodies genuinely perceive Scaffold (temporary problem with sunset mechanisms). Data certification systems genuinely perform Piton (ritual without functional content). Researchers perceive Tangled Rope (mixed coordination and extraction). The analytical observer risks Mountain (statistical inevitability) but this is a false summit — the constraint is a contingent institutional arrangement, not a law of nature. The mandatrophy is not resolved by choosing one type; it is resolved by recognizing that all types are legitimate perspectival readings of the same structural phenomenon. The constraint IS all six types simultaneously — from different observation positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_data_filtering_sufficiency,
    'Can automated filtering (adversarial detection, out-of-distribution scoring, linguistic anomaly detection) reliably remove sci-fi slop from synthetic corpora without over-filtering legitimate domain language?',
    'Benchmarking studies comparing filtered vs. unfiltered synthetic-data performance on domain-specific downstream tasks; false-positive rates on legitimate specialized terminology; linguistic diversity metrics pre- and post-filtering',
    'If filtering succeeds: suppression drops substantially, constraint becomes Rope. If filtering fails: suppression persists, the institutional certification systems remain purely performative (Piton), extraction mechanism is entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_filtering_sufficiency, empirical, 'Whether automated filtering can reliably remove slop while preserving domain language').

omega_variable(
    cost_differential_threshold,
    'At what cost ratio between human annotation and synthetic data generation does the economic incentive structure flip toward human annotation as the rational default?',
    'Historical tracking of annotation cost reduction through automation; model quality degradation costs (performance loss on specialized tasks); regulatory compliance costs (data provenance, synthetic disclosure); infrastructure investment for alternative pathways',
    'If threshold < 1.5x current synthetic costs: market pressure alone may reverse the trend toward synthetic-dominated training. If threshold > 3.0x: regulatory intervention or institutional mandates become necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_differential_threshold, empirical, 'Cost ratio at which human annotation becomes economically rational').

omega_variable(
    cascade_depth_irreversibility,
    'How many training cycles can models be trained on slop-contaminated data before the statistical contamination becomes effectively irreversible through cascade effects (slop-trained models generating more slop, which trains downstream models, etc.)?',
    'Longitudinal corpus analysis tracking sci-fi trope density across training generations; information-theoretic measures of semantic divergence from pre-synthetic-data baselines; retraining experiments on human-only subsets to measure recovery trajectory',
    'If cascade depth < 3 cycles: contamination is recoverable with focused intervention. If > 5 cycles: contamination becomes structurally embedded, requiring wholesale rebuilding of foundational models. This determines whether the constraint is temporary (Scaffold) or permanent (Snare/Mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cascade_depth_irreversibility, empirical, 'Number of training cycles before slop contamination becomes irreversible').

omega_variable(
    sci_fi_corpus_representation_bias,
    'Is the dominance of sci-fi patterns in synthetic data a feature of sci-fi''s representation in entertainment corpora, or a convergence effect where the data generation process itself gravitates toward sci-fi stylistic modes as a statistical attractor?',
    'Comparative analysis of sci-fi trope density in original entertainment corpora vs. synthetic data generated from those corpora; analysis of synthetic-data generation algorithms'' implicit biases; controlled generation experiments with diverse source corpora',
    'If feature of source corpora: filtering source data and diversifying training corpora solves the problem (Rope perspective). If convergence effect: the constraint is intrinsic to how synthetic data generation works at scale — extraction mechanism is structural (Snare/Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sci_fi_corpus_representation_bias, empirical, 'Whether sci-fi dominance is source-biased or an algorithmic convergence effect').

omega_variable(
    long_tail_knowledge_irretrievability,
    'What proportion of long-tail linguistic and domain-specific patterns can be recovered through fine-tuning or specialized training, versus lost entirely once supplanted by synthetic-data statistical modes?',
    'Linguistic diversity metrics before and after contamination; benchmarks on long-tail domain tasks (rare languages, specialized jargon, uncommon registers); fine-tuning effectiveness studies on subsets with clean data preservation',
    'If recovery rate > 80%: the constraint is largely a temporary degradation (Scaffold). If < 50%: permanent loss of linguistic and epistemic diversity (Snare/Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_tail_knowledge_irretrievability, empirical, 'What proportion of long-tail knowledge can be recovered after slop contamination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clawderberg_recursive_slop, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slop_tr_t0, clawderberg_recursive_slop, theater_ratio, 0, 0.42).
narrative_ontology:measurement(slop_tr_t2, clawderberg_recursive_slop, theater_ratio, 2, 0.55).
narrative_ontology:measurement(slop_tr_t4, clawderberg_recursive_slop, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(slop_be_t0, clawderberg_recursive_slop, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(slop_be_t2, clawderberg_recursive_slop, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(slop_be_t4, clawderberg_recursive_slop, base_extractiveness, 4, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(slop_su_t0, clawderberg_recursive_slop, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(slop_su_t2, clawderberg_recursive_slop, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(slop_su_t4, clawderberg_recursive_slop, suppression_requirement, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clawderberg_recursive_slop, resource_allocation).
narrative_ontology:boltzmann_floor_override(clawderberg_recursive_slop, 0.22).
narrative_ontology:affects_constraint(clawderberg_recursive_slop, linguistic_diversity_erosion).
narrative_ontology:affects_constraint(clawderberg_recursive_slop, specialized_domain_model_degradation).
narrative_ontology:affects_constraint(clawderberg_recursive_slop, annotation_labor_displacement).

% DUAL FORMULATION NOTE:
% The recursive slop loop decomposes into three structurally distinct constraints: (1) slop_contamination_mechanism (ε=0.58, this story, Tangled Rope) — the cost-quality tradeoff in synthetic data generation; (2) linguistic_diversity_erosion (ε=0.72, downstream victim story, Snare) — irreversible loss of minority language patterns; (3) specialized_domain_model_degradation (ε=0.64, domain-expert victim story, Tangled Rope) — performance loss on non-entertainment tasks. These three stories have distinct victim sets, measurement dynamics, and regulatory pathways. This story focuses on the mechanism and its mixed coordination/extraction character. The downstream stories capture distinct extraction targets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(clawderberg_recursive_slop, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
