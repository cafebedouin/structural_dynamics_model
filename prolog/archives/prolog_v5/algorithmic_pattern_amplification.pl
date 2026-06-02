% ============================================================================
% CONSTRAINT STORY: algorithmic_pattern_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_pattern_amplification, []).

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
 *   constraint_id: algorithmic_pattern_amplification
 *   human_readable: Algorithmic Pattern Amplification in Machine Learning Systems
 *   domain: technology/artificial_intelligence/machine_learning
 *
 * SUMMARY:
 *   Algorithmic pattern amplification represents a structural constraint
 *   where machine learning systems, optimized for predictive accuracy on
 *   historical data, systematically amplify statistical patterns including
 *   those reflecting historical discrimination. The constraint exhibits the
 *   classical tangled rope structure: genuine coordination function (shared
 *   standards for model evaluation, fairness frameworks enabling
 *   multi-stakeholder collaboration on bias mitigation) coexists with
 *   asymmetric extraction (developers benefit from simplified optimization
 *   metrics while affected populations bear downstream costs of amplified
 *   discrimination). The rising theater ratio (0.35 → 0.65 over the interval)
 *   reflects that fairness and interpretability discussions have become
 *   increasingly performative relative to actual discrimination reduction in
 *   deployed systems. The extractiveness trajectory (0.32 → 0.62) shows
 *   accumulating costs as algorithmic decision-making expands into
 *   higher-stakes domains (criminal justice, credit, employment) without
 *   corresponding improvements in fairness mechanisms.
 *
 * KEY AGENTS:
 *   - Underrepresented Populations: Primary victims (powerless/trapped) — trapped in algorithmic systems with no capacity to modify training data or model behavior; bear extraction costs of historical bias amplification
 *   - Model Developers and ML Engineers: Primary beneficiaries (institutional/arbitrage) — can select optimization objectives, training data sources, and fairness constraints; benefit from simplified accuracy metrics without bearing discrimination costs
 *   - Domain Practitioners and Auditors: Secondary actors (moderate/constrained) — face computational barriers and institutional pressure to optimize metrics; benefit from fairness frameworks and standardization
 *   - Fairness and Transparency Movements: Organized coalition (organized/constrained) — academic researchers, civil rights organizations, regulatory bodies building alternative pathways (debiasing techniques, external audits, fairness constraints) with potential sunset timeline
 *   - Optimization Frameworks and Benchmarking Institutions: Institutional infrastructure (institutional/arbitrage) — perpetuate accuracy-centric metrics; maintain theater through unchanged evaluation practices despite known inadequacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing algorithmic discrimination as inherent to statistical learning, obscuring its institutional contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_pattern_amplification, 0.58).
domain_priors:suppression_score(algorithmic_pattern_amplification, 0.62).
domain_priors:theater_ratio(algorithmic_pattern_amplification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_pattern_amplification, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_pattern_amplification, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithmic_pattern_amplification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_pattern_amplification, tangled_rope).
narrative_ontology:human_readable(algorithmic_pattern_amplification, "Algorithmic Pattern Amplification in Machine Learning Systems").
narrative_ontology:topic_domain(algorithmic_pattern_amplification, "technology/artificial_intelligence/machine_learning").

domain_priors:requires_active_enforcement(algorithmic_pattern_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_pattern_amplification, model_developers).
narrative_ontology:constraint_beneficiary(algorithmic_pattern_amplification, data_platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_pattern_amplification, optimization_frameworks).
narrative_ontology:constraint_victim(algorithmic_pattern_amplification, underrepresented_populations).
narrative_ontology:constraint_victim(algorithmic_pattern_amplification, discriminated_groups).
narrative_ontology:constraint_victim(algorithmic_pattern_amplification, model_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERREPRESENTED POPULATIONS (SNARE) — Trapped in datasets and decision systems that amplify historical discrimination. No exit option from algorithmic systems increasingly governing credit, employment, criminal justice, healthcare. Bear extraction costs without ability to modify training data or model behavior. Maximum structural capture.
constraint_indexing:constraint_classification(algorithmic_pattern_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMAIN PRACTITIONERS (TANGLED ROPE) — Face constraints: limited access to training data, computational barriers to model inspection, institutional pressure to optimize metrics. Also benefit from coordination mechanisms (fairness frameworks, audit standards, transparency requirements) that improve practice. Mixed experience: genuine benefit from standardization alongside asymmetric information access.
constraint_indexing:constraint_classification(algorithmic_pattern_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODEL DEVELOPERS (ROPE) — Primary beneficiaries with exit options. Can choose training objectives, data sources, and optimization targets. Experience the constraint as coordination mechanism: pattern amplification enables achieving performance targets efficiently. Benefits from existing data infrastructure without bearing costs of historical bias embedded in data.
constraint_indexing:constraint_classification(algorithmic_pattern_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FAIRNESS MOVEMENTS (SCAFFOLD) — Organized agents (academic fairness research, regulatory frameworks, transparency requirements) see pattern amplification as a temporary coordination failure with exit pathways. Debiasing techniques, fairness constraints, and external auditing represent building alternative pathways. Sunset logic: regulatory mandates and fairness-aware ML maturation provide structured phase-out, though timeline uncertain.
constraint_indexing:constraint_classification(algorithmic_pattern_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPTIMIZATION THEATER (PITON) — The accuracy metric itself has become largely performative: maximizing test-set accuracy no longer guarantees real-world fairness, safety, or interpretability. The optimization ritual persists through institutional inertia in ML curricula, benchmarking practices, and corporate incentive structures despite recognized inadequacy. Theater ratio reflects that accuracy optimization continues despite known failure to capture downstream harms.
constraint_indexing:constraint_classification(algorithmic_pattern_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From universal/civilizational perspective, pattern amplification appears inherent to statistical learning: any system trained to extract signal from correlated data must amplify correlations present in training data. This appears as immutable mathematical law. However, the structural data contradicts mountain classification — the engine identifies this as false summit, revealing that what is mathematically describable is institutionally contingent.
constraint_indexing:constraint_classification(algorithmic_pattern_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_pattern_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_pattern_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_pattern_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_pattern_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_pattern_amplification, TR),
    TR >= 0.70.

:- end_tests(algorithmic_pattern_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting sustained asymmetry between developers' optimization benefits and affected populations' discrimination costs. The value is not higher because genuine coordination mechanisms (fairness research, audit standards, transparency frameworks) do exist and provide real structure for bias mitigation, preventing this from being pure extraction. However, these mechanisms remain insufficient relative to deployment scale and discrimination severity. Suppression (0.62): Moderate-high, reflecting multiple barriers to exit: technical complexity of model inspection, computational costs of fairness-aware training, institutional pressure to optimize traditional metrics, information asymmetry between developers and affected populations, legal barriers to access training data for independent audit. Barriers are not absolute (some practitioners do modify models; some debiasing techniques are effective) but sufficient to prevent most affected populations from exercising meaningful control. Theater ratio (0.58): Moderate-high and rising. The increase over the interval reflects proliferation of fairness terminology, explainability tools, and audit frameworks without corresponding reduction in deployed discrimination. Fairness becomes theatrical: saliency maps, SHAP values, and fairness papers published while downstream discrimination persists. The theater rate is not higher (>0.70) because some genuine fairness work exists and some models have been genuinely improved, but the trend is toward increasing performativity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Model developers see coordination (Rope) — fairness frameworks enable collaboration on bias mitigation. The organized coalition sees temporary dysfunction with structured solutions (Scaffold) — regulatory mandates and debiasing techniques provide exit pathways with plausible sunset. The optimization theater sees its own ritual as degraded (Piton) — accuracy metrics are known inadequate yet persist through inertia. Domain practitioners see mixed coordination and extraction (Tangled Rope) — they benefit from shared standards yet face institutional pressure to optimize harmful metrics. Underrepresented populations see pure extraction (Snare) — algorithmic systems amplify discrimination with no meaningful control mechanism. The analytical observer risks seeing mathematical inevitability (Mountain) — that statistical learning must amplify correlations in training data — but the structural data reveals this as false summit: which correlations get amplified is a choice (algorithmic and institutional), not an immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from agent structural position within the extraction flow. Model developers as beneficiaries with arbitrage exit options experience low d (~0.15), producing negative effective extraction (they are compensated). Underrepresented populations as trapped victims experience high d (~0.95), producing maximum f(d) (~1.42), amplifying experienced extractiveness. Domain practitioners as constrained victims experience d ~0.55, producing moderate f(d), experiencing substantial but not maximal extraction. The organized coalition experiences d ~0.50, producing balanced f(d) reflecting mixed agency and constraint. The optimization imperative experiences d ~0.15 (beneficiary of current institutional structure), experiencing institutional-level directionality. The analytical observer at d~0.72 reflects the cosmopolitan perspective position. Scope modifier σ(global) = 1.2 amplifies all χ computations, reflecting that algorithmic discrimination scales globally through deployment of trained models.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint is neither pure coordination (Rope) nor pure extraction (Snare) but genuinely both. The coordination dimension is real: fairness research, audit standards, and transparency frameworks enable multi-stakeholder collaboration on bias mitigation. These are not merely performative — they have structured genuine improvements in some systems. However, the asymmetric extraction dimension is equally real: developers benefit from simplified optimization metrics (maximize accuracy) while affected populations bear discrimination costs. The institutional structure ensures developers can externalize discrimination costs while internalizing optimization benefits. The mandatrophy is resolved by recognizing that the constraint coordinates optimization (coordination function) while simultaneously extracting discrimination tolerance (extraction function). Both are structural, both are necessary to explain agent behavior, and both determine the mixed classification. The theater ratio rising without corresponding discrimination reduction suggests the theater is expanding (fairness discussions become more prominent while actual harm persists), which could eventually tip the classification toward Piton if the coordination function atrophies completely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_origin_specification,
    'Does pattern amplification primarily reflect historical bias in source data, or does the learning algorithm introduce new discriminatory patterns not present in input?',
    'Comparative analysis: train identical architectures on demographically balanced synthetic data vs. naturally biased real data. Measure divergence in learned discrimination patterns.',
    'If amplification dominates: constraint is algorithmic (harder to solve without architectural change). If bias inheritance dominates: problem is data curation (more tractable via preprocessing). Classification shifts toward snare if algorithmic; toward tangled_rope if data-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bias_origin_specification, empirical, 'Attribution of pattern amplification to algorithm vs. source data bias').

omega_variable(
    fairness_objective_proxy,
    'Are fairness-aware learning objectives (demographic parity, equalized odds, individual fairness) genuine solutions or performative substitutes that relocate discrimination without eliminating it?',
    'Longitudinal outcome tracking: measure downstream consequences of fairness-constrained models in actual deployment contexts. Compare discrimination patterns across multiple protected attributes.',
    'If genuine: fairness frameworks enable real exit from pattern amplification (scaffold perspective confirmed). If performative: fairness becomes theater obscuring continued extraction (piton risk, theater ratio rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_objective_proxy, empirical, 'Whether fairness objectives constitute real solutions or proxy measures').

omega_variable(
    regulatory_capability_gap,
    'Can regulatory frameworks and external auditing actually detect and constrain pattern amplification, or do they constitute theater that gives false assurance while mechanisms persist?',
    'Audit effectiveness analysis: compare audit findings to actual discriminatory outcomes; measure rate of regulatory non-compliance escaping detection; analyze whether regulatory attention changes deployment practices.',
    'If regulators effective: scaffolding becomes viable, sunset timeline crystallizes. If ineffective: theatrical compliance emerges, suppression persists despite regulatory appearance of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capability_gap, empirical, 'Regulatory framework efficacy in constraining pattern amplification').

omega_variable(
    interpretability_illusion,
    'Does model interpretability (saliency maps, SHAP values, attention mechanisms) actually enable practitioners to understand and control discrimination, or does it create false confidence in systems that remain fundamentally opaque?',
    'Practitioner manipulation studies: train experts to use interpretability tools to modify model behavior; measure success rate in actually reducing unintended discrimination vs. introducing new forms.',
    'If interpretability enables control: tangled_rope classification holds (genuine coordination function). If interpretability is illusion: practitioners remain trapped despite appearance of agency (shifts toward snare), and theater ratio increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_illusion, empirical, 'Whether interpretability tools enable actual discrimination control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_pattern_amplification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_tr_t0, algorithmic_pattern_amplification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algo_tr_t3, algorithmic_pattern_amplification, theater_ratio, 3, 0.48).
narrative_ontology:measurement(algo_tr_t6, algorithmic_pattern_amplification, theater_ratio, 6, 0.58).
narrative_ontology:measurement(algo_tr_t9, algorithmic_pattern_amplification, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(algo_be_t0, algorithmic_pattern_amplification, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(algo_be_t3, algorithmic_pattern_amplification, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(algo_be_t6, algorithmic_pattern_amplification, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(algo_be_t9, algorithmic_pattern_amplification, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_pattern_amplification, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_pattern_amplification, credit_scoring_discrimination).
narrative_ontology:affects_constraint(algorithmic_pattern_amplification, criminal_justice_risk_assessment).
narrative_ontology:affects_constraint(algorithmic_pattern_amplification, hiring_algorithmic_screening).

% DUAL FORMULATION NOTE:
% Algorithmic pattern amplification is a general structural constraint whose extractiveness value depends on the specific application domain. Credit scoring amplification (ε≈0.72, pure snare) exhibits maximum extraction with minimal coordination benefit. Criminal justice amplification (ε≈0.68, snare with procedural theater) has institutional safeguards limiting some extraction. Hiring amplification (ε≈0.55, tangled rope) has genuine coordination function via shared hiring standards alongside extractive cost-shifting. Each domain story decomposes the general constraint into structurally specific claims with distinct ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_pattern_amplification, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
