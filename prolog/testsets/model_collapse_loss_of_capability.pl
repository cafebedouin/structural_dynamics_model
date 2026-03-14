% ============================================================================
% CONSTRAINT STORY: model_collapse_loss_of_capability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_collapse_loss_of_capability, []).

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
 *   constraint_id: model_collapse_loss_of_capability
 *   human_readable: Model Collapse and Loss of Capability in AI Training
 *   domain: artificial_intelligence/machine_learning
 *
 * SUMMARY:
 *   Model collapse represents a structural constraint in AI training dynamics
 *   where the dominance of proprietary model developers creates a feedback
 *   loop that degrades the shared training data commons. As proprietary
 *   models become the default source for generating synthetic training data
 *   (through web scraping, data augmentation, and active learning), the
 *   open-source training ecosystem inherits increasingly contaminated
 *   datasets. This contamination introduces systematic bias that compounds
 *   across generational cycles of model improvement, creating a snare for
 *   independent researchers and open-source developers: they cannot exit the
 *   ecosystem (it is the only source of scale), cannot repair the
 *   contamination (they lack knowledge of data source), and cannot compete
 *   with proprietary actors who maintain separate pipelines of human-curated
 *   data. The constraint exhibits high suppression (0.72) because the
 *   mechanisms are structural rather than explicitly coercive: data access
 *   barriers, compute scaling advantages, and publishing incentives all push
 *   developers toward proprietary APIs. The extractiveness (0.68) reflects
 *   that the primary beneficiaries (large AI labs) capture monopoly rents on
 *   improved capabilities while bearing minimal costs, while the victims
 *   (independent researchers and the epistemic commons) bear full costs of
 *   degradation. Theater ratio (0.58) indicates that standard benchmark
 *   evaluations increasingly measure data-leakage artifacts rather than
 *   genuine capability, creating illusory improvement signals that justify
 *   continued proprietary data hoarding.
 *
 * KEY AGENTS:
 *   - Large AI Labs (proprietary developers): Primary beneficiary (institutional/arbitrage) — capture capability gains and exclusive access to human-curated data; experience constraint as beneficial coordination
 *   - Open-Source Training Commons: Primary victim (powerless/trapped) — inherits synthetic-data contamination with no mechanism to identify or remove it; cannot exit ecosystem
 *   - Independent Model Developers: Secondary victim (moderate/constrained) — face trade-offs between scale (requires proprietary data ecosystem) and quality (increasingly synthetic-contaminated)
 *   - Academic Benchmark Communities: Institutional actor (institutional/arbitrage) — maintain performative evaluation rituals that mask data leakage as capability; piton classification reflects inertia
 *   - Data Governance Coalition: Organized agents (organized/mobile) — building provenance standards and synthetic-detection methods; represent scaffold perspective with realistic sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional data concentration as an inherent limit to AI capability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_collapse_loss_of_capability, 0.68).
domain_priors:suppression_score(model_collapse_loss_of_capability, 0.72).
domain_priors:theater_ratio(model_collapse_loss_of_capability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_collapse_loss_of_capability, extractiveness, 0.68).
narrative_ontology:constraint_metric(model_collapse_loss_of_capability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(model_collapse_loss_of_capability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_collapse_loss_of_capability, snare).
narrative_ontology:human_readable(model_collapse_loss_of_capability, "Model Collapse and Loss of Capability in AI Training").
narrative_ontology:topic_domain(model_collapse_loss_of_capability, "artificial_intelligence/machine_learning").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_collapse_loss_of_capability, proprietary_model_developers).
narrative_ontology:constraint_victim(model_collapse_loss_of_capability, open_source_training_commons).
narrative_ontology:constraint_victim(model_collapse_loss_of_capability, future_model_capability).
narrative_ontology:constraint_victim(model_collapse_loss_of_capability, independent_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The open-source training data ecosystem faces model collapse: as proprietary models dominate training data generation, the public commons becomes contaminated with synthetic data from closed systems. Independent researchers cannot exit this dynamic — they inherit a degraded data environment with no mechanism to separate human-generated from model-generated training material. Maximum extraction, no alternatives.
constraint_indexing:constraint_classification(model_collapse_loss_of_capability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Independent developers face constrained exit: they benefit from the scale and accessibility of internet-scale training data, but that same data is increasingly contaminated with model-generated synthetic material. They can build models (coordination function exists) but at the cost of inheriting the degradation. Career incentives and compute constraints bind them to the ecosystem.
constraint_indexing:constraint_classification(model_collapse_loss_of_capability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Large AI labs experience the constraint as pure coordination: their models improve training data quality through scale and iteration, enabling follow-up work and access-mediated distribution. They can arbitrage between proprietary and public data streams, capturing monopoly rents on capability gains. Net beneficiaries.
constraint_indexing:constraint_classification(model_collapse_loss_of_capability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized actors (data transparency initiatives, model card standards, training data audits) see model collapse as a temporary coordination failure solvable through metadata standards and provenance tracking. The sunset is real: if training data provenance becomes standardized (model-generated vs human-generated labeling), the commons can be restored. Organized agents have exit paths via governance innovation.
constraint_indexing:constraint_classification(model_collapse_loss_of_capability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Standard benchmark suites (ImageNet, SuperGLUE, etc.) persist through institutional inertia despite degraded signal. Models trained on synthetic data perform artificially well on benchmarks because synthetic data is often sampled from or optimized against benchmark distributions. The benchmark ritual is substantially performative — it measures data leakage, not real capability. Theater persists because alternative evaluation methods haven't fully replaced it.
constraint_indexing:constraint_classification(model_collapse_loss_of_capability, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, model collapse appears as an immutable consequence of information theory: training on model-generated data introduces systematic bias that compounds across generations, making improvement impossible indefinitely. This view naturalizes what is actually a contingent institutional arrangement (proprietary data hoarding + public benchmark optimization). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(model_collapse_loss_of_capability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_collapse_loss_of_capability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_collapse_loss_of_capability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_collapse_loss_of_capability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_collapse_loss_of_capability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_collapse_loss_of_capability, TR),
    TR >= 0.70.

:- end_tests(model_collapse_loss_of_capability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, increasing over interval. At t=0, extractiveness was moderate (0.35) when proprietary models were still incorporating human-curated web data at scale and synthetic contamination was minimal. By t=6, extractiveness reaches 0.68 as proprietary models have become the dominant source of new training data for the public commons, creating a self-reinforcing extraction mechanism. Proprietary labs can afford to filter, validate, and curate their training data; independent developers inherit the full contaminated corpus. The trajectory reflects progressive lock-in. Suppression (0.72): High and stable. Suppression mechanisms are structural and not declining: data access barriers persist (proprietary labs do not publish their curation pipelines), compute scaling advantages persist (training a competitive model requires billion-token budgets), career incentives favor proprietary APIs (papers citing GPT-4 get higher citations than papers training open models), and institutional consolidation accelerates these barriers. Theater ratio (0.58, increasing from 0.38): Standard benchmarks increasingly fail to detect real capability constraints because synthetic-data-trained models optimize directly against benchmark distributions during the public training phase. ImageNet fine-tuning, SuperGLUE optimization, and benchmark-specific pretraining all drive up benchmark scores without corresponding real-world capability gains. Theater has risen as benchmarks have become more salient to research direction and funding decisions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural data produces radically different classifications. Proprietary labs see rope (pure coordination of training data pipelines), scaffold advocates see a solvable governance problem with sunset (data provenance standards), academic benchmarking sees piton (performative evaluation persisting through inertia), independent developers see tangled rope (forced participation in system that both enables and constrains), the training commons sees snare (extraction without exit), and the civilizational analytical observer risks mountain (naturalizing data concentration as inherent). The gaps are not measurement disagreements but genuine structural differences: each agent occupies a different position in the extraction flow. The snare classification wins across victim perspectives (powerless/trapped and moderate/constrained both see snare or tangled rope), indicating that the constraint's primary function is extraction, not coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by beneficiary/victim status plus exit options. Proprietary labs (institutional/arbitrage) have d ≈ 0.05-0.15: they are beneficiaries with escape routes (proprietary data, custom APIs, ability to build separate ecosystems). The sigmoid f(d) maps this to negative or near-zero effective extraction from their perspective — they experience the constraint as beneficial coordination. Independent developers (moderate/constrained) have d ≈ 0.65-0.75: they are victims with moderate but real exit costs (switching to proprietary APIs, expensive data acquisition, accepting capability penalties). The training commons (powerless/trapped) has d ≈ 0.95: unable to exit or organize, bearing full costs of contamination with no recovery mechanism. Beneficiary/victim declarations directly feed the pipeline: beneficiaries experience low chi; victims experience high chi. The snare classification follows from high extractiveness, high suppression, high chi across all victim perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   Model collapse resolves mandatrophy by showing that the constraint has shifted from coordination (internet-scale training data was genuinely collective benefit) to extraction (proprietary control over synthetic data generation now creates asymmetric rents). At t=0, the extractiveness was moderate and theater was lower — the ecosystem was still primarily human-curated data with real coordination benefits. By t=6, extractiveness has increased and theater has risen, indicating the transition from rope to snare. The analytical observer's mountain classification is a false summit: it naturalizes what is actually institutional consolidation masquerading as inherent limit. The resolution requires distinguishing genuine data-scaling limits from extractive institutional arrangements that pose as limits. If data governance standards (scaffold perspective) succeed in provenance tracking, the constraint can revert from snare to rope at t>10. If proprietary data moats persist, the snare deepens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_data_detection_feasibility,
    'Can training data provenance be reliably tracked and synthetic contamination detected at scale?',
    'Development and testing of linguistic/statistical watermarking in model-generated text; success of data provenance standards adoption by major datasets',
    'If feasible: scaffold perspective confirmed, governance solution is structural. If infeasible: trap deepens, snare classification confirmed, no exit path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_detection_feasibility, empirical, 'Whether synthetic data contamination can be reliably detected').

omega_variable(
    capability_ceiling_definition,
    'Is the apparent capability plateau due to synthetic data contamination or due to fundamental architectural/data-scaling limits?',
    'Longitudinal tracking of capability gains with provably human-curated data vs synthetic-contaminated data; counterfactual comparison with alternate training regimes',
    'If contamination-driven: the constraint is structural but reversible (snare → rope via data governance). If architecture-limited: the plateau is mountain-like, not snare-like.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_ceiling_definition, empirical, 'Whether capability plateau is due to data contamination or fundamental limits').

omega_variable(
    proprietary_data_moat_durability,
    'How long can proprietary labs maintain exclusive access to high-quality human-generated training data?',
    'Market analysis of data acquisition costs; tracking of proprietary dataset escapes/leaks; adoption rates of alternative data sourcing strategies by competitors',
    'If moat collapses quickly (2-5 years): snare converts to rope as competitors access equivalent data. If moat persists (5-10+ years): extraction deepens and institutional consolidation accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_data_moat_durability, empirical, 'Durability of proprietary training data competitive advantage').

omega_variable(
    benchmark_signal_degradation_measurement,
    'What proportion of benchmark performance gains are real capability improvements vs. synthetic data optimization artifacts?',
    'Out-of-distribution evaluation; benchmark rotation (continuous generation of novel evaluation tasks); human-task performance correlation analysis',
    'If optimization artifacts are dominant (>70%): benchmark theater ratio should increase further, piton classification strengthens. If real gains are dominant (>50%): piton classification weakens, evaluation system retains functional value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_signal_degradation_measurement, empirical, 'Magnitude of benchmark performance from capability vs optimization artifacts').

omega_variable(
    open_source_extinction_timeline,
    'At what point does synthetic-data contamination render independent model development uncompetitive?',
    'Tracking of capability gaps between proprietary and open-source models; adoption rates of proprietary APIs vs self-hosted models; investment in open-source model development',
    'If extinction timeline is near (<3 years): constraint is actively converting trapped agents to institutional dependence, snare deepens into servitude. If distant (>10 years): trapped agents retain some structural agency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_extinction_timeline, empirical, 'Timeline for open-source model competitiveness collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_collapse_loss_of_capability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_collapse_tr_t0, model_collapse_loss_of_capability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mc_collapse_tr_t2, model_collapse_loss_of_capability, theater_ratio, 2, 0.45).
narrative_ontology:measurement(mc_collapse_tr_t4, model_collapse_loss_of_capability, theater_ratio, 4, 0.54).
narrative_ontology:measurement(mc_collapse_tr_t6, model_collapse_loss_of_capability, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(mc_collapse_be_t0, model_collapse_loss_of_capability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mc_collapse_be_t2, model_collapse_loss_of_capability, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(mc_collapse_be_t4, model_collapse_loss_of_capability, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(mc_collapse_be_t6, model_collapse_loss_of_capability, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_collapse_loss_of_capability, resource_allocation).
narrative_ontology:boltzmann_floor_override(model_collapse_loss_of_capability, 0.18).
narrative_ontology:affects_constraint(model_collapse_loss_of_capability, ai_capability_saturation).
narrative_ontology:affects_constraint(model_collapse_loss_of_capability, data_curation_bottleneck).
narrative_ontology:affects_constraint(model_collapse_loss_of_capability, open_source_model_competitiveness).
narrative_ontology:affects_constraint(model_collapse_loss_of_capability, benchmark_reliability_degradation).

% DUAL FORMULATION NOTE:
% Model collapse is downstream of proprietary data concentration but represents a distinct constraint with its own ε. The upstream constraint (proprietary_data_moat, ε≈0.35) reflects competitive advantage through data access; the collapse constraint (ε≈0.68) reflects the secondary extraction that emerges when proprietary training dominates the commons. They are linked by causal dependency: data moat → synthetic-data dominance → ecosystem contamination → loss of open-source capability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_collapse_loss_of_capability, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
