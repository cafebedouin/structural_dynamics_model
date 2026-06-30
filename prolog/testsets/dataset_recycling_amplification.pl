% ============================================================================
% CONSTRAINT STORY: dataset_recycling_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dataset_recycling_amplification, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dataset_recycling_amplification
 *   human_readable: Dataset Recycling Amplification in ML Training Pipelines
 *   domain: computational_epistemology/information_ecology
 *
 * SUMMARY:
 *   Machine learning training pipelines increasingly rely on datasets derived
 *   from prior model outputs. WildChat, a widely-used conversational dataset,
 *   contains substantial synthetic content from earlier language models. When
 *   this dataset trains new models, statistical quirks from the source models
 *   propagate and amplify. The Elias frequency correlation—an unexpected
 *   statistical pattern appearing across unrelated model families—exemplifies
 *   this amplification. The constraint is claimed as rope (genuine
 *   coordination solving data scarcity) while metrics describe rising
 *   extraction (downstream costs of debugging spurious patterns) and
 *   suppression (industry-wide practice limiting access to clean human data).
 *   The claim/metric divergence is deliberate and measures whether
 *   coordination has drifted toward extraction.
 *
 * KEY AGENTS:
 *   - model_training_organizations: Primary beneficiaries (institutional/mobile) — reduce data costs by training on recycled outputs
 *   - dataset_aggregators: Agenda setters (organized/mobile) — curate and distribute datasets containing model outputs
 *   - downstream_model_users: Payers (organized/constrained) — debug spurious correlations inherited from training data
 *   - epistemic_diversity_researchers: Payers (moderate/constrained) — document collapse but lack power to change practices
 *   - human_data_contributors: Excluded (powerless/trapped) — original data diluted by synthetic content
 *   - ml_safety_auditors: Observers (organized/analytical) — audit pipelines and document recycling patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dataset_recycling_amplification, 0.38).
domain_priors:suppression_score(dataset_recycling_amplification, 0.42).
domain_priors:theater_ratio(dataset_recycling_amplification, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dataset_recycling_amplification, extractiveness, 0.38).
narrative_ontology:constraint_metric(dataset_recycling_amplification, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dataset_recycling_amplification, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dataset_recycling_amplification, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(dataset_recycling_amplification, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dataset_recycling_amplification, rope).
narrative_ontology:human_readable(dataset_recycling_amplification, "Dataset Recycling Amplification in ML Training Pipelines").
narrative_ontology:topic_domain(dataset_recycling_amplification, "computational_epistemology/information_ecology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dataset_recycling_amplification, model_training_organizations).
narrative_ontology:constraint_beneficiary(dataset_recycling_amplification, dataset_aggregators).
narrative_ontology:constraint_victim(dataset_recycling_amplification, downstream_model_users).
narrative_ontology:constraint_victim(dataset_recycling_amplification, epistemic_diversity_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train foundation models on large-scale datasets that include synthetic outputs from prior models. Benefit from reduced data collection costs and faster iteration cycles. WildChat and similar datasets provide ready-made conversational data at scale without human annotation overhead. The practice is economically rational given competitive pressure to ship models quickly.
narrative_ontology:constraint_stakeholder(dataset_recycling_amplification, model_training_organizations, beneficiary,
    institutional, biographical, mobile, global).

% Curate and distribute training datasets by scraping model outputs, user interactions, and synthetic conversations. Set the composition and filtering criteria that determine what statistical patterns propagate. Operate under open-data norms and academic sharing conventions. Can choose to filter model-generated content but face no strong incentive to do so when such content is high-volume and grammatically clean.
narrative_ontology:constraint_stakeholder(dataset_recycling_amplification, dataset_aggregators, agenda_setter,
    organized, biographical, mobile, global).

% Deploy models in production systems and discover unexpected correlations or statistical quirks that propagate across model families. Bear the cost of debugging spurious patterns, retraining on cleaner data, or working around amplified biases. Exit options are constrained because the recycling pattern is industry-wide and not disclosed in model cards.
narrative_ontology:constraint_stakeholder(dataset_recycling_amplification, downstream_model_users, payer,
    organized, biographical, constrained, global).

% Study distributional shift, model collapse, and epistemic monoculture in ML systems. Document cases like Elias frequency correlation across unrelated model families. Their research reveals the constraint's operation but they lack institutional power to change training practices. Constrained exit because the phenomenon is systemic and affects the entire research domain.
narrative_ontology:constraint_stakeholder(dataset_recycling_amplification, epistemic_diversity_researchers, payer,
    moderate, generational, constrained, global).

% Generate original conversational data through platform interactions, which gets scraped and mixed with synthetic outputs in training sets. Their contributions are diluted by model-generated content and they have no visibility into or control over how their data is recycled. Structurally excluded from decisions about dataset composition.
narrative_ontology:constraint_stakeholder(dataset_recycling_amplification, human_data_contributors, excluded,
    powerless, immediate, trapped, global).

% Audit training pipelines for data provenance, distributional integrity, and collapse risks. Can observe the recycling pattern through statistical forensics and cross-model correlation analysis. Produce recommendations but lack enforcement power over industry training practices.
narrative_ontology:constraint_stakeholder(dataset_recycling_amplification, ml_safety_auditors, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the data scarcity and annotation cost problem for training large language models: synthetic and model-generated conversational data provides high-volume, grammatically fluent training signal without expensive human labeling.
% TRANSFER_FUNCTION: Moves statistical quirks and distributional artifacts from early-generation models into later training sets, then into downstream models and production systems. The cost is borne by users debugging spurious correlations and researchers tracking epistemic collapse.
% ABSENT_VOICES: Human data contributors whose original interactions are diluted by synthetic content, and future model developers who will inherit increasingly homogenized training distributions. Neither group participates in current dataset curation decisions.
% DISAPPEARANCE_RATIONALE: If dataset recycling stopped overnight, training organizations would face immediate data scarcity and higher annotation costs, forcing either smaller models or investment in human data pipelines. Cross-model statistical correlations would decay over subsequent training generations as fresh human data diluted inherited quirks.
% FOUNDING_PROBLEM: Early foundation model training faced a genuine data bottleneck: human-generated conversational data at the scale needed for billion-parameter models did not exist in clean, accessible form.
% FOUNDING_PROBLEM_CORROBORATION: Training organizations attest the data scarcity problem remains live and recycling is necessary for competitive model development. ML safety researchers and epistemic diversity auditors attest that the founding problem has shifted: the bottleneck is now data quality and diversity, not volume, and recycling exacerbates the new problem while solving the old one. Independent analysis from academic ML conferences supports the shifted-bottleneck reading.
narrative_ontology:disappearance_verdict(dataset_recycling_amplification, world_rearranges).
narrative_ontology:founding_problem_status(dataset_recycling_amplification, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dataset_recycling_amplification, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-30',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(dataset_recycling_amplification, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dataset_recycling_amplification_tests).
:- end_tests(dataset_recycling_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38) because the practice imposes real costs on downstream users and researchers who must work around amplified artifacts, but the cost is diffuse and not concentrated on powerless agents. Suppression is moderate (0.42) because the industry-wide adoption of recycling makes clean human data increasingly scarce, but alternatives still exist for well-resourced actors. Theater ratio is low-moderate (0.28) because data provenance documentation exists but often omits the extent of model-generated content in training sets. Accessibility collapse is just above neutral (0.51) because alternatives to recycled data remain available but require significant additional investment. Resistance is moderate (0.47) because epistemic diversity researchers actively document the problem and some organizations are beginning to filter synthetic content, but industry momentum favors the status quo. All measurements use a shared time grid covering the 24-month interval from early adoption to current widespread practice.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (training organizations, dataset aggregators) should compute as rope or low-extraction tangled_rope: they experience genuine coordination benefits and can exit to cleaner data if needed. The payer seats (downstream users, epistemic researchers) should compute as higher-extraction tangled_rope or snare: they experience the constraint as imposed cost with limited alternatives. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Training organizations and dataset aggregators are structural beneficiaries: they collect cost savings and faster iteration cycles from recycling, with mobile exit options allowing them to choose cleaner data if the costs of amplification exceed benefits. Downstream users and epistemic researchers are targets: they bear the debugging costs and epistemic collapse risks with constrained exit because the practice is industry-wide. Human contributors are excluded entirely from the coordination function. The divergence between beneficiary and payer seats drives the perspectival gap: from the training organization's position this is efficient resource use; from the researcher's position it is epistemic pollution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was solving data scarcity for foundation model training. That function remains partially live: high-quality human conversational data at billion-example scale is still expensive to collect. But the constraint now also produces a secondary effect not part of its original mandate: amplification of statistical quirks across model families, creating epistemic monoculture risk. The measurements show extraction and suppression rising over time as the practice becomes entrenched and clean alternatives become scarcer. This is not yet resolved mandatrophy—the founding problem is contested, not clearly dead—but the trajectory suggests drift toward a state where the coordination function is overshadowed by extractive side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amplification_threshold,
    'At what concentration of model-generated content in training data does statistical amplification become detectable in downstream model behavior?',
    'Controlled experiments varying the ratio of human to synthetic data in training sets, measuring cross-model correlation of known statistical quirks like Elias frequency.',
    'A low threshold would establish that even modest recycling produces measurable amplification, supporting stricter filtering. A high threshold would suggest current practices are within safe bounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amplification_threshold, empirical, 'Concentration threshold for detectable statistical amplification').

omega_variable(
    epistemic_collapse_timeline,
    'How many training generations can recycle model outputs before distributional collapse makes models unusable for novel tasks?',
    'Longitudinal study tracking model performance on out-of-distribution tasks across successive training generations using increasingly recycled data.',
    'A short timeline would establish urgent need for intervention; a long timeline would suggest the practice is sustainable with current filtering.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_collapse_timeline, empirical, 'Number of recycling generations before epistemic collapse').

omega_variable(
    coordination_extraction_separability,
    'Can the data scarcity coordination function be preserved while eliminating the amplification extraction, or are they structurally inseparable?',
    'Development and adoption of synthetic data generation methods that preserve distributional diversity while providing training volume, or dataset filtering techniques that remove model-generated content without losing scale.',
    'If separable, the constraint can be reformed to preserve coordination while eliminating extraction. If inseparable, the trade-off is fundamental and requires choosing between data volume and epistemic diversity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction functions are structurally separable').

omega_variable(
    disclosure_sufficiency,
    'Would full disclosure of dataset provenance (percentage model-generated, source model families) in model cards be sufficient to shift the constraint''s operation, or is the problem structural regardless of transparency?',
    'Natural experiment from organizations that adopt comprehensive provenance disclosure: measure whether downstream users change deployment decisions and whether training organizations change data sourcing in response to user pressure.',
    'If disclosure shifts behavior, the constraint is partly a transparency failure and can be addressed through documentation norms. If disclosure has no effect, the problem is structural and requires intervention in training practices themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_sufficiency, empirical, 'Whether transparency alone can address amplification costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dataset_recycling_amplification, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(data_tr_t0, dataset_recycling_amplification, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(data_tr_t0, observed).
narrative_ontology:measurement(data_tr_t6, dataset_recycling_amplification, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(data_tr_t6, observed).
narrative_ontology:measurement(data_tr_t12, dataset_recycling_amplification, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(data_tr_t12, observed).
narrative_ontology:measurement(data_tr_t18, dataset_recycling_amplification, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(data_tr_t18, observed).
narrative_ontology:measurement(data_tr_t24, dataset_recycling_amplification, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(data_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(data_be_t0, dataset_recycling_amplification, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(data_be_t0, observed).
narrative_ontology:measurement(data_be_t6, dataset_recycling_amplification, base_extractiveness, 6, 0.27).
narrative_ontology:measurement_basis(data_be_t6, observed).
narrative_ontology:measurement(data_be_t12, dataset_recycling_amplification, base_extractiveness, 12, 0.32).
narrative_ontology:measurement_basis(data_be_t12, observed).
narrative_ontology:measurement(data_be_t18, dataset_recycling_amplification, base_extractiveness, 18, 0.35).
narrative_ontology:measurement_basis(data_be_t18, observed).
narrative_ontology:measurement(data_be_t24, dataset_recycling_amplification, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(data_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(data_su_t0, dataset_recycling_amplification, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(data_su_t0, observed).
narrative_ontology:measurement(data_su_t6, dataset_recycling_amplification, suppression_requirement, 6, 0.31).
narrative_ontology:measurement_basis(data_su_t6, observed).
narrative_ontology:measurement(data_su_t12, dataset_recycling_amplification, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(data_su_t12, observed).
narrative_ontology:measurement(data_su_t18, dataset_recycling_amplification, suppression_requirement, 18, 0.39).
narrative_ontology:measurement_basis(data_su_t18, observed).
narrative_ontology:measurement(data_su_t24, dataset_recycling_amplification, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(data_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dataset_recycling_amplification, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of alignment_constraint_narrowing: the upstream constraint's narrowing of acceptable model outputs creates the statistical quirks that this constraint then amplifies through recycling. The two constraints form a feedback loop where alignment filtering produces distinctive patterns and dataset recycling propagates those patterns across model families.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
