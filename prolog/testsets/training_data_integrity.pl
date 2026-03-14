% ============================================================================
% CONSTRAINT STORY: training_data_integrity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_training_data_integrity, []).

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
 *   constraint_id: training_data_integrity
 *   human_readable: Training Data Integrity Constraint in Machine Learning Systems
 *   domain: machine_learning/data_governance
 *
 * SUMMARY:
 *   The training data integrity constraint operates as a hybrid
 *   coordination-extraction mechanism where the need to source, label, and
 *   validate billions of data points creates genuine coordination problems
 *   (standardizing annotation, managing quality assurance, preventing data
 *   duplication) while simultaneously enabling systematic extraction from
 *   data subjects, labeling contractors, and downstream users. The constraint
 *   exhibits all defining characteristics of Tangled Rope: a real
 *   coordination function (models cannot be trained without reliable data)
 *   exists alongside asymmetric extraction (deployers capture value while
 *   subjects and contractors bear costs). The theater ratio (0.65) reflects
 *   that formal governance structures (data sheets, model cards, audit
 *   frameworks) create performative compliance without proportional
 *   functional improvement in actual data quality. The extractiveness
 *   trajectory (0.32 → 0.58 over 8 time periods) shows how cost-cutting
 *   pressures have intensified extraction as the scale of model training has
 *   accelerated — the same coordination function is now deployed at larger
 *   scale with lower verification overhead. This constraint family decomposes
 *   into three structurally distinct claims: (1) data collection and
 *   annotation as a coordination problem (Rope), (2) training data as an
 *   extractive value-capture mechanism (Snare for data subjects), and (3)
 *   regulatory governance of data integrity (Tangled Rope at the regime
 *   level). The analytical observer risks naturalizing what are contingent
 *   institutional choices (opacity in sourcing, cost minimization, weak
 *   enforcement) as inherent properties of machine learning.
 *
 * KEY AGENTS:
 *   - Model Deployers: Primary beneficiary (institutional/arbitrage) — capture value from cheap, abundant data; have multiple data sourcing options and high exit optionality
 *   - Data Subjects: Primary victim (powerless/trapped) — individuals whose data is harvested without meaningful control or compensation; cannot exit once included
 *   - Downstream Users: Secondary victim (powerless/trapped) — affected by biased/degraded models; cannot audit training data or exercise quality control
 *   - Data Labeling Contractors: Mixed agent (moderate/constrained) — economically dependent on annotation work; participate in ecosystem but with low wages and no quality oversight
 *   - Regulatory Regime: Institutional actor (powerful/mobile) — governs data governance but benefits from extractive practices; politically constrained by industry lobbying
 *   - Data Quality Auditing Function: Institutional actor (institutional/arbitrage) — performs formal auditing; benefits from appearance of oversight without bearing functional responsibility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent arrangements as inherent properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(training_data_integrity, 0.58).
domain_priors:suppression_score(training_data_integrity, 0.62).
domain_priors:theater_ratio(training_data_integrity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(training_data_integrity, extractiveness, 0.58).
narrative_ontology:constraint_metric(training_data_integrity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(training_data_integrity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(training_data_integrity, tangled_rope).
narrative_ontology:human_readable(training_data_integrity, "Training Data Integrity Constraint in Machine Learning Systems").
narrative_ontology:topic_domain(training_data_integrity, "machine_learning/data_governance").

domain_priors:requires_active_enforcement(training_data_integrity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(training_data_integrity, model_deployers).
narrative_ontology:constraint_beneficiary(training_data_integrity, data_labeling_contractors).
narrative_ontology:constraint_victim(training_data_integrity, downstream_users).
narrative_ontology:constraint_victim(training_data_integrity, training_data_subjects).
narrative_ontology:constraint_victim(training_data_integrity, model_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAINING DATA SUBJECT (SNARE) — Individuals whose data is used to train models without meaningful consent or control. No exit option from the data pipeline once their information is harvested. Bears full cost of model bias, privacy breaches, and downstream harms. Maximum experienced extraction.
constraint_indexing:constraint_classification(training_data_integrity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM USER (SNARE) — End-users affected by models trained on degraded data. Cannot audit training sets. Trapped in systems where data quality problems are hidden by model deployments. Bears full extraction cost through biased, unreliable, or harmful model behavior.
constraint_indexing:constraint_classification(training_data_integrity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DATA LABELING CONTRACTOR (TANGLED ROPE) — Low-wage workers performing annotation work. Constrained by economic dependency and information asymmetry. Benefits from participation in data ecosystem (employment, income) while bearing extraction (low wages, no quality oversight, repetitive cognitive labor). Genuine coordination function exists (annotation enables model training) alongside asymmetric extraction.
constraint_indexing:constraint_classification(training_data_integrity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MODEL DEPLOYER (ROPE) — Benefits from access to cheap, abundant training data. Experiences the constraint as a coordination mechanism: data sourcing, labeling standardization, and quality assurance enable model development. Net beneficiary — extraction flows toward this agent. High exit optionality (can switch data sources, relabel, retrain).
constraint_indexing:constraint_classification(training_data_integrity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REGIME (TANGLED ROPE) — Governs data governance (GDPR, CCPA, etc.) yet benefits from economic productivity gains of unencumbered data use. Must coordinate privacy protection with innovation incentives. Suppression manifests as enforcement gaps, regulatory arbitrage between jurisdictions, and technical complexity obscuring violations. Mobile exit options but politically constrained by industry lobbying.
constraint_indexing:constraint_classification(training_data_integrity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DATA QUALITY AUDIT FUNCTION (PITON) — Formal auditing, data sheets, model cards, and quality benchmarks are largely performative. Organizations publish data documentation as a ritual (showing compliance) while actual data sourcing practices remain opaque. Theater ratio high — the audit apparatus has atrophied as a functional mechanism, maintained through institutional inertia. Deployers benefit from the appearance of quality assurance without bearing actual verification costs.
constraint_indexing:constraint_classification(training_data_integrity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From a civilizational view, one might naturalize training data quality problems as inherent to machine learning: scaling to billions of data points mathematically requires statistical sampling and annotation error is unavoidable. This perspective risks treating contingent institutional choices (cost minimization, opacity, weak enforcement) as natural laws. The engine's false summit detector will flag this as naturalization of extractive arrangements.
constraint_indexing:constraint_classification(training_data_integrity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(training_data_integrity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(training_data_integrity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(training_data_integrity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(training_data_integrity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(training_data_integrity, TR),
    TR >= 0.70.

:- end_tests(training_data_integrity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Model deployers capture significant value from training data through reduced labeling costs, access to proprietary corpora, and avoidance of consent friction. The extraction is not total (0.70+) because some coordination benefits are genuine — data standardization and quality assurance create real value. The 8-period trajectory (0.32 → 0.58) reflects intensifying cost-cutting as model scale has grown. Suppression (0.62): High. Multiple suppression mechanisms: (1) data subjects lack knowledge of inclusion, (2) labeling contractors face economic dependency and limited information, (3) regulatory enforcement gaps allow arbitrage, (4) technical opacity obscures data sourcing practices, (5) downstream users cannot audit training sets. Theater ratio (0.65): Moderate-high. Formal audit structures (data sheets, model cards, model governance) are increasingly performative — organizations publish documentation as compliance ritual while actual data practices remain opaque. The ratio has increased over the interval as regulatory pressure has driven documentation without proportional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival gap. Data subject (Snare): 'I cannot exit, I bear all costs, extraction is total.' Model deployer (Rope): 'I am solving a coordination problem, I benefit, I have alternatives.' Labeling contractor (Tangled Rope): 'I benefit economically but am exploited through low wages and opacity.' Regulatory regime (Tangled Rope): 'I must coordinate protection and innovation while industry circumvents my enforcement.' Audit function (Piton): 'We perform governance ritual; actual quality improvement is limited.' Analytical observer (false Mountain): 'Data quality lag is inherent to ML; we cannot fundamentally solve this.' The gap reveals that what appears as an immutable property of machine learning is actually a contingent institutional arrangement where cost-cutting drives extraction and opacity enables suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position: their power level, exit options, and beneficiary/victim status. Data subjects + trapped exit + victim status = d approaching 1.0, f(d) ≈ 1.42, maximum experienced extraction. Downstream users + trapped exit + victim status = similar maximum extraction. Labeling contractors + constrained exit + mixed beneficiary/victim = d ≈ 0.55-0.65, f(d) ≈ 0.75-1.00, moderate-high extraction but with some benefit participation. Model deployers + arbitrage exit + beneficiary status = d approaching 0.05-0.15, f(d) ≈ -0.12 to -0.01, negative or minimal extraction (they benefit). Regulatory regime + mobile exit but politically constrained + enforcer role = d ≈ 0.55-0.65, f(d) ≈ 0.75-1.00, moderate extraction from the constraint (enforcement burden, political opposition). Audit function + arbitrage exit + beneficiary status = d ≈ 0.15, f(d) ≈ -0.01, minimal effective extraction (they benefit from the ritual). The scope modifier σ(S) = 1.2 (global) amplifies chi for all perspectives, reflecting that data flows cross borders and create global-scale extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY TRIGGER: Extractiveness 0.58 > 0.46 requires omega variables and measurement data (present). Does not require full mandatrophy resolution (0.70+) but the classification is fragile. The constraint resolves the mandatrophy by decomposing into its three sub-constraints: (1) DATA SOURCING COORDINATION (Rope): the genuine problem of finding, obtaining, and standardizing data sources. (2) DATA SUBJECT EXTRACTION (Snare): the asymmetric harvesting of personal data without meaningful consent or compensation. (3) LABELING CONTRACTOR EXPLOITATION (Tangled Rope): economic participation mixed with extraction. Each has different ε and requires different policy responses. The integrated story is Tangled Rope because all three are entangled — deployers solve the coordination problem by externalizing costs onto trapped subjects and constrained contractors. The mandatrophy is resolved by recognizing that the constraint family has three members with increasing extraction intensity, linked by network edges, not by forcing a single classification to represent them all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    annotation_error_vs_intentional_degradation,
    'What proportion of training data quality problems stem from inherent annotation error vs intentional cost-cutting and suppression of quality signals?',
    'Comparative analysis of audit data quality vs actual data quality in same systems; correlation between cost minimization pressure and measured error rates; investigation of whether higher-cost labeling produces proportionally better outcomes.',
    'If primarily annotation error: constraint is Rope (coordination problem). If primarily cost-driven degradation: constraint is Snare (extraction mechanism). Mixed split determines Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annotation_error_vs_intentional_degradation, empirical, 'Whether data quality problems are from unavoidable annotation error or deliberate cost-cutting').

omega_variable(
    consent_feasibility_boundary,
    'Is meaningful informed consent for training data use technologically and economically feasible at scale, or is it fundamentally impossible given data collection realities?',
    'Analysis of opt-in vs opt-out consent mechanisms in practice; correlation between consent friction and data availability; investigation of whether granular consent reduces model performance or viability.',
    'If feasible: data subjects have exit option (mobile/constrained rather than trapped). Classification shifts from Snare toward Tangled Rope or Rope. If infeasible: trapped exit is structural; Snare classification is correct and consent framing is false.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_feasibility_boundary, empirical, 'Whether meaningful consent is technologically/economically feasible for training data').

omega_variable(
    regulatory_arbitrage_persistence,
    'Can regulatory regimes establish enforcement mechanisms that prevent jurisdictional arbitrage (training on GDPR-compliant EU data by routing through non-EU intermediaries), or is arbitrage fundamentally unmanageable at global scale?',
    'Analysis of actual enforcement actions; tracking of data flows across jurisdictions; assessment of whether technical data masking/pseudonymization defeats regulatory intent or creates enforceable audit trails.',
    'If arbitrage preventable: regulatory regime has real teeth; mobile exit becomes constrained. If arbitrage inevitable: regulatory suppression metric drops (regulators cannot enforce) but extractive freedom increases (deployers have arbitrage exits regulators cannot police).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_persistence, empirical, 'Whether global regulatory arbitrage for training data can be prevented').

omega_variable(
    audit_effectiveness_vs_theater,
    'Do formal data audits, model cards, and data documentation genuinely improve training data quality, or are they primarily performative compliance with minimal functional impact?',
    'Longitudinal comparison of model behavior before/after audit implementation; correlation between audit results and measured model bias; investigation of whether organizations change practices based on audit findings.',
    'If effective: audit function is Rope or Tangled Rope; coordination mechanism works. If performative: classification is Piton; inertial institutional structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audit_effectiveness_vs_theater, empirical, 'Whether data auditing actually improves quality or is primarily theater').

omega_variable(
    data_subject_awareness_and_exit,
    'To what extent do training data subjects know their data is being used, and would this knowledge change their behavior if they did?',
    'Survey data on awareness levels; analysis of opt-out behavior when choices are available; investigation of whether information disclosure reduces data availability or alters annotation quality.',
    'High awareness + exit availability: subjects shift from trapped to constrained or mobile. Low awareness: subjects remain trapped. If awareness causes data withdrawal: suppression metric rises for deployers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_awareness_and_exit, empirical, 'Whether training data subjects know/could act on their inclusion in datasets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(training_data_integrity, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tdi_tr_t0, training_data_integrity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tdi_tr_t4, training_data_integrity, theater_ratio, 4, 0.54).
narrative_ontology:measurement(tdi_tr_t8, training_data_integrity, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(tdi_be_t0, training_data_integrity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tdi_be_t4, training_data_integrity, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(tdi_be_t8, training_data_integrity, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(training_data_integrity, resource_allocation).
narrative_ontology:affects_constraint(training_data_integrity, model_bias_and_fairness).
narrative_ontology:affects_constraint(training_data_integrity, data_subject_privacy_rights).
narrative_ontology:affects_constraint(training_data_integrity, labeling_contractor_labor_standards).

% DUAL FORMULATION NOTE:
% Training data integrity decomposes into three structurally distinct constraints: (1) data_sourcing_coordination (ε ≈ 0.25, Rope) — standardizing sources, preventing duplication, managing scale; (2) data_subject_extraction (ε ≈ 0.75, Snare) — harvesting without consent/compensation; (3) labeling_contractor_exploitation (ε ≈ 0.55, Tangled Rope) — economic participation mixed with wage suppression. This integrated story represents the Tangled Rope classification where all three are entangled. Upstream effects on model_bias_and_fairness (low-quality training data produces biased models) and downstream effects on data_subject_privacy_rights and labeling_contractor_labor_standards.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(training_data_integrity, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
