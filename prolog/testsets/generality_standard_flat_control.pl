% ============================================================================
% CONSTRAINT STORY: generality_standard_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generality_standard_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: generality_standard_flat_control
 *   human_readable: AGI Generality Standard
 *   domain: epistemology/measurement/forecasting
 *
 * SUMMARY:
 *   The AGI generality standard is the operationalized definition of what
 *   makes a capability 'general' rather than narrow—the benchmark suites,
 *   task batteries, and performance thresholds that let the AI field
 *   coordinate around shared capability assessments. It emerged to solve a
 *   genuine coordination problem: without a shared standard, capability
 *   claims were incommensurable and forecasting was impossible. But the
 *   standard has accumulated extractive overhead as it calcified: benchmark
 *   designers and leading labs shape what counts as generality, forecasters
 *   and independent researchers bear the cost when benchmarks prove
 *   inadequate, and alternative operationalizations are suppressed by the
 *   institutional weight of the dominant framework. The constraint is claimed
 *   as tangled_rope because it exhibits both genuine coordination function
 *   and asymmetric extraction, with active enforcement maintaining the
 *   standard against competing definitions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generality_standard_flat_control, 0.68).
domain_priors:suppression_score(generality_standard_flat_control, 0.71).
domain_priors:theater_ratio(generality_standard_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generality_standard_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(generality_standard_flat_control, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(generality_standard_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(generality_standard_flat_control, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(generality_standard_flat_control, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generality_standard_flat_control, tangled_rope).
narrative_ontology:human_readable(generality_standard_flat_control, "AGI Generality Standard").
narrative_ontology:topic_domain(generality_standard_flat_control, "epistemology/measurement/forecasting").

domain_priors:requires_active_enforcement(generality_standard_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(generality_standard_flat_control, generality_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generality_standard_flat_control, capability_benchmark_designers).
narrative_ontology:constraint_beneficiary(generality_standard_flat_control, ai_safety_institutions).
narrative_ontology:constraint_beneficiary(generality_standard_flat_control, leading_ai_labs).
narrative_ontology:constraint_victim(generality_standard_flat_control, capability_forecasters).
narrative_ontology:constraint_victim(generality_standard_flat_control, independent_researchers).
narrative_ontology:constraint_victim(generality_standard_flat_control, policy_analysts).
narrative_ontology:constraint_vindicates(generality_standard_flat_control, measurability_of_intelligence).
narrative_ontology:constraint_vindicates(generality_standard_flat_control, benchmark_validity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain the benchmark suites that operationalize 'general' capability. They choose which tasks count as evidence of generality, what performance thresholds matter, and how to weight different capability dimensions. Their benchmarks become the de facto standard because leading labs report against them and safety institutions cite them in policy recommendations.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, capability_benchmark_designers, agenda_setter,
    institutional, biographical, mobile, global).

% Benefit from a stable, legible generality standard that lets them demonstrate progress to investors and regulators. They participate in benchmark design consortia and can steer which capabilities get measured. When a benchmark becomes inconvenient they can propose alternatives or argue their system exhibits 'true' generality that the benchmark misses, but the existence of a shared standard reduces the cost of capability claims.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, leading_ai_labs, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(generality_standard_flat_control, leading_ai_labs, agenda_setter).

% Need operationalized generality thresholds to ground policy recommendations about when systems require oversight, when deployment is premature, or when capability jumps warrant intervention. They benefit from benchmark consensus because it makes their recommendations legible to policymakers, even when the benchmarks imperfectly capture the risks they care about.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, ai_safety_institutions, beneficiary,
    institutional, generational, constrained, global).

% Must forecast AGI timelines and capability trajectories using whatever generality standard is institutionally recognized. When the standard shifts or proves inadequate they bear reputational cost for forecasts that were calibrated to the old standard. They cannot exit to a private definition of generality because their forecasts are evaluated against consensus benchmarks.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, capability_forecasters, payer,
    moderate, biographical, constrained, global).

% Critique benchmark validity and propose alternative operationalizations of generality, but lack the institutional weight to shift the standard. Their work is evaluated against the dominant benchmarks even when their research questions require different capability measures. Publishing requires engaging with the standard framework even to argue against it.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, independent_researchers, payer,
    moderate, biographical, constrained, global).

% Must translate technical capability claims into policy recommendations using whatever generality standard the technical community provides. When benchmarks fail to predict deployment risks or capability jumps they bear the cost of policy misalignment, but they lack the technical authority to reject the standard or propose alternatives that would be taken seriously.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, policy_analysts, payer,
    organized, biographical, constrained, national).

% Study construct validity, operationalization adequacy, and measurement theory but are structurally excluded from benchmark design processes. Their critiques about what 'general' could or should mean are treated as philosophical rather than technical and do not shape the standards that govern capability assessment.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, philosophy_of_measurement_community, excluded,
    moderate, generational, analytical, global).

% Analyze how the generality standard functions as a coordination mechanism and what it reveals about the relationship between operationalization and the thing being operationalized. They see the full structure: genuine coordination need, extractive overhead from premature standardization, and the gap between benchmark performance and the capability concept it proxies.
narrative_ontology:constraint_stakeholder(generality_standard_flat_control, epistemology_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared operationalization of 'general' capability so that labs, forecasters, safety institutions, and policymakers can communicate about AI progress using comparable metrics rather than incommensurable capability claims.
% TRANSFER_FUNCTION: Moves epistemic authority from those who question benchmark validity to those who design and maintain the benchmarks; moves reputational risk from labs making capability claims to forecasters and researchers whose work is evaluated against benchmark-defined standards.
% ABSENT_VOICES: Philosophy of measurement researchers who study construct validity are structurally excluded from benchmark design. Alternative operationalizations of generality that would foreground different capability dimensions are not represented in the standard-setting process.
% DISAPPEARANCE_RATIONALE: If the generality standard vanished overnight, capability claims would fragment into lab-specific definitions, forecasting would lose its shared reference frame, safety institutions would struggle to ground policy thresholds, and the field would reorganize around competing operationalizations until a new coordination equilibrium emerged.
% FOUNDING_PROBLEM: Early AI capability assessment was fragmented: each lab defined 'general' differently, forecasters could not compare systems across organizations, and policy discussions about AGI timelines had no shared empirical referent.
% FOUNDING_PROBLEM_CORROBORATION: Benchmark designers and leading labs attest the coordination problem is live and the standard solves it. Independent researchers and philosophy of measurement scholars attest the founding problem is partly solved but the current standard has calcified prematurely, suppressing alternative operationalizations that might better capture the capability concept; their testimony comes from outside the benchmark-design institutions and is documented in measurement theory literature and capability forecasting post-mortems.
narrative_ontology:disappearance_verdict(generality_standard_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(generality_standard_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(generality_standard_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-29',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(generality_standard_flat_control, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generality_standard_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(generality_standard_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(generality_standard_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the standard concentrates epistemic authority in benchmark-design institutions while distributing reputational risk to forecasters and researchers who must work within the framework. Suppression is high (0.71) because alternative operationalizations face institutional barriers: publishing requires engaging with the dominant benchmarks, policy recommendations must cite recognized standards, and forecasts are evaluated against consensus metrics. Theater ratio is moderate (0.42): the benchmarks do measure real capabilities, but a growing share of benchmark activity is defending the standard's validity and updating it incrementally rather than solving the original coordination problem. The measurement series shows accumulation over the interval as the standard matured from flexible coordination tool to enforced framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (benchmark designers, leading labs) the standard is genuine coordination they maintain and improve. From the payer seats (forecasters, independent researchers, policy analysts) the same structure operates as enforced operationalization that suppresses alternative approaches and concentrates risk on those who must forecast or analyze using inadequate metrics. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Benchmark designers and leading labs are structural beneficiaries: they set the standard, can navigate around inconvenient benchmarks, and benefit from the coordination the standard provides. Capability forecasters, independent researchers, and policy analysts are targets: they must use the standard, bear costs when it fails, and have constrained exit because their work is evaluated against it. AI safety institutions are beneficiaries despite constrained exit: they need the standard for policy legibility even when it imperfectly captures their concerns. Philosophy of measurement researchers are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy dynamics: the founding coordination problem (fragmented capability assessment) is partly solved, but the standard persists with increasing extractive overhead as benchmark validity becomes contested and alternative operationalizations are suppressed. The theater ratio trajectory shows the shift from functional coordination to defensive maintenance of the standard's authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    construct_validity_gap,
    'Do the benchmark tasks that operationalize ''general'' capability actually measure the construct the field cares about, or has the standard drifted toward measuring what is easy to benchmark?',
    'Systematic comparison of benchmark performance against deployment capability: if systems that excel on generality benchmarks fail at tasks the field considers paradigmatically general, or if systems that fail benchmarks succeed at those tasks, the construct validity is low.',
    'Low construct validity would establish that the standard''s coordination function is decoupled from the capability concept it claims to measure, supporting the reading that the standard persists as institutional coordination rather than valid measurement. High construct validity would support the benchmark designers'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construct_validity_gap, empirical, 'Whether generality benchmarks measure the capability construct they claim to operationalize.').

omega_variable(
    premature_standardization,
    'Did the field standardize the generality definition before the capability concept was sufficiently understood, and is the standard now suppressing conceptual progress?',
    'Historical analysis of capability theory development relative to benchmark adoption, plus measurement of how often benchmark updates lag conceptual advances in understanding what ''general'' means.',
    'If standardization was premature, the constraint''s extractiveness is higher than coordination cost alone would justify—the standard is extracting from those who would advance the concept. If standardization timing was appropriate, more of the measured extraction is necessary coordination overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(premature_standardization, conceptual, 'Whether the standard calcified before the capability concept matured.').

omega_variable(
    benchmark_gaming_vs_capability,
    'Are systems increasingly optimized for benchmark performance rather than the underlying general capability, and if so, does this represent Goodhart drift in the standard itself?',
    'Comparison of benchmark performance trends against independent capability assessments; measurement of how much capability gain comes from benchmark-specific optimization versus domain-general improvement.',
    'Substantial benchmark gaming would indicate the standard has become the target rather than the measure, validating the theater ratio trajectory and supporting higher extractiveness readings. Minimal gaming would support the standard''s continued validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_gaming_vs_capability, empirical, 'Whether optimization pressure has shifted from capability to benchmark performance.').

omega_variable(
    alternative_operationalization_suppression,
    'Are alternative operationalizations of generality suppressed by institutional weight of the dominant standard, or do they fail to gain traction because they are less valid?',
    'Natural experiment from research communities that adopt alternative standards: if those communities produce better capability predictions or more valid assessments, suppression is institutional rather than merit-based.',
    'If suppression is institutional, the constraint''s suppression metric understates the actual barrier to alternative approaches and the extractiveness is correspondingly higher. If alternatives fail on merit, the suppression is lower and more of the constraint is genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_operationalization_suppression, conceptual, 'Whether the standard suppresses alternatives through institutional power or validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generality_standard_flat_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, generality_standard_flat_control, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gene_tr_t5, generality_standard_flat_control, theater_ratio, 5, 0.28).
narrative_ontology:measurement(gene_tr_t10, generality_standard_flat_control, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gene_tr_t15, generality_standard_flat_control, theater_ratio, 15, 0.39).
narrative_ontology:measurement(gene_tr_t20, generality_standard_flat_control, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, generality_standard_flat_control, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gene_be_t5, generality_standard_flat_control, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(gene_be_t10, generality_standard_flat_control, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(gene_be_t15, generality_standard_flat_control, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(gene_be_t20, generality_standard_flat_control, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, generality_standard_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(gene_su_t5, generality_standard_flat_control, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(gene_su_t10, generality_standard_flat_control, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(gene_su_t15, generality_standard_flat_control, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(gene_su_t20, generality_standard_flat_control, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generality_standard_flat_control, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
