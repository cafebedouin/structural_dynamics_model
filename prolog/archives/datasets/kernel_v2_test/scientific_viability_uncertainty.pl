% ============================================================================
% CONSTRAINT STORY: scientific_viability_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scientific_viability_uncertainty, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scientific_viability_uncertainty
 *   human_readable: Scientific Viability Uncertainty in Polygenic Scoring
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The scientific viability uncertainty in polygenic scoring represents a
 *   fundamental epistemic constraint at the frontier of genomic medicine: the
 *   question of whether polygenic scores can achieve sufficient predictive
 *   accuracy and cross-population portability to deliver clinical utility is
 *   answerable only through empirical investigation, not through policy
 *   choice or institutional arrangement. This constraint appears as a
 *   mountain from all perspectives — patients, clinicians, healthcare
 *   systems, and analytical observers all face the same irreducible
 *   uncertainty about whether the science will work. However, the constraint
 *   exhibits a false summit signature: identifiable beneficiaries (genomics
 *   research industry, direct-to-consumer testing companies, precision
 *   medicine advocates) profit from treating the uncertainty as resolved and
 *   deploying polygenic scores commercially before clinical utility is
 *   established. The temporal measurements show modest but steady increases
 *   in extractiveness (0.05 → 0.12) and theater ratio (0.10 → 0.15) over the
 *   2010-2022 interval, reflecting the growth of commercial polygenic testing
 *   marketed to consumers despite ongoing scientific debate about validity.
 *   The constraint's low base extractiveness (0.12) and suppression (0.08)
 *   reflect that the uncertainty itself is not coercive — researchers and
 *   clinicians can and do acknowledge the limitations openly — but the
 *   commercial deployment creates extraction by charging patients for tests
 *   of uncertain value. The high accessibility collapse (0.92) and low
 *   resistance (0.05) reflect that once the statistical genetics is
 *   understood, the epistemic constraint becomes obvious: you cannot know
 *   whether a prediction model will generalize to new populations without
 *   testing it in those populations, and current GWAS samples are heavily
 *   skewed toward European ancestry (~78%). The constraint is not enforced by
 *   any institution — it emerges from the structure of the scientific
 *   question itself.
 *
 * KEY AGENTS:
 *   - Individual Patient: Primary target (powerless/trapped) — faces irreducible uncertainty about whether polygenic scores will work for their ancestry group; cannot exit the epistemic constraint through choice or advocacy
 *   - Clinical Geneticist: Moderate agent (moderate/constrained) — must make treatment decisions under uncertainty; constrained by current evidence base but cannot resolve the scientific question through clinical practice alone
 *   - Healthcare System: Institutional agent (institutional/constrained) — faces resource allocation decisions under fundamental uncertainty about population-scale clinical utility; cannot exit through policy alone
 *   - Genomics Research Industry: Primary beneficiary (institutional/arbitrage) — profits from research funding, commercial partnerships, and intellectual property regardless of whether clinical utility is achieved; can exit to other research domains if polygenic scoring fails
 *   - Direct-to-Consumer Testing Companies: Primary beneficiary (institutional/arbitrage) — profits from selling polygenic risk reports to consumers before clinical validity is established; low accountability for accuracy claims
 *   - Precision Medicine Advocates: Secondary beneficiary (organized/mobile) — institutional and funding legitimacy depends on polygenic scoring success; can pivot to other precision medicine modalities if genomic prediction fails
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as an irreducible epistemic limit but must distinguish genuine natural law from false summit (constructed constraint that benefits identifiable actors)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scientific_viability_uncertainty, 0.12).
domain_priors:suppression_score(scientific_viability_uncertainty, 0.08).
domain_priors:theater_ratio(scientific_viability_uncertainty, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scientific_viability_uncertainty, extractiveness, 0.12).
narrative_ontology:constraint_metric(scientific_viability_uncertainty, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(scientific_viability_uncertainty, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(scientific_viability_uncertainty, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(scientific_viability_uncertainty, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scientific_viability_uncertainty, mountain).
narrative_ontology:human_readable(scientific_viability_uncertainty, "Scientific Viability Uncertainty in Polygenic Scoring").
narrative_ontology:topic_domain(scientific_viability_uncertainty, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:emerges_naturally(scientific_viability_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scientific_viability_uncertainty, genomics_research_industry).
narrative_ontology:constraint_beneficiary(scientific_viability_uncertainty, direct_to_consumer_testing_companies).
narrative_ontology:constraint_beneficiary(scientific_viability_uncertainty, precision_medicine_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PATIENT (MOUNTAIN) — Faces the irreducible uncertainty about whether polygenic scores will work for their specific ancestry group and clinical context. Cannot exit the epistemic constraint — the science either delivers clinical utility or it doesn't, regardless of patient advocacy or choice.
constraint_indexing:constraint_classification(scientific_viability_uncertainty, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CLINICAL GENETICIST (MOUNTAIN) — Must make treatment decisions under irreducible uncertainty about score validity. Constrained by current evidence base but cannot change the underlying scientific question through clinical practice alone. The uncertainty is a structural feature of the current state of genomic knowledge.
constraint_indexing:constraint_classification(scientific_viability_uncertainty, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE SYSTEM (MOUNTAIN) — Faces resource allocation decisions under fundamental uncertainty about whether polygenic scoring will achieve clinical utility at population scale. Cannot exit the constraint through policy alone — the scientific viability question must be resolved empirically over generational timescales.
constraint_indexing:constraint_classification(scientific_viability_uncertainty, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Recognizes the constraint as an irreducible epistemic limit at the current frontier of genomic science. The question 'can polygenic scores achieve sufficient accuracy and portability?' is answerable only through empirical investigation, not through institutional arrangement or policy choice. However, the presence of identifiable beneficiaries (genomics industry, DTC testing companies, precision medicine advocates) who profit from treating the uncertainty as resolved creates a false summit signature.
constraint_indexing:constraint_classification(scientific_viability_uncertainty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scientific_viability_uncertainty_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(scientific_viability_uncertainty, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scientific_viability_uncertainty, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(scientific_viability_uncertainty, ExtMetricName, E),
    domain_priors:suppression_score(scientific_viability_uncertainty, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(scientific_viability_uncertainty),
    narrative_ontology:constraint_metric(scientific_viability_uncertainty, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(scientific_viability_uncertainty, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(scientific_viability_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Low but non-zero. The scientific uncertainty itself is not extractive — it is a genuine epistemic limit. However, commercial deployment of polygenic scores before clinical utility is established creates extraction: patients pay for tests of uncertain value, and the genomics industry captures research funding and market share by treating the uncertainty as resolved. The modest increase over time (0.05 → 0.12) reflects the growth of direct-to-consumer testing marketed with overstated claims. Suppression (0.08): Very low. The constraint is not coercive — researchers and clinicians can and do acknowledge the limitations openly. The modest suppression reflects publication bias favoring positive results and industry pressure to downplay portability concerns in marketing, but these are weak compared to the open scientific debate. Theater ratio (0.15): Low. Most activity around polygenic scoring is functional research (GWAS, validation studies, methods development). The theater component reflects regulatory review processes that approve tests without requiring population-specific validation, and industry marketing that presents uncertain science as established clinical tool. Accessibility collapse (0.92): Very high. Once the statistical genetics is understood, the epistemic constraint is obvious: prediction models trained on one population may not generalize to others, and current samples are heavily European. No alternative framing makes this constraint disappear. Resistance (0.05): Very low. The constraint meets minimal resistance because it is a straightforward consequence of statistical principles and sample composition. Resistance exists only from commercial actors with incentive to deploy prematurely.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as mountain because the scientific viability question is genuinely irreducible at the current state of knowledge — no amount of policy intervention or institutional rearrangement can resolve whether polygenic scores will achieve clinical utility without empirical investigation. However, the false summit signature emerges from the beneficiary structure: the genomics industry, DTC testing companies, and precision medicine advocates profit from premature deployment and have incentive to naturalize the uncertainty (treat it as resolved or inevitable) rather than acknowledge it openly. The analytical observer's task is to distinguish the genuine epistemic constraint (mountain) from the extractive institutional arrangement layered on top of it (the commercial deployment before validation). The temporal measurements show the extraction mechanism growing over time as the DTC testing market expands, while the underlying scientific uncertainty remains unresolved. This is the classic false summit pattern: a real natural law (statistical genetics principles) is invoked to justify a constructed constraint (premature commercialization) that benefits identifiable actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure reveals the false summit signature. Patients (powerless/trapped) are victims of premature commercialization — they bear the cost of uncertain tests and potential clinical harms from inaccurate risk predictions. Clinicians (moderate/constrained) are secondary victims — they face liability and decision-making burden under uncertainty. Healthcare systems (institutional/constrained) are tertiary victims — they face resource allocation pressure to cover tests of unproven value. The genomics research industry, DTC testing companies, and precision medicine advocates are beneficiaries — they profit from research funding, commercial sales, and institutional legitimacy regardless of whether clinical utility is achieved. The analytical observer recognizes this asymmetry: if polygenic scoring were a genuine natural law (pure mountain), there would be no beneficiaries — nobody profits from the speed of light or the incompleteness theorems. The presence of actors who benefit from treating uncertainty as resolved is the diagnostic signal of a false summit. The engine's directionality computation will assign low d (beneficiary) to industry actors and high d (victim) to patients and clinicians, producing a perspectival gap that reveals the extraction mechanism layered onto the genuine epistemic constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN WITH FALSE SUMMIT SIGNATURE: The constraint resolves the mandatrophy by demonstrating that a genuine epistemic limit (mountain) can coexist with an extractive institutional arrangement (false summit). The scientific viability uncertainty is real — polygenic scores may or may not achieve clinical utility, and the question is answerable only through empirical investigation over generational timescales. This is a legitimate mountain: the constraint emerges from the structure of complex trait genetics and current sample limitations, not from institutional enforcement. However, the presence of beneficiaries who profit from treating the uncertainty as resolved creates a false summit: the genomics industry and DTC testing companies extract value by deploying polygenic scores commercially before clinical validity is established, and precision medicine advocates gain institutional legitimacy by overstating the technology's readiness. The false summit detector (FSM) will flag this constraint because it declares beneficiaries despite claiming mountain status. The omega variables document the irreducible ambiguity: is this a genuine natural law that will resolve through scientific progress, or a constructed constraint that benefits actors who weaponize uncertainty to justify premature deployment? The temporal measurements provide the empirical test: if extractiveness continues to rise while scientific uncertainty remains unresolved, the false summit hypothesis is confirmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the scientific viability uncertainty an irreducible epistemic limit (genuine mountain) or a constructed constraint that benefits identifiable actors who profit from premature deployment?',
    'Longitudinal analysis of: (1) whether predictive accuracy improves with larger, more diverse GWAS samples at rates consistent with statistical power calculations (genuine epistemic limit) or plateaus despite sample growth (constructed limit); (2) whether industry actors systematically overstate clinical utility in marketing while acknowledging uncertainty in scientific publications (extraction signal); (3) whether regulatory frameworks evolve to require population-specific validation or remain permissive (policy choice masking as natural law)',
    'If genuine mountain: the constraint is an irreducible feature of complex trait genetics and will resolve only through scientific progress. If constructed: the uncertainty is being weaponized to justify premature commercialization, and the mountain classification naturalizes what is actually extractive institutional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Whether viability uncertainty is natural epistemic limit or constructed extraction mechanism').

omega_variable(
    portability_threshold,
    'What level of cross-population predictive accuracy constitutes ''sufficient portability'' for clinical deployment?',
    'Consensus development process among clinical genetics professional societies; cost-effectiveness analysis comparing polygenic score-guided interventions to standard of care across diverse populations; ethical framework for acceptable accuracy disparities',
    'If threshold is set high (e.g., >80% accuracy parity across populations): current polygenic scores fail viability test and the constraint remains binding. If threshold is set low (e.g., >50% parity): many existing scores pass and the constraint dissolves into a coordination problem about implementation standards.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(portability_threshold, preference, 'Normative threshold for acceptable cross-population accuracy').

omega_variable(
    environmental_variance_ceiling,
    'Is there a fundamental ceiling on genomic prediction imposed by environmental variance, or will larger samples and better methods continue to improve accuracy?',
    'Heritability partitioning studies; twin studies; longitudinal GWAS with increasing sample sizes tracking variance explained trajectories; theoretical modeling of genotype-environment interaction limits',
    'If ceiling exists and is low (e.g., <30% variance explained for most complex diseases): polygenic scoring has inherent limits and the mountain is genuine. If no ceiling or ceiling is high: current limitations are sample size artifacts and the constraint is temporary (scaffold, not mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_variance_ceiling, empirical, 'Whether environmental variance imposes fundamental ceiling on genomic prediction').

omega_variable(
    sample_diversity_sufficiency,
    'Can existing GWAS sample diversity (currently ~78% European ancestry) be corrected through statistical methods, or is prospective diverse recruitment required?',
    'Comparative validation studies: polygenic scores derived from European samples with statistical corrections vs. scores derived from ancestry-matched samples; analysis of whether correction methods eliminate or merely reduce portability gaps',
    'If statistical correction suffices: the constraint is a coordination problem (rope/scaffold) solvable through better methods. If prospective recruitment required: the constraint is a genuine epistemic limit requiring generational investment in diverse cohorts (mountain at biographical timescale, scaffold at generational).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sample_diversity_sufficiency, empirical, 'Whether sample diversity gaps can be corrected statistically or require prospective recruitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scientific_viability_uncertainty, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pgs_theater_2010, scientific_viability_uncertainty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pgs_theater_2013, scientific_viability_uncertainty, theater_ratio, 3, 0.12).
narrative_ontology:measurement(pgs_theater_2016, scientific_viability_uncertainty, theater_ratio, 6, 0.13).
narrative_ontology:measurement(pgs_theater_2019, scientific_viability_uncertainty, theater_ratio, 9, 0.14).
narrative_ontology:measurement(pgs_theater_2022, scientific_viability_uncertainty, theater_ratio, 12, 0.15).

% Extraction over time
narrative_ontology:measurement(pgs_extract_2010, scientific_viability_uncertainty, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(pgs_extract_2013, scientific_viability_uncertainty, base_extractiveness, 3, 0.08).
narrative_ontology:measurement(pgs_extract_2016, scientific_viability_uncertainty, base_extractiveness, 6, 0.1).
narrative_ontology:measurement(pgs_extract_2019, scientific_viability_uncertainty, base_extractiveness, 9, 0.11).
narrative_ontology:measurement(pgs_extract_2022, scientific_viability_uncertainty, base_extractiveness, 12, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(pgs_suppress_2010, scientific_viability_uncertainty, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(pgs_suppress_2016, scientific_viability_uncertainty, suppression_requirement, 6, 0.07).
narrative_ontology:measurement(pgs_suppress_2022, scientific_viability_uncertainty, suppression_requirement, 12, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scientific_viability_uncertainty, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific polygenic score deployment decisions (clinical implementation, insurance coverage, regulatory approval) but represents a distinct epistemic constraint. Downstream constraints have their own extractiveness values reflecting institutional arrangements; this constraint's extractiveness reflects the commercial exploitation of scientific uncertainty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
