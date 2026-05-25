% ============================================================================
% CONSTRAINT STORY: performance_measurement_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_measurement_asymmetry, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_measurement_asymmetry
 *   human_readable: Performance Measurement Asymmetry in Healthcare AI Validation
 *   domain: healthcare_ai/clinical_practice/technology_governance
 *
 * SUMMARY:
 *   The performance measurement asymmetry in healthcare AI validation creates
 *   a staged pathway from controlled simulation studies to clinical
 *   deployment. AI systems are initially evaluated using synthetic scenarios
 *   with physician patient-actors (typically 20 standardized cases) to
 *   demonstrate algorithmic competence, then subjected to FDA/regulatory
 *   approval processes requiring real-world safety validation before clinical
 *   use. This constraint coordinates the validation pipeline by providing a
 *   low-cost filter for technical competence before expensive clinical
 *   trials. The asymmetry is structural — simulation studies cannot fully
 *   replicate the complexity of real clinical environments — but serves a
 *   coordination function rather than an extractive one. All institutional
 *   actors (AI developers, academic researchers, regulators, clinical
 *   institutions) benefit from the staged approach, which makes the
 *   validation problem tractable. The constraint exhibits low extraction
 *   (0.22) and low suppression (0.18), with modest theater (0.35) reflecting
 *   that some simulation studies are more performative than predictive. The
 *   key analytical question is whether the asymmetry remains coordination
 *   (high simulation fidelity, low patient safety externality) or degrades
 *   toward extraction (simulation studies provide false confidence,
 *   real-world failure modes are systematically missed).
 *
 * KEY AGENTS:
 *   - AI Development Teams: Primary beneficiary (institutional/mobile) — simulation studies enable rapid iterative development before clinical trials
 *   - Academic Research Groups: Primary beneficiary (institutional/mobile) — standardized benchmarks enable reproducible algorithmic comparison
 *   - Regulatory Agencies: Primary beneficiary (institutional/constrained) — staged validation makes regulatory review tractable by filtering non-viable candidates early
 *   - Clinical Institutions: Primary beneficiary (institutional/mobile) — can track promising systems through validation pipeline without committing to unvalidated technology
 *   - Clinician End-Users: Beneficiary (moderate/mobile) — staged validation ensures systems reaching practice have passed both technical and safety gates
 *   - Patients: Potential victim if simulation fidelity is low (powerless/trapped) — would bear safety risk if simulation studies systematically miss real-world failure modes, but not currently a victim under base case assumptions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees staged validation as legitimate coordination mechanism for high-stakes AI deployment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_measurement_asymmetry, 0.22).
domain_priors:suppression_score(performance_measurement_asymmetry, 0.18).
domain_priors:theater_ratio(performance_measurement_asymmetry, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_measurement_asymmetry, extractiveness, 0.22).
narrative_ontology:constraint_metric(performance_measurement_asymmetry, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(performance_measurement_asymmetry, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_measurement_asymmetry, rope).
narrative_ontology:human_readable(performance_measurement_asymmetry, "Performance Measurement Asymmetry in Healthcare AI Validation").
narrative_ontology:topic_domain(performance_measurement_asymmetry, "healthcare_ai/clinical_practice/technology_governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_measurement_asymmetry, ai_development_teams).
narrative_ontology:constraint_beneficiary(performance_measurement_asymmetry, academic_research_groups).
narrative_ontology:constraint_beneficiary(performance_measurement_asymmetry, regulatory_agencies).
narrative_ontology:constraint_beneficiary(performance_measurement_asymmetry, clinical_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AI DEVELOPMENT TEAMS (ROPE) — Simulation-based validation provides a standardized, reproducible coordination mechanism for iterative development. The asymmetry between simulation and deployment is a legitimate staged-validation pathway: controlled scenarios enable rapid hypothesis testing before expensive clinical trials. Low extraction — the constraint coordinates development work efficiently.
constraint_indexing:constraint_classification(performance_measurement_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: ACADEMIC RESEARCH GROUPS (ROPE) — Simulation studies solve the coordination problem of comparing algorithmic approaches on standardized benchmarks. The 20-scenario design enables reproducible comparison across research groups. The gap between simulation and clinical deployment is understood as a necessary validation stage, not an extractive barrier. Beneficiary of the coordination function.
constraint_indexing:constraint_classification(performance_measurement_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCIES (ROPE) — The measurement asymmetry coordinates the validation pipeline: simulation studies provide preliminary evidence of algorithmic competence; clinical trials provide deployment safety evidence. The staged approach reduces regulatory burden by filtering out non-viable candidates early. Constrained exit (cannot unilaterally change validation standards) but net beneficiary — the asymmetry makes their job tractable.
constraint_indexing:constraint_classification(performance_measurement_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CLINICAL INSTITUTIONS (ROPE) — Hospitals and clinics benefit from the staged validation: simulation studies provide early signal of which AI systems are worth piloting; regulatory approval provides safety assurance before full deployment. The asymmetry coordinates resource allocation — institutions can track promising systems through the pipeline without committing to unvalidated technology. Mobile exit (can choose which systems to adopt).
constraint_indexing:constraint_classification(performance_measurement_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: CLINICIAN END-USERS (ROPE) — Physicians using AI decision support benefit from the measurement asymmetry: simulation studies demonstrate algorithmic logic on standardized cases; clinical validation demonstrates safety in real-world conditions. The gap between the two is a feature, not a bug — it ensures that systems reaching clinical practice have passed both technical and safety gates. Low experienced extraction.
constraint_indexing:constraint_classification(performance_measurement_asymmetry, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — The performance measurement asymmetry is a coordination mechanism for staged validation in high-stakes domains. Simulation studies provide a low-cost, high-throughput filter for algorithmic competence; clinical trials provide high-cost, low-throughput validation of real-world safety. The asymmetry is not extractive — it solves the coordination problem of how to validate complex AI systems without exposing patients to unvalidated technology. The constraint exhibits low extraction, low suppression, and genuine coordination function across all perspectives.
constraint_indexing:constraint_classification(performance_measurement_asymmetry, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_measurement_asymmetry_tests).
:- end_tests(performance_measurement_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The measurement asymmetry creates a modest career and funding advantage for AI systems with high simulation performance, but this is largely a legitimate first-mover reward for technical competence. The asymmetry does not create significant rent extraction — all institutional actors benefit from the coordination function. The value reflects that some simulation studies are optimized for benchmark performance rather than clinical relevance (mild gaming), but the gaming is not severe. Suppression (0.18): Low. Alternative validation pathways exist (direct clinical trials, real-world evidence studies, post-market surveillance). The staged approach is not mandatory — some systems bypass simulation studies entirely. Barriers to alternative validation are primarily resource constraints (clinical trials are expensive) rather than regulatory prohibition. Theater ratio (0.35): Moderate-low. Some simulation studies are performative — designed to demonstrate algorithmic sophistication rather than predict clinical utility — but most serve a genuine filtering function. The theater has increased modestly over the interval as simulation benchmarks have become more standardized and gameable, but remains well below the piton threshold (0.70).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all agents classify it as rope. The uniformity reflects that the measurement asymmetry genuinely solves a coordination problem (how to validate AI systems efficiently without exposing patients to unvalidated technology) and does not create significant extraction. The staged validation pathway benefits all institutional actors: developers get rapid feedback, researchers get reproducible benchmarks, regulators get tractable review processes, clinicians get safety-validated systems. The lack of perspectival gap is diagnostic — it indicates a genuine coordination mechanism rather than a contested extraction dynamic. The omega variables identify conditions under which the gap would emerge: if simulation fidelity is low, patients become victims and see the constraint as snare; if regulatory capture is high, the constraint becomes tangled_rope with mixed coordination and extraction. But under base case assumptions (high fidelity, low capture, low externality), the rope classification is stable across all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries are institutional actors with mobile or constrained exit options. AI development teams, academic research groups, and clinical institutions have mobile exit — they can choose alternative validation pathways or opt out of AI development/adoption entirely. Regulatory agencies have constrained exit — they cannot unilaterally change validation standards without legislative/policy changes — but are net beneficiaries because the staged approach makes their regulatory function tractable. No victims are declared in the base case because the constraint does not currently extract from any agent group — patients would become victims only if simulation fidelity is low (omega variable 1) or patient safety externality is high (omega variable 3), neither of which is established. The directionality derivation produces low d values for all perspectives (beneficiaries with mobile/constrained exit → d ≈ 0.10-0.25 → low/negative chi), consistent with rope classification across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination mechanisms can exhibit structural asymmetries without being extractive. The measurement asymmetry — simulation studies measure different properties than clinical deployment requires — is real and consequential, but it serves a coordination function rather than enabling rent extraction. The mandatrophy question 'Is this coordination or extraction?' is answered by the structural data: low extractiveness (0.22), low suppression (0.18), all institutional actors as beneficiaries, no declared victims, and uniform rope classification across perspectives. The constraint is coordination. The omega variables preserve analytical humility by identifying conditions under which the classification would change: if simulation fidelity degrades, if regulatory capture increases, or if patient safety externalities emerge, the constraint would reclassify toward tangled_rope or snare. But the current structural evidence supports rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what level of simulation fidelity does the measurement asymmetry transition from coordination (enabling efficient development) to extraction (creating a misleading validation pathway)?',
    'Longitudinal tracking of simulation performance vs clinical performance for deployed AI systems; identification of systematic gaps that simulation studies fail to predict',
    'If simulation fidelity is high (correlation > 0.85 between simulation and clinical performance): asymmetry remains coordination. If low (correlation < 0.60): asymmetry becomes extractive — simulation studies provide false confidence, and the constraint reclassifies toward tangled_rope or snare from patient safety perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Simulation fidelity threshold for coordination vs extraction').

omega_variable(
    regulatory_capture_risk,
    'Does the staged validation pathway create regulatory capture opportunities where AI developers influence simulation benchmark design to favor their systems?',
    'Analysis of benchmark design governance; identification of conflicts of interest in scenario selection; comparison of industry-designed vs independent benchmarks',
    'If capture is minimal: rope classification holds. If capture is significant: the constraint reclassifies toward tangled_rope — genuine coordination function exists but is contaminated by extractive benchmark gaming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Whether benchmark design is captured by AI developers').

omega_variable(
    patient_safety_externality,
    'Does the measurement asymmetry externalize patient safety risk by allowing systems with high simulation performance but poor real-world robustness to reach clinical deployment?',
    'Post-market surveillance data; adverse event reporting for AI-assisted clinical decisions; comparison of simulation-predicted vs observed failure modes',
    'If externality is minimal (post-market performance matches simulation predictions): rope classification holds. If externality is significant (simulation studies systematically miss real-world failure modes): constraint reclassifies toward tangled_rope or snare from patient perspectives, with patients as victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_safety_externality, empirical, 'Whether measurement asymmetry externalizes patient safety risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_measurement_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_meas_tr_t0, performance_measurement_asymmetry, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_meas_tr_t3, performance_measurement_asymmetry, theater_ratio, 3, 0.3).
narrative_ontology:measurement(perf_meas_tr_t6, performance_measurement_asymmetry, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(perf_meas_be_t0, performance_measurement_asymmetry, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(perf_meas_be_t3, performance_measurement_asymmetry, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(perf_meas_be_t6, performance_measurement_asymmetry, base_extractiveness, 6, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_measurement_asymmetry, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is structurally independent — it represents a validation methodology asymmetry rather than a specific AI system or clinical application. Downstream constraints (specific AI diagnostic systems, treatment recommendation algorithms, clinical decision support tools) would link to this constraint via affects_constraints if their validation pathways exhibit this measurement asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
