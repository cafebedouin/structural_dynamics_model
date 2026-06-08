% ============================================================================
% CONSTRAINT STORY: clinical_deskilling_automation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clinical_deskilling_automation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: clinical_deskilling_automation
 *   human_readable: Clinical Deskilling Through AIGHP Automation
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The integration of AI-Guided Healthcare Platforms (AIGHP) into clinical
 *   workflows creates a structural tension between efficiency gains and
 *   workforce deskilling. As AIGHP systems handle increasingly complex
 *   diagnostic and treatment decisions, clinicians develop automation bias
 *   (uncritical acceptance of system recommendations) and lose the diagnostic
 *   skills that medical training emphasizes. This constraint is downstream of
 *   dependency_lock_in: once institutions commit to AIGHP infrastructure, the
 *   workforce adapts to AIGHP-dependent practice patterns, and skill erosion
 *   becomes self-reinforcing. The constraint exhibits high theater ratio
 *   (0.81) because medical education continues to train diagnostic skills
 *   that practice environments no longer sustain — graduates enter
 *   AIGHP-heavy workflows where manual diagnostic capacity atrophies
 *   immediately. The piton classification reflects that the original
 *   coordination function (decision support) has been overshadowed by
 *   performative maintenance: institutions maintain the appearance of
 *   clinical autonomy (override protocols, continuing education requirements)
 *   while actual practice is automation-dependent. The constraint's
 *   extractiveness has increased over the 8-year interval as AIGHP
 *   penetration deepened and skill erosion became irreversible for many
 *   clinicians.
 *
 * KEY AGENTS:
 *   - Clinical Workforce: Primary victim (powerless to moderate / trapped to constrained) — bears skill erosion, automation bias, and career lock-in; extraction concentrates here
 *   - Patients During System Failures: Secondary victim (powerless / trapped) — face acute harm when AIGHP fails and clinicians cannot compensate; no exit from healthcare system
 *   - AIGHP Platform Vendors: Primary beneficiary (institutional / arbitrage) — capture recurring revenue from dependency lock-in and network effects
 *   - Healthcare Administrators: Secondary beneficiary (institutional / arbitrage) — gain efficiency and cost reduction; experience constraint as coordination
 *   - Medical Education System: Institutional actor (institutional / constrained) — maintains traditional training curricula despite practice environment mismatch; high theater ratio
 *   - Clinical Autonomy Coalition: Organized resistance (organized / mobile) — medical societies, patient safety advocates, regulators building hybrid training models and override protocols; sees constraint as transitional with sunset
 *   - Analytical Observer: Civilizational view (analytical / analytical) — recognizes genuine coordination function alongside substantial extraction; tangled rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clinical_deskilling_automation, 0.68).
domain_priors:suppression_score(clinical_deskilling_automation, 0.72).
domain_priors:theater_ratio(clinical_deskilling_automation, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clinical_deskilling_automation, extractiveness, 0.68).
narrative_ontology:constraint_metric(clinical_deskilling_automation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(clinical_deskilling_automation, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clinical_deskilling_automation, piton).
narrative_ontology:human_readable(clinical_deskilling_automation, "Clinical Deskilling Through AIGHP Automation").
narrative_ontology:topic_domain(clinical_deskilling_automation, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:requires_active_enforcement(clinical_deskilling_automation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clinical_deskilling_automation, aighp_platform_vendors).
narrative_ontology:constraint_beneficiary(clinical_deskilling_automation, healthcare_administrators).
narrative_ontology:constraint_beneficiary(clinical_deskilling_automation, early_career_efficiency_metrics).
narrative_ontology:constraint_victim(clinical_deskilling_automation, clinical_workforce).
narrative_ontology:constraint_victim(clinical_deskilling_automation, patients_during_system_failures).
narrative_ontology:constraint_victim(clinical_deskilling_automation, medical_education_depth).
narrative_ontology:constraint_vindicates(clinical_deskilling_automation, automation_efficiency_doctrine).
narrative_ontology:constraint_vindicates(clinical_deskilling_automation, clinical_standardization_imperative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESKILLED CLINICIAN (SNARE) — Trapped in AIGHP-dependent practice patterns with eroding diagnostic capacity. Cannot exit to non-AIGHP settings (skills atrophied), cannot function effectively when systems fail. Career trajectory locked into automation dependency with no recovery path. Maximum extraction: bears full cost of skill erosion while system captures efficiency gains.
constraint_indexing:constraint_classification(clinical_deskilling_automation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT DURING SYSTEM FAILURE (SNARE) — Faces immediate harm when AIGHP systems fail and clinicians cannot compensate due to skill atrophy. No exit from the healthcare system, no alternative when automation fails. Extraction is acute and concentrated: the efficiency gains accrue to administrators and vendors; the failure risk concentrates on patients.
constraint_indexing:constraint_classification(clinical_deskilling_automation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MID-CAREER CLINICIAN (TANGLED ROPE) — Experiences genuine coordination benefits (faster workflows, decision support) alongside extraction (skill erosion, automation bias). Constrained exit: can move to less-automated settings but at career cost (lower pay, fewer resources). Mixed experience: the system both enables and degrades clinical capacity.
constraint_indexing:constraint_classification(clinical_deskilling_automation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HEALTHCARE ADMINISTRATOR (ROPE) — Benefits from efficiency gains, reduced labor costs, standardized workflows. Experiences the constraint as pure coordination: AIGHP integration solves throughput problems and reduces variance. Arbitrage exit: can switch vendors or revert to traditional models if AIGHP fails to deliver. Net beneficiary with full agency.
constraint_indexing:constraint_classification(clinical_deskilling_automation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AIGHP PLATFORM VENDOR (ROPE) — Primary beneficiary. Captures recurring revenue from dependency lock-in, benefits from network effects as more institutions adopt. Experiences the constraint as coordination: providing decision support infrastructure that healthcare systems demand. Full arbitrage exit: can pivot to other markets if healthcare becomes unprofitable.
constraint_indexing:constraint_classification(clinical_deskilling_automation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MEDICAL EDUCATION SYSTEM (PITON) — Maintains traditional diagnostic training curricula despite AIGHP ubiquity in practice settings. The training ritual persists through institutional inertia: accreditation bodies require diagnostic skill demonstration, but graduates enter AIGHP-dependent workflows where those skills atrophy immediately. High theater ratio: the education system performs skill-building that the practice environment does not sustain.
constraint_indexing:constraint_classification(clinical_deskilling_automation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CLINICAL AUTONOMY COALITION (SCAFFOLD) — Organized resistance (medical societies, patient safety advocates, regulatory bodies) sees AIGHP integration as a temporary coordination problem with a sunset: hybrid training models, mandatory override protocols, and system failure drills are building resilience. Views current deskilling as transitional — the constraint's extractive phase will end when governance catches up to deployment.
constraint_indexing:constraint_classification(clinical_deskilling_automation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes genuine coordination function (AIGHP enables precision medicine at scale, reduces diagnostic variance) alongside substantial extraction (skill erosion, automation bias, vendor lock-in). The constraint coordinates genomic data interpretation while extracting clinical autonomy and resilience. Not a piton (the function is real, not atrophied) and not a snare (coordination benefits are genuine for some agents). Tangled rope: both mechanisms operate simultaneously.
constraint_indexing:constraint_classification(clinical_deskilling_automation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clinical_deskilling_automation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(clinical_deskilling_automation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(clinical_deskilling_automation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(clinical_deskilling_automation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(clinical_deskilling_automation, TR),
    TR >= 0.70.

:- end_tests(clinical_deskilling_automation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. AIGHP vendors and administrators capture efficiency gains and cost reductions, while clinicians bear skill erosion and career lock-in. Patients bear acute harm during system failures. The extraction is substantial but not maximal because some clinicians (mid-career with retained skills) experience genuine coordination benefits. Suppression (0.72): High. Clinicians face strong barriers to exit: AIGHP-dependent practice patterns are industry standard, non-AIGHP settings offer lower pay and fewer resources, and skill atrophy makes exit to traditional practice difficult or impossible. Institutions face vendor lock-in (data migration costs, workflow disruption, retraining burden). Suppression has increased over the interval as AIGHP penetration deepened. Theater ratio (0.81): Very high. Medical education performs diagnostic skill-building that practice environments do not sustain. Institutions maintain override protocols and continuing education requirements that are rarely exercised in practice. The appearance of clinical autonomy persists while actual practice is automation-dependent. Theater ratio has increased sharply as the gap between training and practice widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — AIGHP integration — appears as pure coordination (rope) to beneficiaries, pure extraction (snare) to trapped victims, mixed coordination-extraction (tangled rope) to agents with partial exit, theatrical maintenance (piton) to the education system, and a transitional problem with sunset (scaffold) to organized resistance. The deskilled clinician sees a snare: trapped in automation dependency with no recovery path. The patient during system failure sees a snare: acute harm with no exit. The mid-career clinician sees tangled rope: genuine workflow benefits alongside skill erosion. The administrator sees rope: efficiency gains solving throughput problems. The vendor sees rope: providing infrastructure that healthcare systems demand. The education system sees piton: maintaining training rituals that practice does not sustain. The clinical autonomy coalition sees scaffold: a temporary problem being solved by hybrid training and governance. The analytical observer sees tangled rope: genuine coordination function (precision medicine at scale) alongside substantial extraction (skill erosion, vendor lock-in). The perspectival gap is wide because the constraint's benefits and costs are structurally asymmetric and concentrated on different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. AIGHP vendors and healthcare administrators are declared beneficiaries with arbitrage exit — they experience low or negative effective extraction (the constraint subsidizes them). Clinical workforce is declared victim with trapped to constrained exit — they experience high effective extraction (the constraint extracts from them). Patients during system failures are victims with trapped exit — they experience maximum extraction. The medical education system is not a direct beneficiary or victim but experiences the constraint as theatrical maintenance (piton perspective). The clinical autonomy coalition has mobile exit and sees the constraint as transitional (scaffold perspective). The analytical observer recognizes both coordination and extraction (tangled rope perspective). The directionality derivation captures the structural asymmetry: efficiency gains flow to vendors and administrators; skill erosion and failure risk concentrate on clinicians and patients.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification is justified by the constraint's high theater ratio and the atrophy of its original coordination function. AIGHP was introduced as decision support — a tool to augment clinical judgment, not replace it. But as automation deepened, the coordination function (augmentation) atrophied into dependency, and what remains is largely performative: institutions maintain override protocols that are rarely used, education systems train diagnostic skills that practice does not sustain, and the appearance of clinical autonomy persists while actual practice is automation-dependent. The piton classification does not deny that AIGHP provides genuine value (it does — precision medicine at scale, reduced diagnostic variance) but recognizes that the original mandate (augment, not replace) has been abandoned while the institutional theater (training, protocols, autonomy rhetoric) persists. The mandatrophy is not yet resolved because the theater continues: institutions have not acknowledged that the augmentation model failed and the dependency model succeeded. The scaffold perspective (clinical autonomy coalition) represents an alternative resolution path: if hybrid training and governance succeed, the constraint transitions from piton to rope (genuine coordination with reduced extraction). But the current state is piton: degraded function maintained theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_recovery_timeline,
    'Can clinicians recover diagnostic skills after prolonged AIGHP dependency, and over what timeline?',
    'Longitudinal studies of clinicians transitioning from AIGHP-heavy to traditional settings; controlled retraining interventions; neuroplasticity research on expert skill atrophy and recovery',
    'If recovery is possible within 6-12 months: deskilling is reversible coordination cost (lower extractiveness). If recovery requires years or is incomplete: deskilling is permanent extraction (higher extractiveness, stronger snare classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_recovery_timeline, empirical, 'Timeline and feasibility of diagnostic skill recovery after AIGHP dependency').

omega_variable(
    automation_bias_magnitude,
    'What is the true rate and severity of automation bias in AIGHP-assisted clinical decisions?',
    'Comparison of override rates in AIGHP vs. non-AIGHP settings for equivalent cases; analysis of adverse events attributable to uncritical AIGHP acceptance; controlled studies of diagnostic accuracy with and without AIGHP support',
    'If automation bias is rare and low-severity: coordination benefits dominate (rope from more perspectives). If automation bias is common and high-severity: extraction dominates (snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_bias_magnitude, empirical, 'Prevalence and severity of automation bias in AIGHP-assisted decisions').

omega_variable(
    system_failure_recovery_capacity,
    'Can healthcare systems maintain acceptable patient outcomes during AIGHP system failures, given current workforce skill levels?',
    'Analysis of patient outcomes during documented AIGHP outages; simulation studies of system failure scenarios; comparison of recovery capacity in AIGHP-heavy vs. traditional settings',
    'If recovery capacity is adequate: deskilling is overstated (lower extractiveness). If recovery capacity is severely degraded: deskilling creates acute patient harm (higher extractiveness, stronger snare classification for patients).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_failure_recovery_capacity, empirical, 'Healthcare system resilience during AIGHP failures').

omega_variable(
    training_curriculum_sufficiency,
    'Do current medical training curricula adequately prepare clinicians for AIGHP-assisted practice, or are they training for a practice environment that no longer exists?',
    'Gap analysis between training content and practice requirements; longitudinal tracking of skill utilization post-graduation; employer feedback on graduate preparedness',
    'If curricula are adequate: education system is coordinating (rope). If curricula are misaligned: education system is theatrical (piton), and the gap between training and practice amplifies deskilling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(training_curriculum_sufficiency, empirical, 'Alignment between medical training and AIGHP-assisted practice requirements').

omega_variable(
    vendor_lock_in_reversibility,
    'Can healthcare institutions switch AIGHP vendors or revert to non-AIGHP workflows without prohibitive cost or patient harm?',
    'Analysis of vendor switching costs (data migration, retraining, workflow disruption); case studies of institutions that attempted to switch or revert; assessment of interoperability standards and data portability',
    'If switching is feasible: administrators retain arbitrage exit (rope classification holds). If switching is prohibitively costly: administrators are also trapped (tangled rope or snare classification), and the constraint''s suppression is higher than base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_lock_in_reversibility, empirical, 'Feasibility and cost of switching AIGHP vendors or reverting to traditional workflows').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clinical_deskilling_automation, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deskill_theater_t0, clinical_deskilling_automation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(deskill_theater_t2, clinical_deskilling_automation, theater_ratio, 2, 0.48).
narrative_ontology:measurement(deskill_theater_t4, clinical_deskilling_automation, theater_ratio, 4, 0.61).
narrative_ontology:measurement(deskill_theater_t6, clinical_deskilling_automation, theater_ratio, 6, 0.73).
narrative_ontology:measurement(deskill_theater_t8, clinical_deskilling_automation, theater_ratio, 8, 0.81).

% Extraction over time
narrative_ontology:measurement(deskill_extract_t0, clinical_deskilling_automation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(deskill_extract_t2, clinical_deskilling_automation, base_extractiveness, 2, 0.39).
narrative_ontology:measurement(deskill_extract_t4, clinical_deskilling_automation, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(deskill_extract_t6, clinical_deskilling_automation, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(deskill_extract_t8, clinical_deskilling_automation, base_extractiveness, 8, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deskill_suppress_t0, clinical_deskilling_automation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(deskill_suppress_t2, clinical_deskilling_automation, suppression_requirement, 2, 0.51).
narrative_ontology:measurement(deskill_suppress_t4, clinical_deskilling_automation, suppression_requirement, 4, 0.59).
narrative_ontology:measurement(deskill_suppress_t6, clinical_deskilling_automation, suppression_requirement, 6, 0.67).
narrative_ontology:measurement(deskill_suppress_t8, clinical_deskilling_automation, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clinical_deskilling_automation, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of dependency_lock_in: once institutions commit to AIGHP infrastructure (dependency_lock_in), workforce adaptation and skill erosion follow (clinical_deskilling_automation). The two constraints are structurally distinct: dependency_lock_in describes institutional commitment and switching costs; clinical_deskilling_automation describes workforce skill erosion and automation bias. They have different victim sets (institutions vs. clinicians/patients), different extractiveness values (dependency_lock_in is tangled rope with moderate extraction; clinical_deskilling_automation is piton with high extraction), and different temporal dynamics (dependency_lock_in is immediate; deskilling is biographical to generational).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
