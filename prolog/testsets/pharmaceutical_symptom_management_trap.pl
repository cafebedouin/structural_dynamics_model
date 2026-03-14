% ============================================================================
% CONSTRAINT STORY: pharmaceutical_symptom_management_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_symptom_management_trap, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pharmaceutical_symptom_management_trap
 *   human_readable: Pharmaceutical Symptom Management Trap
 *   domain: healthcare/pharmaceutical_economics
 *
 * SUMMARY:
 *   The pharmaceutical symptom management trap describes a structural
 *   constraint in healthcare systems where chronic disease treatment focuses
 *   exclusively on symptomatic relief rather than addressing root causes,
 *   creating nested extraction mechanisms: pharmaceutical manufacturers
 *   benefit from recurring medication demand; physicians are constrained by
 *   reimbursement structures that reward acute symptom treatment; patients
 *   become locked into medication regimens that address symptoms but not
 *   underlying pathology, generating iatrogenic complications that justify
 *   additional medications. The constraint exhibits all six DR types from
 *   different perspectives. Extractiveness has risen from 0.32 to 0.58 over
 *   the interval as polypharmacy becomes standard and medication-induced side
 *   effects accumulate. Theater ratio (0.68) reflects that pharmaceutical
 *   efficacy is measured through symptom reduction proxies (blood pressure
 *   readings, pain scores) rather than through functional outcome improvement
 *   or disease-progression halting. The constraint functions as a genuine
 *   coordination mechanism (patients need acute symptom relief) combined with
 *   extractive lock-in (chronic dependence on medications that do not address
 *   root causes). The prevention-oriented public health coalition represents
 *   a structural alternative pathway with genuine sunset logic: as
 *   lifestyle-intervention infrastructure matures and reimbursement models
 *   shift toward value-based outcomes, pharmaceutical symptom management
 *   loses its monopoly on disease management. However, root-cause
 *   interventions remain economically and socially inaccessible for many
 *   populations, potentially creating a bifurcated system where prevention is
 *   available to high-income populations while symptom management trap
 *   persists for economically constrained patients.
 *
 * KEY AGENTS:
 *   - Chronic Disease Patients: Primary victim (powerless/trapped) — physiologically dependent on medications, no exit option without disability or untreated suffering
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — structure medications and marketing to maximize chronic dosing and polypharmacy, capturing recurring revenue
 *   - Primary Care Physicians: Mixed actor (moderate/constrained) — genuine coordination need (symptom management) combined with extraction (prescribing patterns that perpetuate dependence); constrained by reimbursement, marketing, and liability
 *   - Insurance and Healthcare Administration: Institutional actor (institutional/arbitrage) — maintain reimbursement machinery that prioritizes acute symptom treatment over root-cause intervention; perform efficacy measurement through symptom proxies rather than functional outcomes
 *   - Prevention-Oriented Public Health Coalition: Organized agents (organized/mobile) — build alternative pathway (lifestyle intervention, environmental modification) that addresses root causes; see symptom-management approach as temporary institutional failure
 *   - Healthcare System Sustainability: Abstract victim (powerless/trapped) — bears cost of escalating medication complexity and iatrogenic complication burden; no self-correction mechanism within symptom-management frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_symptom_management_trap, 0.58).
domain_priors:suppression_score(pharmaceutical_symptom_management_trap, 0.62).
domain_priors:theater_ratio(pharmaceutical_symptom_management_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_symptom_management_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_symptom_management_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(pharmaceutical_symptom_management_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_symptom_management_trap, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_symptom_management_trap, "Pharmaceutical Symptom Management Trap").
narrative_ontology:topic_domain(pharmaceutical_symptom_management_trap, "healthcare/pharmaceutical_economics").

domain_priors:requires_active_enforcement(pharmaceutical_symptom_management_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_symptom_management_trap, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_symptom_management_trap, prescribing_physicians).
narrative_ontology:constraint_victim(pharmaceutical_symptom_management_trap, chronic_disease_patients).
narrative_ontology:constraint_victim(pharmaceutical_symptom_management_trap, healthcare_system_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHRONIC DISEASE PATIENT (SNARE) — Trapped by physiological dependence, symptom management necessity, and medical gatekeeping. No exit option without accepting disability or untreated suffering. Bears full cost of the extraction mechanism: medication dependence, side effect burden, escalating dosages, iatrogenic complications. The constraint appears as medical necessity but functions as extraction lock.
constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIMARY CARE PHYSICIAN (TANGLED ROPE) — Faces genuine coordination problem: patients need symptom management. But also extracts through prescribing patterns that perpetuate dependence. Constrained by insurance reimbursement structures (favor acute symptom treatment over root-cause intervention), pharmaceutical marketing, and liability concerns. Both benefits from and contributes to the constraint.
constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Genuine coordination function: producing medications that alleviate acute suffering. But structured to benefit maximally from the symptom-management frame: chronic dosing generates recurring revenue, symptom escalation justifies dose increases and combination therapies, side effects justify additional medications. Net beneficiary with arbitrage exits — can transition to other markets or drug classes as needed.
constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PREVENTION-ORIENTED PUBLIC HEALTH COALITION (SCAFFOLD) — Organized agents (public health departments, preventive medicine advocates, lifestyle intervention programs) perceive the symptom-management trap as a temporary institutional failure with a sunset clause. Root-cause interventions (diet, exercise, stress reduction, environmental modification) address underlying pathology rather than masking symptoms. As prevention infrastructure matures and reimbursement models shift toward outcomes-based payment, the symptom-management extraction mechanism loses force. Sunset timeline: 15-25 years for prevention-based approaches to compete with pharmaceutical symptom management in reimbursement and medical education.
constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE AND HEALTHCARE ADMINISTRATION (PITON) — The reimbursement machinery that enforces symptom-management prioritization is substantially performative. Insurance companies track 'medication adherence' and 'symptom control metrics' as proxies for health outcomes, but these metrics decouple from actual health improvement over long timescales. The administrative theater persists through institutional inertia — alternatives (outcome-based reimbursement, root-cause intervention funding) have higher initial complexity costs. The system sees its own metrics as degraded but maintains them because the transition costs are high and revenue depends on activity volume.
constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, acute symptom management is a fundamental biological reality: chronic disease symptoms cause suffering, and pharmacological intervention is a natural response to biological constraint. This perspective risks naturalizing what is actually a contingent institutional choice: the exclusive focus on symptom management rather than root-cause intervention, the reimbursement structures that incentivize chronic medication regimens, and the medical training that prioritizes pharmacological tools. The engine's false summit detector will identify this as naturalization of institutional arrangements.
constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_symptom_management_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_symptom_management_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_symptom_management_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_symptom_management_trap, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_symptom_management_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint demonstrates genuine coordination function (patients need symptom relief) combined with extractive lock-in (chronic medication dependence without addressing root causes). The extraction increases over time as patients accumulate medications and iatrogenic side effects justify additional pharmaceutical interventions. Suppression (0.62): Moderately high. Significant barriers to root-cause intervention include: economic dependency on continued healthcare utilization, information asymmetry about alternative approaches, social and professional constraints on lifestyle modification, medical gatekeeping of treatment options, and pharmaceutical marketing that shapes both physician and patient preferences. However, suppression is not total — some patients and physicians do pursue root-cause approaches, and prevention infrastructure is emerging. Theater ratio (0.68): High and increasing. Pharmaceutical efficacy is increasingly measured through symptom reduction metrics (blood pressure, pain scores, A1c levels) that decouple from actual functional improvement over longer timescales. Medication 'adherence' is tracked and rewarded as a proxy for health outcomes, but adherence to a 15-drug regimen may indicate system failure rather than health success. The theater masks the distinction between treating disease and treating treatment side effects.
 *
 * PERSPECTIVAL GAP:
 *   The physician perspective (tangled_rope) reveals the critical gap: physicians experience genuine coordination need (symptom management) but are structurally incentivized to optimize for symptom metrics rather than root-cause intervention. The constraint appears to them as mixed coordination-extraction rather than pure coordination or pure extraction. The pharmaceutical manufacturer perspective (rope) sees only coordination: they are solving the legitimate problem of symptom relief. The patient perspective (snare) sees pure extraction: the entire system functions to lock them into medication dependence. The prevention coalition perspective (scaffold) sees a temporary institutional failure with an exit route: as outcomes-based reimbursement and prevention infrastructure mature, the symptom-management monopoly decays. The analytical observer risks seeing an immutable natural law (mountain) — treating disease symptoms is inherent to medicine — but the structural data reveals this as naturalization of a specific institutional arrangement (reimbursement for acute symptom treatment, pharmaceutical marketing, medical education focused on pharmacological tools).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. Pharmaceutical manufacturers benefit directly and have exit options (arbitrage); their d is low (~0.15), producing negative chi. Physicians benefit from reimbursement but are constrained by medical training and patient expectations; their d is moderate (~0.55), producing moderate chi. Patients are trapped targets; their d is high (~0.92), producing maximum chi (~1.42 with f(d)). The insurance system maintains the constraint structure to preserve reimbursement volume; their d is low (~0.20) but their structural power is high, making them institutional beneficiaries despite their role in enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The pharmaceutical symptom management trap resolves mandatrophy by distinguishing the genuine coordination function (acute symptom relief) from the extraction mechanism (chronic lock-in without root-cause intervention). The tangled_rope classification is stable across multiple justifications: (1) pharmaceutical manufacturers genuinely solve the coordination problem of symptom management; (2) patients genuinely benefit from acute symptom relief; (3) physicians are genuinely constrained by reimbursement and training structures. But the constraint also extracts: (1) medications treat symptoms without addressing root causes; (2) medication side effects justify additional medications; (3) the system is structured to maximize chronic medication use rather than to achieve health outcomes. The classification is not 'either coordination or extraction' but 'coordination mechanism that has been structurally bent toward extraction.' The scaffold perspective (prevention coalition) shows that this is not immutable — alternative coordination mechanisms exist that solve the same coordination problem (symptom management plus root-cause intervention) with lower extraction overhead. The timeline to scaffold realization depends on overcoming institutional lock-in (medical training, reimbursement structures, pharmaceutical marketing influence) — this is a real constraint but not a fundamental law of medicine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symptom_vs_disease_progression_boundary,
    'At what point does symptom management transition from treating disease to treating iatrogenic side effects?',
    'Longitudinal outcome tracking: years on medication regimen vs actual disease progression; correlation between medication burden and disability-adjusted life years; analysis of medication addition patterns triggered by previous medication side effects',
    'If transition occurs early (< 5 years): the constraint is primarily extractive from year 1. If late (> 15 years): symptom management has genuine therapeutic value for extended periods before becoming extraction-dominant. Timeline threshold determines at what point the classification shifts from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symptom_vs_disease_progression_boundary, empirical, 'Boundary between therapeutic symptom management and iatrogenic extraction').

omega_variable(
    root_cause_intervention_feasibility,
    'Are root-cause interventions (lifestyle modification, environmental change, psychological intervention) actually accessible and effective for the majority of chronic disease patients, or are they accessible only to high-income populations?',
    'Comparative effectiveness studies across socioeconomic strata; accessibility analysis of prevention programs; cost-outcome analysis comparing pharmaceutical management vs root-cause intervention by income level',
    'If root-cause interventions are truly accessible and effective broadly: the scaffold perspective is structural and the sunset timeline is realistic. If limited to high-income populations: the prevention-oriented coalition is aspirational, and the pharmaceutical trap persists as structural necessity for economically constrained patients. This determines whether the constraint is temporary (scaffold) or permanent (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(root_cause_intervention_feasibility, empirical, 'Whether root-cause interventions are broadly accessible and effective').

omega_variable(
    physician_incentive_alignment,
    'Do reimbursement structures and pharmaceutical marketing create genuine misalignment between physician incentives and patient root-cause intervention, or do physicians choose symptom management due to legitimate efficiency constraints?',
    'Analysis of prescribing patterns when reimbursement incentives are altered (e.g., value-based care pilots); comparison of prescribing patterns across systems with different marketing restrictions; physician surveys on barriers to lifestyle-intervention prescription',
    'If misalignment is primary: the pharmaceutical trap is partially extractive (beneficiaries deliberately structure physician incentives). If efficiency constraints are primary: the trap is more coordination-failure than extraction. Determines the classification stability of the physician perspective across different regulatory contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_incentive_alignment, empirical, 'Whether physician incentive misalignment is structural or contingent').

omega_variable(
    patient_agency_in_treatment_preference,
    'Do patients prefer pharmaceutical symptom management because the system constrains their awareness of alternatives, or because they genuinely prefer pharmaceutical management when alternatives are presented?',
    'Patient surveys in systems with robust prevention alternatives and patient education; comparison of treatment choice patterns before and after introduction of integrated lifestyle-intervention programs; analysis of patient agency in contexts with symmetric information about pharmaceutical side effects',
    'If preference is largely awareness-constrained: suppression is primarily informational (reducible through education). If preference persists despite awareness: suppression involves deeper structural traps (economic, time, complexity). Determines whether the suppression metric should be adjusted downward or remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_agency_in_treatment_preference, empirical, 'Patient agency in treatment preference formation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_symptom_management_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharm_tr_t0, pharmaceutical_symptom_management_trap, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pharm_tr_t5, pharmaceutical_symptom_management_trap, theater_ratio, 5, 0.6).
narrative_ontology:measurement(pharm_tr_t10, pharmaceutical_symptom_management_trap, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pharm_be_t0, pharmaceutical_symptom_management_trap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pharm_be_t5, pharmaceutical_symptom_management_trap, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pharm_be_t10, pharmaceutical_symptom_management_trap, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_symptom_management_trap, resource_allocation).
narrative_ontology:boltzmann_floor_override(pharmaceutical_symptom_management_trap, 0.18).
narrative_ontology:affects_constraint(pharmaceutical_symptom_management_trap, pharmaceutical_marketing_information_asymmetry).
narrative_ontology:affects_constraint(pharmaceutical_symptom_management_trap, healthcare_reimbursement_acute_bias).
narrative_ontology:affects_constraint(pharmaceutical_symptom_management_trap, iatrogenic_complication_cascade).

% DUAL FORMULATION NOTE:
% The pharmaceutical symptom management trap is downstream of healthcare reimbursement structures and pharmaceutical marketing practices. Separate constraint stories model the information asymmetry mechanism (pharmaceutical marketing constraints) and the reimbursement bias mechanism (healthcare payment structure constraints). This story focuses on the patient-level lock-in mechanism — the structural features that make symptom-management-only treatment appear necessary rather than contingent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
