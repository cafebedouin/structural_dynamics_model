% ============================================================================
% CONSTRAINT STORY: emrgency_medicine_clinical_guidelines
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emrgency_medicine_clinical_guidelines, []).

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
 *   constraint_id: emrgency_medicine_clinical_guidelines
 *   human_readable: Emergency Medicine Clinical Guidelines as Decision-Support Constraint
 *   domain: medical/legal/institutional
 *
 * SUMMARY:
 *   Emergency medicine clinical guidelines (PECARN for pediatric head trauma,
 *   Ottawa Ankle Rules, SIRS criteria, sepsis bundles) are introduced as
 *   decision-support tools to standardize care, reduce unnecessary testing,
 *   and improve patient safety. They function as a coordination mechanism in
 *   high-acuity, resource-constrained settings where rapid triage and
 *   evidence-based protocols are essential. However, guidelines exhibit
 *   increasing extractive properties as they mature: institutional inertia
 *   causes them to persist beyond their evidence base; liability exposure
 *   pressures physicians to follow guidelines even when clinical judgment
 *   contradicts them; patient populations drift from the original validation
 *   cohorts, causing guideline sensitivity to degrade; and performance
 *   metrics increasingly emphasize guideline 'adherence' rather than patient
 *   outcomes. The constraint exhibits a tangled rope structure: genuine
 *   coordination (standardized care, reduced cognitive load) coupled with
 *   asymmetric extraction (suppressed clinical judgment, trapped liability
 *   exposure, atypical patients forced into standard protocols). The theater
 *   ratio has risen from 0.35 to 0.64 over the measurement interval,
 *   indicating that guideline citations increasingly serve performative
 *   functions (defending care decisions through protocol authority) rather
 *   than validating clinical reasoning.
 *
 * KEY AGENTS:
 *   - Emergency Physicians: Primary victims (powerless/trapped) — face liability exposure for guideline deviation and institutional enforcement of protocol adherence
 *   - Patients with Atypical Presentations: Primary victims (powerless/trapped) — classified by guidelines despite clinical presentation falling outside parameters; overtested or undertested as a result
 *   - Hospital Compliance & Risk Management: Primary beneficiaries (institutional/arbitrage) — genuine coordination benefit from standardized care and liability protection; full exit capacity
 *   - Insurance Payors: Secondary beneficiaries (institutional/arbitrage) — cost control and standardized reimbursement; full exit capacity
 *   - Conscious Guideline Users: Secondary victims (moderate/constrained) — benefit from decision support but constrained by inflexible application; can advocate for modification but face friction
 *   - Guideline Development Bodies: Institutional actors (organized/constrained) — maintain authority over guidelines; theater ratio rises as guideline citations become performative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination and asymmetric extraction in the same institutional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emrgency_medicine_clinical_guidelines, 0.52).
domain_priors:suppression_score(emrgency_medicine_clinical_guidelines, 0.58).
domain_priors:theater_ratio(emrgency_medicine_clinical_guidelines, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emrgency_medicine_clinical_guidelines, extractiveness, 0.52).
narrative_ontology:constraint_metric(emrgency_medicine_clinical_guidelines, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(emrgency_medicine_clinical_guidelines, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emrgency_medicine_clinical_guidelines, tangled_rope).
narrative_ontology:human_readable(emrgency_medicine_clinical_guidelines, "Emergency Medicine Clinical Guidelines as Decision-Support Constraint").
narrative_ontology:topic_domain(emrgency_medicine_clinical_guidelines, "medical/legal/institutional").

domain_priors:requires_active_enforcement(emrgency_medicine_clinical_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emrgency_medicine_clinical_guidelines, hospital_administrators).
narrative_ontology:constraint_beneficiary(emrgency_medicine_clinical_guidelines, insurance_payors).
narrative_ontology:constraint_beneficiary(emrgency_medicine_clinical_guidelines, guideline_authors).
narrative_ontology:constraint_victim(emrgency_medicine_clinical_guidelines, emergency_physicians).
narrative_ontology:constraint_victim(emrgency_medicine_clinical_guidelines, patient_autonomy).
narrative_ontology:constraint_victim(emrgency_medicine_clinical_guidelines, atypical_presentations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGENCY PHYSICIAN (SNARE) — Trapped by liability exposure, peer pressure, and institutional enforced compliance. Guideline deviation triggers chart audits, malpractice liability even when deviation improves outcomes, and career consequences. Cannot exit; must follow protocols even when clinical judgment contradicts them. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ATYPICAL PATIENT (SNARE) — Clinical presentation falls outside guideline parameters but guideline still applied. Overtesting when guideline triggers without clinical correlation; undertesting when patient doesn't match guideline risk-stratification. Trapped in standardized protocol regardless of individual presentation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GUIDELINE-AWARE PHYSICIAN (TANGLED ROPE) — Benefits from decision support: guidelines reduce cognitive load in high-acuity settings, provide legal protection, and coordinate care across institutions. But constrained by inflexible application; experiences tension between guideline compliance and clinical judgment. Can advocate for guideline modification but faces institutional friction. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HOSPITAL COMPLIANCE SYSTEM (ROPE) — Genuine coordination function: guidelines standardize care, reduce liability exposure, and enable quality measurement. Experiences the constraint as a benign information system. Can abandon or modify guidelines at will; has full exit capacity. Benefits from reduced malpractice claims and insurer pricing. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE PAYORS (ROPE) — Guidelines enable cost control through standardized protocols. Guideline-driven care reduces unnecessary testing and high-cost interventions. Payors have full exit capacity (they can ignore guidelines or refuse reimbursement for guideline deviation). Benefits from protocol standardization. d≈0.10, f(d)≈-0.07, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GUIDELINE AUTHORSHIP (PITON) — Once published, guidelines persist through institutional inertia despite degraded predictive value as patient populations evolve. Theater ratio rises: guideline citations become performative (citing authority rather than validating approach), literature emphasizes guideline 'adherence' metrics rather than actual patient outcomes. Author groups have professional incentive to maintain guideline authority. theater_ratio=0.64 shows moderate degradation. d≈0.25, f(d)≈0.12, σ=1.2 → χ≈0.08.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, guidelines represent a genuine coordination solution (heterogeneous practice → standardized quality measurement) AND an asymmetric extraction mechanism (standardization → suppressed clinical judgment and reduced responsiveness to rare presentations). Both functions are structural. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emrgency_medicine_clinical_guidelines_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emrgency_medicine_clinical_guidelines, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emrgency_medicine_clinical_guidelines, TR),
    TR >= 0.70.

:- end_tests(emrgency_medicine_clinical_guidelines_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The initial extractiveness was low (0.28) when guidelines were true decision-support tools grounded in recent evidence. As time passes, extractiveness increases (0.40 at midpoint, 0.52 at endpoint) because: (1) original evidence bases degrade as patient populations drift, (2) liability exposure pressures guideline adherence beyond evidence warrant, (3) institutional inertia causes guidelines to persist despite outdated evidence. Suppression (0.58): Moderate-high. Significant barriers to guideline deviation include liability exposure, peer pressure, institutional auditing, and career consequences. However, suppression is not total — some physician groups resist guidelines, and institutional modifications occur. Theater ratio (0.64): Moderate-high. Guidelines increasingly function as performative authority rather than genuine decision-support. Guideline 'adherence' becomes a quality metric independent of patient outcomes; citations emphasize guideline authority rather than validating the specific clinical decision. Theater has risen substantially over the interval as guidelines have aged.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates opposing perspectives from trapped victims (physicians and atypical patients) versus beneficiary institutions (compliance, payors). The trapped physicians see an extractive snare — they cannot deviate without liability exposure. The atypical patient sees pure extraction — the guideline imposes a standard protocol regardless of individual presentation. The conscious guideline user sees a tangled rope — genuine decision support coupled with inflexibility. The institutional beneficiaries (compliance, payors) see only rope — a benign coordination mechanism that reduces liability and costs. The analytical observer sees tangled rope — both genuine coordination and extraction are structurally present. The perspectival gap reflects the asymmetry between institutional beneficiaries (who can exit) and physician/patient victims (who cannot).
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency physicians: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Atypical patients: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Conscious guideline users: Victim + constrained → d≈0.58, f(d)≈0.72. Significant extraction. Hospital compliance: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary with negative effective extraction (subsidized by protocol authority). Payors: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.07. Net beneficiary. Guideline authors: Institutional + constrained → d≈0.25, f(d)≈0.12. Low extraction; piton classification derives from theater gate rather than high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Tangled rope from civilizational scope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES AS TANGLED ROPE WITH DEGRADATION TRAJECTORY: The constraint begins as a genuine rope (pure coordination) but accumulates extraction properties over time as institutional inertia and liability exposure intensify. At T=0 (guideline introduction), extractiveness=0.28, theater=0.35, classification is rope — genuinely useful decision-support. By T=10, extractiveness=0.52, theater=0.64, classification is tangled rope — coordination function still present but coupled with extraction. The mandatrophy is resolved by measuring the temporal trajectory: the constraint's extractive component grows as the coordination component degrades. Physicians initially see guidelines as helpful tools; over 10 years, they experience increasing liability pressure and protocol inflexibility, causing the same guidelines to function as constraints. The theater ratio rise (0.35→0.64) indicates that guideline adherence increasingly serves performative functions (defending decisions through protocol authority) rather than validating clinical reasoning. This trajectory is characteristic of institutional aging: a genuine coordination solution accumulates extraction properties through inertia, liability pressure, and metric-gaming (optimizing for 'guideline adherence' rather than patient outcomes). The classification holds at tangled rope across the interval because both functions remain present; the degradation manifests as rising extractiveness and theater, not a phase transition to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guideline_evidence_decay,
    'At what rate does guideline predictive validity degrade as patient populations drift from the original validation cohort?',
    'Longitudinal validation studies comparing guideline performance at 2-year, 5-year, and 10-year intervals post-publication; stratification by demographic/comorbidity drift',
    'If decay is rapid (>20% sensitivity loss per 5 years): guidelines become snares rather than ropes; extractiveness rises above 0.70. If decay is slow: guidelines retain coordination function longer; tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guideline_evidence_decay, empirical, 'Rate of guideline predictive validity decay over time').

omega_variable(
    deviation_liability_correlation,
    'Does guideline deviation actually increase malpractice liability, or is this perception enforced by risk management theater?',
    'Analysis of malpractice claims stratified by guideline adherence vs deviation; comparison of settlement rates and award amounts for guideline-adherent vs guideline-deviating cases',
    'If deviation increases liability: suppression gate (0.58) is justified; physicians face genuine trapped exit. If liability is myth: suppression is lower; exit options improve; classification shifts toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deviation_liability_correlation, empirical, 'Whether guideline deviation increases malpractice liability').

omega_variable(
    atypical_outcome_data,
    'What proportion of improved or standard outcomes occur in patients presenting outside guideline risk-stratification parameters?',
    'Outcomes analysis for guideline-concordant vs guideline-discordant presentations; identification of false-positive and false-negative guideline triggers',
    'If high proportion of atypical presentations have good outcomes: guidelines are overextracted; extractiveness rises above 0.60. If atypical presentations have poorer outcomes: guideline adherence protective; extractiveness drops below 0.45.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atypical_outcome_data, empirical, 'Outcomes distribution for presentations outside guideline parameters').

omega_variable(
    institutional_modification_capacity,
    'Can individual hospitals or physician groups meaningfully modify guidelines in response to local data without external authorization?',
    'Survey of institutional guideline modification processes; case studies of attempted local guideline changes and institutional responses',
    'If modification is easy: exit options improve; moderate agents shift toward mobile/arbitrage; classification becomes more rope-like. If modification is blocked: exit options remain constrained; snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_modification_capacity, empirical, 'Institutional capacity for local guideline modification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emrgency_medicine_clinical_guidelines, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emcg_tr_t0, emrgency_medicine_clinical_guidelines, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emcg_tr_t5, emrgency_medicine_clinical_guidelines, theater_ratio, 5, 0.5).
narrative_ontology:measurement(emcg_tr_t10, emrgency_medicine_clinical_guidelines, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(emcg_be_t0, emrgency_medicine_clinical_guidelines, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(emcg_be_t5, emrgency_medicine_clinical_guidelines, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(emcg_be_t10, emrgency_medicine_clinical_guidelines, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emrgency_medicine_clinical_guidelines, enforcement_mechanism).
narrative_ontology:affects_constraint(emrgency_medicine_clinical_guidelines, defensive_medicine_liability_cascade).
narrative_ontology:affects_constraint(emrgency_medicine_clinical_guidelines, medical_protocol_standardization_trap).

% DUAL FORMULATION NOTE:
% Emergency medicine guidelines decompose into two structurally distinct constraints: (1) the coordination function (decision-support standardization, ε≈0.25, rope), which operates in the evidence-validation domain; (2) the extraction mechanism (liability exposure, institutional enforcement, ε≈0.52, tangled rope), which operates in the institutional/legal domain. The network links these as sequential: guideline introduction (rope) triggers institutional adoption (coordination), which then solidifies into enforced protocol (extraction). The rising theater ratio (0.35→0.64) reflects the shift from genuine decision-support toward performative protocol adherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emrgency_medicine_clinical_guidelines, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
