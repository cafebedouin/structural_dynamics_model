% ============================================================================
% CONSTRAINT STORY: patient_informed_consent_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patient_informed_consent_capacity, []).

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
 *   constraint_id: patient_informed_consent_capacity
 *   human_readable: Patient Informed Consent Capacity Assessment
 *   domain: medical_ethics/healthcare_governance
 *
 * SUMMARY:
 *   Patient informed consent capacity assessment represents a fundamental
 *   tension in medical ethics between two non-negotiable values: respect for
 *   individual autonomy and protection of vulnerable persons. The constraint
 *   exists to resolve this tension by authorizing medical institutions
 *   (primarily treating physicians) to gate patient decision-making authority
 *   when cognitive capacity is questioned. The mechanism exhibits all six DR
 *   types across different observer positions, making it diagnostic of how
 *   indexical classification reveals hidden structures in medical governance.
 *   From the vulnerable patient's perspective, capacity assessment appears as
 *   pure extraction (snare) — a mechanism that disempowers without
 *   transparent standards or appeal rights. From the medical institution's
 *   perspective, it appears as coordination (rope) — a framework that enables
 *   decision-making and provides legal protection. From patient autonomy
 *   advocates, it appears as a temporary institutional failure being
 *   supplanted by supported decision-making frameworks (scaffold). From the
 *   perspective of the capacity assessment ritual itself, it appears as
 *   degraded institutional theater (piton) — formal machinery maintained
 *   through inertia despite recognized inconsistency and bias. The increasing
 *   theater_ratio (0.52 → 0.68 over 30 years) reflects diagnostic drift: as
 *   neuroscience and psychiatry advance, our ability to objectively measure
 *   capacity has improved minimally, while institutional documentation and
 *   risk management requirements have expanded significantly, increasing the
 *   performative content of the assessment.
 *
 * KEY AGENTS:
 *   - Cognitively Vulnerable Patients: Primary victims (powerless/trapped) — dementia, severe mental illness, developmental disability, acute delirium; face unilateral assessment with no transparency or appeal
 *   - Family Advocates: Secondary actors (moderate/constrained) — navigate the system, provide information, but lack decision-making power and bear emotional labor
 *   - Treating Physicians: Primary beneficiaries and enforcers (powerful/mobile) — benefit from capacity framework (liability reduction, decision authority); bear documentation burden and potential conflicts
 *   - Medical Institutions: Institutional beneficiaries (institutional/arbitrage) — use capacity assessment to coordinate liability, decision authority, and institutional continuity
 *   - Patient Autonomy Advocates: Organized reformers (organized/constrained) — bioethics committees, legal frameworks, patient advocacy groups building alternatives (supported decision-making, presumption of capacity)
 *   - Capacity Assessment Ritual: Institutional performer (institutional/arbitrage) — formal machinery persisting through inertia and institutional compliance requirements
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent medical governance as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patient_informed_consent_capacity, 0.58).
domain_priors:suppression_score(patient_informed_consent_capacity, 0.62).
domain_priors:theater_ratio(patient_informed_consent_capacity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patient_informed_consent_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(patient_informed_consent_capacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(patient_informed_consent_capacity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patient_informed_consent_capacity, tangled_rope).
narrative_ontology:human_readable(patient_informed_consent_capacity, "Patient Informed Consent Capacity Assessment").
narrative_ontology:topic_domain(patient_informed_consent_capacity, "medical_ethics/healthcare_governance").

domain_priors:requires_active_enforcement(patient_informed_consent_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patient_informed_consent_capacity, medical_institutions).
narrative_ontology:constraint_beneficiary(patient_informed_consent_capacity, treating_physicians).
narrative_ontology:constraint_victim(patient_informed_consent_capacity, cognitively_vulnerable_patients).
narrative_ontology:constraint_victim(patient_informed_consent_capacity, patient_autonomy_ideal).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COGNITIVELY VULNERABLE PATIENT (SNARE) — Patients with dementia, severe mental illness, developmental disability, or acute delirium face gatekeeping of their own medical decisions. Cannot exit the assessment regime. Physicianically determines capacity through unilateral evaluation. Suppression is structural: cognitive limitations are real, but the assessment itself lacks transparency, reproducibility, or patient appeal. Patient bears full cost of being deemed incapable while benefiting from neither autonomy nor protection.
constraint_indexing:constraint_classification(patient_informed_consent_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FAMILY ADVOCATE (TANGLED ROPE) — Family members benefit from the consent framework (protects patient from harm, provides structure for decision-making) but also experience extraction: their input may be overridden, they bear emotional labor of navigating the system, they may be excluded from information. Constrained exit: cannot refuse participation without abandoning patient to institutional control. Both genuine coordination function (safety) and asymmetric extraction (disempowerment) present.
constraint_indexing:constraint_classification(patient_informed_consent_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL INSTITUTION (ROPE) — Benefits from the consent framework as coordination mechanism: protects against liability, establishes decision-making authority, enables institutional continuity. Experiences the constraint as efficient coordination — capacity assessment is the mechanism by which institutions solve the collective action problem of balancing patient autonomy with institutional responsibility and legal protection. Arbitrage exit: institutions can navigate between different regulatory jurisdictions and standards.
constraint_indexing:constraint_classification(patient_informed_consent_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TREATING PHYSICIAN (TANGLED ROPE) — Physicians benefit from capacity assessment (resolves decision-making authority, reduces legal liability, provides framework for uncertainty) but also bear extraction in the form of documentation burden, cognitive responsibility for contested judgments, and potential conflict with patient/family preferences. Mobile exit: can transfer patient, change specialties, or practice defensively. Moderate experience of both coordination (clarity of authority) and extraction (burden and liability).
constraint_indexing:constraint_classification(patient_informed_consent_capacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PATIENT AUTONOMY ADVOCACY COALITION (SCAFFOLD) — Organized agents (bioethics committees, patient advocacy groups, legal frameworks like the UN Convention on the Rights of Persons with Disabilities) see the capacity assessment as a temporary coordination failure with sunset: shifting toward supported decision-making, advance directives, presumption of capacity, and least-restrictive alternatives. Low effective extraction because this coalition perceives and is building an exit path. Has sunset clause: norms increasingly favor supported decision-making over capacity gatekeeping.
constraint_indexing:constraint_classification(patient_informed_consent_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CAPACITY ASSESSMENT RITUAL (PITON) — The formal machinery of capacity evaluation (structured interviews, documentation, legal thresholds) persists through institutional inertia despite recognized limitations: assessments are inconsistent across clinicians, lack validation, frequently reflect implicit biases, and do not predict actual treatment adherence or outcomes. Theater ratio (0.68) reflects substantial performative content — the ritual demonstrates due diligence and institutional compliance but low functional capacity to actually identify which patients can or cannot meaningfully consent. The assessment ritual is maintained because alternatives (pure patient autonomy, pure institutional paternalism) appear worse, not because the ritual works.
constraint_indexing:constraint_classification(patient_informed_consent_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some gatekeeper must exist to protect those unable to protect themselves — this is a logical necessity of any moral system that acknowledges both autonomy and vulnerability. This perspective sees the constraint as an immutable feature: any system that values both individual choice and harm prevention must have some agent authorized to override consent when capacity is absent. However, the structural data (suppression 0.62, extractiveness 0.58, theater 0.68) contradicts pure mountain — the engine will detect this as a false summit, revealing that the 'logical necessity' framing naturalizes what is actually a contingent institutional arrangement (discretionary physician assessment without transparency or appeal).
constraint_indexing:constraint_classification(patient_informed_consent_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patient_informed_consent_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patient_informed_consent_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patient_informed_consent_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patient_informed_consent_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patient_informed_consent_capacity, TR),
    TR >= 0.70.

:- end_tests(patient_informed_consent_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts autonomy from vulnerable patients to provide institutional clarity and liability protection. This is not maximal extraction because some genuine coordination function exists (protection of incapable patients from serious harm), and because capacity assessment at least nominally aims to respect autonomy where it exists. The increasing trajectory (0.35 → 0.58) reflects rising institutional use of capacity determinations to manage liability rather than protect patients. Suppression (0.62): Moderate-high. Barriers to exit include cognitive limitations (real), but also systemic barriers: lack of transparency in assessment criteria, absent or weak appeal mechanisms, implicit bias in clinician judgment, and institutional control of information flow. Suppression is structural (real barriers) but partially psychological (internalized helplessness and medical authority). Theater_ratio (0.68): High. Capacity assessment involves substantial performative content: structured interviews and documentation that satisfy institutional compliance requirements but have questionable validity in predicting actual decision-making capacity or outcomes. The theater has increased as institutional risk management demands more documentation without corresponding improvement in assessment quality. This is classic piton dynamics: maintaining ritual without functional efficacy.
 *
 * PERSPECTIVAL GAP:
 *   Why does capacity assessment appear as snare to patients but rope to physicians? Because directionality is different. Patients have d ≈ 0.95 (trapped targets); physicians have d ≈ 0.20 (beneficiaries with mobile exit). The sigmoid function f(d) produces dramatically different χ values for the same base ε. A patient with d=0.95 experiences χ ≈ ε × 1.42 (powerless scaling), while a physician with d=0.20 experiences χ ≈ ε × 0.02 (beneficiary scaling). The gap is not about disagreement on the facts; it is about different structural positions producing different experienced extractiveness from the same mechanism. Why does the analytical observer risk seeing a mountain (natural law) when the structural data shows tangled rope? Because the mountain argument ('any system must protect the incapable') is appealing but false — it naturalizes a specific institutional arrangement (unilateral physician assessment) as if it were a logical necessity. The logical necessity is much weaker: some mechanism for protecting incapable patients may be necessary, but the current mechanism (discretionary physician judgment without transparency or appeal) is contingent. Alternative mechanisms (supported decision-making, legal guardianship with due process, family-centered decision-making, patient advocacy) might serve the same protective function with less extraction. The false summit occurs when the mountain perspective conflates 'protection of vulnerable people is necessary' with 'this specific institutional arrangement is necessary.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically by agent. Vulnerable patients (powerless/trapped) have d ≈ 0.95 — maximum target status because they have no exit options and bear full cost of assessment. Treating physicians (powerful/mobile) have d ≈ 0.20 — predominantly beneficiary status because they gain decision authority and liability protection while retaining exit options (can transfer patient, practice defensively). Medical institutions (institutional/arbitrage) have d ≈ 0.05 — full beneficiary because the capacity framework serves institutional interests and institutions have multiple regulatory jurisdictions to navigate. Family advocates (moderate/constrained) have d ≈ 0.62 — split between target and beneficiary because they both benefit (patient protection) and lose (decision power, information access). Patient advocacy organizations (organized/constrained) have d ≈ 0.40 — moderate target status because they are fighting an entrenched institutional mechanism but have some agency and perceive an exit path (supported decision-making). These directionality variations explain why the constraint classifies as snare (powerless view), rope (institutional view), and tangled rope (moderate/organized views) — the derivation chain correctly captures that different agents experience different effective extraction values from the same structural mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: The constraint resolves the mandatrophy by demonstrating that 'informed consent capacity' is not a univocal concept but a bundle of overlapping institutional mechanisms serving different functions for different actors. For the vulnerable patient, capacity assessment is a mechanism of disempowerment (snare). For the institution, it is a mechanism of liability management and decision authority (rope). For the treatment team, it is a mechanism of clinical certainty (tangled rope — both genuine safety coordination and burden/liability extraction). For reformers, it is a temporary problem being solved by supported decision-making (scaffold). For the assessment ritual itself, it is a performative institutional practice maintained by inertia (piton). For the civilizational observer, it appears as natural law but is actually a false summit — the specific institutional form is contingent. The mandatrophy is resolved not by choosing one type, but by recognizing that all six types are legitimate perspectival observations of different functional and dysfunctional aspects of the same constraint. The diagnostic value is in the perspectival gaps: where the institution sees coordination, the patient sees extraction. Where reformers see a solvable temporary problem, institutional maintenance mechanisms see a permanent necessity. These gaps reveal the hidden structure that a single-perspective analysis (e.g., medical ethics focused only on physician and institution perspectives) would miss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_definition_instability,
    'Is diminished consent capacity a natural fact or a socially constructed category?',
    'Cross-cultural comparison of capacity standards; analysis of how diagnostic thresholds shift with medical context and legal jurisdiction; identification of whether capacity assessments predict actual decision-making quality or merely satisfy institutional requirements',
    'If natural fact: mountain classification is correct, and current assessment methods are merely imperfect tools for detecting a real phenomenon. If socially constructed: the constraint is extraction disguised as protection, and alternative frameworks (supported decision-making) are equally valid and less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_definition_instability, conceptual, 'Whether consent capacity is a natural fact or socially constructed category').

omega_variable(
    implicit_bias_in_assessment,
    'To what degree do capacity assessments reflect clinician implicit bias rather than patient cognitive capacity?',
    'Audit studies: same patient vignettes presented to multiple clinicians; comparison of capacity ratings by clinician demographic/specialty; analysis of whether capacity determinations correlate with protected class status (race, age, disability, socioeconomic status, mental health diagnosis)',
    'If bias is substantial: suppression value should be higher (assessment weaponizes bias against vulnerable populations). If bias is minimal: suppression value is accurate. Either way, this determines whether the constraint is primarily a protection mechanism or a discrimination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_bias_in_assessment, empirical, 'Extent of implicit bias in clinical capacity assessments').

omega_variable(
    supported_decision_making_efficacy,
    'Do supported decision-making frameworks (patient + advocate collaborating on decisions without capacity gatekeeping) produce better outcomes than standard capacity assessment + physician override?',
    'RCT comparing traditional capacity assessment to supported decision-making; measurement of decision alignment with patient values, long-term satisfaction, treatment adherence, adverse outcomes, and quality of life',
    'If supported decision-making is efficacious: the scaffold sunset is real and accelerating — the constraint''s extraction mechanism becomes optional. If not efficacious: capacity gatekeeping remains necessary, and the tangled rope classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supported_decision_making_efficacy, empirical, 'Whether supported decision-making produces better outcomes than traditional capacity gatekeeping').

omega_variable(
    paternalism_extraction_boundary,
    'Where is the boundary between justified paternalism (protecting incapable patients from serious harm) and extractive paternalism (using incapacity determination to serve institutional convenience)?',
    'Analysis of decision override rates by diagnosis, treatment type, and institutional context; comparison of override rates to objective measures of decision consequences; identification of patterns in which patient preferences get overridden vs. respected',
    'If boundary is clear: capacity assessment can be reformed (better training, transparency, appeal) without fundamentally changing the constraint type. If boundary is ambiguous or absent: the entire framework is extractive, and the constraint should reclassify as snare rather than tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paternalism_extraction_boundary, preference, 'Boundary between justified paternalism and extractive paternalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patient_informed_consent_capacity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pic_tr_t0, patient_informed_consent_capacity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pic_tr_t15, patient_informed_consent_capacity, theater_ratio, 15, 0.6).
narrative_ontology:measurement(pic_tr_t30, patient_informed_consent_capacity, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(pic_be_t0, patient_informed_consent_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pic_be_t15, patient_informed_consent_capacity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(pic_be_t30, patient_informed_consent_capacity, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patient_informed_consent_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(patient_informed_consent_capacity, guardianship_and_substitute_decision_making).
narrative_ontology:affects_constraint(patient_informed_consent_capacity, advance_directive_enforcement).
narrative_ontology:affects_constraint(patient_informed_consent_capacity, medical_paternalism_boundaries).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(patient_informed_consent_capacity, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
