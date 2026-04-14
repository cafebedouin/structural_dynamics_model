% ============================================================================
% CONSTRAINT STORY: patient_medication_adherence_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patient_medication_adherence_paradox, []).

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
 *   constraint_id: patient_medication_adherence_paradox
 *   human_readable: Patient Medication Adherence Paradox
 *   domain: healthcare/behavioral_economics
 *
 * SUMMARY:
 *   The medication adherence paradox emerges from a structural tension in
 *   modern healthcare: medications are scientifically sophisticated and
 *   individually necessary, yet systems for prescribing and administering
 *   them are designed with minimal patient input and maximal
 *   cognitive/logistical burden. Non-adherence is conventionally framed as a
 *   patient behavior problem—willpower, memory, motivation—but this framing
 *   obscures a deeper institutional architecture. Patients are expected to
 *   manage complex regimens (multiple medications, different dosing
 *   schedules, side effects, drug interactions, cost barriers) without the
 *   systems, information, or authority to make changes. Non-adherence becomes
 *   medicalized—treated as patient failure rather than system failure. This
 *   constraint exhibits genuine coordination function (medications do treat
 *   diseases) overlaid with significant extraction (complexity offloaded to
 *   patients, burden blamed on patient behavior, data mined for profit,
 *   alternatives suppressed). The theater ratio rises over time as adherence
 *   monitoring becomes increasingly sophisticated (digital pill bottles,
 *   smartphone reminders, data dashboards) while the actual patient
 *   experience—navigating real barriers—receives minimal redesign. The
 *   constraint classifies as Tangled Rope from institutional perspectives
 *   (genuine disease-treatment coordination with extraction overlay) and as
 *   Snare from the non-adherent patient's perspective (trapped without exit,
 *   maximum suppression, blamed for system failures).
 *
 * KEY AGENTS:
 *   - Non-Adherent Patient: Primary victim (powerless/trapped) — faces cognitive burden, side effects, cost, appointment friction; non-adherence is medicalized as personal failure while system barriers remain invisible
 *   - Patient Population (Organized): Secondary victim (moderate/constrained) — when organized, reveal coordination function and capture; constrained exit between forgoing treatment and accepting patient-unfriendly systems
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — profits from disease persistence, medication non-adherence (repeat prescriptions for failed regimens), and data mining; genuine drug development function overlaid with extraction
 *   - Healthcare System Administrator: Secondary beneficiary (institutional/arbitrage) — sees adherence as resource coordination; arbitrage options to shift models; incentives often misaligned with patient health
 *   - Prescribing Physician: Institutional actor (institutional/constrained) — caught in degraded practice; legally obligated to prescribe but squeezed by time, prior authorization, formulary restrictions; adherence counseling becomes theater
 *   - Patient-Centered Design Movement: Organized agents (organized/mobile) — patient advocates, design researchers, some health systems building alternative pathways with genuine sunset logic via simplified regimens and accessible systems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (medication complexity, patient burden) as inherent human behavioral limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patient_medication_adherence_paradox, 0.58).
domain_priors:suppression_score(patient_medication_adherence_paradox, 0.72).
domain_priors:theater_ratio(patient_medication_adherence_paradox, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patient_medication_adherence_paradox, extractiveness, 0.58).
narrative_ontology:constraint_metric(patient_medication_adherence_paradox, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(patient_medication_adherence_paradox, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patient_medication_adherence_paradox, tangled_rope).
narrative_ontology:human_readable(patient_medication_adherence_paradox, "Patient Medication Adherence Paradox").
narrative_ontology:topic_domain(patient_medication_adherence_paradox, "healthcare/behavioral_economics").

domain_priors:requires_active_enforcement(patient_medication_adherence_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patient_medication_adherence_paradox, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(patient_medication_adherence_paradox, healthcare_systems).
narrative_ontology:constraint_beneficiary(patient_medication_adherence_paradox, prescribing_physicians).
narrative_ontology:constraint_victim(patient_medication_adherence_paradox, patient_health_outcomes).
narrative_ontology:constraint_victim(patient_medication_adherence_paradox, patient_autonomy).
narrative_ontology:constraint_victim(patient_medication_adherence_paradox, medication_efficacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ADHERENT PATIENT (SNARE) — Trapped between medical necessity and lived reality. Lacks structural exit: stopping medication risks acute health crisis; continuing faces cognitive burden, side effects, cost barriers, and complex regimens that no voluntary system can sustain. Maximum suppression: pharmacy availability, insurance gatekeeping, appointment scheduling, literacy barriers, competing life demands. No voice in medication design. Bears full extraction: their non-adherence is medicalized as personal failure while systemic barriers remain invisible.
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PATIENT POPULATION—ORGANIZED (TANGLED ROPE) — When patients organize (patient advocacy groups, disability communities), they reveal a genuine coordination function: medication adherence research actually does improve care when design is user-centered. But the coordination is captured: pharmaceutical marketing, clinician authority norms, and health system incentives privilege corporate interests over patient-centered design. Constrained exit: leaving the medication system means forgoing beneficial treatments, but staying means accepting systems designed without patient input. Mixed benefits and costs.
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (TANGLED ROPE) — Net beneficiary. Genuine coordination function: drug development does solve disease problems. But overlaid with extraction: marketing directly to patients and doctors creates demand independent of medical need; adherence monitoring creates data asset; non-adherence is blamed on patient behavior, obscuring drug design flaws. Arbitrage exit: manufacturers can shift markets, change formulations, exit unprofitable indications. The constraint coordinates (solves disease problems) while extracting (maximizes revenue, offloads burden to patients).
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE SYSTEM ADMINISTRATOR (ROPE) — Experiences the constraint primarily as coordination: managing limited resources, matching patients to medications, tracking outcomes. Adherence monitoring is framed as quality improvement. Theater is present (adherence dashboards emphasizing compliance metrics over health outcomes) but the coordinator sees genuine utility in the system. Arbitrage options: can shift reimbursement models, change formularies, implement different adherence support strategies. Low extraction from this perspective because the administrator's incentives partially align with patient health (though payment models often misalign).
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRESCRIBING PHYSICIAN (PITON) — Caught in degraded institutional practice. Physicians ostensibly coordinate care (genuine function) but the real work—understanding patient barriers, designing user-centered regimens, supporting behavior change—is increasingly offloaded to patients themselves or reduced to pharmaceutical marketing messages. Adherence is monitored but solutions are pharmaceutical (new formulations, dosing simplification) rather than structural (appointment scheduling, cost support, complexity reduction). Constrained exit: legally and ethically obligated to prescribe but increasingly squeezed by time constraints, prior authorization, formulary restrictions. The practice has become performative: adherence counseling in a 15-minute visit becomes theater rather than coordination.
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PATIENT-CENTERED DESIGN MOVEMENT (SCAFFOLD) — Organized actors (patient advocates, design researchers, some healthcare systems) see adherence paradox as a temporary institutional failure being solved through user-centered medication design, simplified regimens, accessible systems, and shared decision-making. The movement has genuine agency: e-health systems, medication synchronization, community health worker support. These are building alternative pathways. Sunset clause: as digital health, lower-cost generic regimens, and patient-centered norms mature, the extraction mechanism (complexity offloaded to patients) loses force. Mobile exit: organized patients can demand and sometimes achieve redesigned systems.
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal view, medication adherence is an immutable property of human behavior: cognitive load, behavioral inertia, and the gap between intention and action are inherent features of how humans navigate complex medical regimens. This perspective naturalizes what is actually a contingent institutional arrangement—the choice to design medications without user input, to offload complexity to patients, to measure adherence as patient failure rather than system failure. The engine's false summit detector will flag this: the 'inherent to human nature' framing masks choices made by institutional actors.
constraint_indexing:constraint_classification(patient_medication_adherence_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patient_medication_adherence_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patient_medication_adherence_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patient_medication_adherence_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patient_medication_adherence_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patient_medication_adherence_paradox, TR),
    TR >= 0.70.

:- end_tests(patient_medication_adherence_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from patients (behavioral burden, data, repeated prescriptions for failed regimens) and delivers it to pharmaceutical companies and healthcare systems. But extractiveness is not maximal because genuine disease-treatment coordination exists—medications do provide health benefits. The extraction is layered onto real coordination, not pure rent-seeking. Suppression (0.72): High. Patients face multiple reinforcing barriers: cognitive load (regimen complexity), economic barriers (cost, insurance gatekeeping), temporal barriers (appointment scheduling, work-life conflict), informational barriers (literacy, language, side effect understanding), social barriers (stigma, family pressure). Suppression is structural and intentional: the system was designed without patient input, and simplifying it would reduce extractive margin. Theater ratio (0.68): High and rising. Adherence monitoring technology (digital pill bottles, smartphone apps, data dashboards) creates appearance of active management while the actual design of medications and regimens remains unchanged. Adherence counseling in 15-minute clinical visits is theater—insufficient time for genuine behavior change support. Marketing messages emphasizing 'just remember to take it' suggest the problem is patient will rather than system design. The theater has increased over the measurement interval as digital monitoring expanded while patient-centered redesign stalled.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between beneficiaries and victims. Pharmaceutical companies experience low extraction (they are net extractors), healthcare systems experience moderate extraction, and non-adherent patients experience maximum extraction. The organized patient perspective reveals how coordination function is captured—patient advocacy reveals genuine benefits of coordinated care but also how those benefits are constrained by pharmaceutical and system design choices. The piton perspective (prescribing physician) is diagnostic: the same activity (adherence counseling) that appears to be coordination from the healthcare administrator's view appears degraded/performative from the physician's view, suggesting the coordination function has atrophied while the ritual persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations are fundamental to directionality. Pharmaceutical manufacturers and healthcare systems are declared beneficiaries because the constraint creates revenue (repeated prescriptions, data assets, managed care efficiency gains). Patient health outcomes, patient autonomy, and medication efficacy are declared victims because the constraint creates burden (cognitive load, side effects, costs) without corresponding system redesign, and because non-adherence is blamed on patients rather than on system design flaws. The organized patient perspective shows how declaring organized patients as both victims AND as agents with mobile exit options changes the classification: they can see and name the capture mechanism, which Tangled Rope classification reflects. The designer movement is a beneficiary of the design-centered frame (they gain professional legitimacy and funding) but their beneficiary status is misaligned with pharmaceutical and health system beneficiary status—the designers are extracting legitimacy and attention, not economic value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification changes structurally based on agent power and exit options, not based on measurement ambiguity. The powerless, trapped patient sees Snare (pure extraction). The organized, mobile patient community sees Tangled Rope (mixed coordination and extraction, but with some agency to demand redesign). The institutional beneficiaries see Rope (coordination with minimal perceived extraction because they are the extractors). The physician sees Piton (coordination function degraded, ritual persists). The designer movement sees Scaffold (temporary problem with a sunset as design practices improve). The analytical observer risks seeing Mountain (immutable human behavioral limits) but false summit detection flags this as naturalization of contingent institutional choices. The mandatrophy is resolved by recognizing that all six classifications are valid from different structural positions—the constraint is not 'which type?' but 'which perspective reveals what institutional design choices are being obscured?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adherence_measurement_bias,
    'Is measured non-adherence a property of patient behavior or a measurement artifact of systems that were not designed to support adherence?',
    'Comparative adherence data: between simplified vs complex regimens, between patient-designed vs physician-designed regimens, between systems with vs without adherence support infrastructure. If adherence rates converge when complexity and barriers are removed, the phenomenon is system design, not patient failure.',
    'If measurement artifact: non-adherence is a snare mechanism (system-designed burden blamed on patient). If genuine behavioral property: partial rope (patients genuinely struggle with all complex regimens). Current measurement conflates both, masking the institutional choice to assign complexity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adherence_measurement_bias, empirical, 'Whether non-adherence is behavioral or system-design artifact').

omega_variable(
    pharmaceutical_design_incentive_alignment,
    'Are pharmaceutical companies structurally incentivized to design medications for adherence, or are adherence barriers economically rational from a revenue perspective?',
    'Cost-benefit analysis of simplified regimens: does market expansion from easier adherence outweigh revenue from disease persistence and repeat non-compliance? Patent cliff analysis: do companies extend patents via formulation changes (complexity increase) or simplification (adherence increase)?',
    'If profit-maximizing: adherence barriers are intentional features (Snare/Tangled Rope). If margin-neutral or negative: companies face genuine prisoner''s dilemma (Rope with extraction overlay). Current evidence suggests simplified regimens reduce company revenue—extracted margin from non-adherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_design_incentive_alignment, empirical, 'Alignment of pharmaceutical incentives with patient adherence').

omega_variable(
    patient_centered_design_scalability,
    'Can patient-centered medication design scale to standard-of-care, or does it remain boutique intervention dependent on privileged patients and well-resourced systems?',
    'Longitudinal implementation data from primary care systems: percentage of patients receiving simplified regimens, adherence outcomes at scale, cost structures, sustainability after external funding ends.',
    'If scalable: scaffold perspective is structurally real (sunset is achievable). If boutique: scaffold is aspirational; patient-centered care remains constrained exit option for organized patients only. Most evidence suggests boutique status—limiting sunset logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_centered_design_scalability, empirical, 'Scalability of patient-centered medication design').

omega_variable(
    identity_locked_patient_compliance,
    'Is patient non-adherence a structural barrier (trapped/constrained) or does the patient internalize medication identity—''I am the kind of person who forgets'' or ''I am non-compliant''—such that the binding mechanism is identity-fusion rather than material barrier?',
    'Qualitative patient interviews and identity trajectory analysis: do patients report internalized compliance identity? Does identity-reframing intervention (without removing material barriers) change adherence? Do patients maintain non-adherence behaviors after material barriers are removed?',
    'If identity-locked: exit requires patient to reconstruct identity, not just remove barriers (internal suppression persists after barrier removal). If constrained: removal of barriers enables adherence. Clinical implications differ: identity-locked requires psychological intervention plus structural change; constrained requires structural change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_patient_compliance, empirical, 'Whether patient non-adherence involves identity-lock binding mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patient_medication_adherence_paradox, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmap_tr_t0, patient_medication_adherence_paradox, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pmap_tr_t10, patient_medication_adherence_paradox, theater_ratio, 10, 0.65).
narrative_ontology:measurement(pmap_tr_t20, patient_medication_adherence_paradox, theater_ratio, 20, 0.68).
narrative_ontology:measurement(pmap_tr_t30, patient_medication_adherence_paradox, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(pmap_be_t0, patient_medication_adherence_paradox, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pmap_be_t10, patient_medication_adherence_paradox, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pmap_be_t20, patient_medication_adherence_paradox, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pmap_be_t30, patient_medication_adherence_paradox, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patient_medication_adherence_paradox, resource_allocation).
narrative_ontology:affects_constraint(patient_medication_adherence_paradox, pharmaceutical_marketing_capture).
narrative_ontology:affects_constraint(patient_medication_adherence_paradox, health_literacy_barrier).
narrative_ontology:affects_constraint(patient_medication_adherence_paradox, medication_side_effect_suppression).

% DUAL FORMULATION NOTE:
% Medication adherence is conventionally treated as a single constraint (patient behavior problem). Structural decomposition reveals multiple overlapping constraints: (1) medication design complexity (interaction with patient cognition/capacity), (2) pharmaceutical profit incentive alignment (revenue from disease persistence vs simplified regimens), (3) healthcare system incentive misalignment (reimbursement models that don't reward adherence support), (4) physician capacity constraint (time for genuine behavior change counseling). Each has different ε. The adherence paradox story focuses on the aggregate institutional arrangement; decomposed stories would address each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(patient_medication_adherence_paradox, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
