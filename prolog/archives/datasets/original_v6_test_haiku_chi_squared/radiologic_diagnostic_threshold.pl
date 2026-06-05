% ============================================================================
% CONSTRAINT STORY: radiologic_diagnostic_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_radiologic_diagnostic_threshold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: radiologic_diagnostic_threshold
 *   human_readable: The Radiologic Diagnostic Threshold
 *   domain: medical/technological/legal
 *
 * SUMMARY:
 *   The radiologic diagnostic threshold is the invisible boundary that
 *   separates reported findings from unreported ones, actionable diagnoses
 *   from incidental observations, and clinical facts from noise. This
 *   threshold is not a law of nature—it is a sociotechnical construction
 *   shaped by radiologists' liability exposure, healthcare system resource
 *   constraints, and patients' structural powerlessness. The same imaging
 *   result (e.g., a 5mm nodule) may be classified as 'benign incidental' by a
 *   high-threshold reader, 'probably benign recommend follow-up' by a
 *   moderate-threshold reader, or 'suspicious for malignancy urgent
 *   follow-up' by a low-threshold reader. These are not differences in the
 *   image; they are differences in the threshold logic applied to it. The
 *   constraint exhibits a classic tangled rope structure: it provides a
 *   genuine coordination function (triaging the flood of incidental findings
 *   that modern imaging generates) while simultaneously extracting from
 *   patients and primary care physicians through liability-driven
 *   over-calling, suppressed communication of uncertainty, and cascading
 *   unnecessary procedures. Over the 40-year interval (1984–2024), the
 *   theater ratio has increased from 0.42 to 0.68 as imaging has become more
 *   sensitive, as liability fears have grown, and as defensive medicine has
 *   become standard practice. Extractiveness has increased from 0.28 to 0.52
 *   as the gap between technical capability and clinical action has widened.
 *
 * KEY AGENTS:
 *   - Patients: Primary victim (powerless/trapped) — bear costs of both missed diagnoses and cascade harm from over-calling; lack autonomy in threshold setting
 *   - Primary Care Physicians: Secondary victim (moderate/constrained) — constrained by radiologist authority and liability exposure; lack power to adjust thresholds or challenge classifications
 *   - Radiologists: Primary beneficiary and coordinator (organized/constrained) — benefit from threshold gatekeeping authority and liability protection; coordinate patient triage; also constrained by defensive medicine pressure
 *   - Healthcare Systems: Secondary beneficiary (institutional/arbitrage) — benefit from radiologist gatekeeping to manage resource flow and liability exposure; calibrate thresholds to reduce system risk
 *   - Imaging Manufacturers: Peripheral beneficiary (institutional/arbitrage) — benefit from threshold complexity that justifies algorithm complexity and upgrade cycles
 *   - Legal/Regulatory Framework: Institutional enforcer (institutional/arbitrage) — maintains threshold through 'standard of care' doctrine; drives defensive calibration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the ROC curve tradeoff as immutable, obscuring the contingent choice of where to position the threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(radiologic_diagnostic_threshold, 0.52).
domain_priors:suppression_score(radiologic_diagnostic_threshold, 0.65).
domain_priors:theater_ratio(radiologic_diagnostic_threshold, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, extractiveness, 0.52).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(radiologic_diagnostic_threshold, tangled_rope).
narrative_ontology:human_readable(radiologic_diagnostic_threshold, "The Radiologic Diagnostic Threshold").
narrative_ontology:topic_domain(radiologic_diagnostic_threshold, "medical/technological/legal").

domain_priors:requires_active_enforcement(radiologic_diagnostic_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, radiologists).
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, healthcare_systems).
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, imaging_manufacturers).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, patients).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, primary_care_physicians).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, diagnostic_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (SNARE) — Cannot exit the diagnostic system; bears full cost of threshold-driven misclassification. A finding classified as 'benign' is not communicated; reclassified as 'critical' leads to cascading interventions. No autonomy over the threshold setting. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIMARY CARE PHYSICIAN (SNARE) — Constrained by radiologist's threshold authority and liability exposure. Must act on radiologist's classification without power to challenge or adjust thresholds. Victim of both under- and over-calling (missed findings vs cascade of false positives). d≈0.80, f(d)≈1.20, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RADIOLOGIST (TANGLED ROPE) — Benefits from threshold authority (gating function for medical system), coordination of referral streams, and liability protection. Also constrained by defensive medicine pressure, algorithm-mediated recommendations, and professional obligation to err on side of over-calling (suppresses understaffing, technology limits). d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTHCARE SYSTEM (TANGLED ROPE) — Benefits from radiologist threshold gatekeeping (controls patient flow, manages resource allocation, defensive liability posture). Experiences tension between cost containment and risk mitigation. Coordination function: radiology department is essential to triage system. Extraction: thresholds are calibrated to reduce system liability, not optimize patient outcomes. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IMAGING MANUFACTURER (ROPE) — Coordinates workflow between imaging modality and diagnostic software. Thresholds embedded in algorithm parameterization create dependency lock-in and drive upgrade cycles. Benefits from threshold complexity (proprietary algorithms justify cost). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGAL/REGULATORY FRAMEWORK (PITON) — Maintains radiologic threshold as a defense mechanism against malpractice liability. The 'standard of care' is largely performative: thresholds are calibrated to documented defensibility rather than optimal sensitivity/specificity. theater_ratio=0.68 indicates high performative content (defensive documentation > clinical utility). Persists through institutional inertia and risk aversion.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SIGNAL DETECTION (MOUNTAIN) — From a universal perspective, any diagnostic threshold reflects inherent tradeoff between sensitivity and specificity; there is no threshold-free diagnosis. ROC curves are immutable mathematical objects. But structural data (ε=0.52, suppression=0.65) contradicts mountain classification — this is a false summit. The 'inherent tradeoff' is real but orthogonal to the constraint: the CHOICE of where to position the threshold is contingent, not immutable.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(radiologic_diagnostic_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(radiologic_diagnostic_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(radiologic_diagnostic_threshold, TR),
    TR >= 0.70.

:- end_tests(radiologic_diagnostic_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The threshold extracts from patients and PCP by suppressing communication of uncertainty, driving cascade procedures, and concentrating diagnostic authority in radiologists. But it also provides genuine coordination (triaging incidental findings). The value reflects that extraction is real but mixed with coordination. Suppression (0.65): Significant barriers include liability law (malpractice exposure for under-calling), institutional risk aversion, professional norms favoring sensitivity over specificity, and patient information asymmetry. Radiologists cannot freely optimize for accuracy; they must optimize for defensibility. Theater ratio (0.68): High and increasing. Defensive documentation (the detailed report justifying the classification) has become decoupled from clinical utility. Many findings are reported with 'recommend follow-up' even when the radiologist's genuine belief is low-probability benign — the report is performative liability protection, not clinical communication. The rise from 0.42 to 0.68 reflects increasing defensive verbosity without proportional increase in actual diagnostic utility.
 *
 * PERSPECTIVAL GAP:
 *   The patient and PCP see a Snare: they are trapped by a threshold logic that was not their choice and that suppresses critical information. The radiologist sees a Tangled Rope: the threshold provides real coordination (gating function) but also constrains their professional judgment (defensive pressure to over-call). The healthcare system sees a Tangled Rope: the threshold coordinates resource flow but also drives unnecessary procedures. The manufacturer sees a Rope: threshold complexity creates demand for algorithmic solutions. The legal framework sees itself as maintaining necessary standards but is actually enforcing a Piton — a degraded version of evidence-based medicine maintained through liability inertia. The analytical observer risks seeing an immutable Mountain (ROC curve mathematics) that actually conceals a contingent institutional choice (where on the ROC curve to position the operating point). The perspectival gap reveals that the constraint is not 'what is the truth in this image?' but 'who decides what threshold to use, and what incentives drive that decision?'
 *
 * DIRECTIONALITY LOGIC:
 *   Patients: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction from the most powerless agent. Primary care physicians: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction; they have some agency (can order follow-up) but are constrained by radiologist authority and liability exposure. Radiologists: Beneficiary + constrained (not arbitrage) → d≈0.45, f(d)≈0.50. They benefit from threshold authority but are constrained by defensive medicine pressure; the entry point to the constraint is coordinating the diagnostic stream, not pure extraction. Healthcare system: Beneficiary + arbitrage → d≈0.35, f(d)≈0.35. Net beneficiary with capacity to exit (can relocate radiology operations, select imaging vendors), but locked in by regulatory requirements and operational dependencies. Imaging manufacturer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Peripheral to the constraint; primarily benefits from threshold complexity without direct exposure to patient harm. Legal framework: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Enforcer with arbitrage ability; doesn't directly bear costs of its own standards.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY TENSION: The constraint appears to be both a legitimate coordination mechanism (triaging incidental findings, routing patients appropriately) and an extraction mechanism (liability-driven over-calling, suppressed communication, cascade harm). The mandatrophy is resolved by recognizing that the constraint's classification depends on the measurement baseline: if the baseline is 'what is the medically optimal threshold?', the constraint is a Snare (extractive liability protection). If the baseline is 'how should incidental findings be routed in a resource-constrained system?', the constraint is a Rope (coordination mechanism). The Tangled Rope classification reflects that both baselines are simultaneously true: the constraint provides real coordination while simultaneously extracting through liability-driven suppression. The path to resolve: empirical measurement of whether thresholds converge toward evidence-based optimality or toward defensive consensus (omega variable 4). If thresholds are driven primarily by liability fears rather than clinical evidence, the extraction component dominates and the constraint should reclassify as Snare (ε→0.65, suppression→0.75). If thresholds converge toward clinical optimality, the coordination component is primary and the constraint should reclassify as Rope (ε→0.25, theater→0.35). Current evidence (prevalence of defensive medicine, ACR guideline drift toward sensitivity) suggests the extraction-driven reading is more accurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_vs_patient_harm,
    'Does radiologic threshold calibration minimize total harm (malpractice + missed diagnosis + cascade harm) or minimize legal liability (biased toward over-calling)?',
    'Comparison of threshold recommendations by expert consensus (e.g., ACR) vs liability insurance guidelines vs empirical harm models; outcome data correlating threshold setting with adverse events, unnecessary procedures, and missed diagnoses',
    'If liability-driven: threshold is pure extraction mechanism (Snare). If harm-optimized: threshold is coordination mechanism with legitimate tradeoff (Rope). Current evidence suggests liability-driven > harm-optimized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_vs_patient_harm, empirical, 'Whether thresholds optimize for total harm or legal liability').

omega_variable(
    algorithm_vs_radiologist_authority,
    'Does AI-assisted diagnosis (algorithm recommendations + radiologist override) represent genuine threshold decomposition or performative compliance theater?',
    'Audit of radiologist overrides: frequency, direction (tightening vs loosening), clinical outcomes. Analysis of whether algorithm recommendations change radiologist behavior or merely provide legal cover for pre-existing thresholds.',
    'If genuine decomposition: algorithm constraint and radiologist constraint are separate stories (two omegas, two χ values). If theater: algorithm is piton (maintains appearance of objectivity without functional change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_vs_radiologist_authority, empirical, 'Whether AI-assisted diagnosis provides genuine threshold decomposition').

omega_variable(
    communication_asymmetry_necessity,
    'Is the non-communication of ''benign'' findings (findings below threshold not reported to patient or PCP) a necessary feature of diagnostic efficiency or an extraction mechanism concealing uncertainty?',
    'Comparative study: systems with vs without patient-direct communication of benign findings; patient knowledge surveys (what fraction of benign findings on their images do patients learn about?); medical-legal analysis of negligence standards',
    'If efficiency-necessary: threshold acts as information gate for legitimate triage (Rope component). If extraction: threshold suppresses patient autonomy (Snare component intensified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communication_asymmetry_necessity, conceptual, 'Whether non-communication of benign findings is operationally necessary').

omega_variable(
    threshold_convergence_across_systems,
    'Do radiologic thresholds converge toward objectively justified positions (e.g., maximizing diagnostic accuracy) or toward defensive consensus (e.g., ACR guidelines shaped by liability fears)?',
    'Historical analysis of threshold evolution: compare threshold changes following clinical evidence vs following major liability case law. Analysis of ACR guideline development process (expert consensus vs liability input).',
    'If evidence-driven convergence: threshold is a legitimate coordination mechanism despite indexical appearance (Rope). If defensive convergence: threshold is an extraction mechanism coordinated through liability pressure (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_convergence_across_systems, empirical, 'Whether thresholds converge toward clinical evidence or liability defense').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(radiologic_diagnostic_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(radi_tr_t0, radiologic_diagnostic_threshold, theater_ratio, 0, 0.42).
narrative_ontology:measurement(radi_tr_t20, radiologic_diagnostic_threshold, theater_ratio, 20, 0.58).
narrative_ontology:measurement(radi_tr_t40, radiologic_diagnostic_threshold, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(radi_be_t0, radiologic_diagnostic_threshold, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(radi_be_t20, radiologic_diagnostic_threshold, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(radi_be_t40, radiologic_diagnostic_threshold, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(radiologic_diagnostic_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(radiologic_diagnostic_threshold, incidentaloma_cascade).
narrative_ontology:affects_constraint(radiologic_diagnostic_threshold, malpractice_liability_threshold).
narrative_ontology:affects_constraint(radiologic_diagnostic_threshold, patient_information_asymmetry_medical).

% DUAL FORMULATION NOTE:
% The radiologic diagnostic threshold decomposes into two structurally distinct constraints: (1) Signal-detection mathematics (ROC curve positioning) — immutable mathematical limit constraining sensitivity/specificity tradeoff. ε≈0.08. (2) Institutional threshold calibration (where radiology community positions itself on ROC curve) — contingent, driven by liability and incentives. ε≈0.52. This story addresses the institutional constraint. The mathematical constraint is downstream (affects_constraints) and would classify as Mountain if analyzed separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
