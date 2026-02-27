% ============================================================================
% CONSTRAINT STORY: radiologic_diagnostic_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
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
 *   This constraint governs the classification of medical imaging findings
 *   into "benign," "incidental," or "critical." It is not a purely biological
 *   fact but an indexical classification dependent on the reader's power
 *   level, legal exposure, and technological tools. The patient bears the
 *   downstream costs of a risk-averse system, while the radiologist benefits
 *   from clear diagnostic protocols and legal protections.
 *
 * KEY AGENTS:
 *   - Patients: Primary target (powerless/trapped) — bear the costs of overdiagnosis and unnecessary follow-up.
 *   - Referring Physicians: Secondary target (moderate/constrained) — bear the burden of communicating potentially alarming findings.
 *   - Radiologists: Primary beneficiary (institutional/arbitrage) — benefit from risk mitigation afforded by diagnostic protocols.
 *   - Hospital Legal Teams: (Institutional/constrained) intended coordination, becomes performative piton
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(radiologic_diagnostic_threshold, 0.5).
domain_priors:suppression_score(radiologic_diagnostic_threshold, 0.6).
domain_priors:theater_ratio(radiologic_diagnostic_threshold, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, extractiveness, 0.5).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(radiologic_diagnostic_threshold, tangled_rope).
narrative_ontology:human_readable(radiologic_diagnostic_threshold, "The Radiologic Diagnostic Threshold").
narrative_ontology:topic_domain(radiologic_diagnostic_threshold, "medical/technological/legal").

domain_priors:requires_active_enforcement(radiologic_diagnostic_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, radiologists).
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, hospital_legal_teams).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, patients).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, referring_physicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (SNARE) — The patient is trapped in the system, bearing the costs of overdiagnosis, anxiety, and unnecessary follow-up procedures stemming from low diagnostic thresholds.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REFERRING PHYSICIAN (TANGLED ROPE) — Constrained by legal liability and patient expectations, the referring physician benefits from the risk mitigation provided by radiologist interpretations, but bears the burden of communicating potentially alarming findings to patients even if the clinical significance is uncertain.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RADIOLOGIST (ROPE) — Benefits from clear diagnostic protocols and legal protections afforded by erring on the side of caution. The radiologist can arbitrage their position, minimizing legal risk, by classifying ambiguous findings as potentially significant, triggering further investigation.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HOSPITAL LEGAL TEAM (PITON) — While intended to coordinate risk mitigation, defensive medicine and CYA is largely performative. A risk-averse culture creates a degraded function that continues through institutional inertia.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the diagnostic threshold is a complex interplay of technology, legal pressures, and evolving medical understanding. A mixed coordination-extraction constraint with a substantial theater component.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(radiologic_diagnostic_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.50): Moderate. The system extracts resources from patients through unnecessary procedures and from referring physicians through increased workload. Suppression (0.60): High. Patients have limited ability to challenge radiologist interpretations; referring physicians are constrained by the legal and professional norms.
 *
 * PERSPECTIVAL GAP:
 *   The patient experiences the system as a snare, bearing the cost of overdiagnosis and unnecessary procedures. The radiologist experiences it as a rope, providing a valuable service and mitigating risk. The referring physician occupies a middle ground, experiencing a tangled rope as they must balance patient well-being with legal concerns. The hospital legal team is the degraded piton. 
 *
 * DIRECTIONALITY LOGIC:
 *   The radiologist benefits from the system's risk-averse nature (low d), while the patient bears the costs of overdiagnosis (high d). The referring physician is in a more ambivalent position, constrained by the system but also benefiting from the risk mitigation provided by radiologist interpretations (intermediate d). The legal team perspective reflects the performative maintenance. 
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by showing how the same structural data creates different constraint experiences depending on perspective. The radiologist's arbitrage is real from their perspective. The legal team's CYA is often performative. The patient's 'trapped' condition is structural: a high extraction snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_liability_vs_patient_autonomy,
    'What balance between legal liability and patient autonomy defines the radiologic diagnostic threshold?',
    'Comparative legal analysis across jurisdictions; patient surveys on risk tolerance',
    'If liability predominates: lower thresholds, increased overdiagnosis. If autonomy is prioritized: higher thresholds, potential for missed diagnoses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_liability_vs_patient_autonomy, preference, 'The balance between legal liability and patient autonomy').

omega_variable(
    technological_artifact_detection_vs_clinical_significance,
    'To what extent are incidental findings detected due to technological artifacts rather than true clinical significance?',
    'Correlation analysis between image resolution, noise levels, and the incidence of incidental findings',
    'If artifacts dominate: thresholds should be raised. If true pathology is being detected: thresholds may be appropriate or too high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_artifact_detection_vs_clinical_significance, empirical, 'The impact of technology on detection of abnormalities').

omega_variable(
    evolving_clinical_knowledge_impact,
    'How rapidly does clinical knowledge evolve, affecting the interpretation of radiological findings?',
    'Bibliometric analysis of relevant medical literature; expert elicitation on changing diagnostic criteria',
    'Rapid evolution: frequent recalibration of diagnostic thresholds needed. Slow evolution: stable thresholds are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_clinical_knowledge_impact, empirical, 'The rate clinical knowledge evolves').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(radiologic_diagnostic_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(radi_tr_t0, radiologic_diagnostic_threshold, theater_ratio, 0, 0.2).
narrative_ontology:measurement(radi_tr_t5, radiologic_diagnostic_threshold, theater_ratio, 5, 0.3).
narrative_ontology:measurement(radi_tr_t10, radiologic_diagnostic_threshold, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(radi_be_t0, radiologic_diagnostic_threshold, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(radi_be_t5, radiologic_diagnostic_threshold, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(radi_be_t10, radiologic_diagnostic_threshold, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(radiologic_diagnostic_threshold, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
