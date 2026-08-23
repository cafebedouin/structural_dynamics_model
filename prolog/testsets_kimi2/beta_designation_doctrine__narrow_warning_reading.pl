% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__narrow_warning_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Doctrine â Narrow Warning Reading
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint instantiates the narrow_warning_reading of the contested
 *   beta_designation_doctrine kernel. Under this reading, beta designation is
 *   strictly a time-bounded testing disclosure regime: developers receive a
 *   temporary liability shield only during a genuine testing phase, must
 *   inform users, and cannot use the label to waive base product liability
 *   permanently. The coordination function is transitionalâbeta status is
 *   justified by the need to move from untested software to a stable release,
 *   not as a steady-state liability regime. The reading directly contradicts
 *   the expansive_shield_reading (indefinite comprehensive waiver) and
 *   coexists with the severity_carve_out_reading (unavailable for critical
 *   systems).
 *
 * KEY AGENTS:
 *   - beta_software_vendors: Primary beneficiary (powerful/constrained) â receives temporary liability shield during genuine testing phases
 *   - end_users: Dual-positioned beneficiary/payer (moderate/mobile) â bears defect risk during testing but retains base liability rights and meaningful exit
 *   - judiciary: Agenda setter (institutional/analytical) â interprets bounds of genuine testing phase and administers doctrine
 *   - consumer_protection_agencies: Observer (institutional/analytical) â monitors for evasion and advocates for narrow construction
 *   - product_liability_plaintiffs: Excluded voice (moderate/constrained) â would argue for broader liability but absent from doctrinal design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.28).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.25).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine â Narrow Warning Reading").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '1f05c38c-1ef3-4d2e-95de-c01165be1942').
narrative_ontology:cs_kernel_codification('1f05c38c-1ef3-4d2e-95de-c01165be1942', formalized).
narrative_ontology:cs_authority_grounding('1f05c38c-1ef3-4d2e-95de-c01165be1942', lineage).
narrative_ontology:cs_interpretation_layer_present('1f05c38c-1ef3-4d2e-95de-c01165be1942').
narrative_ontology:cs_reading_relation('1f05c38c-1ef3-4d2e-95de-c01165be1942', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('1f05c38c-1ef3-4d2e-95de-c01165be1942', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('1f05c38c-1ef3-4d2e-95de-c01165be1942', foundational, beta_liability_proportional_to_testing_phase).
narrative_ontology:cs_axiom_status(beta_liability_proportional_to_testing_phase, holdable).
narrative_ontology:cs_axiom_grounding('1f05c38c-1ef3-4d2e-95de-c01165be1942', beta_liability_proportional_to_testing_phase, conventional).
narrative_ontology:cs_axiom('1f05c38c-1ef3-4d2e-95de-c01165be1942', foundational, informed_user_consent_preserves_base_recourse).
narrative_ontology:cs_axiom_status(informed_user_consent_preserves_base_recourse, holdable).
narrative_ontology:cs_axiom_grounding('1f05c38c-1ef3-4d2e-95de-c01165be1942', informed_user_consent_preserves_base_recourse, conventional).
narrative_ontology:cs_reference_frame('1f05c38c-1ef3-4d2e-95de-c01165be1942', bounded_testing_privilege).
narrative_ontology:cs_drift_state('1f05c38c-1ef3-4d2e-95de-c01165be1942', contemporary_software_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f05c38c-1ef3-4d2e-95de-c01165be1942', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_software_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, end_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, end_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Release software under beta designation to obtain real-world testing data and user feedback while enjoying a temporary reduction in liability exposure during the genuine testing period. Must adhere to time bounds and disclosure requirements to maintain the designation.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_software_vendors, beneficiary,
    powerful, biographical, constrained, national).

% Gain early access to software and contribute to testing by reporting real-world usage conditions. Bear the risk of defects and instability during the testing phase, but are informed of these risks through disclosure and retain base product liability rights for harm caused by the software.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, end_users, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, end_users, payer).

% Interprets and enforces the boundaries of the beta designation doctrine, determining whether a given deployment constitutes a genuine testing phase and whether the developer maintained adequate disclosure and time bounds.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Monitor beta deployments to ensure disclosure is adequate and that vendors do not use the beta designation to evade permanent liability. Can bring enforcement actions or advocate for legislative narrowing of the doctrine.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_agencies, observer,
    institutional, generational, analytical, national).

% Would argue for broader vendor liability and narrower beta shields if included in the doctrinal conversation; currently absent from the policy framework but potentially active in litigation testing the boundaries.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, product_liability_plaintiffs, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables software developers to deploy unfinished products to real users for field testing when artificial testing environments are insufficient, while preserving a baseline liability floor that prevents the testing privilege from becoming a general waiver.
% TRANSFER_FUNCTION: Temporarily shifts a portion of liability risk from vendors to informed end users during a bounded testing period; the transfer automatically expires when the genuine testing phase concludes and base product liability remains intact throughout.
% ABSENT_VOICES: Product liability plaintiffs and some consumer advocates who would argue against any liability reduction for commercial software releases regardless of disclosure; technically non-literate users who cannot meaningfully parse beta disclosures despite their legal significance.
% DISAPPEARANCE_RATIONALE: If the narrow beta designation doctrine vanished, vendors would face full product liability for all deployed software including pre-release versions. Field testing with external users would become legally perilous, likely pushing testing entirely in-house or slowing release cycles; the software development lifecycle would reorganize around liability avoidance rather than iterative user feedback.
% FOUNDING_PROBLEM: Software defects often manifest only under real-world usage conditions that cannot be replicated in controlled test environments, yet exposing unfinished software to users creates liability exposure that may deter necessary field testing.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering literature on the limitations of synthetic testing corroborates the need for field exposure; courts and regulators outside the vendor beneficiary class acknowledge the educational and quality benefits of beta programs while disputing their scope.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).
:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the liability reduction is strictly time-bounded and base liability is preserved; users retain recourse and the transfer is consensual. Suppression is low (0.25) because users are informed and can decline beta participation; enforcement consists mainly of judicial boundary-setting rather than coerced participation. Theater ratio is low (0.20) because the narrow reading maintains a functional testing privilege with little performative maintenance, though some drift toward perpetual beta labeling has emerged. Accessibility collapse is moderate (0.35): in-house testing and staged rollouts remain available alternatives. Resistance is moderate-low (0.30): consumer advocates resist any liability reduction but the narrow bounds of this reading limit organized opposition. The measurement series shows slight drift upward over the interval as beta labeling has become more common and occasionally performative, but the core structure remains bounded.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as necessary coordination for innovationâwithout a bounded testing privilege, field testing would be prohibitively risky. The user seat experiences it as a calculated trade: early access and influence over product development in exchange for bearing known defect risk, with the safety net of preserved base liability. The judicial seat experiences it as a bounded legal privilege requiring continuous line-drawing between genuine testing and general release. The engine should compute different types across these seats: the vendor as beneficiary of a scaffold, the user as near-symmetric participant, and the judiciary as agenda setter administering the transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Beta software vendors are declared beneficiaries and receive low directionality (subsidy side): the constraint temporarily reduces their liability exposure. End users are not declared victims because the narrow reading preserves their base liability and treats their participation as informed consent; with mobile exit options, their directionality sits near symmetric. The judiciary is agenda setter with analytical exit and does not sit on the beneficiary-victim axis. Product liability plaintiffs are excluded from the conversation and would sit on the target side if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because its founding problem (the need for real-world field testing) remains live, and its sunset clause (genuine testing phase) ties its persistence to the transition rather than the steady state. If the doctrine persisted after the testing phase ended or if it were applied to finished products, it would be a snare; the narrow reading's time bound and liability preservation prevent that misclassification. The slight upward drift in theater_ratio and extractiveness over the interval is monitored but has not yet reached piton thresholds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the narrow_warning_reading of the beta_designation_doctrine kernel; siblings are expansive_shield_reading and severity_carve_out_reading. What would change structurally if the expansive shield reading were adopted instead?',
    'Comparative legal analysis of jurisdictions adopting each reading, tracking duration bounds and liability preservation rates.',
    'Adoption of the expansive reading would remove the time bound and preserve the liability shield indefinitely, converting this scaffold into a snare or tangled rope by severing the sunset that justifies the coordination as transitional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural consequences of adopting the expansive sibling reading.').

omega_variable(
    testing_phase_boundary,
    'What constitutes a genuine testing phase sufficient to trigger the narrow reading''s liability shield, and who determines when it ends?',
    'Longitudinal case law tracking or regulatory guideline enumeration of factors courts use to distinguish genuine testing from general release.',
    'If genuine testing cannot be objectively bounded, the scaffold''s sunset is illusory and the constraint drifts toward piton or snare as the designation becomes a permanent liability workaround.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testing_phase_boundary, empirical, 'Empirical boundedness of the testing-phase sunset.').

omega_variable(
    user_victimization_threshold,
    'Does informed disclosure eliminate user victimization under this reading, or merely mitigate it?',
    'Empirical study of beta user harm rates, disclosure comprehension rates, and successful liability recourse against beta vendors.',
    'If users systematically cannot exercise preserved base liability due to disclosure complexity or cost barriers, the reading''s not-victimized claim fails and the constraint shows higher effective extraction than the narrow framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_victimization_threshold, empirical, 'Whether informed consent fully prevents victimization in beta relationships.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(beta_designation_doctrine__narrow_warning_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
