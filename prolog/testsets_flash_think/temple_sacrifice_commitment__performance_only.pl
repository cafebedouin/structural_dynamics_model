% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Law (Performance-Only Reading)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'performance_only' reading of the
 *   'temple_sacrifice_commitment' kernel. This reading asserts that the
 *   divine commandment for Temple sacrifices requires material instantiation
 *   to be active; study without actual performance is merely archival
 *   preservation of a defunct practice, not an occupation of the commitment
 *   itself. In the absence of the Temple, the sacrifice law, as an active
 *   constraint, is a dormant husk.
 *
 * KEY AGENTS:
 *   - halakhic_scholars: Agenda-setter (institutional/identity_locked) — interpret and transmit the law.
 *   - observant_jews: Payer (moderate/identity_locked) — bear the theological gap of non-performance.
 *   - messianic_restorationists: Beneficiary (organized/identity_locked) — benefit from the idea of future restoration.
 *   - secular_historians: Observer (analytical/analytical) — study the law as a historical artifact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Law (Performance-Only Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, 'cc593188-b64a-4278-8a75-55ae7737e8ca').
narrative_ontology:cs_kernel_codification('cc593188-b64a-4278-8a75-55ae7737e8ca', fixed_text).
narrative_ontology:cs_authority_grounding('cc593188-b64a-4278-8a75-55ae7737e8ca', lineage).
narrative_ontology:cs_interpretation_layer_present('cc593188-b64a-4278-8a75-55ae7737e8ca').
narrative_ontology:cs_reading_relation('cc593188-b64a-4278-8a75-55ae7737e8ca', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('cc593188-b64a-4278-8a75-55ae7737e8ca', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('cc593188-b64a-4278-8a75-55ae7737e8ca', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('cc593188-b64a-4278-8a75-55ae7737e8ca', foundational, material_instantiation_required).
narrative_ontology:cs_axiom_status(material_instantiation_required, holdable).
narrative_ontology:cs_axiom_grounding('cc593188-b64a-4278-8a75-55ae7737e8ca', material_instantiation_required, deontological).
narrative_ontology:cs_axiom('cc593188-b64a-4278-8a75-55ae7737e8ca', secondary, study_is_archival_only).
narrative_ontology:cs_axiom_status(study_is_archival_only, holdable).
narrative_ontology:cs_axiom_grounding('cc593188-b64a-4278-8a75-55ae7737e8ca', study_is_archival_only, conventional).
narrative_ontology:cs_reference_frame('cc593188-b64a-4278-8a75-55ae7737e8ca', temple_era_performance).
narrative_ontology:cs_drift_state('cc593188-b64a-4278-8a75-55ae7737e8ca', post_second_temple_destruction, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('cc593188-b64a-4278-8a75-55ae7737e8ca', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, messianic_restorationists).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, observant_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the sacrifice law, defining its requirements and current status. They ensure its textual preservation and intellectual understanding, even in the absence of performance, but do not claim that study itself fulfills the commandment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Acknowledge the divine origin and future potential of the sacrifice law, but cannot perform it due to the absence of the Temple. Their religious life is shaped by its historical significance and the aspiration for its future reinstatement, bearing the 'cost' of non-performance as a theological gap.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, observant_jews, payer,
    moderate, biographical, identity_locked, global).

% Actively advocate for the rebuilding of the Temple and the reinstatement of sacrifices. They benefit from the idea of the law's future restoration, which gives their movement purpose and legitimacy, viewing the law as merely suspended, not defunct.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, messianic_restorationists, beneficiary,
    organized, generational, identity_locked, global).

% Study the sacrifice law as a historical, cultural, and legal artifact, without religious commitment to its performance or future. They analyze its structure and impact from an external, academic perspective.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, secular_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in its active, performative form. The law, in its dormant state, implicitly coordinates the preservation of its textual and interpretive tradition among scholars, awaiting future conditions for performance.
% TRANSFER_FUNCTION: None in its active, performative form. Historically, it transferred offerings from individuals to the Temple, and spiritual merit from God to the people. In its dormant state, it transfers scholarly effort to archival preservation.
% ABSENT_VOICES: Those who believe that study or prayer *is* a form of sacrifice, or that the commitment has been symbolically transformed, would object to the characterization of the law as a 'defunct practice' or 'dormant husk'.
% DISAPPEARANCE_RATIONALE: The physical conditions for the law's performance (the Temple) are absent, so its active function is already nil. Its disappearance as a live constraint would not change current religious practice, though it would profoundly alter theological discourse and messianic aspirations.
% FOUNDING_PROBLEM: To establish and maintain a covenantal relationship between the Israelite people and God through prescribed ritual offerings, atonement, and communal worship.
% FOUNDING_PROBLEM_CORROBORATION: Ancient biblical texts, historical accounts (e.g., Josephus), and archaeological evidence corroborate the historical function of Temple sacrifices. Rabbinic literature throughout history attests to the problem's historical 'solution' and its current non-applicability due to the Temple's destruction.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Piton because its primary function (actual performance of sacrifices) has atrophied due to the destruction of the Temple. The law persists due to institutional inertia and profound theological significance, but without active enforcement or extraction. Base extractiveness and suppression are very low (0.05) as there is no active practice to extract from or suppress. Theater ratio is low (0.10) because scholarly study is genuinely archival and preservative, not a theatrical performance of a live constraint. Accessibility collapse is high (0.90) because the physical conditions for performance are entirely absent. Resistance is low (0.10) as there is no active practice to resist.
 *
 * PERSPECTIVAL GAP:
 *   While this reading views the law as a dormant husk, other readings (e.g., 'study_as_exercise' or 'symbolic_transformation') perceive the commitment as actively occupied through alternative means. The engine's classification of this reading as a Piton highlights the structural inactivity, contrasting with the active engagement claimed by other perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Since the constraint is largely inert, there are no direct beneficiaries or victims in an extractive sense. Halakhic scholars act as agenda-setters by defining the law's status. Observant Jews are 'payers' in a theological sense, bearing the aspiration for future performance. Messianic restorationists are 'beneficiaries' of the future-oriented aspect of the law, which fuels their movement. Secular historians are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the sacrifice law was active performance. This mandate has outlived its function due to the destruction of the Temple. The constraint is a Piton because its core function has atrophied, but the law itself persists due to its divine origin and the inertia of religious tradition. There is no concentrated beneficiary actively maintaining the defunct practice, nor is there sufficient harm to drive its 'fixing' (i.e., formal repudiation or transformation within this reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_instantiation_ambiguity,
    'Is the requirement for ''material instantiation'' an inherent, immutable feature of divine law, or is it a rabbinic interpretation that could be re-evaluated under changed circumstances?',
    'A re-interpretation by a recognized halakhic authority that formally redefines ''performance'' or ''instantiation'' in the absence of the Temple.',
    'If re-evaluated, the constraint''s status could shift from defunct to active (e.g., as a ''study_as_exercise'' or ''symbolic_transformation'' type), dramatically altering its extractiveness and coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_instantiation_ambiguity, conceptual, 'Ambiguity regarding the immutability of the material instantiation requirement.').

omega_variable(
    future_restoration_impact,
    'If conditions for Temple sacrifice were restored, would the re-instated practice be a high-extraction Snare or a genuine Rope, given the historical context of power dynamics and potential for coercion?',
    'Observation of actual practice if the Temple were rebuilt and sacrifices resumed, particularly regarding access, cost, and enforcement mechanisms.',
    'If restoration led to high extraction and suppression, the constraint would reclassify from Piton to Snare, with significant implications for victims and beneficiaries. If it genuinely solved coordination problems with minimal extraction, it would be a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_restoration_impact, empirical, 'Potential for future extraction if the practice were reinstated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_commitment__performance_only, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(temp_tr_t1930, temple_sacrifice_commitment__performance_only, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(temp_tr_t1960, temple_sacrifice_commitment__performance_only, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(temp_tr_t1990, temple_sacrifice_commitment__performance_only, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(temp_tr_t2025, temple_sacrifice_commitment__performance_only, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_commitment__performance_only, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(temp_be_t1930, temple_sacrifice_commitment__performance_only, base_extractiveness, 1930, 0.05).
narrative_ontology:measurement(temp_be_t1960, temple_sacrifice_commitment__performance_only, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(temp_be_t1990, temple_sacrifice_commitment__performance_only, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(temp_be_t2025, temple_sacrifice_commitment__performance_only, base_extractiveness, 2025, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t1900, temple_sacrifice_commitment__performance_only, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(temp_su_t1930, temple_sacrifice_commitment__performance_only, suppression_requirement, 1930, 0.05).
narrative_ontology:measurement(temp_su_t1960, temple_sacrifice_commitment__performance_only, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(temp_su_t1990, temple_sacrifice_commitment__performance_only, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(temp_su_t2025, temple_sacrifice_commitment__performance_only, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
