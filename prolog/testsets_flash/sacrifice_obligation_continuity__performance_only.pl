% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation: Performance Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the 'performance only' reading of the
 *   sacrifice obligation within a religious tradition, where physical
 *   performance of sacrifices is deemed essential for fulfillment. Study of
 *   the laws is considered preparation for a future restoration, not a
 *   substitute for actual performance. This reading places the current
 *   generation of adherents in a state of unfulfillable obligation,
 *   generating high spiritual extractiveness and suppression due to the lack
 *   of a present remedy. The constraint is claimed as a Snare because it
 *   traps adherents in a cycle of unfulfilled duty without a current means of
 *   resolution, while benefiting future institutions and traditions.
 *
 * KEY AGENTS:
 *   - current_generation_adherents: Primary target (powerless/identity_locked) — bears unfulfillable obligation
 *   - pious_individuals: Secondary target (powerless/identity_locked) — bears spiritual frustration
 *   - future_priesthood: Primary beneficiary (institutional/analytical) — benefits from preserved role
 *   - messianic_tradition: Secondary beneficiary (institutional/analytical) — benefits from reinforced narrative
 *   - rabbinic_authorities_performance_only: Agenda setter (institutional/constrained) — administers and teaches this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '3029afb0-a319-41ff-b69f-c30704660fca').
narrative_ontology:cs_kernel_codification('3029afb0-a319-41ff-b69f-c30704660fca', fixed_text).
narrative_ontology:cs_authority_grounding('3029afb0-a319-41ff-b69f-c30704660fca', lineage).
narrative_ontology:cs_interpretation_layer_present('3029afb0-a319-41ff-b69f-c30704660fca').
narrative_ontology:cs_reading_relation('3029afb0-a319-41ff-b69f-c30704660fca', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('3029afb0-a319-41ff-b69f-c30704660fca', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('3029afb0-a319-41ff-b69f-c30704660fca', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('3029afb0-a319-41ff-b69f-c30704660fca', foundational, physical_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('3029afb0-a319-41ff-b69f-c30704660fca', physical_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('3029afb0-a319-41ff-b69f-c30704660fca', secondary, study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('3029afb0-a319-41ff-b69f-c30704660fca', study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('3029afb0-a319-41ff-b69f-c30704660fca', pre_destruction_temple_cult).
narrative_ontology:cs_drift_state('3029afb0-a319-41ff-b69f-c30704660fca', post_destruction_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3029afb0-a319-41ff-b69f-c30704660fca', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, future_priesthood).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, messianic_tradition).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, pious_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents who believe in the binding nature of the sacrifice obligation but are unable to perform it due to the absence of the Temple. They experience a constant state of unfulfilled duty and spiritual debt, with no current means of direct atonement through sacrifice. Their study of the laws is seen as preparation, not fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, identity_locked, global).

% Individuals deeply committed to the religious tradition who feel the weight of the unfulfilled obligation most acutely. They invest significant time and emotional energy in studying the laws of sacrifice, but this only reinforces the impossibility of actual performance, leading to spiritual frustration and a sense of inadequacy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, pious_individuals, payer,
    powerless, biographical, identity_locked, global).

% The theoretical future priestly class that would administer sacrifices upon the Temple's restoration. This reading preserves their future role and the sanctity of their lineage, ensuring the continuity of the ritual system even in its current dormant state. They benefit from the preservation of the performance-only doctrine.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, future_priesthood, beneficiary,
    institutional, generational, analytical, global).

% The theological framework that anticipates a future messianic era and the restoration of the Temple and its sacrificial cult. This reading reinforces the necessity of messianic redemption for the full practice of religious law, thereby strengthening the messianic narrative and its importance within the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_tradition, beneficiary,
    institutional, civilizational, analytical, global).

% The rabbinic scholars and leaders who uphold and teach this specific interpretation of the sacrifice obligation. They maintain the strict requirement for physical performance and emphasize study as a preparatory act, guiding adherents in navigating the spiritual implications of unfulfilled commandments. Their authority is tied to the preservation of this doctrine.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_authorities_performance_only, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual and intellectual efforts of adherents towards a future state of ritual fulfillment, maintaining the integrity of the sacrificial system's requirements even in its absence.
% TRANSFER_FUNCTION: Transfers a sense of unfulfilled obligation and spiritual longing from current adherents to the messianic future, while transferring authority and interpretive power to those who maintain the strict performance-only doctrine.
% ABSENT_VOICES: Adherents of the 'study as performance' reading, who would argue that their current textual engagement fully satisfies the obligation, are absent from the interpretive authority of this reading. They would challenge the notion of unfulfilled duty.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the spiritual landscape for many adherents would fundamentally shift. The burden of unfulfilled obligation would lift, potentially leading to new forms of religious practice or a reinterpretation of atonement. The messianic tradition's role in ritual restoration would be diminished, and rabbinic authority tied to this reading would be challenged.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of physical sacrifices, creating a crisis of how to fulfill divine commandments that require a physical cult.
% FOUNDING_PROBLEM_CORROBORATION: The problem is live as long as the Temple remains unbuilt and sacrifices cannot be performed. Corroboration comes from historical texts documenting the immediate spiritual crisis post-destruction, and from ongoing theological discourse across various traditions that grapple with the implications of the Temple's absence, not just from the beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because adherents are subject to a binding obligation they cannot fulfill, leading to spiritual guilt and a sense of inadequacy without a present remedy. Suppression is also high (0.9) as the theological framework and communal norms strongly enforce this interpretation, leaving no legitimate alternative for fulfilling the obligation in the present. Accessibility collapse is near total (0.95) because the physical conditions for performance (Temple, priesthood) are absent. Resistance is low (0.1) because the constraint is deeply internalized and framed as a divine decree, making direct resistance difficult within the tradition. Theater ratio is low (0.1) as the constraint's primary function is to maintain the strict performance requirement, not to create a performative substitute.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current adherents, this constraint is a Snare, trapping them in an unfulfillable duty. From the perspective of the future priesthood and messianic tradition, it is a necessary Rope or even a Mountain, preserving the integrity of the ritual system and the future redemption. The rabbinic authorities, as agenda setters, experience it as a foundational principle they must uphold, despite its current costs to adherents.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation adherents and pious individuals are full targets (d=1.0) as they bear the full weight of the unfulfillable obligation. The future priesthood and messianic tradition are beneficiaries (d=0.0) as this reading ensures their future relevance and theological coherence. Rabbinic authorities are agenda setters, benefiting from the preservation of their interpretive authority while also bearing the responsibility of guiding adherents through this difficult doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare because its primary function has shifted from facilitating ritual performance to maintaining a state of unfulfilled obligation, which benefits specific future-oriented institutions and interpretive authorities. The 'coordination' of preparing for future restoration serves as a cover for the ongoing extraction of spiritual debt from current adherents. The classification prevents mislabeling this as a Rope (genuine coordination) or a Mountain (natural law) by highlighting the identifiable victims and the active maintenance of an unfulfillable duty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_interpretive_choice,
    'Is the ''performance only'' requirement an irreducible feature of divine law (Mountain), or an interpretive choice by rabbinic authorities (Snare)?',
    'Comparative theological analysis across different religious traditions facing similar ritual cessation, examining whether alternative interpretations (e.g., symbolic fulfillment, suspension) are considered equally valid within their respective frameworks. If other traditions successfully adopt alternative fulfillments, it suggests interpretive choice.',
    'If a Mountain, the extractiveness is an inherent feature of reality. If an interpretive choice (Snare), the extractiveness is a consequence of a specific, enforceable doctrine that could be otherwise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_interpretive_choice, conceptual, 'Ambiguity between divine decree and human interpretation.').

omega_variable(
    spiritual_debt_vs_motivation,
    'Does the unfulfillable obligation primarily generate spiritual debt and guilt (extraction), or does it primarily serve as a powerful motivator for messianic hope and study (coordination)?',
    'Sociological and psychological studies of adherent communities: surveys on reported spiritual well-being, levels of guilt, and motivations for study. Longitudinal studies tracking adherence rates and engagement with messianic themes.',
    'If primarily debt/guilt, the extractiveness is confirmed. If primarily motivation, the constraint might have a stronger coordination function than currently assessed, potentially shifting it towards a Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_debt_vs_motivation, empirical, 'Ambiguity in the primary effect of the unfulfillable obligation.').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the high suppression primarily structural (absence of Temple, lack of legitimate alternatives) or internalized (adherents'' self-concept fused with the unfulfilled duty)?',
    'Post-restoration trajectory: if the sense of unfulfilled duty persists or re-emerges even after the Temple is rebuilt and sacrifices are possible, it suggests a strong internalized component. If it immediately resolves, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit (even if the structural barrier is removed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__performance_only, theater_ratio, 500, 0.12).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__performance_only, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__performance_only, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__performance_only, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 500, 0.75).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1500, 0.83).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 500, 0.8).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1000, 0.85).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1500, 0.88).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'sacrifice_obligation_continuity' kernel. Each reading offers a distinct structural interpretation of the obligation in the absence of the Temple, leading to different classifications and stakeholder impacts. This 'performance_only' reading emphasizes unfulfillable duty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
