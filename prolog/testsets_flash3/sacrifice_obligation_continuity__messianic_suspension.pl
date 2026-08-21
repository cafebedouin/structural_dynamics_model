% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Continuity (Messianic Suspension Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the 'messianic suspension' reading of the
 *   sacrifice obligation continuity kernel. It posits that the divine
 *   commandment for sacrifices is not abrogated or fulfilled by study, but
 *   rather suspended pending the messianic era and the rebuilding of the
 *   Temple. The primary activity is the study and preservation of the laws of
 *   sacrifice, which serves as a form of readiness and spiritual engagement,
 *   rather than a direct fulfillment or a mere archival exercise. This
 *   reading avoids the victim set of an active obligation while maintaining
 *   its future relevance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.45).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.6).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Continuity (Messianic Suspension Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '124b9480-f9e1-4c4b-9686-865283f20e4d').
narrative_ontology:cs_kernel_codification('124b9480-f9e1-4c4b-9686-865283f20e4d', fixed_text).
narrative_ontology:cs_authority_grounding('124b9480-f9e1-4c4b-9686-865283f20e4d', lineage).
narrative_ontology:cs_interpretation_layer_present('124b9480-f9e1-4c4b-9686-865283f20e4d').
narrative_ontology:cs_reading_relation('124b9480-f9e1-4c4b-9686-865283f20e4d', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('124b9480-f9e1-4c4b-9686-865283f20e4d', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('124b9480-f9e1-4c4b-9686-865283f20e4d', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('124b9480-f9e1-4c4b-9686-865283f20e4d', foundational, divine_commandment_eternality_and_irrevocability).
narrative_ontology:cs_axiom_status(divine_commandment_eternality_and_irrevocability, holdable).
narrative_ontology:cs_axiom_grounding('124b9480-f9e1-4c4b-9686-865283f20e4d', divine_commandment_eternality_and_irrevocability, theological).
narrative_ontology:cs_axiom('124b9480-f9e1-4c4b-9686-865283f20e4d', foundational, messianic_era_as_prerequisite_for_physical_restoration).
narrative_ontology:cs_axiom_status(messianic_era_as_prerequisite_for_physical_restoration, holdable).
narrative_ontology:cs_axiom_grounding('124b9480-f9e1-4c4b-9686-865283f20e4d', messianic_era_as_prerequisite_for_physical_restoration, theological).
narrative_ontology:cs_reference_frame('124b9480-f9e1-4c4b-9686-865283f20e4d', post_temple_destruction_rabbinic_consensus).
narrative_ontology:cs_drift_state('124b9480-f9e1-4c4b-9686-865283f20e4d', contemporary_era_of_zionist_movements, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('124b9480-f9e1-4c4b-9686-865283f20e4d', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, lay_adherents).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, messianic_era_restoration).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, divine_commandment_eternality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the textual tradition, ensuring the knowledge of sacrifice law is preserved and understood. They maintain the intellectual infrastructure for future restoration, bearing the burden of continuous study and teaching.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives spiritual guidance and a sense of continuity with divine commandments, without the immediate burden of ritual performance. They are prepared for future restoration through the scholars' work, but also bear the diffuse cost of maintaining the scholarly tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_community, beneficiary,
    organized, generational, identity_locked, global).

% Contribute to the upkeep of religious institutions and support scholars, indirectly funding the maintenance of sacrifice law readiness. They experience a moderate burden of expectation and readiness, but no direct ritual obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, lay_adherents, payer,
    moderate, biographical, constrained, local).

% Believe active preparation for sacrifice, beyond mere study, is required now. Their calls for more immediate action are often sidelined by the dominant 'suspension' reading, which defers physical performance to a future, divinely ordained moment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messianic_restoration_advocates, excluded,
    organized, immediate, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's understanding and practice regarding sacrifice, ensuring continuity of tradition and readiness for future messianic restoration, preventing premature or unauthorized ritual attempts.
% TRANSFER_FUNCTION: Transfers the active burden of ritual performance from the general community to a deferred future, while transferring the intellectual and spiritual maintenance burden to religious scholars and institutions.
% ABSENT_VOICES: Those who advocate for immediate, physical re-establishment of sacrificial rituals are largely excluded from the mainstream discourse, which emphasizes study and spiritual readiness over physical action pending messianic arrival.
% DISAPPEARANCE_RATIONALE: If the messianic suspension reading vanished, the religious community would face immediate and profound theological and practical crises regarding the status of divine commandments, potentially leading to fragmentation or unauthorized ritual attempts. The entire framework of religious law and eschatology would need to be re-evaluated.
% FOUNDING_PROBLEM: The destruction of the Temple rendered physical sacrificial rituals impossible, creating a crisis of continuity for divine commandments related to sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and centuries of rabbinic commentary attest to the historical problem. The ongoing absence of the Temple and the messianic era corroborates the problem's live status, as acknowledged by nearly all religious authorities outside of fringe groups.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) due to the ongoing burden of study and institutional maintenance without immediate ritual benefit, but not high because it avoids the guilt of non-performance. Suppression (0.6) reflects the strong communal and theological pressure to adhere to this interpretation, discouraging alternative readings that might lead to premature action or abandonment of the tradition. Theater ratio is low (0.1) as the study is genuinely seen as a necessary preparatory act, not a mere performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious scholars, this is a vital rope, coordinating the community's spiritual life and ensuring future continuity. From the perspective of messianic restoration advocates, it might appear as a tangled rope or even a snare, as it defers and suppresses what they see as an immediate divine imperative.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars are beneficiaries as they gain status and purpose from maintaining the tradition, but also bear the primary cost of its intellectual upkeep. The religious community benefits from spiritual continuity and deferred obligation. Lay adherents are payers through their support of institutions. Messianic restoration advocates are excluded, as their interpretation of immediate action is suppressed by the dominant 'suspension' reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a piton or snare. While there's a burden, the 'suspension' reading actively solves the problem of how to maintain a divine commandment when its physical performance is impossible, without claiming the study itself is the full performance. It's a genuine coordination for a future state, not an atrophied function or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_suspension,
    'Is the ''suspension'' of the sacrifice obligation a temporary deferral of an active command, or a reinterpretation that effectively redefines the command''s current scope?',
    'Theological consensus shifts or a definitive messianic event. Absent such, it remains a matter of interpretive stance.',
    'If a mere deferral, the potential for future extraction (when the obligation becomes active) is high. If a redefinition, the current extractiveness is stable, but the ''readiness'' aspect might become theatrical if the redefinition is permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_suspension, conceptual, 'Ambiguity in the theological nature of the obligation''s suspension.').

omega_variable(
    study_as_fulfillment_ambiguity,
    'To what extent does the ''study'' component of this reading implicitly function as a form of ''fulfillment'' for some adherents, blurring the line with the ''study_as_performance'' sibling reading?',
    'Empirical sociological study of adherent beliefs and practices, or explicit theological rulings clarifying the distinction.',
    'If study is widely perceived as fulfillment, this reading''s extractiveness might be lower (as the burden is offset by perceived spiritual gain), but its distinctness from the ''study_as_performance'' reading would diminish, potentially leading to a reclassification towards that sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_ambiguity, empirical, 'Overlap in perceived function between study as readiness and study as fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(sacr_su_t40, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(sacr_su_t80, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, messianic_expectation_norms).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, religious_education_curriculum).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_continuity' kernel. Other readings include 'study_as_performance', 'performance_only', and 'archival_preservation', each representing a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
