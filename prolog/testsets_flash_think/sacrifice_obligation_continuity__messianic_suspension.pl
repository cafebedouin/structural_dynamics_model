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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Suspended Pending Messianic Restoration
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint describes the religious legal position that the
 *   obligation for animal sacrifice is suspended, neither fulfilled nor
 *   violated, during the period of the Temple's destruction, pending its
 *   messianic restoration. The study of sacrificial laws is understood as a
 *   means of maintaining readiness for this future event. This reading
 *   emphasizes continuity and future-orientation, framing the current state
 *   as a temporary but necessary holding pattern.
 *
 * KEY AGENTS:
 *   - community_of_scholars: Agenda-setter/Beneficiary (organized/identity_locked) — defines and transmits the tradition.
 *   - adherents: Payer/Beneficiary (moderate/identity_locked) — bear the burden of study, benefit from spiritual continuity.
 *   - messianic_aspirants: Beneficiary (powerless/identity_locked) — their hope is sustained by this framework.
 *   - external_observers: Observer (analytical/analytical) — study the phenomenon without normative commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.45).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Suspended Pending Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'a39e0454-0143-47bc-87e1-4e21db8fe6e1').
narrative_ontology:cs_kernel_codification('a39e0454-0143-47bc-87e1-4e21db8fe6e1', fixed_text).
narrative_ontology:cs_authority_grounding('a39e0454-0143-47bc-87e1-4e21db8fe6e1', lineage).
narrative_ontology:cs_interpretation_layer_present('a39e0454-0143-47bc-87e1-4e21db8fe6e1').
narrative_ontology:cs_reading_relation('a39e0454-0143-47bc-87e1-4e21db8fe6e1', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('a39e0454-0143-47bc-87e1-4e21db8fe6e1', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('a39e0454-0143-47bc-87e1-4e21db8fe6e1', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_axiom('a39e0454-0143-47bc-87e1-4e21db8fe6e1', foundational, obligation_is_suspended_not_abrogated).
narrative_ontology:cs_axiom_status(obligation_is_suspended_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('a39e0454-0143-47bc-87e1-4e21db8fe6e1', obligation_is_suspended_not_abrogated, deontological).
narrative_ontology:cs_axiom('a39e0454-0143-47bc-87e1-4e21db8fe6e1', foundational, study_is_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('a39e0454-0143-47bc-87e1-4e21db8fe6e1', study_is_preparatory_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('a39e0454-0143-47bc-87e1-4e21db8fe6e1', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('a39e0454-0143-47bc-87e1-4e21db8fe6e1', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a39e0454-0143-47bc-87e1-4e21db8fe6e1', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, community_of_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, adherents).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, messianic_aspirants).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for interpreting, preserving, and transmitting the laws of sacrifice. They derive status and purpose from this role, ensuring the tradition's continuity. Their identity is deeply intertwined with this scholarly pursuit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, community_of_scholars, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, community_of_scholars, beneficiary).

% Bear the burden of engaging in study and maintaining readiness for a future they may not personally witness. They benefit from the spiritual continuity and the hope for messianic restoration, which is central to their religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, adherents, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, adherents, beneficiary).

% Their entire worldview and hope for redemption are sustained by the belief in future restoration and the community's readiness. They are deeply invested in the constraint's persistence, even though they have no direct control over its 'sunset'.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messianic_aspirants, beneficiary,
    powerless, civilizational, identity_locked, universal).

% Academics or individuals outside the religious community who study the phenomenon from a historical, sociological, or theological perspective, without being bound by its normative claims.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, external_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the knowledge, interpretive framework, and communal readiness for the future re-establishment of sacrificial rituals, ensuring the continuity of a core religious obligation across generations during a period of physical impossibility.
% TRANSFER_FUNCTION: Transfers intellectual, spiritual, and communal effort from adherents and scholars into the preservation and interpretation of sacred texts and practices, ensuring the tradition's future viability and the collective hope for messianic restoration.
% ABSENT_VOICES: Those who believe the obligation is entirely abrogated or purely symbolic, or those who advocate for immediate, symbolic performance. They are excluded by the premise of suspension and future restoration, which defines the community's current practice.
% DISAPPEARANCE_RATIONALE: If the obligation of study and readiness vanished, the entire framework of messianic hope and the continuity of the sacrificial cult would collapse. This would necessitate a fundamental re-evaluation of religious identity, purpose, and practice for the community.
% FOUNDING_PROBLEM: The destruction of the Temple, rendering physical sacrifice impossible, while maintaining the divine commandment for sacrifice and the expectation of its future re-establishment.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts, rabbinic commentaries, and the ongoing practice of study within the community attest to the problem's origin and persistence. The physical absence of the Temple serves as continuous corroboration from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is moderate, reflecting the significant, ongoing intellectual and spiritual investment required to maintain readiness, but without the direct material costs or guilt associated with an active, unfulfilled obligation. Suppression is low, as adherence is primarily voluntary and identity-driven, rather than coercively enforced. Theater ratio is very low, as the commitment to study and future restoration is deeply genuine within the community. The constraint is classified as a Scaffold because it provides temporary support for a transitional period, with a clear 'sunset' (messianic restoration) and a function of maintaining readiness for a future state.
 *
 * PERSPECTIVAL GAP:
 *   From within the community, the constraint is a necessary and meaningful framework for spiritual continuity. From an external, secular perspective, the 'burden' of study might be seen as an arbitrary imposition, but this perspective does not capture the identity-locked nature of the participants.
 *
 * DIRECTIONALITY LOGIC:
 *   The community of scholars and adherents are both beneficiaries (of continuity and meaning) and payers (of effort and commitment). Messianic aspirants are primarily beneficiaries, as their core hope is sustained. No direct 'victims' exist, as the obligation is suspended, not actively extracting from unwilling parties. The 'burden' is a cost of participation in a shared identity and future, not a coercive extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining readiness for restoration) is still considered live by its adherents, preventing it from being a Piton. The 'sunset clause' (messianic restoration) is a core tenet, making Scaffold the appropriate classification. The absence of active enforcement and victims distinguishes it from a Snare or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_fulfillment_ambiguity,
    'Does the act of studying sacrificial laws constitute a form of ''fulfillment'' of the commandment, or is it purely preparatory?',
    'Further theological and halakhic (Jewish legal) discourse, potentially leading to a re-evaluation of the ''study_as_performance'' sibling reading''s status within this framework.',
    'If study is deemed a form of fulfillment, the ''messianic_suspension'' reading would shift closer to the ''study_as_performance'' reading, potentially reducing the perceived ''burden'' (extractiveness) by re-framing it as active observance rather than mere readiness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_fulfillment_ambiguity, conceptual, 'Ambiguity regarding the nature of ''fulfillment'' versus ''preparation'' in the absence of the Temple.').

omega_variable(
    messianic_timeline_uncertainty,
    'How does the indefinite and potentially very long timeline for messianic restoration impact the ''temporary'' nature of the suspension?',
    'No empirical resolution; depends on theological interpretation of eschatological timelines and the community''s capacity for sustained, long-term readiness.',
    'If the ''temporary'' nature is perceived as effectively permanent, the ''scaffold'' classification might drift towards a ''piton'' (inertial maintenance) or a ''rope'' (stable coordination for an indefinite future), as the transitional aspect becomes less salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timeline_uncertainty, conceptual, 'Uncertainty about the duration of the ''suspension'' and its impact on the constraint''s transitional nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(sacr_tr_t1925, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1925, 0.05).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(sacr_tr_t1975, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(sacr_tr_t2025, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(sacr_be_t1925, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1925, 0.42).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1950, 0.43).
narrative_ontology:measurement(sacr_be_t1975, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1975, 0.44).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(sacr_be_t2025, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t1900, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(sacr_su_t1925, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1925, 0.1).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(sacr_su_t1975, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(sacr_su_t2025, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
