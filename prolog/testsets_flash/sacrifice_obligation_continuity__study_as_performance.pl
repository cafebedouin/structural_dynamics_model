% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity: Study as Performance
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint posits that the study of sacrifice law itself constitutes
 *   fulfillment of the commandment, thereby ensuring the continuity of the
 *   obligation through textual engagement rather than physical ritual. This
 *   reading emerged in contexts where physical sacrifice was impossible or
 *   impractical, allowing adherents to maintain active observance. It is
 *   framed as a Rope because it provides a coordination mechanism for ongoing
 *   religious practice without significant extraction, benefiting those who
 *   engage in study.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity: Study as Performance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '32ee0c4d-9469-4634-8c54-8a9a171ee2f4').
narrative_ontology:cs_kernel_codification('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', fixed_text).
narrative_ontology:cs_authority_grounding('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', lineage).
narrative_ontology:cs_interpretation_layer_present('32ee0c4d-9469-4634-8c54-8a9a171ee2f4').
narrative_ontology:cs_reading_relation('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', sacrifice_obligation_continuity__performance_only, influences).
narrative_ontology:cs_reading_relation('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', foundational, textual_engagement_is_ritual_performance).
narrative_ontology:cs_axiom_status(textual_engagement_is_ritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', textual_engagement_is_ritual_performance, deontological).
narrative_ontology:cs_axiom('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', foundational, obligation_persists_through_study).
narrative_ontology:cs_axiom_status(obligation_persists_through_study, holdable).
narrative_ontology:cs_axiom_grounding('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', obligation_persists_through_study, deontological).
narrative_ontology:cs_reference_frame('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', post_temple_destruction_observance).
narrative_ontology:cs_drift_state('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('32ee0c4d-9469-4634-8c54-8a9a171ee2f4', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, devout_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals dedicate their lives to the study and interpretation of religious texts, including sacrifice law. This reading validates their intellectual pursuit as a direct fulfillment of religious commandment, granting their work profound spiritual significance and maintaining their professional identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Individuals who seek to fulfill religious obligations but lack the means or context for physical sacrifice. This reading provides them with an accessible and meaningful path to observance through personal or communal study, alleviating the burden of an unfulfillable commandment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, devout_adherents, beneficiary,
    moderate, biographical, constrained, local).

% Groups who believe that sacrifice can only be fulfilled through physical ritual, to be restored in a messianic era. While not directly harmed, this reading offers an alternative that they view as a compromise, potentially diluting the urgency of their own agenda for physical restoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_restorationists, excluded,
    organized, generational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates the ongoing religious observance of a core commandment (sacrifice) for a dispersed community lacking a central temple, by re-framing textual study as a valid form of ritual performance.
% TRANSFER_FUNCTION: It transfers the spiritual burden of an unfulfillable physical commandment into an accessible intellectual and devotional practice, from the community to the individual adherent and scholar.
% ABSENT_VOICES: Those who insist on the literal, physical performance of sacrifice (e.g., 'performance_only' adherents or messianic restorationists) are implicitly sidelined by this reading, as it offers an alternative fulfillment. They would argue that study is preparation, not performance.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, devout adherents would face a profound crisis of religious observance, as a central commandment would become unfulfillable. The entire structure of religious education and scholarly engagement with sacrifice law would lose its direct spiritual valence, forcing a re-evaluation of religious identity and practice.
% FOUNDING_PROBLEM: The destruction of the central temple and the subsequent inability to perform physical sacrifices, leaving a core religious commandment unfulfillable for generations of adherents.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and historical accounts from the period following the temple's destruction corroborate the problem of unfulfillable sacrifice. Contemporary theological discourse and the continued absence of a central temple confirm the problem remains live. This is attested by a broad range of religious authorities and historians, not just the beneficiaries of this reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because engaging in study is generally accessible and does not impose significant material costs. Suppression is minimal (0.05) as there are no coercive mechanisms to enforce study, and alternatives (other forms of religious observance or non-observance) are not suppressed. Theater ratio is zero as the act of study is considered genuinely functional fulfillment within this reading. Accessibility collapse is high (0.8) because once this interpretation is accepted, the 'alternative' of physical sacrifice is no longer seen as the only valid path to fulfillment, making the study path the primary and most accessible option. Resistance is low (0.05) as this reading is widely accepted within its tradition as a legitimate mode of observance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those who adhere to this reading, the constraint is a clear Rope, providing a beneficial and accessible path to religious observance. From the perspective of those who might hold a 'performance_only' reading, this constraint might be seen as a reinterpretation that diminishes the original intent, but it does not directly extract from them. The engine's classification as Rope aligns with the structural benefits and low costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and devout adherents are the primary beneficiaries (d near 0.0) as they gain a path to fulfilling a core religious obligation through an accessible and intellectually enriching activity. There are no identifiable victims, as the constraint provides a means of fulfillment rather than imposing a burden. The constraint subsidizes these agents by providing a viable and accessible path to religious observance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine fulfillment of the sacrifice commandment through study, or is it a reinterpretation that avoids the practical difficulties of physical sacrifice?',
    'Theological consensus shift or the re-establishment of a central sacrificial cult.',
    'If reinterpretation, the constraint''s ''fulfillment'' aspect is performative, potentially shifting its classification towards Piton or Tangled Rope if it extracts from those who genuinely seek physical performance. If genuine fulfillment, it remains a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''study_as_performance'' reading of the ''sacrifice_obligation_continuity'' kernel. Sibling readings include ''performance_only'' (which this reading influences by offering an alternative fulfillment), ''messianic_suspension'' (which this reading coexists with by offering an active form of observance during suspension), and ''archival_preservation'' (which this reading forecloses by asserting normative force).').

omega_variable(
    study_accessibility_ambiguity,
    'Is the ''study'' required for fulfillment genuinely accessible to all adherents, or does it implicitly create an elite class of scholars whose engagement counts more?',
    'Empirical study of access to religious education and textual literacy across the adherent population.',
    'If study is not broadly accessible, the constraint''s low extractiveness is misleading, as it implicitly extracts from those unable to perform the ''fulfillment'' through study, potentially shifting it towards a Snare for the less educated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_accessibility_ambiguity, empirical, 'Ambiguity regarding the practical accessibility of the ''study'' required for fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 10, 0.0).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 20, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t10, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 20, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'sacrifice_obligation_continuity' kernel, each representing a distinct structural claim about the nature of religious obligation in the absence of a temple. Each reading has a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
