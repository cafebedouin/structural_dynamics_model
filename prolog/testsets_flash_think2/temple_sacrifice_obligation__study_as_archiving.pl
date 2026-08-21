% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation: Study as Archiving
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the halakhic (Jewish legal) reading that the
 *   study of Temple sacrificial laws serves to preserve knowledge for future
 *   restoration but does not, in itself, fulfill the divine obligation to
 *   perform these sacrifices. It acknowledges the unfulfilled nature of the
 *   command while providing a framework for continuity. The constraint is a
 *   reading of the 'temple_sacrifice_obligation' kernel, distinguishing
 *   itself from sibling readings that propose suspension or fulfillment
 *   through study.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.6).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.7).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation: Study as Archiving").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious_studies/halakhic_authority/commitment_systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '6fc402f8-3fd6-447e-a3f2-14ff40f62334').
narrative_ontology:cs_kernel_codification('6fc402f8-3fd6-447e-a3f2-14ff40f62334', fixed_text).
narrative_ontology:cs_authority_grounding('6fc402f8-3fd6-447e-a3f2-14ff40f62334', lineage).
narrative_ontology:cs_interpretation_layer_present('6fc402f8-3fd6-447e-a3f2-14ff40f62334').
narrative_ontology:cs_reading_relation('6fc402f8-3fd6-447e-a3f2-14ff40f62334', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('6fc402f8-3fd6-447e-a3f2-14ff40f62334', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_axiom('6fc402f8-3fd6-447e-a3f2-14ff40f62334', foundational, divine_command_is_immutable).
narrative_ontology:cs_axiom_status(divine_command_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('6fc402f8-3fd6-447e-a3f2-14ff40f62334', divine_command_is_immutable, theological).
narrative_ontology:cs_axiom('6fc402f8-3fd6-447e-a3f2-14ff40f62334', foundational, study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('6fc402f8-3fd6-447e-a3f2-14ff40f62334', study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('6fc402f8-3fd6-447e-a3f2-14ff40f62334', post_temple_halakhic_continuity).
narrative_ontology:cs_drift_state('6fc402f8-3fd6-447e-a3f2-14ff40f62334', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6fc402f8-3fd6-447e-a3f2-14ff40f62334', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, future_generations).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the binding nature of the unperformable sacrificial laws, ensuring their preservation through study. Their authority is maintained by this role, even as the core obligation remains unfulfilled.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Live under the burden of an unfulfilled divine command, finding spiritual meaning and continuity in the study of sacrificial laws, but without the full resolution of actual performance. They accept the authority's interpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, community_members, payer,
    moderate, biographical, constrained, global).

% The abstract entity representing the unfulfilled obligation for Temple sacrifices. It remains binding and unperformed, serving as a constant reminder of a broken covenant or incomplete state.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, divine_command, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, divine_command).

% Will inherit the meticulously preserved knowledge of the sacrificial laws, enabling their potential restoration and performance in a messianic era. They benefit from the archiving function of study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, future_generations, beneficiary,
    powerless, generational, analytical, global).

% Advocate for a different understanding of the obligation's status, perhaps that it is suspended or fulfilled by other means. Their interpretations are not the dominant halakhic view and are actively suppressed by the prevailing authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the detailed knowledge of the sacrificial rites and their associated laws, ensuring the continuity of tradition and readiness for future restoration, preventing the loss of complex halakhic details.
% TRANSFER_FUNCTION: Transfers the responsibility of knowledge preservation to scholars and the burden of unfulfilled obligation to a future, restored state, while maintaining the authority of the halakhic system in the present.
% ABSENT_VOICES: Those who believe the obligation is entirely suspended or fulfilled by other means (e.g., prayer, study as full occupation) are marginalized by this reading, as it insists on the unfulfilled nature of the command.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, the entire framework for post-Temple Jewish life would collapse. The community would lose its guiding principle for relating to the unperformable command, leading to profound theological and practical disarray.
% FOUNDING_PROBLEM: How to maintain the divine command for Temple sacrifices as binding law after the destruction of the Temple, when its performance is impossible, without either abrogating the law or falling into despair.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts, rabbinic commentaries across millennia, and the ongoing practice of studying these laws attest to the problem's persistence and the community's continuous engagement with it. This is corroborated by the very existence of extensive halakhic literature on the topic.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.6) because the entire post-Temple period is characterized by non-compliance with a divine command, creating a persistent state of unfulfilled duty. Suppression is high (0.7) as the halakhic authority actively maintains this interpretation, suppressing alternative views that might claim the obligation is suspended or fulfilled by other means. The theater ratio is moderate (0.4) because while study is a genuine functional activity (archiving), the maintenance of an unperformable law as 'binding' carries a performative aspect that reinforces authority in the absence of actual performance. Accessibility collapse is high (0.8) because the physical means to perform the sacrifices are absent, and the divine command itself is immutable. Resistance is low (0.2) as this reading is widely accepted within the traditional community.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic authorities, this constraint is a necessary and functional framework for maintaining tradition and divine law. From the perspective of the divine command (as an abstract victim), it represents a persistent state of non-compliance. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are beneficiaries (d=0.0) as they maintain their institutional role and interpretive authority. Future generations are also beneficiaries (d=0.0) as they receive preserved knowledge. The divine command itself and community members are targets (d=1.0) as they bear the burden of the unfulfilled obligation. Messianic advocates are excluded, as their alternative interpretations are not accepted by the dominant authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fulfillment_vs_archiving_ambiguity,
    'Is the act of studying sacrificial laws merely archiving for the future, or does it carry a degree of spiritual fulfillment or ''occupation'' of the obligation in the present?',
    'Theological consensus shifts or a new authoritative halakhic ruling that redefines the spiritual efficacy of study in the absence of the Temple.',
    'If study is deemed to carry a higher degree of fulfillment, the extractiveness of the constraint (the unfulfilled obligation) would decrease, potentially shifting its classification towards a Rope or even a Piton if the ''archiving'' function atrophies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fulfillment_vs_archiving_ambiguity, conceptual, 'Ambiguity regarding the spiritual efficacy of study in fulfilling the obligation.').

omega_variable(
    authority_maintenance_vs_divine_will,
    'To what extent does the insistence on the unfulfilled nature of the obligation serve to maintain the authority of the halakhic system, versus genuinely reflecting divine will?',
    'External theological critique or internal reform movements that challenge the interpretive framework, or a messianic event that renders the question moot.',
    'If primarily for authority maintenance, the constraint''s extractiveness and suppression would be re-evaluated as higher, potentially solidifying its Snare-like qualities. If purely divine will, the extractiveness is an inherent feature, not a product of human agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_maintenance_vs_divine_will, preference, 'Distinguishing between institutional self-preservation and faithful interpretation of divine command.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative interpretations structural (authority of tradition) or internalized (community acceptance)?',
    'Sociological studies of community attitudes towards alternative interpretations, or the emergence of significant dissenting movements within the halakhic world.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — community members carry the suppression with them after exposure to alternatives. If structural, removing the authoritative enforcement would lead to rapid diversification of views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.35).
narrative_ontology:measurement(temp_tr_t390, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 390, 0.37).
narrative_ontology:measurement(temp_tr_t780, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 780, 0.38).
narrative_ontology:measurement(temp_tr_t1170, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1170, 0.39).
narrative_ontology:measurement(temp_tr_t1560, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1560, 0.4).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(temp_be_t390, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 390, 0.57).
narrative_ontology:measurement(temp_be_t780, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 780, 0.58).
narrative_ontology:measurement(temp_be_t1170, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1170, 0.59).
narrative_ontology:measurement(temp_be_t1560, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1560, 0.6).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1950, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(temp_su_t390, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 390, 0.67).
narrative_ontology:measurement(temp_su_t780, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 780, 0.68).
narrative_ontology:measurement(temp_su_t1170, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1170, 0.69).
narrative_ontology:measurement(temp_su_t1560, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1560, 0.7).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
