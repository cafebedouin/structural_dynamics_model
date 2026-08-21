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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Halakhic Obligation of Temple Sacrifice: Study as Archiving Reading
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'study as archiving' reading of
 *   the Temple sacrifice obligation, which emerged after the destruction of
 *   the Second Temple. This reading posits that while the divine command for
 *   sacrifices remains binding, its performance is currently impossible.
 *   Therefore, the study of the sacrificial laws serves to preserve knowledge
 *   for future restoration, but does not fulfill the obligation itself. This
 *   creates a state of perpetual non-compliance for the Jewish community,
 *   maintained by halakhic authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.65).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.75).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Halakhic Obligation of Temple Sacrifice: Study as Archiving Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious_studies/halakhic_authority/commitment_systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '2ea8a69c-a5b5-4324-a91d-994cd1b4e587').
narrative_ontology:cs_kernel_codification('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', fixed_text).
narrative_ontology:cs_authority_grounding('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', lineage).
narrative_ontology:cs_interpretation_layer_present('2ea8a69c-a5b5-4324-a91d-994cd1b4e587').
narrative_ontology:cs_reading_relation('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', foundational, divine_command_unfulfilled_without_action).
narrative_ontology:cs_axiom_status(divine_command_unfulfilled_without_action, holdable).
narrative_ontology:cs_axiom_grounding('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', divine_command_unfulfilled_without_action, deontological).
narrative_ontology:cs_axiom('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', foundational, study_is_preparation_not_substitute).
narrative_ontology:cs_axiom_status(study_is_preparation_not_substitute, holdable).
narrative_ontology:cs_axiom_grounding('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', study_is_preparation_not_substitute, conventional).
narrative_ontology:cs_reference_frame('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', post_temple_halakhic_continuity).
narrative_ontology:cs_drift_state('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2ea8a69c-a5b5-4324-a91d-994cd1b4e587', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, future_generations).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, scholars_of_halakha).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the halakha, maintaining the obligation's binding status while acknowledging its current unperformability. They benefit from the continuity of their interpretive authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Bears the collective burden of the unfulfilled divine command. Engages in study as a means of connection to the tradition, but this reading asserts it does not fulfill the core obligation, creating a state of perpetual non-compliance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, jewish_community, payer,
    organized, generational, identity_locked, global).

% Benefit from the preservation of intricate knowledge and the continuity of the tradition, ensuring that the laws of sacrifice are understood and available for potential future restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).

% Benefit from the intellectual engagement and academic careers built around the study of these complex laws, contributing to the archiving function and maintaining their professional standing.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, scholars_of_halakha, beneficiary,
    moderate, biographical, mobile, global).

% Advocate for immediate action towards Temple restoration and the resumption of sacrifices, viewing the 'study as archiving' reading as insufficient, delaying, or even a theological compromise. Their voices are often marginalized by the established halakhic consensus.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_activists, excluded,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the intricate knowledge of Temple sacrifices and their laws, ensuring the continuity of tradition and readiness for future restoration, preventing the loss of vital religious practice and identity.
% TRANSFER_FUNCTION: Transfers the *burden* of non-compliance with the divine command from the present generation to a future, restored state, while transferring the *responsibility* of knowledge preservation to scholars and the community. It also transfers authority to those who maintain this interpretation.
% ABSENT_VOICES: Messianic activists who believe the obligation should be fulfilled actively now, or those who believe study *does* constitute a legitimate fulfillment of the obligation, are structurally marginalized by this reading's emphasis on non-fulfillment and archiving.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the entire halakhic framework for the post-Temple period would collapse, requiring a radical reinterpretation of divine command and communal responsibility. This would fundamentally alter Jewish religious practice, identity, and the role of rabbinic authority, leading to widespread theological and social reorganization.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the central act of Jewish worship (sacrifices) impossible, creating a profound crisis of religious practice, divine command, and the continuity of Jewish identity.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts (Talmud, Rishonim, Acharonim) and the continuous practice of halakhic study across centuries attest to the problem's origin and ongoing status. The lived experience of the Jewish community in diaspora, without a Temple, corroborates the persistence of this foundational challenge.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the core divine command remains unfulfilled, imposing a theological burden of non-compliance on the community. Suppression is also high (0.75) as halakhic authorities actively enforce this interpretation, marginalizing alternative readings that might claim fulfillment through study or advocate for immediate, potentially premature, restoration. Theater ratio is low (0.20) because the study itself is a genuine, functional activity of knowledge preservation, though the claim of non-fulfillment maintains a theological ideal.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic authorities, this reading is a necessary and responsible adaptation to an impossible situation, ensuring continuity. From the perspective of the Jewish community, it represents a centuries-long state of collective non-compliance with a central divine command, a form of theological extraction. Messianic activists would view it as a failure of will or faith.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities benefit by maintaining their interpretive authority and the continuity of the tradition. Future generations and scholars of halakha benefit from the preservation of knowledge. The divine command itself is a 'victim' as it remains unfulfilled, and the Jewish community bears the burden of this unfulfilled obligation, experiencing it as a form of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Rope (which would imply full coordination and fulfillment) or a pure Snare (which would ignore the genuine coordination function of knowledge preservation). It correctly identifies the dual nature: a vital coordination function (archiving for restoration) intertwined with asymmetric extraction (the burden of unfulfilled obligation maintained by authority).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_validity,
    'Is the ''study as archiving'' reading the most accurate interpretation of the Temple sacrifice obligation in the absence of the Temple, or do sibling readings offer a more complete or less extractive framework?',
    'Theological and halakhic discourse, communal acceptance, and the emergence of new historical or archaeological evidence related to Temple practice or messianic era expectations.',
    'If a sibling reading (e.g., ''study as occupation'') were to gain wider acceptance, the constraint''s extractiveness would decrease, and its classification might shift towards a Rope, as the obligation would be considered fulfilled through study. If ''messianic suspension'' gained dominance, the obligation might be seen as temporarily inert, reducing current extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Ambiguity regarding the definitive interpretation of the Temple sacrifice obligation in the post-Temple era.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (halakhic authority, communal norms) or internalized (identity-locked adherence to tradition, fear of theological deviance)?',
    'Sociological studies of religious adherence and dissent within the Jewish community, examining the consequences of adopting alternative interpretations. If dissent leads to social exclusion or identity crisis, internalized suppression is significant.',
    'If internalized suppression is a major component, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint within their self-concept, making exit or alternative interpretations more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in maintaining halakhic interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 70, 0.1).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 400, 0.12).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 800, 0.15).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(temp_tr_t2024, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 70, 0.6).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 400, 0.62).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 800, 0.63).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1200, 0.64).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(temp_be_t2024, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 70, 0.7).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 400, 0.72).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 800, 0.73).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1200, 0.74).
narrative_ontology:measurement(temp_su_t1600, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(temp_su_t2024, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel. This reading ('study_as_archiving') focuses on preserving knowledge for future restoration without fulfilling the present obligation. It is distinct from 'study_as_occupation' (which claims study *does* fulfill the obligation) and 'messianic_suspension' (which claims the obligation is suspended).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
