% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents a specific halakhic (Jewish legal) reading of
 *   the obligation to offer sacrifices, holding that the obligation is
 *   divinely suspended, not transformed or fulfilled by other means, until
 *   the messianic restoration. Study of the sacrificial laws is understood as
 *   maintaining operational readiness for that future time. The constraint is
 *   classified as a Rope due to its coordination function in providing a
 *   stable framework for religious life and study, with minimal extraction,
 *   as the primary obligation is in abeyance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '892a7344-2b7f-4097-977f-acdee6a071ee').
narrative_ontology:cs_kernel_codification('892a7344-2b7f-4097-977f-acdee6a071ee', fixed_text).
narrative_ontology:cs_authority_grounding('892a7344-2b7f-4097-977f-acdee6a071ee', lineage).
narrative_ontology:cs_interpretation_layer_present('892a7344-2b7f-4097-977f-acdee6a071ee').
narrative_ontology:cs_reading_relation('892a7344-2b7f-4097-977f-acdee6a071ee', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('892a7344-2b7f-4097-977f-acdee6a071ee', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('892a7344-2b7f-4097-977f-acdee6a071ee', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('892a7344-2b7f-4097-977f-acdee6a071ee', foundational, divine_suspension_of_mitzvah).
narrative_ontology:cs_axiom_status(divine_suspension_of_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('892a7344-2b7f-4097-977f-acdee6a071ee', divine_suspension_of_mitzvah, deontological).
narrative_ontology:cs_axiom('892a7344-2b7f-4097-977f-acdee6a071ee', foundational, study_as_operational_readiness).
narrative_ontology:cs_axiom_status(study_as_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('892a7344-2b7f-4097-977f-acdee6a071ee', study_as_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('892a7344-2b7f-4097-977f-acdee6a071ee', divine_command_abeyance).
narrative_ontology:cs_drift_state('892a7344-2b7f-4097-977f-acdee6a071ee', contemporary_halakhic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('892a7344-2b7f-4097-977f-acdee6a071ee', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakha (Jewish law), maintaining the understanding that sacrifice is suspended until the messianic era. Their role is central to preserving the knowledge and practice for future restoration, elevating the importance of study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to the halakhic ruling of suspension, engaging in extensive study of the sacrificial laws as a form of spiritual engagement and preparation. They benefit from a clear framework for religious life in the absence of the Temple, but bear the 'cost' of deferred ritual and intellectual effort.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews, payer).

% Are the ultimate beneficiaries of the preserved knowledge and operational readiness, ensuring that the sacrificial system can be reinstituted correctly upon messianic restoration. They have no agency in the current interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, global).

% Certain groups who advocate for immediate action towards messianic restoration, potentially including attempts to reinstitute sacrifices, find their views marginalized or foreclosed by this reading's emphasis on divine suspension and patient study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_activists, excluded,
    moderate, immediate, constrained, local).

% Study the halakhic tradition and its evolution as a cultural and historical phenomenon, without necessarily adhering to its religious claims. They observe the constraint's persistence and its impact on Jewish life.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, secular_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious practice and intellectual focus of the Jewish community during the period of Temple destruction, providing a framework for maintaining the integrity of the sacrificial laws and preparing for their future restoration.
% TRANSFER_FUNCTION: Transfers communal religious energy and intellectual effort from the physical performance of sacrifices (currently impossible) to the diligent study and theoretical understanding of those laws, preserving knowledge for a future time.
% ABSENT_VOICES: Messianic activists who believe in immediate, active performance of sacrifices, or those who view the laws as purely symbolic/historical, are excluded. They would argue for different forms of engagement with the mitzvah, but their interpretations are foreclosed by this reading's core premise.
% DISAPPEARANCE_RATIONALE: If this understanding of divine suspension and the role of study vanished, the entire framework of observant Jewish life would be radically re-evaluated. The focus of prayer, study, and communal aspiration would shift dramatically, leading to profound theological and practical reorganization.
% FOUNDING_PROBLEM: How to maintain the divine covenant and the integrity of the sacrificial system's laws after the destruction of the Second Temple, when physical performance of the mitzvah became impossible.
% FOUNDING_PROBLEM_CORROBORATION: The problem and its status are attested by centuries of rabbinic literature, communal prayer, and educational curricula across diverse Jewish communities, not solely by those who benefit from the current interpretive framework. The continued absence of the Temple and the messianic era keeps the problem actively relevant.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.1) because the core obligation is suspended, meaning no party is actively 'paying' for non-performance in a punitive sense. Suppression is low (0.15) as this reading is widely accepted within Orthodox Judaism, and there's no active enforcement against alternative interpretations beyond social/communal pressure. Theater ratio is very low (0.05) because the study of these laws is genuinely functional for preserving knowledge and maintaining spiritual connection, not merely performative. Accessibility collapse is high (0.9) because the physical performance of sacrifices is genuinely impossible without the Temple. Resistance is low (0.1) as this interpretation is foundational for many observant communities.
 *
 * PERSPECTIVAL GAP:
 *   While the constraint is generally accepted, different seats might perceive the 'cost' of deferred ritual differently. For scholars, the focus on study is a benefit; for some lay adherents, the inability to perform sacrifices might be a spiritual longing. However, the structural classification remains Rope due to the overall coordination and low extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are agenda-setters and beneficiaries, as their role in interpreting and transmitting this framework is central. Observant Jews are beneficiaries (spiritual continuity) and payers (intellectual effort). Future generations are beneficiaries of preserved knowledge. Messianic activists are excluded, as their views on immediate restoration are foreclosed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_practical_suspension,
    'Is the suspension of sacrifice obligation primarily a divine decree, or a practical consequence of the Temple''s destruction?',
    'Theological analysis of rabbinic texts and historical precedents. If the suspension is framed as purely practical, it might open avenues for human-initiated reinstitution.',
    'If purely practical, the constraint''s ''emerges_naturally'' aspect (in a theological sense) would be weaker, potentially increasing perceived extractiveness for those who desire performance. If divine, the current low extractiveness is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_practical_suspension, conceptual, 'Ambiguity regarding the nature of the sacrifice obligation''s suspension.').

omega_variable(
    study_instrumental_vs_substitutive,
    'Is the study of sacrificial laws purely instrumental (maintaining readiness), or does it carry a substitutive spiritual value that partially fulfills the mitzvah?',
    'Further theological and halakhic discourse within the tradition. The ''study_as_exercise_reading'' sibling directly addresses this.',
    'If study is seen as partially substitutive, the perceived ''cost'' of deferred ritual for observant Jews would decrease, potentially lowering effective extraction. If purely instrumental, the longing for actual performance remains a ''cost''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_instrumental_vs_substitutive, conceptual, 'The precise spiritual status and efficacy of studying sacrificial laws during suspension.').

omega_variable(
    messianic_timeline_ambiguity,
    'How does the ambiguity of the messianic timeline (when will restoration occur?) affect the perceived urgency and nature of ''operational readiness''?',
    'No direct resolution; ongoing theological and communal reflection. Different communities may emphasize different aspects of readiness based on their messianic expectations.',
    'A perception of imminent restoration might increase the ''resistance'' to the current suspended state from some groups, while a distant timeline reinforces the stability of the current framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timeline_ambiguity, empirical, 'Impact of messianic timeline uncertainty on the constraint''s dynamics.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''sacrifice_obligation_kernel'', or could its core tenets be integrated into a broader, more encompassing interpretation?',
    'Comparative analysis with sibling readings and their foundational axioms. The ''forecloses'' relations suggest strong distinctness.',
    'If it''s a distinct reading, its classification stands. If it could be integrated, the broader kernel might compute as a different type, reflecting a more complex, possibly extractive, underlying structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one specific reading of the ''sacrifice_obligation_kernel''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t1920, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(sacr_tr_t1945, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(sacr_tr_t1970, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(sacr_tr_t1995, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(sacr_tr_t2020, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t1920, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1920, 0.1).
narrative_ontology:measurement(sacr_be_t1945, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(sacr_be_t1970, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(sacr_be_t1995, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1995, 0.1).
narrative_ontology:measurement(sacr_be_t2020, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2020, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t1920, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1920, 0.15).
narrative_ontology:measurement(sacr_su_t1945, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(sacr_su_t1970, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(sacr_su_t1995, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(sacr_su_t2020, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2020, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_kernel', which also includes 'performance_only_reading', 'study_as_exercise_reading', and 'symbolic_archive_reading'. Each reading instantiates a distinct constraint with its own ε and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
