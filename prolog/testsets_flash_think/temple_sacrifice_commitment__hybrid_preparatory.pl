% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment (Hybrid Preparatory Reading)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid preparatory' reading of the
 *   commitment to Temple sacrifice within Jewish tradition. It posits that
 *   while material sacrifices are currently impossible, the study of their
 *   laws is a vital preparatory exercise for their messianic restoration,
 *   maintaining the commitment in a suspended state. This is distinct from
 *   viewing study as a replacement for sacrifice or as mere archival work.
 *   The constraint extracts cognitive and financial resources for a deferred,
 *   uncertain future benefit, making it a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.58).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.25).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment (Hybrid Preparatory Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'af7cb1db-b922-4e10-8dab-2e2aa4616b9a').
narrative_ontology:cs_kernel_codification('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', fixed_text).
narrative_ontology:cs_authority_grounding('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', lineage).
narrative_ontology:cs_interpretation_layer_present('af7cb1db-b922-4e10-8dab-2e2aa4616b9a').
narrative_ontology:cs_reading_relation('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', foundational, material_sacrifice_is_deferred_command).
narrative_ontology:cs_axiom_status(material_sacrifice_is_deferred_command, holdable).
narrative_ontology:cs_axiom_grounding('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', material_sacrifice_is_deferred_command, deontological).
narrative_ontology:cs_axiom('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', foundational, study_prepares_for_future_performance).
narrative_ontology:cs_axiom_status(study_prepares_for_future_performance, holdable).
narrative_ontology:cs_axiom_grounding('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', study_prepares_for_future_performance, theological).
narrative_ontology:cs_reference_frame('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', post_temple_destruction_halakhic_continuity).
narrative_ontology:cs_drift_state('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', contemporary_era_of_indefinite_deferral, gap(stable, minor, true)).
narrative_ontology:cs_created_at('af7cb1db-b922-4e10-8dab-2e2aa4616b9a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, future_messianic_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_members).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, community_members).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, halakhic_continuity_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_eschatology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakhic tradition, defining the parameters of preparatory study and its significance. They gain status, purpose, and intellectual engagement from maintaining the commitment in this suspended state. Their professional and spiritual identity is deeply fused with this role.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Provide financial and social support for institutions of Torah study, including those focused on sacrifice law. They benefit from the spiritual continuity and the hope of messianic restoration, but bear the cost of diverting resources from other communal needs. Exit means abandoning a core communal identity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_members, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, community_members, beneficiary).

% Dedicate significant cognitive resources and time to the study of sacrifice law, often at the expense of secular pursuits or other forms of religious observance. They gain spiritual merit, deep knowledge, and a sense of participation in the messianic project. Their identity is often deeply intertwined with this scholarly pursuit.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, students_of_halakha, beneficiary).

% The ultimate beneficiary of the preparatory work, as the commitment ensures the knowledge and readiness for the restoration of the Temple and its services. This is a conceptual, future-oriented beneficiary.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, future_messianic_community, beneficiary,
    analytical, civilizational, analytical, universal).

% View the practice from an external, non-participatory perspective, analyzing its social, cultural, or historical implications without internalizing its religious claims or obligations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing commitment of the Jewish people to the future restoration of the Temple and its sacrificial service, ensuring the preservation of knowledge and readiness across generations despite current non-performability.
% TRANSFER_FUNCTION: Transfers intellectual, spiritual, and financial resources from the present community to the maintenance of a deferred religious practice, ensuring its continuity and future viability.
% ABSENT_VOICES: Those who believe the sacrificial system is entirely defunct or has been permanently replaced by prayer and ethical deeds (e.g., some Reform Jewish perspectives), or those who prioritize immediate social action over future-oriented ritual preparation. They are excluded by the traditional framework's emphasis on halakhic continuity.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, a core pillar of traditional Jewish eschatology and identity would collapse. The intellectual and spiritual life of many communities would be profoundly altered, and the sense of historical continuity and future hope would be severely diminished.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE, which rendered the central commands of the Torah regarding sacrificial worship impossible to perform, creating a profound crisis of religious practice and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing physical absence of the Temple, the continued study of sacrifice laws in traditional academies, and the daily prayers for its restoration attest to the founding problem's live status. Historical texts and theological discourse from across the tradition corroborate this, not just benefiting parties.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.58, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because significant resources (time, intellectual effort, financial support) are dedicated to studying laws that cannot currently be performed, with the benefit being future-oriented and contingent on messianic arrival. Suppression is low (0.25) as adherence is primarily driven by internal religious commitment and social tradition, not overt coercion. Theater ratio is low (0.15) because the study is genuinely understood as preparatory and purposeful within this reading, not merely performative maintenance of a defunct practice. Accessibility collapse is moderate (0.45) as alternative forms of religious engagement exist, but exiting this specific commitment means departing from a core traditional framework. Resistance is low (0.1) due to the internal nature of the commitment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and deeply committed students, this is a vital, meaningful practice. From an external or less committed perspective, the diversion of resources to non-performable laws might appear less efficient or more extractive. The engine's classification captures this tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars are agenda-setters and beneficiaries, gaining purpose and status from leading this interpretive framework. Community members and students of Halakha are payers, contributing resources and time, but also beneficiaries of spiritual continuity and future hope. The future messianic community is a conceptual beneficiary. Adherence is largely identity-locked for those deeply embedded in the tradition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timing_uncertainty,
    'How does the indefinite deferral of the messianic era impact the perceived extractiveness and justification of preparatory study?',
    'Theological developments or shifts in communal eschatological expectations; empirical observation of resource allocation changes in response to perceived messianic proximity.',
    'If the messianic era is perceived as increasingly distant or uncertain, the extractiveness of diverting resources to preparatory study might be re-evaluated as higher, potentially shifting the constraint towards a Piton if the function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timing_uncertainty, conceptual, 'Uncertainty regarding the timing of messianic restoration and its impact on the constraint''s perceived utility.').

omega_variable(
    resource_diversion_justification,
    'Is the allocation of significant communal and individual resources to preparatory study optimally justified compared to other religious or social priorities?',
    'Internal communal debate, external economic analysis of resource allocation within religious institutions, or shifts in communal values.',
    'If the justification for resource diversion weakens, the ''payer'' seats'' directionality could shift further towards ''target'', increasing effective extraction and potentially reclassifying to a Snare if the coordination function is deemed insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_justification, preference, 'Whether the resource allocation for preparatory study is justified against alternative uses.').

omega_variable(
    reading_distinction_clarity,
    'How clearly is the ''hybrid preparatory'' reading distinguished from the ''study as exercise'' and ''performance only'' readings in actual communal practice and understanding?',
    'Sociological studies of religious practice, qualitative analysis of sermons and educational curricula, or formal theological pronouncements clarifying the distinctions.',
    'If the distinctions blur, the classification could shift. If it collapses into ''study as exercise'', extractiveness might decrease (study is performance). If it collapses into ''performance only'' (archiving), extractiveness might increase (less perceived utility).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distinction_clarity, empirical, 'Ambiguity in the practical and conceptual distinction between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.15).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 10, 0.15).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.15).
narrative_ontology:measurement(temp_tr_t30, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 30, 0.15).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.15).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(temp_be_t30, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(temp_su_t10, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(temp_su_t30, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, halakhic_legal_system).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, jewish_identity_formation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'temple_sacrifice_commitment' kernel, focusing on the 'hybrid preparatory' interpretation. Sibling readings (study_as_exercise, performance_only, symbolic_transformation) represent alternative structural claims about the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
