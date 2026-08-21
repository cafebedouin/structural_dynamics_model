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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment: Hybrid Preparatory Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid preparatory' reading of the Temple
 *   sacrifice commitment within Halakhic Judaism. In this reading, the study
 *   of sacrificial laws is neither a full substitute for performance nor a
 *   mere archival exercise, but an active, preparatory engagement that
 *   maintains the commitment in a suspended state, awaiting messianic
 *   restoration. This reading justifies the allocation of significant
 *   cognitive and financial resources to the study of non-performable laws,
 *   extracting from the community while benefiting scholars and institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.45).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.6).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment: Hybrid Preparatory Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'e852a2ab-fec3-4aa6-893d-226062cf662b').
narrative_ontology:cs_kernel_codification('e852a2ab-fec3-4aa6-893d-226062cf662b', fixed_text).
narrative_ontology:cs_authority_grounding('e852a2ab-fec3-4aa6-893d-226062cf662b', lineage).
narrative_ontology:cs_interpretation_layer_present('e852a2ab-fec3-4aa6-893d-226062cf662b').
narrative_ontology:cs_reading_relation('e852a2ab-fec3-4aa6-893d-226062cf662b', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('e852a2ab-fec3-4aa6-893d-226062cf662b', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('e852a2ab-fec3-4aa6-893d-226062cf662b', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('e852a2ab-fec3-4aa6-893d-226062cf662b', foundational, halakha_is_eternal_and_unchanging).
narrative_ontology:cs_axiom_status(halakha_is_eternal_and_unchanging, holdable).
narrative_ontology:cs_axiom_grounding('e852a2ab-fec3-4aa6-893d-226062cf662b', halakha_is_eternal_and_unchanging, deontological).
narrative_ontology:cs_axiom('e852a2ab-fec3-4aa6-893d-226062cf662b', foundational, study_is_a_form_of_spiritual_preparation).
narrative_ontology:cs_axiom_status(study_is_a_form_of_spiritual_preparation, holdable).
narrative_ontology:cs_axiom_grounding('e852a2ab-fec3-4aa6-893d-226062cf662b', study_is_a_form_of_spiritual_preparation, theological).
narrative_ontology:cs_reference_frame('e852a2ab-fec3-4aa6-893d-226062cf662b', post_temple_destruction_halakhic_continuity).
narrative_ontology:cs_drift_state('e852a2ab-fec3-4aa6-893d-226062cf662b', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e852a2ab-fec3-4aa6-893d-226062cf662b', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_donors).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, lay_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakhic tradition, including laws of temple sacrifice. They benefit from the intellectual and institutional resources dedicated to maintaining the commitment through study, which validates their scholarly role and provides career paths. Their identity is deeply fused with this interpretive tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive funding and students for the study of all Jewish law, including the non-performable laws of sacrifice. The hybrid-preparatory reading provides a strong justification for this ongoing resource allocation, maintaining their institutional relevance and financial stability.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary,
    organized, generational, constrained, regional).

% Provide financial support to yeshivas and scholars, often with the understanding that this study contributes to the spiritual well-being and future restoration of the community. They bear the financial cost of maintaining the commitment in its suspended state, with an uncertain return on investment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_donors, payer,
    moderate, biographical, constrained, local).

% Are encouraged to engage in study and prayer related to the Temple, contributing their time and cognitive resources. They derive spiritual benefit and a sense of continuity with tradition, but also bear the cognitive load of maintaining a commitment to a practice that is currently impossible to perform, with no immediate material benefit. Their identity is often tied to this communal commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, lay_adherents, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, lay_adherents, beneficiary).

% Advocate for immediate, material preparation for Temple reconstruction and the resumption of sacrifices, often viewing extensive theoretical study as insufficient or even delaying the messianic era. Their more direct, action-oriented approach is often marginalized by the rabbinic establishment that prioritizes study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_activists, excluded,
    moderate, immediate, constrained, national).

% Analyze the historical evolution of Jewish law and practice, including the shift from Temple-centered worship to rabbinic Judaism. They observe the mechanisms by which commitment to a defunct practice is maintained, without participating in the normative framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a collective commitment to the future restoration of Temple sacrifice, ensuring the knowledge and spiritual readiness for its resumption are preserved across generations, preventing the tradition from becoming a mere historical archive.
% TRANSFER_FUNCTION: Transfers cognitive and financial resources from the broader community (donors, lay adherents) to rabbinic scholars and yeshiva institutions, in exchange for the maintenance and transmission of the sacrificial laws as a live, albeit suspended, commitment.
% ABSENT_VOICES: Messianic activists who prioritize immediate material action over extensive theoretical study are often excluded from the mainstream discourse, as are those who view the commitment as having been symbolically transformed rather than merely suspended. They would argue for a more direct or reinterpreted engagement with the divine command.
% DISAPPEARANCE_RATIONALE: If the hybrid-preparatory commitment vanished, the vast institutional infrastructure of yeshivas and scholarly networks dedicated to this study would lose a significant part of its justification and funding. The collective identity and future orientation of many adherents would be profoundly altered, leading to a re-evaluation of religious priorities and resource allocation.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the Jewish people faced the challenge of maintaining their covenantal relationship with God and the integrity of the Torah's sacrificial laws, despite the inability to perform them.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and the majority of Orthodox Jewish communities attest that the problem of maintaining the sacrificial laws in a state of suspension, awaiting messianic restoration, remains live. This is corroborated by the ongoing theological and halakhic discourse, and the continued dedication of resources to this area of study, which is not contested by external observers as a 'solved' problem.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because resources are diverted to study a practice that cannot be performed, with an uncertain future benefit. Suppression (0.6) is present as alternative interpretations (e.g., immediate action, symbolic transformation) are marginalized by the dominant rabbinic discourse. Theater ratio is low (0.1) because the study is genuinely seen as a vital, active form of commitment, not a mere performance. The claimed type is Tangled Rope because it serves a genuine coordination function (preserving tradition) but involves asymmetric extraction (resources flow to scholars/institutions from the community for this suspended commitment).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars, this is a vital Rope, ensuring the continuity of tradition. From the perspective of community donors, it is a Tangled Rope, where their contributions maintain a system that primarily benefits the scholarly class, with a deferred and uncertain collective benefit. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and yeshiva institutions are beneficiaries and agenda-setters, as they receive resources and define the terms of the commitment's maintenance. Community donors and lay adherents are payers, contributing financially and cognitively without direct control or immediate material return. Messianic activists are excluded, as their preferred mode of engagement (direct action) is suppressed by the emphasis on study.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_justification,
    'Is the current level of resource allocation to the study of sacrificial laws justified by the ''preparatory'' function, or does it exceed what is necessary for mere preservation?',
    'Comparative analysis of resource allocation in other traditions for suspended or future-oriented practices, or internal communal debate leading to a re-evaluation of priorities.',
    'If the allocation is found to be excessive, the extractiveness of this constraint would be re-evaluated upward, potentially shifting its classification towards a Snare for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_justification, preference, 'Whether resource allocation aligns with the stated preparatory goal.').

omega_variable(
    messianic_timing_uncertainty,
    'How does the irreducible uncertainty of messianic timing affect the perceived value and extractiveness of a ''preparatory'' commitment?',
    'Theological consensus on the imminence or deferral of the messianic era, or a shift in communal eschatological expectations.',
    'If the messianic era is perceived as indefinitely distant, the ''preparatory'' aspect might be seen as less urgent, increasing the perceived extractiveness of ongoing resource commitments. If imminent, extractiveness might be seen as justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timing_uncertainty, conceptual, 'Impact of messianic timing on the constraint''s perceived value.').

omega_variable(
    alternative_engagement_suppression,
    'To what extent does the emphasis on ''hybrid preparatory'' study actively suppress or marginalize alternative forms of engagement with the Temple commitment (e.g., direct action, symbolic reinterpretation)?',
    'Sociological study of communal discourse and institutional funding patterns, or a shift in rabbinic leadership to explicitly endorse a wider range of engagement modes.',
    'If suppression of alternatives is found to be high, the constraint''s overall suppression metric would be re-evaluated upward, reinforcing its Tangled Rope or Snare classification for excluded parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_engagement_suppression, empirical, 'Degree to which alternative forms of commitment are suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.09).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.09).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.1).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 80, 0.1).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(temp_su_t60, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(temp_su_t80, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'temple_sacrifice_commitment' kernel. Its extractiveness and beneficiary structure differ significantly from other readings, necessitating separate constraint stories. This reading (hybrid_preparatory) influences the resource allocation and legitimacy conditions for the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
