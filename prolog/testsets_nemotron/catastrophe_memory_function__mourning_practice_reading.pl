% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av Mourning-Practice Boundary Maintenance
 *   domain: religious/ritual/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the mourning_practice_reading of the
 *   catastrophe_memory_function kernel: Tisha B'Av as a commemorative ritual
 *   that maintains Jewish group identity through obligatory mourning-practice
 *   and boundary-norms (D1/D4). The ritual coordinates communal identity by
 *   requiring annual enactment of destruction-memory — fasting, liturgical
 *   lamentation (kinot), restrictions on joy — which functions as a
 *   boundary-maintenance mechanism distinguishing the observant community
 *   from surrounding cultures. No survival-competence transmission (D5) is
 *   claimed; the reading holds that the ritual's coordination function is
 *   purely identity-maintenance through memorial obligation. The constraint
 *   is claimed as a rope: genuine coordination with minimal coercive
 *   overhead, participants as net beneficiaries, alternatives not suppressed.
 *
 * KEY AGENTS:
 *   - observant_community_members: Primary beneficiaries (organized/mobile) — enact mourning-practice, receive identity-coordination
 *   - rabbinic_authorities: Agenda-setters (institutional/biographical) — define observance parameters, authorize liturgy
 *   - collective_identity_structure: Non-agent beneficiary (analytical) — the group boundary itself is maintained
 *   - non_observant_jews: Excluded (moderate/constrained) — would object to boundary rigidity but not in the conversation
 *   - external_cultures: Observers (powerful/arbitrage) — adjacent societies the boundary distinguishes from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av Mourning-Practice Boundary Maintenance").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious/ritual/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'af8053cd-9c09-4f43-8c22-964051ddc126').
narrative_ontology:cs_kernel_codification('af8053cd-9c09-4f43-8c22-964051ddc126', fixed_text).
narrative_ontology:cs_authority_grounding('af8053cd-9c09-4f43-8c22-964051ddc126', lineage).
narrative_ontology:cs_interpretation_layer_present('af8053cd-9c09-4f43-8c22-964051ddc126').
narrative_ontology:cs_reading_relation('af8053cd-9c09-4f43-8c22-964051ddc126', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('af8053cd-9c09-4f43-8c22-964051ddc126', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('af8053cd-9c09-4f43-8c22-964051ddc126', foundational, mourning_practice_constitutes_boundary).
narrative_ontology:cs_axiom_status(mourning_practice_constitutes_boundary, holdable).
narrative_ontology:cs_axiom_grounding('af8053cd-9c09-4f43-8c22-964051ddc126', mourning_practice_constitutes_boundary, deontological).
narrative_ontology:cs_axiom('af8053cd-9c09-4f43-8c22-964051ddc126', foundational, survival_competence_not_transmitted).
narrative_ontology:cs_axiom_status(survival_competence_not_transmitted, holdable).
narrative_ontology:cs_axiom_grounding('af8053cd-9c09-4f43-8c22-964051ddc126', survival_competence_not_transmitted, conventional).
narrative_ontology:cs_reference_frame('af8053cd-9c09-4f43-8c22-964051ddc126', post_churban_rabbinic_framework).
narrative_ontology:cs_drift_state('af8053cd-9c09-4f43-8c22-964051ddc126', contemporary_diaspora_observance, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('af8053cd-9c09-4f43-8c22-964051ddc126', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, collective_identity_structure).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, boundary_maintenance_through_ritual).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, communal_identity_via_commemorative_obligation).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, mourning_practice_as_group_cohesion_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact the annual mourning-practice (fasting, kinot recitation, behavioral restrictions) as a constitutive identity performance. Receive communal belonging, historical continuity, and boundary-delineation from participation. Exit (non-observance) is socially possible but carries identity-cost: one ceases to be 'observant' in the communal sense.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_community_members, beneficiary,
    organized, generational, mobile, global).

% Define the halakhic parameters of observance (fast details, liturgical text, prohibition scope), authorize kinot compositions, and adjudicate boundary-cases. Their authority derives from the tradition's chain of transmission; they do not materially extract from the ritual. Exit would mean leaving the rabbinic role — structurally constrained by vocational identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities, agenda_setter,
    institutional, biographical, constrained, global).

% The group boundary itself — the distinction between 'observant community' and 'outside' — is maintained by the ritual's annual enactment. This is not an actor but a structural entity that benefits from the constraint's operation. Listed as a vindicated proposition in base_properties; included here as a non-agent stakeholder for completeness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, collective_identity_structure, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__mourning_practice_reading, collective_identity_structure).

% Jews who do not observe Tisha B'Av but are categorized by the boundary the ritual maintains. They experience the constraint as exclusionary — the ritual defines a communal boundary they fall outside of. They would object to the boundary's rigidity if present in the conversation, but the ritual's coordination function does not include them. Exit from the boundary's definition is not available — it is imposed by the observant community's practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, non_observant_jews, excluded,
    moderate, biographical, constrained, global).

% Surrounding societies (Christian, Muslim, secular) against which the Jewish boundary is drawn. The ritual's boundary-maintenance function partly serves to distinguish the community from these external cultures. They are analytical observers of the constraint's operation; their power is structural (demographic, political) but not exercised through this constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, external_cultures, observer,
    powerful, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish group identity and communal boundary across diaspora and historical rupture through an annual obligatory mourning-practice that enacts destruction-memory. The ritual solves the coordination problem: how does a dispersed people maintain a shared identity without sovereign institutions? Answer: synchronized commemorative enactment.
% TRANSFER_FUNCTION: Moves time, attention, and emotional labor from individual community members into the collective identity structure via the annual ritual enactment. No material transfer; the 'payment' is participatory obligation, the 'gain' is boundary-maintenance.
% ABSENT_VOICES: Non-observant Jews (secular, Reform, cultural) who experience the boundary as exclusionary but are not participants in the ritual conversation. Also early maskilim (Haskalah proponents) who historically objected to the ritual's 'exile mentality' — their voices were excluded from the halakhic conversation that defined the practice.
% DISAPPEARANCE_RATIONALE: If Tisha B'Av observance vanished overnight, the observant community would lose its primary annual synchronized enactment of destruction-memory and boundary-maintenance. Alternative identity-coordination mechanisms exist (Shabbat, kashrut, prayer) but none carries the specific historical-memory function. The communal boundary would likely become more porous; identity-transmission to next generations would degrade.
% FOUNDING_PROBLEM: Post-70 CE: how to maintain Jewish peoplehood and covenantal identity without Temple, sovereignty, or geographic center? The ritual was built to solve the coordination problem of diaspora identity-maintenance through commemorative obligation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Josephus (War 6.4-6), rabbinic literature (Mishnah Ta'anit 4:6, BT Ta'anit 29a), and modern historians (Seth Schwartz, 'The Ancient Jews from Alexander to Muhammad') — sources outside the current benefiting parties. The problem (diaspora identity without sovereign institutions) remains live per contemporary Jewish demography (Pew 2013, 2020 studies showing observance correlates with identity-retention).
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the primary transfer is time/attention to ritual enactment, not material extraction; the coordination benefit (identity maintenance) is reciprocally valued by participants. Suppression is moderate (0.42) because boundary-maintenance requires some social pressure to sustain observance norms, but exit (non-observance) is socially possible — no legal coercion. Theater is low (0.18): the ritual's performative aspects serve the coordination function directly. Accessibility collapse (0.55) reflects that alternatives to this specific ritual for identity-maintenance are limited within the tradition, but not zero (secular Jewish identity exists). Resistance (0.35) reflects historical reform/rejection movements but not systemic overthrow. The interval spans ~2000 years (Second Temple destruction to present) with measurements at ~500-year increments capturing major historical inflections.
 *
 * PERSPECTIVAL GAP:
 *   From the observant member's seat, the constraint is experienced as meaningful coordination (d ~ 0.3) — the obligation is accepted as identity-constituting. From the rabbinic authority seat, it is agenda-setting with low extraction (d ~ 0.2). From the non-observant Jewish seat, the boundary feels extractive/exclusionary (d ~ 0.7) — the constraint defines them out. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Observant members are beneficiaries: they receive identity-coordination and communal belonging through the ritual. Rabbinic authorities are agenda-setters: they define the practice but do not materially extract from it. The collective_identity_structure is a non-agent vindicated proposition — the boundary itself benefits from maintenance. Non-observant Jews are excluded: they bear the boundary's exclusion without participating in its coordination. No victims are declared because exit (non-observance) is socially possible and the ritual does not coercively extract from non-participants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-destruction identity maintenance without Temple/sovereignty) remains live — the constraint has not outlived its function. The mandate (commemorative obligation) matches the current coordination need (boundary-maintenance in diaspora). No mandatrophy: the arrangement persists because the problem it solves persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine structural feature of the Tisha B''Av observance (a rope coordinating communal identity through mourning-practice), or is it one reading of a contested kernel that also supports survival-competence and hybrid-transformation readings?',
    'Comparative structural analysis of the sibling readings (survival_competence_reading, hybrid_transformation_reading) against the same observance data; if all three produce stable but distinct ε values and beneficiary/victim structures, the kernel is genuinely multi-reading and each reading is a separate constraint.',
    'If multi-reading, this constraint''s classification applies only to the mourning-practice reading; the kernel as a whole would require a family of linked constraint stories. If single-reading, this constraint describes the whole phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether catastrophe_memory_function is a single constraint or a kernel with multiple readings.').

omega_variable(
    boundary_maintenance_naturalness,
    'Does the boundary-maintenance function emerge naturally from the ritual''s structure, or is it actively maintained by rabbinic authority and communal pressure?',
    'Historical analysis of Tisha B''Av observance during periods of weakened rabbinic authority (e.g., early Haskalah, Soviet suppression); if boundary maintenance persists without enforcement, naturalness is supported.',
    'If emergent, strengthens rope classification; if authority-dependent, introduces extraction risk from enforcement apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_naturalness, empirical, 'Naturalness of the boundary-maintenance coordination function.').

omega_variable(
    survival_competence_absence,
    'Does the mourning_practice_reading genuinely lack survival-competence transmission (D5), or is D5 implicitly embedded in the boundary-maintenance structure such that this reading''s ''pure D1/D4'' claim is a framing choice?',
    'Content analysis of kinot (lamentations) and liturgical texts for adaptive-mechanism encoding; practitioner interviews on whether observance transmits practical resilience skills.',
    'If D5 is embedded, the reading forecloses survival_competence_reading structurally (not just conceptually); if genuinely absent, the readings coexist as distinct structural slices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_absence, conceptual, 'Whether survival-competence transmission is structurally absent or implicitly present in the mourning-practice reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmf_mpr_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cmf_mpr_tr_t500, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 500, 0.14).
narrative_ontology:measurement(cmf_mpr_tr_t1000, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1000, 0.16).
narrative_ontology:measurement(cmf_mpr_tr_t1500, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(cmf_mpr_tr_t2000, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 2000, 0.18).

% Extraction over time
narrative_ontology:measurement(cmf_mpr_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cmf_mpr_be_t500, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 500, 0.24).
narrative_ontology:measurement(cmf_mpr_be_t1000, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1000, 0.26).
narrative_ontology:measurement(cmf_mpr_be_t1500, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement(cmf_mpr_be_t2000, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 2000, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cmf_mpr_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cmf_mpr_su_t500, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 500, 0.38).
narrative_ontology:measurement(cmf_mpr_su_t1000, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1000, 0.42).
narrative_ontology:measurement(cmf_mpr_su_t1500, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 1500, 0.4).
narrative_ontology:measurement(cmf_mpr_su_t2000, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 2000, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_function kernel. The mourning_practice_reading claims pure D1/D4 (boundary-maintenance through mourning-practice); survival_competence_reading claims pure D5 (adaptive capacity transmission); hybrid_transformation_reading claims D1/D4 + D5. They share the same observance (Tisha B'Av) but differ in which structural functions they attribute to it, producing different ε, beneficiary structures, and classifications. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, organized, 0.3).
constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, institutional, 0.2).
constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
