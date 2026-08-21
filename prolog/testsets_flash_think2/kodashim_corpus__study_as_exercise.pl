% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Study of Kodashim as Mitzvah Fulfillment
 *   domain: religious/theological/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the 'study_as_exercise' reading of the
 *   'kodashim_corpus' kernel, which posits that continuous
 *   intellectual-spiritual engagement with the laws of sacrifice (Kodashim)
 *   is itself the performance of the mitzvah (divine commandment). This
 *   reading emerged as a foundational response to the destruction of the
 *   Second Temple, providing a means for the Jewish community to maintain its
 *   connection to the sacrificial tradition in the absence of physical
 *   performance. It is claimed as a Rope, reflecting its genuine coordination
 *   function around shared interpretive practice and spiritual fulfillment.
 *
 * KEY AGENTS:
 *   - Talmudic Scholars: Primary agenda-setters and beneficiaries (institutional/identity_locked)
 *   - Jewish Community: Beneficiary (organized/constrained)
 *   - Lay Students: Beneficiary (moderate/mobile)
 *   - Messianic Restorationists: Excluded (organized/identity_locked)
 *   - Analytical Theologians: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.03).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study of Kodashim as Mitzvah Fulfillment").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/theological/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, 'e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c').
narrative_ontology:cs_kernel_codification('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', fixed_text).
narrative_ontology:cs_authority_grounding('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', lineage).
narrative_ontology:cs_interpretation_layer_present('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c').
narrative_ontology:cs_reading_relation('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', foundational, intellectual_engagement_as_mitzvah_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_as_mitzvah_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', intellectual_engagement_as_mitzvah_fulfillment, theological).
narrative_ontology:cs_reference_frame('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', post_temple_rabbinic_synthesis).
narrative_ontology:cs_drift_state('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', contemporary_jewish_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e7ff9f89-9e3e-45e9-82bd-2b7f0c3a8a9c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, talmudic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_community).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, lay_students).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_as_spiritual_practice).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, intellectual_engagement_as_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and perpetuators of this reading, they define the parameters of study and its spiritual significance. They derive immense spiritual fulfillment and communal respect from this role, with their professional and personal identities deeply intertwined with the practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, talmudic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefits from the spiritual continuity and cosmic order believed to be maintained through the study of Kodashim. This practice provides a vital connection to the sacrificial tradition in the absence of the Temple, fostering a sense of collective identity and purpose.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_community, beneficiary,
    organized, generational, constrained, global).

% Individuals who engage in the study of Kodashim for personal spiritual growth and fulfillment. While they may not be institutional leaders, their participation is crucial for the widespread adoption and perpetuation of this reading. They are free to choose their level of engagement.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, lay_students, beneficiary,
    moderate, biographical, mobile, local).

% Adherents of the 'performance_only' reading, they believe that only the physical performance of sacrifices, awaiting the rebuilding of the Temple, constitutes true fulfillment of the mitzvah. They are structurally excluded from the interpretive framework that elevates study to this status, viewing it as a temporary substitute rather than a complete spiritual act.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, messianic_restorationists, excluded,
    organized, civilizational, identity_locked, global).

% Scholars who analyze the theological and historical development of this interpretive tradition, without necessarily being adherents. They provide critical and comparative perspectives on its function and evolution within Jewish thought.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, analytical_theologians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual engagement of the Jewish people with the divine, ensuring the continuity of the sacrificial tradition through intellectual means in the absence of the Temple, fostering shared interpretive practice.
% TRANSFER_FUNCTION: Transfers spiritual merit, cosmic order, and communal identity from the intellectual engagement of scholars and students to the entire community and the world, maintaining a living connection to the divine commandments.
% ABSENT_VOICES: Messianic restorationists, who believe only physical sacrifice is true fulfillment, are structurally excluded from the interpretive framework that elevates study to this status. They would argue that study is a placeholder, not a complete spiritual act.
% DISAPPEARANCE_RATIONALE: If the belief that study fulfills the mitzvah vanished, a central pillar of post-Temple Judaism would collapse. The primary means of engaging with the sacrificial laws would be lost, leading to a profound spiritual and communal crisis and a reordering of religious priorities.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE and the cessation of physical sacrifices, which left a profound void in Jewish religious practice and challenged the community's ability to fulfill the mitzvah of sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature (Talmud, Midrash) and centuries of continuous Jewish practice attest to this interpretation as a direct and enduring response to the Temple's destruction. Historical accounts of Jewish life in exile corroborate the necessity of such an interpretive framework for spiritual survival.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.03, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect a genuine Rope: extractiveness is very low (0.03) because study is considered fulfillment, not a burden or a means of extraction. Suppression is low (0.05) as participation is voluntary and driven by spiritual commitment, not coercion. Theater ratio is low (0.03) because the intellectual engagement is sincere and functional, not performative. Accessibility collapse is moderate (0.6) as it requires specialized knowledge but is open to all who commit to study. Resistance is low (0.05) as it is a widely accepted and cherished religious practice.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between this 'study_as_exercise' reading and the 'performance_only' reading, which views study as a mere placeholder for future physical sacrifice. From the 'study_as_exercise' perspective, the constraint is a source of profound spiritual benefit and continuity; from the 'performance_only' perspective, it is a necessary but incomplete substitute. The engine will compute these divergent classifications based on the structural data of each reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively prevents mandatrophy for the mitzvah of sacrifice. By re-interpreting the mandate of sacrifice from physical performance to intellectual engagement, it ensures the mandate remains 'live' and continuously fulfilled, rather than becoming an atrophied or obsolete practice awaiting a future restoration. This re-interpretation is a core mechanism for the persistence of the tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fulfillment_vs_placeholder,
    'Does intellectual engagement truly fulfill the mitzvah of sacrifice, or is it merely a placeholder until physical performance can resume?',
    'Theological consensus shifts within the rabbinic tradition, or a definitive messianic event that re-establishes physical sacrifice.',
    'If reclassified as a placeholder, the ''study_as_exercise'' reading would lose its claim to full spiritual efficacy, potentially increasing perceived extractiveness (as it would be seen as an incomplete act) and shifting its type towards a Piton or even a Snare if the ''placeholder'' status were enforced coercively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_vs_placeholder, conceptual, 'Ambiguity regarding the spiritual efficacy of study versus physical performance.').

omega_variable(
    coordination_vs_identity_lock,
    'Is the ''identity_locked'' exit option for scholars a genuine expression of commitment, or does it represent a subtle form of internalized suppression where alternative spiritual paths are unthinkable?',
    'Sociological studies of rabbinic identity formation and exit experiences, or comparative analysis with other religious traditions where intellectual engagement is central.',
    'If internalized suppression is found to be significant, the effective suppression for scholars would be higher than currently measured, potentially shifting their seat classification towards a Tangled Rope or Snare, despite the low base suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_identity_lock, empirical, 'Structural vs. internalized suppression mechanism for identity-locked scholars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_corpus__study_as_exercise, theater_ratio, 70, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.04).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.04).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.03).
narrative_ontology:measurement(koda_tr_t2024, kodashim_corpus__study_as_exercise, theater_ratio, 2024, 0.03).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__study_as_exercise, base_extractiveness, 70, 0.05).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.04).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.04).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(koda_be_t2024, kodashim_corpus__study_as_exercise, base_extractiveness, 2024, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_corpus__study_as_exercise, suppression_requirement, 70, 0.1).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__study_as_exercise, suppression_requirement, 500, 0.08).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.07).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__study_as_exercise, suppression_requirement, 1500, 0.06).
narrative_ontology:measurement(koda_su_t2024, kodashim_corpus__study_as_exercise, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel, each representing a distinct interpretation of the status and practice of sacrificial law in the absence of the Temple. The other readings are 'performance_only' and 'substitution_archive'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
