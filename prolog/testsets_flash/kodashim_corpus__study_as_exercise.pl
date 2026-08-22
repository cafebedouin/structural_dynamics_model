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
 *   human_readable: Kodashim Corpus: Study as Mitzvah Performance
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the rabbinic Jewish understanding that the
 *   diligent study of the laws of sacrifice (Kodashim) is itself a
 *   fulfillment of the mitzvah (divine commandment), effectively 'occupying'
 *   the kernel of sacrificial practice in the absence of the Temple. This
 *   reading provides a continuous mode of religious observance and spiritual
 *   engagement, transforming a potentially inert legal corpus into a living
 *   spiritual exercise. It is a reading of the 'kodashim_corpus' kernel,
 *   specifically the 'study_as_exercise' interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.01).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.01).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus: Study as Mitzvah Performance").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '67a26b5c-40c8-4edf-be52-f027bc4aa778').
narrative_ontology:cs_kernel_codification('67a26b5c-40c8-4edf-be52-f027bc4aa778', fixed_text).
narrative_ontology:cs_authority_grounding('67a26b5c-40c8-4edf-be52-f027bc4aa778', lineage).
narrative_ontology:cs_interpretation_layer_present('67a26b5c-40c8-4edf-be52-f027bc4aa778').
narrative_ontology:cs_reading_relation('67a26b5c-40c8-4edf-be52-f027bc4aa778', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('67a26b5c-40c8-4edf-be52-f027bc4aa778', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('67a26b5c-40c8-4edf-be52-f027bc4aa778', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('67a26b5c-40c8-4edf-be52-f027bc4aa778', study_is_equivalent_to_performance, deontological).
narrative_ontology:cs_axiom('67a26b5c-40c8-4edf-be52-f027bc4aa778', secondary, divine_presence_through_intellectual_engagement).
narrative_ontology:cs_axiom_status(divine_presence_through_intellectual_engagement, holdable).
narrative_ontology:cs_axiom_grounding('67a26b5c-40c8-4edf-be52-f027bc4aa778', divine_presence_through_intellectual_engagement, theological).
narrative_ontology:cs_reference_frame('67a26b5c-40c8-4edf-be52-f027bc4aa778', rabbinic_post_temple_halakha).
narrative_ontology:cs_drift_state('67a26b5c-40c8-4edf-be52-f027bc4aa778', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('67a26b5c-40c8-4edf-be52-f027bc4aa778', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary practitioners and interpreters of Kodashim. Their continuous intellectual engagement with the sacrificial laws is considered the fulfillment of the mitzvah, maintaining the spiritual order. Their professional and spiritual identity is deeply fused with this practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefits from the spiritual continuity and cosmic order maintained by the scholars' study. This reading provides a path for religious observance in the absence of the Temple, fostering communal identity and resilience.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_community, beneficiary,
    organized, generational, constrained, global).

% Participate in the study of Kodashim to varying degrees, often through communal learning. They benefit from the spiritual merit and intellectual engagement, contributing to their personal religious practice without being primary interpreters.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, lay_adherents, beneficiary,
    moderate, biographical, mobile, local).

% Believe that only the physical performance of sacrifices in a rebuilt Temple constitutes true fulfillment. While they may engage in study, they view it as preparation, not substitution, and are excluded from the 'study as performance' framing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, messianic_restorationists, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual and intellectual engagement of the Jewish people with the divine commandments concerning sacrifices, providing a continuous mode of observance in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit and communal cohesion through the intellectual effort of studying the sacrificial laws, from scholars and adherents to the entire community and, cosmically, to the divine order.
% ABSENT_VOICES: Messianic restorationists would object, arguing that study is merely a placeholder for actual performance, and that this reading diminishes the urgency and necessity of rebuilding the Temple and restoring physical sacrifices. Their voices are marginalized within the dominant rabbinic discourse that emphasizes intellectual engagement.
% DISAPPEARANCE_RATIONALE: If the understanding that 'study is performance' vanished, a core pillar of post-Temple rabbinic Judaism would collapse. The spiritual practice of countless individuals and the communal identity of the Jewish people would be profoundly disrupted, leading to a crisis of religious meaning and continuity.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of physical sacrifices left a profound void in Jewish religious practice, threatening the continuity of divine commandment and spiritual connection.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts, contemporary theological discourse, and the lived experience of Jewish communities for nearly two millennia corroborate that the problem of post-Temple observance remains live. The solution of 'study as performance' is widely attested as foundational to rabbinic Judaism by scholars and practitioners across diverse movements.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.01, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is negligible (0.01) because this constraint is a spiritual practice that benefits participants without imposing significant costs or extracting resources. Suppression is low (0.05) as participation is voluntary, though deeply embedded in religious identity. Theater ratio is zero, as the practice is considered genuinely functional and spiritually efficacious. Accessibility collapse is high (0.9) because for adherents of this reading, there are no viable alternatives to intellectual engagement for fulfilling the mitzvah in the current era. Resistance is low (0.02) because this reading is widely accepted within mainstream rabbinic Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and the mainstream Jewish community, this constraint is a pure Rope, coordinating spiritual practice and maintaining continuity. From the perspective of messianic restorationists, it might be seen as a form of 'theater' or 'substitution' that delays or distracts from the true, physical performance of sacrifices, though this reading does not impose costs on them directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars are primary beneficiaries and agenda-setters, as their identity and spiritual practice are centered on this study. The broader Jewish community and lay adherents are also beneficiaries, gaining spiritual merit and communal cohesion. There are no identifiable victims, as no party is deprived or bears asymmetric costs through this practice. Messianic restorationists are 'excluded' as their framing of the kernel differs fundamentally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_occupation_ambiguity,
    'Is the ''study as exercise'' reading a full occupation of the Kodashim kernel, or a temporary substitution awaiting physical restoration?',
    'Theological consensus shift or the rebuilding of the Temple and resumption of sacrifices. If sacrifices resume, the ''study as exercise'' reading would likely be re-evaluated.',
    'If it''s a full occupation, the constraint remains a Rope. If it''s a temporary substitution, its classification might shift towards Scaffold (transitional support) or even Piton (if the ''temporary'' aspect becomes performative over millennia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_occupation_ambiguity, conceptual, 'Ambiguity regarding the permanence of ''study as exercise'' as a mode of observance.').

omega_variable(
    identity_lock_vs_choice,
    'To what extent is the ''identity_locked'' exit option for rabbinic scholars a genuine structural constraint versus a deeply internalized, chosen commitment?',
    'Analysis of ex-Haredi communities and their post-exit identity formation. If the commitment persists after structural ties are severed, it''s more internalized; if it dissolves, it''s more structural.',
    'If more structural, the ''suppression'' metric might be slightly higher for those within the system. If more internalized, the low suppression is accurate, reflecting genuine adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_choice, empirical, 'Structural vs. internalized nature of identity lock for scholars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_corpus__study_as_exercise, theater_ratio, 70, 0.0).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(koda_tr_t2024, kodashim_corpus__study_as_exercise, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__study_as_exercise, base_extractiveness, 70, 0.01).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.01).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.01).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.01).
narrative_ontology:measurement(koda_be_t2024, kodashim_corpus__study_as_exercise, base_extractiveness, 2024, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_corpus__study_as_exercise, suppression_requirement, 70, 0.05).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__study_as_exercise, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__study_as_exercise, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(koda_su_t2024, kodashim_corpus__study_as_exercise, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. Its ε value is near zero, reflecting its nature as a spiritual practice, distinct from the 'performance_only' (also low ε) and 'substitution_archive' (potentially higher ε if it implies a loss of direct observance) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
