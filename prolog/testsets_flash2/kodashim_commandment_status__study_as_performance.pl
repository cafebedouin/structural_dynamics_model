% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Commandment Status: Study as Performance
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the Halakhic reading that studying the laws of
 *   sacrifices (Kodashim) is itself a fulfillment of the commandment,
 *   effectively substituting intellectual engagement for physical ritual in
 *   the absence of the Temple. This reading ensures the commandment remains
 *   'live' and accessible, preventing a spiritual void. It is classified as a
 *   Mountain because its persistence is rooted in theological and
 *   interpretive principles that are considered immutable within its
 *   framework, and it extracts negligible cost from its adherents, instead
 *   offering a path to fulfillment. The beneficiaries are those who engage in
 *   this study, and the victim set is empty as no one is harmed by this mode
 *   of performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Commandment Status: Study as Performance").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '66813a5b-8190-4f6a-940c-e0244a01a742').
narrative_ontology:cs_kernel_codification('66813a5b-8190-4f6a-940c-e0244a01a742', fixed_text).
narrative_ontology:cs_authority_grounding('66813a5b-8190-4f6a-940c-e0244a01a742', lineage).
narrative_ontology:cs_interpretation_layer_present('66813a5b-8190-4f6a-940c-e0244a01a742').
narrative_ontology:cs_reading_relation('66813a5b-8190-4f6a-940c-e0244a01a742', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('66813a5b-8190-4f6a-940c-e0244a01a742', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('66813a5b-8190-4f6a-940c-e0244a01a742', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('66813a5b-8190-4f6a-940c-e0244a01a742', study_is_equivalent_to_performance, theological).
narrative_ontology:cs_axiom('66813a5b-8190-4f6a-940c-e0244a01a742', foundational, divine_will_desires_fulfillment_through_accessible_means).
narrative_ontology:cs_axiom_status(divine_will_desires_fulfillment_through_accessible_means, holdable).
narrative_ontology:cs_axiom_grounding('66813a5b-8190-4f6a-940c-e0244a01a742', divine_will_desires_fulfillment_through_accessible_means, deontological).
narrative_ontology:cs_reference_frame('66813a5b-8190-4f6a-940c-e0244a01a742', rabbinic_continuity_framework).
narrative_ontology:cs_drift_state('66813a5b-8190-4f6a-940c-e0244a01a742', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('66813a5b-8190-4f6a-940c-e0244a01a742', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, observant_jews).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_as_ultimate_mitzvah).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, intellectual_engagement_as_spiritual_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of Jewish law, who define the parameters and significance of Torah study. They benefit from the intellectual engagement and the perpetuation of the tradition through their scholarship.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Fulfill the commandment of sacrifices through the act of studying the relevant laws, thereby maintaining a connection to the divine mandate even in the absence of the Temple. They experience spiritual fulfillment and continuity of tradition.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, observant_jews, beneficiary,
    moderate, biographical, identity_locked, global).

% Believe the commandment is suspended until the Messianic era and the rebuilding of the Temple. While they may study, they do not view it as a full performance of the commandment itself, but rather as preparation. Their view is not central to this reading's operational logic.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_deferral_proponents, excluded,
    organized, generational, constrained, global).

% Hold that the commandment is strictly contingent on the physical performance of sacrifices in the Temple. For them, study is not a substitute for the actual ritual, and the commandment is currently inoperative. Their perspective is foreclosed by this reading's core axiom.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_proponents, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, accessible means for observant Jews to fulfill the divine commandment of sacrifices, ensuring the spiritual and intellectual continuity of the tradition across generations, even in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers the spiritual merit and obligation of the sacrifice commandment from a physically impossible ritual to an accessible intellectual and spiritual practice (Torah study), from the divine to the individual.
% ABSENT_VOICES: Those who believe the commandment is either strictly suspended until the Messianic era (messianic_deferral_proponents) or entirely inoperative without the Temple (performance_only_proponents) are excluded from the interpretive framework that grants study full commandment status. They would argue that this reading diminishes the unique significance of the physical ritual.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, a significant portion of Jewish religious practice and identity would be destabilized. Observant Jews would lose a primary means of connecting to a central commandment, leading to a crisis of religious continuity and a re-evaluation of the role of study in Jewish life.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the physical performance of animal sacrifices impossible, creating a crisis of how to fulfill a central divine commandment.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature (Talmud, Midrash) and subsequent halakhic responsa universally attest to the problem of Temple destruction and the need for alternative modes of commandment fulfillment. This is a foundational historical and theological problem for all branches of Judaism.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.05) because this reading provides a path to fulfillment rather than imposing a burden; the minimal value accounts for the intellectual effort required. Suppression is zero as participation is voluntary and encouraged, not coerced. Theater ratio is zero because the study is considered genuinely efficacious. Accessibility collapse is high (0.95) because, within this framework, the alternative (physical sacrifice) is genuinely impossible, making study the only viable path. Resistance is low (0.05) as this reading is widely accepted within Orthodox Judaism, though other readings exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the commandment is fully active and accessible. From the 'performance_only' perspective, the commandment is suspended and study is merely preparatory, not performative. This divergence is a core feature of the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and observant Jews are beneficiaries; the former define and transmit the practice, the latter gain spiritual fulfillment. Both are identity_locked into the tradition. Proponents of other readings are excluded, as their interpretations are not operative within this specific constraint's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commandment_fulfillment_equivalence,
    'Is the spiritual efficacy of studying sacrifice laws truly equivalent to the physical performance of sacrifices, or is it a rabbinic concession to necessity?',
    'Theological consensus shift or a future Messianic era where physical sacrifices are restored and their relationship to study can be re-evaluated.',
    'If not equivalent, the extractiveness might be higher (a ''cost'' of incomplete fulfillment) and the claimed type might shift towards a ''scaffold'' (temporary measure) or ''tangled_rope'' (coordination with a hidden cost of diminished fulfillment). If fully equivalent, the mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commandment_fulfillment_equivalence, conceptual, 'Ambiguity regarding the equivalence of study and performance for commandment fulfillment.').

omega_variable(
    natural_law_vs_rabbinic_decree,
    'Is the ''study as performance'' principle an inherent, natural law of Torah, or a rabbinic decree (takkanah) enacted to preserve the tradition?',
    'Deep textual analysis and historical-theological inquiry into the origins of the principle within rabbinic literature.',
    'If a rabbinic decree, the ''emerges_naturally'' claim for the mountain classification is weakened, potentially reclassifying it as a ''rope'' or ''tangled_rope'' that requires active maintenance of the rabbinic authority. If inherent, the mountain classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_rabbinic_decree, conceptual, 'Whether the principle is a natural law or a rabbinic enactment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t25, kodashim_commandment_status__study_as_performance, theater_ratio, 25, 0.0).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__study_as_performance, theater_ratio, 50, 0.0).
narrative_ontology:measurement(koda_tr_t75, kodashim_commandment_status__study_as_performance, theater_ratio, 75, 0.0).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__study_as_performance, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(koda_be_t25, kodashim_commandment_status__study_as_performance, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__study_as_performance, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(koda_be_t75, kodashim_commandment_status__study_as_performance, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(koda_su_t25, kodashim_commandment_status__study_as_performance, suppression_requirement, 25, 0.0).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__study_as_performance, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(koda_su_t75, kodashim_commandment_status__study_as_performance, suppression_requirement, 75, 0.0).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__study_as_performance, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel. This reading asserts that studying sacrifice laws fulfills the commandment itself, ensuring continuity. The 'performance_only' reading holds the commandment is suspended without the Temple, and the 'messianic_deferral' reading views study as preparation for future restoration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
