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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Corpus: Study as Mitzvah Fulfillment
 *   domain: religious/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the rabbinic interpretation within Judaism that
 *   the diligent study of the laws of sacrifice (Kodashim) is itself
 *   considered a fulfillment of the mitzvah (divine commandment) of offering
 *   sacrifices, particularly after the destruction of the Temple. This
 *   reading posits that the kernel of divine command is 'occupied' through
 *   continuous intellectual and spiritual engagement, providing a path for
 *   spiritual merit and continuity of tradition. It is classified as a Rope
 *   because it coordinates a shared interpretive practice that benefits
 *   participants with minimal extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus: Study as Mitzvah Fulfillment").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/rabbinic_judaism/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f').
narrative_ontology:cs_kernel_codification('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', fixed_text).
narrative_ontology:cs_authority_grounding('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', lineage).
narrative_ontology:cs_interpretation_layer_present('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f').
narrative_ontology:cs_reading_relation('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', foundational, torah_study_is_mitzvah_fulfillment).
narrative_ontology:cs_axiom_status(torah_study_is_mitzvah_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', torah_study_is_mitzvah_fulfillment, theological).
narrative_ontology:cs_axiom('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', foundational, intellectual_engagement_occupies_kernel).
narrative_ontology:cs_axiom_status(intellectual_engagement_occupies_kernel, holdable).
narrative_ontology:cs_axiom_grounding('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', intellectual_engagement_occupies_kernel, theological).
narrative_ontology:cs_reference_frame('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', talmudic_rabbinic_continuity).
narrative_ontology:cs_drift_state('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', contemporary_rabbinic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0a4d6a8a-9982-4e1b-bfcd-e5b9cb80fc1f', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, scholars_of_kodashim).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, lay_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_corpus__study_as_exercise, lay_adherents).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_as_spiritual_merit).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, continuity_of_divine_command).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars dedicate their lives to the intricate study of the Kodashim (sacrifices) tractates of the Talmud. For them, this intellectual and spiritual engagement is itself the fulfillment of the mitzvah (divine commandment), maintaining cosmic order and spiritual connection. Their identity is deeply fused with this practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, scholars_of_kodashim, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, scholars_of_kodashim, beneficiary).

% Participate in communal study sessions or personal engagement with Kodashim, deriving spiritual merit and a sense of continuity with tradition. They benefit from the spiritual framework provided by the scholars but also invest time and effort. Their options are bounded by the accepted interpretations within their community.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, lay_adherents, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, lay_adherents, payer).

% Adhere to the belief that physical sacrifices must be restored with the coming of the Messiah and the rebuilding of the Temple. They view study as preparation for this future, not as a substitute or fulfillment in itself. Their perspective is structurally excluded from the 'study as fulfillment' reading.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, messianic_restorationists, excluded,
    organized, civilizational, identity_locked, global).

% Believe that prayer and Torah study have definitively replaced the sacrificial cult, and that Kodashim serves primarily as a historical archive documenting a superseded practice. Their view of the kernel's status is incompatible with its 'active occupation' through study.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, liturgical_innovators, excluded,
    organized, generational, identity_locked, global).

% Academics and scholars of religion who study Rabbinic Judaism and commitment systems from an external, analytical perspective. They do not participate in the spiritual practice but analyze its structure and function.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates continuous intellectual and spiritual engagement with the divine command concerning sacrifices, maintaining the relevance of a non-practicable mitzvah and providing a path for spiritual fulfillment in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit, intellectual rigor, communal identity, and a sense of continuity with divine command through the act of studying the laws of sacrifice.
% ABSENT_VOICES: Messianic restorationists would argue that study is insufficient and only physical performance counts. Liturgical innovators would argue that study is merely historical archiving, not active fulfillment. Both are present in the broader discourse but their core premises are incompatible with this reading's claim of fulfillment.
% DISAPPEARANCE_RATIONALE: If the interpretation that 'study is itself the performance of the mitzvah' vanished, a significant portion of rabbinic intellectual and spiritual life would lose its meaning. The vast corpus of Kodashim study would become purely academic history, leading to a profound reorientation of religious practice and identity for many adherents who currently find spiritual fulfillment through it.
% FOUNDING_PROBLEM: How to fulfill the divine command of sacrifices (mitzvah) after the destruction of the Second Temple (70 CE), when physical performance became impossible, without abandoning the command or the associated spiritual merit and cosmic significance.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts from the Talmudic era (e.g., Tractate Menachot 110a, which states 'whoever studies the laws of sacrifice, it is as if he offered a sacrifice') and continuous rabbinic tradition attest to this interpretation as a valid and active response to the Temple's destruction. This is corroborated by the ongoing centrality of Kodashim study in yeshivot and batei midrash globally, independent of any specific benefiting party's claim.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is near zero because the act of study is seen as a complete fulfillment, not a burden or a partial substitute. Suppression is minimal as participation is voluntary and driven by spiritual aspiration. Theater ratio is low because the engagement is genuine and deeply meaningful to its adherents. Accessibility collapse and resistance are low because this is an established and widely accepted interpretive path within its domain, with alternatives existing for those who prefer other forms of spiritual engagement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of scholars and lay adherents of this reading, the constraint is a pure coordination mechanism for spiritual fulfillment. From the perspective of 'messianic restorationists' or 'liturgical innovators' (excluded stakeholders), this reading might be seen as an incomplete or even incorrect interpretation, but within its own framework, it functions as a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholars of Kodashim are primary beneficiaries and agenda-setters, as they define and perpetuate this interpretive practice, gaining spiritual merit and intellectual engagement. Lay adherents are also beneficiaries, gaining spiritual connection and communal identity. There are no direct 'victims' as the constraint is framed as a path to fulfillment, not a deprivation. Excluded stakeholders represent alternative readings that are incompatible with this one's core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resolves the mandatrophy of the sacrificial system post-Temple destruction by reinterpreting the mitzvah's fulfillment. It prevents the original mandate from becoming a 'piton' (atrophied function) or a 'snare' (extraction from an impossible demand) by providing a live, accessible, and spiritually meaningful alternative. The 'founding_problem_status' is 'live' because the problem of fulfilling the mitzvah without a Temple is still active, and this reading provides a solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as one reading of the ''kodashim_corpus'' kernel, specifically the ''study_as_exercise'' reading?',
    'Analysis of primary rabbinic texts and secondary scholarship on the interpretation of Kodashim, comparing this reading''s core tenets against the broader interpretive landscape.',
    'If misidentified, the classification of this constraint and its relationship to sibling readings would be inaccurate, potentially leading to incorrect conclusions about the commitment system''s dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint''s identity as a specific reading of a contested kernel.').

omega_variable(
    performance_only_delta,
    'How would the structural properties of the ''kodashim_corpus'' kernel change if the ''performance_only'' reading were universally adopted?',
    'Counterfactual analysis: if the kernel were viewed as a ''husk awaiting restoration,'' its current ''occupation'' through study would cease to be a source of spiritual merit, shifting the constraint''s function from active fulfillment to archival preservation.',
    'The ''study_as_exercise'' constraint would cease to exist as a live practice; the kernel would become inert, awaiting a future event, leading to a reclassification of the underlying commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_delta, conceptual, 'Examines the impact of a sibling reading on the kernel''s structural function.').

omega_variable(
    substitution_archive_delta,
    'How would the structural properties of the ''kodashim_corpus'' kernel change if the ''substitution_archive'' reading were universally adopted?',
    'Counterfactual analysis: if prayer and Torah study were seen as having ''replaced'' sacrifice, and Kodashim as a ''memorial archive,'' the claim that study *is* performance would be invalidated, and the kernel''s active spiritual function would be reattributed to other practices.',
    'The ''study_as_exercise'' constraint would be reclassified as a historical or archival practice, losing its ''rope'' function of coordinating active spiritual fulfillment, and the kernel would be seen as superseded rather than actively occupied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_archive_delta, conceptual, 'Examines the impact of a sibling reading on the kernel''s structural function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t5, kodashim_corpus__study_as_exercise, theater_ratio, 5, 0.1).
narrative_ontology:measurement(koda_tr_t10, kodashim_corpus__study_as_exercise, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(koda_be_t5, kodashim_corpus__study_as_exercise, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(koda_be_t10, kodashim_corpus__study_as_exercise, base_extractiveness, 10, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(koda_su_t5, kodashim_corpus__study_as_exercise, suppression_requirement, 5, 0.05).
narrative_ontology:measurement(koda_su_t10, kodashim_corpus__study_as_exercise, suppression_requirement, 10, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
