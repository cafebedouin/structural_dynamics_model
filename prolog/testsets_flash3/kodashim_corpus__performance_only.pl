% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus: Performance Only Reading
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance only' reading of the Kodashim
 *   corpus within Rabbinic Judaism. In this reading, the detailed laws of
 *   sacrifice are understood as an archived blueprint, awaiting a future
 *   messianic restoration for their physical performance. Any present-day
 *   engagement with these laws (e.g., study) is seen as preparatory but not a
 *   substitute for actual performance. This reading generates high extraction
 *   from those who devote themselves to its study, as their efforts are
 *   directed towards an unrealizable present-day goal, while benefiting
 *   institutions that promote this messianic vision.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.85).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.7).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus: Performance Only Reading").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68').
narrative_ontology:cs_kernel_codification('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', fixed_text).
narrative_ontology:cs_authority_grounding('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', lineage).
narrative_ontology:cs_interpretation_layer_present('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68').
narrative_ontology:cs_reading_relation('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', foundational, physical_sacrifice_is_mitzvah_performance).
narrative_ontology:cs_axiom_status(physical_sacrifice_is_mitzvah_performance, holdable).
narrative_ontology:cs_axiom_grounding('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', physical_sacrifice_is_mitzvah_performance, theological).
narrative_ontology:cs_axiom('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', foundational, present_study_is_preparation_not_performance).
narrative_ontology:cs_axiom_status(present_study_is_preparation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', present_study_is_preparation_not_performance, conventional).
narrative_ontology:cs_reference_frame('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', messianic_restoration_of_cult).
narrative_ontology:cs_drift_state('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', post_temple_destruction_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3d0dbb4c-f3e0-4f74-9834-8d08fd6dda68', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devoted_students_of_kodashim).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions derive legitimacy and resources from promoting the idea that the Kodashim corpus is a blueprint for a future, physically-performed sacrificial cult. They actively discourage interpretations that diminish the need for a literal restoration, thereby maintaining their mandate and funding.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who dedicate significant time and spiritual energy to the study of Kodashim, believing it to be a living, actionable practice. They misallocate their devotion and resources towards an unrealizable future state, experiencing spiritual and intellectual extraction as their efforts yield no present-day 'performance'.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devoted_students_of_kodashim, payer,
    powerless, biographical, identity_locked, local).

% General members of the community who are taught that the Kodashim corpus is solely for future physical performance. They contribute to institutions and maintain a worldview that defers spiritual fulfillment to a messianic era, experiencing a form of deferred spiritual extraction.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lay_adherents, payer,
    powerless, biographical, identity_locked, local).

% Scholars who analyze the legal and historical aspects of Kodashim, often aware of the interpretive debates. From this seat, the 'performance only' reading is one among several, but its institutional power is evident.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_scholars_of_halakha, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the community's focus on a future messianic redemption, providing a shared eschatological vision and a framework for collective anticipation.
% TRANSFER_FUNCTION: Transfers spiritual and material resources (devotion, study time, donations) from individuals and communities to institutions that promote the 'performance only' interpretation, in exchange for a promise of future, messianic fulfillment.
% ABSENT_VOICES: Those who advocate for a purely spiritual or symbolic understanding of sacrifice, or who believe the messianic era will not involve physical sacrifices, are marginalized. They would argue for a re-evaluation of present-day religious practice and a redirection of communal resources.
% DISAPPEARANCE_RATIONALE: If the 'performance only' interpretation vanished, institutions built around messianic preparation would lose their primary mandate. Devotion and study would be re-directed towards present-day spiritual practices or alternative interpretations of Kodashim, fundamentally altering the religious landscape and resource allocation.
% FOUNDING_PROBLEM: The problem of maintaining the relevance and sanctity of the sacrificial laws (Kodashim) after the destruction of the Temple and the cessation of physical sacrifices.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by messianic-preparation institutions and many lay adherents, who genuinely believe in the necessity of future physical performance. However, alternative rabbinic traditions and historical analyses (outside the benefiting parties) contest this, arguing the problem has been re-solved through prayer and study, making the 'performance only' status contested.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the 'performance only' interpretation demands significant spiritual and intellectual investment (study, devotion) without offering any present-day 'return' in terms of actual ritual performance. Suppression (0.7) is maintained through institutional authority and the framing of alternative interpretations as less authentic or even heretical. The theater ratio is very high (0.9) because the primary 'function' of the corpus (physical sacrifice) is entirely performative in the present, existing only as anticipation, while the actual activity (study, institutional maintenance) serves to reinforce the future-oriented mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of messianic-preparation institutions, this is a 'rope' or 'scaffold' – a necessary coordination mechanism for future redemption. From the perspective of devoted students and lay adherents, it operates as a 'snare', extracting their devotion for an unfulfilled promise, with their identity often locked into this deferred fulfillment.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions are clear beneficiaries, as their existence and funding depend on the perpetuation of this 'performance only' reading. Devoted students and lay adherents are victims, as their spiritual and intellectual resources are extracted for a future event, with limited present-day spiritual 'return'. Their identity is often fused with this messianic anticipation, making exit difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'snare' prevents mislabeling the constraint as a 'scaffold' or 'rope'. While it purports to coordinate towards a future state (messianic restoration), the high extractiveness and suppression, coupled with the high theater ratio, indicate that the primary function has atrophied into a mechanism for extracting resources and legitimacy in the present, rather than genuinely supporting a transition. The mandate (preparing for sacrifice) has outlived its functional capacity for present-day performance, becoming a source of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_fulfillment_certainty,
    'Is the messianic restoration, including the resumption of physical sacrifices, a certainty or a contingent event?',
    'Theological consensus shift or a historical event that either fulfills or definitively precludes the restoration.',
    'If certainty diminishes, the extractiveness of the ''performance only'' reading would increase for victims, as the promise becomes less credible. If precluded, the constraint would collapse or reclassify as a ''piton'' of pure theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_fulfillment_certainty, conceptual, 'The certainty of the messianic era''s arrival and the resumption of physical sacrifices.').

omega_variable(
    alternative_spiritual_fulfillment,
    'To what extent do alternative interpretations (e.g., study as performance, prayer as substitution) offer genuine spiritual fulfillment that mitigates the extraction of the ''performance only'' reading?',
    'Empirical study of spiritual well-being and communal engagement among adherents of different readings, or a shift in institutional endorsement of alternative practices.',
    'If alternative fulfillments are widely accepted, the ''performance only'' reading''s suppression and extractiveness would decrease, as victims would have viable, less extractive exit options within the broader tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_spiritual_fulfillment, empirical, 'The availability and efficacy of alternative spiritual paths within the tradition.').

omega_variable(
    kernel_reading_difference,
    'This constraint is one reading of the ''kodashim_corpus'' kernel. What specific structural element does this ''performance_only'' reading differ on compared to its siblings?',
    'Conceptual analysis of the core premises of each reading and their implications for present-day religious practice and resource allocation.',
    'The ''performance_only'' reading''s core premise (future physical performance) directly leads to high extractiveness and theater, which would be absent or significantly reduced in readings that posit present-day spiritual or symbolic fulfillment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'The core disagreement among readings of the Kodashim corpus: whether the laws are for future physical performance, present-day spiritual exercise, or historical archive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.8).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.83).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.86).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.88).
narrative_ontology:measurement(koda_tr_t80, kodashim_corpus__performance_only, theater_ratio, 80, 0.89).
narrative_ontology:measurement(koda_tr_t100, kodashim_corpus__performance_only, theater_ratio, 100, 0.9).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(koda_be_t80, kodashim_corpus__performance_only, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(koda_be_t100, kodashim_corpus__performance_only, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(koda_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(koda_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(koda_su_t60, kodashim_corpus__performance_only, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(koda_su_t80, kodashim_corpus__performance_only, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(koda_su_t100, kodashim_corpus__performance_only, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
