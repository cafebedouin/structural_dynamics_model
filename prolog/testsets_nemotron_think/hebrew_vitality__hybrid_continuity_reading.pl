% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hybrid Continuity Reading of Hebrew Vitality
 *   domain: sociolinguistics/historical_linguistics/jewish_studies
 *
 * SUMMARY:
 *   The hybrid continuity reading of Hebrew vitality asserts that liturgical
 *   preservation was a necessary enabler but insufficient for vernacular
 *   vitality; the revival required both the preserved substrate and active
 *   reconstruction. This reading positions itself as a scholarly synthesis
 *   resolving the contest between liturgical traditionalists (who equate
 *   ritual use with vitality) and native-generation advocates (who equate
 *   vitality exclusively with native daily speech). As an analytical
 *   synthesis rather than an actionable constraint, it exhibits low
 *   extractiveness and suppression; its operation is coordinative within
 *   sociolinguistic discourse, offering a shared framework that incorporates
 *   elements of both rival readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hybrid Continuity Reading of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/historical_linguistics/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '99bf435c-6123-4ebf-ab1d-ed8bd812f76c').
narrative_ontology:cs_kernel_codification('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', distributed).
narrative_ontology:cs_authority_grounding('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', practice).
narrative_ontology:cs_reading_relation('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', foundational, dual_condition_vitality).
narrative_ontology:cs_axiom_status(dual_condition_vitality, holdable).
narrative_ontology:cs_axiom_grounding('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', dual_condition_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', revival_dual_condition).
narrative_ontology:cs_drift_state('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', post_nativization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99bf435c-6123-4ebf-ab1d-ed8bd812f76c', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_revival_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_practitioners).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, sociolinguistic_field).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, liturgical_traditionalists).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, native_daily_advocates).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, historical_revival_dual_causality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars of Hebrew revival who adopt the hybrid synthesis as their analytical framework; they benefit from a coherent narrative that incorporates both liturgical continuity and vernacular innovation without forcing a monocausal choice.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_revival_scholars, beneficiary,
    organized, biographical, mobile, global).

% Religious and traditionalist scholars who hold that unbroken liturgical use constitutes Hebrew vitality; the hybrid reading qualifies their claim by treating liturgy as necessary substrate but insufficient for vernacular vitality, which they experience as a demotion of their core position.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_traditionalists, payer,
    organized, generational, identity_locked, global).

% Language activists and modern Hebrew speakers who hold that only native, daily generation constitutes true vitality; the hybrid reading qualifies their claim by treating native generation as the outcome of reconstruction on a liturgical substrate, which they experience as conceding dependency on a tradition they reject.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_daily_advocates, payer,
    organized, biographical, constrained, national).

% Practitioners working on other language revivals who use the Hebrew case as a model; the hybrid reading gives them a usable framework that distinguishes substrate preservation from active reconstruction, both of which they must manage in their own projects.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Analysts of language revival cross-linguistically who evaluate the hybrid reading against other revival cases (Māori, Welsh, Hawaiian) to test whether dual-condition causality generalizes.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a synthesized historical framework that integrates liturgical preservation as necessary substrate with vernacular reconstruction as sufficient condition for vitality, coordinating scholarly discourse across the liturgical/native divide.
% TRANSFER_FUNCTION: Moves explanatory weight from monocausal accounts (liturgical-only or native-only) to a dual-condition account; moves scholarly legitimacy toward synthesis positions that must account for both continuity and rupture.
% ABSENT_VOICES: Ultra-orthodox traditionalists who reject any vitality beyond liturgy, and secular nationalists who reject liturgical dependency entirely; both are present in discourse but their stronger exclusionary claims are not accommodated in the synthesis.
% DISAPPEARANCE_RATIONALE: The synthesis resolves a live scholarly dispute; its absence would leave the coordination problem (how to account for both continuity and rupture in Hebrew's revival) unsolved, reverting the field to polarized liturgical vs. native accounts.
% FOUNDING_PROBLEM: The need to explain how a language with no native speakers for 1700 years became a living vernacular again, without denying either its unbroken liturgical use or the radical reconstruction involved.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by historical linguists outside Jewish studies (e.g., Fishman on reversing language shift, Hinton on revival models) who identify substrate-plus-reconstruction as a cross-linguistic pattern in successful revitalization.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the reading does not extract resources or compliance from agents; it organizes scholarly interpretation. Suppression is low (0.1) because rival readings remain live and published; the synthesis does not institutionally exclude them. Theater ratio is low but rising (0.1→0.2) as the synthesis becomes textbook orthodoxy, risking ritualized citation over active engagement with the underlying historical complexity. Accessibility collapse is moderate (0.4) because alternative readings remain accessible and defended. Resistance is moderate (0.5) because both rival camps actively resist full absorption into the synthesis.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (scholars, practitioners), the constraint appears as a rope: genuine coordination with minimal coercion. From the payer seats (traditionalists, native-daily advocates), the same structure may appear as a tangled rope: it coordinates the field but extracts the concession that their preferred monocausal account is insufficient. The engine computes this divergence from the structural data; the authored claim (rope) reflects the synthetic intent, while the metrics capture the lived experience of the contested seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revival scholars and revitalization practitioners are beneficiaries (d ≈ 0.2): they gain a coherent, usable framework. Liturgical traditionalists and native-daily advocates are payers (d ≈ 0.6-0.7): their stronger monocausal claims are qualified by the synthesis, which treats each as partial. Sociolinguistic observers sit near symmetric (d ≈ 0.5): they use the framework analytically without personal stake. The reading's coordination function is genuine — it solves a real explanatory problem — and its extraction is the minimal overhead of scholarly consensus-building.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining Hebrew's unique revival) remains live in revitalization theory. The hybrid reading has not become a piton because it continues to do explanatory work for new revival cases; its theater ratio rise bears watching but has not yet displaced functional engagement. Mandatrophy is not resolved — the synthesis remains the best available coordination for the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_hebrew_vitality,
    'How does the hybrid_continuity_reading relate structurally to the sibling readings liturgical_reading and native_daily_reading within the hebrew_vitality kernel?',
    'Map the logical relations: does the dual-condition claim foreclose either monocausal claim, or do all three coexist as live positions in scholarly discourse? Trace citation networks and curriculum adoption to see which readings are treated as compatible vs. competing.',
    'If forecloses, the kernel has a logical hierarchy; if coexists_with, the kernel is a site of persistent pluralism; if influences, the hybrid reading reshapes the legitimacy conditions for the siblings without eliminating them. This determines whether the kernel''s contest is resolvable or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_hebrew_vitality, conceptual, 'Structural relationship of this reading to its kernel siblings.').

omega_variable(
    synthesis_accuracy_vs_coherence,
    'Does the hybrid synthesis accurately capture the historical causality of Hebrew''s revival, or does it impose retrospective coherence on a messier process?',
    'Compare the synthesis''s claims against primary sources from the revival period (Ben-Yehuda''s writings, early teacher reports, settlement records) and against cross-linguistic revival data. Test whether substrate and reconstruction were genuinely distinct phases or retrospectively distinguished.',
    'If the synthesis imposes coherence, its low extractiveness may mask epistemic extraction — it extracts explanatory simplicity from historical complexity. If accurate, the low extractiveness is genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_accuracy_vs_coherence, empirical, 'Whether the dual-condition account is historically warranted or a convenient synthesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t1950, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t1964, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1964, 0.08).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t1978, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t1992, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t2006, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t2020, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t1950, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t1964, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1964, 0.08).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t1978, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1978, 0.12).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t1992, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t2006, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2006, 0.14).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t2020, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_vitality_hybrid_su_t1950, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hebrew_vitality_hybrid_su_t1964, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1964, 0.07).
narrative_ontology:measurement(hebrew_vitality_hybrid_su_t1978, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1978, 0.08).
narrative_ontology:measurement(hebrew_vitality_hybrid_su_t1992, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1992, 0.1).
narrative_ontology:measurement(hebrew_vitality_hybrid_su_t2006, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2006, 0.1).
narrative_ontology:measurement(hebrew_vitality_hybrid_su_t2020, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__hybrid_continuity_reading, 0.02).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_vitality kernel. The liturgical_reading and native_daily_reading are sibling constraints. All three share the kernel_id but instantiate different constraints with different ε, beneficiary structures, and claimed types. The hybrid reading coordinates across the siblings; the siblings each treat their preferred condition as necessary and sufficient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
