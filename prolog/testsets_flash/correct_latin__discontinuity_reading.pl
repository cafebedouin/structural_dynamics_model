% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Correct Latin: Discontinuity Reading (Classical as Preserved Text)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint represents the 'discontinuity reading' of Correct Latin,
 *   which asserts a fundamental rupture between Classical Latin (the
 *   'correct' form preserved in ancient texts) and Medieval Latin (a 'corrupt
 *   deviation' requiring reconstruction). This reading, largely driven by
 *   Renaissance humanism and later philology, established a prescriptive
 *   standard that devalued the living, evolving tradition of Latin. The
 *   constraint operates as a Tangled Rope: it coordinates academic standards
 *   and pedagogical practices (benefiting philologists) but extracts
 *   conformity and devalues the work of those studying or practicing later
 *   forms of Latin (victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.6).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.7).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Correct Latin: Discontinuity Reading (Classical as Preserved Text)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '649d92f8-57a9-450b-b720-9103d0cc5697').
narrative_ontology:cs_kernel_codification('649d92f8-57a9-450b-b720-9103d0cc5697', fixed_text).
narrative_ontology:cs_authority_grounding('649d92f8-57a9-450b-b720-9103d0cc5697', lineage).
narrative_ontology:cs_interpretation_layer_present('649d92f8-57a9-450b-b720-9103d0cc5697').
narrative_ontology:cs_reading_relation('649d92f8-57a9-450b-b720-9103d0cc5697', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('649d92f8-57a9-450b-b720-9103d0cc5697', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('649d92f8-57a9-450b-b720-9103d0cc5697', foundational, classical_latin_is_the_sole_correct_form).
narrative_ontology:cs_axiom_status(classical_latin_is_the_sole_correct_form, holdable).
narrative_ontology:cs_axiom_grounding('649d92f8-57a9-450b-b720-9103d0cc5697', classical_latin_is_the_sole_correct_form, deontological).
narrative_ontology:cs_axiom('649d92f8-57a9-450b-b720-9103d0cc5697', foundational, medieval_latin_is_corrupt_deviation).
narrative_ontology:cs_axiom_status(medieval_latin_is_corrupt_deviation, holdable).
narrative_ontology:cs_axiom_grounding('649d92f8-57a9-450b-b720-9103d0cc5697', medieval_latin_is_corrupt_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('649d92f8-57a9-450b-b720-9103d0cc5697', classical_textual_purity).
narrative_ontology:cs_drift_state('649d92f8-57a9-450b-b720-9103d0cc5697', contemporary_linguistic_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('649d92f8-57a9-450b-b720-9103d0cc5697', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, renaissance_humanists).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_scribes).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, contemporary_latin_speakers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint imposes a significant cost on anyone working with or valuing medieval Latin, forcing them to operate within a framework that labels their subject as 'corrupt.' Suppression is also high, as the academic and cultural authority of this reading actively suppresses alternative views of Latin's historical continuity. Theater ratio is moderate, reflecting that while genuine philological work occurs, a portion of the effort is performative maintenance of the 'purity' narrative. The metrics show a rise in extractiveness and suppression as the discontinuity reading became entrenched, with a slight decline in recent decades due to increased scholarship on medieval Latin.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical philologists, this constraint is a necessary 'Rope' for preserving linguistic purity and intellectual rigor. From the perspective of medieval Latin scholars and practitioners, it functions as a 'Snare' or 'Tangled Rope,' imposing an artificial standard that devalues a rich, continuous linguistic tradition. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and Renaissance humanists are clear beneficiaries, as their authority and intellectual projects are grounded in this reading. Medieval Latin scholars, scribes, and contemporary Latin speakers are victims, as their linguistic practices or subjects of study are devalued or policed by this standard. The 'agenda_setter' role for classical_philologists reflects their active role in defining and enforcing this standard.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_rupture_ambiguity,
    'Is the perceived discontinuity between Classical and Medieval Latin a genuine linguistic rupture, or a prescriptive judgment imposed by later scholars?',
    'Detailed diachronic linguistic analysis tracing phonological, morphological, and syntactic changes across the Classical-Medieval divide, comparing them to other instances of language evolution.',
    'If a genuine rupture, the constraint''s ''emerges_naturally'' component would increase, pushing it towards a Mountain. If a prescriptive judgment, its extractiveness and suppression would be further highlighted, reinforcing its Tangled Rope nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(linguistic_rupture_ambiguity, empirical, 'Whether the discontinuity is a linguistic fact or a scholarly construct.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of medieval Latin forms structural (academic gatekeeping, publishing norms) or internalized (scholars self-censor their usage to conform)?',
    'Surveys of Latin scholars on their perceived freedom to use or teach non-Classical forms, combined with analysis of academic hiring and publication trends in medieval Latin studies.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit. If purely structural, removing gatekeeping would quickly change practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-Classical Latin.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where is the core disagreement between the discontinuity reading and its siblings located structurally?',
    'Comparative analysis of the axioms and reference frames of the ''discontinuity_reading'', ''continuity_reading'', and ''hybrid_reading'' constraints.',
    'The disagreement is located in the ''legitimate_usage_set'' and ''transmission_mechanism'' axioms. The discontinuity reading forecloses the ''continuous_living_practice'' axiom of the continuity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'The core structural point of contention between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1400, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1400, correct_latin__discontinuity_reading, theater_ratio, 1400, 0.2).
narrative_ontology:measurement(corr_tr_t1600, correct_latin__discontinuity_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(corr_tr_t1800, correct_latin__discontinuity_reading, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(corr_tr_t2020, correct_latin__discontinuity_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(corr_be_t1400, correct_latin__discontinuity_reading, base_extractiveness, 1400, 0.4).
narrative_ontology:measurement(corr_be_t1600, correct_latin__discontinuity_reading, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(corr_be_t1800, correct_latin__discontinuity_reading, base_extractiveness, 1800, 0.7).
narrative_ontology:measurement(corr_be_t2020, correct_latin__discontinuity_reading, base_extractiveness, 2020, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1400, correct_latin__discontinuity_reading, suppression_requirement, 1400, 0.5).
narrative_ontology:measurement(corr_su_t1600, correct_latin__discontinuity_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(corr_su_t1800, correct_latin__discontinuity_reading, suppression_requirement, 1800, 0.8).
narrative_ontology:measurement(corr_su_t2020, correct_latin__discontinuity_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
