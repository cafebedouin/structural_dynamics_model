% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Study as Cosmic Performance
 *   domain: religious/textual
 *
 * SUMMARY:
 *   The 'study as performance' reading of the Kodashim obligation holds that
 *   engaging with the sacrificial tractates of the Talmud does not merely
 *   preserve knowledge or prepare for a future Temple — it actively enacts
 *   the cosmic function of sacrifice in the present. The physical absence of
 *   the Temple is irrelevant because the spiritual efficacy operates in a
 *   register that does not depend on material instantiation. This reading
 *   claims zero extractiveness: no party pays, no party collects, no
 *   enforcement is required. The beneficiary is named as 'cosmic order
 *   itself' — a vindicated proposition, not a human actor. The constraint
 *   presents as a Mountain: a spiritual law that would operate identically
 *   regardless of human recognition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Study as Cosmic Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/textual").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '12d62e19-eb82-41af-8128-07d36387d19e').
narrative_ontology:cs_kernel_codification('12d62e19-eb82-41af-8128-07d36387d19e', fixed_text).
narrative_ontology:cs_authority_grounding('12d62e19-eb82-41af-8128-07d36387d19e', lineage).
narrative_ontology:cs_interpretation_layer_present('12d62e19-eb82-41af-8128-07d36387d19e').
narrative_ontology:cs_reading_relation('12d62e19-eb82-41af-8128-07d36387d19e', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('12d62e19-eb82-41af-8128-07d36387d19e', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('12d62e19-eb82-41af-8128-07d36387d19e', foundational, study_enacts_cosmic_function).
narrative_ontology:cs_axiom_status(study_enacts_cosmic_function, holdable).
narrative_ontology:cs_axiom_grounding('12d62e19-eb82-41af-8128-07d36387d19e', study_enacts_cosmic_function, theological).
narrative_ontology:cs_axiom('12d62e19-eb82-41af-8128-07d36387d19e', secondary, temple_restoration_not_required_for_efficacy).
narrative_ontology:cs_axiom_status(temple_restoration_not_required_for_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('12d62e19-eb82-41af-8128-07d36387d19e', temple_restoration_not_required_for_efficacy, theological).
narrative_ontology:cs_reference_frame('12d62e19-eb82-41af-8128-07d36387d19e', study_as_cosmic_performance).
narrative_ontology:cs_drift_state('12d62e19-eb82-41af-8128-07d36387d19e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('12d62e19-eb82-41af-8128-07d36387d19e', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, devout_practitioners).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, cosmic_order_maintained_through_study).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, temple_absence_irrelevant_to_spiritual_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in the study of Kodashim tractates as a religious obligation; they do not experience the study as extracting from them nor as a coordination problem to be solved, but as participation in a cosmic order that the study itself enacts.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, talmudic_scholars, observer,
    organized, generational, arbitrage, global).

% Experience the study of sacrificial law as spiritually efficacious in itself; their identity is fused with the practice such that exit is not a live option, but they do not bear costs — they receive the spiritual benefit the tradition claims the study confers.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, devout_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is not a coordination arrangement but a claim about the ontological status of study: the act of studying sacrificial law IS the performance of the sacrifice in the cosmic register, requiring no human coordination to take effect.
% TRANSFER_FUNCTION: None — no value, resource, or obligation transfers between parties; the study's efficacy is intrinsic to the act itself in relation to cosmic order.
% ABSENT_VOICES: None — the debate is internal to the tradition; all positions are represented by living interpretive communities. No external party is structurally excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If the human practice of studying Kodashim ceased, the cosmic law it reflects would persist unchanged — the study does not sustain the law; the law sustains the study. The constraint's disappearance would not rearrange the world because the constraint is descriptive of a spiritual reality, not constitutive of it.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) raised the question of how sacrificial obligation persists without a physical Temple and functioning priesthood.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (Menachot 110a, Ta'anit 27b) record the debate; medieval codifiers (Rambam Hilkhot Temidin u'Musafin, Ra'avad's glosses) take opposing positions; modern scholars (Jacob Neusner, Moshe Halbertal) analyze the dispute from outside the benefiting tradition.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_unchanged).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the study is not a mechanism that transfers value from one party to another; suppression is zero because no coercion maintains the practice — practitioners engage voluntarily from identity conviction; theater ratio is zero because there is no performative gap between the practice's claimed function and its actual operation; accessibility collapse is near-total (0.95) because the claim itself denies alternatives (the study IS the sacrifice, not a substitute); resistance is zero because the constraint meets no opposition — those who disagree simply hold a different reading. The metrics and claim align: this reading structurally presents as a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   The sibling readings (study_as_preparation, study_as_archive) would compute different seat classifications: preparation reading sees scholars as paying costs now for future benefit (d > 0.5 for current scholars); archive reading sees study as identity maintenance with diffuse benefits. But from THIS reading's structural data, all human seats are non-extractive participants in a cosmic law.
 *
 * DIRECTIONALITY LOGIC:
 *   All human seats are near-symmetric (d ≈ 0.5) or slightly beneficiary-ward: scholars and practitioners experience the study as a spiritual opportunity, not a cost. No seat is a target. The 'beneficiary' is cosmic order — a vindicated proposition that collects no rents. The engine's directionality derivation will assign low d to all human seats, producing near-zero effective extraction for all.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint cannot suffer mandatrophy because it claims no mandate — it describes a perpetual spiritual law. The founding problem (Temple destruction) is contested as to whether it remains live, but the reading's claim is that the law's efficacy never depended on the Temple's physical presence, so the founding problem's status is irrelevant to the constraint's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_study_as_performance,
    'Is the ''study as performance'' reading a genuine description of a spiritual law (Mountain) or a theological construction that serves institutional interests?',
    'Comparative analysis of whether the reading''s claims generate observable extractive dynamics (institutional control, resource flows, identity coercion) or whether the practice exhibits the Mountain signature (zero extraction, zero suppression, high accessibility collapse, zero resistance) across all seats.',
    'If the reading is a Mountain, its classification is stable across all observer seats. If it is a constructed claim masking institutional extraction, it would reclassify as Tangled Rope or Snare under FSM or standard gates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_study_as_performance, conceptual, 'Whether this kernel reading describes a natural/spiritual law or a constructed theological claim.').

omega_variable(
    spiritual_law_naturalness,
    'Does the claim ''study enacts cosmic sacrifice'' describe a mind-independent spiritual reality, or is it a human interpretive framework?',
    'No empirical resolution possible; the question is constitutive of the theological/philosophical divide. The omega records the irreducible ambiguity.',
    'If mind-independent, Mountain classification holds. If human construction, the constraint is a vindicated proposition without extractive force — still Mountain-like but with different epistemic status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiritual_law_naturalness, preference, 'Ontological status of the spiritual law claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_performance, 0.0).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% Kodashim obligation kernel family: three readings decompose the single label 'obligation to study sacrificial law' into structurally distinct constraints. This reading (study_as_performance) claims Mountain status with zero extraction; study_as_preparation claims ongoing binding obligation with deferred performance (Tangled Rope?); study_as_archive claims historical preservation function (Rope or Scaffold). Each has different ε, different beneficiary/victim structures, different type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
