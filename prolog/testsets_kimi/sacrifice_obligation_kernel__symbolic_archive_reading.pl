% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Obligation Kernel â Symbolic Archive Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint instantiates the symbolic_archive_reading of the
 *   sacrifice_obligation_kernel. It treats the corpus of sacrificial law not
 *   as a currently binding halakhic obligation but as a cultural-historical
 *   archive whose study preserves Jewish collective memory and continuity.
 *   The reading makes no normative claim that study fulfills or replaces the
 *   mitzvah; participation is entirely voluntary. Because there is no
 *   coercion, no victim set, and no material extraction, the constraint
 *   functions as a pure coordination mechanism (rope) for cultural
 *   preservation. The zero-extractiveness claim is deliberately paired with
 *   near-zero suppression and theater metrics; the engine will evaluate
 *   whether the structural data supports rope classification from every seat.
 *
 * KEY AGENTS:
 *   - torah_study_communities (beneficiary/moderate/mobile): voluntary participants who derive identity continuity from archival study
 *   - educational_stewards (agenda_setter/organized/mobile): non-coercive curriculum leaders who maintain transmission
 *   - traditional_halakhic_authorities (observer/institutional/analytical): hold competing normative readings and observe this stance descriptively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.04).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Obligation Kernel â Symbolic Archive Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'd79b60f2-15cc-4032-a2ad-fa092846ce43').
narrative_ontology:cs_kernel_codification('d79b60f2-15cc-4032-a2ad-fa092846ce43', fixed_text).
narrative_ontology:cs_authority_grounding('d79b60f2-15cc-4032-a2ad-fa092846ce43', lineage).
narrative_ontology:cs_interpretation_layer_present('d79b60f2-15cc-4032-a2ad-fa092846ce43').
narrative_ontology:cs_reading_relation('d79b60f2-15cc-4032-a2ad-fa092846ce43', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('d79b60f2-15cc-4032-a2ad-fa092846ce43', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('d79b60f2-15cc-4032-a2ad-fa092846ce43', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('d79b60f2-15cc-4032-a2ad-fa092846ce43', foundational, study_preserves_identity_without_binding).
narrative_ontology:cs_axiom_status(study_preserves_identity_without_binding, holdable).
narrative_ontology:cs_axiom_grounding('d79b60f2-15cc-4032-a2ad-fa092846ce43', study_preserves_identity_without_binding, conventional).
narrative_ontology:cs_axiom('d79b60f2-15cc-4032-a2ad-fa092846ce43', foundational, sacrifice_text_as_historical_memory).
narrative_ontology:cs_axiom_status(sacrifice_text_as_historical_memory, holdable).
narrative_ontology:cs_axiom_grounding('d79b60f2-15cc-4032-a2ad-fa092846ce43', sacrifice_text_as_historical_memory, conventional).
narrative_ontology:cs_reference_frame('d79b60f2-15cc-4032-a2ad-fa092846ce43', cultural_archive_preservation).
narrative_ontology:cs_drift_state('d79b60f2-15cc-4032-a2ad-fa092846ce43', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d79b60f2-15cc-4032-a2ad-fa092846ce43', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, torah_study_communities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_memory_preservation).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, textual_continuity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in voluntary study of sacrificial law tractates as part of religious education and cultural continuity; derive identity reinforcement and historical connection; participation is elective and carries no punitive consequence for disengagement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, torah_study_communities, beneficiary,
    moderate, generational, mobile, global).

% Organize curricula, publish commentaries, and lead study cycles focused on sacrificial law; act as pedagogical leaders without coercive enforcement; maintain the archive's accessibility and transmission.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, educational_stewards, agenda_setter,
    organized, generational, mobile, national).

% Hold competing normative readings in which sacrifice law implies binding halakhic obligation; observe the symbolic archive reading as a descriptive cultural stance rather than a normative legal position; do not govern or enforce this reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, traditional_halakhic_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving textual knowledge and collective identity across generations where the physical practice of Temple sacrifice is impossible; solving the collective-action problem of maintaining a complex legal-historical archive through distributed voluntary study.
% TRANSFER_FUNCTION: Moves time, attention, and interpretive labor from individuals and communities into the maintenance of a shared cultural archive; no material extraction or coerced obligation.
% ABSENT_VOICES: Those who view study as insufficient without messianic restoration or physical performance hold competing readings; they are present in the broader discourse but are not parties to this specific non-coercive constraint.
% DISAPPEARANCE_RATIONALE: If the archive and its study practice vanished overnight, Jewish educational curricula and identity practices organized around Talmudic preservation would lose a key textual anchor; study communities would reorganize around other tractates or cultural practices, and a distinctive mode of memory-preservation would disappear.
% FOUNDING_PROBLEM: The destruction of the Temple and cessation of physical sacrificial practice created a risk of liturgical and legal amnesia; the problem was how to preserve the textual tradition and communal memory without the physical institution.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of religion and Jewish cultural studies attest the preservation problem from outside the beneficiary community; traditional halakhic authorities from competing readings contest that archival study solves the problem, arguing instead that the obligation remains unfulfilled and suspended.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.04, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.04) because participation is voluntary and no agent is compelled to transfer resources under threat. Suppression is minimal (0.02) because alternatives to this practice are not suppressed; individuals may engage with other tractates or disengage entirely. Theater ratio is very low (0.05) because the study activity is functional for memory preservation rather than performative maintenance of an atrophied structure. Accessibility collapse is low (0.15) because alternatives to this mode of preservation exist, and resistance is negligible (0.02) because no party is harmed by the practice. The measurement series is flat across the interval, reflecting the stable, non-coercive character of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The study community and educational stewards experience the constraint as a benign identity resource with low directionality (near-beneficiary). Traditional halakhic authorities, holding competing readings, observe it from an analytical distance with no personal extraction; their seat computes as observer. There is no payer seat because no agent is structurally targeted. The engine should compute rope from all engaged seats and observer from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The sole declared beneficiary is torah_study_communities, which drives directionality toward the beneficiary end (low d) for participants. Educational stewards, while administering the archive, do not extract from participants; their directionality remains low. Traditional halakhic authorities are not victims of this constraintâthey are external observers holding a different readingâso no high-d seat is produced by the structural data. The absence of a victim array is structurally accurate for a rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling this voluntary cultural practice as extraction or as an atrophied mandate (piton). There is no mandate that has outlived its function; the function is ongoing preservation, and the mechanism (study) is well-aligned with that function. A piton reading would require theatrical maintenance of a dead function with diffuse costs, which is not present here. A snare reading would require identifiable victims and coercion, which are absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_normativity_drift,
    'Does the symbolic archive reading structurally drift toward claiming halakhic normativity when institutionalized in yeshiva curricula that treat tractate study as mandatory?',
    'Comparative study of yeshiva curricula and student self-reporting over time; tracking whether curricular mandatory status correlates with normative claim-making or enforcement patterns.',
    'If drift occurs, the constraint would shift from rope toward tangled_rope or scaffold as institutional enforcement layers onto voluntary practice, raising extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_normativity_drift, empirical, 'Stability of archive/non-normative boundary under institutionalization').

omega_variable(
    kernel_reading_containment,
    'Can the symbolic archive reading persist as a stable attractor, or will it be subsumed by messianic suspension or performance-oriented readings as political or theological conditions change?',
    'Tracking prevalence and institutional support for this reading across denominational and geographic communities over generational time.',
    'If subsumed, this constraint dissolves into a different reading (different constraint ID); persistence confirms it as a stable rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Long-term stability of the archive reading within the kernel family').

omega_variable(
    committer_framing_ambiguity,
    'Is the symbolic archive reading best framed as a reading of the halakhic kernel itself, or as a separate non-halakhic cultural practice that borrows the kernel''s text?',
    'Ethnographic analysis of how practitioners categorize their own activity (halakhic vs. cultural-historical) and whether they treat the text as lineage-authoritative.',
    'If practitioners frame it as halakhic, the reading is a genuine commitment-system reading of the kernel; if purely cultural, it may warrant omission of CS structure entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, conceptual, 'Whether the practice is a kernel reading or external cultural borrowing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 12, 0.05).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement(sacr_tr_t36, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 36, 0.05).
narrative_ontology:measurement(sacr_tr_t48, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 48, 0.05).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 60, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 12, 0.04).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 24, 0.04).
narrative_ontology:measurement(sacr_be_t36, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 36, 0.04).
narrative_ontology:measurement(sacr_be_t48, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 48, 0.04).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 60, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel. The kernel decomposes into structurally distinct constraints because each reading assigns a different epsilon, beneficiary/victim structure, and type. This reading (symbolic_archive) claims zero extractiveness and rope classification; siblings claim varying degrees of normativity, obligation, and enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
