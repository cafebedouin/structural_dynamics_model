% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading: Measurement as Primitive Ontological Boundary
 *   domain: philosophy_of_physics
 *
 * SUMMARY:
 *   This story addresses one specific structural claim inside the colloquial
 *   label 'the Copenhagen interpretation': that wavefunction collapse is a
 *   real physical process, that measurement marks an absolute epistemic (and
 *   on strong readings, ontological) boundary, and that indeterminism at
 *   measurement is irreducible rather than apparent. This is NOT a story
 *   about the empirical predictions of quantum mechanics (uncontested,
 *   Mountain-grade across all interpretations) — it is a story about the
 *   institutional and pedagogical dominance of one metaphysical reading of
 *   those predictions over live alternatives. The extraction here is modest
 *   but real: career and curriculum resources flow toward treating this
 *   reading as settled physics rather than as one live interpretation among
 *   several empirically equivalent options.
 *
 * KEY AGENTS:
 *   - copenhagen_aligned_physics_departments: institutional beneficiary/agenda_setter — sets curriculum defaults
 *   - foundations_researchers_outside_mainstream: primary payer — bears career cost of dissent
 *   - graduate_students_seeking_ontological_clarity: powerless payer — inherits unexamined default
 *   - philosophers_of_physics: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.28).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.42).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading: Measurement as Primitive Ontological Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'df5223d7-6e21-476e-b78d-f02cf5b2264d').
narrative_ontology:cs_kernel_codification('df5223d7-6e21-476e-b78d-f02cf5b2264d', distributed).
narrative_ontology:cs_authority_grounding('df5223d7-6e21-476e-b78d-f02cf5b2264d', practice).
narrative_ontology:cs_interpretation_layer_present('df5223d7-6e21-476e-b78d-f02cf5b2264d').
narrative_ontology:cs_reading_relation('df5223d7-6e21-476e-b78d-f02cf5b2264d', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('df5223d7-6e21-476e-b78d-f02cf5b2264d', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('df5223d7-6e21-476e-b78d-f02cf5b2264d', foundational, collapse_is_physically_real).
narrative_ontology:cs_axiom_status(collapse_is_physically_real, holdable).
narrative_ontology:cs_axiom_grounding('df5223d7-6e21-476e-b78d-f02cf5b2264d', collapse_is_physically_real, empirically_contingent).
narrative_ontology:cs_axiom('df5223d7-6e21-476e-b78d-f02cf5b2264d', foundational, measurement_is_ontologically_primitive).
narrative_ontology:cs_axiom_status(measurement_is_ontologically_primitive, holdable).
narrative_ontology:cs_axiom_grounding('df5223d7-6e21-476e-b78d-f02cf5b2264d', measurement_is_ontologically_primitive, conventional).
narrative_ontology:cs_created_at('df5223d7-6e21-476e-b78d-f02cf5b2264d', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_aligned_physics_departments).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operationalist_textbook_publishers).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, standard_model_pedagogy_establishment).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, foundations_researchers_outside_mainstream).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, hidden_variable_and_realist_program_advocates).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, graduate_students_seeking_ontological_clarity).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_problem_is_a_genuine_physical_discontinuity).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_is_non_eliminable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach the standard textbook formalism (Born rule, projection postulate, 'shut up and calculate') as settled physics rather than as one interpretation among several. Curriculum design, hiring committees, and journal gatekeeping in mainstream physics defer to this reading by default, which lowers their own teaching and grant-writing burden relative to engaging live foundational disputes.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_aligned_physics_departments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, copenhagen_aligned_physics_departments, agenda_setter).

% Produce and sell standardized quantum mechanics textbooks that present collapse-on-measurement as the physical story, avoiding the cost and market risk of presenting interpretive pluralism. Benefit from a settled-looking narrative that requires no revision as foundational debates continue unresolved.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operationalist_textbook_publishers, beneficiary,
    organized, biographical, mobile, global).

% Sets qualifying-exam content, licensing exam material, and the default working ontology transmitted to the next generation of physicists. Administers what counts as 'understanding quantum mechanics' for professional certification purposes, entrenching the collapse postulate as pedagogical bedrock rather than as a contested philosophical commitment.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, standard_model_pedagogy_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Work on many-worlds, pilot-wave, objective-collapse, and relational interpretations but face reduced publication venues, fewer faculty lines, and characterization as 'merely philosophical' rather than doing physics. Their exit options are constrained by career dependence on physics departments still organized around the Copenhagen default.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundations_researchers_outside_mainstream, payer,
    moderate, biographical, constrained, global).

% Argue that treating measurement as an irreducible primitive forecloses legitimate research into deterministic completions of quantum theory. Historically marginalized (post-Bohr, pre-Bell) to the point of near-disappearance from mainstream training; still bear reputational cost for raising the question.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, hidden_variable_and_realist_program_advocates, payer,
    powerless, biographical, trapped, national).

% Are taught the collapse postulate as physical fact without exposure to the interpretive contest, then must either accept it uncritically to pass exams and advance, or spend unrewarded time and reputational capital investigating foundations, which their advisors and committees may not value.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, graduate_students_seeking_ontological_clarity, payer,
    powerless, biographical, trapped, national).

% Study the interpretive landscape without a stake in which reading wins, documenting how each reading's ontological commitments differ and how institutional incentives shape which reading gets taught as default.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophers_of_physics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides physicists with a single operational recipe (Born rule + projection postulate) that lets experimentalists compute predictions and communicate results without resolving deep ontological disputes — a genuinely useful calculational convention.
% TRANSFER_FUNCTION: Moves institutional legitimacy, teaching time, publication venues, and career viability toward physicists and departments that treat collapse-on-measurement as settled fact, and away from researchers pursuing deterministic or no-collapse alternatives.
% ABSENT_VOICES: Pilot-wave and many-worlds theorists are structurally present in the debate but underrepresented in undergraduate and qualifying-exam curricula; working physicists who never encounter the interpretive contest at all are the largest absent voice, since the pedagogical default is presented to them as though no contest exists.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading's institutional dominance vanished overnight, working physics — the equations and predictions — would be entirely unchanged, since all interpretations agree on the empirical predictions. What would rearrange is pedagogy, hiring, and prestige allocation in foundations research; Copenhagen-aligned departments dispute that anything would meaningfully change, while foundations researchers argue the field would open substantially.
% FOUNDING_PROBLEM: Early quantum mechanics (1920s-30s) needed an operational framework that let physicists make and verify predictions without waiting for a resolved ontology — Bohr and Heisenberg's instrumentalist stance let calculation proceed while metaphysics remained open.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science (outside both the Copenhagen-aligned institutions and the dissenting foundations community) attest that the original Copenhagen position was more epistemically modest than its later pedagogical calcification — Bohr's own writings hedge more than modern textbooks do. This corroboration comes from science historians with no stake in which interpretation wins, and it supports the reading that the founding operational-modesty problem has been quietly replaced by an ontological-realist claim that was never actually settled.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).
:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28) because the primary function — giving physicists a working calculational recipe — is genuinely useful and the extraction rides alongside it rather than replacing it; this is not a pure rent-seeking arrangement. Suppression (0.42) reflects real but partial gatekeeping: dissenting interpretations are published, debated, and taught in specialized courses, but are demoted from default status in ordinary curricula and qualifying exams. Accessibility collapse (0.55) and resistance (0.55) sit at moderate levels because the interpretive alternatives are neither fully suppressed (they exist, well-funded research programs pursue them) nor fully accessible to the ordinary physics student, who typically never learns the interpretations are contested at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Copenhagen-aligned departments and pedagogy institutions are the structural beneficiaries: they set the default, bear no cost for the metaphysical ambiguity, and benefit from a curriculum that requires no engagement with unresolved foundations. Foundations researchers, hidden-variable advocates, and unprepared graduate students are the targets: they either pay a career cost for dissent or inherit a metaphysically loaded default without being told it is contested. The tangled-rope character is precise: the coordination function (a working calculational standard) is real, and the extraction (institutional capture of what counts as 'understanding' quantum mechanics) rides on top of it via the same mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an operational stance that let calculation proceed while ontology stayed open — is largely solved and was never actually about asserting collapse as physically real; Bohr's own instrumentalism was more modest than the pedagogical inheritance. Treating this as a resolved mandatrophy would be wrong, however: the coordination function (shared calculational standard) remains live and valuable, so classifying this as pure Snare would mislabel genuine coordination as pure extraction. Tangled Rope captures both: real coordination value plus asymmetric extraction of institutional legitimacy that has outlived the founding epistemic modesty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_primitive_vs_pedagogical_convenience,
    'Is treating measurement as an irreducible ontological primitive a genuine physical discovery, or a pedagogical simplification that calcified into an unexamined metaphysical default because it was easier to teach than the alternatives?',
    'Historical analysis of how Bohr''s and Heisenberg''s original operationalist claims (epistemically modest) diverged from later textbook presentations (ontologically assertive); comparison with how many-worlds and pilot-wave programs handle the same empirical data without a collapse postulate.',
    'If the collapse postulate is discovered to be substantially a pedagogical artifact rather than an empirically forced conclusion, the extraction component of this reading''s institutional dominance is larger than currently measured, and the coordination story (needed operational convenience) looks more like retroactive justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_primitive_vs_pedagogical_convenience, conceptual, 'Whether the ontological primitiveness of measurement is a physical finding or a curricular convenience that became doctrine.').

omega_variable(
    committer_kernel_disagreement_location,
    'Where exactly does the disagreement between the three quantum_formalism readings live — is it a genuine empirical underdetermination (all three save the phenomena equally), or does one reading (e.g. pilot-wave) actually make different predictions in some regime not yet tested?',
    'Track ongoing experimental programs (e.g. weak measurement, macroscopic superposition tests, decoherence timescale measurements) for any regime where the readings'' predictions diverge, however slightly.',
    'If a genuine empirical divergence is found, this ceases to be a pure interpretive/metaphysical dispute and one reading would gain or lose Mountain-adjacent status; until then, the dispute is properly interpretive, and this story''s classification (institutional extraction riding on an underdetermined metaphysical choice) holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, empirical, 'Whether the sibling readings are strictly empirically equivalent or diverge in some untested regime.').

omega_variable(
    observer_role_eliminability,
    'Is the non-eliminable observer role a structural fact about quantum measurement, or an artifact of choosing a Heisenberg-cut placement that could in principle be pushed arbitrarily far into the macroscopic apparatus (per decoherence-based accounts)?',
    'Decoherence theory and consistent-histories analysis of where and whether a principled cut location can be identified without appeal to consciousness or measurement-as-primitive.',
    'If the cut is shown to be conventional rather than principled, the Copenhagen reading''s claim to an ''absolute epistemic boundary'' loses its strongest structural justification, strengthening the case that this reading''s persistence is institutional rather than physically forced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_role_eliminability, conceptual, 'Whether the observer/measurement primitive is physically forced or a placement convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.05).
narrative_ontology:measurement(quan_tr_t1950, quantum_formalism__copenhagen_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__copenhagen_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__copenhagen_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.15).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__copenhagen_reading, base_extractiveness, 1990, 0.26).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__copenhagen_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__copenhagen_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
