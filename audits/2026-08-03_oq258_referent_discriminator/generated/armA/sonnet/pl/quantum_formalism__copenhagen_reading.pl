% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading of Wavefunction Collapse (Measurement as Primitive Epistemic Boundary)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story instantiates ONLY the Copenhagen reading of the quantum
 *   formalism kernel: the claim that wavefunction collapse names a real
 *   physical process (or an absolute, non-eliminable epistemic boundary)
 *   triggered by measurement, and that indeterminism at measurement is
 *   irreducible rather than apparent. It does not evaluate many-worlds or
 *   pilot-wave readings — those are separate constraints
 *   (many_worlds_reading, pilot_wave_reading) linked via
 *   network.affects_constraints. The ε authored here describes the Copenhagen
 *   reading's own operation: how much interpretive/institutional extraction
 *   accrues to treating this one reading as the discipline's operational
 *   default, assessed by the reading's own lights on the standing arrangement
 *   (its institutional dominance), not by comparison to a rival ontology.
 *
 * KEY AGENTS:
 *   - operationalist_physics_pedagogy: institutional agenda_setter administering what counts as settled quantum mechanics
 *   - instrumentalist_textbook_publishers: organized beneficiary of a cheap, closed-question presentation
 *   - measurement_problem_avoidance_researchers: organized beneficiary/agenda_setter whose careers proceed by treating measurement as an unanalyzed primitive
 *   - realist_foundations_researchers: moderate-power payer bearing the burden of arguing against an entrenched default
 *   - graduate_students_taught_collapse_as_settled: powerless payer, epistemically formed before evaluating the kernel contest
 *   - quantum_foundations_philosophers: analytical observer of the postulate's structural status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.34).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.42).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading of Wavefunction Collapse (Measurement as Primitive Epistemic Boundary)").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '87bdcba1-063a-4ff4-99b4-6d7cc93e201c').
narrative_ontology:cs_kernel_codification('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', distributed).
narrative_ontology:cs_authority_grounding('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', practice).
narrative_ontology:cs_interpretation_layer_present('87bdcba1-063a-4ff4-99b4-6d7cc93e201c').
narrative_ontology:cs_reading_relation('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', foundational, measurement_as_irreducible_ontological_primitive).
narrative_ontology:cs_axiom_status(measurement_as_irreducible_ontological_primitive, holdable).
narrative_ontology:cs_axiom_grounding('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', measurement_as_irreducible_ontological_primitive, empirically_contingent).
narrative_ontology:cs_axiom('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', foundational, indeterminism_is_physically_fundamental_not_epistemic_artifact).
narrative_ontology:cs_axiom_status(indeterminism_is_physically_fundamental_not_epistemic_artifact, holdable).
narrative_ontology:cs_axiom_grounding('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', indeterminism_is_physically_fundamental_not_epistemic_artifact, empirically_contingent).
narrative_ontology:cs_axiom('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', secondary, observer_role_non_eliminable_from_physical_description).
narrative_ontology:cs_axiom_status(observer_role_non_eliminable_from_physical_description, holdable).
narrative_ontology:cs_axiom_grounding('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', observer_role_non_eliminable_from_physical_description, conventional).
narrative_ontology:cs_reference_frame('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', copenhagen_operational_consensus_1927).
narrative_ontology:cs_drift_state('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', post_decoherence_theory_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87bdcba1-063a-4ff4-99b4-6d7cc93e201c', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operationalist_physics_pedagogy).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, instrumentalist_textbook_publishers).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, measurement_problem_avoidance_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, realist_foundations_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, graduate_students_taught_collapse_as_settled).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, cross_program_interpretive_dialogue).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, operational_completeness_of_quantum_mechanics).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_as_non_derivable_primitive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Standard undergraduate and graduate curricula present collapse-on-measurement as the default, often the only, account taught. This administers what counts as 'understanding quantum mechanics' for the discipline and sets the terms on which alternative readings must justify themselves as deviations from a settled baseline.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operationalist_physics_pedagogy, agenda_setter,
    institutional, generational, arbitrage, global).

% Textbooks and problem sets built around 'shut up and calculate' plus collapse postulate are cheaper to write, easier to test, and already institutionally entrenched. They benefit from treating the measurement postulate as closed rather than as one contested reading among several, since revision costs are borne by others.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, instrumentalist_textbook_publishers, beneficiary,
    organized, generational, mobile, global).

% Careers built on treating measurement as an unanalyzed primitive can proceed with experimental and applied work (quantum information, metrology) without resolving foundational disputes. Treating the boundary as settled physical fact rather than interpretive commitment removes a source of career risk and grant-proposal friction.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, measurement_problem_avoidance_researchers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, measurement_problem_avoidance_researchers, agenda_setter).

% Physicists and philosophers pursuing many-worlds, pilot-wave, or objective-collapse programs must argue against an entrenched default that is taught as though it were not itself an interpretive stance. Publication venues, hiring committees, and textbook framing default to Copenhagen-flavored operationalism, raising the evidentiary bar specifically for rival ontological programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_foundations_researchers, payer,
    moderate, biographical, constrained, global).

% Enter the field having been told the measurement problem is either solved or a philosophical distraction, without exposure to the kernel contest. Their subsequent research orientation, and their felt sense of what counts as a real physics question, is shaped before they are equipped to evaluate the interpretive commitment they inherited.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, graduate_students_taught_collapse_as_settled, payer,
    powerless, biographical, trapped, national).

% A genuinely open comparative assessment of collapse, branching, and hidden-variable ontologies is structurally sidelined when one reading occupies the position of 'what the formalism just says.' This is not a person but a possible discourse that does not get convened because its convening would cost the entrenched reading its default status.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, cross_program_interpretive_dialogue, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(quantum_formalism__copenhagen_reading, cross_program_interpretive_dialogue).

% Analyze the structural status of the measurement postulate itself — whether it names a physical process, an epistemic limit, or an unresolved gap papered over by convention. Not bound by disciplinary incentives to treat the kernel as closed.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_foundations_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides working physicists with a stable, operational recipe (state preparation, unitary evolution, measurement postulate, Born rule) that lets quantum mechanics be taught, computed, and applied without every practitioner resolving the interpretive question first. This is a genuine coordination service — it lets quantum information science, metrology, and applied physics proceed.
% TRANSFER_FUNCTION: Moves interpretive authority and default-status from a genuinely open question (what does measurement do, ontologically) to one reading, at the cost of alternative research programs' visibility, funding competitiveness, and the epistemic formation of students who are not told the postulate is contested.
% ABSENT_VOICES: Many-worlds and pilot-wave theorists are present in the literature but structurally treated as the burden-bearing challengers to a default, rather than as co-equal readings of an open kernel; students are the least-represented voice, since the contest is rarely disclosed to them as a contest at all.
% DISAPPEARANCE_RATIONALE: If Copenhagen's default status vanished overnight, working experimentalists could likely continue calculating (the formalism's operational core does not require the collapse postulate to be read realistically). But pedagogy, hiring signaling, and the self-image of 'mainstream' quantum mechanics would visibly reorganize — some in the field would say nothing of substance changes (the math is untouched), others would say the entire disciplinary common-sense about what quantum mechanics IS would be up for renegotiation.
% FOUNDING_PROBLEM: Early 20th-century physicists needed a working rule to connect the abstract formalism (wavefunctions, operators) to definite experimental outcomes, without a completed theory of measurement apparatus or observer. The collapse postulate and appeal to an epistemic/physical boundary at measurement provided that working rule.
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of physics working outside the operationalist mainstream (e.g., foundations-of-QM literature reviewing the measurement problem's persistence since von Neumann) attest that the original problem — connecting formalism to outcomes without an agreed account of measurement — remains open, not solved; it argue the postulate functions today as an institutionalized placeholder rather than a settled physical mechanism. Proponents within the operationalist tradition itself, by contrast, generally treat the matter as functionally closed for practical purposes.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.34, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.34) is moderate: the reading does real coordination work (it lets calculation proceed) but also extracts default-status advantage for one ontological commitment over open rivals, at cost to rival programs' visibility and to student epistemic formation. Suppression (0.42) reflects institutional/pedagogical entrenchment (what gets taught as 'the' interpretation) more than active coercion — there is no legal bar on holding many-worlds or pilot-wave views, but there is a strong default-status asymmetry maintained by curriculum, hiring signaling, and textbook conventions. Theater ratio rises from 0.05 (1927, genuinely provisional working rule under live debate at Solvay) to 0.28 (2025, textbook presentation increasingly ritualized as settled fact despite the measurement problem remaining formally open in the literature) — this is the Goodhart-style drift the measurement series is built to expose: the postulate's function shifted from 'working rule under active dispute' toward 'assumed background fact,' while the underlying interpretive question did not actually resolve.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting pedagogical seat, the postulate is simply how quantum mechanics is taught and used — genuine, working coordination. From the realist-foundations payer seat, the same arrangement operates as an entrenched default that structurally disadvantages rival, equally rigorous ontological programs. The engine computes this divergence from the declared power/exit structure; the claimed_type (tangled_rope) is authored because both a real coordination function (teachable, computable formalism) and a real asymmetric cost (foreclosed default status for rivals) are structurally present together, which is exactly the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional pedagogy and textbook publishers sit near the beneficiary end: they collect stability, teachability, and reduced revision cost from treating the postulate as closed. Researchers whose programs require measurement to remain an open, unanalyzed primitive (rather than the target of a deeper reduction) also benefit from Copenhagen's default status, since it forecloses pressure to engage foundational alternatives. Realist foundations researchers and students sit near the target end: the former bear an asymmetric argumentative burden their rivals do not; the latter absorb a settled-fact framing before they can assess it as one reading among three live, structurally distinct positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — connecting formalism to observed outcomes without a completed measurement theory — remains genuinely live in the foundations literature (von Neumann's cut has not been resolved by consensus), yet the operational convenience the postulate provides has, in much of mainstream pedagogy, been treated as though it settled the ontological question too. This is exactly the mismatch the founding_problem_status (contested) plus disappearance_verdict (contested) pairing is meant to surface: the arrangement's practical function persists cleanly, but its ontological warrant does not automatically travel with it. Classifying this as tangled_rope rather than mountain prevents the error of mistaking institutional/pedagogical entrenchment for physical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_physical_or_epistemic_boundary,
    'Does the Copenhagen reading''s ''collapse'' name an actual physical process in the world, or is it better understood as marking the limit of a particular epistemic/operational framework applied to quantum systems?',
    'No known experiment currently distinguishes Copenhagen''s collapse postulate from unitary-only alternatives (many-worlds) at the level of observable predictions; resolution would require either a proposed experimental signature of objective collapse (cf. objective-collapse models like GRW, which ARE empirically distinguishable in principle) or a philosophical argument establishing that the distinction is not empirically meaningful at all.',
    'If collapse is genuinely physical, the Copenhagen reading''s claim to mark an ''absolute epistemic boundary'' understates its own ontological commitment (it would be a physical claim, testable in principle). If collapse is not physical but merely a bookkeeping convenience for updating a probability distribution given new information, the reading''s institutional dominance would need to be justified purely on pedagogical/operational grounds, sharply reducing its claim to describe reality rather than to describe a convention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_physical_or_epistemic_boundary, conceptual, 'Whether the Copenhagen collapse postulate is a physical-process claim or a framework-relative epistemic claim.').

omega_variable(
    beneficiary_structure_natural_or_constructed,
    'Is the Copenhagen reading''s disciplinary default status a natural consequence of it being the historically first, empirically adequate account (in which case beneficiary structure is incidental), or is it a constructed, actively maintained default that happens to also serve identifiable institutional interests (textbook economics, career-path stability)?',
    'Comparative history-of-science analysis of how quickly and by what mechanisms rival interpretations (Bohmian mechanics, 1952; Everett, 1957) were marginalized relative to their empirical and mathematical merits at the time, versus how much of the marginalization tracked genuine explanatory superiority.',
    'If constructed, the tangled_rope classification is strongly supported (coordination function riding alongside identifiable institutional beneficiaries). If naturally emergent from genuine explanatory or historical priority, the classification should weight more heavily toward a rope with incidental beneficiaries rather than tangled extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_natural_or_constructed, empirical, 'Whether Copenhagen''s institutional dominance reflects genuine merit or constructed path-dependence.').

omega_variable(
    reading_relation_framing_ambiguity,
    'Is the Copenhagen/many-worlds relationship best modeled as coexists_with (both remain live scientific positions) or as a deeper forecloses relationship, given that Copenhagen''s commitment to irreducible indeterminism and many-worlds'' commitment to strict unitary determinism appear to be direct contradictories at the level of what happens to the wavefunction during measurement?',
    'A careful philosophy-of-science analysis of whether ''irreducible indeterminism at measurement'' and ''no collapse ever occurs, only branching'' can both be true of the same physical system under different descriptions, or whether accepting one requires denying the other as a matter of logic rather than mere scientific preference.',
    'If the relation is genuinely forecloses rather than coexists_with, that would indicate the two readings cannot be held within one coherent scientific framework simultaneously (unlike, say, two co-existing legal readings of an ambiguous statute) — a stronger claim than this story currently authors. This story treats the readings as coexisting within the discipline''s plural framework (different working scientists hold different readings) while acknowledging the underlying philosophical tension is closer to contradiction than mere disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_framing_ambiguity, conceptual, 'Whether Copenhagen and many-worlds coexist as scientific positions or are logically foreclosing at the framework level.').


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
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__copenhagen_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__copenhagen_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.15).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.26).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__copenhagen_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__copenhagen_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.2).
narrative_ontology:measurement(quan_su_t1950, quantum_formalism__copenhagen_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__copenhagen_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(quan_su_t1990, quantum_formalism__copenhagen_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__copenhagen_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__copenhagen_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the quantum_formalism kernel family. copenhagen_reading (this story): measurement as ontological primitive, irreducible indeterminism, tangled_rope (operational coordination plus interpretive default-status extraction). many_worlds_reading: universal deterministic unitary evolution, apparent branching, no collapse — structurally distinct beneficiary/victim profile (favors different research communities). pilot_wave_reading: deterministic hidden-variable trajectories, physical pilot wave — restores classical-style ontology at the cost of nonlocal guidance equations. All three share the identical experimentally-verified formalism (state vectors, unitary evolution, Born rule statistics) and are empirically indistinguishable from one another under current experimental capability; they differ irreducibly in what they claim happens at measurement. Per the ε-invariance principle, each is authored as a separate constraint with its own ε, beneficiary structure, and type, rather than as one story with an interpretation parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
