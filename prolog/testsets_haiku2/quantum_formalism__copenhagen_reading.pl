% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading: Measurement as Ontological Boundary
 *   domain: physical/philosophical
 *
 * SUMMARY:
 *   The Copenhagen reading of quantum mechanics posits that wavefunction
 *   collapse is a real physical process marking an absolute epistemic
 *   boundary: before measurement, the quantum system evolves
 *   deterministically under the Schrödinger equation; at measurement, the
 *   wavefunction undergoes indeterminate collapse to an eigenstate.
 *   Measurement itself is treated as a primitive ontological category — the
 *   interaction between observer and observed that actuates the transition
 *   from potentiality to actuality. This reading claims mountain status:
 *   measurement-induced indeterminism and the necessity of observer role are
 *   presented as irreducible features of physical reality, not choices or
 *   conventions. However, the reading is contested by two sibling
 *   interpretations (many-worlds and pilot-wave) that claim to preserve
 *   determinism or recover hidden variables. The measurement problem remains
 *   unsolved: how does a deterministic equation yield definite outcomes?
 *   Copenhagen dissolves it by making collapse primitive; the alternatives
 *   deny collapse altogether. The false-summit omega documents the
 *   irreducible ambiguity: is the Copenhagen reading a genuine natural law,
 *   or an institutional consensus that benefits research programs claiming to
 *   have solved the measurement problem when the problem remains open?
 *
 * KEY AGENTS:
 *   - Copenhagen interpretation research programs: derive legitimacy from the reading's framework; benefit from institutional entrenchment in pedagogy and standard laboratory practice
 *   - Operational quantum mechanics community: use Copenhagen-framed calculation; benefit from collapse postulate's computational simplicity
 *   - Many-worlds research programs: excluded from mainstream resources; claim collapse is physically unmotivated and determinism is preservable
 *   - Pilot-wave research programs: excluded and underfunded; claim hidden variables recover classical ontology and determinism
 *   - Foundational physics community: arbitrates empirical claims; investigates whether any reading is justified
 *   - The measurement problem itself: the open foundational puzzle all three readings claim to solve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.31).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.18).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading: Measurement as Ontological Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "physical/philosophical").

domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'bc7be138-2c73-4885-85ae-de34f329b21b').
narrative_ontology:cs_kernel_codification('bc7be138-2c73-4885-85ae-de34f329b21b', fixed_text).
narrative_ontology:cs_authority_grounding('bc7be138-2c73-4885-85ae-de34f329b21b', expertise).
narrative_ontology:cs_interpretation_layer_present('bc7be138-2c73-4885-85ae-de34f329b21b').
narrative_ontology:cs_reading_relation('bc7be138-2c73-4885-85ae-de34f329b21b', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc7be138-2c73-4885-85ae-de34f329b21b', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('bc7be138-2c73-4885-85ae-de34f329b21b', foundational, measurement_irreducible_primitive).
narrative_ontology:cs_axiom_status(measurement_irreducible_primitive, holdable).
narrative_ontology:cs_axiom_grounding('bc7be138-2c73-4885-85ae-de34f329b21b', measurement_irreducible_primitive, deontological).
narrative_ontology:cs_axiom('bc7be138-2c73-4885-85ae-de34f329b21b', foundational, indeterminism_at_collapse_boundary).
narrative_ontology:cs_axiom_status(indeterminism_at_collapse_boundary, holdable).
narrative_ontology:cs_axiom_grounding('bc7be138-2c73-4885-85ae-de34f329b21b', indeterminism_at_collapse_boundary, empirically_contingent).
narrative_ontology:cs_axiom('bc7be138-2c73-4885-85ae-de34f329b21b', secondary, observer_role_non_eliminable).
narrative_ontology:cs_axiom_status(observer_role_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('bc7be138-2c73-4885-85ae-de34f329b21b', observer_role_non_eliminable, deontological).
narrative_ontology:cs_reference_frame('bc7be138-2c73-4885-85ae-de34f329b21b', measurement_as_primitive_indeterminism).
narrative_ontology:cs_drift_state('bc7be138-2c73-4885-85ae-de34f329b21b', contemporary_quantum_foundations_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc7be138-2c73-4885-85ae-de34f329b21b', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_interpretation_research_programs).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operational_quantum_mechanics_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives legitimacy and research direction from the Copenhagen reading's framework. Employs wave-function collapse, measurement as primitive category, and irreducible indeterminism as foundational ontological commitments. Gains from the reading's institutional entrenchment in quantum mechanics pedagogy and standard laboratory interpretation. Would face foundational restructuring if the reading were abandoned.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_interpretation_research_programs, beneficiary,
    organized, civilizational, constrained, global).

% Uses Copenhagen-framed calculation and prediction: collapse postulate simplifies practical computation, measurement-induced randomness aligns with empirical uncertainty bounds, no need to track hidden variables or parallel branches. The reading vindicates working-physicist pragmatism — 'shut up and calculate' operationalism.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operational_quantum_mechanics_community, beneficiary,
    institutional, biographical, mobile, global).

% Are marginalized by Copenhagen institutional dominance but maintain live alternative interpretation. They argue collapse is physically unmotivated and that determinism can be preserved via branching. Would gain legitimacy if collapse's empirical support erodes.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, many_worlds_research_programs, excluded,
    organized, civilizational, trapped, global).

% Argue determinism and classical particle ontology are recoverable through hidden variables and guidance laws. Are systematically underfunded and institutionally marginalized relative to Copenhagen and many-worlds. Would gain research resources and legitimacy if hidden-variable programs were treated as empirically equivalent.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, pilot_wave_research_programs, excluded,
    organized, civilizational, trapped, global).

% Investigates which reading (if any) is empirically justified, ontologically economical, or both. Conducts Bell tests, entanglement experiments, foundational axiomatics. Arbitrates empirical claims but does not control institutional resource allocation to interpretations.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_physics_community, observer,
    institutional, civilizational, analytical, global).

% The unsolved foundational puzzle: how does the deterministic Schrödinger equation yield definite outcomes at measurement? All three readings claim to dissolve it; the Copenhagen reading does so by making measurement primitive and abandoning determinism at the boundary. The measurement problem itself is not an actor but a constraint that structures all three readings' claims.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, measurement_problem, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quantum_formalism__copenhagen_reading, measurement_problem).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function. This is a constraint on the structure of physical reality and our epistemic access to it, not an arrangement between agents.
% TRANSFER_FUNCTION: No transfer function. The constraint is about the nature of indeterminism and measurement, not about resource flows.
% ABSENT_VOICES: Pilot-wave and many-worlds researchers; they would challenge the Copenhagen reading's claim that measurement is an irreducible ontological category and defend either hidden variables or branching as alternatives. They are structurally excluded from the institutional consensus that treats Copenhagen operationalism as the standard framework.
% DISAPPEARANCE_RATIONALE: The constraint is about the structure of physical reality itself — the relationship between quantum state evolution, measurement, and indeterminism. If this reading were abandoned, reality's structure would not rearrange; the reading would simply be replaced by a different interpretation of the same experiments. The quantum phenomena themselves would persist; only the ontological framework interpreting them would shift.
% FOUNDING_PROBLEM: Early quantum mechanics produced the measurement problem: Schrödinger's equation is deterministic and reversible, but measurement yields apparently random outcomes. How can a deterministic equation governing a system's evolution yield indeterminate results? The Copenhagen reading dissolves this by treating measurement as an ontologically primitive event where the deterministic law breaks down.
% FOUNDING_PROBLEM_CORROBORATION: The measurement problem remains open and contested in contemporary foundational physics. Wigner, von Neumann, and Heisenberg attested its severity in the 1920s–1930s; contemporary foundational physicists (Bell, Wallace, Vaidman, Goldstein) from multiple interpretations confirm the problem is unresolved, not that Copenhagen solved it — they dispute whether Copenhagen's answer (making measurement primitive) is correct or merely instrumental.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_unchanged).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__copenhagen_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.31 at interval end) because the constraint describes a feature of physical reality — indeterminism and measurement as primitive — not an arrangement that transfers rents. Suppression is also low (0.18) because the constraint's persistence does not depend on coercion; no agent is forced to adopt Copenhagen by external force, only by institutional convention and pedagogical tradition. Theater ratio is modest (0.22, rising from 0.08) because Copenhagen operationalism emphasizes practical calculation over interpretive justification, but increasingly faces pressure to justify its metaphysical claims (the rise in theater reflects growing debate about whether collapse is real). Accessibility collapse is very high (0.89) because once the measurement problem is understood, alternatives appear structurally blocked: you either accept collapse, or commit to many-worlds branching, or embrace hidden variables — there is no middle ground that avoids all three interpretive moves. Resistance is moderate (0.42) because while Copenhagen dominates institutional practice, active alternative research programs mount steady resistance. The measurement series traces institutional entrenchment (extractiveness rising) combined with growing interpretive scrutiny (theater rising then plateauing), with suppression requirement rising as institutional pressure mounts to defend the reading against alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the Copenhagen research program's seat, the reading is a true description of quantum mechanical structure: measurement is primitive, indeterminism is real, the observer role is essential. From the many-worlds or pilot-wave seats, the same reading is a false claim dressed up as natural law — a consensus masking unresolved alternatives. The engine computes this perspectival gap from the structural data: the Copenhagen program is the beneficiary (gains legitimacy and resource allocation from the reading's institutional status); alternative programs are the excluded payers (lose funding, graduate recruitment, and publication venues to the dominant reading). The accessibility collapse (0.89) reflects institutional entrenchment, not the empirical universality of collapse. The false-summit omega makes explicit the irreducible tension: is the reading a mountain, or a constructed constraint that benefits identifiable research programs while claiming to describe nature?
 *
 * DIRECTIONALITY LOGIC:
 *   The Copenhagen interpretation research programs and operational quantum mechanics community are the beneficiaries: they gain legitimacy, institutional resources, and theoretical coherence from treating the reading as a solved problem (measurement is primitive, collapse is real, indeterminism is fundamental). Alternative research programs (many-worlds, pilot-wave) are the excluded payers: they lose resources and institutional visibility because Copenhagen's entrenchment makes competing interpretations appear marginal or misguided. The foundational physics community is the observer seat: it investigates whether any reading is justified but does not control resource allocation. The directionality derives from the institutional structure, not from empirical fact — the measurement problem remains open, yet Copenhagen receives disproportionate resources as if it were settled. The accessibility collapse (0.89) is high because once you understand that quantum mechanics requires interpretation, you face a forced choice: Copenhagen, many-worlds, pilot-wave, or agnosticism. No middle ground is available. The resistance (0.42) is moderate because while Copenhagen dominates, the alternatives are live enough to mount steady critique. If the measurement problem were solved empirically (via collapse detection, hidden-variable discrimination, or branching evidence), the directionality would shift: whichever reading was vindicated would gain further resources, and the others would lose.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate for the Copenhagen reading was to solve the measurement problem: explain how deterministic dynamics yield definite outcomes. The reading attempts this by making collapse primitive — a new physical law at the measurement boundary. However, the measurement problem remains open; Copenhagen has not solved it, only repositioned it. The founding_problem_status is 'live' because contemporary foundational physicists from all interpretations confirm the problem is unresolved. This is the mandate-function decay that triggers mandatrophy analysis. The reading persists not because it solved its founding problem, but because it became institutionally entrenched — it is the standard framework in textbooks, laboratory practice, and pedagogy. The theater ratio (rising from 0.08 to 0.22) captures this: increasingly, the reading is maintained through argumentative theater (justifying why collapse is real) rather than foundational work (solving the measurement problem). The false-summit omega makes this explicit: Copenhagen may be a constraint that benefits research programs by claiming to have solved the measurement problem when the problem remains open. If the mandate is decay and theater is rising, the reading is a piton candidate — an atrophied function maintained by institutional inertia. However, the classification remains contested (an omega, not settled) because the measurement problem is genuinely open; if collapse were empirically detected or the measurement problem solved, Copenhagen's mandate would be revived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_collapse_empirical_status,
    'Is wavefunction collapse a real physical process, or is it a useful fiction masking deeper deterministic dynamics (hidden variables, branching, or decoherence)?',
    'Direct detection of collapse dynamics (GRW-type spontaneous localization models with testable deviations from standard quantum mechanics); resolution of the measurement problem via an alternative reading (many-worlds or pilot-wave) that preserves determinism; or empirical proof that collapse is not needed to recover observational predictions.',
    'If collapse is not real, the Copenhagen reading''s core ontological claim fails and the reading reclassifies from mountain to contested-extraction-of-legitimacy (an institutional consensus covering a false natural law). If collapse is real, the reading''s mountain status is reinforced. If the measurement problem remains open and empirically indistinguishable, the reading retains institutional entrenchment despite unresolved foundations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_collapse_empirical_status, empirical, 'Whether wavefunction collapse is physically real or theoretically expendable.').

omega_variable(
    measurement_primitive_vs_derived,
    'Is measurement a primitive ontological category that cannot be further reduced, or can measurement interactions be absorbed into the deterministic evolution of the full system?',
    'Successful reduction of measurement to system-environment decoherence (many-worlds path) or hidden-variable guidance (pilot-wave path) without invoking collapse; or proof that no such reduction is possible in principle.',
    'If measurement can be derived from deterministic law plus environment, the Copenhagen reading''s claim to capture something irreducibly primitive fails. If measurement is truly primitive, Copenhagen''s asymmetry (determinism + law below measurement, indeterminism + collapse at measurement) is vindicated as structurally necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_primitive_vs_derived, conceptual, 'Whether measurement is an irreducible primitive or a derived consequence of deterministic dynamics.').

omega_variable(
    observer_role_eliminability,
    'Is the observer an essential feature of quantum mechanics, or is observer-independence recoverable?',
    'Demonstration that all quantum processes (including measurement) can be formalized without invoking observer agency, measurement choice, or consciousness; or proof that some version of observer-dependence is empirically mandatory.',
    'If observer-independence is recovered (via many-worlds or objective-collapse models), the Copenhagen reading''s non-eliminable observer role becomes a theoretical liability. If observer-dependence is genuine, Copenhagen''s framework captures something essential about the relationship between subject and object in quantum mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_role_eliminability, conceptual, 'Whether observer role is ontologically essential or eliminable in principle.').

omega_variable(
    copenhagen_reading_false_summit_ambiguity,
    'Does the Copenhagen reading describe a genuine feature of physical reality (a mountain), or does it benefit from institutional entrenchment while obscuring deeper indeterminacy about the measurement problem?',
    'Establishment of consensus on whether measurement is fundamentally indeterminate (Copenhagen vindicated) or deterministic at a deeper level (hidden variables, branching, or decoherence); empirical discrimination between interpretations via novel experiments; or proof that interpretations are empirically equivalent but ontologically distinct (underdetermination is permanent).',
    'If Copenhagen describes genuine physical structure, the mountain claim holds and the beneficiary declarations are vindicative (propositions, not extractive actors). If Copenhagen is an institutional consensus masking unresolved alternatives, the constraint reclassifies to false-summit (a constructed constraint benefiting research programs that claim natural-law status). The accessibility_collapse and resistance metrics would be reinterpreted: high accessibility collapse reflects institutional entrenchment, not the universality of the constraint itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(copenhagen_reading_false_summit_ambiguity, conceptual, 'Whether Copenhagen reading instantiates a genuine natural law or a false-summit institutional consensus.').

omega_variable(
    kernel_contest_underdetermination,
    'Are the Copenhagen, many-worlds, and pilot-wave readings empirically equivalent, or do they make distinguishing predictions?',
    'Development of experiments that produce different predictions under the three readings; or formal proof that the readings are mathematically equivalent but ontologically distinct (underdetermination is permanent and empirically irrelevant).',
    'If empirically equivalent and permanent, the contest cannot be resolved by evidence alone — the readings occupy different metaphysical commitments that transcend observation. If distinguishing experiments exist, empirical resolution becomes possible and institutional dominance might shift from convention to evidence. The frame of the contest itself (can physics choose between readings?) remains open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_underdetermination, conceptual, 'Whether the three readings are empirically distinguishable or permanently underdetermined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(quan_tr_t20, observed).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__copenhagen_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(quan_tr_t40, observed).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__copenhagen_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(quan_tr_t60, observed).
narrative_ontology:measurement(quan_tr_t80, quantum_formalism__copenhagen_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement_basis(quan_tr_t80, observed).
narrative_ontology:measurement(quan_tr_t100, quantum_formalism__copenhagen_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(quan_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(quan_be_t20, observed).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__copenhagen_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(quan_be_t40, observed).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__copenhagen_reading, base_extractiveness, 60, 0.29).
narrative_ontology:measurement_basis(quan_be_t60, observed).
narrative_ontology:measurement(quan_be_t80, quantum_formalism__copenhagen_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement_basis(quan_be_t80, observed).
narrative_ontology:measurement(quan_be_t100, quantum_formalism__copenhagen_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(quan_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__copenhagen_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement_basis(quan_su_t20, observed).
narrative_ontology:measurement(quan_su_t40, quantum_formalism__copenhagen_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(quan_su_t40, observed).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__copenhagen_reading, suppression_requirement, 60, 0.17).
narrative_ontology:measurement_basis(quan_su_t60, observed).
narrative_ontology:measurement(quan_su_t80, quantum_formalism__copenhagen_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement_basis(quan_su_t80, observed).
narrative_ontology:measurement(quan_su_t100, quantum_formalism__copenhagen_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(quan_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quantum formalism kernel. The kernel is the contested claim that quantum mechanics requires interpretation — that the mathematical formalism underdetermines the physical meaning of wavefunction, superposition, and measurement. The Copenhagen reading instantiates one specific constraint: measurement is primitive, collapse is real, indeterminism is irreducible. The many-worlds and pilot-wave readings instantiate separate constraints that deny all three claims. Each reading has its own ε, its own stakeholder structure, its own classification. They are linked as sibling constraints via network.affects_constraints because they compete to interpret the same kernel. The ε values differ sharply: Copenhagen (0.31 base extractiveness, rising from institutional entrenchment and false-summit ambiguity) versus many-worlds (expected lower extractiveness if determinism is preserved) versus pilot-wave (expected lower extractiveness if hidden variables are recoverable). The readings do not argue about facts; they argue about which ontological framework correctly interprets the same mathematical formalism. Each reading is a constraint story in its own right, with its own beneficiaries, victims (excluded alternatives), and epistemological structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
