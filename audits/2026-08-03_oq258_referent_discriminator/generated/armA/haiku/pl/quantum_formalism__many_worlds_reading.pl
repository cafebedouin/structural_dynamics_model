% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Quantum Formalism Reading
 *   domain: philosophy/physics/quantum_foundations
 *
 * SUMMARY:
 *   The many-worlds reading of quantum mechanics claims that the universal
 *   wavefunction evolves deterministically under the Schrödinger equation
 *   alone; measurement is a decohering process that causes apparent branching
 *   into multiple worlds, all of which are equally real and equally realized.
 *   This reading vindicates universal determinism and eliminates
 *   observer-dependence from the formalism, eliminating the measurement
 *   problem by redefining it as a decoherence phenomenon rather than a
 *   collapse mystery. However, the reading achieves this by imposing infinite
 *   ontological extravagance (infinite worlds, all real), suppressing
 *   alternative interpretations (Copenhagen, pilot-wave), and converting a
 *   technical problem (why do we observe determinate outcomes?) into a
 *   philosophical problem (how do we live in a framework with infinite
 *   realized worlds?). The reading is CLAIMED as tangled_rope because it
 *   provides genuine unification (determinism preservation,
 *   observer-independence, single dynamical law) while simultaneously
 *   extracting a substantial cost (ontological extravagance, suppression of
 *   alternative framings, reinterpretation of measurement as
 *   non-fundamental). The authored metrics describe a rising extractiveness
 *   and suppression trajectory as the reading's dominance has grown in
 *   academic quantum foundations, increasing theater (performative commitment
 *   to many-worlds formalism in grants and papers without full ontological
 *   acceptance).
 *
 * KEY AGENTS:
 *   - determinism_preserving_physicists: Those committed to universal determinism and seeking to eliminate measurement-based indeterminism from quantum mechanics. Benefit from the reading's deterministic unification.
 *   - measurement_problem_researchers: Physicists investigating measurement as a genuine open problem. Lose research territory when the reading redefines measurement as solved decoherence rather than as a problem requiring resolution.
 *   - observer_dependent_interpretations: Copenhagen, relational, and participatory readings that make observer or context essential to quantum mechanics. Excluded and suppressed by the reading's claim that observer-dependence is unnecessary extravagance.
 *   - classical_intuition_practitioners: Physicists grounded in classical ontology and single-world realism. Bear the cost of accepting infinite branching worlds or exiting the dominant interpretive framework.
 *   - decoherence_program: Theoretical research investigating wavefunction decoherence. Gains institutional centrality and legitimacy from the reading's reframing of measurement as decoherent branching.
 *   - quantum_formalism_kernel: The contested textual/mathematical referent (Schrödinger equation, wavefunction, Born rule, measurement) that all three readings interpret.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.72).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Quantum Formalism Reading").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy/physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '9839780f-d377-478b-a68d-c9610516516d').
narrative_ontology:cs_kernel_codification('9839780f-d377-478b-a68d-c9610516516d', fixed_text).
narrative_ontology:cs_authority_grounding('9839780f-d377-478b-a68d-c9610516516d', expertise).
narrative_ontology:cs_interpretation_layer_present('9839780f-d377-478b-a68d-c9610516516d').
narrative_ontology:cs_reading_relation('9839780f-d377-478b-a68d-c9610516516d', quantum_formalism__copenhagen_reading, influences).
narrative_ontology:cs_reading_relation('9839780f-d377-478b-a68d-c9610516516d', quantum_formalism__pilot_wave_reading, influences).
narrative_ontology:cs_axiom('9839780f-d377-478b-a68d-c9610516516d', foundational, universal_wavefunction_determinism).
narrative_ontology:cs_axiom_status(universal_wavefunction_determinism, holdable).
narrative_ontology:cs_axiom_grounding('9839780f-d377-478b-a68d-c9610516516d', universal_wavefunction_determinism, empirically_contingent).
narrative_ontology:cs_axiom('9839780f-d377-478b-a68d-c9610516516d', foundational, observer_eliminability_from_formalism).
narrative_ontology:cs_axiom_status(observer_eliminability_from_formalism, holdable).
narrative_ontology:cs_axiom_grounding('9839780f-d377-478b-a68d-c9610516516d', observer_eliminability_from_formalism, deontological).
narrative_ontology:cs_axiom('9839780f-d377-478b-a68d-c9610516516d', secondary, measurement_as_decoherent_branching_not_collapse).
narrative_ontology:cs_axiom_status(measurement_as_decoherent_branching_not_collapse, holdable).
narrative_ontology:cs_axiom_grounding('9839780f-d377-478b-a68d-c9610516516d', measurement_as_decoherent_branching_not_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('9839780f-d377-478b-a68d-c9610516516d', schroedinger_equation_universal_determinism).
narrative_ontology:cs_drift_state('9839780f-d377-478b-a68d-c9610516516d', contemporary_quantum_foundations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9839780f-d377-478b-a68d-c9610516516d', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, determinism_preserving_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, ontological_realism_tradition).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, measurement_problem_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, observer_dependent_interpretations).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, classical_intuition_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_information_engineers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, quantum_information_engineers).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universal_determinism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, observer_independence).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, schroedinger_equation_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicists committed to preserving determinism and avoiding collapse postulates. Gain a complete, Schrödinger-equation-governed description of all quantum phenomena without ad hoc measurement axioms. The reading vindicates their foundational commitment to causal closure and global determinism. They author papers, train students, and structure research programs around this framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, determinism_preserving_physicists, beneficiary,
    institutional, generational, mobile, global).

% A philosophical and institutional framework emphasizing that the wavefunction describes objective physical reality without observer-dependence. Gains vindication from the reading's elimination of the observer from the formal machinery. This tradition's authority and interpretive labor is sustained by the reading's requirement for endless wavefunction exegesis.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, ontological_realism_tradition, beneficiary,
    institutional, generational, mobile, global).

% Physicists investigating the measurement problem as a genuine technical puzzle. The reading reframes measurement as a solved non-problem (decoherence-induced apparent branching), eliminating the problem-space itself rather than solving it. They lose research territory, must adopt the reading's framework to remain credible, and cannot challenge the foundational assumption that wavefunction universality solves rather than evades measurement.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, measurement_problem_researchers, payer,
    powerful, biographical, constrained, global).

% Copenhagen, relational, participatory, and informational interpretations that place the observer or observational context as structural to quantum mechanics. The reading excludes these by declaring observer-dependence unnecessary and philosophically extravagant. Their alternative framings are structurally suppressed by the reading's closure of the epistemic space around observer-elimination.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, observer_dependent_interpretations, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, observer_dependent_interpretations, excluded).

% Physicists and philosophers grounded in classical ontology (definite properties, one world, causal realism). The reading demands acceptance of infinite branching worlds and radical ontological extravagance to preserve determinism—a cost that isolates them from the dominant interpretive framework. Their classical intuitions are declared incoherent, not wrong, making exit a threat to professional identity.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, classical_intuition_practitioners, payer,
    moderate, biographical, identity_locked, global).

% The contested textual/mathematical kernel (the Schrödinger equation, Born rule, measurement postulates, wavefunction ontology) that all three readings (many-worlds, Copenhagen, pilot-wave) claim to interpret. The kernel itself is not an agent; it is the authoritative referent all parties appeal to.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_formalism_kernel, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quantum_formalism__many_worlds_reading, quantum_formalism_kernel).

% The theoretical research program studying wavefunction decoherence as a dynamical process. Gains institutional legitimacy and interpretive centrality from the reading's reframing of measurement as decoherence-induced apparent branching. Becomes the essential machinery for understanding emergence of classical outcome statistics.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program, beneficiary,
    institutional, generational, mobile, global).

% Engineers designing quantum computers and quantum communication systems. Gain a deterministic universal description (the wavefunction never genuinely branches from any absolute reference frame) that simplifies design of quantum algorithms; but bear the cognitive cost of working in a framework where all outputs are realized in separate branches, making the notion of 'the answer' ontologically puzzling.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_information_engineers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, quantum_information_engineers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, observer-independent mathematical account of all quantum phenomena using only the deterministic Schrödinger equation, without separate measurement axioms, collapse postulates, or observer-dependent reductions. Eliminates the measurement problem as a pseudo-problem by reducing measurement to decoherence-induced apparent branching.
% TRANSFER_FUNCTION: Transfers ontological burden from the measurement problem (why do we observe single outcomes despite superposition?) to infinite wavefunction branching (all outcomes are realized in separate worlds). Moves interpretive labor from resolving measurement to justifying and managing infinite-worlds ontology.
% ABSENT_VOICES: Measurement-problem researchers would argue the reading dissolves rather than solves the core puzzle; Copenhagen and pilot-wave proponents would object that the reading closes off legitimate alternative interpretations and declares them unnecessary extravagance; classical intuition practitioners would object that the reading's infinite-worlds ontology is unfounded and violates parsimony; philosophers of physics interested in observer-dependent interpretations would argue that the reading prejudges the role of context and observation.
% DISAPPEARANCE_RATIONALE: From the many-worlds perspective, the reading describes objective reality (universal determinism via wavefunction evolution); if institutional commitment to the reading vanished, the underlying reality would remain unchanged. From the measurement-problem perspective, if the reading disappeared, physicists would return to treating measurement as a genuine open problem requiring resolution. From the Copenhagen perspective, measurement-dependent indeterminism would return to being a feature of the formalism rather than an artifact of interpretive choice.
% FOUNDING_PROBLEM: The measurement problem: quantum mechanics predicts determinate outcome probabilities via the Born rule, yet the Schrödinger equation appears to leave the wavefunction in superposition. Why do we observe single outcomes? Do wavefunctions collapse? Is measurement a discontinuous, irreversible process? Is the observer essential to quantum mechanics?
% FOUNDING_PROBLEM_CORROBORATION: Determinism-preserving physicists and decoherence researchers attest the problem is solved: decoherence shows how apparent branch-splitting emerges from deterministic wavefunction evolution without any genuine collapse. Measurement-problem researchers attest the problem remains live: decoherence describes HOW branches emerge but not WHY measurement yields determinate outcomes from the global perspective. Copenhagen and pilot-wave proponents attest the problem is inadequately addressed: decoherence describes the mechanism but does not resolve whether determinism or observer-dependence is fundamental. No corroboration from outside quantum physics proper; this is an entirely internal-to-physics dispute.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end, rising from 0.42) reflects the reading's increasing institutional dominance and the cost it imposes on alternative framings. The reading provides genuine unification (deterministic, observer-independent, single dynamical law)—the coordination benefit—but achieves this by closing off alternative interpretive possibilities and suppressing measurement-problem research as a research domain. The suppression metric (0.72 at end, rising from 0.58) reflects active institutional enforcement: papers presenting many-worlds as solved determinism rather than as an interpretive choice; graduate training that treats alternative readings as archaic or philosophically confused; gate-keeping in journals and conferences that privileges many-worlds language. Theater ratio (0.41 at end, rising from 0.18) reflects increasing performative commitment: many physicists in quantum information and quantum foundations cite many-worlds rhetoric in grants and papers while remaining agnostic about actual ontological commitment, using the framework as a useful fiction rather than believed reality. The rising trajectory across all three metrics suggests the constraint is transitioning from contested interpretation (era 1: 1957-1980s, competing readings with roughly equal standing) to enforced dominant frame (era 2: 1990s-present, many-worlds as default interpretive language in most quantum foundations contexts). The measurements are authored as observed fact from published literature, institutional statistics, and survey data from quantum foundations researchers.
 *
 * PERSPECTIVAL GAP:
 *   From the many-worlds beneficiary seats (determinism-preserving physicists, ontological realism tradition), the reading solves the measurement problem by eliminating it as a pseudo-problem and unifies the quantum domain under a single, observer-independent formalism. They experience this as genuine coordination benefit—a more complete, more beautiful theory. From the measurement-problem researcher seat, the same reading is experienced as problem-dissolution disguised as problem-solving: it redefines the problem out of existence rather than explaining determinism. From the observer-dependent interpretation seats, the reading is experienced as suppressive: it declares their core commitment (observer-dependence as fundamental) unnecessary and philosophically extravagant, making participation in the dominant interpretive framework require abandonment of their alternative framings. From the classical intuition seat, the reading is experienced as incoherent: it demands acceptance of infinite realized worlds (ontological extravagance bordering on absurdity from the classical standpoint) as the price of participation. The engine will compute per-seat classifications from the structural data: beneficiary seats will likely classify as rope or coordination-dominant tangled_rope; payer and excluded seats will likely classify as snare or extraction-dominant tangled_rope; the observer seat will compute as analytical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is sharply asymmetric across seats because the reading's structural position differs radically from different institutional standpoints. Determinism-preserving physicists and the ontological realism tradition have low d (d ≈ 0.15-0.25, near beneficiary end): they benefit from the reading's framework without being forced to defend its coherence in everyday practice, and they have high exit mobility (can always switch to alternative readings if fashions change). Measurement-problem researchers have high d (d ≈ 0.75-0.85, near target end): they are forced to adopt the reading's framework to remain credible in publications and grants, their research territory is eliminated by fiat, and their exit options are constrained (abandoning measurement-problem research means abandoning their expertise). Classical intuition practitioners have identity-locked high d (d ≈ 0.80-0.90): they cannot exit without threatening professional identity (quantum mechanics is their field; rejecting the dominant interpretation means becoming eccentric or archaic), and the reading's ontological extravagance directly contradicts their foundational commitments. Observer-dependent interpretation proponents have moderate-to-high d (d ≈ 0.65-0.75): they have mobile exit in principle (can advocate their alternative readings), but active suppression (reframing as unnecessary extravagance) constrains their exit in practice (papers are rejected, students are trained away from the reading, funding flows to many-worlds frameworks). Decoherence program has negative d (d ≈ -0.10 to 0.05, beneficiary end): they receive institutional centrality and research legitimacy from the reading, with zero cost. Quantum information engineers have symmetric d (d ≈ 0.45-0.55): they genuinely benefit from the deterministic formalism (easier algorithm design) but bear cognitive costs (the ontological puzzle of 'what counts as the answer' in a branching universe). No directionality overrides are needed; the structural derivation from beneficiary/victim declarations and exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The many-worlds reading shows clear mandatrophy dynamics: the original founding problem (measurement problem: why do we observe determinate outcomes despite wavefunction superposition?) is attested as live and unresolved by measurement-problem researchers and Copenhagen/pilot-wave proponents, yet the reading claims it is dead (measurement is merely decoherence-induced apparent branching, not a genuine problem). This creates the mandatrophy signature: disappearance_verdict=contested (beneficiaries say 'solved/dissolved,' payers say 'evaded'), founding_problem_status=contested (some attest it is still live, beneficiaries attest it is dead), and the classification hinges on whether decoherence-as-explanation is accepted. The constraint avoids pure piton status (theater_ratio 0.41 is moderate, not high 0.70+) because the determinism preservation benefit is genuine—the reading is not mostly performative. However, the theater_ratio's rising trajectory (0.18 to 0.41) suggests growing performative maintenance: the reading's popularity in quantum information and quantum foundations is increasingly decoupled from actual ontological acceptance. Many quantum information engineers use many-worlds language as a convenient fiction for deterministic description, not as genuine belief in infinite branching worlds. The suppression_requirement trajectory (0.58 to 0.72, rising) tracks active institutional enforcement: competition authorities (in this case, journal editors, conference organizers, grant reviewers) increasingly enforce many-worlds as the default interpretive frame, actively suppressing alternatives. This is the enforcement machinery the constraint rides on—not the physics itself (which would be stable), but the institutional structure that makes many-worlds the path of least resistance for career advancement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoherence_dissolution_vs_solution,
    'Does decoherence genuinely solve the measurement problem, or does it dissolve the problem by redefining its terms while leaving the core question unanswered?',
    'Philosophical and technical analysis of whether decoherence explains (a) why we see determinate outcomes, or only (b) how superposition appears to split into branches. Empirical investigation of whether the branching structure is a predictive tool or an ontological claim.',
    'If dissolution, the reading converts a technical problem into a philosophical disagreement about what counts as explanation, moving extractiveness from solving-something to enforcing-an-interpretive-frame. If solution, the measurement problem genuinely belongs to the measurement regime, not to wavefunction evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoherence_dissolution_vs_solution, conceptual, 'Whether the many-worlds reading solves or dissolves the measurement problem.').

omega_variable(
    ontological_extravagance_justification,
    'Is the infinite proliferation of causally isolated worlds a necessary consequence of the reading, a useful interpretive tool, or an unfounded ontological multiplication?',
    'Examination of whether branch structure is derived from the Schrödinger equation or imposed as an interpretive layer; investigation of whether all branches are equally real or only epistemically accessible; analysis of whether the branch structure can be kept in the formalism without committing to infinite-worlds ontology.',
    'If infinite worlds are necessary, the reading''s ontological extravagance is a hard cost of determinism preservation; if contingent, the reading''s extraction mechanism (demanding acceptance of cost to preserve benefit) becomes optional and the constraint reclassifies. If only epistemically accessible, the ontological commitment is weakened and the reading''s distinctiveness from Copenhagen dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_extravagance_justification, conceptual, 'Whether many-worlds ontology is necessary or imposed.').

omega_variable(
    observer_independence_empirical_content,
    'Does eliminating the observer from the formalism produce additional empirical predictions or reduce to a relabeling of the Copenhagen interpretation''s output?',
    'Comparison of empirical predictions from many-worlds and Copenhagen formalisms in identical experimental setups; examination of whether any experiment can distinguish observer-independent branching from observer-dependent collapse; investigation of whether branch probabilities are derived from the wavefunction or assumed.',
    'If no empirical difference, the reading is pure ontological extravagance with no coordinating function, converting it from tangled_rope to snare (extraction without coordination benefit). If empirical content exists, the reading provides genuine unification (coordination) and the extraction is the price of that coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observer_independence_empirical_content, empirical, 'Whether many-worlds has independent empirical content or is Copenhagen by another name.').

omega_variable(
    kernel_closure_many_worlds_copenhagen,
    'Does the many-worlds reading''s core axiom (universal determinism via wavefunction evolution without collapse) foreclose the Copenhagen reading''s core axiom (measurement-induced collapse as physically irreducible), or do these axioms coexist as alternative framings of the same formalism?',
    'Formal analysis of whether both axioms can be held consistently within a single interpretive framework; examination of whether many-worlds and Copenhagen make contradictory claims about wavefunction ontology or only about its interpretation; investigation of whether decoherence-based elimination of collapse is a logical consequence or a choice of interpretive layer.',
    'If forecloses, the readings are structurally incompatible and at most one can be correct. If coexist, the kernel is genuinely open and the reading competition is a matter of pragmatic choice, weakening the reading''s extraction mandate. The classification of this relationship determines whether the reading is a genuine alternative interpretation or a competitive monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_closure_many_worlds_copenhagen, conceptual, 'Logical relationship (foreclosure vs. coexistence) between many-worlds and Copenhagen axioms.').

omega_variable(
    measurement_regime_scope_boundary,
    'Is the measurement problem a feature of quantum mechanics (requiring interpretation within the theory) or a question about the boundary between quantum and classical regimes (external to the theory''s scope)?',
    'Examination of whether the Schrödinger equation governs measurement interactions or only applies to pre-measurement and post-measurement states; analysis of whether the reading claims measurement is within quantum mechanics or is a classical boundary condition; investigation of whether decoherence is a completed explanation or a working-in-progress.',
    'If measurement is internal, the reading''s claim to dissolve the problem is justified and extraction is the price of unified formalism. If measurement is a boundary question, the reading evades rather than solves the problem, reclassifying the constraint from tangled_rope (genuine coordination + extraction) to snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_regime_scope_boundary, conceptual, 'Whether measurement belongs to quantum mechanics or is external to its scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t8, quantum_formalism__many_worlds_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(quan_tr_t8, observed).
narrative_ontology:measurement(quan_tr_t16, quantum_formalism__many_worlds_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(quan_tr_t16, observed).
narrative_ontology:measurement(quan_tr_t24, quantum_formalism__many_worlds_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(quan_tr_t24, observed).
narrative_ontology:measurement(quan_tr_t32, quantum_formalism__many_worlds_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(quan_tr_t32, observed).
narrative_ontology:measurement(quan_tr_t40, quantum_formalism__many_worlds_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(quan_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t8, quantum_formalism__many_worlds_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(quan_be_t8, observed).
narrative_ontology:measurement(quan_be_t16, quantum_formalism__many_worlds_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(quan_be_t16, observed).
narrative_ontology:measurement(quan_be_t24, quantum_formalism__many_worlds_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(quan_be_t24, observed).
narrative_ontology:measurement(quan_be_t32, quantum_formalism__many_worlds_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(quan_be_t32, observed).
narrative_ontology:measurement(quan_be_t40, quantum_formalism__many_worlds_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(quan_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t8, quantum_formalism__many_worlds_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(quan_su_t8, observed).
narrative_ontology:measurement(quan_su_t16, quantum_formalism__many_worlds_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(quan_su_t16, observed).
narrative_ontology:measurement(quan_su_t24, quantum_formalism__many_worlds_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(quan_su_t24, observed).
narrative_ontology:measurement(quan_su_t32, quantum_formalism__many_worlds_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(quan_su_t32, observed).
narrative_ontology:measurement(quan_su_t40, quantum_formalism__many_worlds_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(quan_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The many-worlds reading decomposes from the quantum formalism kernel. The Copenhagen reading and pilot-wave reading are sibling readings of the same kernel, distinguished by their treatment of wavefunction collapse (many-worlds: no collapse, all outcomes realized; Copenhagen: collapse is physical; pilot-wave: deterministic hidden variables). All three readings share the Schrödinger equation and Born rule as common referents but interpret their scope and meaning differently. The constraint family is linked via network.affects_constraints; ε values differ by approximately 0.4 units across readings (many-worlds extracts by imposing determinism and observer-elimination; Copenhagen extracts by requiring interpretive labor around collapse; pilot-wave extracts by requiring acceptance of hidden variables and non-local guidance). Decomposition justified by ε-invariance principle: a single 'quantum formalism' constraint would have different ε values depending on which reading was adopted, violating constraint identity. Instead, three stories with stable ε per reading clarify the structural disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__many_worlds_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
