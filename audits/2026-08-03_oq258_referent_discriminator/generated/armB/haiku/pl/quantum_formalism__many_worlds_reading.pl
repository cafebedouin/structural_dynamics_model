% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Many-Worlds Interpretation: Universal Determinism via Decoherence
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The many-worlds reading of quantum mechanics interprets the universal
 *   wavefunction as the complete ontic description of reality, with all
 *   branches of every superposition existing simultaneously in parallel
 *   worlds. The reading is claimed as a rope (genuine coordination solution
 *   to the measurement problem) but authored metrics reflect substantial
 *   extractiveness (0.41 final) and suppression (0.38 final). The divergence
 *   between claim and metrics is deliberate: the constraint APPEARS as
 *   coordination (solves an open problem in quantum foundations), but
 *   operates with asymmetric burden distribution—Copenhagen defenders bear
 *   the cost of defending their position against this alternative;
 *   many-worlds adherents collect conceptual rent by claiming determinism and
 *   observer-elimination; and the empirical indistinguishability of the
 *   readings creates persistent suppression of the alternative framings. The
 *   measurement trajectory shows slow but steady extraction rise over the
 *   70-year interval (1957 Everett to 2026 contemporary status), as the
 *   reading accumulates institutional presence and graduate-level adoption,
 *   incrementally constraining the interpretive discourse.
 *
 * KEY AGENTS:
 *   - interpretive_physicists_many_worlds_school: Defenders of the reading who gain conceptual authority and research trajectory coherence
 *   - copenhagen_school_defenders: Physicists defending collapse-based interpretation against the many-worlds alternative
 *   - measurement_problem_research_community: Investigators of the foundation crisis who use many-worlds to dissolve the problem
 *   - quantum_mechanics_students: Excluded learners who encounter the reading as exotic alternative to textbook Copenhagen
 *   - analytic_philosophy_of_physics: Critical observers evaluating internal consistency and empirical adequacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.41).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.38).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Interpretation: Universal Determinism via Decoherence").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, 'db710f7e-ecd3-4c09-9d69-aebdc580b686').
narrative_ontology:cs_kernel_codification('db710f7e-ecd3-4c09-9d69-aebdc580b686', distributed).
narrative_ontology:cs_authority_grounding('db710f7e-ecd3-4c09-9d69-aebdc580b686', expertise).
narrative_ontology:cs_interpretation_layer_present('db710f7e-ecd3-4c09-9d69-aebdc580b686').
narrative_ontology:cs_reading_relation('db710f7e-ecd3-4c09-9d69-aebdc580b686', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('db710f7e-ecd3-4c09-9d69-aebdc580b686', quantum_formalism__pilot_wave_reading, influences).
narrative_ontology:cs_axiom('db710f7e-ecd3-4c09-9d69-aebdc580b686', foundational, universal_determinism).
narrative_ontology:cs_axiom_status(universal_determinism, holdable).
narrative_ontology:cs_axiom_grounding('db710f7e-ecd3-4c09-9d69-aebdc580b686', universal_determinism, deontological).
narrative_ontology:cs_axiom('db710f7e-ecd3-4c09-9d69-aebdc580b686', foundational, observer_eliminability).
narrative_ontology:cs_axiom_status(observer_eliminability, holdable).
narrative_ontology:cs_axiom_grounding('db710f7e-ecd3-4c09-9d69-aebdc580b686', observer_eliminability, deontological).
narrative_ontology:cs_reference_frame('db710f7e-ecd3-4c09-9d69-aebdc580b686', universal_wavefunction_ontology).
narrative_ontology:cs_drift_state('db710f7e-ecd3-4c09-9d69-aebdc580b686', contemporary_measurement_problem_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('db710f7e-ecd3-4c09-9d69-aebdc580b686', '2026-06-15T14:23:47Z').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, interpretive_physicists_many_worlds_school).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, measurement_problem_research_community).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, copenhagen_school_defenders).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, measurement_problem_research_community).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universal_determinism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, observer_elimination).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, global_lawfulness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicists and philosophers who adopt the many-worlds reading gain intellectual coherence (determinism at the fundamental level, no observer-dependent collapse, no irreducible randomness). They defend the reading as the most straightforward extrapolation of Schrödinger dynamics and publish research programs grounded in it. No direct economic rent; the benefit is conceptual elegance and immunity from the measurement problem.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, interpretive_physicists_many_worlds_school, beneficiary,
    organized, generational, mobile, global).

% Physicists committed to the Copenhagen interpretation (wavefunction collapse as physical process, indeterminism as fundamental) experience the many-worlds reading as a competitive interpretive claim that challenges the conceptual foundations they teach. They bear the cost of defending their framework against the many-worlds alternative in conference presentations, textbooks, and grant review processes.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, copenhagen_school_defenders, payer,
    organized, generational, mobile, global).

% Researchers investigating the measurement problem (the foundation crisis: why does quantum mechanics seem to require an irreducible role for observation?) find in the many-worlds reading a way to dissolve the problem entirely—by denying that measurement induces ontological change, only apparent branching. This generates a coherent research program but also creates a commitment to infinite parallel worlds, which some find epistemically extravagant.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, measurement_problem_research_community, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, measurement_problem_research_community, payer).

% Conduct experiments whose outcomes depend on quantum mechanics regardless of which interpretation is correct. The interpretive dispute does not constrain their experimental design or results; however, they are implicitly forced to choose which interpretive framework they communicate results within, which shapes how the community receives the work.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, experimental_quantum_physicists, observer,
    organized, biographical, constrained, global).

% Learn quantum mechanics primarily through textbooks that teach Copenhagen interpretation as standard, with many-worlds presented (if at all) as an exotic alternative. They are excluded from the interpretive debate even though they are learning the formalism that the debate concerns.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_mechanics_students, excluded,
    powerless, biographical, constrained, global).

% Philosophical community scrutinizing the internal consistency, empirical adequacy, and ontological commitments of competing interpretations. Evaluates claims from the structural side: does the many-worlds reading actually deliver what it promises (determinism, observer-elimination)? What are its hidden costs (infinite branches, branching when and how?)?
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, analytic_philosophy_of_physics, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a conceptually unified framework for quantum mechanics by eliminating the measurement problem: removes the need for an observer-dependent collapse postulate, grounds all dynamics in the universal wavefunction's deterministic evolution, and restores global lawfulness to the quantum realm.
% TRANSFER_FUNCTION: Moves conceptual authority and intellectual legitimacy from Copenhagen's observer-centric epistemology to a reading centered on the universal wavefunction's ontic status. Transfers the burden of explanation from 'why does measurement produce indeterminism?' to 'how do we interpret infinite branching?'
% ABSENT_VOICES: Working quantum engineers and technicians (quantum computing, quantum sensing) are absent from the interpretive debate; they care that the formalism makes predictions, not which interpretation is true. Philosophers outside the Anglo-American analytic tradition are underrepresented in the international discussion.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished as a live interpretive option (i.e., physicists abandoned it entirely), the measurement problem would not vanish—it would revert to the Copenhagen-vs-pilot-wave debate. The epistemic situation would rearrange: interpretive theorists would have fewer frameworks to work with, and some problems (the basis of the branching rule, the choice of preferred basis) would become invisible again because they would have no interpretive home. The reading's disappearance would not alter experimental outcomes, but it would alter how the community conceptualizes the meaning of those outcomes.
% FOUNDING_PROBLEM: The measurement problem: quantum mechanics predicts superpositions, but observers never see superpositions, only definite outcomes. This appears to require either (1) a collapse mechanism that adds indeterminism to the theory, or (2) something special about observers that the theory does not explain. Many-worlds dissolves the problem by asserting all outcomes ARE realized; what appears to be collapse is the observer becoming entangled with the measured system and branching.
% FOUNDING_PROBLEM_CORROBORATION: The measurement problem is attested as live and deep by the Vienna Circle era (Heisenberg, von Neumann, Wigner) and by contemporary philosophers of physics (Albert, Barrett, Detlef Dürr on pilot waves, Tim Maudlin on nonlocality). The many-worlds reading is advanced as a solution primarily by its own adherents (Everett 1957, DeWitt, Wallace, Vaidman) and by philosophers of physics who find it empirically adequate and conceptually cleaner than collapse (Deutsch, David Albert). Copenhagen defenders (e.g., Landau & Lifshitz, some contemporary Bohmians) dispute that the problem is real or that many-worlds solves it rather than multiplying it.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).
:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.41) because the constraint operates as an interpretive claim competing for intellectual authority, not as economic rent or coercive enforcement. The reading benefits its adherents by offering conceptual elegance (determinism, observer-elimination, no collapse mechanism) and research-program coherence, while imposing a cost on Copenhagen defenders who must continuously defend their framework against this alternative. The cost is diffuse and epistemic rather than economic, which keeps extractiveness in the 0.30–0.45 range rather than higher. Suppression (0.38) reflects the empirical indistinguishability of interpretations: no experiment can definitively rule out many-worlds because all interpretations make identical predictions. This indistinguishability is structural suppression—it prevents falsification and forces the choice of interpretation to be based on philosophical preference rather than empirical discovery. Theater (0.19) is low because the actual intellectual work—deriving consequences of the reading, working through the preferred-basis problem, handling decoherence mathematics—is substantive, not performative. The measurement trajectory shows slow accumulation because many-worlds has gained institutional traction (graduate programs, research clusters, textbook presence) over the interval without becoming dominant; the constraint's extractive pressure has intensified incrementally as the reading's presence in the discourse has grown, but it remains in stable competition with Copenhagen, not yet dominant.
 *
 * PERSPECTIVAL GAP:
 *   The Copenhagen defender's seat and the many-worlds adherent's seat should compute very differently. From the Copenhagen seat, the many-worlds reading appears as an interpretive competitor imposing the cost of continuous defense while offering no experimental advantage (suppression is high from this perspective because no empirical test can eliminate the threat). From the many-worlds seat, the reading appears as a genuine solution to a coordination problem (how to reconcile determinism with quantum mechanics) that Copenhagen leaves unresolved. Both seats are right about their own situation; the engine computes this divergence from the stakeholder power levels and exit options. The Copenhagen defender has mobile exit (can switch to pilot-wave or embrace Copenhagen antirealism) but faces institutional lock-in (their career publications assume Copenhagen framing); the many-worlds adherent faces research-program coherence pressure but less institutional entrenchment. The framework is organized around institutional power (organized seats) with global scope, which amplifies the per-seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The many-worlds school (beneficiary) has low directionality (~0.25) because the reading coheres their research, offers intellectual elegance, and requires no external suppression—they defend it voluntarily. The Copenhagen defenders (payer) have moderate-high directionality (~0.65) because they bear the cost of continuous defense against an empirically indistinguishable alternative, yet they have mobility (can adopt antirealism, embrace pilot-wave, or accept indeterminism) and organized power. The measurement-problem community sits near 0.50 (symmetric): they benefit from having the problem dissolved, but they bear the cost of committing to infinite branches. Experimental physicists are observers (analytical directionality ~0.50, but with constrained exit because they must still communicate their results in some interpretive frame). Students are excluded: their exit is constrained (must learn the reading from textbooks) but they collect no benefit or bearing of cost because the interpretive debate is not conducted at their level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurement problem: why does quantum mechanics require observers?) is genuinely live—attested by generations of physicists from Heisenberg onward. The many-worlds reading addresses it directly by eliminating the observer's special role. However, a mandatrophy signal would fire if the founding problem became dead (measurement no longer puzzling) while the reading persisted—and that is NOT the case here. The reading persists BECAUSE the founding problem is live; if quantum mechanics were reinterpreted as purely epistemic or if a collapse mechanism were experimentally confirmed, the reading's raison d'être would evaporate. The classification is stable: the constraint solves a live problem and remains coordinated to that problem's persistence. No mandatrophy resolution is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the many-worlds reading a natural consequence of taking Schrödinger dynamics seriously (natural law, inevitable given the formalism), or a constructed interpretive choice among logically equivalent formulations?',
    'Formal equivalence proofs (showing many-worlds, Copenhagen, and pilot-wave are empirically equivalent under all possible experiments) versus foundational arguments (showing one reading is uniquely privileged by internal coherence or parsimony).',
    'If natural law: the reading deserves higher accessibility_collapse and lower resistance (alternatives appear forced/unnatural). If constructed: the reading''s extractiveness may be higher (it benefits adherents by claiming naturalness it may not possess).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether many-worlds is inevitable or chosen.').

omega_variable(
    ontological_extravagance_cost,
    'How should we weigh the many-worlds reading''s benefit (determinism, observer-elimination, global lawfulness) against its cost (infinite parallel worlds, branching mechanics, world-counting ambiguity)?',
    'Comparative metaphysical analysis: is the cost of infinite worlds higher or lower than Copenhagen''s cost (unexplained observers, indeterminism) and pilot-wave''s cost (hidden variables, nonlocal guidance)?',
    'If the cost is judged as prohibitive, the reading''s extractiveness may be overstated; if manageable, the extractiveness estimate is sound. Either way, the reading''s persistence depends on accepting the tradeoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_extravagance_cost, preference, 'The normative weight of ontological parsimony versus determinism.').

omega_variable(
    preferred_basis_problem_solvability,
    'Can the preferred-basis problem (why do we observe definite outcomes in the position basis, not energy or momentum?) be solved within the many-worlds framework, or does it represent a permanent incoherence?',
    'Decoherence analysis: does decoherence automatically pick out the preferred basis, or is the basis choice contingent on the environment (and thus not explained by the reading)?',
    'If solvable: the reading''s coherence is vindicated and its extractiveness justified. If not: the reading carries a hidden philosophical cost (basis choice is posited, not derived), which may increase theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preferred_basis_problem_solvability, empirical, 'Whether decoherence resolves the basis ambiguity or leaves it open.').

omega_variable(
    empirical_indistinguishability_suppression,
    'Is the empirical indistinguishability of quantum interpretations a permanent feature of the measurement situation, or could future experiments or refined analysis distinguish among Copenhagen, many-worlds, and pilot-wave?',
    'Active research in quantum foundations: proposals for empirical tests (e.g., Colosi & Rovelli on observation of evaporation, or modified uncertainty-principle experiments) that might break the degeneracy.',
    'If indistinguishability is permanent, suppression remains structural and the reading''s persistence is decoupled from empirical progress. If distinguishability emerges, suppression drops and the constraint''s type may shift toward pure coordination (Rope) or pure extraction (Snare) depending on outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_indistinguishability_suppression, empirical, 'Whether quantum interpretations are empirically equivalent.').

omega_variable(
    reading_vs_kernel_boundary,
    'Does the many-worlds reading instantiate a distinct philosophical claim from the quantum_formalism kernel, or is it an inevitable consequence of interpreting the kernel literally?',
    'Formal decomposition: can the kernel (universal Schrödinger evolution) be instantiated in non-many-worlds form without logical incoherence? (Answer: yes, via Copenhagen or pilot-wave.) Does the many-worlds reading add substantive commitments beyond the kernel? (Answer: yes—it commits to all branches being equally real.)',
    'This omega documents the constraint''s role as a reading within a larger kernel dispute. The reading is distinct from the kernel and coexists with sibling readings; it is not a necessary consequence of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_boundary, conceptual, 'The distinction between the quantum_formalism kernel and the many_worlds_reading instantiation of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t10, quantum_formalism__many_worlds_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(quan_tr_t10, observed).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__many_worlds_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(quan_tr_t20, observed).
narrative_ontology:measurement(quan_tr_t35, quantum_formalism__many_worlds_reading, theater_ratio, 35, 0.17).
narrative_ontology:measurement_basis(quan_tr_t35, observed).
narrative_ontology:measurement(quan_tr_t50, quantum_formalism__many_worlds_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(quan_tr_t50, observed).
narrative_ontology:measurement(quan_tr_t70, quantum_formalism__many_worlds_reading, theater_ratio, 70, 0.19).
narrative_ontology:measurement_basis(quan_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t10, quantum_formalism__many_worlds_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(quan_be_t10, observed).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__many_worlds_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(quan_be_t20, observed).
narrative_ontology:measurement(quan_be_t35, quantum_formalism__many_worlds_reading, base_extractiveness, 35, 0.39).
narrative_ontology:measurement_basis(quan_be_t35, observed).
narrative_ontology:measurement(quan_be_t50, quantum_formalism__many_worlds_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement_basis(quan_be_t50, observed).
narrative_ontology:measurement(quan_be_t70, quantum_formalism__many_worlds_reading, base_extractiveness, 70, 0.41).
narrative_ontology:measurement_basis(quan_be_t70, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__many_worlds_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.02).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel decomposes into three structurally distinct readings: many_worlds_reading (this constraint), copenhagen_reading, and pilot_wave_reading. All three interpret the same universal wavefunction and its deterministic evolution, but diverge on the meaning of measurement, the role of observers, and the ontological status of superpositions. The constraint family is linked by network.affects_constraints: each reading influences the legitimacy conditions and research agendas of its siblings through empirical equivalence and philosophical competition. See the related constraint stories for Copenhagen and pilot-wave readings; each has a distinct epsilon, victim/beneficiary structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
