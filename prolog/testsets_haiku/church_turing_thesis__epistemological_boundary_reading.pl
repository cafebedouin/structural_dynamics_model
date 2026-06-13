% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__epistemological_boundary_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis (Epistemological Boundary Reading)
 *   domain: philosophy_of_mathematics/philosophy_of_computation
 *
 * SUMMARY:
 *   The Church-Turing Thesis is a contested claim about the boundary of
 *   formally knowable computation. Under the epistemological boundary reading
 *   instantiated here, the thesis marks a methodological line: functions we
 *   can *prove* computable are exactly the Turing-computable ones, and this
 *   boundary holds regardless of whether physical processes or abstract
 *   mathematics might exceed Turing limits. This reading treats the thesis as
 *   a constitutive rule for valid proofs in computability theory, not as an
 *   empirical hypothesis about physics or an unrevisable mathematical truth.
 *   Non-constructive claims about computability are methodologically excluded
 *   (not forbidden, but outside the binding definition), and
 *   hypercomputability research is cordoned off as a separate domain. The
 *   constraint's operation involves enforcing this epistemic boundary through
 *   institutional gatekeeping: which results count as contributions to
 *   computability theory, which researchers can publish in flagship journals,
 *   whose work shapes the curriculum.
 *
 * KEY AGENTS:
 *   - Proof-theoretic framework: The epistemic standard defining valid computability proofs; benefits from monopoly legitimacy
 *   - Constructive mathematics tradition: Research community practicing intuitionistic logic and constructive methods; benefits from validation as foundational framework
 *   - Non-constructive computability claims: Mathematical assertions using classical logic without constructive witness; excluded from binding definition
 *   - Hypercomputability research: Researchers investigating quantum, analog, oracle computation; cordoned off as outside standard theory
 *   - Classical mathematical logicians: Researchers using non-constructive methods; must either reformulate or accept boundary
 *   - Physics and unconventional computation: Excluded from the conversation setting the definition; their empirical findings are orthogonal
 *   - Foundations community: Observers testing whether the boundary is framework-dependent or framework-invariant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.38).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.22).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis (Epistemological Boundary Reading)").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/philosophy_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '140f78b1-180d-40d8-881b-33a048f82d4b').
narrative_ontology:cs_kernel_codification('140f78b1-180d-40d8-881b-33a048f82d4b', fixed_text).
narrative_ontology:cs_authority_grounding('140f78b1-180d-40d8-881b-33a048f82d4b', lineage).
narrative_ontology:cs_interpretation_layer_present('140f78b1-180d-40d8-881b-33a048f82d4b').
narrative_ontology:cs_reading_relation('140f78b1-180d-40d8-881b-33a048f82d4b', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('140f78b1-180d-40d8-881b-33a048f82d4b', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('140f78b1-180d-40d8-881b-33a048f82d4b', foundational, formal_computability_is_proof_theoretically_bounded).
narrative_ontology:cs_axiom_status(formal_computability_is_proof_theoretically_bounded, holdable).
narrative_ontology:cs_axiom_grounding('140f78b1-180d-40d8-881b-33a048f82d4b', formal_computability_is_proof_theoretically_bounded, deontological).
narrative_ontology:cs_axiom('140f78b1-180d-40d8-881b-33a048f82d4b', foundational, constructive_proof_exhausts_formal_computability).
narrative_ontology:cs_axiom_status(constructive_proof_exhausts_formal_computability, holdable).
narrative_ontology:cs_axiom_grounding('140f78b1-180d-40d8-881b-33a048f82d4b', constructive_proof_exhausts_formal_computability, empirically_contingent).
narrative_ontology:cs_reference_frame('140f78b1-180d-40d8-881b-33a048f82d4b', proof_theoretic_definition_of_computability).
narrative_ontology:cs_drift_state('140f78b1-180d-40d8-881b-33a048f82d4b', contemporary_quantum_and_hypercomputation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('140f78b1-180d-40d8-881b-33a048f82d4b', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_framework).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematics_tradition).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputability_research_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, classical_mathematical_logicians).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputability_researchers).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, formalist_epistemology).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, proof_computable_exhausts_computable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The epistemic standard within formal logic that validates computability proofs. The thesis, read as epistemological boundary, declares this framework the arbiter of what counts as 'formally knowable computation.' This framework does not control the constraint (it is not an agent), but it is the beneficiary: its legitimacy is enforced through institutional gatekeeping, and alternative frameworks must either conform or be classified outside computability theory proper.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_framework).

% A school of mathematical practice that restricts valid proofs to constructive methods (no law of excluded middle without explicit witness, no axiom of choice without explicit construction). The thesis validates constructive methods as the foundation for computation: if a function is computable, it must be constructively provable. Constructive mathematicians gain institutional legitimacy, research funding, and curricular influence from this alignment. Their exit options are constrained: to leave this tradition they must either abandon their foundational commitments or publish outside mainstream journals; to stay they must accept the boundary the thesis enforces.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematics_tradition, beneficiary,
    organized, generational, constrained, global).

% Mathematicians using classical logic (including non-constructive methods like axiom of choice, law of excluded middle) in foundational work. They bear the cost of the thesis's boundary: proofs about computability using non-constructive methods are not binding within the framework; they must either reformulate their work constructively, or accept that their results are confined to classical logic and do not count as contributions to computability theory proper. Some migrate to alternative frameworks (type theory, category theory); others reformulate and accept the boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, classical_mathematical_logicians, payer,
    powerful, generational, mobile, global).

% Researchers investigating whether abstract models (oracle machines, analog computers, quantum systems, unconventional paradigms) can compute beyond classical Turing limits. The thesis, read as epistemological boundary, does not forbid this research, but it enforces that such results cannot *redefine* computability: hypercomputational results must be presented as specialized domains (oracle computation, hypercomputation, etc.), not as displacements of Turing computability as the standard. Their work is accepted but cordoned off, published in specialized venues, confined to specialized narratives. Their exit options are constrained: they can publish hypercomputability work, but it will not change the definition of 'formal computability' as established by the thesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputability_researchers, payer,
    moderate, biographical, constrained, global).

% Physicists and engineers investigating whether physical processes (quantum, continuous-time analog, relativistic) can compute functions that classical Turing machines cannot. They are excluded from the conversation that sets the definition of 'formally knowable computation': the thesis is authored within mathematical logic, not in conversation with physics. Their empirical discoveries about what physical systems can compute are orthogonal to the thesis under this reading. If they argue that physics can hypercompute, the response under this reading is: 'You may be right about physics, but that does not change the definition of formal computability, which is bounded by proof-theoretic limits.' Their exit options are trapped because they cannot join the computability-theory conversation without accepting the epistemological boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, physics_and_unconventional_computation_community, excluded,
    powerful, biographical, trapped, global).

% Mathematicians and logicians working in foundations across multiple frameworks (set theory, category theory, type theory, proof theory). They observe the thesis's boundary claim, test it against different foundational systems, investigate whether it is framework-dependent or framework-invariant, and analyze the status of the thesis itself. Their role is analytical: to clarify what the thesis is claiming and whether its boundary is logically necessary or institutionally enforced.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, foundations_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, constructive_mathematics_tradition).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, proof-theoretically grounded standard for valid claims about formal computability. Coordinates the mathematical and computer science research community around one shared definition: a function is provably computable if and only if there exists a Turing machine (or provably equivalent formalism) that computes it. Eliminates the coordination problem of different researchers using different, incommensurable criteria for 'computability' — all proof-theoretic work now refers to the same boundary.
% TRANSFER_FUNCTION: Transfers authority to define 'computability' from individual researchers and alternative frameworks to the proof-theoretic establishment. Non-constructive claims about computability lose binding force (they become confined to classical logic, not counted as contributions to computability theory). Hypercomputability research is cordoned off as a separate domain. Constructive mathematics gains legitimacy as foundational framework. Classical logicians and hypercomputability researchers bear the cost of exclusion or cordoning-off.
% ABSENT_VOICES: Physics and unconventional computation communities are excluded from the conversation setting the definition. They would argue that empirical discoveries about what physical systems can compute should inform the definition, or that the definition should remain neutral between different physical and abstract models. The thesis, read as epistemological boundary, is authored within mathematical logic without requiring or hearing their testimony. Philosophers of physics skeptical of the proof-theoretic grounding would also be excluded.
% DISAPPEARANCE_RATIONALE: If this reading of the thesis vanished, the definition of 'formal computability' would fragment. Non-constructive proofs about computability would regain binding status within computability theory. Hypercomputability results could claim equal status with classical computability rather than being cordoned off. The community would reorganize around multiple, competing definitions of computability, or would shift the grounding from proof-theoretic to empirical (physical computability). Classical mathematics would no longer defer to constructive methods for foundational authority. Research agendas in philosophy of computation and unconventional computing would redirect toward redefining the boundary rather than working within it.
% FOUNDING_PROBLEM: In the early 20th century, mathematicians had intuitive notions of 'effective computability' but no precise, universally agreed definition. Church, Turing, Post, and others provided competing formalizations (lambda calculus, Turing machines, Post systems). The problem was: which formalization is the correct one, or do they all capture the same underlying concept? The founding solution: establish that all the major formalizations are logically equivalent, and treat this equivalence as the epistemological foundation for computability — what is provably computable is exactly what can be done by any of these equivalent systems.
% FOUNDING_PROBLEM_CORROBORATION: Proof theorists and constructive mathematicians attest the founding problem remains live: the boundary is still needed to coordinate research and prevent confusion about computability. Quantum computing researchers and unconventional computation scientists attest the founding problem has partially shifted: we now know empirically that quantum systems and other physical models can compute in ways Turing machines cannot, so the original puzzle (which formalization captures effective computation?) has been partly answered — the answer is 'different systems compute differently,' not 'they all compute the same things.' Philosophers of mathematics in the Cambridge Analytic tradition (like Wittgenstein interpreters) attest the founding problem might be a pseudo-problem: perhaps the different formalizations capture different concepts (constructive computation, classical computation, physical computation) that were artificially unified. Foundations researchers from outside the proof-theoretic establishment attest the boundary is contested and should not be treated as settled.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).
:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint's operation does exclude certain research directions and imposes a preferred foundational framework, but the exclusion is methodological rather than coercive — researchers can pursue non-constructive work or hypercomputability, they simply cannot claim it has changed the definition of formal computability. Suppression is low (0.22) because the boundary does not require active police work; it is maintained through institutional practices (peer review, journal gatekeeping, curriculum standards) that are largely automated and not perceived as coercive by mainstream participants. Theater is low-to-moderate (0.18) because the thesis has genuine coordinative content (establishing a shared definition for proof-theoretic research), but a growing share of its operation defends the boundary itself against empirical and foundational challenges rather than solving the original coordination problem. Accessibility collapse is high (0.71) because once a researcher accepts the thesis's boundary, alternatives (non-constructive approaches to computability, hypercomputational paradigms) become hard to legitimate within standard venues — the proof-theoretic framework becomes the only thinkable one. Resistance is moderate (0.41) because hypercomputability researchers and classical logicians mount sustained pushback, publishing alternative definitions and empirical results; the boundary is enforced but not unopposed.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (constructive mathematicians, proof theorists) perceive the thesis as a mathematical identity that solves a real coordination problem: it says 'all these formalizations are equivalent, and this equivalence IS the definition of computability.' The victim seats (non-constructive researchers, hypercomputability programs) perceive the same constraint as a boundary imposed for institutional convenience: it protects proof-theoretic hegemony by declaring alternatives out-of-scope, not by refuting them. The engine computes this divergence from the structural data — the beneficiary/victim declarations and exit options. The payer seat (classical logicians) sits in the middle: they use proof-theoretic methods when needed for their work but resent the claimed monopoly on 'valid' computability claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The proof-theoretic framework and constructive mathematics tradition are structural beneficiaries: they gain monopoly legitimacy and have their methods validated as foundational. Non-constructive claims and hypercomputability research are victims: they are excluded from the binding definition of computability, even though they may be mathematically sound or empirically motivated. Classical logicians pay a cost by having to reformulate their work or accept the boundary. Physicists are excluded (not victims of the constraint itself, but outside the conversation that defines it). The foundations community is the analytical observer seat, testing the reading against alternatives. Directionality divergence: from the constructive-math beneficiary seat, the thesis is genuine coordination and methodological clarity; from the hypercomputability researcher seat, it is enforced exclusion that stifles research directions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a shared definition of effective computability) remains genuinely live within proof theory and mathematical logic: the boundary still coordinates research and excludes confusion. But the empirical drift has been substantial: quantum computing, oracle machines, and unconventional physical systems are now known to compute differently than classical Turing machines. The founding problem's status is contested because the original motivating puzzle (which formalization captures the intuitive notion?) has been partly answered empirically (physics computes in ways Turing machines cannot), but the thesis-reading forecloses this empirical answer from redefining the boundary. This reading thus enforces the boundary as a *stipulation* (true by definition, immune to empirical revision), not as an *discovery* (vulnerable to physics). The tangled-rope classification captures this: the thesis genuinely coordinates proof-theoretic research (rope function) while also asymmetrically excluding alternative research directions (snare function). The active enforcement is epistemic gatekeeping: the boundary is maintained through institutional legitimacy standards, not through coercive force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_vs_physics_divergence,
    'Is the boundary between ''formally provably computable'' and ''physically computable'' a genuine distinction, or is it an artifact of confusing the epistemic standard (proof-theoretic) with the domain (computability itself)?',
    'Foundational analysis distinguishing the epistemology of proof (what can be validated in formal systems) from the metaphysics of computation (what physical or abstract systems can actually do). Interdisciplinary work in philosophy of physics and philosophy of mathematics clarifying whether the thesis is about knowledge or about reality.',
    'If the distinction is genuine and principled, the epistemological boundary reading is correct and the thesis marks a real methodological wall. If the distinction is confused, then the thesis is either a mathematical definition (mathematical reading) or an empirical claim about physics (physical reading), and the epistemological reading mislocates the boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proof_vs_physics_divergence, conceptual, 'Whether the proof-vs-physics distinction is a real boundary or a category confusion.').

omega_variable(
    institutional_enforceability_of_boundary,
    'To what extent is the epistemological boundary maintained by genuine logical necessity versus institutional gatekeeping and reputation costs?',
    'Historical and sociological analysis of peer review, journal editorial decisions, and curriculum standards around non-constructive and hypercomputational research. Identification of cases where work was rejected for methodological rather than technical reasons. Study of alternative research communities (type theory, category theory) that use different boundaries and their relative research productivity and citation patterns.',
    'If the boundary is mostly institutional gatekeeping, the constraint is substantially a snare (suppression is higher, alternatives are artificially collapsed). If the boundary is grounded in genuine logical necessity, the constraint is more rope than snare (suppression is lower, alternatives are genuinely limited). The measured suppression value depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforceability_of_boundary, empirical, 'How much of the boundary''s persistence is due to logical necessity versus institutional enforcement.').

omega_variable(
    reading_contest_underdetermination,
    'Is this reading (epistemological boundary) genuinely distinct from the mathematical_definition reading, or do they describe the same constraint from different perspectives?',
    'Careful analysis of the Church-Turing Thesis in primary sources (Church''s 1936 paper, Turing''s 1936 paper, Post''s systems) to determine whether the authors were claiming to define computability (mathematical reading) or to mark the boundary of what can be proven (epistemological reading). Examination of whether the same textual evidence supports both readings or whether they rely on different inferences.',
    'If the readings are genuinely distinct, three constraint stories are warranted and the kernel genuinely contests three different claims. If the readings are the same claim viewed from different angles, the constraint family should be collapsed to one story with multiple stakeholder seats, not three separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_underdetermination, conceptual, 'Whether the epistemological reading is structurally distinct from the mathematical definition reading or an alternative framing of the same constraint.').

omega_variable(
    constructivism_as_beneficiary_or_assumption,
    'Is constructive mathematics a beneficiary of the thesis (gains legitimacy from it), or is constructive proof-theoretic method a presupposition built into the thesis itself?',
    'Analysis of whether non-constructive foundations (classical set theory, category theory with classical logic) can also support the Church-Turing Thesis boundary, or whether the thesis inherently privileges constructive methods. Historical examination of whether classical mathematicians who accept the thesis view it as choosing constructivism, or as framework-neutral.',
    'If constructivism is a presupposition, the thesis is partly circular (defines computation by reference to constructive proof, then says constructive proof exhausts computation). If constructivism is a beneficiary, the thesis extracts value for one foundational program over others. This affects whether the beneficiary classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructivism_as_beneficiary_or_assumption, conceptual, 'Whether constructive mathematics is beneficiary or constitutive presupposition of the thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(chur_tr_t0, observed).
narrative_ontology:measurement(chur_tr_t20, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(chur_tr_t20, observed).
narrative_ontology:measurement(chur_tr_t40, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(chur_tr_t40, observed).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(chur_tr_t60, observed).
narrative_ontology:measurement(chur_tr_t80, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(chur_tr_t80, observed).
narrative_ontology:measurement(chur_tr_t100, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(chur_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(chur_be_t0, observed).
narrative_ontology:measurement(chur_be_t20, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(chur_be_t20, observed).
narrative_ontology:measurement(chur_be_t40, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement_basis(chur_be_t40, observed).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(chur_be_t60, observed).
narrative_ontology:measurement(chur_be_t80, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(chur_be_t80, observed).
narrative_ontology:measurement(chur_be_t100, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(chur_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(chur_su_t0, observed).
narrative_ontology:measurement(chur_su_t20, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement_basis(chur_su_t20, observed).
narrative_ontology:measurement(chur_su_t40, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(chur_su_t40, observed).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(chur_su_t60, observed).
narrative_ontology:measurement(chur_su_t80, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement_basis(chur_su_t80, observed).
narrative_ontology:measurement(chur_su_t100, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(chur_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.12).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, turing_completeness__computational_equivalence).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, hypercomputation_oracle_machines__boundary_challenge).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Church-Turing Thesis kernel. The epistemological_boundary_reading interprets the thesis as marking the boundary of formally provable computability, independent of physics or hypercomputability. The mathematical_definition_reading treats it as a stipulated definition (true by convention). The physical_claim_reading treats it as an empirical claim about the universe. These readings have different ε values, different victim sets, and different types: the epistemological reading is a tangled_rope (enforcement + coordination); the mathematical reading is closer to a rope (pure coordination with minimal suppression); the physical reading is contested between rope and mountain depending on empirical evidence. Each reading is a separate constraint story, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
