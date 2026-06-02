% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis as Epistemological Boundary (Proof-Theoretic Reading)
 *   domain: foundations_of_computer_science/philosophy_of_mathematics/proof_theory
 *
 * SUMMARY:
 *   The Church-Turing thesis, interpreted as an epistemological boundary of
 *   formally knowable computation, defines what constitutes a valid proof of
 *   computability within classical mathematics. This reading treats the
 *   thesis not as a claim about physical reality or mathematical metaphysics,
 *   but as a methodological framework that establishes proof-theoretic
 *   legitimacy standards. Functions we can prove computable are exactly those
 *   that can be shown Turing-computable through accepted mathematical
 *   argument forms. This epistemological reading creates a constraint because
 *   it simultaneously (1) enables unified formalization and research
 *   standards (rope function) and (2) excludes or marginalizes
 *   non-constructive approaches, hypercomputational models, and alternative
 *   mathematical traditions (extraction function). The constraint exhibits
 *   dual beneficiaries and victims: proof-theoretic mathematics benefits from
 *   the boundary's clarity, while non-constructive claims and
 *   hypercomputation research bear suppressive costs. This is distinct from
 *   the mathematical definition reading (which asks 'what is the essential
 *   nature of computation?') and the physical reading (which asks 'what
 *   physical processes compute?'). The epistemological reading answers: 'what
 *   forms of argument count as valid proof of computability?'
 *
 * KEY AGENTS:
 *   - Proof-Theoretic Mathematics Community: Primary beneficiary (institutional/constrained) — the thesis provides canonical formalization standards and research methodology
 *   - Non-Constructive Mathematical Claims: Primary victim (powerless/trapped) — structurally excluded from the boundary of knowable computation without alternative legitimacy frameworks
 *   - Hypercomputation Research Programs: Secondary victim (moderate/constrained) — face publication bias, funding gatekeeping, and career risk despite potential mathematical validity
 *   - Computability Research Discipline: Beneficiary (institutional/arbitrage) — the thesis enables unified research agendas, complexity hierarchies, and decidability analysis
 *   - Constructive and Intuitionistic Traditions: Ambiguous position (institutional/constrained) — can develop alternative computability boundaries but at cost to interoperability with mainstream classical mathematics
 *   - Analytical Observer: Methodological perspective (analytical/analytical) — sees the thesis as coordination mechanism defining proof-theoretic standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.38).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.52).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary (Proof-Theoretic Reading)").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "foundations_of_computer_science/philosophy_of_mathematics/proof_theory").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '516f89d4-482d-4591-9f7c-12a48cbbbdd4').
narrative_ontology:cs_kernel_codification('516f89d4-482d-4591-9f7c-12a48cbbbdd4', fixed_text).
narrative_ontology:cs_authority_grounding('516f89d4-482d-4591-9f7c-12a48cbbbdd4', expertise).
narrative_ontology:cs_interpretation_layer_present('516f89d4-482d-4591-9f7c-12a48cbbbdd4').
narrative_ontology:cs_reading_relation('516f89d4-482d-4591-9f7c-12a48cbbbdd4', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('516f89d4-482d-4591-9f7c-12a48cbbbdd4', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('516f89d4-482d-4591-9f7c-12a48cbbbdd4', foundational, proof_theoretic_computability_is_foundational).
narrative_ontology:cs_axiom_status(proof_theoretic_computability_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('516f89d4-482d-4591-9f7c-12a48cbbbdd4', proof_theoretic_computability_is_foundational, deontological).
narrative_ontology:cs_axiom('516f89d4-482d-4591-9f7c-12a48cbbbdd4', secondary, non_turing_frameworks_are_methodologically_marginal).
narrative_ontology:cs_axiom_status(non_turing_frameworks_are_methodologically_marginal, holdable).
narrative_ontology:cs_axiom_grounding('516f89d4-482d-4591-9f7c-12a48cbbbdd4', non_turing_frameworks_are_methodologically_marginal, instrumental).
narrative_ontology:cs_reference_frame('516f89d4-482d-4591-9f7c-12a48cbbbdd4', classical_proof_theoretic_computability).
narrative_ontology:cs_drift_state('516f89d4-482d-4591-9f7c-12a48cbbbdd4', contemporary_computational_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('516f89d4-482d-4591-9f7c-12a48cbbbdd4', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_mathematics).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_research_community).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematical_claims).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CONSTRUCTIVE CLAIMS (SNARE) — Non-constructive existence proofs, realizability-incompatible claims, and arguments about infinite mathematical objects are structurally excluded from the boundary of 'knowable computation' by this reading. The trapped agent cannot exit this classification without accepting the thesis's methodological premise. Maximum extraction: the constraint dictates what may be formalized as computable without offering alternative legitimacy frameworks for non-constructive reasoning about computation.
constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: HYPERCOMPUTATION RESEARCHERS (SNARE) — Researchers investigating oracles, super-Turing models, and generalized computability face structural suppression: publications require justifying why non-Turing frameworks are relevant despite the thesis's methodological dominance. High suppression (peer review bias, funding gatekeeping, career risk of heterodoxy) with minimal offsetting coordination function. Constrained exit (can publish in heterodox venues, but at significant career cost).
constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROOF-THEORETIC MATHEMATICS (TANGLED ROPE) — The community that uses the thesis as a working methodological boundary experiences both coordination (the boundary enables consistent formalization frameworks, published standards for computability proofs) and extraction (the thesis constrains which mathematical claims can be legitimated within the formal system). Constrained exit: mathematicians can adopt intuitionistic or constructive frameworks, but at cost to interoperability with mainstream mathematics.
constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPUTABILITY RESEARCH DISCIPLINE (ROPE) — The canonical computability community (recursion theory, computational complexity, formal language theory) sees the thesis primarily as a coordination mechanism: it enables unified mathematical frameworks, proof standards, and research agendas. The thesis eliminates ambiguity about what counts as a valid computability claim. High arbitrage: researchers can cite Church-Turing equivalence as settled and focus on other problems. Net beneficiary — the thesis structures the research landscape favorably.
constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / METHODOLOGICAL READING (ROPE) — From a generational/universal perspective, the Church-Turing thesis as epistemological boundary is a coordination mechanism, not extraction. It defines the scope of 'provable computability' within classical mathematics and establishes a research methodology. The coordination benefit is real: the thesis enables proof techniques, decidability analysis, and computational complexity theory. Minimal extraction because the boundary is explicitly methodological — no agent claims physical universality here.
constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: INSTITUTIONAL PITON READING (PITON) — At civilizational timescale, the thesis persists as an institutional orthodoxy despite reduced functional role. Modern computer science has moved beyond the thesis for practical purposes (quantum computation, analog systems, physical hypercomputation claims). The theorem remains true but is now primarily performative — cited for legitimacy in computability courses and foundational papers, maintained through textbook repetition and professional identity rather than active epistemological work. Theater ratio (0.68) reflects that contemporary discourse treats the thesis as settled law rather than active research boundary.
constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(church_turing_thesis__epistemological_boundary_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, TR),
    TR >= 0.70.

:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The epistemological boundary reading produces moderate extraction primarily through methodological exclusion rather than coercive suppression. Non-constructive mathematics is not forbidden — it is methodologically marginalized as 'not proven computable' under the thesis. The victim set is well-defined but not physically coerced. Suppression (0.52): Moderate-high. Non-constructive approaches face peer review bias, publication gatekeeping (hypercomputation research relegated to specialty venues), career risk for heterodox work, and funding concentration in mainstream computability. However, suppression is not maximal because alternative mathematical traditions persist in academia (constructive mathematics, intuitionistic logic, predicative arithmetic departments). Theater ratio (0.68): Moderate-high. Contemporary discourse increasingly treats the thesis as settled institutional orthodoxy rather than active methodological boundary. Modern computer science has moved beyond classical Turing-computability (quantum computation, probabilistic algorithms, analog systems) but the thesis persists in foundational teaching and legitimacy rhetoric. This is piton-level theater — the thesis is maintained through institutional inertia and professional identity (every computation theory course presents it) rather than active epistemological function.
 *
 * PERSPECTIVAL GAP:
 *   The epistemological boundary reading produces stark perspectival divergence. Proof-theoretic mathematicians see coordination and research enablement (Rope) — the thesis provides proof standards. Hypercomputation researchers see suppression and methodological gatekeeping (Snare) — non-Turing models are structurally marginalized. The analytical observer initially sees Rope (methodological boundary is legitimate coordination). But the false summit detector identifies hidden extraction: the thesis's institutional treatment as 'law' rather than 'methodology' resembles the snare pattern. Non-constructive mathematics is told 'you are not proven computable' (boundary enforcement) without being offered path to computability-proof under the thesis's own terms. This is extraction through methodological closure — you cannot participate in the legitimacy game because the game's rules exclude your framework a priori.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position. Proof-theoretic mathematicians are beneficiaries with institutional power and arbitrage exit (can publish in mainstream venues, cite the thesis as settled authority) — derive low d, negative chi. Non-constructive mathematics faces trapped/powerless exit (cannot be 'proven computable' without abandoning non-constructivity) — derives high d, high chi. Hypercomputation researchers have moderate power and constrained exit (can publish heterodoxly at career cost) — derive moderate-high d, moderate chi. The epistemological reading's mechanism is methodological exclusion, not physical coercion, so the directionality is about proof-theoretic legitimacy rather than resource control. This softens chi values relative to a snare, producing the tangled rope classification: genuine coordination function (the thesis enables rigorous computability proofs) combined with asymmetric extraction (the proof framework excludes certain approaches).
 *
 * MANDATROPHY ANALYSIS:
 *   The Church-Turing epistemological boundary resolves mandatrophy by showing that the thesis is NOT a pure coordination mechanism (which would be Rope) because the coordination happens to require exclusion of alternative mathematical traditions. The thesis could be Rope if it permitted multiple legitimate frameworks for computability proof. But the thesis as epistemological boundary defines what 'legitimate proof' means — it is not neutral coordination, it is contested legitimacy. This is classic Tangled Rope: the coordination function (unified proof standards, research methodology) is real and valuable, but it is embedded in a framework that asymmetrically excludes competing approaches. The mandatrophy resolves: the constraint is neither pure extraction (Snare) because coordination genuinely occurs, nor pure coordination (Rope) because the coordination mechanism requires suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thesis_scope_ambiguity,
    'Does the thesis claim universal (physical/mathematical) computability, or only scope-limited (proof-theoretic/formal) computability?',
    'Exegesis of Church and Turing''s original statements vs. contemporary interpretation; comparison with the physical Church-Turing thesis variants; analysis of which mathematical claims are treated as foreclosed vs. merely marginalized',
    'If scope-limited: the thesis is Rope (coordination within proof theory). If universal: the thesis becomes Tangled Rope or Snare depending on how non-computable claims are treated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thesis_scope_ambiguity, conceptual, 'Whether the thesis is scope-limited (proof-theoretic) or universal (physical/mathematical)').

omega_variable(
    hypercomputation_falsifiability,
    'Can hypercomputational models (oracles, infinite-time Turing machines, analog systems) be legitimately developed within mathematics without violating Church-Turing methodology?',
    'Formal construction of hypercomputational systems in constructive mathematics; analysis of whether such systems can be presented as mathematical objects with the same rigor as standard Turing machines; peer acceptance in mainstream venues',
    'If yes: the thesis constrains but does not foreclose; hypercomputation is mathematically legitimate alternative. If no: the thesis forecloses hypercomputation entirely — becomes more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypercomputation_falsifiability, empirical, 'Whether hypercomputational models can be legitimately developed within mathematics').

omega_variable(
    reading_identity_ambiguity,
    'Is this epistemological boundary reading genuinely distinct from the mathematical definition reading, or is it a reinterpretation of the same kernel with different emphasis?',
    'Analysis of whether the proof-theoretic vs. definitional approaches produce different victim sets, different beneficiary structures, or different classification types for the same test cases',
    'If genuinely distinct: this constraint story is valid and the sibling reading is a true alternative. If merely emphasized differently: the constraints are the same and should be merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether epistemological boundary and mathematical definition readings are distinct').

omega_variable(
    constructive_mathematics_compatibility,
    'Does the epistemological boundary reading permit constructive and intuitionistic mathematics to coexist with classical proof-theoretic boundaries, or does it require classical logic as the sole legitimacy framework?',
    'Examination of whether non-classical frameworks (intuitionistic logic, predicative arithmetic, homotopy type theory) can instantiate their own computability boundaries independently without being subordinated to the classical Church-Turing boundary',
    'If compatible: suppression is lower and the constraint is more Rope-like. If classical-logic-dependent: suppression is higher and the constraint is more Snare-like for non-classical traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_mathematics_compatibility, conceptual, 'Compatibility between epistemological boundary and constructive mathematics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cteb_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cteb_tr_t20, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(cteb_tr_t50, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(cteb_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cteb_be_t20, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(cteb_be_t50, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, halting_problem_decidability).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, computable_function_equivalence_thesis).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis kernel decomposes into three structurally distinct constraints with different ε values and beneficiary/victim structures. The epistemological boundary reading (this constraint, ε ≈ 0.38) focuses on proof-theoretic legitimacy standards. The mathematical definition reading (ε ≈ 0.05) treats the thesis as an essential property of computation (Mountain). The physical claim reading (ε ≈ 0.55) treats the thesis as a hypothesis about physical reality (Snare). Each reading produces different classifications and victim sets. All three affect downstream constraints about computability, decidability, and Turing equivalence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
