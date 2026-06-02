% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__pilot_wave_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot Wave Ontology as Deterministic Hidden Variable Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The pilot-wave reading of quantum formalism (de Broglie-Bohm mechanics)
 *   proposes that particles have definite positions at all times, guided by a
 *   real wavefunction acting as a physical field. This reading directly
 *   contests the Copenhagen orthodoxy by restoring classical particle
 *   ontology, eliminating the observer, and introducing deterministic
 *   dynamics at the cost of nonlocal guidance. The constraint operates at the
 *   intersection of mathematical necessity (Bell's theorem forces nonlocality
 *   on any deterministic hidden-variable theory) and institutional power
 *   (Copenhagen interpretation dominates pedagogy and funding despite
 *   philosophical unresolved-ness of the measurement problem). The pilot-wave
 *   reading instantiates one coherent way to interpret the quantum formalism
 *   kernel — treating the wavefunction as a real field that guides particle
 *   trajectories rather than as epistemic/probabilistic or as a collapse
 *   mechanism. The measurement and theater ratios track how institutional
 *   suppression of alternatives has increased over 65 years (1960–2025):
 *   theater rises as Copenhagen is maintained through pedagogical
 *   consolidation despite being philosophically incomplete; suppression rises
 *   as grant structures, hiring, and publication norms increasingly enforce
 *   the standard interpretation.
 *
 * KEY AGENTS:
 *   - Classical Mechanics Analogy Program: Primary beneficiary (institutional/arbitrage) — pilot-wave restores particle ontology and determinism, enabling classical-like intuition and ontological clarity. Benefits through intellectual coherence.
 *   - Deterministic Research Program: Primary beneficiary (institutional/arbitrage) — gains foundational coherence by eliminating wavefunction collapse and observer. Organized research network around Bohmian mechanics.
 *   - The Measurement Problem: Primary victim (powerless/trapped) — the unresolved interpretive ambiguity that all interpretations face; pilot-wave claims to solve it but remains marginal.
 *   - Observer Independence Debate: Primary victim (powerless/trapped) — Copenhagen keeps observer in the formalism; pilot-wave claims to eliminate it. The debate itself is institutionally suppressed.
 *   - Working Physicist: Moderate agent (moderate/constrained) — uses formalism instrumentally; no immediate stake in interpretation. Constrained by pedagogy and career risk.
 *   - Empirical Testing Coalition: Organized agents (organized/constrained) — Bell tests, contextuality proofs, quantum computing move debate from interpretation to empirical facts. Sees sunset clause.
 *   - Copenhagen Pedagogical Canon: Institutional beneficiary maintaining constraint (institutional/arbitrage) — sustains pilot-wave suppression through textbook hierarchy, funding concentration, and philosophical deference to Niels Bohr tradition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.38).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.52).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot Wave Ontology as Deterministic Hidden Variable Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '9f6c9ce6-6f61-4cd7-939e-d07afc83ddce').
narrative_ontology:cs_kernel_codification('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', fixed_text).
narrative_ontology:cs_authority_grounding('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', extraction).
narrative_ontology:cs_interpretation_layer_present('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce').
narrative_ontology:cs_reading_relation('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', foundational, particles_have_definite_positions).
narrative_ontology:cs_axiom_status(particles_have_definite_positions, holdable).
narrative_ontology:cs_axiom_grounding('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', particles_have_definite_positions, deontological).
narrative_ontology:cs_axiom('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', foundational, wavefunction_as_physical_field).
narrative_ontology:cs_axiom_status(wavefunction_as_physical_field, holdable).
narrative_ontology:cs_axiom_grounding('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', wavefunction_as_physical_field, deontological).
narrative_ontology:cs_axiom('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', foundational, determinism_restored).
narrative_ontology:cs_axiom_status(determinism_restored, holdable).
narrative_ontology:cs_axiom_grounding('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', determinism_restored, deontological).
narrative_ontology:cs_axiom('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', secondary, observer_eliminated).
narrative_ontology:cs_axiom_status(observer_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', observer_eliminated, deontological).
narrative_ontology:cs_reference_frame('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', deterministic_particle_ontology_with_field_guidance).
narrative_ontology:cs_drift_state('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', contemporary_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9f6c9ce6-6f61-4cd7-939e-d07afc83ddce', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, classical_mechanics_analogy).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, deterministic_research_program).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, observer_independence_debate).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, measurement_problem_resolution_consensus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MEASUREMENT PROBLEM (SNARE) — The interpretive consensus that collapses wavefunctions has no advocate; the problem statement itself bears costs of remaining unresolved. Trapped in the formalism. Cannot exit without solving interpretation, which the pilot-wave reading claims to do but the consensus rejects. Maximum experienced opacity — the problem is unsolved by the dominant paradigm.
constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKING PHYSICIST (TANGLED ROPE) — Uses quantum formalism instrumentally (predictions work perfectly) but experiences the interpretive requirement as overhead. Pilot wave offers genuine coordination benefit (no observer special role, deterministic trajectory) but at cost of nonlocal guidance field and computational complexity. Constrained by pedagogy, funding structures favoring standard interpretation, and career risk of heterodox foundational commitment. Moderate agent with mixed benefits/costs.
constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DETERMINISTIC RESEARCH PROGRAM (ROPE) — Benefits from pilot-wave ontology as a coordination solution: it solves the measurement problem (restores classical particle ontology + determinism), eliminates observer, and provides alternative to Copenhagen/Many-Worlds. This faction experiences the constraint as enabling coordination of research around foundational issues. Net beneficiary through intellectual coherence.
constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPIRICAL TESTING COALITION (SCAFFOLD) — Organized effort (Bell test closures, contextuality proofs, quantum computing applications) to move beyond interpretation debates by establishing empirical facts about entanglement and nonlocality. Sees pilot-wave reading as a temporary framewalk with sunset: once Bell inequalities and contextuality are fully resolved empirically, the interpretive competition becomes academic. Constrained by ongoing empirical work, but sees clear exit path through experimental facts.
constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COPENHAGEN PEDAGOGICAL CANON (PITON) — The institutional standard interpretation persists through inertia and pedagogical consolidation despite decades of pressure to interpret foundations seriously. Wavefunctions as predictive tools, measurement causing collapse, observer role as inevitable — these remain textbook standard not because the interpretation solves foundational problems (it explicitly defers them) but because teaching it first and alternatives later is institutional practice. The constraint on pilot-wave reading's adoption is substantially theater: Copenhagen dominates pedagogy without actually resolving the interpretive questions. Theater ratio high because the standard interpretation is maintained through institutional positioning rather than epistemic superiority.
constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FORMALISM CONSTRAINT (MOUNTAIN) — From civilizational/universal timescale, the mathematical constraint that pilot-wave ontology must impose nonlocal guidance (to recover Born rule and account for entanglement correlations) is a fundamental feature of any deterministic hidden-variable theory satisfying empirical predictions. Bell's theorem (no local hidden variables reproduce quantum statistics) makes nonlocal guidance not a contingent choice but a structural necessity. This perspective views the pilot-wave constraint as mapping an immutable mathematical boundary: any theory restoring determinism and particle ontology MUST pay the nonlocality cost. However, the structural data reveals this as potentially a false summit — the institutional arrangements (pedagogy, career incentives, publishing barriers) may be the real constraint, with the mathematical boundary used to justify them.
constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_formalism__pilot_wave_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, TR),
    TR >= 0.70.

:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The pilot-wave reading imposes substantial costs on alternative research programs (marginal funding, slower publication, career risk) but does not impose total extraction. The Bohmian mechanics community sustains active research, publishes regularly, and operates within mainstream physics institutions. The extraction is real (asymmetric disadvantage) but not maximal. The 65-year trend (0.28 → 0.38) shows increasing extraction as Copenhagen consolidation deepens. Suppression (0.52): Moderate-high. The pedagogical dominance of Copenhagen (required undergraduate curriculum, standard textbook ordering, Bohr-affiliated prestige) creates substantial barriers to alternative research. However, suppression is not total — universities maintain Bohmian mechanics positions, arXiv hosts papers, conferences accept presentations. The trend (0.35 → 0.52) reflects increasing pedagogical enforcement without outright institutional exclusion. Theater ratio (0.68): High. The pilot-wave reading is substantially constrained by theater: the claim that 'Copenhagen is standard because it works' is performative — it works instrumentally, but so do other interpretations. The institutional standard is maintained through tradition and pedagogical convenience, not through superior problem-solving. Theater increases (0.52 → 0.68) as Copenhagen's philosophical incompleteness becomes better-known but institutional dominance persists anyway.
 *
 * PERSPECTIVAL GAP:
 *   This kernel reading instantiates the full indexical gap across observer positions. The deterministic research program (institutional/arbitrage) sees the pilot-wave reading as enabling solution of the measurement problem — genuine coordination gain. The Copenhagen pedagogy (institutional/arbitrage) maintains its position through inertia not epistemic superiority — the reading is constrained by theater (institutional performance of authority). The working physicist (moderate/constrained) experiences the constraint as real cost-benefit tension: deterministic clarity minus career risk. The measurement problem itself (powerless/trapped) remains unsolved because interpretive orthodoxy defers it — constrained agents cannot solve it, beneficiaries do not need to. The empirical coalition (organized/constrained) sees the institutional constraint as temporary: empirical facts (Bell closures, contextuality, entanglement) will settle which reading maps to real structure. The analytical observer risks naturalizing the mathematical boundary (Bell's no-local-hidden-variables theorem) as immutable constraint but recognizes through structural analysis that institutional factors have increasingly captured the 'naturalness' story.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by its structural position relative to the pilot-wave constraint. The deterministic research program experiences low d (beneficiary, arbitrage options) — they gain intellectual coherence and foundational clarity from the reading. The Copenhagen canon experiences medium-low d (beneficiary, institutional position) — it maintains dominance through inertia and pedagogy. The working physicist experiences medium d (moderate power, constrained exit) — they benefit from clarity but bear costs of heterodoxy. The measurement problem and observer debate experience high d (powerless, trapped) — they bear the cost of remaining epistemically unresolved in alternative readings. The empirical coalition experiences medium-low d (organized, constrained exit toward empirical facts) — they benefit from moving the dispute beyond interpretation toward experimentation. The d values reflect not moral judgment but structural position within the constraint mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The pilot-wave constraint exhibits entanglement of coordination (real problem-solving around foundational consistency) and extraction (institutional suppression of alternatives). The mandatrophy is resolved by recognizing that both elements are real: (1) Pilot-wave reading DOES offer genuine coordination benefit — it solves the measurement problem by commitment to particle ontology and determinism (rope coordination function), (2) Pilot-wave adoption is ALSO constrained by institutional extraction — Copenhagen pedagogy, funding hierarchies, and prestige of Bohr tradition extract costs from heterodox research (snare mechanism for suppressed readings). The tangled-rope classification reflects that these are not competing — the same structural mechanism (pedagogy, funding) both coordinates research around foundational questions AND extracts from non-standard readings. The constraint is not 'is this coordination or extraction?' but 'how do we measure the balance?' The increasing theater ratio (0.52 → 0.68) indicates that institutional enforcement is increasingly performative — Copenhagen maintains orthodoxy not through demonstrated superiority but through pedagogical consolidation. The empirical coalition's scaffold perspective (sunset clause through Bell test resolutions) suggests that empirical closure of loopholes may eventually shift the constraint from institutional to empirical grounds — once Bell inequalities are definitively closed, institutional preferences become less defensible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nonlocality_acceptable_cost,
    'Is nonlocal guidance field a genuine cost to pilot-wave ontology, or does it solve equally severe costs in alternative interpretations (observer role, wavefunction collapse, branching)?',
    'Comparative ontological cost accounting: catalog unexplained features in each interpretation (nonlocality in PW, observer in Copenhagen, branching in Many-Worlds) and assess which is most problematic for coherent physical picture.',
    'If nonlocality is acceptable trade: pilot-wave gains epistemic standing as equally good or better interpretation. If nonlocality is disqualifying: Copenhagen/Many-Worlds competitive advantage restored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonlocality_acceptable_cost, conceptual, 'Whether nonlocal guidance is acceptable cost compared to rival interpretations').

omega_variable(
    determinism_versus_instrumentalism,
    'Does the working physicist actually require determinism, or does instrumental predictive success suffice? Is the demand for deterministic ontology a genuine constraint or an aesthetic preference?',
    'Survey of working physicists on whether they care about underlying determinism vs. satisfied by predictive accuracy. Historical analysis of whether interpretation choice correlates with research productivity in quantum mechanics vs. quantum field theory.',
    'If determinism required: pilot-wave constraint is real for foundational physicists. If instrumentalism sufficient: constraint is imposed by minority preference; extraction mechanism becomes apparent (those wanting determinism extract cost from those satisfied with Copenhagen pragmatism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_versus_instrumentalism, preference, 'Whether working physicists demand determinism or accept instrumentalism').

omega_variable(
    bell_closure_sufficiency,
    'Once Bell inequalities are closed (loophole-free experiments established), does the empirical resolution of nonlocal correlations settle interpretation debates, or do interpretations remain equally viable post-Bell?',
    'Analysis of recent Bell closure experiments (Aspect 2022, Clauser, Zeilinger Nobel work) and whether they produce consensus on interpretation or merely eliminate specific local-realism variants while leaving Copenhagen/PW/MW all viable.',
    'If Bell closure settles interpretation: scaffold sunset is real. If Bell closure leaves all interpretations viable: interpretation debate is permanent structural feature, and pilot-wave constraint persists as institutional rather than empirical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bell_closure_sufficiency, empirical, 'Whether Bell test closures settle interpretation disputes').

omega_variable(
    kernel_vs_institutional_reading_contest,
    'Is the pilot-wave reading a live reading of a genuinely contested kernel (quantum formalism as multiple coherent interpretations), or is it a minority preference constrained by institutional power that naturalizes Copenhagen as ''the'' interpretation?',
    'Track whether pilot-wave research is systematically disadvantaged in funding, publication, career advancement compared to Copenhagen interpretations. Assess whether textbooks present pilot-wave as alternative reading or curiosity.',
    'If institutional power: false summit confirmed; the ''formalism'' kernel is not equally open to all readings; Copenhagen orthodoxy extracts costs from alternatives. If genuine epistemological contest: constraint is pure coordination problem (Rope) — all readings valid, need to choose one for pedagogy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_institutional_reading_contest, empirical, 'Whether kernel is genuinely contested or Copenhagen hegemon naturalizes through institutional power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pwrd_theater_1960, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pwrd_theater_1995, quantum_formalism__pilot_wave_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement(pwrd_theater_2025, quantum_formalism__pilot_wave_reading, theater_ratio, 65, 0.68).

% Extraction over time
narrative_ontology:measurement(pwrd_extract_1960, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pwrd_extract_1995, quantum_formalism__pilot_wave_reading, base_extractiveness, 35, 0.33).
narrative_ontology:measurement(pwrd_extract_2025, quantum_formalism__pilot_wave_reading, base_extractiveness, 65, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pwrd_suppress_1960, quantum_formalism__pilot_wave_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(pwrd_suppress_1995, quantum_formalism__pilot_wave_reading, suppression_requirement, 35, 0.48).
narrative_ontology:measurement(pwrd_suppress_2025, quantum_formalism__pilot_wave_reading, suppression_requirement, 65, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.12).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, measurement_problem_unresolved).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, bell_theorem_nonlocality_constraint).

% DUAL FORMULATION NOTE:
% The quantum formalism kernel has three sibling readings (Copenhagen, Many-Worlds, Pilot-Wave). Each reading is a structurally distinct constraint with different ε values, beneficiary/victim structures, and institutional positions. The pilot-wave reading's extractiveness (0.38) reflects institutional suppression; Copenhagen's would reflect pedagogical dominance; Many-Worlds' would reflect theoretical proliferation costs. All three should be authored as separate constraint stories linked via network.affects_constraints. The kernel-level analysis (which reading is 'correct' or 'best') is NOT resolved by individual constraint analysis — it requires cross-reading comparison and empirical adjudication (Bell tests, contextuality proofs, quantum computing applications).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
