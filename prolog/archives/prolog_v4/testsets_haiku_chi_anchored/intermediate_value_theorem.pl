% ============================================================================
% CONSTRAINT STORY: intermediate_value_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intermediate_value_theorem, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: intermediate_value_theorem
 *   human_readable: Accessibility Barrier to the Intermediate Value Theorem
 *   domain: technological/mathematics_education
 *
 * SUMMARY:
 *   The Intermediate Value Theorem is a foundational result in real analysis,
 *   typically presented as requiring mastery of epsilon-delta limit
 *   definitions and continuity proofs. However, this formal prerequisite
 *   structure creates a significant accessibility barrier: students without
 *   rigorous training in formal analysis cannot access the theorem's
 *   statement or applications, despite the core intuition—a continuous
 *   function must pass through intermediate values—being accessible to far
 *   broader audiences. The constraint exhibits all six DR types across
 *   different perspectives, revealing whether the barrier is a mathematical
 *   necessity (mountain) or a contingent pedagogical gatekeeping mechanism
 *   (snare/tangled rope). The escalating theater ratio (0.38 → 0.64) reflects
 *   increasing performative content in how IVT is taught: formal
 *   epsilon-delta frameworks are maintained even as computational and visual
 *   alternatives become viable. The rising extractiveness (0.32 → 0.52)
 *   indicates that credential gatekeeping has intensified relative to genuine
 *   mathematical necessity, as educational stratification has widened access
 *   gaps between elite and non-elite institutions.
 *
 * KEY AGENTS:
 *   - Mathematics Learners Without Prerequisites: Primary victim (powerless/trapped) — cannot access IVT without formal foundation; no alternative pedagogical pathway provided
 *   - Applied Practitioners: Secondary victim (moderate/constrained) — need IVT for algorithms and numerical methods but excluded from theorem development and extension
 *   - Elite Mathematicians: Primary beneficiary (institutional/arbitrage) — maintain intellectual gatekeeping and credential signaling through formal prerequisite monopoly
 *   - Non-Elite Educational Systems: Secondary victim (moderate/constrained) — cannot afford instructors capable of teaching formal epsilon-delta rigor; students excluded from advanced mathematics
 *   - Open Mathematics Movement: Organized agents (organized/constrained) — Khan Academy, Desmos, computational platforms building alternative pedagogical pathways with sunset logic
 *   - Formal Curriculum Structure: Institutional actor (institutional/arbitrage) — maintains performative epsilon-delta requirement through accreditation and disciplinary tradition (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent pedagogical choices as mathematical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intermediate_value_theorem, 0.52).
domain_priors:suppression_score(intermediate_value_theorem, 0.68).
domain_priors:theater_ratio(intermediate_value_theorem, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intermediate_value_theorem, extractiveness, 0.52).
narrative_ontology:constraint_metric(intermediate_value_theorem, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(intermediate_value_theorem, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intermediate_value_theorem, tangled_rope).
narrative_ontology:human_readable(intermediate_value_theorem, "Accessibility Barrier to the Intermediate Value Theorem").
narrative_ontology:topic_domain(intermediate_value_theorem, "technological/mathematics_education").

domain_priors:requires_active_enforcement(intermediate_value_theorem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intermediate_value_theorem, advanced_mathematicians).
narrative_ontology:constraint_beneficiary(intermediate_value_theorem, elite_institutions).
narrative_ontology:constraint_victim(intermediate_value_theorem, mathematics_learners_without_prerequisites).
narrative_ontology:constraint_victim(intermediate_value_theorem, non_elite_educational_systems).
narrative_ontology:constraint_victim(intermediate_value_theorem, computational_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED LEARNER (SNARE) — Students without rigorous epsilon-delta foundation cannot access IVT's proof structure. Formal prerequisites form a suppressive barrier (suppression=0.68) with no alternative pedagogical pathway. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.64. Trapped exit; must either accept incomplete understanding or abandon the topic entirely.
constraint_indexing:constraint_classification(intermediate_value_theorem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: APPLIED PRACTITIONER (TANGLED ROPE) — Engineers, computational scientists, and data analysts need IVT for root-finding algorithms and numerical methods but cannot access formal proofs. They benefit from the theorem's existence (coordination: established mathematical toolkit) but are extracted from via credential gatekeeping (cannot participate in theorem refinement or extension). d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(intermediate_value_theorem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE MATHEMATICS COMMUNITY (ROPE) — Advanced mathematicians benefit from IVT as a coordination device: it unifies disparate results under a single canonical framework. The access restriction enables this community to maintain intellectual gatekeeping and credential signaling. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary through institutional closure.
constraint_indexing:constraint_classification(intermediate_value_theorem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN MATHEMATICS MOVEMENT (SCAFFOLD) — Organized agents (Khan Academy, Desmos, arXiv, open-source symbolic computation) are building alternative pedagogical pathways: interactive visualizations, computational demonstrations, and constructive proofs that bypass formal epsilon-delta foundations. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20. Sunset logic: as these alternatives mature (estimated 15-20 years), the formal gatekeeping loses force.
constraint_indexing:constraint_classification(intermediate_value_theorem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL CURRICULUM STRUCTURE (PITON) — The requirement to teach IVT only after epsilon-delta limits is largely performative: students memorize epsilon-delta definitions without deep comprehension, then learn IVT as a theorem to apply rather than understand. The ritual persists through institutional inertia and accreditation requirements. theater_ratio=0.64 satisfies piton gate. Many curricula have already decoupled IVT intuition from formal proof rigor, but formal presentation standards remain unchanged.
constraint_indexing:constraint_classification(intermediate_value_theorem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MATHEMATICAL NECESSITY (MOUNTAIN) — From a logical perspective, the Intermediate Value Theorem is a consequence of the definition of continuity in ℝ. The prerequisite structure (continuity requires limits, limits require epsilon-delta) appears as a mathematical necessity. However, base properties (ε=0.52, suppression=0.68) contradict pure mountain classification — the engine detects a false summit, revealing that prerequisite sequencing is a contingent pedagogical choice, not a mathematical law.
constraint_indexing:constraint_classification(intermediate_value_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intermediate_value_theorem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intermediate_value_theorem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intermediate_value_theorem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intermediate_value_theorem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intermediate_value_theorem, TR),
    TR >= 0.70.

:- end_tests(intermediate_value_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The formal prerequisite structure extracts from excluded learners through credential gatekeeping, but the extraction is not absolute — alternative computational and visual approaches exist, reducing the monopoly. The 0.52 value reflects that the barrier is real and consequential but not as severe as a pure snare (which would exceed 0.66). Suppression (0.68): High. Significant barriers include: (1) epsilon-delta prerequisites are rarely taught in pre-calculus; (2) most applied curricula lack time for formal rigor; (3) institutional gatekeeping via accreditation standards; (4) social signaling value of formal proof credentials. However, suppression is not total (≥0.80) because computational alternatives are proliferating. Theater ratio (0.64): Moderate-high. Many instructors teach epsilon-delta definitions performatively — students memorize δ and ε manipulation without understanding continuity conceptually. IVT is then presented as a consequence of this formal machinery, making the proof structure largely theatrical. Computational approaches have lower theater because numerical root-finding directly demonstrates the theorem's content without formal apparatus. Theater has increased over the interval as distance between formal presentation standards and actual pedagogical effectiveness has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same mathematical fact (IVT) produces six different classification readings depending on structural position. Elite mathematicians see a pure coordination device (Rope) — the formal framework unifies results and enables theorem development. Applied practitioners see extraction masked as coordination (Tangled Rope) — they need the theorem but are excluded from its ecosystem. Excluded learners see pure extraction (Snare) — the formal barrier extracts from them without reciprocal benefit. The open mathematics movement sees a temporary problem (Scaffold) — alternative pedagogies will eventually make the formal monopoly unnecessary. The curriculum structure sees its own degradation (Piton) — epsilon-delta requirements persist through inertia despite low pedagogical efficacy. The civilizational analytical observer risks seeing mathematical necessity (Mountain) — continuity fundamentally requires epsilon-delta — but structural data reveals this as a false summit: the necessity is pedagogical (how we teach) not mathematical (what the theorem is).
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematics learners without prerequisites: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — absolute barrier, no exit. Applied practitioners: Victim + constrained → d≈0.70, f(d)≈1.08. High extraction — can use theorem via algorithms but excluded from understanding or extending it. Elite mathematicians: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — maintain intellectual monopoly. Non-elite systems: Victim + trapped → d≈0.85, f(d)≈1.20. Severe extraction — cannot access rigorous training infrastructure. Open mathematics movement: Organized + constrained → d≈0.35, f(d)≈0.32. Low effective extraction — has agency and viable alternatives. Formal curriculum structure: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (≥0.70), not from χ. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; false summit detected.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY AT ε=0.52: This constraint exhibits genuine tension between classification as Tangled Rope (requires coordination + extraction + enforcement) versus classification as contingent Snare (if the formal prerequisite is entirely extractive gatekeeping with no real coordination function). The resolution depends on OMEGA_1 (prerequisite_necessity_boundary): if formal epsilon-delta foundations are mathematically necessary for deep understanding of IVT (high confidence), the Tangled Rope classification stands — the barrier provides real coordination (unified mathematical framework) alongside extraction (credential gatekeeping). If formal foundations are pedagogically optional (computational alternatives equally rigorous), the constraint devolves to pure Snare — the coordination function is theater, and the barrier is pure extraction. The analytical observer's mountain perspective is a false summit: it naturalizes the choice to teach via formal proof as a mathematical necessity when it is actually a contingent disciplinary tradition. Computational alternatives (numerical root-finding, graphical demonstrations) prove that IVT's mathematical content can be accessed without formal prerequisites. The mandatrophy cannot be resolved here without field-specific pedagogical research — we lack confidence data on whether epsilon-delta rigor is essential for student understanding or merely institutional performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prerequisite_necessity_boundary,
    'What is the minimum mathematical foundation required for students to develop meaningful understanding of IVT?',
    'Comparative pedagogical studies: track conceptual understanding of IVT using visual/computational approaches vs formal proof-first approaches; measure retention and transfer to new problem domains',
    'If formal prerequisites unnecessary: barrier is purely extractive gatekeeping (Snare dominates). If prerequisites essential for deep understanding: barrier is partly coordination (more Tangled Rope or Rope perspectives valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prerequisite_necessity_boundary, empirical, 'Whether formal epsilon-delta prerequisites are mathematically necessary for IVT comprehension').

omega_variable(
    computational_substitute_equivalence,
    'Do computational demonstrations (numerical root-finding, graphical proof) provide mathematically equivalent understanding to formal proof?',
    'Cognitive science studies comparing proof-understanding outcomes between formal and computational approaches; analysis of which proof techniques generalize to new theorems',
    'If equivalent: open mathematics pathways are genuine alternatives, scaffold classification holds. If inequivalent: formal monopoly is justified, barrier is less extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_substitute_equivalence, empirical, 'Whether computational approaches provide mathematical equivalence to formal proofs').

omega_variable(
    credentialing_function_separation,
    'What fraction of IVT gatekeeping serves credential signaling versus actual mathematical necessity?',
    'Labor market analysis: compare employment outcomes and research productivity for mathematicians trained via formal vs alternative approaches; track whether credential gatekeeping correlates with mathematical competence or primarily with institutional closure',
    'If credentialing function > 50%: barrier is primarily extractive (Snare amplified). If < 30%: barrier is primarily functional coordination (Rope or Tangled Rope justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_function_separation, conceptual, 'How much of IVT gatekeeping serves credentialing versus mathematical function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intermediate_value_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ivt_tr_t0, intermediate_value_theorem, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ivt_tr_t5, intermediate_value_theorem, theater_ratio, 5, 0.51).
narrative_ontology:measurement(ivt_tr_t10, intermediate_value_theorem, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(ivt_be_t0, intermediate_value_theorem, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ivt_be_t5, intermediate_value_theorem, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ivt_be_t10, intermediate_value_theorem, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intermediate_value_theorem, information_standard).
narrative_ontology:affects_constraint(intermediate_value_theorem, mathematical_rigor_gatekeeping).
narrative_ontology:affects_constraint(intermediate_value_theorem, real_analysis_curriculum_design).

% DUAL FORMULATION NOTE:
% This constraint story decomposes the accessibility barrier into a pedagogical/institutional constraint (ivt_accessibility_barrier, ε=0.52) separate from the mathematical fact (IVT as a theorem, ε→0.05 or Mountain). The upstream constraint (mathematical rigor gatekeeping) affects downstream IVT accessibility via prerequisite enforcement. The decomposition reflects that 'IVT' in natural language conflates the mathematical theorem (high-confidence, near-mountain) with the institutional gatekeeping mechanism (contested, tangled rope/snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intermediate_value_theorem, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
