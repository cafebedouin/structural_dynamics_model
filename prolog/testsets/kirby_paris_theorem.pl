% ============================================================================
% CONSTRAINT STORY: kirby_paris_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kirby_paris_theorem, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kirby_paris_theorem
 *   human_readable: The Kirby-Paris Theorem (Independence of Goodstein's Theorem)
 *   domain: mathematical_logic/foundational_mathematics
 *
 * SUMMARY:
 *   The Kirby-Paris theorem (1982) establishes that Goodstein's theorem—a
 *   statement about the termination of specific sequences of natural numbers
 *   under a transformation called Goodstein reduction—is true in standard
 *   arithmetic but unprovable within Peano Arithmetic (PA). The theorem
 *   itself is a demonstration of Gödel incompleteness: it shows that natural
 *   mathematical truths exist that PA's first-order axioms cannot derive. The
 *   constraint is the boundary between what PA can express/prove and what
 *   requires stronger axiomatic systems (Zermelo-Fraenkel set theory with the
 *   axiom of infinity). This boundary is not negotiable, suppressible, or
 *   escapable through cleverness, resources, or institutional access—it is a
 *   mathematical necessity. The constraint operates at the level of formal
 *   logical structure, not at the level of institutional practice or
 *   technological capacity.
 *
 * KEY AGENTS:
 *   - Peano Arithmetic (PA): The formal system that cannot prove Goodstein's theorem—not an agent but a fixed logical structure
 *   - Goodstein's Theorem: The mathematical statement whose independence from PA defines the constraint
 *   - Ordinal Analysis Framework: The mathematical tool (specifically, analysis of ordinals beyond ε₀) required to prove Goodstein's theorem outside PA
 *   - Zermelo-Fraenkel Set Theory (ZFC): The stronger axiomatic framework within which Goodstein's theorem becomes provable
 *   - Mathematical Community: Institutional agents who must choose which axiomatization to work within
 *   - Computational Systems: Technological agents that cannot recover the proof by increased processing power alone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kirby_paris_theorem, 0.12).
domain_priors:suppression_score(kirby_paris_theorem, 0.03).
domain_priors:theater_ratio(kirby_paris_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(kirby_paris_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(kirby_paris_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kirby_paris_theorem, mountain).
narrative_ontology:human_readable(kirby_paris_theorem, "The Kirby-Paris Theorem (Independence of Goodstein's Theorem)").
narrative_ontology:topic_domain(kirby_paris_theorem, "mathematical_logic/foundational_mathematics").

domain_priors:emerges_naturally(kirby_paris_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of formal mathematical logic, Goodstein's theorem exhibits an immutable property: its truth value is independent of Peano Arithmetic's axiomatization. This is not a contingent institutional choice or a coordination problem. The theorem's unprovability in PA is a necessary mathematical fact, derivable only within stronger systems (Zermelo-Fraenkel set theory). No escape route exists; no alternative axiomatization of PA can recover the proof. This constraint is a pure mathematical necessity.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even powerful computational systems and formal proof assistants (Coq, Lean, Isabelle) cannot generate a proof of Goodstein's theorem within PA-level reasoning. The constraint is not suppressible by resources or cleverness. Computational exploration reaches the same immovable boundary: the theorem requires ordinal analysis and transfinite induction, concepts outside PA's formal reach. The constraint persists regardless of institutional access to computation.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Even institutional agents with access to alternative axiomatic frameworks cannot 'work around' the independence result by staying within PA. The theorem's unprovability in PA is a fixed fact about PA's expressive power. Mathematicians with access to ZFC can prove Goodstein's theorem, but doing so involves leaving PA's axiomatization entirely—not a workaround, but an acknowledgment that the constraint is unavoidable at PA's level. The extraction (incompleteness) cannot be negotiated or arbitraged away.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% From the perspective of Gödel's incompleteness theorems, Goodstein's theorem exemplifies a necessary structural property of formal systems: any consistent arithmetic strong enough to express basic number theory must be incomplete. The Kirby-Paris result is not a defect in PA or a design choice; it is a consequence of mathematical reality itself. No finitary axiomatic system can simultaneously be complete, consistent, and sufficiently expressive. This is a law of formal mathematics.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kirby_paris_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kirby_paris_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kirby_paris_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kirby_paris_theorem, ExtMetricName, E),
    domain_priors:suppression_score(kirby_paris_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kirby_paris_theorem),
    narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kirby_paris_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. This constraint does not extract value from anyone—it is a boundary condition of formal mathematics. No agent or institutional arrangement bears a cost imposed by the constraint; rather, the constraint reveals a structural limit in PA's expressive power. The extractiveness value reflects the minimal 'cost' of working within PA: one simply cannot prove certain true statements within its axioms. This is not extraction in the economic sense (transfer of value from victim to beneficiary), but rather a gap in expressibility. Suppression (0.03): Minimal. The constraint cannot be suppressed or circumvented. There is no coercive mechanism because the constraint is not an institutional arrangement but a mathematical law. One cannot 'choose' to suppress PA's incompleteness any more than one can suppress the incompleteness of a finite formal system. Theater ratio (0.15): Minimal. The proof and statement of the constraint are substantive and directly address the mathematical question. There is no performative activity or symbolic substitution; the theorem makes a precise claim about formal provability and delivers a rigorous proof. The small theater value accounts for the meta-level conceptual work (axiomatization, ordinal analysis definition) that is somewhat abstract relative to direct computation.
 *
 * PERSPECTIVAL GAP:
 *   Despite four distinct perspectives, all converge on the same classification: Mountain. This is the signature of a uniform-type constraint—a natural law. The logical necessity of the incompleteness transcends the observer's institutional position, computational access, or temporal horizon. A powerless agent faces the same boundary as an institutional one; a computational system faces the same limit regardless of resource allocation; a biographical or civilizational observer sees the same immovable constraint. The lack of perspectival gap is itself the diagnostic signature: when all agents, regardless of power and exit options, encounter the same fixed boundary, the constraint is a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality concepts do not meaningfully apply to this constraint. The Kirby-Paris theorem does not create asymmetric extraction or coordination asymmetries between agents. All perspectives have identical exit_options (analytical) and spatial_scope (universal) because the constraint operates at the level of formal logical structure, not institutional or technological structure. The d-parameter for all agents converges to a neutral value reflecting the absence of asymmetric extraction. The constraint is not directional—it is isotropic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_analysis_necessity,
    'Is the requirement for ordinal analysis beyond PA''s ordinals (specifically, the ordinal ε₀) a fundamental necessity or a contingent feature of current proof techniques?',
    'Exhaustive formal exploration of alternative proof strategies; theoretical analysis of whether weaker ordinal resources could suffice; comparative study of proof paths in neighboring independence results',
    'If fundamental: the mountain classification is robust—PA''s incompleteness is intrinsic. If contingent: future proof techniques might recover proofs within stronger subsystems closer to PA (e.g., PA plus transfinite induction restricted to smaller ordinals), weakening the independence claim from Mountain toward Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_analysis_necessity, conceptual, 'Whether ordinal analysis beyond ε₀ is fundamentally necessary for Goodstein proofs').

omega_variable(
    synthetic_versus_analytic_incompleteness,
    'Is Goodstein''s independence from PA a synthetic fact about the structure of arithmetic, or is it analytic—a necessary consequence of PA''s definition?',
    'Philosophical analysis of axiomatization choices; study of alternative first-order arithmetics and their incompleteness profiles; investigation of whether different PA-equivalent systems exhibit the same independence pattern',
    'If synthetic: the constraint is a discovered law (Mountain). If analytic: the constraint is a tautology of the formalism (still Mountain, but with different metaphysical status). Either way, the classification does not change, but the mandatrophy reasoning shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_versus_analytic_incompleteness, conceptual, 'Whether Goodstein independence is synthetic or analytic to PA''s axiomatization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kirby_paris_theorem, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kpt_tr_t0, kirby_paris_theorem, theater_ratio, 0, 0.15).
narrative_ontology:measurement(kpt_tr_t30, kirby_paris_theorem, theater_ratio, 30, 0.15).
narrative_ontology:measurement(kpt_tr_t60, kirby_paris_theorem, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(kpt_be_t0, kirby_paris_theorem, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(kpt_be_t30, kirby_paris_theorem, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(kpt_be_t60, kirby_paris_theorem, base_extractiveness, 60, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kirby_paris_theorem, godel_incompleteness_first).
narrative_ontology:affects_constraint(kirby_paris_theorem, ordinal_analysis_proof_strength).

% DUAL FORMULATION NOTE:
% The Kirby-Paris theorem stands at the intersection of three constraint families: (1) Gödel's incompleteness results—the parent Mountain from which Kirby-Paris derives as a specific instantiation; (2) ordinal analysis frameworks—the technical machinery required to escape PA; (3) transfinite induction limits—the foundational boundary that makes ordinal analysis necessary. This story focuses on the independence result itself as a structural constraint on what first-order arithmetic can express. The upstream constraint (Gödel incompleteness) is more general; the downstream constraints (specific independence results for various formal systems) are more specialized instances.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
