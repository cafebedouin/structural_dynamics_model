% ============================================================================
% CONSTRAINT STORY: halting_problem_decidability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_halting_problem_decidability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: halting_problem_decidability
 *   human_readable: Halting Problem Decidability
 *   domain: theoretical_computer_science/mathematical_logic
 *
 * SUMMARY:
 *   The halting problem decidability constraint is a pure mathematical limit
 *   on computation. It states that no algorithm can, in finite time,
 *   determine for all possible program-input pairs whether that program will
 *   eventually halt or run forever. Alan Turing proved this in 1936 through a
 *   diagonal argument (self-referential proof by contradiction). The
 *   constraint is logically unavoidable within any formal system of
 *   computational power equivalent to or exceeding the Turing machine. It has
 *   persisted unchanged for nearly 90 years because it is not a constraint
 *   enforced by agents or institutions — it is a feature of the logical
 *   landscape itself. There are no beneficiaries or victims, no extraction
 *   mechanism, and no suppression. The constraint simply exists: some
 *   problems are undecidable. The structure is identical from every observer
 *   position. This is a uniform-type mountain (NL-type uniform).
 *
 * KEY AGENTS:
 *   - Programmers: Encounter the limit when attempting to build universal debugging tools or program analysis systems
 *   - Theoretical Computer Scientists: Prove and refine the boundary between decidable and undecidable problems
 *   - Computer Science Institutions: Teach the halting problem as a foundational result constraining all subsequent theory
 *   - Formal Logic Community: Recognize undecidability as a deep structural feature connecting computation, mathematics, and Gödel incompleteness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(halting_problem_decidability, 0.08).
domain_priors:suppression_score(halting_problem_decidability, 0.02).
domain_priors:theater_ratio(halting_problem_decidability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(halting_problem_decidability, extractiveness, 0.08).
narrative_ontology:constraint_metric(halting_problem_decidability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(halting_problem_decidability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(halting_problem_decidability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(halting_problem_decidability, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(halting_problem_decidability, mountain).
narrative_ontology:human_readable(halting_problem_decidability, "Halting Problem Decidability").
narrative_ontology:topic_domain(halting_problem_decidability, "theoretical_computer_science/mathematical_logic").

domain_priors:emerges_naturally(halting_problem_decidability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRAMMER (MOUNTAIN) — A developer attempting to build a universal halting detector faces an absolute logical barrier. No amount of computational power, cleverness, or resources can overcome this limit. The barrier is not enforced but inherent — it is a feature of logic itself, not a constraint imposed by external agents.
constraint_indexing:constraint_classification(halting_problem_decidability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THEORETICAL COMPUTER SCIENTIST (MOUNTAIN) — From the position of formal analysis, the halting problem is undecidable by Turing's proof. The proof is constructive and universal: it applies to all possible abstract machines with equivalent computational capacity. There is no degree of freedom. The boundary between decidable and undecidable problems is immutable across all formal systems of equivalent power.
constraint_indexing:constraint_classification(halting_problem_decidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: COMPUTER SCIENCE COMMUNITY (MOUNTAIN) — The undecidability of the halting problem is a foundational theorem that constrains all subsequent theory. Research institutions, funding bodies, and curriculum designers all operate under this constraint. It cannot be negotiated, legislated, or innovated away. The constraint is invariant across all institutional positions and has been since 1936.
constraint_indexing:constraint_classification(halting_problem_decidability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(halting_problem_decidability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(halting_problem_decidability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(halting_problem_decidability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(halting_problem_decidability, ExtMetricName, E),
    domain_priors:suppression_score(halting_problem_decidability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(halting_problem_decidability),
    narrative_ontology:constraint_metric(halting_problem_decidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(halting_problem_decidability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(halting_problem_decidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness = 0.08: Minimal. The constraint does not extract value from anyone. It is not a mechanism for transferring resources or power from one agent to another. It is a logical boundary. Suppression = 0.02: Minimal. There is no coercion. Agents are not forced to accept the constraint through threats or barriers — they accept it because it is true. Theater ratio = 0.05: Minimal. There is no performative activity. The proof is direct and constructive. The constraint emerges naturally from the logical structure of self-reference and the definition of decidability itself. Accessibility collapse = 0.95: Extremely high. Every attempted workaround (restricting to finite programs, using probabilistic methods, limiting input domains) either fails to decide the general halting problem or shifts to a different constraint. The logical landscape offers no accessible alternatives. Resistance = 0.03: Minimal. The proof has been refined, formalized, and cross-verified from multiple angles (Rice's theorem, incompleteness connection, oracle machines, quantum complexity). Resistance to the fundamental result is near zero.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The halting problem is undecidable from every observer position. A programmer trying to build a halting detector, a theorist analyzing computability, and an institution designing a CS curriculum all encounter exactly the same constraint: it cannot be done. This is a uniform-type mountain. The classification outcome is invariant across all indexical tuples because the constraint itself is logical rather than structural. No degree of power, time horizon, exit options, or scope changes the truth of Turing's theorem.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint. There is no extraction flow. No agent benefits at another agent's expense. The constraint has zero beneficiaries and zero victims — it is a feature of the logical landscape, not a relationship between agents. The chi formula does not apply because there is no directionality value d to compute. This is a structural signature of a true mountain: the absence of any asymmetric relationship or flow of resources.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypercomputation_feasibility,
    'Could a hypercomputer (oracle machine, quantum system with access to infinite resources, or non-Turing model) decide the halting problem for Turing machines?',
    'Formal proof that oracle machines can decide the halting problem for standard Turing machines; analysis of whether such proofs require abandoning Turing-equivalence or accepting a hierarchy of undecidability levels',
    'If hypercomputation is possible and feasible: the constraint is local to Turing-class systems, not universal. If hypercomputation is unphysical or requires resources exceeding any realizable system: the constraint remains effectively universal for all practical computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypercomputation_feasibility, conceptual, 'Whether hypercomputation could circumvent halting problem undecidability').

omega_variable(
    problem_formulation_boundary,
    'Does the undecidability result depend on the specific formulation (self-referential proof by contradiction) or is it truly independent of proof technique?',
    'Analysis of alternative proofs of halting undecidability (Rice''s theorem, Gödel incompleteness connection); verification that all proofs converge on the same conclusion regardless of formulation',
    'If formulation-dependent: the constraint might be overcome by choosing a different problem statement or encoding. If formulation-independent: the constraint is deeper and truly unavoidable. Current evidence strongly supports formulation-independence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(problem_formulation_boundary, conceptual, 'Whether undecidability depends on specific proof technique').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(halting_problem_decidability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(halt_tr_t0, halting_problem_decidability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(halt_tr_t10, halting_problem_decidability, theater_ratio, 10, 0.05).
narrative_ontology:measurement(halt_tr_t20, halting_problem_decidability, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(halt_be_t0, halting_problem_decidability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(halt_be_t10, halting_problem_decidability, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(halt_be_t20, halting_problem_decidability, base_extractiveness, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(halting_problem_decidability, information_standard).
narrative_ontology:affects_constraint(halting_problem_decidability, godel_incompleteness).
narrative_ontology:affects_constraint(halting_problem_decidability, rice_theorem).
narrative_ontology:affects_constraint(halting_problem_decidability, turing_machine_universality).

% DUAL FORMULATION NOTE:
% The halting problem undecidability is a foundational result that constrains the entire landscape of computability theory. It does not decompose into multiple constraints with different epsilon values — the boundary between decidable and undecidable is invariant across all formulations. However, it relates structurally to Gödel incompleteness (both involve self-reference and logical limits) and Rice's theorem (which generalizes undecidability to all non-trivial properties of programs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
