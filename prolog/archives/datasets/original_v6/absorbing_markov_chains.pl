% ============================================================================
% CONSTRAINT STORY: absorbing_markov_chains
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_absorbing_markov_chains, []).

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
 *   constraint_id: absorbing_markov_chains
 *   human_readable: State Space Trap in Absorbing Markov Chain
 *   domain: technological/mathematical_foundations
 *
 * SUMMARY:
 *   An absorbing Markov chain is a mathematical structure in which certain
 *   states (absorbing states) have the property that, once entered, the chain
 *   can never leave them. Formally, if state i is absorbing, then P_ii = 1.0
 *   and P_ij = 0 for all j ≠ i. This constraint is a paradigmatic example of
 *   a mountain—an irreducible structural limit that emerges from the
 *   mathematical definition itself. The trap is not imposed by an external
 *   agent or contingent social arrangement; it is a necessary feature of the
 *   formal system. The constraint applies identically across all observer
 *   positions, all time horizons, and all spatial scopes. No beneficiary or
 *   victim relationship exists because the constraint is not extractive—it is
 *   a boundary condition that all agents must respect equally. The absorbing
 *   state trap reveals the difference between constraints that are engineered
 *   (and hence negotiable) and constraints that are structural (and hence
 *   inviolable).
 *
 * KEY AGENTS:
 *   - System Designer: Agent who chooses the absorbing state structure (powerful/mobile) — creates the trap intentionally, but cannot escape it
 *   - Trapped Agent: Entity that enters the absorbing state (moderate/constrained) — experiences absolute irreversibility
 *   - Downstream Stakeholder: Institution relying on the Markov chain system (institutional/arbitrage) — can avoid the trap prospectively but not retrospectively
 *   - Analytical Observer: Mathematical/scientific perspective (analytical/analytical) — recognizes the constraint as a necessary feature of the formal system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absorbing_markov_chains, 0.12).
domain_priors:suppression_score(absorbing_markov_chains, 0.03).
domain_priors:theater_ratio(absorbing_markov_chains, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absorbing_markov_chains, extractiveness, 0.12).
narrative_ontology:constraint_metric(absorbing_markov_chains, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(absorbing_markov_chains, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(absorbing_markov_chains, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(absorbing_markov_chains, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absorbing_markov_chains, mountain).
narrative_ontology:human_readable(absorbing_markov_chains, "State Space Trap in Absorbing Markov Chain").
narrative_ontology:topic_domain(absorbing_markov_chains, "technological/mathematical_foundations").

domain_priors:emerges_naturally(absorbing_markov_chains).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL STRUCTURE (MOUNTAIN) — From the standpoint of formal mathematics, the absorbing state property is a logical necessity that follows from the definition of an absorbing Markov chain. Once a state is designated as absorbing (by construction: P_ii = 1.0, P_ij = 0 for j ≠ i), the chain cannot transition out. This is not contingent on implementation or observer position—it is a structural feature of the mathematical object itself. No agent can circumvent this; no exit option exists from the formal system.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SYSTEM DESIGNER (MOUNTAIN) — A system architect choosing to model a process as an absorbing Markov chain deliberately creates irreversibility. The trap is not externally imposed—it is a design choice. Yet once the choice is made and the chain instantiated, the absorbing state becomes logically inescapable. Even the designer cannot violate the mathematical structure they created. The absorbing property holds independently of the designer's power or intent.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: TRAPPED AGENT / SUBJECTIVE VIEW (MOUNTAIN) — An entity that enters an absorbing state (bankruptcy, locked-in dataset, obsolete system state) experiences the trap as an absolute boundary. From the agent's internal perspective, there are no degrees of freedom, no negotiations, no hidden exit paths. The mathematical constraint becomes lived reality. Suppression is absolute because the boundary is not socially constructed or negotiable—it is enforced by the formal structure of the state space.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOWNSTREAM STAKEHOLDER / INSTITUTIONAL (MOUNTAIN) — An institution relying on a Markov chain system that absorbs (e.g., a data processing pipeline, a state machine for resource allocation) faces an inescapable constraint: once the system enters certain states, recovery is impossible. No institutional workaround can override the mathematical structure. Arbitrage options (switching systems, redefining the state space) represent a way to avoid the trap prospectively, but once trapped, no arbitrage is available. The constraint is absolute.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(absorbing_markov_chains_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(absorbing_markov_chains, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(absorbing_markov_chains, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(absorbing_markov_chains, ExtMetricName, E),
    domain_priors:suppression_score(absorbing_markov_chains, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(absorbing_markov_chains),
    narrative_ontology:constraint_metric(absorbing_markov_chains, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(absorbing_markov_chains, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(absorbing_markov_chains_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The absorbing state property does not extract value from any agent—it is a symmetrical constraint that applies uniformly. No redistribution occurs; no asymmetry in who benefits or who pays. The minimal non-zero value reflects that the constraint has real consequences (agents become trapped), but these consequences are not due to extraction by a beneficiary. They are due to the mathematical structure itself. Suppression (0.03): Minimal. There is no suppression in the sense of coercive enforcement or hidden alternatives. The constraint is transparent and operates through logical necessity. Once the state space is defined and the transition matrix is set, the absorbing property is evident to all observers. Theater ratio (0.15): Low. No performative component exists. The mathematical constraint is exact and requires no theatrical performance to maintain. The minimal non-zero value reflects only the inherent abstraction between the formal system and its real-world instantiation.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint identically as Mountain. There is no perspectival gap because the absorbing state property is not relative to observer position—it is a structural feature of the mathematical object. The system designer, the trapped agent, the downstream stakeholder, and the analytical observer all perceive the same constraint: once an absorbing state is entered, exit is impossible. This uniformity across perspectives confirms the mountain classification. The constraint exhibits the hallmark property of mountains: invariance under changes in observer position, power level, time horizon, or spatial scope.
 *
 * DIRECTIONALITY LOGIC:
 *   The standard directionality derivation does not apply to this constraint because there is no beneficiary or victim. Mountains are characterized by the absence of asymmetric extraction. All agents—regardless of power level or exit options—experience the same logical boundary. The absorbing state property does not distribute benefits to one group while imposing costs on another. Instead, it imposes a symmetrical constraint on the entire state space. This is why no directionality override is needed and no beneficiary/victim declaration is meaningful.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face a mandatrophy resolution problem because it exhibits no confusion between coordination and extraction. It is neither: it is a structural boundary. The mountain classification is not threatened by alternative interpretations. The absorbing state property is not contingent on how we measure it, what domain we apply it to, or what perspective we adopt. It follows logically from the definition of an absorbing Markov chain. The only omega variable that matters (state space redefinition) is conceptual, not empirical—it asks whether the mathematical formalism itself is an appropriate choice, not whether the constraint is correctly classified within the formalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_space_redefinition,
    'Can the state space be redefined or extended to convert the absorbing state into a transient state in a larger chain?',
    'Formal analysis of state space transformations; examples of ''escape'' via augmented Markov chain structures (e.g., hierarchical or layered chains)',
    'If state space is immutable: Mountain classification confirmed (absolute trap). If state space is malleable: Mountain is contingent on the choice of state space representation—the true trap is a meta-level constraint (choice of formalism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_space_redefinition, conceptual, 'Whether state space can be redefined to escape absorbing states').

omega_variable(
    probabilistic_escape_boundary,
    'Is the mathematical zero-probability of escape (P_escape = 0.0) an exact boundary or an asymptotic limit?',
    'Rigorous analysis of probability measure theory; investigation of whether numerical approximations or continuous relaxations of the discrete chain alter the boundary',
    'If exact zero: Mountain. If asymptotic limit: Mountain is an artifact of discretization; a continuous relaxation might permit rare escapes, converting the constraint to Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probabilistic_escape_boundary, conceptual, 'Whether escape probability is exactly zero or asymptotic').

omega_variable(
    absorbing_state_purpose,
    'When an absorbing Markov chain is used to model real processes (retirement, system shutdown, bankruptcy), is the absorbing property a feature or a flaw?',
    'Case analysis across domains (finance, operations, reliability engineering); assessment of whether absorbing states match or violate domain-level recovery requirements',
    'If feature: Mountain captures domain reality (retirement is irreversible). If flaw: Mountain reveals a mismatch between mathematical model and domain, suggesting alternative formalisms (continuous-time chains, hierarchical structures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorbing_state_purpose, preference, 'Whether absorbing states represent domain features or modeling flaws').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absorbing_markov_chains, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amct_tr_t0, absorbing_markov_chains, theater_ratio, 0, 0.1).
narrative_ontology:measurement(amct_tr_t1, absorbing_markov_chains, theater_ratio, 1, 0.12).
narrative_ontology:measurement(amct_tr_t2, absorbing_markov_chains, theater_ratio, 2, 0.15).

% Extraction over time
narrative_ontology:measurement(amct_be_t0, absorbing_markov_chains, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(amct_be_t1, absorbing_markov_chains, base_extractiveness, 1, 0.11).
narrative_ontology:measurement(amct_be_t2, absorbing_markov_chains, base_extractiveness, 2, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(absorbing_markov_chains, information_standard).
narrative_ontology:affects_constraint(absorbing_markov_chains, path_dependence_in_system_dynamics).
narrative_ontology:affects_constraint(absorbing_markov_chains, technological_lock_in).

% DUAL FORMULATION NOTE:
% Absorbing Markov chains form a mathematical family with two related constraints: (1) the absorbing state property (this story, ε=0.12, Mountain), which is a formal necessity; (2) the practical lock-in when absorbing states model irreversible real-world transitions (technological_lock_in, higher ε, Tangled Rope), which involves social and institutional barriers to reversibility. The first constraint is about the mathematics; the second is about the institutional consequences of using that mathematics. Both stories link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
