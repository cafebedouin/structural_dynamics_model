% ============================================================================
% CONSTRAINT STORY: nash_equilibrium_coordination
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_nash_equilibrium_coordination, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: nash_equilibrium_coordination
 * human_readable: Nash Equilibrium
 * domain: economic/social
 * * SUMMARY:
 * A Nash Equilibrium is a state in a non-cooperative game where no player can improve their outcome by changing their strategy unilaterally, given the strategies of others. It represents a fundamental structural "gravity" that stabilizes social and economic systems, often at sub-optimal levels. This constraint is the logic that traps agents in patterns like the Prisoner's Dilemma or market price collusion.
 * * KEY AGENTS:
 * - The Strategic Actor: The subject trapped in a state where deviating alone results in a worse personal outcome.
 * - The Institutional Architect: A beneficiary who uses the equilibrium as a "Rope" to design stable market rules or voting systems.
 * - The Collective: The victim whose potential for a better, Pareto-optimal outcome is extracted to maintain individual stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(nash_equilibrium_coordination, 0.52). % Extraction of Pareto-optimal potential. High enough for Tangled Rope.
domain_priors:suppression_score(nash_equilibrium_coordination, 0.45).   % Suppresses cooperative alternatives by making them individually irrational.
domain_priors:theater_ratio(nash_equilibrium_coordination, 0.1).       % Low theater; the logic is highly functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(nash_equilibrium_coordination, extractiveness, 0.52).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% It is often presented as an inescapable law of rational behavior.
narrative_ontology:constraint_claim(nash_equilibrium_coordination, tangled_rope).
narrative_ontology:human_readable(nash_equilibrium_coordination, "Nash Equilibrium").

% Binary flags
% The equilibrium is "enforced" by the rational calculations of the agents themselves.
% No external police force is needed, but the structure requires this self-enforcement.
domain_priors:requires_active_enforcement(nash_equilibrium_coordination).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(nash_equilibrium_coordination, incumbents_in_stable_systems).
narrative_ontology:constraint_victim(nash_equilibrium_coordination, collective_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE STRATEGIC ACTOR (MOUNTAIN)
% For an individual shopkeeper or prisoner, the equilibrium is an unchangeable
% fact of their environment. Deviating is irrational and costly.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL ARCHITECT (ROPE)
% A central banker or market designer sees the equilibrium as a tool for
% coordination, using policy to nudge agents toward a desirable stable state.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (for incumbents) and the
% asymmetric extraction (of collective potential). The logic is self-enforcing.
% This combination is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(nash_equilibrium_coordination, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nash_equilibrium_coordination_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Mountain), institutional (Rope), and analytical (Tangled Rope) views.
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == rope,
    TypeAnalytical == tangled_rope,
    TypePowerless \= TypeInstitutional.

test(tangled_rope_structural_requirements) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(nash_equilibrium_coordination, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(nash_equilibrium_coordination, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(nash_equilibrium_coordination).

:- end_tests(nash_equilibrium_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this model is the perspectival gap. To a powerless agent inside the game, the equilibrium is a 'Mountain'—an immutable law of their reality. To an institutional agent who can change the game's payoffs, it's a 'Rope' for achieving stable coordination.
 * The analytical classification is 'Tangled Rope' because the constraint meets all three structural requirements:
 * 1. Coordination Function: It stabilizes outcomes for beneficiaries (e.g., incumbents).
 * 2. Asymmetric Extraction: It extracts the potential for a better collective outcome (Pareto improvement) from the victims (the collective).
 * 3. Active Enforcement: The logic of individual rationality actively enforces the equilibrium, preventing deviation.
 * This prevents the system from misclassifying this complex social phenomenon as either a pure Mountain (ignoring its constructed nature) or a pure Snare (ignoring its genuine coordination function for some agents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nash_equilibrium_coordination,
    'In games with multiple equilibria, what mechanism selects the one a society settles into?',
    'Historical analysis of focal points, cultural path-dependency, and initial conditions.',
    'If path-dependent, the equilibrium is a Snare of history. If purely conventional, it is a Rope that could be changed.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nash_equilibrium_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The concept of Nash Equilibrium has been stable since its formulation.
% The extraction and theater have not significantly drifted over the interval.
%
% Theater ratio over time:
narrative_ontology:measurement(nash_equilibrium_coordination_tr_t0, nash_equilibrium_coordination, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nash_equilibrium_coordination_tr_t5, nash_equilibrium_coordination, theater_ratio, 5, 0.1).
narrative_ontology:measurement(nash_equilibrium_coordination_tr_t10, nash_equilibrium_coordination, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(nash_equilibrium_coordination_ex_t0, nash_equilibrium_coordination, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(nash_equilibrium_coordination_ex_t5, nash_equilibrium_coordination, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(nash_equilibrium_coordination_ex_t10, nash_equilibrium_coordination, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The equilibrium governs how agents allocate strategic choices.
narrative_ontology:coordination_type(nash_equilibrium_coordination, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */