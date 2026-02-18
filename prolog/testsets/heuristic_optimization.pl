% ============================================================================
% CONSTRAINT STORY: heuristic_optimization
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_heuristic_optimization, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: heuristic_optimization
 *   human_readable: Heuristic Optimization ("Good Enough" Solutions)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Heuristics are problem-solving strategies or "rules of thumb" that
 *   prioritize speed and practicality over guaranteed optimality. They provide
 *   functional, "good enough" solutions for complex problems (like NP-hard
 *   problems) where finding a perfect solution is computationally intractable.
 *   This constraint represents the trade-off between tractability and perfection.
 *
 * KEY AGENTS (by structural relationship):
 *   - AI Assistant / Embedded System: Primary target (powerless/trapped) — executes the heuristic as an immutable law.
 *   - Engineering Firm: Primary beneficiary (institutional/mobile) — uses heuristics to deliver projects on time and budget.
 *   - Optimization Theorist: Analytical observer (analytical/analytical) — evaluates the gap between the heuristic solution and the theoretical optimum.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heuristic_optimization, 0.20).
domain_priors:suppression_score(heuristic_optimization, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(heuristic_optimization, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heuristic_optimization, extractiveness, 0.20).
narrative_ontology:constraint_metric(heuristic_optimization, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(heuristic_optimization, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Required because one perspective is 'mountain'.
narrative_ontology:constraint_metric(heuristic_optimization, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(heuristic_optimization, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(heuristic_optimization, rope).
narrative_ontology:topic_domain(heuristic_optimization, "technological/mathematical").
narrative_ontology:human_readable(heuristic_optimization, "Heuristic Optimization (\"Good Enough\" Solutions)").

% --- Emergence flag (required for mountain constraints) ---
% Heuristics emerge naturally from cognitive and computational scarcity.
% Required for the mountain metric gate to fire for the powerless perspective.
domain_priors:emerges_naturally(heuristic_optimization).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(heuristic_optimization, real_time_systems_and_engineers).
%
% Who bears disproportionate cost? (The "cost" is the loss of perfection)
narrative_ontology:constraint_victim(heuristic_optimization, theoretical_optimality_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE AI ASSISTANT / EMBEDDED SYSTEM (MOUNTAIN)
% For an embedded system or AI assistant, the heuristic is an immutable law
% of its operation. It cannot choose another path; the "rule of thumb" is
% its reality. The low suppression and extraction make it a Mountain.
constraint_indexing:constraint_classification(heuristic_optimization, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ENGINEERING FIRM (ROPE)
% For the firm, heuristics are a pure coordination tool (Rope) to deliver
% functional solutions within budget and time. They coordinate resources
% towards a viable outcome, where the pursuit of perfection would lead to
% project failure.
constraint_indexing:constraint_classification(heuristic_optimization, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% The theorist or auditor sees the full trade-space. They recognize that
% the heuristic is a coordination mechanism to solve intractable problems.
% While they are aware of the sub-optimality (the extraction), the overall
% structure is a Rope that enables progress where none would otherwise be possible.
constraint_indexing:constraint_classification(heuristic_optimization, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heuristic_optimization_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(heuristic_optimization, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heuristic_optimization, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == mountain,
    TypeBeneficiary == rope.

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(heuristic_optimization, ClaimedType),
    constraint_indexing:constraint_classification(heuristic_optimization, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(heuristic_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.20): Low. The "cost" is the gap between the
 *     heuristic solution and the theoretical optimum, a form of opportunity cost.
 *   - Suppression (S=0.05): Very low. Heuristics don't suppress alternatives;
 *     they are chosen precisely because alternatives (like exhaustive search)
 *     are already rendered inaccessible by computational complexity. This low
 *     score is critical for the 'mountain' classification to be valid.
 *   - NL Profile: The `accessibility_collapse` is high (0.95) for the powerless
 *     agent, as the heuristic is its only mode of operation. `resistance` is
 *     low (0.05) as the agent cannot resist its own programming.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between Mountain and Rope. For the powerless AI assistant, the
 *   heuristic is an unchangeable law of its environment (Mountain). For the
 *   engineering firm that designs or chooses the heuristic, it's a flexible
 *   coordination tool to manage complexity (Rope). The agent's ability to
 *   choose or alter the constraint determines the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `real_time_systems_and_engineers` benefit from having a
 *     workable, fast solution that allows them to function.
 *   - Victims: `theoretical_optimality_seekers` bear the cost, which is the
 *     loss of perfection. The system extracts from the "ideal solution space"
 *     to provide a practical one.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of a functional Rope. It could be
 *   misinterpreted as a Snare by a theorist who is "trapped" by their desire
 *   for perfection. However, the low base extractiveness (ε=0.20) makes a Snare
 *   classification structurally impossible (requires χ ≥ 0.66). The framework
 *   correctly identifies it as a coordination mechanism, not a coercive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_heuristic_optimization,
    'Does the practical value of "good enough" solutions render the pursuit of theoretical optimality a form of theatrical performance in most real-world domains?',
    'Longitudinal studies comparing the total lifecycle cost (including computation and delay) of optimal solutions versus heuristic solutions in critical industries (e.g., logistics, drug discovery).',
    'If heuristics are sufficient, the value of pure optimization theory becomes primarily academic (a Piton). If optimality provides critical leverage, heuristics remain a necessary but compromised Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_heuristic_optimization, conceptual, 'Is the pursuit of theoretical optimality a practical goal or a theatrical one in complex systems?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(heuristic_optimization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is low (0.20), so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Heuristics are a form of information standard for decision-making.
narrative_ontology:coordination_type(heuristic_optimization, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */