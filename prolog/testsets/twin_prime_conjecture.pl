% ============================================================================
% CONSTRAINT STORY: constraint_twin_prime_conjecture
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-13
% ============================================================================

:- module(constraint_twin_prime_conjecture, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constraint_twin_prime_conjecture
 *   human_readable: The Unproven Nature of the Twin Prime Conjecture
 *   domain: mathematical / logical
 *
 * SUMMARY:
 *   The twin prime conjecture posits that there are infinitely many prime pairs (p, p+2).
 *   Despite overwhelming numerical evidence and significant progress on related problems
 *   (e.g., bounded prime gaps), a formal proof remains elusive. This constraint represents
 *   the boundary of current mathematical knowledge—a problem whose truth seems apparent
 *   but is beyond the reach of existing formal methods. It is a natural law of logic,
 *   unchangeable by any agent, only discoverable.
 *
 * KEY AGENTS (by structural relationship):
 *   - Research Mathematician: (moderate/mobile) — Experiences the constraint as a fundamental challenge guiding research.
 *   - Novice Mathematician: (powerless/trapped) — Experiences the constraint as an absolute, unassailable barrier to understanding.
 *   - Mathematical Institutions: (institutional/analytical) — Views the constraint as a stable feature of the intellectual landscape.
 *   - Analytical Observer: (analytical/analytical) — Sees the full structure of the problem as a limit of formal systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Epsilon is low, representing the fact that the constraint is a feature of
% a logical system, not an imposed cost. The "extraction" is the intellectual
% effort required to grapple with it.
domain_priors:base_extractiveness(constraint_twin_prime_conjecture, 0.15).
% Suppression is near-zero. No alternative mathematical realities are being suppressed.
domain_priors:suppression_score(constraint_twin_prime_conjecture, 0.05).
% Theater is low. The work is genuine research, not performative.
domain_priors:theater_ratio(constraint_twin_prime_conjecture, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, extractiveness, 0.15).
narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
% A mathematical truth has near-total collapse of alternatives and faces no meaningful resistance.
narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, resistance, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(constraint_twin_prime_conjecture, mountain).
narrative_ontology:human_readable(constraint_twin_prime_conjecture, "The Unproven Nature of the Twin Prime Conjecture").
narrative_ontology:topic_domain(constraint_twin_prime_conjecture, "mathematical / logical").

% --- Binary flags ---
% No sunset clause or enforcement needed for a mathematical conjecture.

% --- Emergence flag (required for mountain constraints) ---
% The constraint emerges from the axioms of number theory.
domain_priors:emerges_naturally(constraint_twin_prime_conjecture).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain constraint representing a logical limit,
% the concepts of 'beneficiary' and 'victim' are not structurally applicable.
% The challenge it poses to mathematicians is an effect, not a targeted extraction.
%
% narrative_ontology:constraint_beneficiary(constraint_twin_prime_conjecture, mathematical_community).
% narrative_ontology:constraint_victim(constraint_twin_prime_conjecture, mathematicians).

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

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it represents a fundamental,
% objective feature of a formal system.

% PERSPECTIVE 1: THE NOVICE MATHEMATICIAN (MOUNTAIN)
% For a student or amateur, the problem is an absolute, unchangeable barrier.
constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE RESEARCH MATHEMATICIAN (MOUNTAIN)
% For the active researcher, it is a fixed challenge to be overcome, a feature
% of the landscape they are trying to map.
constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE MATHEMATICAL INSTITUTION (MOUNTAIN)
% From an institutional view, it's a stable, long-term feature of the field
% that organizes research programs and funding.
constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context, which correctly identifies the constraint
% as a fundamental limit of current formal methods.
constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_twin_prime_conjecture_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain across all defined perspectives.
    constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, TypeModerate, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(constraint_twin_prime_conjecture, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == mountain,
    TypeModerate == mountain,
    TypeInstitutional == mountain.

test(mountain_threshold_validation) :-
    narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, extractiveness, E),
    narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_metrics) :-
    narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, accessibility_collapse, Collapse),
    narrative_ontology:constraint_metric(constraint_twin_prime_conjecture, resistance, Resistance),
    Collapse >= 0.85,
    Resistance =< 0.15.

:- end_tests(constraint_twin_prime_conjecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores reflect that the unproven nature of the twin prime conjecture is a
 *   fundamental feature of the current state of mathematics, not an imposed or
 *   coercive constraint. Base extractiveness (0.15) represents the intellectual
 *   cost of engaging with the problem. Suppression (0.05) is minimal, as no
 *   alternative mathematical frameworks are being actively suppressed. The NL
 *   profile metrics (collapse=0.98, resistance=0.02) confirm its status as a
 *   natural law within its formal system.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All agents, regardless of their power, time
 *   horizon, or exit options, perceive the constraint as a Mountain. This
 *   invariance is a key signature of a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. As a Mountain constraint, directionality is undefined. The
 *   notions of beneficiary and victim do not apply to a fundamental feature of a
 *   logical system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain correctly identifies this as a feature of
 *   reality (in this case, a formal system) rather than a Snare (a coercive rule)
 *   or a Piton (an obsolete rule). It prevents misinterpreting an intellectual
 *   challenge as a form of social extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_twin_prime,
    'Is the twin prime conjecture provable within ZFC, or is it independent?',
    'Development of a novel proof technique or a proof of independence.',
    'If provable, the constraint is resolved. If independent, it becomes a permanent, known limit of the ZFC system, akin to the Continuum Hypothesis.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constraint_twin_prime_conjecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness is low (0.15 < 0.46).
% The provided data shows a stable, non-drifting constraint, consistent with a Mountain.
narrative_ontology:measurement(twin_prime_tr_t0, constraint_twin_prime_conjecture, theater_ratio, 0, 0.2).
narrative_ontology:measurement(twin_prime_tr_t5, constraint_twin_prime_conjecture, theater_ratio, 5, 0.3).
narrative_ontology:measurement(twin_prime_tr_t10, constraint_twin_prime_conjecture, theater_ratio, 10, 0.3).

narrative_ontology:measurement(twin_prime_ex_t0, constraint_twin_prime_conjecture, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(twin_prime_ex_t5, constraint_twin_prime_conjecture, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(twin_prime_ex_t10, constraint_twin_prime_conjecture, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% A mathematical conjecture serves as a standard for what is known vs. unknown.
narrative_ontology:coordination_type(constraint_twin_prime_conjecture, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(constraint_twin_prime_conjecture, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(constraint_twin_prime_conjecture, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(constraint_twin_prime_conjecture, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */