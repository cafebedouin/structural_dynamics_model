% ============================================================================
% CONSTRAINT STORY: abstraction_boundary_overrun
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abstraction_boundary_overrun, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abstraction_boundary_overrun
 *   human_readable: The Leaky Black-Box Collapse
 *   domain: technological/computational
 *
 * SUMMARY:
 *   The Leaky Black-Box Collapse occurs when a system designed to abstract
 *   away complexity fails to do so, exposing users and developers to the
 *   messy details it was meant to hide. This results in increased cognitive
 *   load, decreased productivity, and a general sense of frustration. The
 *   abstraction becomes a 'leaky' black box, its internal workings spilling
 *   out and contaminating the user experience. This typically occurs when
 *   underlying complexity grows beyond the abstraction's capacity to manage
 *   it, or when the abstraction itself introduces new layers of complexity.
 *
 * KEY AGENTS:
 *   - Users of the Abstraction: Primary target (powerless/trapped) – bear the costs of increased complexity and decreased productivity.
 *   - Downstream Developers: Secondary target (moderate/constrained) – must deal with the leaky abstraction when building upon it.
 *   - Vendors of Complexity Solutions: Primary beneficiary (institutional/arbitrage) – profit from the need to manage the complexity exposed by the leaky abstraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abstraction_boundary_overrun, 0.55).
domain_priors:suppression_score(abstraction_boundary_overrun, 0.65).
domain_priors:theater_ratio(abstraction_boundary_overrun, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abstraction_boundary_overrun, extractiveness, 0.55).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abstraction_boundary_overrun, tangled_rope).
narrative_ontology:human_readable(abstraction_boundary_overrun, "The Leaky Black-Box Collapse").
narrative_ontology:topic_domain(abstraction_boundary_overrun, "technological/computational").

domain_priors:requires_active_enforcement(abstraction_boundary_overrun).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abstraction_boundary_overrun, vendors_of_complexity_solutions).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, users_of_the_abstraction).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, downstream_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The end user, trapped by the complexity and lack of alternatives, experiences the full extraction of the leaky abstraction as a snare.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The downstream developer is constrained by the need to work with the abstraction, but may also benefit from it to some extent, experiencing the situation as a tangled rope.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Vendors who offer solutions to the underlying complexity benefit from the leaky abstraction, seeing it as a rope that enables their business model.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The high levels of complexity and suppression lead analytical observers to conclude that this is an example of a failed paradigm or degraded pattern. The theater of maintaining an abstraction that is now failing (but which was once useful) is now a useless ritual.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_boundary_overrun_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abstraction_boundary_overrun, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abstraction_boundary_overrun, TR),
    TR >= 0.70.

:- end_tests(abstraction_boundary_overrun_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The leaky abstraction extracts time and effort from users and developers, forcing them to grapple with complexity they shouldn't have to. Suppression (0.65): High. Users are often locked into the abstraction due to lack of alternatives or high switching costs, limiting their ability to escape the extraction. Theater Ratio (0.40): Moderate. There's some performative activity in maintaining the failing abstraction, but also a genuine attempt to keep it functioning as intended. The ratio tends to increase over time as the abstraction becomes more degraded, leading to a piton categorization by analytical observers.
 *
 * PERSPECTIVAL GAP:
 *   The end user, trapped by the complexity and lack of alternatives, experiences the full extraction of the leaky abstraction as a snare. The downstream developer is constrained by the need to work with the abstraction, but may also benefit from it to some extent, experiencing the situation as a tangled rope. The vendors who offer solutions to the underlying complexity benefit from the leaky abstraction, seeing it as a rope that enables their business model.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural relationship to the leaky abstraction. Users are primary targets (high d), vendors are primary beneficiaries (low d), and downstream developers are somewhere in between (moderate d). The high d for users reflects their lack of exit options and the significant costs they bear.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_source,
    'Is the underlying complexity inherent or accidental?',
    'Historical analysis of the system''s evolution and design decisions.',
    'If inherent, the abstraction was always doomed to leak. If accidental, better design might have prevented the collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_source, empirical, 'Whether complexity is inherent or accidental').

omega_variable(
    alternative_abstraction_availability,
    'Are there alternative, less leaky abstractions available?',
    'Comparative analysis of different abstraction implementations and their tradeoffs.',
    'If alternatives exist, users have an exit option and the situation is less of a snare. If not, the extraction is unavoidable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_abstraction_availability, empirical, 'Availability of alternative abstractions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abstraction_boundary_overrun, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abst_tr_t0, abstraction_boundary_overrun, theater_ratio, 0, 0.1).
narrative_ontology:measurement(abst_tr_t5, abstraction_boundary_overrun, theater_ratio, 5, 0.25).
narrative_ontology:measurement(abst_tr_t10, abstraction_boundary_overrun, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(abst_be_t0, abstraction_boundary_overrun, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(abst_be_t5, abstraction_boundary_overrun, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(abst_be_t10, abstraction_boundary_overrun, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abstraction_boundary_overrun, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
