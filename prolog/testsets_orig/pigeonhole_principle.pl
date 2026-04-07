% ============================================================================
% CONSTRAINT STORY: pigeonhole_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pigeonhole_principle, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pigeonhole_principle
 *   human_readable: The Pigeonhole Principle
 *   domain: mathematical_logic
 *
 * SUMMARY:
 *   The pigeonhole principle is a fundamental constraint in discrete
 *   mathematics: if n items are placed into m containers and n > m, at least
 *   one container must contain more than one item. This principle has no
 *   beneficiaries or victims — it is not an extractive or coordinative
 *   mechanism but a pure logical necessity. The constraint applies
 *   universally across all domains where cardinality relationships obtain:
 *   ballots and precincts (birthday paradox), hashes and collision tables,
 *   spectrum assignments and interference, committee assignments and
 *   conflicts, refugee allocation and shelter capacity. The principle cannot
 *   be violated, negotiated, exploited, or circumvented — only accommodated.
 *   It emerges naturally from the definitions of injection and cardinality.
 *   No measurement methodology, institutional arrangement, or observer
 *   perspective can change the underlying logical structure. This makes it
 *   the canonical example of a Mountain constraint.
 *
 * KEY AGENTS:
 *   - Any distributed entity: Structurally confined — agents must occupy slots and will collide if outnumber them
 *   - The mathematical structure itself: The constraint emerges from pure logical necessity, not from power relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pigeonhole_principle, 0.08).
domain_priors:suppression_score(pigeonhole_principle, 0.02).
domain_priors:theater_ratio(pigeonhole_principle, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pigeonhole_principle, extractiveness, 0.08).
narrative_ontology:constraint_metric(pigeonhole_principle, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pigeonhole_principle, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pigeonhole_principle, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(pigeonhole_principle, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pigeonhole_principle, mountain).
narrative_ontology:human_readable(pigeonhole_principle, "The Pigeonhole Principle").
narrative_ontology:topic_domain(pigeonhole_principle, "mathematical_logic").

domain_priors:emerges_naturally(pigeonhole_principle).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of any entity distributed across finite slots: the constraint is absolute. No number of agents can distribute across fewer slots without collision. Zero escape routes, zero alternatives. The mathematical necessity applies universally regardless of agent intent, power, or negotiating capacity.
constraint_indexing:constraint_classification(pigeonhole_principle, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the logical perspective: the pigeonhole principle is a direct consequence of cardinality and the definition of injective functions. No observables, measurement bases, or framings can change the mathematical structure. The constraint emerges from pure logical necessity — the definition of 'fewer slots than agents' automatically implies at least one collision. This is not contingent on physical law or institutional arrangement; it is contingent only on the coherence of set theory itself.
constraint_indexing:constraint_classification(pigeonhole_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even institutional actors with resources and power cannot exempt themselves from the principle. A system designer attempting to place 11 items in 10 bins cannot engineer their way out — they can only choose which item will share a bin or choose to add more bins. The constraint is not negotiable; only the response to the constraint is.
constraint_indexing:constraint_classification(pigeonhole_principle, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pigeonhole_principle_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(pigeonhole_principle, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pigeonhole_principle, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pigeonhole_principle, ExtMetricName, E),
    domain_priors:suppression_score(pigeonhole_principle, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(pigeonhole_principle),
    narrative_ontology:constraint_metric(pigeonhole_principle, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pigeonhole_principle, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(pigeonhole_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Nearly zero. The pigeonhole principle extracts nothing from anyone — it simply describes a necessary consequence of cardinality. The minimal value reflects that the principle itself has no extractive content; it does not benefit anyone systematically nor harm anyone asymmetrically. Theater ratio (0.15): Very low. No performative element — the principle is pure mathematical structure. Suppression (0.02): Negligible. There are no alternatives to suppress; the principle is not a constraint on agents' choices but a logical fact about the consequences of their distribution. Accessibility collapse (0.92): Near-total. Once you understand the definitions of 'fewer' and 'items in containers,' the principle becomes immediately obvious and inescapable. No gap between ideal and actual — the logical necessity is transparent. Resistance (0.05): Minimal. No agent or institution resists the principle because resistance is incoherent — you cannot choose not to have collisions if you have more items than containers.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on mountain. The distributed agent experiences absolute constraint (trapped, powerless). The analytical observer sees pure logical necessity (civilizational, universal). The institutional actor with resources still cannot escape the mathematical fact (arbitrage options matter nothing because the principle is not a scarcity constraint that resources can overcome). There is no perspectival divergence because the principle applies identically from all positions. This uniform classification is not a flaw in the framework — it is the correct diagnosis: some constraints are truly invariant across all observation sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is undefined for this constraint because there are no beneficiaries or victims. The pigeonhole principle is not distributive — it does not extract from some agents and benefit others. It simply describes a structural fact. All agents, regardless of power or exit options, face the same constraint. The absence of directionality is the signature of a pure mountain — it structures the space of possibilities but does not allocate extraction asymmetrically.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pigeonhole_principle, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pigeonhole_principle, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
