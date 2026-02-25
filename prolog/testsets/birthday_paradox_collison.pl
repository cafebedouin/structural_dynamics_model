% ============================================================================
% CONSTRAINT STORY: birthday_paradox_collison
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birthday_paradox_collison, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: birthday_paradox_collison
 *   human_readable: The Birthday Paradox (Collision Probability)
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Birthday Paradox is a mathematical theorem stating that in a set of
 *   randomly chosen items, the probability of a 'collision' (two items
 *   sharing a property, like a birthday) grows much faster than intuition
 *   suggests. For a set of 365 possibilities, a 50% collision probability is
 *   reached with only 23 items. This is not a paradox in the logical sense,
 *   but a verifiable mathematical truth that is counter-intuitive. As a
 *   fundamental principle of probability, it is an unchangeable feature of
 *   any system involving random sampling from a finite set.
 *
 * KEY AGENTS:
 *   - Probability Students: Individuals (powerless/trapped) who must learn and accept the principle as a given fact.
 *   - Cryptographic Engineers: Organized professionals (organized/constrained) who must design systems (e.g., hash functions) that are robust against attacks exploiting this principle.
 *   - System Designers: Any actor creating systems with unique identifiers (e.g., hash tables) who must account for collision probability as a natural law of their domain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birthday_paradox_collison, 0.02).
domain_priors:suppression_score(birthday_paradox_collison, 0.01).
domain_priors:theater_ratio(birthday_paradox_collison, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birthday_paradox_collison, extractiveness, 0.02).
narrative_ontology:constraint_metric(birthday_paradox_collison, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(birthday_paradox_collison, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(birthday_paradox_collison, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(birthday_paradox_collison, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birthday_paradox_collison, mountain).
narrative_ontology:human_readable(birthday_paradox_collison, "The Birthday Paradox (Collision Probability)").
narrative_ontology:topic_domain(birthday_paradox_collison, "mathematical/technological").

domain_priors:emerges_naturally(birthday_paradox_collison).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PROBABILITY STUDENT (MOUNTAIN) — An individual encountering the principle for the first time. It is a counter-intuitive but immutable fact of mathematics that must be learned and accepted. There is no exit from its logical consequences.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CRYPTOGRAPHIC ENGINEER (MOUNTAIN) — A professional whose work is constrained by this principle. They must design hash functions and protocols to account for the probability of collisions. The paradox is a fundamental design constraint, a law of their operational environment.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a detached, civilizational viewpoint, the birthday paradox is a timeless, non-negotiable feature of probability theory. Its classification is invariant across all observers and contexts.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birthday_paradox_collison_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(birthday_paradox_collison, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birthday_paradox_collison, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(birthday_paradox_collison, ExtMetricName, E),
    domain_priors:suppression_score(birthday_paradox_collison, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(birthday_paradox_collison),
    narrative_ontology:constraint_metric(birthday_paradox_collison, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(birthday_paradox_collison, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(birthday_paradox_collison_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a canonical example of a Mountain. Extractiveness (ε=0.02) and Suppression (0.01) are near zero, as a mathematical theorem does not extract value or suppress alternatives; it merely describes reality. The NL Profile metrics confirm this: it 'emerges_naturally' from axioms, has extremely high 'accessibility_collapse' (0.98) as its proof is logically undeniable once understood, and extremely low 'resistance' (0.02) as it cannot be argued against. The theater ratio is zero, as it is pure mathematical function.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a defining characteristic of a true Mountain constraint. Whether viewed by a student, a professional engineer, or a detached analyst, the constraint's nature is invariant. It is a fixed, universal feature of mathematics, and all observers converge on the same classification. Its truth is not dependent on power, time horizon, or exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   As a pure Mountain constraint representing a mathematical law, there are no structural beneficiaries or victims. The principle is neutral and symmetric. Any benefit or harm comes from its application in other, separate constraints (e.g., a 'birthday attack' on a cryptographic system), not from the principle itself. Therefore, the beneficiary and victim lists are empty, and directionality is not a relevant factor.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the Birthday Paradox as a Mountain correctly identifies it as a non-negotiable, non-anthropogenic feature of reality. This prevents the mandatrophy of misinterpreting a mathematical law as a social convention or a system that could be altered through policy or negotiation. It establishes a hard floor of reality upon which other, more malleable constraints (like cryptographic standards) are built.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birthday_paradox_collison, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(birthday_paradox_collison, birthday_attack_vulnerability).

% DUAL FORMULATION NOTE:
% The Birthday Paradox itself is a Mountain (a mathematical law). The vulnerability it creates in specific technological systems, known as a 'birthday attack', is a separate, downstream constraint. That constraint (birthday_attack_vulnerability) would likely be classified as a Tangled Rope or Snare, as it involves active exploitation and has clear victims (users of the compromised system) and beneficiaries (the attackers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
