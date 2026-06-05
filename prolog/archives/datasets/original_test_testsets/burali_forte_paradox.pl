% ============================================================================
% CONSTRAINT STORY: burali_forte_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burali_forte_paradox, []).

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
 *   constraint_id: burali_forte_paradox
 *   human_readable: Burali-Forti Paradox
 *   domain: technological/mathematics
 *
 * SUMMARY:
 *   The Burali-Forti paradox, discovered in 1897, demonstrates a fundamental
 *   contradiction in naive set theory arising from the attempt to form a 'set
 *   of all ordinal numbers'. If such a set Ω existed, it would be
 *   well-ordered and thus have an ordinal number, say ω. This implies ω is an
 *   element of Ω. However, by the properties of ordinals, every element of Ω
 *   must be smaller than ω, leading to the contradiction ω < ω. This paradox,
 *   along with others like Russell's, forced the abandonment of naive set
 *   theory and spurred the development of axiomatic systems like ZFC, which
 *   avoid the paradox by distinguishing between 'sets' and 'proper classes'.
 *   The collection of all ordinals is a proper class, not a set, and thus has
 *   no ordinal number.
 *
 * KEY AGENTS:
 *   - Naive Set Theorists (e.g., early Cantor, Frege): Primary agents whose framework was invalidated by the constraint (powerless/trapped).
 *   - Axiomatic Set Theorists (e.g., Zermelo, Russell, von Neumann): Agents who reformulated the foundations of mathematics to accommodate the constraint (institutional/arbitrage).
 *   - Mathematical Consistency: The abstract beneficiary. The paradox enforces consistency by invalidating contradictory systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burali_forte_paradox, 0.01).
domain_priors:suppression_score(burali_forte_paradox, 0.02).
domain_priors:theater_ratio(burali_forte_paradox, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burali_forte_paradox, extractiveness, 0.01).
narrative_ontology:constraint_metric(burali_forte_paradox, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(burali_forte_paradox, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(burali_forte_paradox, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burali_forte_paradox, mountain).
narrative_ontology:human_readable(burali_forte_paradox, "Burali-Forti Paradox").
narrative_ontology:topic_domain(burali_forte_paradox, "technological/mathematics").

domain_priors:emerges_naturally(burali_forte_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NAIVE SET THEORIST (MOUNTAIN) — From the perspective of a mathematician operating under naive set theory, the paradox is an immovable, incomprehensible contradiction. They are trapped within an inconsistent system, and the paradox appears as a hard, unchangeable barrier to their work. There is no exit from the logical consequence itself.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: AXIOMATIC SET THEORIST (MOUNTAIN) — For the architects of modern set theory, the paradox is not a contradiction but a theorem. It's a fixed feature of the mathematical landscape that proves the collection of all ordinals is a 'proper class,' not a set. Their 'exit' is the arbitrage of choosing new axioms (like ZFC) to build a consistent system around this fixed point.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: MODERN STUDENT (MOUNTAIN) — A student learning logic has no power to alter this foundational result. They are constrained to accept its validity to understand the subject. It is presented as an unchangeable law of the formal system they are studying.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The canonical analytical view confirms the paradox as a fundamental, unchangeable logical limit. Its structure has zero degrees of freedom. The extremely low extraction and suppression scores, combined with the natural law profile, make this a quintessential Mountain.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burali_forte_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(burali_forte_paradox, ExtMetricName, E),
    domain_priors:suppression_score(burali_forte_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(burali_forte_paradox),
    narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(burali_forte_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(burali_forte_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a classic Mountain, representing a logical/mathematical limit. Extractiveness (ε=0.01) is near zero; the paradox doesn't extract value, it reveals a structural truth. Suppression (0.02) is also near zero; it doesn't suppress human alternatives through coercion, it logically invalidates inconsistent mathematical systems. Theater (0.0) is zero as it is a purely functional, non-performative result. The Natural Law profile is met: it emerges naturally from definitions (true), has extremely high accessibility collapse (0.98) once the premises are understood, and near-zero resistance (0.01) as it is a proven theorem.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a key signature of a Mountain constraint. All rational perspectives, from the naive theorist who hits it as a wall to the axiomatic theorist who builds around it, ultimately classify it as an unchangeable, fixed feature of reality. The invariance of the 'Mountain' classification across all indices demonstrates its status as a natural law of logic.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint with no declared beneficiaries or victims, directionality is undefined and irrelevant. The constraint is symmetric and applies universally to any agent attempting to construct a set of all ordinals. The d-value for all observers is effectively neutral, leading to a near-zero effective extraction (χ) for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint serves as a baseline case that *prevents* mandatrophy. It is an unambiguous example of a non-social, non-extractive, unchangeable limit. Its clear Mountain classification provides a grounding point for the entire system, demonstrating what a true 'natural law' looks like. Any attempt to classify this as a Snare or Tangled Rope would be a fundamental misreading of its structure, highlighting the importance of the ε-invariance principle and the Natural Law profile metrics for correctly identifying non-contingent constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burali_forte_paradox, 1897, 1908).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(burali_forte_paradox, russell_paradox).
narrative_ontology:affects_constraint(burali_forte_paradox, cantor_paradox).
narrative_ontology:affects_constraint(burali_forte_paradox, zfc_axiomatization).

% DUAL FORMULATION NOTE:
% The Burali-Forti, Russell, and Cantor paradoxes form a family of constraints that collectively invalidated naive set theory, leading directly to the development of axiomatic set theory (e.g., zfc_axiomatization) as a resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
