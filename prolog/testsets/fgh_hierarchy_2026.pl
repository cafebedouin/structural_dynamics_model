% ============================================================================
% CONSTRAINT STORY: fgh_hierarchy_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_fgh_hierarchy, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: fgh_hierarchy_2026
 * human_readable: The Fast-Growing Hierarchy Mountain
 * domain: mathematical/computational
 * * SUMMARY:
 * The Fast-Growing Hierarchy (FGH) is a family of functions indexed by 
 * transfinite ordinals that classifies the growth rates of computable 
 * functions. It is a Mountain—a logical limit where the "extraction" is the 
 * baseline cost of algorithmic reality, and the suppression of 
 * sub-exponential or even Ackermann-level growth is absolute at higher stages.
 * * KEY AGENTS:
 * - Computations: Subject (Powerless) - Bound by the recursive limits of ordinals.
 * - Formal Logic: Beneficiary (Institutional) - Providing the scaffolding for 
 * computational complexity.
 * - Complexity Theorists: Auditor (Analytical) - Mapping the transfinite landscape.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Mountains are defined by near-zero extraction and maximum suppression.
domain_priors:base_extractiveness(fgh_hierarchy_2026, 0.01). 
domain_priors:suppression_score(fgh_hierarchy_2026, 0.99).   
domain_priors:theater_ratio(fgh_hierarchy_2026, 0.10). % Low theater; pure abstract logic.

% Primary keys for classification engine
narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, 0.01).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, theater_ratio, 0.10).

% Identification of coordination beneficiary (Logical Integrity)
narrative_ontology:constraint_beneficiary(fgh_hierarchy_2026, computational_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMPUTATIONAL PROCESS (MOUNTAIN)
% Effective Extraction: 0.01 * 1.5 (powerless) * 1.2 (universal logic) = 0.018.
% Since χ < 0.05, the "agent" (the algorithm) perceives this as a natural law.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTIONAL FRAMEWORK (MOUNTAIN)
% Effective Extraction: 0.01 * -0.2 (institutional) * 1.2 = -0.0024.
% The logic serves as the foundational "floor" for building complexity classes.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fgh_hierarchy_2026_tests).

test(mountain_validation) :-
    % Mountains must satisfy the low-extraction threshold χ < 0.05.
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, E),
    E < 0.05.

test(absolute_suppression_check) :-
    % Logical/Mathematical laws exclude alternative growth rates with near-total score.
    domain_priors:suppression_score(fgh_hierarchy_2026, S),
    S > 0.95.

:- end_tests(fgh_hierarchy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Fast-Growing Hierarchy is the "Inertial Limit" of recursion. In the 
 * Deferential Realism ontology, it is a Mountain because its growth rates—
 * from the successor function to the Wainer hierarchy—are not subject to 
 * institutional negotiation or biographical agency. The "trapped" status 
 * reflects the fact that any computational agent is ultimately bound by 
 * the Church-Turing thesis and the ordinals available for recursion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_large_ordinal_limit,
    'Does the physical universe support computation beyond the Bachmann-Howard ordinal?',
    'Review of quantum-gravitational information density limits.',
    'Physical limits convert the logical Mountain into a physical Piton.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fgh_hierarchy_2026, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Logical Mountains are static; their metrics do not drift.
narrative_ontology:measurement(fgh_tr_t0, fgh_hierarchy_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fgh_ex_t0, fgh_hierarchy_2026, base_extractiveness, 0, 0.01).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
