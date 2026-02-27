% ============================================================================
% CONSTRAINT STORY: medical_residency_match
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_residency_match, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: medical_residency_match
 *   human_readable: The NRMP Medical Residency Match
 *   domain: economic/social
 *
 * SUMMARY:
 *   The National Resident Matching Program (NRMP) Medical Residency Match is
 *   a centralized clearinghouse using a stable matching algorithm
 *   (Roth-Peranson) to pair medical students with residency programs. The
 *   Match aims to create a fair and efficient process for matching students
 *   and programs, resolving what would otherwise be a chaotic and potentially
 *   exploitative market.
 *
 * KEY AGENTS:
 *   - Medical Students: Beneficiaries (moderate/mobile) - gain a structured pathway to residency positions.
 *   - Residency Programs: Beneficiaries (institutional/arbitrage) - efficiently fill residency slots with qualified candidates.
 *   - Unmatched Medical Students: Potential victims (powerless/trapped) - those who do not match may face significant challenges.
 *   - NRMP: Administrator (institutional/constrained) - responsible for maintaining and operating the matching algorithm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_residency_match, 0.35).
domain_priors:suppression_score(medical_residency_match, 0.25).
domain_priors:theater_ratio(medical_residency_match, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_residency_match, extractiveness, 0.35).
narrative_ontology:constraint_metric(medical_residency_match, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(medical_residency_match, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_residency_match, tangled_rope).
narrative_ontology:human_readable(medical_residency_match, "The NRMP Medical Residency Match").
narrative_ontology:topic_domain(medical_residency_match, "economic/social").

domain_priors:requires_active_enforcement(medical_residency_match).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_residency_match, medical_students).
narrative_ontology:constraint_beneficiary(medical_residency_match, residency_programs).
narrative_ontology:constraint_victim(medical_residency_match, unmatched_medical_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Match provides a structured way for students to obtain residency positions, avoiding a chaotic free-for-all. Mobile exit because they can choose not to participate, but constrained in that participation greatly increases chances of placement.
constraint_indexing:constraint_classification(medical_residency_match, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The Match offers a centralized system for programs to fill residency slots with qualified candidates. Arbitrage in that the program can withdraw.
constraint_indexing:constraint_classification(medical_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Match represents a significant improvement over previous, less-organized systems, leading to a more efficient allocation of resources and talent.
constraint_indexing:constraint_classification(medical_residency_match, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% For unmatched medical students, they can be considered trapped in the matching process, powerless to improve the outcome. There is also extraction in that there is no program for them and they may not be able to continue in their planned career. 
constraint_indexing:constraint_classification(medical_residency_match, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_residency_match_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_residency_match, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(medical_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): The Match extracts a degree of autonomy from both students and programs, as they must abide by the algorithm's results. However, this is offset by the benefits of a stable and organized system.Suppression (0.25): Participation in the Match is not strictly mandatory, but it is strongly encouraged, creating a degree of suppression of alternative pathways. Theater Ratio (0.10): The Match is primarily a functional process, with minimal theatrical or performative elements.
 *
 * PERSPECTIVAL GAP:
 *   Students and programs generally perceive the Match as beneficial, but unmatched students may experience it as a snare. An analytical observer recognizes the Match as a positive development, but also acknowledges the potential for unintended consequences or areas for improvement.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical students and residency programs benefit directly from the match. Unmatched students bear the cost of the program, though there may be ways for them to find a path forward.
 *
 * MANDATROPHY ANALYSIS:
 *   The NRMP match solves the mandatrophy problem by being fundamentally a coordination mechanism instead of an extraction mechanism, and is only tangled rope from a position of weakness in the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_residency_match, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_residency_match, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
