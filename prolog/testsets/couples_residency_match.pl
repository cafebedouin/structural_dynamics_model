% ============================================================================
% CONSTRAINT STORY: couples_residency_match
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: NRMP / Roth-Peranson Algorithm (Couples Module)
% ============================================================================

:- module(constraint_couples_match, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: couples_residency_match
 * human_readable: The Couples Match (Joint Stability)
 * domain: technological/social
 * temporal_scope: Biographical (Final Year of Residency)
 * spatial_scope: National (USA)
 * * SUMMARY:
 * The "Couples Match" is a specialized module of the NRMP that allows two 
 * medical students to link their rank order lists (ROLs). To maintain 
 * stability, the algorithm must find a pair of programs that is stable for 
 * BOTH partners simultaneously. This significantly increases the 
 * computational complexity and restricts the available "solution space."
 * * KEY AGENTS:
 * - The Couple: Two agents acting as a single unit with linked preferences.
 * - The Residencies: Individual programs with fixed quotas.
 * - The Algorithm: The Roth-Peranson modification for coupled stability.
 * * NARRATIVE ARC:
 * The constraint functions as a "Gravity Well." By linking their fates, 
 * the couple creates a higher barrier to entry. They must often sacrifice 
 * individual prestige (moving down their personal ROL) to find a joint 
 * geographic match, turning a coordination tool into a self-imposed limit.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(couples_residency_match, 0, 10).
narrative_ontology:constraint_claim(couples_residency_match, mountain).

% Base extractiveness score (0.5)
% The system extracts a "choice penalty." To guarantee being together, 
% couples often match at lower-ranked programs than they would individually.
domain_priors:base_extractiveness(couples_residency_match, 0.5).

% Suppression score (0.95)
% Like the standard match, side-deals are strictly prohibited.
domain_priors:suppression_score(couples_residency_match, 0.95).

% Enforcement requirements
domain_priors:requires_active_enforcement(couples_residency_match).

% Metrics
narrative_ontology:constraint_metric(couples_residency_match, extractiveness, 0.5).
narrative_ontology:constraint_metric(couples_residency_match, suppression_requirement, 0.95).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(couples_residency_match, regional_hospital_systems).
constraint_victim(couples_residency_match, high_achieving_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE "COUPLE" - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (subject to joint probability)
   WHEN: biographical (marriage/family formation)
   WHERE: trapped (linked lists create a binary "match together or fail")
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The couple experiences the constraint as a "Snare" because the number 
   of valid stable matches drops exponentially when ROLs are linked. 
   The binding nature of the contract means they might be forced into 
   their 50th-ranked pair of programs simply because it's the only 
   one that works for both.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    couples_residency_match,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(couples_residency_match, S),
    S > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROGRAM DIRECTOR - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: generational
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   Program directors see the Couples Match as a "Rope." It helps them 
   recruit high-quality candidates who might otherwise go to a 
   different city. By offering a joint solution, the Match 
   coordinates the social needs of residents with the labor needs 
   of the hospital.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    couples_residency_match,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: civilizational
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the analyst, the Couples Match is a "Mountain." Adding couples 
   to the Stable Marriage Problem makes finding a stable match 
   potentially impossible (non-existence of stability). This is a 
   fundamental NP-hard frontier of game theory.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    couples_residency_match,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(couples_residency_match),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(couples_match_tests).

test(complexity_noose) :-
    % Linked lists (trapped) = Snare
    constraint_indexing:constraint_classification(couples_residency_match, snare, context(_, _, trapped, _)).

test(analytical_mountain) :-
    % The mathematical impossibility of certain stable matches is a Mountain.
    constraint_indexing:constraint_classification(couples_residency_match, mountain, context(analytical, _, _, _)).

:- end_tests(couples_match_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * The Couples Match is a "voluntary snare." Agents choose to link their 
 * lists to solve a social coordination problem (staying together), but 
 * the math of the Match turns this into a much more restrictive constraint.
 * * OMEGAS:
 * 1. Does a stable match ALWAYS exist with couples? 
 * (Historically no, but the algorithm finds 'approximate' stability).
 */

omega_variable(
    stability_existence,
    "Is a stable match mathematically guaranteed for all couples?",
    resolution_mechanism("Algorithmic proof of Roth-Peranson convergence on specific datasets"),
    impact("If No: The system is a 'Scaffold' that could collapse at any time."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
