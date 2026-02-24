% ============================================================================
% CONSTRAINT STORY: us_two_party_duopoly
% Status: [RESOLVED MANDATROPHY]
% ============================================================================
% Generated: 2026-01-22
% Model: Gemini 2.0 Flash
% Source: Pew Research (2026), CFR Task Force Report 83 (2025)
% ============================================================================

:- module(constraint_us_two_party_duopoly, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for 2026 DR-Audit Suite) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: us_two_party_duopoly
 * human_readable: The U.S. Two-Party Duopoly
 * domain: political
 * temporal_scope: 2024-2026 (Strategic landscape)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * The U.S. electoral landscape is structurally locked into a two-party system 
 * via "winner-take-all" mechanics. While it coordinates stable governance (Rope), 
 * it extracts representation from the 37% of Americans who desire more options, 
 * creating a structural "Snare" for alternative movements.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(us_two_party_duopoly, 0, 10).
narrative_ontology:constraint_claim(us_two_party_duopoly, tangled_rope).

% Base extractiveness (0.75): Captures nearly 100% of seats despite 44% youth 
% dissatisfaction with the binary choice.
domain_priors:base_extractiveness(us_two_party_duopoly, 0.75).

% Suppression (0.80): High ballot access barriers and the 15% debate threshold 
% actively hide alternatives.
domain_priors:suppression_score(us_two_party_duopoly, 0.80).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_two_party_duopoly, suppression_requirement, 0.8).

domain_priors:requires_active_enforcement(us_two_party_duopoly).

% Mandatory Asymmetry Hooks
constraint_beneficiary(us_two_party_duopoly, [democratic_party, republican_party]).
constraint_victim(us_two_party_duopoly, [independent_voters, young_voters, third_party_candidates]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISILLUSIONED VOTER - Snare
   WHO: powerless | EXIT: trapped | SCOPE: national
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(us_two_party_duopoly, snare, 
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))) :-
    domain_priors:base_extractiveness(us_two_party_duopoly, E), E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: PARTY STRATEGIST - Rope
   WHO: institutional | EXIT: mobile | SCOPE: national
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(us_two_party_duopoly, rope, 
    context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(national))) :-
    domain_priors:requires_active_enforcement(us_two_party_duopoly),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Tangled Rope
   WHO: analytical | EXIT: analytical | SCOPE: global
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :-
    domain_priors:base_extractiveness(us_two_party_duopoly, E), E > 0.4,
    domain_priors:suppression_score(us_two_party_duopoly, S), S > 0.5,

    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(duopoly_tests).

test(multi_perspective_variance) :-
    % Voter (Snare) vs Strategist (Rope) vs Analyst (Tangled Rope)
    constraint_indexing:constraint_classification(us_two_party_duopoly, T1, context(powerless, immediate, trapped, national)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, T2, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3, T1 \= T3.

:- end_tests(duopoly_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 1. TANGLED ROPE RATIONALE: The duopoly is a textbook hybrid. It coordinates 
 * large-scale legislative action (Rope) but extracts political representation 
 * by penalizing third-party "spoilers" (Snare).
 * 2. MANDATROPHY RESOLUTION: The status is [RESOLVED MANDATROPHY] because high 
 * extractiveness (0.75) is only functional when indexed to institutional 
 * stability; for the voter, it remains purely coercive.
 * 3. OMEGA:
 * omega_variable(rcv_adoption,
 * "Will Ranked Choice Voting (RCV) untangle the duopoly extraction?",
 * resolution_mechanism("Monitor 2026/2028 state-level ballot initiatives"),
 * impact("If yes: Rope. If no: Snare."),
 * confidence_without_resolution(medium)).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
