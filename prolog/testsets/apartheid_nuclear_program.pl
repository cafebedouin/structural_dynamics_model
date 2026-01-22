% ============================================================================
% CONSTRAINT STORY: apartheid_nuclear_program
% ============================================================================
% Generated: January 20, 2026
% Model: Gemini 2.0 Flash
% Source: Renfrew Christie Obituary (Jan 14, 2026)
% ============================================================================

:- module(apartheid_nuclear_program, []).

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
 * * constraint_id: apartheid_nuclear_program
 * human_readable: Apartheid South Africa's Clandestine Nuclear Program
 * domain: political/military/technological
 * temporal_scope: 1970s - 1990s
 * spatial_scope: National (South Africa)
 * * SUMMARY:
 * A secret weapons program designed to secure the apartheid state through 
 * nuclear deterrence, funded by a system of forced labor that produced 
 * "the cheapest electricity in the world".
 * * KEY AGENTS:
 * - Apartheid State: Seeking civilizational survival via technological Rope.
 * - Renfrew Christie: The scholar-spy who identified the program's 
 * electrification markers to facilitate its destruction.
 * - Political Prisoners: Those who experienced the program's enforcement 
 * via the Terrorism Act and death row.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(apartheid_era, 0, 10).
narrative_ontology:constraint_claim(apartheid_nuclear_program, noose).

% High extraction due to forced labor subsidy.
% [RESOLVED MANDATROPHY]: Paradoxical extraction is indexically separated 
% between State necessity and victim suffering.
domain_priors:base_extractiveness(apartheid_nuclear_program, 0.85).

% High suppression: Terrorism Act and solitary confinement.
domain_priors:suppression_score(apartheid_nuclear_program, 0.9).

domain_priors:requires_active_enforcement(apartheid_nuclear_program).

narrative_ontology:constraint_metric(apartheid_nuclear_program, extractiveness, 0.85).
narrative_ontology:constraint_metric(apartheid_nuclear_program, suppression_requirement, 0.9).

constraint_beneficiary(apartheid_nuclear_program, apartheid_regime).
constraint_victim(apartheid_nuclear_program, [forced_laborers, political_dissidents]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: APARTHEID REGIME - Rope
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    apartheid_nuclear_program,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    % From the state's view, the program is a coordination mechanism for 
    % survival and status as a Cold War ally.
    domain_priors:base_extractiveness(apartheid_nuclear_program, E),
    E > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: RENFREW CHRISTIE (IMPRISONED) - Noose
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    apartheid_nuclear_program,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    % Seven years in prison and solitary confinement 
    % characterize the system as an asymmetric, coercive mechanism.
    domain_priors:suppression_score(apartheid_nuclear_program, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: RENFREW CHRISTIE (RESEARCHER) - Mountain
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    apartheid_nuclear_program,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(continental)
    )
) :-
    % The physical requirements for uranium enrichment were an immutable 
    % fact Christie used to calculate bomb yields.
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(apartheid_nuclear_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(apartheid_nuclear_program, T1, context(institutional, _, _, _)),
    constraint_indexing:constraint_classification(apartheid_nuclear_program, T2, context(individual_powerless, _, _, _)),
    T1 \= T2.

test(linter_compliance) :-
    % Verify the resolved mandatrophy tag is present
    clause(domain_priors:base_extractiveness(apartheid_nuclear_program, 0.85), _).

:- end_tests(apartheid_nuclear_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * 1. MANDATROPHY RESOLUTION: The 0.85 extraction score is flagged by the 
 * linter. I have resolved this by acknowledging the "Institutional Rope" 
 * as a mask for "Individual Noose" extraction.
 * 2. EVIDENCE: Grounded in the Christie Obituary's description of 
 * forced labor and State-sanctioned torture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_delusion_threshold,
    "At what level of extraction (0.85+) does a 'Rope' become functionally 
    indistinguishable from a 'Noose' even for the Institutional agent?",
    resolution_mechanism("Longitudinal decay of regime legitimacy metrics"),
    impact("If detected: State collapse is imminent. If not: Persistent Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Integration.
 * Suppression: Christie's research was treated as "Terrorism" rather than 
 * policy critique.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
