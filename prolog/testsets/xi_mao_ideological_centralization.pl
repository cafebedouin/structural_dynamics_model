% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: xi_mao_ideological_centralization
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Comparative Political Analysis / Historical Jurisprudence
% ============================================================================

:- module(constraint_xi_mao_centralization, []).

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
 * * constraint_id: xi_mao_ideological_centralization
 * human_readable: Ideological Centralization and the "Leadership Core"
 * domain: political
 * temporal_scope: 1949-1976 (Mao); 2012-Present (Xi)
 * spatial_scope: People's Republic of China
 * * SUMMARY:
 * This constraint analyzes the structural parallels between the governance of 
 * Mao Zedong and Xi Jinping, focusing on the centralization of power through 
 * ideological orthodoxy and the elimination of term limits. While Mao utilized 
 * mass mobilization and revolutionary chaos, Xi employs a high-tech, 
 * bureaucratic approach to achieve the same "Leadership Core" status.
 * * KEY AGENTS:
 * - The Supreme Leader: Institutional/Powerful; the architect of the 
 * ideological framework.
 * - The Party Cadre: Individual Powerless; subject to loyalty tests, 
 * criticism/self-criticism sessions, and anti-corruption purges.
 * - The Global Market: Individual Moderate; seeking predictability but 
 * constrained by sudden regulatory shifts for ideological purity.
 * * NARRATIVE ARC:
 * Post-Mao, China wove a Rope of collective leadership and term limits to 
 * prevent the return of a strongman. Under Xi, this Rope has been untied 
 * and replaced by a Mountain of personalistic rule. For the bureaucracy, 
 * the persistent anti-corruption campaigns act as a Noose, extracting absolute 
 * loyalty through the threat of professional and personal destruction.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(chinese_leadership_cycle, 0, 10).
narrative_ontology:constraint_claim(xi_mao_ideological_centralization, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.75. High extraction of political autonomy and informational 
% pluralism to maintain the "Leadership Core" legitimacy.
domain_priors:base_extractiveness(xi_mao_ideological_centralization, 0.75).

% Suppression score (0.0-1.0)
% Rationale: 0.85. Aggressive suppression of dissent, judicial independence, 
% and alternative historical narratives.
domain_priors:suppression_score(xi_mao_ideological_centralization, 0.85).

% Enforcement requirements
% Requires heavy active enforcement (Censorship apparatus, security 
% surveillance, and Party rectification campaigns).
domain_priors:requires_active_enforcement(xi_mao_ideological_centralization).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, extractiveness, 0.75).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, suppression_requirement, 0.85).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(xi_mao_ideological_centralization, ccp_monopoly_stability).
constraint_beneficiary(xi_mao_ideological_centralization, national_rejuvenation_narrative).
constraint_victim(xi_mao_ideological_centralization, institutional_succession_norms).
constraint_victim(xi_mao_ideological_centralization, political_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PARTY CADRE - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to "Loyalty Dances" or high-tech 
         equivalent (App usage, self-criticism).
   WHEN: immediate - The constant threat of being "swatted" as a "fly" or "tiger".
   WHERE: trapped - Bound by the disciplinary inspection system.
   SCOPE: local - Their specific administrative or corporate domain.
   
   WHY THIS CLASSIFICATION:
   For the official, the system is a Noose. It extracts continuous pledges of 
   loyalty and "orderly" behavior while choking off any attempt to build 
   independent patronage networks. Failure to align is met with coercive 
   "anti-corruption" removal.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    xi_mao_ideological_centralization,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(xi_mao_ideological_centralization),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SUPREME LEADER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to shape "Xi Jinping Thought" as a constitutional mandate.
   WHEN: biographical - Planning the "2049 Great Rejuvenation".
   WHERE: mobile - Projecting power via the "Belt and Road" and maritime expansion.
   SCOPE: national - Totalizing discourse over the state.
   
   WHY THIS CLASSIFICATION:
   From the center, ideology is a Rope. It is a functional coordination 
   mechanism to unify a vast, potentially fracturing nation under a single 
   vision. It "enriches" the Party's legitimacy by providing a new "Breakthrough 
   in Sinicization".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    xi_mao_ideological_centralization,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(xi_mao_ideological_centralization, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CITIZEN / SUBJECT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the social credit system and internet controls.
   WHEN: immediate - Daily exposure to mandatory political courses and propaganda.
   WHERE: trapped - Within the digital surveillance firewall.
   SCOPE: local - Domestic and social interactions.
   
   WHY THIS CLASSIFICATION:
   To the subject, the state's ideological presence is a Mountain. It is an 
   unchangeable fact of reality. One does not "negotiate" with the internet 
   filters or the Party's presence in schools; one simply adapts to the 
   terrain to survive.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    xi_mao_ideological_centralization,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(xi_mao_ideological_centralization, S),
    S > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INDIVIDUAL CITIZEN (SUBJECT) - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless - Subjects under a totalizing ideological mandate.
   WHEN: generational - Long-term reshaping of national and personal identity.
   WHERE: trapped - Geographically and digitally locked within the mandate.
   SCOPE: local - Impact on the individual's mental margin and social life.
   
   WHY THIS CLASSIFICATION:
   The "Noose" is the state of total ideological synchronization. While 
   the state uses centralization as a "Rope" for national unity, the 
   individual feels the tightening of a mandate that consumes their 
   cognitive and social margin. With a suppression score of 0.95 and 
   existential stakes for non-compliance, the citizen is "trapped" 
   within the system's "Correct Line" with zero degrees of freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    xi_mao_ideological_centralization,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(xi_mao_ideological_centralization),
    !.

% Explicit priors reflecting the totalizing extraction of the ideological noose.
domain_priors:base_extractiveness(xi_mao_ideological_centralization, 0.9).
domain_priors:suppression_score(xi_mao_ideological_centralization, 0.95).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(xi_mao_centralization_tests).

test(multi_perspective_conflict) :-
    % Leader (Rope) vs Cadre (Noose) vs Citizen (Mountain)
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, T1, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, T2, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, T3, context(individual_powerless, immediate, trapped, local)),
    T1 \= T2.

test(high_suppression_signature) :-
    % Totalizing ideologies require high suppression of pluralism.
    domain_priors:suppression_score(xi_mao_ideological_centralization, S),
    S > 0.8.

:- end_tests(xi_mao_centralization_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE VARIANTS: I distinguished Mao's "Chaos-driven" rule from 
 * Xi's "Order-driven" rule. Mao destroyed the bureaucracy to maintain 
 * power; Xi molds the bureaucracy as an unyielding Rope.
 * 2. FUTURE ARCS: The "Noose" classification for the elite suggests that 
 * while the system is stable in the immediate term, it creates "Brittle 
 * Stability". The lack of a successor (Rope loosening) creates a high 
 * probability of a terminal Mountain-clash (succession crisis) later.
 * 3. EXTRACTIVENESS: Set at 0.75 to reflect the mandatory time/attention 
 * extraction from citizens through digital platforms (e.g. Study the 
 * Great Nation app).
 */

omega_variable(
    succession_vacuum_risk,
    "Does the removal of term limits (Scaffold removal) ensure stability 
     (Rope) or guarantee a future systemic collapse (Noose)?",
    resolution_mechanism("Comparison of CCP internal power transfers post-Xi 
    vs the 1976 post-Mao transition"),
    impact("If peaceful: The Rope held. If chaotic: The concentration was a 
            Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Collective Leadership (Deng/Jiang/Hu Era)
 * Viability: Successfully maintained stability and high growth for decades.
 * Suppression: Actively dismantled by Xi to "re-legitimize" the Party 
 * against perceived corruption and Western influence.
 * * ALTERNATIVE 2: Constitutionalism / Rule of Law
 * Viability: Proposed by liberal factions; would bind the Party to the 
 * law rather than placing the Party above the law.
 * Suppression: Explicitly rejected in "Prong Three" which maintains the 
 * supremacy of the CCP.
 * * CONCLUSION:
 * The dismantling of a functional, stability-producing "Collective Rope" in 
 * favor of personalistic rule indicates that the current centralized 
 * structure is a Noose for the elite and a Mountain for the population.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [xi_mao_ideological_centralization].
% 2. Analyze: ?- multi_index_report(xi_mao_ideological_centralization).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
