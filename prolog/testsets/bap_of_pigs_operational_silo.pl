% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: bay_of_pigs_operational_silo
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Bay of Pigs Invasion (Declassified CIA/National Security Archive)
% ============================================================================

:- module(constraint_bay_of_pigs, []).

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
 * * constraint_id: bay_of_pigs_operational_silo
 * human_readable: The CIA-Pentagon Intelligence/Action Silo (Operation Zapata)
 * domain: political/military
 * temporal_scope: 1960-1961
 * spatial_scope: Regional (US-Cuba-Guatemala)
 * * SUMMARY:
 * Operation Zapata was constrained by an extreme "need to know" operational 
 * silo that prevented internal skepticism from reaching decision-makers. 
 * This constraint manifested as "groupthink," where the CIA's plan relied on 
 * an internal logic that assumed a popular uprising in Cuba would 
 * spontaneously occur upon landing.
 * * KEY AGENTS:
 * - Brigade 2506 Exile: Powerless agent trapped in a plan they didn't author.
 * - CIA Project Directors (Bissell/Cabanis): Institutional architects of the silo.
 * - President Kennedy: Powerful agent blinded by the suppression of alternatives.
 * * NARRATIVE ARC:
 * The operational secrecy began as a Rope (coordination for surprise) but 
 * mutated into a Noose. By suppressing the JCS's pessimistic 
 * assessments and failing to inform the Brigade of the lack of US air 
 * cover, the silo ensured the mission's failure was inevitable.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(operation_zapata_timeline, 0, 10).
narrative_ontology:constraint_claim(bay_of_pigs_operational_silo, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.9. High extraction of life and political capital from the 
% Brigade and the Kennedy administration for the sake of CIA agency prestige.
domain_priors:base_extractiveness(bay_of_pigs_operational_silo, 0.9).

% Suppression score (0.0-1.0)
% Rationale: 0.8. Active suppression of dissent and alternative assessments 
% from the State Department and lower-level CIA analysts.
domain_priors:suppression_score(bay_of_pigs_operational_silo, 0.8).

% Enforcement requirements
% Requires active enforcement (extreme classification and compartmentalization).
domain_priors:requires_active_enforcement(bay_of_pigs_operational_silo).

% Metrics required for Executive Summary
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, extractiveness, 0.9).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bay_of_pigs_operational_silo, cia_operational_autonomy).
constraint_victim(bay_of_pigs_operational_silo, brigade_2506_exiles).
constraint_victim(bay_of_pigs_operational_silo, kennedy_administration_reputation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: BRIGADE 2506 MEMBER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Trained in secret, given a fixed objective.
   WHEN: immediate - The 72 hours of the landing.
   WHERE: trapped - Landed on a beach with no retreat path (swamps).
   SCOPE: local - Playa Girón/Playa Larga.
   
   WHY THIS CLASSIFICATION:
   For the exiles, the CIA's plan was a Mountain. They were told US air 
   support was a certainty and that the Cuban people would join them. 
   When these turned out to be false, the exiles had no power to change the 
   landscape they were dying in.
   
   NARRATIVE EVIDENCE:
   "The Brigade was never told that the President had specifically forbidden 
   the use of US combat forces".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bay_of_pigs_operational_silo,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(bay_of_pigs_operational_silo),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CIA LEADERSHIP - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power over the flow of information.
   WHEN: biographical - Seeking a definitive win in the Cold War.
   WHERE: mobile - Had the ability to abort or modify but chose not to.
   SCOPE: regional - Caribbean and Central America.
   
   WHY THIS CLASSIFICATION:
   To Bissell and Dulles, the compartmentalization was a Rope—a necessary 
   functional coordination tool to prevent leaks that would jeopardize 
   deniability. They viewed the constraints they built as helpful 
   protections for the mission's success.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bay_of_pigs_operational_silo,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(bay_of_pigs_operational_silo, E),
    E > 0.5, % High E is invisible to them
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TAYLOR COMMITTEE (POST-MORTEM) - Noose
   --------------------------------------------------------------------------
   
   WHO: analytical - Commissioned to find why the failure occurred.
   WHEN: historical - Looking at the systemic collapse of 1961.
   WHERE: analytical - Not bound by the operational secrecy after the fact.
   SCOPE: global - Context of the Cold War and future US policy.
   
   WHY THIS CLASSIFICATION:
   The post-mortem analysis identifies the silo as a Noose. The lack of 
   external review meant that the plan's flaws (e.g., the "escaping to the 
   mountains" fallback being 80 miles away) were never corrected, 
   choking the mission to death.
   
   NARRATIVE EVIDENCE:
   The Taylor Report noted that "the planners gave the President the impression 
   the operation would trigger a spontaneous revolt," which was factually 
   unsupported.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bay_of_pigs_operational_silo,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(bay_of_pigs_operational_silo, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(bay_of_pigs_tests).

test(multi_perspective_conflict) :-
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, T2, context(institutional, biographical, mobile, regional)),
    T1 \= T2.

test(high_extraction_detection) :-
    domain_priors:base_extractiveness(bay_of_pigs_operational_silo, E),
    E > 0.8.

:- end_tests(bay_of_pigs_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.9): I rated this extremely high because the "cost" 
 * was almost entirely borne by the exiles and the President, while the 
 * agency maintained its internal culture until the total collapse.
 * * 2. PERSPECTIVE: Kennedy's perspective is complex; he acts as both a 
 * powerful agent and a victim of the analytical suppression. 
 * I focused on the Taylor Committee for the "Noose" perspective to provide 
 * the cleanest "Analytical" view required by the prompt.
 */

omega_variable(
    castro_internal_strength,
    "Was a popular uprising ever truly possible, or was the CIA logic a 
     metaphysical delusion?",
    resolution_mechanism("Analysis of 1961 Cuban internal security records and public sentiment data"),
    impact("If Possible: The silo was a failed Rope. If Delusion: The silo 
            was a total Noose from inception."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Joint CIA-Pentagon Planning
 * Viability: Using the JCS as a peer reviewer would have exposed the 
 * logistical impossibilities of the Trinidad/Zapata plan.
 * Suppression: Suppressed by CIA leadership to maintain control over the 
 * budget and operational direction.
 * * CONCLUSION:
 * The active suppression of the Joint Chiefs of Staff's skepticism confirms 
 * the classification of the silo as a Noose.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [bay_of_pigs_operational_silo].
% 2. Multi-perspective report: ?- multi_index_report(bay_of_pigs_operational_silo).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(bap_of_pigs_operational_silo, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(bap_of_pigs_operational_silo, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(bap_of_pigs_operational_silo, noose, agent_power(individual_powerless)).
