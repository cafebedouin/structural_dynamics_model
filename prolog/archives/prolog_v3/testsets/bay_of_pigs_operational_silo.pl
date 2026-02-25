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

% --- Namespace Hooks (Updated for v3.4) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 * mutated into a Snare. By suppressing the JCS's pessimistic 
 * assessments and failing to inform the Brigade of the lack of US air 
 * cover, the silo ensured the mission's failure was inevitable.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Fixed ID to match constraint for drift detection
narrative_ontology:interval(bay_of_pigs_operational_silo, 0, 10).
narrative_ontology:constraint_claim(bay_of_pigs_operational_silo, snare).
narrative_ontology:human_readable(bay_of_pigs_operational_silo, "The CIA-Pentagon Intelligence/Action Silo (Operation Zapata)").
narrative_ontology:topic_domain(bay_of_pigs_operational_silo, "political/military").

% Base Properties
% Rationale: 0.90 extraction of life and political capital.
domain_priors:base_extractiveness(bay_of_pigs_operational_silo, 0.90).
domain_priors:suppression_score(bay_of_pigs_operational_silo, 0.80).
domain_priors:theater_ratio(bay_of_pigs_operational_silo, 0.72). % Performative "need to know" rituals.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, extractiveness, 0.9).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, theater_ratio, 0.72).

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(bay_of_pigs_operational_silo).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(bay_of_pigs_operational_silo, cia_operational_autonomy).
narrative_ontology:constraint_victim(bay_of_pigs_operational_silo, brigade_2506_exiles).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: BRIGADE 2506 MEMBER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Trained in secret, given a fixed objective.
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
        agent_power(powerless),
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
   PERSPECTIVE 3: THE TAYLOR COMMITTEE (POST-MORTEM) - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Commissioned to find why the failure occurred.
   WHEN: historical - Looking at the systemic collapse of 1961.
   WHERE: analytical - Not bound by the operational secrecy after the fact.
   SCOPE: global - Context of the Cold War and future US policy.
   
   WHY THIS CLASSIFICATION:
   The post-mortem analysis identifies the silo as a Snare. The lack of 
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
    snare,
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
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, T1, context(powerless, immediate, trapped, local)),
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
 * I focused on the Taylor Committee for the "Snare" perspective to provide 
 * the cleanest "Analytical" view required by the prompt.
 */

omega_variable(
    castro_internal_strength,
    "Was a popular uprising ever truly possible, or was the CIA logic a 
     metaphysical delusion?",
    resolution_mechanism("Analysis of 1961 Cuban internal security records and public sentiment data"),
    impact("If Possible: The silo was a failed Rope. If Delusion: The silo 
            was a total Snare from inception."),
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
 * the classification of the silo as a Snare.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [bay_of_pigs_operational_silo].
% 2. Multi-perspective report: ?- multi_index_report(bay_of_pigs_operational_silo).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional security (0.15) to the extreme 
% " compartmentalization theater" (0.72) that blinded President Kennedy.
narrative_ontology:measurement(zapata_tr_t0, bay_of_pigs_operational_silo, theater_ratio, 0, 0.15).
narrative_ontology:measurement(zapata_tr_t5, bay_of_pigs_operational_silo, theater_ratio, 5, 0.48).
narrative_ontology:measurement(zapata_tr_t10, bay_of_pigs_operational_silo, theater_ratio, 10, 0.72).

% Extraction: Tracking the intensification of the "Groupthink" Snare as the 
% plan's flaws were extracted from external review.
narrative_ontology:measurement(zapata_ex_t0, bay_of_pigs_operational_silo, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(zapata_ex_t5, bay_of_pigs_operational_silo, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(zapata_ex_t10, bay_of_pigs_operational_silo, base_extractiveness, 10, 0.90).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

