% ============================================================================
% CONSTRAINT STORY: hiv_prep_prevention_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "We can block the spread of HIV" by Carissa Wong (Jan 2026)
% ============================================================================

:- module(hiv_prep_prevention_2026, []).

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
 * * constraint_id: hiv_prep_prevention_2026
 * human_readable: PrEP-Mediated HIV Prevention
 * domain: technological/social/biological
 * temporal_scope: 2010-2026
 * spatial_scope: Global (over 150 countries)
 * * SUMMARY:
 * Pre-exposure prophylaxis (PrEP) is a daily antiviral pill that drastically reduces 
 * the risk of HIV transmission by more than 90 per cent when used correctly. 
 * It has transformed HIV from an inescapable threat associated with fear and stigma 
 * into a manageable health constraint, enhancing sexual freedom for millions.
 * * KEY AGENTS:
 * - The PrEP User: Individuals, particularly in underserved communities, who use 
 * the drug to regain sexual well-being and freedom.
 * - The Healthcare Institution (WHO/FDA): Bodies that approve and recommend 
 * PrEP to slash transmission rates globally.
 * - The Underserved Subject: Communities (e.g., men who have sex with men) who 
 * face healthcare barriers and historical stigma.
 * * NARRATIVE ARC:
 * From the early epidemic fears to the 2010 landmark trial, the advent of PrEP 
 * shifted the constraint of HIV transmission from an "act of god" or "condom 
 * failure" risk to a programmable biological barrier. By 2023, 
 * 3.5 million people were utilizing this "enormous revelation" to exchange 
 * bodily fluids safely.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(hiv_prep_prevention_2026, 0, 10).
narrative_ontology:constraint_claim([hiv_prep_prevention_2026], [biotechnological_intervention]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: PrEP is highly generative of freedom; however, the cost of 
% daily medication and the reliance on pharmaceutical supply chains 
% introduces a minor extractive element.
domain_priors:base_extractiveness(hiv_prep_prevention_2026, 0.15).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: PrEP suppresses the transmission capacity of the virus itself 
% and suppresses the fear/stigma associated with risk.
domain_priors:suppression_score(hiv_prep_prevention_2026, 0.5).

% Enforcement requirements
% Requires active enforcement (Daily pill adherence and medical monitoring).
domain_priors:requires_active_enforcement(hiv_prep_prevention_2026).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, extractiveness, 0.15).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hiv_prep_prevention_2026, global_sexual_freedom). %
constraint_beneficiary(hiv_prep_prevention_2026, healthcare_stability). %
constraint_victim(hiv_prep_prevention_2026, hiv_pathogen_spread). %

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PREP USER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to biological risk and stigma)
   WHEN: biographical (Focus on life-long sexual well-being)
   WHERE: trapped (Within an epidemic that has claimed 44 million lives)
   SCOPE: local (Personal intimate contact)
   
   WHY THIS CLASSIFICATION:
   For the user, PrEP is a "Rope"—a tool for coordination that allows them to 
   engage in the "kind of sex they wanted" while safely exchanging bodily 
   fluids. It replaces the "Snare" of fear with a functional pill.
   
   NARRATIVE EVIDENCE:
   "Allowed them to have the kind of sex they wanted to have, where bodily fluids 
   could be exchanged".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    hiv_prep_prevention_2026,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE WHO/FDA REGULATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making/Recommending bodies)
   WHEN: historical (Managing the global HIV response since 2010)
   WHERE: arbitrage (Managing distribution across 150+ countries)
   SCOPE: global (Worldwide health policy)
   
   WHY THIS CLASSIFICATION:
   For regulators, PrEP is a "Rope"—a functional policy tool used to slash 
   transmission rates and stabilize global health.
   
   NARRATIVE EVIDENCE:
   "The World Health Organization recommended PrEP in 2015 and... more than 150 
   countries adopted the approach".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hiv_prep_prevention_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOLOGICAL RESEARCHER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the "landmark trial" results)
   WHEN: civilizational (Evolutionary interaction between virus and medicine)
   WHERE: analytical (Universal biochemical interactions)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the efficacy of antivirals against the HIV 
   replication cycle is a "Mountain"—a fixed biological fact of modern medicine 
   demonstrated through repeated study.
   
   NARRATIVE EVIDENCE:
   "Slew of studies showed that, when used correctly, PrEP cuts the risk of HIV 
   transmission by more than 90 per cent".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    hiv_prep_prevention_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hiv_prep_prevention_2026_tests).

test(multi_perspective_prep) :-
    % User perspective (Rope)
    constraint_indexing:constraint_classification(
        hiv_prep_prevention_2026,
        rope,
        context(individual_powerless, biographical, trapped, local)
    ),
    % Analyst perspective (Mountain)
    constraint_indexing:constraint_classification(
        hiv_prep_prevention_2026,
        mountain,
        context(analytical, civilizational, analytical, global)
    ).

test(extractiveness_vs_freedom) :-
    % While extractiveness exists (daily pill), freedom gains are higher.
    domain_priors:base_extractiveness(hiv_prep_prevention_2026, E),
    E < 0.3.

:- end_tests(hiv_prep_prevention_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================= */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.15):
 * Reasoning: PrEP requires a daily pharmaceutical commitment, which is 
 * a minor form of economic and behavioral extraction. 
 * However, it is overwhelmingly beneficial to the user's freedom.
 * * 2. PERSPECTIVE SELECTION:
 * Focused on the individual user's regained sexual freedom (Rope) 
 * vs the objective biological efficacy (Mountain).
 * * 3. CLASSIFICATION RATIONALE:
 * - User (Rope): It provides a "revelation" of contact and exchange without 
 * the previous "Snare" of stigma/fear.
 * - Analyst (Mountain): The >90% efficacy is a physical reality of the 
 * antiviral mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prep_resistance_evolution,
    "Will long-term mass usage lead to HIV strains resistant to current PrEP drugs?",
    resolution_mechanism("Genetic sequencing of transmission breakthroughs in high-usage regions"),
    impact("If yes: Current Rope snaps, requiring a new Mountain of research. If no: Permanent Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    access_equity_as_noose,
    "Is the global adoption truly 150 countries or does it remain a 'Snare' of 
     unavailability for underserved communities within those nations?",
    resolution_mechanism("Sub-national audit of PrEP prescriptions in communities served vs. underserved"),
    impact("If unequal: Snare for the marginalized. If equal: Global Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Condom-Only Prevention
 * Viability: Historically primary; "condoms provide protection".
 * Suppression: Rejected by many as they "can fail" and "make sex less 
 * pleasurable".
 * Evidence: "Many prefer not to use them".
 * * CONCLUSION:
 * The advent of PrEP turned the "Condom Snare" (perceived failure/lack of 
 * pleasure) into a "Biological Rope" (90% reduction + skin contact).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [hiv_prep_prevention_2026].
% Multi-perspective: ?- multi_index_report(hiv_prep_prevention_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
