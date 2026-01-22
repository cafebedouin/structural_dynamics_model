% ============================================================================
% CONSTRAINT STORY: lsd_microdosing_professional_openness
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Ergot on Rye" (citing Scott Alexander and psilocybin studies)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(lsd_microdosing_professional_openness, []).

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
 * * constraint_id: lsd_microdosing_professional_openness
 * human_readable: The Permanent Openness Shift (Microdosing)
 * domain: psychological/professional
 * temporal_scope: Contemporary / 2026 Era
 * spatial_scope: Global (Corporate/Creative Labor Markets)
 * * SUMMARY:
 * This constraint analyzes the professional application of LSD microdosing to increase 
 * the personality dimension of "Openness to Experience". In professional 
 * settings, this serves as a tool to become "more interesting" and gain 
 * "dividends" in creative or analytical roles. The core tension lies in 
 * whether this shift is a temporary performance enhancer or a permanent 
 * personality restructuring.
 * * KEY AGENTS:
 * - The Professional User (Individual Moderate): Utilizes microdosing to "pay real 
 * dividends" in a competitive society.
 * - The Institutional Employer: Benefits from increased employee "Openness" and 
 * creativity, often without full awareness of the biochemical driver.
 * - The Psychiatrist/Analyst: Observes with "terror" that a substance can produce 
 * "large, permanent personality changes" far beyond traditional psychiatry.
 * * NARRATIVE ARC:
 * Derived from the "forgotten plague" of ergot, modern microdosing attempts to 
 * harness lysergic acid precursors to "strap a little madness" to the yoke of 
 * professional sanity. It functions as a "Rope" for competitive advantage, 
 * but risks becoming a "Noose" or "Mountain" if it alters the subject's 
 * personality without ongoing consent.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(lsd_microdosing_interval, 0, 10).
narrative_ontology:constraint_claim([lsd_microdosing_professional_openness], [biochemical_optimization]).

% Base extractiveness score (0.4 = Moderate)
% Rationale: The practice extracts the user's "baseline" personality stability 
% to fuel professional "dividends". If permanent, it extracts 
% future "consent" to return to the original self.
domain_priors:base_extractiveness(lsd_microdosing_professional_openness, 0.4).

% Suppression score (0.5 = Moderate)
% Rationale: Professional "sanity" and legal frameworks suppress the 
% "madness" of psychedelics, even as microdosing trends attempt to 
% normalize it.
domain_priors:suppression_score(lsd_microdosing_professional_openness, 0.5).

% Enforcement: Requires active individual maintenance (dosing) but results 
% may emerge naturally as permanent traits.
domain_priors:requires_active_enforcement(lsd_microdosing_professional_openness).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, extractiveness, 0.4).
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
% Beneficiary: The "More Interesting" Agent (gains competitive edge).
constraint_beneficiary(lsd_microdosing_professional_openness, professional_meritocracy).
% Victim: Personality Stability (eroded in adulthood).
constraint_victim(lsd_microdosing_professional_openness, adult_personality_constancy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMPETITIVE WORKER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Seeking dividends in society)
   WHEN: biographical (Advancing a career over decades)
   WHERE: mobile (Choosing to microdose for openness)
   SCOPE: local (Personal professional performance)
   
   WHY THIS CLASSIFICATION:
   For the professional, microdosing is a "Rope"—a functional coordination 
   mechanism to expand their "Openness to Experience" and become a "more 
   interesting" player in the market.
   
   NARRATIVE EVIDENCE:
   "microdosing suggest that trying to be a little more interesting might pay 
   real dividends in our society".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    lsd_microdosing_professional_openness,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRAPPED SUBJECT (Omega = Permanent) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (After the "permanent" shift has occurred)
   WHEN: civilizational/biographical (A stable shift 16 months later)
   WHERE: trapped (Cannot return to previous personality "standard")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   If the Omega variable resolves to "Permanent," the shift is a "Noose." 
   The agent is "terrified" to find their personality changed "without 
   their consent," with no "neutral ground" to return to the original self.
   
   NARRATIVE EVIDENCE:
   "something that can produce large, permanent personality changes... is 
   kind of terrifying... make you more interesting without your consent".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    lsd_microdosing_professional_openness,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(lsd_microdosing_professional_openness, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INSTITUTIONAL PSYCHIATRIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional/analytical (Observing "immutable" personality scales)
   WHEN: historical (Personality is "otherwise pretty stable after adulthood")
   WHERE: analytical (Observer stance)
   SCOPE: global (The limits of human psychiatry)
   
   WHY THIS CLASSIFICATION:
   To the analytical observer, the "stability" of adult personality is a 
   "Mountain." The fact that a substance can bypass this unchangeable law 
   is a "scary" violation of the psychological terrain.
   
   NARRATIVE EVIDENCE:
   "personality is otherwise pretty stable after adulthood; nothing should be 
   able to do this".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lsd_microdosing_professional_openness,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(lsd_microdosing_professional_openness_tests).

test(multi_perspective_permanence) :-
    % User (Biographical/Mobile) sees a Rope
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, rope, context(individual_moderate, biographical, mobile, local)),
    % Victim (Civilizational/Trapped) sees a Noose if it's permanent
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, noose, context(individual_powerless, civilizational, trapped, local)),
    % Scientist (Historical/Analytical) sees a Mountain (the stability rule)
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, mountain, context(analytical, historical, analytical, global)).

test(extraction_of_consent) :-
    % Shows that individual_powerless (trapped in a new personality) 
    % experiences extraction of their former self.
    constraint_indexing:extractiveness_for_agent(lsd_microdosing_professional_openness, context(individual_powerless, civilizational, trapped, local), E),
    E > 0.3.

:- end_tests(lsd_microdosing_professional_openness_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. IMPACT OF OMEGA VARIABLE:
 * If `hallucinogenic_personality_permanence` is resolved as PERMANENT, the 
 * constraint shifts from a "Rope" (performance enhancer) to a "Mountain/Noose" 
 * (structural alteration). The source specifically highlights the "terror" 
 * of losing the "stable" mountain of adult personality.
 * * 2. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: The system extracts "constancy" for "dividends". 
 * While the user benefits, the "without consent" aspect of a permanent 
 * shift creates a high-asymmetry risk.
 * * 3. PERSPECTIVE SELECTION:
 * Contrasted the "Competitive Worker" who sees utility (Rope) with the 
 * "Trapped Subject" who faces the irreversible consequence of the Omega 
 * variable (Noose).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core Omega identified by the user:
omega_variable(
    hallucinogenic_personality_permanence,
    "Is the increase in Openness a temporary performance state (Rope) or a 
     permanent restructuring of the adult ego (Mountain)?",
    resolution_mechanism("Longitudinal personality tracking of microdosers 5+ years after cessation"),
    impact("If permanent: The Rope becomes a Mountain. If temporary: It remains a state-management Rope."),
    confidence_without_resolution(high)
).

omega_variable(
    microdosing_professional_intent,
    "Is microdosing an individual search for truth (Rope) or a predatory 
     requirement to remain 'interesting' in a hyper-competitive market (Noose)?",
    resolution_mechanism("Audit of 'Openness' requirements in creative/tech job hunting systems"),
    impact("If predatory: Mandatrophy Noose. If elective: User Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "Strap to the Yoke of Sanity"
 * Viability: The traditional, un-augmented state of adult personality constancy.
 * Suppression: Suppressed by a society where "trying to be a little more 
 * interesting" is necessary to "pay dividends".
 * * ALTERNATIVE 2: Ritualized Madness (Eleusinian/Uniao do Vegetal)
 * Viability: Communal, religious contexts for expanding openness.
 * Suppression: Often shunted by "civilized society" in favor of individual, 
 * performance-oriented microdosing.
 * * CONCLUSION:
 * The Omega variable's resolution determines if these alternatives are 
 * merely "other paths" or "lost territories" for the agent.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [lsd_microdosing_professional_openness].
% Run analysis: ?- constraint_indexing:multi_index_report(lsd_microdosing_professional_openness).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
