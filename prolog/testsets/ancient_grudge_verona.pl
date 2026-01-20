% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: ancient_grudge_verona
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Tragedy of Romeo and Juliet by William Shakespeare
% ============================================================================

:- module(constraint_ancient_grudge_verona, []).

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
 * * constraint_id: ancient_grudge
 * human_readable: The Montague-Capulet Feud
 * domain: social/political
 * temporal_scope: Renaissance (Verona)
 * spatial_scope: Urban/Verona
 * * SUMMARY:
 * An inherited transgenerational conflict ("ancient grudge") that mandates 
 * spontaneous violence between two noble houses. This constraint overrides 
 * civil law, personal desire, and religious intervention, functioning as the 
 * primary filter for all social interaction in Verona.
 * * KEY AGENTS:
 * - Romeo/Juliet: Individual powerless; their agency is entirely circumscribed by the feud.
 * - Tybalt: Individual powerful; an enforcer who identifies exclusively with the constraint.
 * - Prince Escalus: Institutional; attempts to suppress the feud through legal edicts.
 * - Friar Lawrence: Analytical/Moderate; attempts to arbitrage the feud using the "Rope" of marriage.
 * * NARRATIVE ARC:
 * The play begins with a "new mutiny" (Act 1), establishing the feud's 
 * reach into the servant class. It peaks with the deaths of Mercutio and 
 * Tybalt (Act 3), which forces the constraint into its "Noose" phase, 
 * leading to the final "extraction" of the heirs in the Capulet monument.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(verona_feud_interval, 0, 10).
narrative_ontology:constraint_claim(ancient_grudge, noose).

% Base extractiveness score: High (0.8)
% Rationale: The feud extracts the lives of the "brace of kinsmen" and the 
% primary heirs of both houses for the sake of an unstated, ancient purpose.
domain_priors:base_extractiveness(ancient_grudge, 0.8).

% Suppression score: High (0.7)
% Rationale: The Prince’s edicts ("Your lives shall pay the forfeit") 
% demonstrate that the feud is a coercive force that actively resists 
% state-level suppression.
domain_priors:suppression_score(ancient_grudge, 0.7).

% Enforcement requirements: Emerges naturally from tribal/family identity.
domain_priors:emerges_naturally(ancient_grudge).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(ancient_grudge, extractiveness, 0.8).
narrative_ontology:constraint_metric(ancient_grudge, suppression_requirement, 0.7).

% Beneficiaries and Victims
constraint_beneficiary(ancient_grudge, [none, "tribal ego"]).
constraint_victim(ancient_grudge, [romeo, juliet, mercutio, tybalt, paris, lady_montague]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ROMEO & JULIET - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Their identities are assigned at birth.
   WHEN: immediate - The constraints apply "from forth the fatal loins."
   WHERE: trapped - Verona's walls contain the feud; exile is "death misterm'd."
   SCOPE: regional - The local world of Verona.
   
   WHY THIS CLASSIFICATION:
   For the lovers, the feud is an unchangeable law of nature. They cannot 
   negotiate with it; they can only attempt to "doff" their names, which 
   results in physical destruction. It has zero degrees of freedom.
   
   NARRATIVE EVIDENCE:
   "My only love sprung from my only hate! / Too early seen unknown, and known too late!"
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ancient_grudge,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(ancient_grudge, []),
        constraint_victim(ancient_grudge, [romeo, juliet]),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(ancient_grudge, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FRIAR LAWRENCE - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical - A chemist and religious counselor.
   WHEN: historical/biographical - Looking to "reconcile your friends."
   WHERE: arbitrage - Using his cell and Mantua as points of exit/leverage.
   SCOPE: local - Focused on the two families.
   
   WHY THIS CLASSIFICATION:
   The Friar views the feud (and the marriage) as a "Rope"—a coordination 
   mechanism that can be manipulated through "holy act" and "philosophy" 
   to achieve a social good. He believes the constraint is changeable.
   
   NARRATIVE EVIDENCE:
   "For this alliance may so happy prove, / To turn your households’ rancour to pure love."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ancient_grudge,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        constraint_beneficiary(ancient_grudge, peace),
        constraint_victim(ancient_grudge, []),
        spatial_scope(local)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PRINCE ESCALUS - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional - The sovereign of Verona.
   WHEN: immediate - Reacting to brawls "bred of an airy word."
   WHERE: constrained - The feud limits his ability to govern.
   SCOPE: regional - The entire city-state.
   
   WHY THIS CLASSIFICATION:
   For the Prince, the feud is a "Noose." It is a coercive, extractive 
   process that forces him to execute citizens and lose his own family 
   members. It is a drain on his political capital.
   
   NARRATIVE EVIDENCE:
   "And I, for winking at your discords too, / Have lost a brace of kinsmen. All are punish’d."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ancient_grudge,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(ancient_grudge, []),
        constraint_victim(ancient_grudge, [mercutio, paris]),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(ancient_grudge, E),
    E > 0.5,
    !.

/* ==========================================================================
   4. TESTS (Validation of the Verona Dataset)
   ========================================================================== */

:- begin_tests(ancient_grudge_tests).

test(perspectival_gap_verona) :-
    % Lovers see Mountain, Prince sees Noose.
    constraint_indexing:constraint_classification(ancient_grudge, mountain, context(agent_power(individual_powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge, noose, context(agent_power(institutional), _, _, _, _, _)).

test(extraction_threshold) :-
    % Verify the feud is highly extractive (deaths > 3).
    domain_priors:base_extractiveness(ancient_grudge, Score),
    Score >= 0.8.

test(analytical_agency) :-
    % The Friar attempts a Rope-based solution.
    constraint_indexing:constraint_classification(ancient_grudge, rope, context(agent_power(analytical), _, _, _, _, _)).

:- end_tests(ancient_grudge_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * * The core conflict of Romeo and Juliet is the failure of "Rope-making." 
 * The Friar attempts to use the marriage as a Rope to pull the families 
 * out of their grudge. However, because the extraction pressure (Noose) 
 * is too high and the timing is governed by "star-crossed" luck 
 * (Omega 1), the Rope snaps.
 * * This demonstrates the system rule: A failed Rope for a powerful agent 
 * is always a Noose for a powerless agent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    star_crossed_variance,
    "Is the tragic outcome a Mountain (Fate/Necessity) or a Noose (Bad Logistics/Friar John's quarantine)?",
    resolution_mechanism("Requires a counter-factual simulation of Friar John delivering the letter."),
    impact("If the letter is delivered, the Noose converts to a Rope; the tragedy is purely operational."),
    confidence_without_resolution(low)
).

omega_variable(
    ancient_origin,
    "What was the specific interest or asset that initiated the 'ancient grudge'?",
    resolution_mechanism("Historical/textual reconstruction (unknown)."),
    impact("If the original cause was a property dispute, the feud is a Noose that has lost its Rope-context."),
    confidence_without_resolution(zero)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Diplomatic Mediation
 * Viability: The Prince attempts this three times.
 * Suppression: Actively suppressed by Tybalt and Mercutio's "unruly spleen."
 * Evidence: Act 3, Scene 1 (The Duel).
 * * ALTERNATIVE 2: Inter-marriage
 * Viability: The primary strategy of the Friar.
 * Suppression: Suppressed by the "Noose" of the exile sentence.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ancient_grudge_verona, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ancient_grudge_verona, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ancient_grudge_verona, noose, agent_power(individual_powerless)).
