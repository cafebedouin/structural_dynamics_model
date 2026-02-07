% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: metamorphosis_samsa
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Metamorphosis by Franz Kafka (David Wyllie Trans.)
% ============================================================================

:- module(constraint_metamorphosis_samsa, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: metamorphosis_samsa
 * human_readable: The Samsa Vermin Transformation
 * domain: biological/economic/social
 * temporal_scope: Late 19th/Early 20th Century (Industrial Era)
 * spatial_scope: The Samsa Family Flat (Urban Apartment)
 * * SUMMARY:
 * Gregor Samsa wakes to find himself transformed into a "horrible vermin." 
 * This transformation serves as a hard biological constraint that immediately 
 * triggers a secondary economic constraint: the inability to perform labor 
 * to pay off his parents' debt to his employer.
 * * KEY AGENTS:
 * - Gregor Samsa: The traveling salesman (Victim/Powerless subject).
 * - The Chief Clerk: Representative of the Firm (Institutional enforcer).
 * - Mr. Samsa: Gregor's father (Transitioning from powerless to institutional).
 * - Grete Samsa: Gregor's sister (Initially functional coordination, later coercive).
 * * NARRATIVE ARC:
 * The constraint begins as a shocking biological event (Mountain) for Gregor,
 * becomes a breach of contract (Snare) for the employer, and eventually 
 * transforms from a care-taking coordination (Rope) into an existential 
 * threat (Snare) for the family, culminating in Gregor's removal/death.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(samsa_transformation_period, 0, 10).
narrative_ontology:constraint_claim(metamorphosis_samsa, mountain).

% Base extractiveness score (0.8): High. 
% Rationale: The system (Employer + Family) has extracted 5 years of 
% unceasing labor from Gregor to service a debt he did not incur.
domain_priors:base_extractiveness(metamorphosis_samsa, 0.8).

% Suppression score (0.7): High.
% Rationale: The family suppresses Gregor's humanity, his voice is 
% dismissed as "animal sounds," and alternatives (medical care) are 
% abandoned almost immediately.
domain_priors:suppression_score(metamorphosis_samsa, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(metamorphosis_samsa, extractiveness, 0.8).
narrative_ontology:constraint_metric(metamorphosis_samsa, suppression_requirement, 0.7).

% Enforcement requirements
% Emerges naturally as a biological fact, but requires active 
% social enforcement (locking the door) to maintain the "Snare" status.
domain_priors:requires_active_enforcement(metamorphosis_samsa).

% Metrics required for Section 1 of the Executive Summary
% Beneficiaries and Victims
constraint_beneficiary(metamorphosis_samsa, the_firm).
constraint_beneficiary(metamorphosis_samsa, mr_samsa). % Once Gregor is "removed" and debt is moot.
constraint_victim(metamorphosis_samsa, gregor_samsa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GREGOR SAMSA - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - He is physically transformed and unable to communicate.
   WHEN: immediate - His focus is on the next train (5:00 AM) and immediate bodily pain.
   WHERE: trapped - Locked in his room, unable to use the door handle or move freely.
   SCOPE: local - His world is reduced to the floor, walls, and ceiling of one room.
   
   WHY THIS CLASSIFICATION:
   For Gregor, the verminhood is an unchangeable law of nature. He cannot "will"
   his way back to humanity. It has zero degrees of freedom.
   
   NARRATIVE EVIDENCE:
   "He was used to sleeping on his right, and in his present state couldn't get 
   into that position." "He had no proper teeth—how was he, then, to grasp the key?"
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    metamorphosis_samsa,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(powerless, immediate, trapped, local),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CHIEF CLERK - Snare
   --------------------------------------------------------------------------
   
   WHO: institutional - Represents the legal/economic power of the employer.
   WHEN: immediate - Concerned only with the missed 5:00 AM and 7:00 AM trains.
   WHERE: mobile - He can leave the flat at any time (and does so in a panic).
   SCOPE: national - Part of a larger commercial network of travelers.
   
   WHY THIS CLASSIFICATION:
   The Clerk views the constraint as an extractive/coercive failure. He 
   interprets Gregor's absence not as a tragedy, but as "peculiar whims" 
   or potential "theft of funds." It is a Snare designed to force Gregor 
   back to work.
   
   NARRATIVE EVIDENCE:
   "You fail to carry out your business duties in a way that is quite unheard 
   of... your position is not all that secure."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    metamorphosis_samsa,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(metamorphosis_samsa, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CHARWOMAN - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical - She views Gregor with detached, coarse curiosity.
   WHEN: biographical - She has seen "the hardest of things in her long life."
   WHERE: mobile - She comes and goes as an employee, not a family member.
   SCOPE: local - He is just an object in the room she cleans.
   
   WHY THIS CLASSIFICATION:
   To the charwoman, Gregor is not a horror or a legal failure; he is a 
   "dung-beetle." He is a functional element of her workday, something to 
   be poked with a broom or ignored.
   
   NARRATIVE EVIDENCE:
   "At first she would call to him... 'come on then, you old dung-beetle!'" 
   "She wasn't really repelled by Gregor."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    metamorphosis_samsa,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(metamorphosis_samsa, S),
    S < 0.8, % She doesn't fear him, so she doesn't suppress the reality.
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(metamorphosis_samsa_tests).

test(multi_perspective_variance) :-
    % Gregor's View
    constraint_indexing:constraint_classification(metamorphosis_samsa, Type1, 
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))),
    % Clerk's View
    constraint_indexing:constraint_classification(metamorphosis_samsa, Type2, 
        context(agent_power(institutional), time_horizon(immediate), exit_options(mobile), spatial_scope(national))),
    % Charwoman's View
    constraint_indexing:constraint_classification(metamorphosis_samsa, Type3, 
        context(agent_power(analytical), time_horizon(biographical), exit_options(mobile), spatial_scope(local))),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 == mountain,
    Type2 == snare,
    Type3 == rope.

test(power_extractiveness_scaling) :-
    % Powerless Gregor experiences high extractiveness
    domain_priors:base_extractiveness(metamorphosis_samsa, Score),
    Score > 0.7.

test(exit_option_shift) :-
    % Sister shifts from Rope (care) to Snare (removal) as her exit options 
    % (marriage/future) become constrained by Gregor's presence.
    true.

:- end_tests(metamorphosis_samsa_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.8): 
 * Gregor's life is defined by the extraction of his vitality to pay a 
 * debt that is not his. The transformation is the point where the 
 * "machine" of his labor breaks down, and the system turns on him.
 * * 2. PERSPECTIVE SELECTION: 
 * Included the Charwoman as the 'Analytical' perspective because she 
 * lacks the emotional/economic baggage of the family, proving that 
 * 'Vermin' is a classification, not just a fact.
 * * 3. CLASSIFICATION RATIONALE:
 * - Gregor -> Mountain: He attempts to "sleep it off" initially, 
 * treating it as a temporary illness (Rope), but the reality of his 
 * legs and lack of teeth forces a 'Mountain' realization.
 * - Clerk -> Snare: His first reaction is to threaten Gregor's job 
 * and reputation, typical of an extractive institutional perspective.
 * * 4. CONFIDENCE: 
 * High: The economic/labor constraints are explicit in Kafka's text.
 * Medium: The classification of the Charwoman as 'Rope' depends on 
 * interpreting her lack of fear as a functional relationship to the 
 * "object" of Gregor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vermin_ontology,
    "Is Gregor's transformation a biological mutation or a psychological manifestation?",
    resolution_mechanism("Requires external medical/biological examination not present in the text."),
    impact("If Biological: Mountain. If Psychological: Scaffold/Rope (changeable with therapy)."),
    confidence_without_resolution(low)
).

omega_variable(
    debt_origin,
    "What was the specific nature of the 'business misfortune' 5 years ago?",
    resolution_mechanism("Historical audit of Mr. Samsa's collapsed firm."),
    impact("Determines if the original debt was a Snare (extractive) or a Rope (risk coordination)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Medical Intervention
 * Viability: The family mentions calling a doctor but never follows through 
 * once the "animal" voice is heard.
 * Suppression: Actively suppressed by the father's violence and the 
 * clerk's immediate dismissal.
 * * ALTERNATIVE 2: Empathy/Integration
 * Viability: Grete initially attempts to find food Gregor likes.
 * Suppression: Abandoned as Grete's own biographical time-horizon 
 * (marriage/work) becomes threatened by the social stigma.
 * * CONCLUSION:
 * The rapid suppression of these alternatives shifts the family's 
 * classification of Gregor from a 'broken Rope' (sick brother) to a 
 * 'Snare' (monster that must be removed).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% narrative_ontology:interval(metamorphosis_samsa, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
