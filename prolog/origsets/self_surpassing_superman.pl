% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: self_surpassing_superman
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Thus Spake Zarathustra by Friedrich Nietzsche
% ============================================================================

:- module(constraint_rear_superman, []).

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
 * * constraint_id: self_surpassing
 * human_readable: The Rearing of the Superman (Übermensch)
 * domain: philosophical/social
 * temporal_scope: Post-Theistic / Modernity
 * spatial_scope: Global (Humanity)
 * * SUMMARY:
 * This constraint represents the imperative to overcome the "All-too-human" 
 * state of the "Last Man" following the "Death of God." It functions as an 
 * existential bridge (Rope) that demands the total transvaluation of existing 
 * "slave" moral values into "master" values of power and creation.
 * * KEY AGENTS:
 * - Zarathustra: The prophet/teacher who defines the "Rope" between beast and Superman.
 * - The Last Man: The degenerate agent who finds "happiness" in static comfort and equality.
 * - The Higher Man: The failed bridge-builder who suffers under the "Snare" of modern nihilism.
 * - The New Idol (The State): The institutional agent that seeks to replace God with a secular Snare.
 * * NARRATIVE ARC:
 * Zarathustra descends from the mountain to offer the "gift" of the Superman. 
 * He is met with the laughter of the market-place (The Last Men), survives 
 * the "Great Disgust," and eventually identifies his "Children" (Posterity) 
 * as the true beneficiaries of his hard-won tables of values.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(zarathustra_hazar, 0, 10).
narrative_ontology:constraint_claim(self_surpassing, rope).

% Base extractiveness score: High (0.8)
% Rationale: The rearing of the Superman extracts everything from the individual—
% "the last full measure" of comfort, safety, and traditional morality.
domain_priors:base_extractiveness(self_surpassing, 0.8).

% Suppression score: High (0.7)
% Rationale: Traditional "Good and Just" values actively suppress the creative 
% "robber" who seeks to break old tables.
domain_priors:suppression_score(self_surpassing, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(self_surpassing, extractiveness, 0.8).
narrative_ontology:constraint_metric(self_surpassing, suppression_requirement, 0.7).

% Enforcement: Requires active will-to-power and the "Bestowing Virtue."
domain_priors:requires_active_enforcement(self_surpassing).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(self_surpassing, [posterity, the_superman, the_earth]).
constraint_victim(self_surpassing, [the_last_man, the_good_and_just, "the spirit of gravity"]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LAST MAN - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The "hop-flea" of the earth who wants only comfort.
   WHEN: immediate - Focused on "little pleasures for the day."
   WHERE: trapped - Bound within the static world of "Equality" and "Happiness."
   SCOPE: global - "The earth hath become small."
   
   WHY THIS CLASSIFICATION:
   To the Last Man, existing reality is a Mountain. He has no desire to 
   change the world or himself; he believes "We have discovered happiness." 
   There are zero degrees of freedom to pursue greatness because greatness 
   is not even conceivable.
   
   NARRATIVE EVIDENCE:
   "No shepherd, and one herd! Every one wanteth the same; every one is equal."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    self_surpassing,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(self_surpassing, the_last_man),
        constraint_victim(self_surpassing, []),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(self_surpassing, S),
    S < 0.5, % Last men don't even perceive the suppression.
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ZARATHUSTRA - Rope
   --------------------------------------------------------------------------
   WHO: analytical - The observer and teacher of the "Great Year."
   WHEN: generational - Thinking of "Children's Land" and the "Hazar."
   WHERE: arbitrage - Moving between the mountains (solitude) and valleys (men).
   SCOPE: global - Defining the "meaning of the earth."
   
   WHY THIS CLASSIFICATION:
   Zarathustra literally describes man as a "rope over an abyss." To him, 
   values are functional tools to coordinate human evolution towards the 
   Superman. They are changeable ("Old and New Tables") and meant to be surpassed.
   
   NARRATIVE EVIDENCE:
   "Man is a rope stretched between the animal and the Superman."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    self_surpassing,
    rope,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(arbitrage),
        constraint_beneficiary(self_surpassing, posterity),
        constraint_victim(self_surpassing, []),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE HIGHER MAN - Snare
   --------------------------------------------------------------------------
   WHO: powerful - Artists, kings, and priests who "cry for help."
   WHEN: biographical - Suffering their "loneliest hours" in a degenerate age.
   WHERE: constrained - Surrounded by the "flies of the market-place."
   SCOPE: local - Their immediate social and spiritual context.
   
   WHY THIS CLASSIFICATION:
   For the Higher Men, the "Good and Just" morality is a Snare. It extracts 
   their power through pity and "slave-morality" resentment. It is a 
   coercive force that makes them "shatter" because they cannot yet 
   overleap the abyss.
   
   NARRATIVE EVIDENCE:
   "I see your soul distorted by the spirit of gravity... thy virtue is a 
   Snare around thy neck."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    self_surpassing,
    snare,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(constrained),
        constraint_beneficiary(self_surpassing, the_herd),
        constraint_victim(self_surpassing, higher_men),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(self_surpassing, E),
    E > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(self_surpassing_tests).

test(perspectival_gap_superman) :-
    % The Last Man (Powerless) sees a Mountain of comfort.
    constraint_indexing:constraint_classification(self_surpassing, mountain, context(agent_power(powerless), _, _, _, _, _)),
    % Zarathustra (Analytical) sees a Rope/Bridge.
    constraint_indexing:constraint_classification(self_surpassing, rope, context(agent_power(analytical), _, _, _, _, _)).

test(extraction_penalty_for_greatness) :-
    % Verify high extractiveness for those attempting the surpass.
    domain_priors:base_extractiveness(self_surpassing, Score),
    Score >= 0.8.

test(herd_suppression) :-
    % The herd acts as a suppression mechanism against new values.
    domain_priors:suppression_score(self_surpassing, Score),
    Score > 0.5.

:- end_tests(self_surpassing_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * * The central tension of Zarathustra is the "type-shift" of constraints 
 * based on the agent's degree of Will to Power. 
 * * For the mediocre (Last Man), the social constraint is a MOUNTAIN: 
 * invisible, natural, and comfortable. 
 * * For the struggling creator (Higher Man), traditional morality is a SNARE: 
 * a device that extracts his potential to feed the herd.
 * * Zarathustra’s project is to convert the SNARE of traditional values into 
 * the ROPE of self-surpassing. He explicitly uses the "Rope" metaphor to 
 * denote a functional, albeit dangerous, coordination mechanism.
 * * The high extractiveness (0.8) reflects the "last full measure of devotion" 
 * Nietzsche requires of the creator—the sacrifice of the self for the 
 * concept of the Superman.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternal_recurrence_verification,
    "Is the 'Eternal Recurrence' a physical Mountain (law of physics) or a Rope (psychological tool)?",
    resolution_mechanism("Depends on the empirical reality of the 'Circuit of Circuits' in thermodynamics."),
    impact("If Mountain: Choice is an illusion. If Rope: It is the ultimate test of Yea-saying."),
    confidence_without_resolution(low)
).

omega_variable(
    superman_feasibility,
    "Is the Superman a real biological possibility or a poetic phantom?",
    resolution_mechanism("Generational monitoring of 'Hazar' developments."),
    impact("If phantom: Zarathustra is a 'false magician' leading to a Snare of despair."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Christian Nihilism
 * Viability: Historically dominant (The Old Table).
 * Suppression: Rejected by the fact of the "Death of God."
 * Evidence: "God is Dead! Of his pity for man hath God died."
 * * ALTERNATIVE 2: The New Idol (The State)
 * Viability: Emerging as the secular replacement for the church.
 * Suppression: Critiqued as "the coldest of all cold monsters."
 * Evidence: "There, where the state ceaseth... there commenceth the man."
 * * CONCLUSION:
 * The existence of these powerful alternatives (Church and State) forces 
 * Zarathustra's path into a SNARE classification for any agent caught 
 * between the old values and the new, as both institutions actively 
 * suppress the "Way of the Creating One."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(self_surpassing_superman, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(self_surpassing_superman, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(self_surpassing_superman, snare, agent_power(powerless)).
