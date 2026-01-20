% ============================================================================
% CONSTRAINT STORY: tractarian_logic_limit
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Tractatus Logico-Philosophicus (Ludwig Wittgenstein)
% ============================================================================

:- module(constraint_tractarian_logic, []).

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
 * * constraint_id: tractarian_logic_limit
 * human_readable: The Limits of Language (Tractatus)
 * domain: logical/philosophical
 * temporal_scope: 1921 - Present (Analytical Philosophy)
 * spatial_scope: Universal / Abstract Logic
 * * SUMMARY:
 * The world consists of facts in logical space. Language is a "picture" of facts. 
 * Any attempt to use language outside the scope of representing states of affairs 
 * (e.g., ethics, aesthetics, metaphysics) hits a hard limit. This limit is 
 * not a choice but an inherent property of logical form.
 * * KEY AGENTS:
 * - The Logician (Wittgenstein/Russell): The analytical observer defining the rules.
 * - The Speaking Subject: The individual agent attempting to express meaning.
 * - The Mystical: The domain that lies "outside" the limit—unspeakable but "shown."
 * * NARRATIVE ARC:
 * The work begins by defining the "World" (1-2), moves through the "Picture Theory" 
 * of language (3-4), formalizes the logic of propositions (5), and culminates in 
 * the famous limit (7): "Whereof one cannot speak, thereof one must be silent."
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(tractarian_logic_limit, 0, 10).
narrative_ontology:constraint_claim(tractarian_logic_limit, mountain).

% Base extractiveness score: High
% Rationale: TLP extracts almost all traditional philosophical territory 
% (Ethics, Religion, Aesthetics) and reclassifies it as "nonsense."
domain_priors:base_extractiveness(tractarian_logic_limit, 0.7).

% Suppression score: High
% Rationale: Metaphysical language is not merely discouraged; it is logically 
% impossible to formulate as a valid "picture" of the world.
domain_priors:suppression_score(tractarian_logic_limit, 0.95).

% Enforcement requirements: Emerges naturally from logical form
domain_priors:emerges_naturally(tractarian_logic_limit).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(tractarian_logic_limit, extractiveness, 0.7).
narrative_ontology:constraint_metric(tractarian_logic_limit, suppression_requirement, 0.95).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(tractarian_logic_limit, [clarity, natural_science]).
constraint_victim(tractarian_logic_limit, [metaphysicians, ethical_discourse]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SPEAKING SUBJECT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - The subject whose world is "my language."
   WHEN: civilizational - A permanent boundary of human cognition.
   WHERE: trapped - "The limits of my language mean the limits of my world."
   SCOPE: global - Universal application.
   
   WHY THIS CLASSIFICATION:
   For the agent inside language, the logical form is a "Mountain." It is an 
   unchangeable fact of reality. One cannot "negotiate" with logic or choose 
   to see facts differently than logic allows. There is zero degrees of 
   freedom in logical space.
   
   NARRATIVE EVIDENCE:
   "Logic is not a theory but a reflexion of the world. Logic is transcendental."
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    tractarian_logic_limit,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        constraint_beneficiary(tractarian_logic_limit, clarity),
        constraint_victim(tractarian_logic_limit, []),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(tractarian_logic_limit, S),
    S > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PHILOSOPHER (Wittgenstein) - Rope
   --------------------------------------------------------------------------
   WHO: analytical - The observer mapping the boundaries.
   WHEN: biographical - The TLP is a ladder to be used and then discarded.
   WHERE: arbitrage - Stepping "outside" language to see its limits.
   SCOPE: national - The domain of scholarship.
   
   WHY THIS CLASSIFICATION:
   Wittgenstein describes his own propositions as a "Rope" (or ladder). They 
   provide a functional coordination mechanism for the mind to reach a 
   certain altitude of clarity. Once the altitude is reached, the "Rope" 
   is recognized as nonsensical and must be "thrown away."
   
   NARRATIVE EVIDENCE:
   "My propositions are elucidatory in this way: he who understands me 
   finally recognizes them as senseless, when he has climbed out through 
   them, on them, over them."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tractarian_logic_limit,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        constraint_beneficiary(tractarian_logic_limit, clarity),
        constraint_victim(tractarian_logic_limit, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ETHICIST / METAPHYSICIAN - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful - The traditional authority on "Value."
   WHEN: immediate - Confronting the total loss of discursive legitimacy.
   WHERE: constrained - Prevented from defining "Good" or "God."
   SCOPE: local - A specific field of study.
   
   WHY THIS CLASSIFICATION:
   To the metaphysician, the Tractatus is a "Noose." It is a coercive logical 
   maneuver that extracts their professional status and communicative ability 
   by declaring their entire field "unspeakable." It is an asymmetric 
   suppression of "Value" in favor of "Fact."
   
   NARRATIVE EVIDENCE:
   "The sense of the world must lie outside the world... In it there is 
   no value—and if there were, it would be of no value."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tractarian_logic_limit,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(tractarian_logic_limit, natural_science),
        constraint_victim(tractarian_logic_limit, metaphysicians),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(tractarian_logic_limit, E),
    E > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tractarian_tests).

test(perspectival_gap_analysis) :-
    % Speaking Subject sees Mountain; Philosopher sees Rope (Ladder).
    constraint_indexing:constraint_classification(tractarian_logic_limit, mountain, context(agent_power(individual_powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(tractarian_logic_limit, rope, context(agent_power(analytical), _, _, _, _, _)).

test(extraction_of_value) :-
    % Metaphysicians experience a 0.7 extraction of their domain (Noose).
    domain_priors:base_extractiveness(tractarian_logic_limit, 0.7).

test(suppression_necessity) :-
    % Suppression is near absolute (0.95).
    domain_priors:suppression_score(tractarian_logic_limit, 0.95).

:- end_tests(tractarian_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * * The TLP is a unique case where the author explicitly defines his 
 * constraint system as a "Rope" (Ladder) that becomes a "Mountain" (Fixed 
 * Boundary) only after the climb is complete.
 * * The central insight of Deferential Realism here is the **Perspectival 
 * Collapse**. Usually, a Rope is beneficial and a Noose is harmful. 
 * Wittgenstein argues that for "The Truth," the Noose (suppression of 
 * metaphysical nonsense) is the ultimate benefit to the Speaking Subject.
 * * The extractiveness is intentionally high because the TLP seeks to 
 * "solve all problems" by removing the right to ask them, effectively 
 * extracting agency from the questioner.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mystical_interaction,
    "How does the 'Mystical' exert influence if it is logically excluded from the world of facts?",
    resolution_mechanism("Requires a recursive analysis of Wittgenstein's later transition to 'Language Games'."),
    impact("If the Mystical cannot interact: TLP is a perfect Mountain. If it does: TLP is a leaking Noose."),
    confidence_without_resolution(low)
).

omega_variable(
    logical_atomism_validity,
    "Is the 'Fact' truly the fundamental unit of the world (Natural Law) or a convenient linguistic unit (Tool)?",
    resolution_mechanism("Verification through quantum logic or cognitive science models."),
    impact("If Fact is a Tool: The Mountain of TLP is actually a Rope of analytic preference."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Ordinary Language (Later Wittgenstein)
 * Viability: High. Language is a social practice (Rope) rather than a 
 * fixed logical mirror (Mountain).
 * Suppression: In the TLP, this is suppressed by the insistence on a 
 * single, underlying logical form for all meaningful speech.
 * * ALTERNATIVE 2: Continental Metaphysics
 * Viability: Realized in historical traditions.
 * Suppression: Categorically rejected as "senseless" because it fails the 
 * "Picture Theory" test.
 * * CONCLUSION:
 * The TLP's existence as a "Noose" for philosophers depends entirely 
 * on the suppression of Alternative 1. When Wittgenstein later accepts 
 * Alternative 1, the "Noose" of the TLP dissolves into a failed "Rope."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
