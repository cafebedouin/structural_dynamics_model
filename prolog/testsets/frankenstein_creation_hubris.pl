% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: frankenstein_creation_hubris
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Frankenstein; or, The Modern Prometheus by Mary Shelley
% ============================================================================

:- module(constraint_frankenstein_creation_hubris, []).

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
 * * constraint_id: frankenstein_creation_hubris
 * human_readable: The Prometheus/Creator Burden
 * domain: technological/existential/social
 * temporal_scope: Late 18th Century / Scientific Revolution
 * spatial_scope: Europe (Geneva, Ingolstadt, Britain, Arctic)
 * * SUMMARY:
 * Victor Frankenstein successfully animates matter, creating a sentient being. 
 * The constraint arises from the irreversible nature of this creation and the 
 * absolute moral and physical obligation the creator has toward the creature—
 * an obligation Victor rejects, triggering a catastrophic cycle of vengeance.
 * * KEY AGENTS:
 * - Victor Frankenstein: The institutional/powerful agent who breaks the laws of nature.
 * - The Creature: The powerless individual, a "biological artifact" with zero social standing.
 * - Robert Walton: The analytical/moderate observer who learns the "Rope" of Victor's warning.
 * * NARRATIVE ARC:
 * The arc begins with Victor's pursuit of "secrets of heaven and earth." Upon 
 * success, the "beauty of the dream vanished" into horror. The abandonment 
 * turns the creature into a "fiend," transforming the scientific achievement 
 * from a potential Rope into a lethal Noose for the entire Frankenstein lineage.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor for the Deferential Realism Index
narrative_ontology:interval(frankenstein_analysis, 0, 10).
narrative_ontology:constraint_claim(frankenstein_creation_hubris, noose).

% Base extractiveness score: High
% Rationale: The creation extracts the life, sanity, and loved ones of the creator.
% It also extracts the possibility of social existence from the creature.
domain_priors:base_extractiveness(frankenstein_creation_hubris, 0.9).

% Suppression score: High
% Rationale: The secret of the creation is suppressed by Victor’s ego and fear, 
% preventing any intervention until it is too late.
domain_priors:suppression_score(frankenstein_creation_hubris, 0.85).

% Enforcement: The constraint is enforced by the physical capabilities of 
% the creature and the psychological guilt of the creator.
domain_priors:requires_active_enforcement(frankenstein_creation_hubris).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(frankenstein_creation_hubris, extractiveness, 0.9).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, suppression_requirement, 0.85).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(frankenstein_creation_hubris, [scientific_knowledge, walton_caution]).
constraint_victim(frankenstein_creation_hubris, [william, justine, elizabeth, clerval, victor, creature]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CREATURE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Abandoned at birth, no legal or social status.
   WHEN: immediate - Every waking moment is defined by physical rejection.
   WHERE: trapped - Bound by a hideous form that precludes any human exit.
   SCOPE: global - "All men hate the wretched."
   
   WHY THIS CLASSIFICATION:
   For the Creature, the laws of human social rejection are as immutable 
   as a Mountain. He cannot negotiate his appearance or the instinctive 
   horror people feel. It appears to him as a natural law of misery.
   
   NARRATIVE EVIDENCE:
   "Was I, then, a monster, a blot upon the earth, from which all men fled?"
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    frankenstein_creation_hubris,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(frankenstein_creation_hubris, []),
        constraint_victim(frankenstein_creation_hubris, creature),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(frankenstein_creation_hubris, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: VICTOR FRANKENSTEIN - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - Possesses the knowledge and wealth to create life.
   WHEN: biographical - The secret haunts him from Ingolstadt to his death.
   WHERE: constrained - The Creature haunts his movements across Europe.
   SCOPE: national - Impacts the entire social structure of his family.
   
   WHY THIS CLASSIFICATION:
   Initially, Victor sees science as a Rope to pull him toward glory. 
   Once the Creature is animated, it becomes a Noose—a coercive 
   force that extracts his peace and the lives of those he loves.
   
   NARRATIVE EVIDENCE:
   "I had unchained an enemy among them whose joy it was to shed their blood."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    frankenstein_creation_hubris,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        constraint_beneficiary(frankenstein_creation_hubris, []),
        constraint_victim(frankenstein_creation_hubris, [elizabeth, clerval]),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(frankenstein_creation_hubris, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ROBERT WALTON - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer recording the tragedy.
   WHEN: historical - Using Victor's life as a lesson for future generations.
   WHERE: arbitrage - Walton can choose to turn back from the North Pole.
   SCOPE: global - Affecting the "passengers" and the future of science.
   
   WHY THIS CLASSIFICATION:
   For Walton, Victor's story is a functional Rope. It provides a 
   coordination mechanism that allows him to re-evaluate his own dangerous 
   ambition and choose safety for his crew over potential "glory."
   
   NARRATIVE EVIDENCE:
   "I have lost my hopes of utility and glory; I have lost my friend."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    frankenstein_creation_hubris,
    rope,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(frankenstein_creation_hubris, walton_caution),
        constraint_victim(frankenstein_creation_hubris, []),
        spatial_scope(global)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(frankenstein_tests).

test(perspectival_gap) :-
    % Creature sees a Mountain; Victor sees a Noose.
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, mountain, context(agent_power(individual_powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, noose, context(agent_power(individual_powerful), _, _, _, _, _)).

test(extraction_analysis) :-
    % High extraction triggers the need for a victim declaration.
    domain_priors:base_extractiveness(frankenstein_creation_hubris, 0.9).

test(walton_exit) :-
    % Analytical agent (Walton) utilizes the Rope for arbitrage.
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, rope, context(agent_power(analytical), _, arbitrage, _, _, _)).

:- end_tests(frankenstein_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: The core tragedy of Frankenstein is the "type shift." 
 * For Victor, the pursuit of science begins as a Rope (functional 
 * coordination with laws of nature for gain). After creation, it becomes 
 * a Noose. For the Creature, the rejection by society is a Mountain—he 
 * cannot find a single "exit option" in human empathy.
 * * 2. OMEGA VARIABLES: I identified the "Creator-Creation Duty" as the 
 * irreducible uncertainty. If Victor had performed his duty (Rope), 
 * would the Creature have remained benevolent?
 * * 3. WALTON'S ROLE: Walton is the only agent who successfully processes the 
 * constraint as a Rope. He treats Victor's death as the "break point" of 
 * the Noose and decides to exit his own ambition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternal_virtue,
    "Would a benevolent upbringing have overridden the Creature's physical 'Noose' of deformity?",
    resolution_mechanism("Comparative narrative analysis with modern psychological developmental models."),
    impact("If yes: The tragedy is social/moral. If no: The tragedy is biological/fate."),
    confidence_without_resolution(low)
).

omega_variable(
    scientific_confession,
    "Would an immediate legal/public confession by Victor have converted the 'Noose' into a 'Rope' of collective containment?",
    resolution_mechanism("Simulating early disclosure of the Creature to the Genevan syndics."),
    impact("Early confession might have saved Justine and William but likely resulted in the Creature's earlier death."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Scientific Paternalism
 * Viability: High. Victor could have raised the creature in isolation.
 * Suppression: Actively suppressed by Victor's "horror and disgust" (the shock of the Noose).
 * Evidence: "Now that I had finished, the beauty of the dream vanished."
 * * ALTERNATIVE 2: Acceptance of the Female Mate
 * Viability: Moderate. Victor started this but destroyed it.
 * Suppression: Suppressed by Victor's fear of "a race of devils."
 * Evidence: "I shuddered to think that future ages might curse me as their pest."
 * * CONCLUSION:
 * The failure to adopt Alternative 1 (Paternalism) is what definitively 
 * converts the biological constraint into an extractive Noose for the creator.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
