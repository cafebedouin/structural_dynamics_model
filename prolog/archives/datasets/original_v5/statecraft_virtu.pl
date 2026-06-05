% ============================================================================
% CONSTRAINT STORY: statecraft_virtu
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Prince by Nicolo Machiavelli
% ============================================================================

:- module(constraint_statecraft_virtu, []).

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
 * * constraint_id: statecraft_virtu
 * human_readable: Machiavellian Virtù and State Maintenance
 * domain: political/social
 * temporal_scope: Renaissance Italy (c. 1469-1527)
 * spatial_scope: Regional (Italian City-States/European Principalities)
 * * SUMMARY:
 * Statecraft is a series of strategic constraints imposed by a Prince to 
 * maintain power and order amidst the "raging river" of Fortune. It involves 
 * the calculated use of law and force (the lion and the fox) to coordinate 
 * subjects and neutralize enemies.
 * * KEY AGENTS:
 * - The New Prince: The institutional agent who must build "foundations" from scratch.
 * - The People: The powerless/moderate subjects who desire only "not to be oppressed."
 * - The Nobles: Powerful individuals who seek to dominate the Prince or the People.
 * - Fortune (Fortuna): The environmental "Mountain" constraint that represents 
 * irreducible variance in human affairs.
 * * NARRATIVE ARC:
 * The text functions as a manual for converting the chaos of "Fortune" into the 
 * order of a "State." It moves from the acquisition of power (Chapter I-VII) 
 * to the maintenance of power (Chapter XV-XIX), culminating in a call for 
 * national liberation (Chapter XXVI).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(statecraft_virtu_analysis, 0, 10).
narrative_ontology:constraint_claim(statecraft_virtu, rope).

% Base extractiveness score: High (0.7)
% Rationale: Machiavelli explicitly argues that "men ought either to be well 
% treated or crushed." Maintaining a state extracts obedience and resources 
% often through asymmetric force.
domain_priors:base_extractiveness(statecraft_virtu, 0.7).

% Suppression score: High (0.9)
% Rationale: The Prince must suppress rivals, factions, and alternative centers 
% of power (like mercenaries or powerful barons) to ensure stability.
domain_priors:suppression_score(statecraft_virtu, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(statecraft_virtu, extractiveness, 0.7).
narrative_ontology:constraint_metric(statecraft_virtu, suppression_requirement, 0.9).

% Enforcement: Requires active enforcement through "good arms" and "good laws."
domain_priors:requires_active_enforcement(statecraft_virtu).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(statecraft_virtu, [the_prince, state_stability]).
constraint_victim(statecraft_virtu, [dispossessed_lords, "unarmed prophets", rival_factions]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEW PRINCE - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical/biographical
   WHERE: arbitrage
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To the Prince, the state is a "Rope"—a functional coordination mechanism 
   that he can and must manipulate. He has the agency to change laws, 
   form alliances, and manage the "spirit of the times."
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    statecraft_virtu,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(statecraft_virtu, the_prince),
        constraint_victim(statecraft_virtu, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PEOPLE - Mountain
   --------------------------------------------------------------------------
   WHO: powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the average citizen, the Prince's rule and the laws of the state 
   appear as "Mountains"—immutable facts of life. They have zero degrees 
   of freedom to change the sovereign and generally view his power as a 
   natural law, provided they are not directly despoiled.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    statecraft_virtu,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(statecraft_virtu, state_stability),
        constraint_victim(statecraft_virtu, []),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(statecraft_virtu, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISPOSSESSED BARON - Snare
   --------------------------------------------------------------------------
   WHO: powerful
   WHEN: immediate
   WHERE: constrained
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   To the rival lord whom the Prince has "extinguished," the constraint 
   system is a "Snare." It is a coercive, asymmetric mechanism designed 
   to extract his power and life to benefit the Prince's consolidation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    statecraft_virtu,
    snare,
    context(
        agent_power(powerful),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(statecraft_virtu, the_prince),
        constraint_victim(statecraft_virtu, dispossessed_lords),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(statecraft_virtu, E),
    E > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(statecraft_virtu_tests).

test(perspectival_gap_analysis) :-
    % The Prince (Institutional) sees a Rope; the People (Powerless) see a Mountain.
    constraint_indexing:constraint_classification(statecraft_virtu, rope, context(agent_power(institutional), _, _, _, _, _)),
    constraint_indexing:constraint_classification(statecraft_virtu, mountain, context(agent_power(powerless), _, _, _, _, _)).

test(extraction_rule) :-
    % Crushing enemies is a high-extraction event (Snare).
    constraint_indexing:constraint_classification(statecraft_virtu, snare, context(agent_power(powerful), _, constrained, _, _, _)).

test(fortune_environmental_constraint) :-
    % Fortune acts as an immutable Mountain for all who do not prepare.
    true.

:- end_tests(statecraft_virtu_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * * The central insight of Machiavelli's work is the conversion of 
 * environmental constraints (Fortune) into social constraints (State). 
 * * Fortune is the ultimate "Mountain"—a raging river with zero degrees 
 * of freedom for the unprepared. 
 * * The "Prince" uses his Virtù to build "dykes and dams" (foundations), 
 * effectively creating a "Rope" (a functional coordination mechanism) 
 * for himself and a "Snare" for those who would oppose the new order.
 * * The extractiveness is intentionally high (0.7) because the system 
 * prioritizes the "security and prosperity" of the state over individual 
 * agency, particularly for rival power centers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prince_competence_ratio,
    "The ratio of the Prince's skill (Virtù) to the randomness of external events (Fortune).",
    resolution_mechanism("Long-term historical survival of the state under pressure."),
    impact("If too low: The Rope of the State snaps, reverting the People to a Snare of war."),
    confidence_without_resolution(low)
).

omega_variable(
    mercenary_fidelity,
    "The unpredictable breaking point of a mercenary's 'simulated' coordination.",
    resolution_mechanism("The moment of actual military engagement (e.g., the battle of Vaila)."),
    impact("If zero: The coordination mechanism is actually a Snare for the Prince hiring them."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Mercenary Arms
 * Viability: The standard practice in Renaissance Italy.
 * Suppression: Rejected by Machiavelli as "useless and dangerous."
 * Evidence: Chapter XII-XIII.
 * * ALTERNATIVE 2: Republics
 * Viability: Successfully maintained in Rome and formerly in Florence.
 * Suppression: Explicitly excluded from the scope of this particular 
 * manual to focus on the unique constraints of Principalities.
 * Evidence: Chapter II.
 * * CONCLUSION:
 * The existence of alternatives like "National Arms" vs "Mercenary Arms" 
 * fundamentally changes the constraint classification. A Prince relying on 
 * mercenaries is caught in a Snare; a Prince with his own army holds a Rope.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
