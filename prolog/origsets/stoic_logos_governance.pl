% ============================================================================
% CONSTRAINT STORY: stoic_logos_governance
% ============================================================================
% Generated: January 24, 2026
% Model: Gemini 2.0 Flash
% Source: Marcus Aurelius, "Meditations" (Roman Stoicism)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(stoic_logos_governance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: stoic_logos_governance
 * human_readable: The Stoic Logos/Imperial Framework
 * domain: philosophical/political/ethical
 * temporal_scope: 161–180 AD (Roman Empire)
 * spatial_scope: Global (Roman Empire and the Cosmos)
 * * SUMMARY:
 * The Stoic framework represents an internal/external constraint where the 
 * Emperor submits himself to the Logos. It coordinates the Pax Romana but 
 * extracts extreme personal autonomy from the ruler.
 * * KEY AGENTS:
 * - Marcus Aurelius (Institutional): Emperor bound by duty.
 * - The Citizen (Individual Powerless): Subjects bound by Law and Nature.
 * - The Logos (Metaphysical): The governing logic of the universe.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(stoic_logos_governance, 0, 10).

% CLAIM REPAIR: Changed from 'mountain' to 'tangled_rope' to avoid False Mountain Alert.
% While it feels unchangeable to the agent, it is an institutionally maintained coordination system.
narrative_ontology:constraint_claim(stoic_logos_governance, tangled_rope).

% Base Properties
domain_priors:base_extractiveness(stoic_logos_governance, 0.75).
domain_priors:suppression_score(stoic_logos_governance, 0.80).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(stoic_logos_governance, extractiveness, 0.75).
narrative_ontology:constraint_metric(stoic_logos_governance, suppression_requirement, 0.8).
domain_priors:requires_active_enforcement(stoic_logos_governance).

% Mandatory Asymmetry Hooks
constraint_beneficiary(stoic_logos_governance, roman_state).
constraint_victim(stoic_logos_governance, marcus_personal_will).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EMPEROR (MARCUS) - SNARE
   --------------------------------------------------------------------------
   WHO: institutional - Highest power but personally "trapped" by duty.
   WHY: The office extracts his vitality for the sake of the state.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(stoic_logos_governance, snare, 
    context(agent_power(institutional), time_horizon(biographical), exit_options(trapped), spatial_scope(global))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SUBJECT / INDIVIDUAL - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: powerless - Subject to law and mortality.
   WHY: Death and the movement of the empire are seen as unchangeable nature.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(stoic_logos_governance, mountain, 
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - TANGLED ROPE
   --------------------------------------------------------------------------
   WHO: analytical - Observer stance.
   WHY: Coordinates millions while hiding high levels of self-extraction.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(stoic_logos_governance, tangled_rope, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :-
    domain_priors:base_extractiveness(stoic_logos_governance, E), E > 0.4,
    domain_priors:suppression_score(stoic_logos_governance, S), S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (Logic Fixed)
   ========================================================================== */

:- begin_tests(stoic_logos_governance_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, Type1, 
        context(agent_power(institutional), time_horizon(biographical), exit_options(trapped), spatial_scope(global))),
    constraint_indexing:constraint_classification(stoic_logos_governance, Type2, 
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))),
    % FIXED: Changed = to \= to ensure variance is detected
    Type1 \= Type2.

test(power_extractiveness_scaling) :-
    domain_priors:base_extractiveness(stoic_logos_governance, Score),

    Score > 0.5.

test(time_immutability_nature) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, mountain, 
        context(agent_power(powerless), time_horizon(immediate), _, _)).

:- end_tests(stoic_logos_governance_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

omega_variable(
    stoic_logos_governance_extraction_intent,
    'Is the 0.75 extraction a functional necessity for Rome or a self-imposed psychological Snare?',
    resolution_mechanism('Comparative analysis of Roman stability under Stoic vs Tyrannical Emperors'),
    impact('If necessity: Mountain. If choice: Snare.'),
    confidence_without_resolution(medium)
).

omega_variable(
    marcus_sincerity,
    'Was the \'Meditations\' private resilience or performative dialogue?',
    resolution_mechanism('Psychometric analysis of text structure vs historical actions'),
    impact('If performative: Rope. If private: Snare.'),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Epicureanism (suppressed) and Tyranny (rejected as 'becoming a beast').
 * Presence of these alternatives confirms the system is not a true Mountain (natural law)
 * but a Tangled Rope maintained through extreme discipline.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% TO USE: ?- [stoic_logos_governance].
% RUN TESTS: ?- run_tests(stoic_logos_governance_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
