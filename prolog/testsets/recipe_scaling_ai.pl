% ============================================================================
% CONSTRAINT STORY: RECIPE_SCALING_AI
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_recipe_scaling, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: recipe_scaling_ai
 * human_readable: The NYT Cooking Generative Scaling Constraint
 * domain: technological/culinary
 * * SUMMARY:
 * A generative AI tool that automates recipe doubling/halving. While it
 * removes the 'mental math' barrier, it introduces 'untested' risks regarding
 * thermodynamics and physical vessel limits.
 * * KEY AGENTS:
 * - The Home Cook: Subject (Powerless against 'orange text' instructions).
 * - NYT Cooking Engineers: Beneficiary (Institutional coordination providers).
 * - Recipe Editors: Auditors (Analytical guardians of 'common sense').
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is 0.49 because the system shifts the risk of 'untested' failure
% onto the user's 'instincts'.
domain_priors:base_extractiveness(recipe_scaling_ai, 0.49).
domain_priors:suppression_score(recipe_scaling_ai, 0.25).
domain_priors:theater_ratio(recipe_scaling_ai, 0.10).

% Binary flags
domain_priors:requires_active_enforcement(recipe_scaling_ai). % User must 'taste and adjust'.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE NOVICE COOK (SNARE)
% Without 'decades of experience,' the scaled-but-untested recipe is a trap.
constraint_indexing:constraint_classification(recipe_scaling_ai, snare,
    context(agent_power(individual_powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BUSY HOST (ROPE)
% A tool that effectively removes 'mental math' to solve coordination.
constraint_indexing:constraint_classification(recipe_scaling_ai, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CULINARY ANALYST (TANGLED ROPE)
% Recognizes that simple arithmetic is 'inexact science' in cooking.
constraint_indexing:constraint_classification(recipe_scaling_ai, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(recipe_scaling_ai, E), E >= 0.46.

% PERSPECTIVE 4: THE BETA FEATURE (SCAFFOLD)
% The feature is 'constantly working to improve,' implying a temporary state.
constraint_indexing:constraint_classification(recipe_scaling_ai, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(recipe_scaling_ai).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(recipe_scaling_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(recipe_scaling_ai, snare, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(recipe_scaling_ai, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(recipe_scaling_ai, E),
    E >= 0.46. % Triggers high-extraction logic requirements.

:- end_tests(recipe_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.49) reflects the 'untested' nature of the output.
 * While the AI provides the 'math,' it extracts the user's cognitive labor
 * to monitor 'doneness cues' and 'vessel capacity'.
 * * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * This is a 'Tangled Rope' because the coordination benefit (25,000+ recipes
 * scaled) outweighs the extraction of human oversight, provided the user
 * ignores the 'math alone' and uses 'common sense'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_gastronomy,
    'Can generative AI reliably compute thermal mass and evaporation rates?',
    'A/B testing of AI-scaled recipes against human-tested control groups.',
    'True = Scaling becomes a Mountain (Law); False = Remains a permanent Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(recipe_scaling_ai, 2025, 2026).
narrative_ontology:has_sunset_clause(recipe_scaling_ai). % AI is 'constantly improving'.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
