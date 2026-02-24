% ============================================================================
% CONSTRAINT STORY: the_novelty_gradient
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_novelty_harvest, []).

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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: the_novelty_gradient
 * human_readable: The Novelty Harvesting Gradient
 * domain: economic/behavioral
 * * SUMMARY:
 * This constraint represents the "extraction of delta." As a user adapts 
 * to a product or environment, their satisfaction drops. The system 
 * (The Harvester) captures this drop by offering a "New Solution," 
 * turning a biological necessity (adaptation) into an economic engine.
 * * KEY AGENTS:
 * - The User: Subject (Powerless). Locked in the decay-cycle of satisfaction.
 * - The Growth Engine: Beneficiary (Institutional). Requires the decay to drive sales.
 * - The Behavioral Ethicist: Auditor (Analytical). Evaluates the "Treadmill" speed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.65) is extremely high because the system 
% does not create value; it simply refills a "bucket" that it knows will leak 
% due to the user's biology.
domain_priors:base_extractiveness(the_novelty_gradient, 0.65). 
domain_priors:suppression_score(the_novelty_gradient, 0.78).   % High: Escaping "consumerist novelty" requires radical lifestyle exit.
domain_priors:theater_ratio(the_novelty_gradient, 0.82).      % Piton: The "New!" label is often theatrical maintenance of an old core.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(the_novelty_gradient, extractiveness, 0.65).
narrative_ontology:constraint_metric(the_novelty_gradient, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(the_novelty_gradient, theater_ratio, 0.82).
domain_priors:requires_active_enforcement(the_novelty_gradient).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE USER (SNARE)
% The user feels "stale" or "behind." The constraint acts as a Snare that 
% pulls them toward the next purchase or update to regain the baseline.
constraint_indexing:constraint_classification(the_novelty_gradient, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE GROWTH ENGINE (ROPE)
% To the corporation, this is a Rope. It coordinates massive industrial 
% pipelines to ensure a steady "flow" of innovation to meet "demand."
constraint_indexing:constraint_classification(the_novelty_gradient, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE BEHAVIORAL ETHICIST (TANGLED ROPE)
% Viewed analytically, the "innovation" (Rope) is structurally dependent 
% on the "planned obsolescence of satisfaction" (Snare).
constraint_indexing:constraint_classification(the_novelty_gradient, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(the_novelty_gradient, E), E > 0.50.

% PERSPECTIVE 4: THE SYSTEM ARCHAEOLOGIST (PITON)
% When a product category stops innovating but continues to use "Novelty" 
% marketing, it becomes a Piton. The constraint of "The Upgrade" is kept 
% alive purely through institutional theater.
constraint_indexing:constraint_classification(the_novelty_gradient, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(the_novelty_gradient, TR), TR > 0.80.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(novelty_gradient_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_novelty_gradient, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_novelty_gradient, rope, context(agent_power(institutional), _, _, _)).

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_novelty_gradient, E),

    E >= 0.46. % Verification of the "Harvesting" intensity.

:- end_tests(novelty_gradient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Harvesting" occurs at the intersection of the Adaptation Mountain 
 * and the Institutional Rope. The extraction score (0.65) reflects that the 
 * system "captures" the byproduct of biological decay (boredom/habituation) 
 * and converts it into financial flow. 
 * The Perspectival Gap exists because the Institution sees a "Solution" 
 * whereas the User is experiencing a "Compulsion."
 *
 * MANDATROPHY ANALYSIS:
 * This is a classic Tangled Rope. If the system stopped providing "Novelty," 
 * the user would eventually adapt to "None," which is a state of psychological 
 * homeostasis. The "Rope" of innovation prevents this homeostasis to 
 * maintain the "Snare" of consumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_harvest_limit,
    'Is there a biological "ceiling" where novelty harvesting results in total psychic burnout (non-adaptation)?',
    'Clinical data on "Dopamine Fasting" efficacy vs. traditional consumption.',
    'If a ceiling exists, the Snare becomes a Scaffold (Temporary Exploitation).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_novelty_gradient, 0, 100). % Measured in "Delta of Satisfaction."

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
