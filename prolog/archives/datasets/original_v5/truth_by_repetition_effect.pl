% ============================================================================
% CONSTRAINT STORY: truth_by_repetition_effect
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(truth_by_repetition_effect, []).

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
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: truth_by_repetition_effect
 * human_readable: The Illusory Truth Loop
 * domain: cognitive/informational/political
 * * SUMMARY:
 * A scenario where the biological heuristic of "fluency" (judging truth by 
 * familiarity) is weaponized by high-frequency digital distribution. 
 * This "Rope" for processing frequent signals efficiently becomes a 
 * "Snare" for the subject, as unverified or false information is 
 * recursively repeated across multiple channels, liquidating the subject's 
 * critical reasoning and trapping them in a state where "truth" is 
 * indistinguishable from "exposure."
 * * KEY AGENTS:
 * - Information Recipient: Subject (Powerless)
 * - Narrative Saturation Engine: Beneficiary (Institutional)
 * - Epistemic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the siphoning of the subject's 
% primary logic faculties to maintain the "fluency" of the narrative.
domain_priors:base_extractiveness(truth_by_repetition_effect, 0.87). 
domain_priors:suppression_score(truth_by_repetition_effect, 0.76). % Unfamiliar truth is suppressed by high-fluency noise.
domain_priors:theater_ratio(truth_by_repetition_effect, 0.90).    % Extreme theater: performative "Fact Checks" that only reinforce the familiar lie.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(truth_by_repetition_effect, extractiveness, 0.87).
narrative_ontology:constraint_metric(truth_by_repetition_effect, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(truth_by_repetition_effect, theater_ratio, 0.9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The recipient is trapped: their biological brain is optimized to trust 
% what it hears most often, liquidating their agency to resist the flood.
constraint_indexing:constraint_classification(truth_by_repetition_effect, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The engine views repetition as a Rope—the only way to coordinate mass 
% belief and achieve social "consensus" in a fragmented information world.
constraint_indexing:constraint_classification(truth_by_repetition_effect, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.90) > 0.70 triggers Piton: the "Official Source" label 
% is an inertial spike; it signals authority based on frequency, not fact.
constraint_indexing:constraint_classification(truth_by_repetition_effect, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(truth_by_repetition_effect, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(truth_by_repetition_effect, E), E >= 0.50,
    domain_priors:suppression_score(truth_by_repetition_effect, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(truth_by_repetition_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless recipient vs Rope for the institutional engine.
    constraint_indexing:constraint_classification(truth_by_repetition_effect, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(truth_by_repetition_effect, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.90) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(truth_by_repetition_effect, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(truth_by_repetition_effect, E),

    E > 0.70.

:- end_tests(truth_by_repetition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" of social truth is achieved by liquidating the subject's 
 * primary critical faculty.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Information Recipient feels a Snare because their brain is 
 * physiologically "tricked" into certainty. The Saturation Engine sees 
 * a Rope because the effect coordinates a stable, legible reality 
 * for the population.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, "Common Knowledge" is no longer functional (Theater 0.90); 
 * it is an inert spike siphoning 0.87 of the species' collective agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_fluency_correction,
    'Can "forced pause" interventions break the Snare, or is the heuristic biological (Snare vs Mountain)?',
    'Tracking the belief-delta in high-repetition environments with 30-second logic delays.',
    'If belief drops: Snare of current interface. If belief holds: Mountain of Human Psychology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(truth_by_repetition_effect, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
