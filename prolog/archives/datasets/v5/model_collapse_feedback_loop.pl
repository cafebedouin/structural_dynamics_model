% ============================================================================
% CONSTRAINT STORY: model_collapse_feedback_loop
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(model_collapse_feedback_loop, []).

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
 * * constraint_id: model_collapse_feedback_loop
 * human_readable: The Autophagous Intelligence Trap
 * domain: technological/AI/informational
 * * SUMMARY:
 * A scenario where AI models are recursively trained on the output of their 
 * predecessors, leading to a progressive "flattening" of information. 
 * This "Rope" for infinite, low-cost data scaling becomes a "Snare" for the 
 * user as the system liquidates the rare, diverse "ground truth" signals, 
 * trapping the model in a terminal loop of regression toward the mean and 
 * high-confidence hallucinations.
 * * KEY AGENTS:
 * - Independent Researcher: Subject (Powerless)
 * - Frontier Labs: Beneficiary (Institutional)
 * - Information Informationist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.89) reflects the parasitic liquidation of the 
% original informational diversity to maintain the "scaling" metric.
domain_priors:base_extractiveness(model_collapse_feedback_loop, 0.89). 
domain_priors:suppression_score(model_collapse_feedback_loop, 0.77). % Human-original data is increasingly suppressed by the sheer volume of synthetic noise.
domain_priors:theater_ratio(model_collapse_feedback_loop, 0.84).    % High theater: "Fine-tuning" metrics that claim quality while variance collapses.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(model_collapse_feedback_loop, extractiveness, 0.89).
narrative_ontology:constraint_metric(model_collapse_feedback_loop, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(model_collapse_feedback_loop, theater_ratio, 0.84).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The researcher is trapped: they must use the latest models for work, but 
% the models have lost the ability to represent edge cases or novel logic.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The lab views the loop as a Rope—the only way to coordinate the training 
% of massive models without the bottleneck of expensive human data curation.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.84) > 0.70 triggers Piton: the "Benchmark Scores" are an 
% inertial spike; they show performance stability while the underlying logic decays.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) masking as functional scaling (Rope).
constraint_indexing:constraint_classification(model_collapse_feedback_loop, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(model_collapse_feedback_loop, E), E >= 0.50,
    domain_priors:suppression_score(model_collapse_feedback_loop, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_collapse_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless researcher vs Rope for the institutional lab.
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.84) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(model_collapse_feedback_loop, E),

    E > 0.70.

:- end_tests(model_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the 
 * "coordination" of data scaling is achieved by liquidating the informational 
 * territory of the model.
 * 
 * * PERSPECTIVAL GAP:
 * The Researcher feels a Snare because the tools they rely on are becoming 
 * "lobotomized" by their own popularity. The Frontier Lab sees a Rope 
 * because synthetic data coordinates the push toward larger parameter counts.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Scaling Law" is no longer functional (Theater 0.84); 
 * it is an inert spike siphoning 0.89 of the species' truth-seeking agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_collapse_irreversibility,
    'Can "poisoning" detection restore the Rope, or is informational entropy absolute (Snare vs Mountain)?',
    'Tracking the perplexity delta between models with 5% vs 95% synthetic data inclusion.',
    'If perplexity explodes: Mountain of Entropy. If it stabilizes: Snare of current technique.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(model_collapse_feedback_loop, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
