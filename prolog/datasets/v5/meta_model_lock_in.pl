% ============================================================================
% CONSTRAINT STORY: meta_model_lock_in
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(meta_model_lock_in, []).

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
 * * constraint_id: meta_model_lock_in
 * human_readable: The Ontological Cage
 * domain: technological/cognitive/informational
 * * SUMMARY:
 * A scenario where a foundational AI model or classification framework (the Meta-Model) 
 * becomes so deeply integrated into social and technical infrastructure that 
 * alternative ways of perceiving or organizing reality are suppressed. This 
 * "Rope" for achieving global semantic interoperability becomes a "Snare" for 
 * the user, as the system liquidates the agency to use non-standard logic or 
 * novel ontologies, trapping the subject in a territory where only "model-legible" 
 * thoughts can be processed or economically rewarded.
 * * KEY AGENTS:
 * - Independent Thinker: Subject (Powerless)
 * - Model Provider: Beneficiary (Institutional)
 * - Epistemic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of cognitive diversity to 
% maintain the "standardized" efficiency of the Meta-Model.
domain_priors:base_extractiveness(meta_model_lock_in, 0.86). 
domain_priors:suppression_score(meta_model_lock_in, 0.79). % Divergent logic is suppressed as "incompatible" or "noise."
domain_priors:theater_ratio(meta_model_lock_in, 0.88).    % High theater: "Fine-tuning" options that mask the rigid underlying ontology.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(meta_model_lock_in, extractiveness, 0.86).
narrative_ontology:constraint_metric(meta_model_lock_in, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(meta_model_lock_in, theater_ratio, 0.88).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The thinker is trapped: they must use the Meta-Model to communicate or 
% work, but doing so liquidates their primary intellectual agency.
constraint_indexing:constraint_classification(meta_model_lock_in, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Provider views the model as a Rope—the essential coordination 
% substrate for global-scale human-AI alignment and data liquidity.
constraint_indexing:constraint_classification(meta_model_lock_in, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "Custom Personalization" 
% settings are an inertial spike; they performatively suggest agency while 
% 0.86 extraction occurs.
constraint_indexing:constraint_classification(meta_model_lock_in, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(meta_model_lock_in, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(meta_model_lock_in, E), E >= 0.50,
    domain_priors:suppression_score(meta_model_lock_in, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_model_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless thinker vs Rope for the institutional provider.
    constraint_indexing:constraint_classification(meta_model_lock_in, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_model_lock_in, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(meta_model_lock_in, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(meta_model_lock_in, E),

    E > 0.70.

:- end_tests(meta_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a universal model is achieved by liquidating 
 * the subject's primary capacity for independent ontology.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Independent Thinker feels a Snare because their unique insights are 
 * effectively "deleted" by the model's filters. The Provider sees a Rope 
 * because the model coordinates a coherent, actionable reality for 
 * billions of users and billions of dollars in AI-automated trade.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Model Customization" feature is no longer functional (Theater 0.88); 
 * it is an inert spike siphoning 0.86 of the species' cognitive surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_model_plasticity,
    'Can "open-source" weights restore the Rope, or is lock-in a physical law of network effects (Snare vs Mountain)?',
    'Tracking the success rate of non-standard logic systems in maintaining economic viability in 2026.',
    'If logic persists: Snare of current tech. If it fails: Mountain of Information Economics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(meta_model_lock_in, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
