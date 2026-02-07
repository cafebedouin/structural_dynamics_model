% ============================================================================
% CONSTRAINT STORY: narrative_overfitting
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(narrative_overfitting, []).

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
 * * constraint_id: narrative_overfitting
 * human_readable: The Procrustean Plot
 * domain: social/technological/psychological
 * * SUMMARY:
 * A scenario where complex real-world data is forced into a simple, compelling 
 * narrative structure to satisfy engagement algorithms or cognitive biases. 
 * This "Rope" for making information legible and sharable becomes a "Snare" 
 * as the nuances of reality are liquidated to fit the story arc, trapping 
 * the subject in a "fictionalized" territory where functional decisions 
 * become impossible because the map has been optimized for "likability" 
 * rather than accuracy.
 * * KEY AGENTS:
 * - Public Participant: Subject (Powerless)
 * - Narrative Architect (Media/AI): Beneficiary (Institutional)
 * - Epistemic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the terminal liquidation of objective nuance 
% to feed the institutional need for viral "storytelling."
domain_priors:base_extractiveness(narrative_overfitting, 0.87). 
domain_priors:suppression_score(narrative_overfitting, 0.76). % Contradictory evidence is suppressed as "plot holes."
domain_priors:theater_ratio(narrative_overfitting, 0.94).    % Extreme theater: performative "story arcs" masking systemic complexity.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(narrative_overfitting, extractiveness, 0.87).
narrative_ontology:constraint_metric(narrative_overfitting, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(narrative_overfitting, theater_ratio, 0.94).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The participant is trapped: they must interpret their life through the 
% "overfitted" narrative to remain socially legible, liquidating their 
% primary sensory agency.
constraint_indexing:constraint_classification(narrative_overfitting, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the narrative as a Rope—the only way to coordinate 
% millions of attention-spans in a world of infinite, noisy data.
constraint_indexing:constraint_classification(narrative_overfitting, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "Viral Story" is an 
% inertial spike; it remains "true" only because it is too simple to fail.
constraint_indexing:constraint_classification(narrative_overfitting, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(narrative_overfitting, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(narrative_overfitting, E), E >= 0.50,
    domain_priors:suppression_score(narrative_overfitting, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_overfitting_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless subject vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(narrative_overfitting, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_overfitting, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(narrative_overfitting, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(narrative_overfitting, E),

    E > 0.70.

:- end_tests(narrative_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" of social meaning is achieved by liquidating the 
 * informational complexity of reality.
 * * 
 * * * PERSPECTIVAL GAP:
 * The Public Participant feels a Snare because their reality is hollowed out 
 * into a series of tropes. The Narrative Architect sees a Rope because 
 * the "story" is the only coordinate system that works for mass mobilization.
 * * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Compelling Story" is no longer functional (Theater 0.94); 
 * it is an inert spike siphoning 0.87 of the species' collective agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_complexity_threshold,
    'Can a society survive the loss of nuance, or is overfitting a terminal "Mountain" (Snare vs Mountain)?',
    'Tracking the failure rate of policy decisions based on "overfitted" viral narratives.',
    'If policies fail: Snare of current media. If they hold: Mountain of Social Simplification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(narrative_overfitting, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
