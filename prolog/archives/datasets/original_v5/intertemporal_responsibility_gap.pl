% ============================================================================
% CONSTRAINT STORY: intertemporal_responsibility_gap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(intertemporal_responsibility_gap, []).

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
 * * constraint_id: intertemporal_responsibility_gap
 * human_readable: The Generational Accountability Void
 * domain: environmental/economic/technological
 * * SUMMARY:
 * A scenario where the benefits of a specific action (e.g., carbon extraction, 
 * rapid AI scaling, national debt) are captured in the immediate term by 
 * current actors, while the costs and systemic risks are deferred to future 
 * generations. This "Rope" for current coordination and wealth creation 
 * becomes a "Snare" for the future subject, whose survival agency is 
 * liquidated by inherited ecological or economic debt, trapping them in 
 * a territory they cannot repair and did not choose.
 * * KEY AGENTS:
 * - Future Citizen: Subject (Powerless)
 * - Current Decision-Maker: Beneficiary (Institutional)
 * - Intergenerational Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.92) reflects the total liquidation of future subjects' 
% resources and agency to maintain current institutional standards.
domain_priors:base_extractiveness(intertemporal_responsibility_gap, 0.92). 
domain_priors:suppression_score(intertemporal_responsibility_gap, 0.85). % Future interests are suppressed by lack of present representation.
domain_priors:theater_ratio(intertemporal_responsibility_gap, 0.88).    % High theater: "Legacy planning" and "Greenwashing" rituals masking terminal extraction.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, extractiveness, 0.92).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, theater_ratio, 0.88).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The future citizen is trapped: they inherit a hollowed-out territory 
% with terminal debt, liquidating their ability to pursue novel goals.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The current actor views the gap as a Rope—the essential coordination 
% substrate for maintaining present stability and competitive growth.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "Long-Term Sustainability" 
% board is an inertial spike; it signals concern while permitting 0.92 extraction.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects extreme extraction (0.92) across time horizons as a hybrid.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(intertemporal_responsibility_gap, E), E >= 0.50,
    domain_priors:suppression_score(intertemporal_responsibility_gap, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intertemporal_responsibility_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless future vs Rope for the current institution.
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % Extraction (0.92) > 0.70 triggers [RESOLVED MANDATROPHY].
    domain_priors:base_extractiveness(intertemporal_responsibility_gap, E),

    E > 0.70.

:- end_tests(intertemporal_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.92) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of current growth is achieved by liquidating 
 * the entire resource and agency base of the future species.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Future Citizen feels a Snare because they are born into a "bankruptcy" 
 * they did not sign for. The Current Decision-Maker sees a Rope because 
 * deferred costs are the only coordination mechanism that prevents 
 * immediate social collapse or stagnation.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, "Legacy Planning" is no longer functional for species 
 * continuity (Theater 0.88); it is an inert spike siphoning 0.92 of 
 * future territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_intergenerational_legal_standing,
    'Can the "future" ever be granted a present legal Rope, or is time an absolute Snare (Snare vs Mountain)?',
    'Tracking the success rate of "Nature-as-Subject" or "Future Generations" lawsuits in national courts.',
    'If lawsuits win: Snare of current legal design. If they fail: Mountain of Temporal Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(intertemporal_responsibility_gap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
