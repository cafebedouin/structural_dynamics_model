% ============================================================================
% CONSTRAINT STORY: fragile_middle_layer_collapse
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(fragile_middle_layer_collapse, []).

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
 * * constraint_id: fragile_middle_layer_collapse
 * human_readable: The Intermediary Decay
 * domain: economic/technological/logistical
 * * SUMMARY:
 * A scenario where the "middle layer" of a system (e.g., regional distributors, 
 * human moderators, local maintenance contractors) is hollowed out by 
 * automation and direct-to-end-user scaling. This "Rope" for achieving 
 * global cost-efficiency becomes a "Snare" during a local failure, as 
 * the subject is trapped in a territory where the specialized human 
 * agency required to resolve the failure has been liquidated. The "Efficient" 
 * system collapses into a binary state of "Working" or "Terminal Failure" 
 * with no middle-tier buffer to provide repair or nuance.
 *
 * * KEY AGENTS:
 * - Local End-User: Subject (Powerless)
 * - Global Platform: Beneficiary (Institutional)
 * - Resilience Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the middle layer's 
% maintenance surplus into the global platform's margin.
domain_priors:base_extractiveness(fragile_middle_layer_collapse, 0.86). 
domain_priors:suppression_score(fragile_middle_layer_collapse, 0.73). % Local repair alternatives are suppressed by proprietary software/legal locks.
domain_priors:theater_ratio(fragile_middle_layer_collapse, 0.84).    % High theater: "Self-service help centers" that mask the lack of actual human support.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, extractiveness, 0.86).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, theater_ratio, 0.84).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The end-user is trapped: when the automated system fails, there is no 
% "middle" human to call, liquidating their agency to restore service.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the collapse as a Rope—the essential coordination 
% substrate for removing "human friction" and achieving perfect price scaling.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.84) > 0.70 triggers Piton: the "Community Support" 
% dashboard is an inertial spike; it performatively signals care while the 
% middle layer has been systematically liquidated.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(fragile_middle_layer_collapse, E), E >= 0.50,
    domain_priors:suppression_score(fragile_middle_layer_collapse, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fragile_middle_layer_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.84) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(fragile_middle_layer_collapse, E),

    E > 0.70.

:- end_tests(fragile_middle_layer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of direct scaling is achieved by liquidating the 
 * repair agency of the system's human middle layer.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Local End-User feels a Snare because they have lost the human 
 * interface that once mediated between them and the machine. The Global 
 * Platform sees a Rope because the removal of the middle layer coordinates 
 * a massive reduction in operational overhead.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Automated Trouble-Ticket" system is no longer functional 
 * for resilience (Theater 0.84); it is an inert spike siphoning 0.86 of the 
 * system's adaptive capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_middle_layer_reconstruction,
    'Can AI "Agents" simulate the middle layer Rope, or is repair a biological "Snare" (Snare vs Mountain)?',
    'Tracking the success rate of autonomous agents in resolving high-context "out-of-distribution" local failures.',
    'If AI succeeds: Snare of current software design. If AI fails: Mountain of Human Tacit Knowledge.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fragile_middle_layer_collapse, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
