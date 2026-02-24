% ============================================================================
% CONSTRAINT STORY: invisible_infrastructure_dependency
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(invisible_infrastructure_dependency, []).

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
 * * constraint_id: invisible_infrastructure_dependency
 * human_readable: The Submerged Substrate Trap
 * domain: technological/logistical/economic
 * * SUMMARY:
 * A scenario where a critical, ubiquitous service (Rope) is so deeply integrated 
 * into the foundation of daily operations that it becomes "invisible" to the 
 * subject. This coordination tool becomes a "Snare" for the user, as their 
 * fundamental survival or operational agency is liquidated into a dependency 
 * they can neither perceive nor replace, trapping them in a territory where 
 * a single failure in the hidden substrate causes a total, unaddressable 
 * systemic collapse.
 * * KEY AGENTS:
 * - End User: Subject (Powerless)
 * - Substrate Monopolist: Beneficiary (Institutional)
 * - Infrastructure Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the siphoning of the subject's autonomy 
% to maintain a dependency that has no visible exit options.
domain_priors:base_extractiveness(invisible_infrastructure_dependency, 0.87). 
domain_priors:suppression_score(invisible_infrastructure_dependency, 0.82). % Alternative infrastructures are suppressed by the "invisible" nature of the primary.
domain_priors:theater_ratio(invisible_infrastructure_dependency, 0.91).    % Extreme theater: "Reliability Reports" that performatively signal stability while hiding structural debt.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, extractiveness, 0.87).
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: they are dependent on a system they don't even 
% know exists, liquidating their ability to prepare for its failure.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The monopolist views the invisibility as a Rope—the essential coordination 
% substrate for providing a frictionless, standardized global utility.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Official Status Page" 
% is an inertial spike; it performatively signals "Green" status while 0.87 extraction continues.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(invisible_infrastructure_dependency, E), E >= 0.50,
    domain_priors:suppression_score(invisible_infrastructure_dependency, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(invisible_infrastructure_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional monopolist.
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(invisible_infrastructure_dependency, E),

    E > 0.70.

:- end_tests(invisible_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a universal substrate is achieved by liquidating the 
 * subject's primary capacity for sovereign resilience.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The End User feels a Snare because they are blind to the gears of their 
 * own survival. The Monopolist sees a Rope because the hidden nature 
 * coordinates a massive, frictionless scale of operation.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Transparency Dashboard" is no longer functional (Theater 0.91); 
 * it is an inert spike siphoning 0.87 of the human species' autonomy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_infrastructure_visibility,
    'Can "Public Auditing" restore the Rope, or is invisibility a physical law of complex systems (Snare vs Mountain)?',
    'Tracking the impact of mandatory open-source requirements on hidden infrastructure layers in 2026.',
    'If auditing restores agency: Snare of current design. If systems remain opaque: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(invisible_infrastructure_dependency, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
