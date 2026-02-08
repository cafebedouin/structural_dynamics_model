% ============================================================================
% CONSTRAINT STORY: emergency_mode_lock_in
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(emergency_mode_lock_in, []).

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
 * * constraint_id: emergency_mode_lock_in
 * human_readable: The Perpetual Crisis Mandate
 * domain: political/organizational/sociological
 * * SUMMARY:
 * A scenario where a "Rope" designed for temporary crisis management (e.g., 
 * emergency powers, martial law protocols, or rapid-response technical overrides) 
 * becomes the permanent operating substrate. This coordination tool for 
 * "short-term survival" becomes a "Snare" for the subject, as their primary 
 * civil, legal, and operational agency is liquidated to maintain the 
 * "Emergency Mode," trapping them in a territory where the crisis is never 
 * declared "over" because the system now depends on the suspension of 
 * normal constraints to function.
 * * KEY AGENTS:
 * - Suspended Citizen: Subject (Powerless)
 * - Emergency Administrator: Beneficiary (Institutional)
 * - Constitutional Decay Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.91) reflects the total liquidation of normal-time rights 
% and agency to fuel the "efficiency" of the emergency mandate.
domain_priors:base_extractiveness(emergency_mode_lock_in, 0.91). 
domain_priors:suppression_score(emergency_mode_lock_in, 0.85). % Normal legal processes are suppressed as "existential risks" or "inefficiencies."
domain_priors:theater_ratio(emergency_mode_lock_in, 0.94).    % Extreme theater: "Sunset Review" sessions that performatively signal an end while finding new crises to justify extension.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(emergency_mode_lock_in, extractiveness, 0.91).
narrative_ontology:constraint_metric(emergency_mode_lock_in, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(emergency_mode_lock_in, theater_ratio, 0.94).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they live under "temporary" rules that have 
% defined their entire biographical horizon, liquidating their agency to 
% advocate for a return to normalcy.
constraint_indexing:constraint_classification(emergency_mode_lock_in, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The administrator views the lock-in as a Rope—the essential coordination 
% substrate for managing a complex, volatile world without the "drag" of 
% standard deliberative processes.
constraint_indexing:constraint_classification(emergency_mode_lock_in, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "Emergency Success Dashboard" 
% is an inertial spike; it signals safety while the subject's agency is permanently atrophied.
constraint_indexing:constraint_classification(emergency_mode_lock_in, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.91) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(emergency_mode_lock_in, E), E >= 0.50,
    domain_priors:suppression_score(emergency_mode_lock_in, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================= */

:- begin_tests(emergency_lock_in_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional administrator.
    constraint_indexing:constraint_classification(emergency_mode_lock_in, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_mode_lock_in, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(emergency_mode_lock_in, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(emergency_mode_lock_in, E),

    E > 0.70.

:- end_tests(emergency_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.91) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of crisis-response is achieved by liquidating the 
 * subject's primary capacity for normal-time governance.
 * * 
 *
 * * PERSPECTIVAL GAP:
 * The Suspended Citizen feels a Snare because the "temporary" measures 
 * have become a permanent cage for their rights. The Administrator 
 * sees a Rope because the lock-in coordinates an efficient, top-down 
 * social order that can pivot instantly to new threats.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Crisis Exit Roadmap" is no longer functional (Theater 0.94); 
 * it is an inert spike siphoning 0.91 of the societal agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_crisis_normalization,
    'Can "Algorithmic Sunsets" restore the Rope, or is emergency mode a physical law of complex hierarchies (Snare vs Mountain)?',
    'Tracking the half-life of emergency mandates in 2026-style digital-sovereignty zones.',
    'If mandates expire: Snare of current technique. If they persist: Mountain of Institutional Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(emergency_mode_lock_in, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
