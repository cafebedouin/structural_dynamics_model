% ============================================================================
% CONSTRAINT STORY: global_protocol_entrenchment
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(global_protocol_entrenchment, []).

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
 * * constraint_id: global_protocol_entrenchment
 * human_readable: The Universal Standard Lock
 * domain: technological/economic
 * * SUMMARY:
 * This scenario shifts the scope of a structural constraint to the global level. 
 * It describes a foundational communication or financial protocol that, 
 * having achieved universal adoption, renders all localized alternatives 
 * non-viable. At this scale, the constraint ceases to be a policy choice 
 * and becomes a prerequisite for participation in the global species-stack.
 * * KEY AGENTS:
 * - Local Startup: Subject (Powerless)
 * - Global Standards Body: Beneficiary (Institutional)
 * - Civilizational Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.80) because universal scope creates total capture of 
% all peripheral innovation and transaction surplus.
domain_priors:base_extractiveness(global_protocol_entrenchment, 0.80). 
domain_priors:suppression_score(global_protocol_entrenchment, 0.95). % Alternatives are effectively non-existent.
domain_priors:theater_ratio(global_protocol_entrenchment, 0.30).    % Low theater; the lock-in is technically absolute.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(global_protocol_entrenchment, extractiveness, 0.8).
narrative_ontology:constraint_metric(global_protocol_entrenchment, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(global_protocol_entrenchment, theater_ratio, 0.3).

% This is an entrenchment scenario, specifically lacking a sunset.
% narrative_ontology:has_sunset_clause(global_protocol_entrenchment). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% At a global scope, the individual sees no "outside"; the protocol is the environment.
constraint_indexing:constraint_classification(global_protocol_entrenchment, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as the ultimate Rope—the only way to coordinate 8 billion agents.
constraint_indexing:constraint_classification(global_protocol_entrenchment, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Viewed historically, the universal lock is a snare that prevents civilizational evolution.
constraint_indexing:constraint_classification(global_protocol_entrenchment, snare, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% Extraction (0.80) triggers the hybrid Tangled Rope signature at the universal scale.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(global_protocol_entrenchment, E), E >= 0.50,
    domain_priors:suppression_score(global_protocol_entrenchment, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_protocol_entrenchment_tests).

test(scope_impact) :-
    % Verify that the global scope solidifies the Mountain classification for the powerless.
    constraint_indexing:constraint_classification(global_protocol_entrenchment, mountain, 
        context(_, _, _, spatial_scope(global))).

test(mandatrophy_resolution) :-
    % Ensure high extraction (0.80) is flagged for resolution.
    domain_priors:base_extractiveness(global_protocol_entrenchment, E),

    E > 0.70.

:- end_tests(global_protocol_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Shifting to a global scope (S=global) removes the "exit" variable for the 
 * Subject, as there is no jurisdictional arbitrage available.
 * * PERSPECTIVAL GAP:
 * The individual experiences a Mountain because the "gravity" of universal 
 * adoption is total. The standards body sees a Rope because they 
 * have achieved the dream of zero-friction global coordination.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. Despite the 
 * 0.80 extraction, the protocol is not a "dead" Piton; it is an active 
 * (though predatory) coordination engine for the entire species.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_extraglobal_exit,
    'Can a decentralized "shadow" protocol emerge that bypasses global entrenchment (Snare vs. Mountain)?',
    'Tracking adoption rates of encrypted, non-standard transport layers.',
    'If shadow protocols thrive: Snare (policy barrier). If they fail to scale: Mountain (physics/logic).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(global_protocol_entrenchment, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
