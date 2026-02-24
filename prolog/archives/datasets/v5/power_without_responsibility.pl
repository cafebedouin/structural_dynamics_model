% ============================================================================
% CONSTRAINT STORY: power_without_responsibility
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(power_without_responsibility, []).

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
 * * constraint_id: power_without_responsibility
 * human_readable: The Asymmetric Mandate
 * domain: political/organizational/legal
 * * SUMMARY:
 * A scenario where a "Rope" designed to grant an agent the authority to manage 
 * high-stakes systems (e.g., emergency powers, automated enforcement, or 
 * sovereign immunity) lacks a corresponding accountability mechanism. 
 * This coordination substrate becomes a "Snare" for the subject, as the 
 * agent's power is used to liquidate the subject's primary rights or assets 
 * without legal recourse, trapping the user in a territory of state or 
 * institutional whim where the "protector" cannot be held responsible 
 * for the harms they inflict.
 * * KEY AGENTS:
 * - Unprotected Citizen: Subject (Powerless)
 * - Immune Authority: Beneficiary (Institutional)
 * - Governance Integrity Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.92) reflects the terminal liquidation of the subject's 
% legal agency to maintain the authority's "decisive" operational Rope.
domain_priors:base_extractiveness(power_without_responsibility, 0.92). 
domain_priors:suppression_score(power_without_responsibility, 0.85). % Alternative legal or social recourse is suppressed by the immunity mandate.
domain_priors:theater_ratio(power_without_responsibility, 0.94).    % Extreme theater: "Oversight Committees" that lack the legal power to issue binding judgments.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(power_without_responsibility, extractiveness, 0.92).
narrative_ontology:constraint_metric(power_without_responsibility, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(power_without_responsibility, theater_ratio, 0.94).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they are subject to the authority's power, but 
% the lack of responsibility liquidates their primary defensive agency.
constraint_indexing:constraint_classification(power_without_responsibility, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views the immunity as a Rope—the essential coordination 
% substrate for taking high-risk, high-speed actions without the friction of litigation.
constraint_indexing:constraint_classification(power_without_responsibility, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "Official Ethics Charter" 
% is an inertial spike; it performatively signals responsibility while 0.92 extraction occurs.
constraint_indexing:constraint_classification(power_without_responsibility, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.92) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(power_without_responsibility, E), E >= 0.50,
    domain_priors:suppression_score(power_without_responsibility, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_responsibility_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional authority.
    constraint_indexing:constraint_classification(power_without_responsibility, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_without_responsibility, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(power_without_responsibility, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (0.92) > 0.70 requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(power_without_responsibility, E),

    E > 0.70.

:- end_tests(power_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.92) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of decisive authority is achieved by liquidating the 
 * subject's primary capacity for legal redress.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Unprotected Citizen feels a Snare because they have no shield against 
 * systemic abuse. The Immune Authority sees a Rope because the lack of 
 * responsibility coordinates a perfectly efficient and unhindered exercise 
 * of power in high-stakes environments.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Independent Review Board" is no longer functional (Theater 0.94); 
 * it is an inert spike siphoning 0.92 of the societal legal-agency. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_redress_recapture,
    'Can "Smart-Contract Penalties" restore the Rope, or is immunity a physical law of sovereignty (Snare vs Mountain)?',
    'Tracking the success rate of cryptographic bond-forfeiture in local governance 2026.',
    'If bonds restore agency: Snare of current legal design. If they fail: Mountain of Political Force.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(power_without_responsibility, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
