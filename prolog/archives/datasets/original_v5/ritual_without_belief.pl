% ============================================================================
% CONSTRAINT STORY: ritual_without_belief
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(ritual_without_belief, []).

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
 * * constraint_id: ritual_without_belief
 * human_readable: The Hollow Orthopraxy
 * domain: social/organizational/religious
 * * SUMMARY:
 * A scenario where the external performance of a ritual or protocol is 
 * strictly enforced, even though the underlying belief or functional utility 
 * has vanished. This coordination "Rope" for institutional stability acts 
 * as a "Snare" for the subject, who must expend cognitive and temporal 
 * resources on performative compliance to avoid social or professional 
 * liquidation.
 * * KEY AGENTS:
 * - Compliance Adherent: Subject (Powerless)
 * - Tradition/Policy Custodian: Beneficiary (Institutional)
 * - Socio-Logician: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the ritual siphons the subject's primary agency 
% into the maintenance of a non-functional social signal.
domain_priors:base_extractiveness(ritual_without_belief, 0.83). 
domain_priors:suppression_score(ritual_without_belief, 0.75). % High cost of non-compliance.
domain_priors:theater_ratio(ritual_without_belief, 0.95).    % Extreme theater: the ritual is purely performative.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ritual_without_belief, extractiveness, 0.83).
narrative_ontology:constraint_metric(ritual_without_belief, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ritual_without_belief, theater_ratio, 0.95).

% This is an entropic state of late-stage institutions.
% narrative_ontology:has_sunset_clause(ritual_without_belief). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: they must perform the ritual to access 
% essential services or status, despite the lack of belief.
constraint_indexing:constraint_classification(ritual_without_belief, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the ritual as a Rope—the only way to coordinate 
% behavior and ensure institutional continuity in a pluralistic environment.
constraint_indexing:constraint_classification(ritual_without_belief, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.95) > 0.70 triggers Piton: the "ritual" is a 
% non-functional, performative artifact of institutional inertia.
constraint_indexing:constraint_classification(ritual_without_belief, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.83) masking as coordination (Rope).
constraint_indexing:constraint_classification(ritual_without_belief, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(ritual_without_belief, E), E >= 0.50,
    domain_priors:suppression_score(ritual_without_belief, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_without_belief_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(ritual_without_belief, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ritual_without_belief, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure extreme theater (0.95) results in Piton detection.
    constraint_indexing:constraint_classification(ritual_without_belief, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.83) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(ritual_without_belief, E),

    E > 0.70.

:- end_tests(ritual_without_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic consumption of cognitive energy.
 * 
 * * PERSPECTIVAL GAP:
 * The Compliance Adherent feels a Snare because they are forced to 
 * lie to survive. The Tradition Custodian sees a Rope because the 
 * external uniformity prevents the chaos of individual dissent.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the coordination is no longer functional relative to belief 
 * (Theater 0.95); it is an inert spike siphoning 0.83 of the subject's agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_belief_recapture,
    'Can the belief be re-instilled, or is the decay irreversible (Snare vs Mountain)?',
    'Tracking the sincerity of internal communications vs external performance over time.',
    'If belief returns: Snare of current culture. If decay persists: Mountain of Cultural Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(ritual_without_belief, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
