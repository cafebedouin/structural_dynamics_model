% ============================================================================
% CONSTRAINT STORY: responsibility_dilution
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(responsibility_dilution, []).

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
 * * constraint_id: responsibility_dilution
 * human_readable: The Accountability Fog
 * domain: organizational/legal/technological
 * * SUMMARY:
 * A scenario where a critical decision is fragmented across so many 
 * autonomous agents, bureaucratic layers, and algorithmic filters that 
 * the "locus of responsibility" effectively vanishes. This "Rope" for 
 * achieving high-scale institutional coordination becomes a "Snare" for 
 * the subject (victim), as their grievance is liquidated by a system where 
 * "everyone followed protocol, but the harm occurred," trapping them in 
 * a territory of unaddressable injury with no legal or moral recourse.
 *
 * * KEY AGENTS:
 * - Harmed Individual: Subject (Powerless)
 * - Distributed Organization: Beneficiary (Institutional)
 * - Forensic Governance Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.90) reflects the near-total liquidation of the subject's 
% right to redress to maintain the institution's "risk-minimized" status.
domain_priors:base_extractiveness(responsibility_dilution, 0.90). 
domain_priors:suppression_score(responsibility_dilution, 0.83). % Individual blame is suppressed by the complexity of the "System."
domain_priors:theater_ratio(responsibility_dilution, 0.91).    % Extreme theater: "Ethics Committees" and "Compliance Audits" that cannot assign liability.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(responsibility_dilution, extractiveness, 0.9).
narrative_ontology:constraint_metric(responsibility_dilution, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(responsibility_dilution, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped: they have suffered a clear harm, but the 
% system's architecture liquidates their ability to point to a responsible actor.
constraint_indexing:constraint_classification(responsibility_dilution, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the dilution as a Rope—the only way to coordinate 
% massive, high-risk operations without the "friction" of individual liability.
constraint_indexing:constraint_classification(responsibility_dilution, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Official Investigation" 
% is an inertial spike; it performatively catalogs the "System Failure" without 
% restoring the subject's agency.
constraint_indexing:constraint_classification(responsibility_dilution, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.90) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(responsibility_dilution, E), E >= 0.50,
    domain_priors:suppression_score(responsibility_dilution, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_dilution_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless victim vs Rope for the institutional hierarchy.
    constraint_indexing:constraint_classification(responsibility_dilution, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_dilution, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(responsibility_dilution, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(responsibility_dilution, E),

    E > 0.70.

:- end_tests(responsibility_dilution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.90) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of large-scale organization is achieved by liquidating 
 * the primary accountability agency of the affected subject.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Harmed Individual feels a Snare because they are screaming into a vacuum 
 * of "automated workflows." The Organization sees a Rope because the 
 * fragmentation of liability coordinates the massive risk-taking necessary 
 * for modern industry.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Compliance Audit" is no longer functional for justice 
 * (Theater 0.91); it is an inert spike siphoning 0.90 of the human 
 * legal-territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_traceable_agency,
    'Can blockchain-based logging restore the Rope, or is dilution a physical law of bureaucracy (Snare vs Mountain)?',
    'Tracking the success rate of "immutable audit trails" in assigning individual legal blame in 2030.',
    'If blame sticks: Snare of current opacity. If it fails: Mountain of Organizational Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(responsibility_dilution, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
