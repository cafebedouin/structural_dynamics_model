% ============================================================================
% CONSTRAINT STORY: asymmetric_burden_distribution
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(asymmetric_burden_distribution, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
% --- Namespace Hooks (Updated for v3.4) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: asymmetric_burden_distribution
 * human_readable: The Externalization Lever
 * domain: economic/social/logistical
 * * SUMMARY:
 * A scenario where the systemic benefits of a process (e.g., global shipping, 
 * digital convenience, urban density) are concentrated at the center, while 
 * the operational burdens (noise, pollution, psychological stress, physical 
 * maintenance) are pushed onto a peripheral population. This "Rope" for 
 * achieving mass-market affordability becomes a "Snare" for the peripheral 
 * subject, whose local agency and quality of life are liquidated to subsidize 
 * the center's "frictionless" experience, trapping them in a territory of 
 * involuntary sacrifice with no mechanism for cost-recapture.
 *
 * * KEY AGENTS:
 * - Peripheral Resident: Subject (Powerless)
 * - Central Beneficiary: Beneficiary (Institutional)
 * - Socio-Economic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

domain_priors:base_extractiveness(asymmetric_burden_distribution, 0.89). 
domain_priors:suppression_score(asymmetric_burden_distribution, 0.81). 
domain_priors:theater_ratio(asymmetric_burden_distribution, 0.86).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(asymmetric_burden_distribution, extractiveness, 0.89).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, theater_ratio, 0.86).

% Mandatory keys for classification engine v3.4
% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, central_beneficiary).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, peripheral_resident).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The resident is trapped: they cannot move due to economic constraints, 
% yet remaining liquidates their health and property value to serve the center.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The central institution views the distribution as a Rope—the only way to 
% coordinate a competitive, low-cost service for the "greater good."
constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.86) > 0.70 triggers Piton: the "Environmental Mitigation Plan" 
% is an inertial spike; it performatively addresses the burden while permitting the extraction.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E), E >= 0.50,
    domain_priors:suppression_score(asymmetric_burden_distribution, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_burden_tests).

test(perspectival_gap) :-
    % Verify Snare for the peripheral subject vs Rope for the central beneficiary.
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.86) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E),
    E > 0.70.

:- end_tests(asymmetric_burden_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of mass-scale infrastructure is achieved by liquidating 
 * the primary survival agency of the peripheral population.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Peripheral Resident feels a Snare because their daily environment is 
 * consumed by a system they don't benefit from. The Central Beneficiary sees 
 * a Rope because the externalization coordinates the affordability that 
 * drives the entire economy.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Compensation Policy" is no longer functional (Theater 0.86); 
 * it is an inert spike siphoning 0.89 of the subject's local agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_burden_recapture,
    'Can cost-internalization (e.g., Pigouvian taxes) restore the Rope, or is asymmetry a physical law of scaling (Snare vs Mountain)?',
    'Tracking the success rate of local "sovereignty zones" in recapturing extraction value over 15 years.',
    'If recapture succeeds: Snare of current policy. If it fails: Mountain of Logistical Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(asymmetric_burden_distribution, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional mitigation (0.25) to 
% inertial "Community Grant" theater (0.86) as the burden scales.
narrative_ontology:measurement(abd_tr_t0, asymmetric_burden_distribution, theater_ratio, 0, 0.25).
narrative_ontology:measurement(abd_tr_t5, asymmetric_burden_distribution, theater_ratio, 5, 0.53).
narrative_ontology:measurement(abd_tr_t10, asymmetric_burden_distribution, theater_ratio, 10, 0.86).

% Extraction: Progressive accumulation of environmental/social costs 
% forced onto the peripheral subject.
narrative_ontology:measurement(abd_ex_t0, asymmetric_burden_distribution, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(abd_ex_t5, asymmetric_burden_distribution, base_extractiveness, 5, 0.67).
narrative_ontology:measurement(abd_ex_t10, asymmetric_burden_distribution, base_extractiveness, 10, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
