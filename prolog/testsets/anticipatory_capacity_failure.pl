% ============================================================================
% CONSTRAINT STORY: anticipatory_capacity_failure
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(anticipatory_capacity_failure, []).

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
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: anticipatory_capacity_failure
 * human_readable: The Blindside Equilibrium
 * domain: organizational/technological/cognitive
 * * SUMMARY:
 * A scenario where an organization or system becomes so optimized for its 
 * current high-efficiency environment (Rope) that it loses the cognitive 
 * surplus and structural flexibility required to anticipate or react to 
 * "out-of-distribution" shocks. This optimization acts as a "Snare" during 
 * a crisis, liquidating the subject's survival agency because the 
 * "efficient" protocols have no mapping for the new territory, leading 
 * to terminal systemic fragility.
 * * KEY AGENTS:
 * - Strategic Planner: Subject (Powerless)
 * - Efficiency-Optimized System: Beneficiary (Institutional)
 * - Risk Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% High extraction (0.83) reflects the liquidation of "redundancy" 
domain_priors:base_extractiveness(anticipatory_capacity_failure, 0.83). 
domain_priors:suppression_score(anticipatory_capacity_failure, 0.71). 
domain_priors:theater_ratio(anticipatory_capacity_failure, 0.90).    

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(anticipatory_capacity_failure, extractiveness, 0.83).
narrative_ontology:constraint_metric(anticipatory_capacity_failure, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(anticipatory_capacity_failure, theater_ratio, 0.9).

% Constraint classification claim
narrative_ontology:constraint_claim(anticipatory_capacity_failure, piton).
narrative_ontology:human_readable(anticipatory_capacity_failure, "The Blindside Equilibrium").
narrative_ontology:topic_domain(anticipatory_capacity_failure, "organizational/technological/cognitive").

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(anticipatory_capacity_failure).

% Multifile declarations for Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(anticipatory_capacity_failure, efficiency_optimized_systems).
narrative_ontology:constraint_victim(anticipatory_capacity_failure, strategic_planners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The planner is trapped: they see the approaching shock but cannot divert 
% resources because the system's "efficiency" logic prohibits redundancy.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the lean optimization as a Rope—the only way to 
% coordinate global-scale logistics and remain competitive in a low-margin world.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.90) > 0.70 triggers Piton: the "Risk Dashboard" is an 
% inertial spike; it monitors known variables while ignoring structural decay.
constraint_indexing:constraint_classification(anticipatory_capacity_failure, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.83) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(anticipatory_capacity_failure, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(anticipatory_capacity_failure, E), E >= 0.50,
    domain_priors:suppression_score(anticipatory_capacity_failure, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anticipatory_capacity_failure_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless planner vs Rope for the institutional system.
    constraint_indexing:constraint_classification(anticipatory_capacity_failure, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(anticipatory_capacity_failure, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.90) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(anticipatory_capacity_failure, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(anticipatory_capacity_failure, E),
    E > 0.70.

:- end_tests(anticipatory_capacity_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of hyper-efficiency is achieved by liquidating 
 * the subject's primary capacity for foresight and resilience.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Strategic Planner feels a Snare because they are forced to participate 
 * in a "just-in-time" logic that they know is terminal. The System sees 
 * a Rope because the removal of "wasteful" redundancy coordinates 
 * maximum throughput in the immediate term.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Optimized State" is no longer functional for long-term 
 * survival (Theater 0.90); it is an inert spike siphoning 0.83 of 
 * the species' agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_fragility_tipping_point,
    'Can "active redundancy" be restored before a shock, or is optimization an entropic "Mountain" (Snare vs Mountain)?',
    'Tracking the failure rate of "resilient" versus "efficient" systems in non-linear market events.',
    'If efficient systems collapse: Snare of current logic. If resilient systems fail: Mountain of Systemic Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(anticipatory_capacity_failure, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional lean-management (0.30) to 
% inertial "Risk Dashboard" theater (0.90) as redundancy is fully liquidated.
narrative_ontology:measurement(acf_tr_t0, anticipatory_capacity_failure, theater_ratio, 0, 0.30).
narrative_ontology:measurement(acf_tr_t5, anticipatory_capacity_failure, theater_ratio, 5, 0.65).
narrative_ontology:measurement(acf_tr_t10, anticipatory_capacity_failure, theater_ratio, 10, 0.90).

% Extraction: Progressive accumulation of structural debt as 
% "just-in-time" optimization liquidates anticipatory surplus.
narrative_ontology:measurement(acf_ex_t0, anticipatory_capacity_failure, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(acf_ex_t5, anticipatory_capacity_failure, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(acf_ex_t10, anticipatory_capacity_failure, base_extractiveness, 10, 0.83).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
