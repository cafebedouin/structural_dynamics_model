% ============================================================================
% CONSTRAINT STORY: fossil_fuel_lock_in
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(fossil_fuel_lock_in, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: fossil_fuel_lock_in
 * human_readable: Fossilized Regionalism (Alberta Carbon-Path Dependency)
 * domain: economic/political
 *
 * SUMMARY:
 * Alberta's economy exhibits a "fossilized" regionalism where decades of 
 * political and economic investment in oil and gas have locked in structural 
 * expectations regarding jobs, identity, and provincial autonomy. 
 * This is not merely a policy choice but a structural reality where the 
 * physical and institutional infrastructure creates a Mountain-like resistance 
 * to rapid transition, despite global climate pressures.
 *
 * KEY AGENTS (by structural relationship):
 * - conventional_workforce: Primary target (moderate/trapped) — tied to the 
 * existing energy infrastructure for livelihood.
 * - provincial_government: Primary beneficiary (institutional/mobile) — relies 
 * on royalties and tax revenue to maintain a low-tax environment.
 * - energy_sector_investors: Secondary actor (powerful/arbitrage) — holding 
 * stranded assets or seeking expanded development.
 * - university_researchers: Analytical observer — tracking the collision of 
 * regional identity with global climate targets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fossil_fuel_lock_in, 0.15).
domain_priors:suppression_score(fossil_fuel_lock_in, 0.02).
domain_priors:theater_ratio(fossil_fuel_lock_in, 0.10).

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(fossil_fuel_lock_in, extractiveness, 0.15).
narrative_ontology:constraint_metric(fossil_fuel_lock_in, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fossil_fuel_lock_in, theater_ratio, 0.10).

% --- NL Profile Metrics (Required for Mountain status) ---
% These confirm the "natural law" status of the resource-based lock-in.
narrative_ontology:constraint_metric(fossil_fuel_lock_in, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fossil_fuel_lock_in, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fossil_fuel_lock_in, mountain).
narrative_ontology:human_readable(fossil_fuel_lock_in, "Fossilized Regionalism (Alberta Carbon-Path Dependency)").
narrative_ontology:topic_domain(fossil_fuel_lock_in, "economic/political").

% --- Emergence flag ---
domain_priors:emerges_naturally(fossil_fuel_lock_in).

% --- Structural relationships ---
% Note: Mountains typically don't require these for logic, but we list them
% to ground the narrative agents for downstream Tangled Ropes.
narrative_ontology:constraint_beneficiary(fossil_fuel_lock_in, provincial_treasury).
narrative_ontology:constraint_victim(fossil_fuel_lock_in, decarbonization_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LOCAL WORKFORCE (MOUNTAIN)
% For workers, the dependency is a physical/biographical reality.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE INSTITUTIONAL STATE (MOUNTAIN)
% The province views this lock-in as a foundational "given" of its fiscal capacity.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes the structural "fossilization" of regionalism as a core invariant.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fossil_fuel_lock_in_tests).

test(mountain_invariance) :-
    % Verify that the classification is mountain across disparate scales.
    constraint_indexing:constraint_classification(fossil_fuel_lock_in, mountain, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(fossil_fuel_lock_in, mountain, context(agent_power(analytical), _, _, _)).

test(nl_signature_gate) :-
    % Check that the NL profile metrics meet the required thresholds for Mountain.
    narrative_ontology:constraint_metric(fossil_fuel_lock_in, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(fossil_fuel_lock_in, resistance, R), R =< 0.15.

:- end_tests(fossil_fuel_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The constraint is classified as a Mountain because it represents a 
 * structural "lock-in". This isn't just a policy; it is 
 * the physical reality of billions in sunk infrastructure and a regional 
 * identity "fossilized" by decades of economic dependence.
 *
 * PERSPECTIVAL GAP:
 * There is minimal gap because all actors—from the worker to the 
 * global observer—concede that Alberta's economy is structurally 
 * hydrocarbon-dependent. The disagreement is not on the fact 
 * of the lock-in, but on the morality of maintaining it.
 *
 * DIRECTIONALITY LOGIC:
 * The provincial government acts as a structural beneficiary because 
 * hydrocarbons subsidize the low-tax, high-spending model of Alberta. 
 * Decarbonization advocates are structural victims as the lock-in makes 
 * climate policy structurally more expensive and socially "extractiveness-heavy" 
 * for the region.
 *
 * MANDATROPHY ANALYSIS:
 * By classifying this as a Mountain, we prevent it from being 
 * misinterpreted as a mere Snare (pure extraction). 
 * The lock-in has a genuine (though risky) coordination function: it 
 * powers the province's current standard of living.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

% /5 form: narrative detail
omega_variable(
    omega_lock_in_permanence,
    'Is the lock-in a physical Mountain of infrastructure or a Piton of institutional choice?',
    'Comparative analysis of transition speed in similar petro-states (e.g., Norway).',
    'If Piton, sovereignty movements are theatrical; if Mountain, they are survivalist.',
    confidence_without_resolution(high)
).

% /3 form: typed classification
narrative_ontology:omega_variable(omega_lock_in_permanence, conceptual, 'Classification of resource dependency as natural law vs. policy choice').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fossil_fuel_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% While low extraction, we track it for "Pitonization" (drift toward theater).
narrative_ontology:measurement(ffli_tr_t0, fossil_fuel_lock_in, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ffli_tr_t5, fossil_fuel_lock_in, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ffli_tr_t10, fossil_fuel_lock_in, theater_ratio, 10, 0.10).

narrative_ontology:measurement(ffli_ex_t0, fossil_fuel_lock_in, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ffli_ex_t5, fossil_fuel_lock_in, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(ffli_ex_t10, fossil_fuel_lock_in, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fossil_fuel_lock_in, resource_allocation).

% This lock-in structurally influences the perception of climate policy as extraction.
% affects_constraint(fossil_fuel_lock_in, climate_policy_extraction).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
