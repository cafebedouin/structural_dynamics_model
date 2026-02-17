:- module(constraint_event_fragmentation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */
/**
 * SUMMARY:
 *   constraint_id: event_fragmentation
 *   human_readable: Event Fragmentation: Journalism's Structural Blindness to Slow Processes
 *
 * Journalism is structurally bound to the "Event." It perceives discrete
 * points in time (bill signings, accidents, elections) while remaining
 * effectively blind to slow-moving systemic processes (soil erosion,
 * cultural drift). This is a Mountain because the medium's unit of
 * production is the "Story," which requires a beginning and an end.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */
domain_priors:base_extractiveness(event_fragmentation, 0.15).
domain_priors:suppression_score(event_fragmentation, 0.04).
domain_priors:theater_ratio(event_fragmentation, 0.10).

narrative_ontology:constraint_metric(event_fragmentation, extractiveness, 0.15).
narrative_ontology:constraint_metric(event_fragmentation, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(event_fragmentation, theater_ratio, 0.10).

% Mountain Profile
narrative_ontology:constraint_metric(event_fragmentation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(event_fragmentation, resistance, 0.05).
domain_priors:emerges_naturally(event_fragmentation).

narrative_ontology:constraint_claim(event_fragmentation, mountain).
narrative_ontology:human_readable(event_fragmentation, "Event Fragmentation: Journalism's Structural Blindness to Slow Processes").

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */
% Uniform across perspectives as a structural limit of the medium.
constraint_indexing:constraint_classification(event_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

constraint_indexing:constraint_classification(event_fragmentation, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. OMEGA VARIABLES
   ========================================================================== */
narrative_ontology:omega_variable(omega_slow_violence, empirical, 'Can "slow violence" or systemic entropy be reformatted into an event without losing its structural essence?').

/* ==========================================================================
   5. INTEGRATION HOOKS
   ========================================================================== */
narrative_ontology:interval(event_fragmentation, 0, 10).
narrative_ontology:affects_constraint(event_fragmentation, access_arbitrage_capture).
