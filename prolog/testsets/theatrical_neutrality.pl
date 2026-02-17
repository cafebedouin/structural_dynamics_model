:- module(constraint_theatrical_neutrality, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */
/**
 * SUMMARY:
 * The "View from Nowhere." By framing asymmetric claims as equally 
 * valid "perspectives" to avoid charges of bias, the publication 
 * actively extracts the reader's ability to discern truth. 
 * This is the ultimate Snare: it suppresses accuracy to maintain 
 * professional standing.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */
domain_priors:base_extractiveness(theatrical_neutrality, 0.68).
domain_priors:suppression_score(theatrical_neutrality, 0.72).
domain_priors:theater_ratio(theatrical_neutrality, 0.85).

narrative_ontology:constraint_metric(theatrical_neutrality, extractiveness, 0.68).
narrative_ontology:constraint_metric(theatrical_neutrality, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(theatrical_neutrality, theater_ratio, 0.85).

narrative_ontology:constraint_claim(theatrical_neutrality, snare).

narrative_ontology:constraint_beneficiary(theatrical_neutrality, editorial_boards).
narrative_ontology:constraint_victim(theatrical_neutrality, civic_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */
% Editorial perspective: It's a Rope/Piton to preserve brand authority.
constraint_indexing:constraint_classification(theatrical_neutrality, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Public perspective: It's a Snare. The more you read, the less you know.
constraint_indexing:constraint_classification(theatrical_neutrality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. TEMPORAL DATA (Drift Detection)
   ========================================================================== */
% As objectivity becomes "theatricality," theater_ratio rises.
narrative_ontology:measurement(neut_tr_t0, theatrical_neutrality, theater_ratio, 0, 0.30).
narrative_ontology:measurement(neut_tr_t5, theatrical_neutrality, theater_ratio, 5, 0.60).
narrative_ontology:measurement(neut_tr_t10, theatrical_neutrality, theater_ratio, 10, 0.85).

narrative_ontology:measurement(neut_ex_t0, theatrical_neutrality, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(neut_ex_t5, theatrical_neutrality, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(neut_ex_t10, theatrical_neutrality, base_extractiveness, 10, 0.68).

/* ==========================================================================
   5. OMEGA VARIABLES
   ========================================================================== */
narrative_ontology:omega_variable(omega_epistemic_exit, preference, 'Does the existence of "Theatrical Neutrality" create a structural demand for alternative, high-bias media as a compensatory mechanism?').

/* ==========================================================================
   6. INTEGRATION HOOKS
   ========================================================================== */
narrative_ontology:interval(theatrical_neutrality, 0, 10).
