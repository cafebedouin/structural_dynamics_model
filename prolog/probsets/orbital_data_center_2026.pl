% ============================================================================
% CONSTRAINT STORY: orbital_data_center_2026
% ============================================================================
% Version: 3.5 (Schema-Corrected Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_orbital_data_center_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: orbital_data_center_2026
 * human_readable: SpaceX Million-Satellite Orbital Compute
 * domain: technological/geopolitical
 * * SUMMARY:
 * SpaceX has filed for an FCC license to deploy one million satellites 
 * functioning as an orbital data center. By leveraging Starship’s 
 * tonnage and direct solar power, the system intends to bypass the 
 * "Snare" of Earth’s electrical grid.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

domain_priors:base_extractiveness(orbital_data_center_2026, 0.82). 
domain_priors:suppression_score(orbital_data_center_2026, 0.75).   
domain_priors:theater_ratio(orbital_data_center_2026, 0.45).       

% Corrected Registry
narrative_ontology:constraint_metric(orbital_data_center_2026, extractiveness, 0.82).
narrative_ontology:constraint_metric(orbital_data_center_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(orbital_data_center_2026, theater_ratio, 0.45).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TERRESTRIAL GRID (SNARE)
constraint_indexing:constraint_classification(orbital_data_center_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE SPACEX ARCHITECT (ROPE)
constraint_indexing:constraint_classification(orbital_data_center_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

/* ==========================================================================
   4. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA)
   ========================================================================== */

% Corrected keys: base_extractiveness -> extractiveness
narrative_ontology:measurement(odc_ex_t0, orbital_data_center_2026, extractiveness, 0, 0.40).
narrative_ontology:measurement(odc_ex_t10, orbital_data_center_2026, extractiveness, 10, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
