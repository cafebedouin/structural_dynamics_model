% ============================================================================
% CONSTRAINT STORY: scam_compound_grey_zone_2026
% ============================================================================
% Version: 3.5 (Schema-Corrected Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_scam_compound_2026, []).

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
 * * constraint_id: scam_compound_2026
 * human_readable: The Southeast Asian Scam Compound "Grey Zone"
 * domain: social/humanitarian/criminal
 * * SUMMARY:
 * In fortified enclaves, the distinction between victims of modern slavery 
 * and criminal perpetrators has collapsed. Workers are trafficked (subjects) 
 * but compelled to refine scams, creating a "victim-offender overlap" that 
 * defies standard legal "Ideal Victim" categories.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

domain_priors:base_extractiveness(scam_compound_2026, 0.85). 
domain_priors:suppression_score(scam_compound_2026, 0.90).   
domain_priors:theater_ratio(scam_compound_2026, 0.65).       

% Corrected Registry
narrative_ontology:constraint_metric(scam_compound_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(scam_compound_2026, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(scam_compound_2026, theater_ratio, 0.65).

narrative_ontology:constraint_beneficiary(scam_compound_2026, organized_crime_syndicates).
narrative_ontology:constraint_victim(scam_compound_2026, trafficked_migrant_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WORKER (SNARE)
constraint_indexing:constraint_classification(scam_compound_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE OPERATOR (ROPE)
constraint_indexing:constraint_classification(scam_compound_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
constraint_indexing:constraint_classification(scam_compound_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA)
   ========================================================================== */

% Corrected keys: base_extractiveness -> extractiveness
narrative_ontology:measurement(scam_ex_t0, scam_compound_2026, extractiveness, 0, 0.50).
narrative_ontology:measurement(scam_ex_t10, scam_compound_2026, extractiveness, 10, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
