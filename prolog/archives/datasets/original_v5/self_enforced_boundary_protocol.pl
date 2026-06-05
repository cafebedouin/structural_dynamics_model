% ============================================================================
% CONSTRAINT STORY: self_enforced_boundary_protocol
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-04
% ============================================================================

:- module(constraint_boundary_logic, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: boundary_protocol
 * human_readable: The Internal Perimeter
 * domain: psychological/social
 * * SUMMARY:
 * A boundary is defined not as a request for another's change, but as an 
 * internal action protocol: "If X happens, I will do Y." It requires the 
 * other person to do nothing, effectively moving the 'exit option' 
 * entirely into the Subject's hands.
 * * KEY AGENTS:
 * - The Voyager: Subject (Powerful/Analytical) - Defines their own perimeter.
 * - The External: Beneficiary (Institutional) - The party whose behavior 
 * triggers the boundary.
 * - The Auditor: Auditor (Analytical) - Observing the stability of the 
 * interaction post-boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is zero; the Subject is not losing resources, but reclaiming them.
domain_priors:base_extractiveness(boundary_protocol, 0.00). 
% Suppression is low because the exit option is internal and immediate.
domain_priors:suppression_score(boundary_protocol, 0.10).   
% Theater Ratio is low; this is a functional rule, not a performative plea.
domain_priors:theater_ratio(boundary_protocol, 0.05).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(boundary_protocol, extractiveness, 0.0).
narrative_ontology:constraint_metric(boundary_protocol, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(boundary_protocol, theater_ratio, 0.05).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VOYAGER (ROPE)
% By defining the boundary as an internal action, it becomes a coordination 
% tool for self-preservation.
constraint_indexing:constraint_classification(boundary_protocol, rope, 
    context(agent_power(powerful), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE EXTERNAL (MOUNTAIN)
% To the person hitting the boundary, it appears as an immovable law of 
% nature—they cannot change it through negotiation.
constraint_indexing:constraint_classification(boundary_protocol, mountain, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SCAFFOLD)
% Initially, the boundary acts as a support structure for the Voyager 
% until the new dynamic becomes "banal" and automatic.
constraint_indexing:constraint_classification(boundary_protocol, scaffold, 
    context(agent_power(analytical), 
            time_horizon(generational), 
            exit_options(analytical), 
            spatial_scope(regional))) :-
    narrative_ontology:has_sunset_clause(boundary_protocol).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This flips the "Snare" of social dependency into a "Rope" of autonomy. 
 * Because it requires the "other person to do nothing," the extraction 
 * score drops to 0.00—the Voyager is no longer paying an emotional tax 
 * to maintain the other's comfort.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Drops as the boundary moves from "stated" to "integrated."
narrative_ontology:measurement(bp_tr_t0, boundary_protocol, theater_ratio, 0, 0.40).
narrative_ontology:measurement(bp_tr_t10, boundary_protocol, theater_ratio, 10, 0.05).

% Extraction: Remains at zero as the Subject refuses to be "siphoned."
narrative_ontology:measurement(bp_ex_t0, boundary_protocol, base_extractiveness, 0, 0.00).
narrative_ontology:measurement(bp_ex_t10, boundary_protocol, base_extractiveness, 10, 0.00).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
