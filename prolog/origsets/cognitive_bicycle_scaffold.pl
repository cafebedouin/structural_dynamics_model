% ============================================================================
% CONSTRAINT STORY: cognitive_bicycle_scaffold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_cognitive_bicycle, []).

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
 * * constraint_id: cognitive_bicycle_scaffold
 * human_readable: The Bicycle of the Mind
 * domain: technological/cognitive
 * * SUMMARY:
 * A tool-based constraint where the agent utilizes A.I. to amplify 
 * reasoning capacity. It functions as a 'Gym' for the powerful and a 
 * 'Construction Site' for the powerless.
 * * KEY AGENTS:
 * - The Voyager: Subject (Powerful/Analytical) - Using A.I. as a bicycle.
 * - The Dependent: Subject (Powerless) - Using A.I. as a crutch/replacement.
 * - The System Architect: Beneficiary (Institutional) - Providing the tool.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

domain_priors:base_extractiveness(cognitive_bicycle_scaffold, 0.20). 
domain_priors:suppression_score(cognitive_bicycle_scaffold, 0.30).   
domain_priors:theater_ratio(cognitive_bicycle_scaffold, 0.15).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, extractiveness, 0.2).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, theater_ratio, 0.15).

% This classification requires a sunset clause: eventually, the 'training 
% wheels' must come off or the tool must become 'banal'.
narrative_ontology:has_sunset_clause(cognitive_bicycle_scaffold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VOYAGER (ROPE)
% For an analytical user, the tool is pure coordination of thought.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, rope, 
    context(agent_power(analytical), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE DEPENDENT (SNARE)
% For a powerless user, the tool extracts the habit of thinking.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% The temporary support provided to transition to a new cognitive state.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, scaffold, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(global))).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 'Bicycle' is a Rope because it empowers the user to 'do bigger, cooler 
 * things'. However, if the user lacks the 'cognitive abilities' to 
 * verify the output, the Rope becomes a Snare—a 'slop factory' that 
 * automates their own obsolescence.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_bicycle_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as the tool becomes 'boring' and banal.
narrative_ontology:measurement(cbs_tr_t0, cognitive_bicycle_scaffold, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cbs_tr_t10, cognitive_bicycle_scaffold, theater_ratio, 10, 0.15).

% Extraction: Remains low as long as the agent maintains 'Gym' status.
narrative_ontology:measurement(cbs_ex_t0, cognitive_bicycle_scaffold, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cbs_ex_t10, cognitive_bicycle_scaffold, base_extractiveness, 10, 0.20).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
