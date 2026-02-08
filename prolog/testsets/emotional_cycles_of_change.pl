% ============================================================================
% CONSTRAINT STORY: emotional_cycles_of_change
% ============================================================================
% Version: 3.5 (Psychological-Temporal Mapping)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_emotional_cycles, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
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
 * * constraint_id: emotional_cycles_2026
 * human_readable: The Kelley-Connor Cycle of Change
 * domain: psychology/behavioral_science
 * * SUMMARY:
 * A five-stage model (Uninformed Optimism, Informed Pessimism, Valley of Despair, 
 * Informed Optimism, Success/Completion) describing the emotional cost of 
 * adopting new systems or habits. The "Valley of Despair" acts as a primary 
 * behavioral Snare where most projects fail.
 * * KEY AGENTS:
 * - Individual/Adopter: Subject (Powerless against neurochemical dips)
 * - The Habit/System: Beneficiary (Institutional - Long-term integration)
 * - Psychometric Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is moderate-high (0.55). The cycle extracts cognitive 
% energy and willpower, peaking during the "Valley of Despair."
domain_priors:base_extractiveness(emotional_cycles_2026, 0.55). 

% Suppression is moderate (0.45). The emotional weight of the cycle 
% suppresses alternative "easy" paths to ensure deep behavioral rewiring.
domain_priors:suppression_score(emotional_cycles_2026, 0.45).   

% Theater ratio is high (0.70). The initial "Uninformed Optimism" 
% stage is almost entirely theatrical—a dopamine-driven performance 
% of a future that has not yet been built.
domain_priors:theater_ratio(emotional_cycles_2026, 0.70).       

% Corrected Registry
narrative_ontology:constraint_metric(emotional_cycles_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(emotional_cycles_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(emotional_cycles_2026, theater_ratio, 0.70).

narrative_ontology:constraint_beneficiary(emotional_cycles_2026, long_term_habit_formation).
narrative_ontology:constraint_victim(emotional_cycles_2026, short_term_psychological_comfort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ADOPTER (SNARE)
% In the "Valley of Despair," the cycle is a Snare: a trap of 
% low motivation and high effort where the exit (quitting) 
% seems most attractive.
constraint_indexing:constraint_classification(emotional_cycles_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(personal))).

% PERSPECTIVE 2: THE COACH/SYSTEM (SCAFFOLD)
% For the instructor, the cycle is a Scaffold: a necessary temporary 
% framework to support the user until the habit becomes self-sustaining.
constraint_indexing:constraint_classification(emotional_cycles_2026, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Analysts view the cycle as a Mountain: an irreducible biological 
% limit on how fast humans can process significant life transitions.
constraint_indexing:constraint_classification(emotional_cycles_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA: 3x2 Matrix)
   ========================================================================== */

% Metrics: theater_ratio (modeling the shift from "hype" to "reality")
narrative_ontology:measurement(ec_tr_t0, emotional_cycles_2026, theater_ratio, 0, 0.95). % Uninformed Optimism
narrative_ontology:measurement(ec_tr_t2, emotional_cycles_2026, theater_ratio, 2, 0.20). % Valley of Despair (No Hype)
narrative_ontology:measurement(ec_tr_t5, emotional_cycles_2026, theater_ratio, 5, 0.10). % Completion (Real Results)

% Metrics: extractiveness (modeling the willpower tax)
narrative_ontology:measurement(ec_ex_t0, emotional_cycles_2026, extractiveness, 0, 0.10). % Low effort
narrative_ontology:measurement(ec_ex_t2, emotional_cycles_2026, extractiveness, 2, 0.85). % Maximum willpower extraction
narrative_ontology:measurement(ec_ex_t5, emotional_cycles_2026, extractiveness, 5, 0.05). % Effortless Habit

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (emotional_cycles_of_change)
% ============================================================================
narrative_ontology:constraint_claim(emotional_cycles_2026, tangled_rope).
domain_priors:requires_active_enforcement(emotional_cycles_2026).
narrative_ontology:interval(emotional_cycles_2026, 0, 10).

omega_variable(
    omega_valley_of_despair_depth,
    "Is the 'Valley of Despair' a fixed neurochemical depth (Mountain) or a variable influenced by coaching/support (Scaffold)?",
    "Comparative study of coached vs. uncoached adoption cycles measuring cortisol and completion rates.",
    "If fixed: The cycle is a permanent Mountain of human limitation. If variable: Coaching transforms the Snare into a manageable Scaffold.",
    confidence_without_resolution(medium)
).
