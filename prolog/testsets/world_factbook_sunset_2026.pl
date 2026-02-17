% ============================================================================
% CONSTRAINT STORY: world_factbook_sunset_2026
% ============================================================================
% Version: 3.5 (Temporal-Resolved Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-07
% ============================================================================

:- module(constraint_world_factbook_sunset, []).

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
    narrative_ontology:constraint_claim/2,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: world_factbook_sunset_2026
 * human_readable: CIA World Factbook Termination
 * domain: geopolitical/information
 * * SUMMARY:
 * CIA Director John Ratcliffe announced the shuttering of the World Factbook 
 * after 60+ years, citing a return to "core missions" amid White House 
 * staffing cuts. This represents a high extraction of public 
 * knowledge capital, removing a foundational reference for journalists, 
 * academics, and analysts.
 * * KEY AGENTS:
 * - Public/Journalists/Academics: Subject (Powerless)
 * - CIA Leadership (Ratcliffe): Beneficiary (Institutional - Budgetary alignment)
 * - Information Auditors: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.70). The closure extracts a multi-decade public 
% utility to offset internal agency resource constraints.
domain_priors:base_extractiveness(world_factbook_sunset_2026, 0.70). 

% Suppression is high (0.85). The removal of the manual suppresses a 
% standardized, unclassified "common language" of global statistics.
domain_priors:suppression_score(world_factbook_sunset_2026, 0.85).   

% Theater ratio is high (0.72). The "core mission" rhetoric acts as a
% theatrical signal masking budgetary austerity as strategic refocus,
% while the actual utility loss (60+ years of unclassified data) is unaddressed.
domain_priors:theater_ratio(world_factbook_sunset_2026, 0.72).       
domain_priors:requires_active_enforcement(world_factbook_sunset_2026).

% Corrected Registry
narrative_ontology:constraint_metric(world_factbook_sunset_2026, extractiveness, 0.70).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, theater_ratio, 0.72).

% Constraint self-claim (analytical classification)
narrative_ontology:constraint_claim(world_factbook_sunset_2026, piton).
narrative_ontology:human_readable(world_factbook_sunset_2026, "CIA World Factbook Termination").

narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, agency_resource_consolidation).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, public_knowledge_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ACADEMIC/JOURNALIST (SNARE)
% For the researcher, the sunset is a Snare: a trap of information poverty 
% where a reliable, free resource is unilaterally removed.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE CIA DIRECTOR (ROPE)
% Leadership views the termination as a Rope: a coordination tool to 
% focus limited staff on "core missions" and AI-driven intelligence.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view the unclassified Factbook as a Piton: an inertial public-facing 
% program that atrophied as the agency shifted focus early in the second term.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

narrative_ontology:interval(world_factbook_sunset_2026, 0, 5).

/* ==========================================================================
   4. MANDATROPHY RESOLUTION HOOK
   ========================================================================== */

omega_variable(
    omega_factbook_successor,
    'Will a private-sector or academic alternative successfully replace the unclassified data stream?',
    'Review of NGO-led data consortiums and open-source intelligence (OSINT) scaling.',
    'Success creates a new Rope; failure hardens the Global Information Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   5. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA: 3x2 Matrix)
   ========================================================================== */

% Metrics: theater_ratio (modeling shift from "public service" to "core mission")
narrative_ontology:measurement(wf_tr_t0, world_factbook_sunset_2026, theater_ratio, 0, 0.10). % Public service era
narrative_ontology:measurement(wf_tr_t2, world_factbook_sunset_2026, theater_ratio, 2, 0.35).
narrative_ontology:measurement(wf_tr_t5, world_factbook_sunset_2026, theater_ratio, 5, 0.72). % Termination announcement

% Metrics: extractiveness (modeling loss of common-knowledge utility)
narrative_ontology:measurement(wf_ex_t0, world_factbook_sunset_2026, extractiveness, 0, 0.15).
narrative_ontology:measurement(wf_ex_t2, world_factbook_sunset_2026, extractiveness, 2, 0.45).
narrative_ontology:measurement(wf_ex_t5, world_factbook_sunset_2026, extractiveness, 5, 0.70).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
