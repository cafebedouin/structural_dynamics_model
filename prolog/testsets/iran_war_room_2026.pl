% ============================================================================
% CONSTRAINT STORY: iran_war_room_2026
% ============================================================================
% Version: 3.5 (Schema-Corrected Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-07
% ============================================================================

:- module(constraint_iran_war_room_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: iran_war_room_2026
 * human_readable: The IRGC "Active War Room" Snare
 * domain: geopolitical/military
 * * SUMMARY:
 * IRGC Aerospace Force commanders have declared an "active war room," 
 * signaling readiness for decisive direct confrontation. Iran's offensive 
 * power is reported as significantly higher than during the "12-day war in 
 * June 2025". This posture creates a high-extraction environment where US 
 * regional and economic interests are indexed as within operational range.
 * * KEY AGENTS:
 * - Regional Military Forces (US/Allies): Subject (Powerless against range-indexing)
 * - IRGC Aerospace Force: Beneficiary (Institutional - Strategic Readiness)
 * - Geopolitical Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.82). The "active war room" identifies US military 
% and economic interests as sites for potential massive regional extraction.
domain_priors:base_extractiveness(iran_war_room_2026, 0.82). 

% Suppression is high (0.78). Intelligence dominance and monitoring of 
% all adversary movements suppress stealth and surprise options.
domain_priors:suppression_score(iran_war_room_2026, 0.78).   

% Theater ratio is high (0.65). The rhetoric of "harsh response" and 
% "greatest advantage" serves as a theatrical signal of deterrent power.
domain_priors:theater_ratio(iran_war_room_2026, 0.65).       

% Corrected Registry
narrative_ontology:constraint_metric(iran_war_room_2026, extractiveness, 0.82).
narrative_ontology:constraint_metric(iran_war_room_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(iran_war_room_2026, theater_ratio, 0.65).

% Constraint self-claim (analytical classification)
narrative_ontology:constraint_claim(iran_war_room_2026, tangled_rope).
narrative_ontology:topic_domain(iran_war_room_2026, "geopolitical/military").
narrative_ontology:human_readable(iran_war_room_2026, "The IRGC \"Active War Room\" Snare").
domain_priors:requires_active_enforcement(iran_war_room_2026).

narrative_ontology:constraint_beneficiary(iran_war_room_2026, irgc_strategic_deterrence).
narrative_ontology:constraint_victim(iran_war_room_2026, regional_economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REGIONAL ADVERSARY (SNARE)
% For US forces, the environment is a Snare: a trap where all assets 
% are pre-indexed as reachable targets within the IRGC war room.
constraint_indexing:constraint_classification(iran_war_room_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE IRGC COMMAND (ROPE)
% The "active war room" is viewed as a Rope: essential coordination 
% infrastructure for unified response and intelligence dominance.
constraint_indexing:constraint_classification(iran_war_room_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Analysts view the potential for regional escalation as a Mountain: 
% an irreducible geopolitical limit that forces a rethinking of US calculations.
constraint_indexing:constraint_classification(iran_war_room_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. MANDATROPHY RESOLUTION HOOK
   ========================================================================== */

/**
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Extraction (0.82) exceeds mandatrophy threshold. The analytical classification
 * as Mountain reflects that the geopolitical confrontation risk is an irreducible
 * limit on US regional strategy regardless of diplomatic posture.
 */

omega_variable(
    omega_us_miscalculation,
    'Will a US miscalculation trigger the regional conflict scenario?',
    'Review of military deployment and theater ratio escalation signals.',
    'Success (deterrence) creates a Scaffold for stability; failure hardens the War Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:interval(iran_war_room_2026, 0, 5).

/* ==========================================================================
   5. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA: 3x2 Matrix)
   ========================================================================== */

% Metrics: theater_ratio (3 time points from June 2025 to Feb 2026)
narrative_ontology:measurement(ir_tr_t0, iran_war_room_2026, theater_ratio, 0, 0.45). % Post-June 2025 conflict
narrative_ontology:measurement(ir_tr_t2, iran_war_room_2026, theater_ratio, 2, 0.55).
narrative_ontology:measurement(ir_tr_t5, iran_war_room_2026, theater_ratio, 5, 0.65). % Feb 2026 activation

% Metrics: extractiveness (3 time points)
narrative_ontology:measurement(ir_ex_t0, iran_war_room_2026, extractiveness, 0, 0.50).
narrative_ontology:measurement(ir_ex_t2, iran_war_room_2026, extractiveness, 2, 0.65).
narrative_ontology:measurement(ir_ex_t5, iran_war_room_2026, extractiveness, 5, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
