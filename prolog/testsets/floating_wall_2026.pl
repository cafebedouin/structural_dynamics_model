% ============================================================================
% CONSTRAINT STORY: floating_wall_2026
% ============================================================================
% Version: 3.5 (Schema-Corrected Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_floating_wall, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * * constraint_id: floating_wall_2026
 * human_readable: The East China Sea Maritime Militia Barrier
 * domain: geopolitical/maritime
 * * SUMMARY:
 * China has twice mobilized massive fleets of fishing vessels (up to 2,000 boats) 
 * to form 400km+ "floating walls" near the Sino-Japanese median line. 
 * These maneuvers, occurring between Christmas 2025 and January 2026, demonstrate 
 * a "flash mob" style of maritime command and control.
 * * KEY AGENTS:
 * - International Cargo/Navigators: Subject (Powerless)
 * - Maritime Militia (PAFMM): Beneficiary (Institutional - Asymmetric Control)
 * - Regional Security Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.65). The barrier forces massive cargo re-routing 
% and zigzagging, extracting time and fuel costs from third parties.
domain_priors:base_extractiveness(floating_wall_2026, 0.65). 

% Suppression is very high (0.80). The density of the formation suppresses 
% freedom of navigation and overwhelms adversary radar/sensor fidelity.
domain_priors:suppression_score(floating_wall_2026, 0.80).   

% Theater ratio is moderate (0.30). The "civilian" profile of the ships 
% provides strategic ambiguity while serving as a clear deterrent signal.
domain_priors:theater_ratio(floating_wall_2026, 0.30).       

% Corrected Registry
narrative_ontology:constraint_metric(floating_wall_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(floating_wall_2026, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(floating_wall_2026, theater_ratio, 0.30).

% Constraint self-claim (analytical classification)
narrative_ontology:constraint_claim(floating_wall_2026, tangled_rope).
narrative_ontology:human_readable(floating_wall_2026, "The East China Sea Maritime Militia Barrier").

% Tangled rope structural requirements
domain_priors:requires_active_enforcement(floating_wall_2026).

narrative_ontology:constraint_beneficiary(floating_wall_2026, maritime_militia_hegemony).
narrative_ontology:constraint_victim(floating_wall_2026, international_maritime_transit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CARGO NAVIGATOR (SNARE)
% For commercial shipping, the wall is a Snare: a physical and electronic 
% trap that forces deviations from established trade lanes.
constraint_indexing:constraint_classification(floating_wall_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE STATE COMMAND (ROPE)
% For the PLA, the militia is a Rope: a coordination mechanism to 
% extend national reach through low-cost "gray zone" mobilization.
constraint_indexing:constraint_classification(floating_wall_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analysts view the wall as a Tangled Rope: a dual-use infrastructure 
% where legitimate fishing utility is inextricably knotted with military goals.
constraint_indexing:constraint_classification(floating_wall_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. OMEGA VARIABLES & INTEGRATION HOOKS
   ========================================================================== */

omega_variable(
    omega_floating_wall_escalation,
    'Will the maritime militia barrier become a permanent fixture or trigger direct confrontation?',
    'Analysis of PAFMM deployment frequency and international shipping rerouting costs.',
    'If permanent: Tangled Rope hardens into Snare. If confrontation: Escalation to Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:interval(floating_wall_2026, 0, 10).

/* ==========================================================================
   5. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA: 3x2 Matrix)
   ========================================================================== */

% Metrics: extractiveness (modeling the transition from routine to barrier)
narrative_ontology:measurement(fw_ex_t0, floating_wall_2026, extractiveness, 0, 0.20). % Routine fishing
narrative_ontology:measurement(fw_ex_t5, floating_wall_2026, extractiveness, 5, 0.45). % Dec 2025 Mobilization
narrative_ontology:measurement(fw_ex_t10, floating_wall_2026, extractiveness, 10, 0.65). % Feb 2026 Stabilization

% Metrics: theater_ratio (modeling the ambiguity of the "militia" profile)
narrative_ontology:measurement(fw_tr_t0, floating_wall_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fw_tr_t5, floating_wall_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(fw_tr_t10, floating_wall_2026, theater_ratio, 10, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
