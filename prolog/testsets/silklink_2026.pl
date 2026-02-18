% ============================================================================
% CONSTRAINT STORY: silklink_2026
% ============================================================================
% Version: 3.5 (Schema-Corrected Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_silklink, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:has_sunset_clause/1,
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
 * * constraint_id: silklink_2026
 * human_readable: SilkLink Syria-Saudi Telecom Project
 * domain: technological/economic
 * * SUMMARY:
 * On Feb 7, 2026, Syria and Saudi Arabia signed the "SilkLink" agreement, 
 * a nearly $1B infrastructure project led by Saudi Telecom Company (STC).
 * It involves laying 4,500km of fiber-optic cables and establishing data centers 
 * to turn Syria into a regional hub connecting Asia and Europe.
 * * KEY AGENTS:
 * - Syrian Citizens/Tech Sector: Subject (Organized - Potential growth)
 * - STC Group: Beneficiary (Institutional - Monopoly access)
 * - Regional Data Carriers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is low (0.25). While providing monopoly revenue, the project 
% restores foundational capacity previously lost to conflict.
domain_priors:base_extractiveness(silklink_2026, 0.25). 

% Suppression is moderate (0.40). The build-out suppresses fragmented 
% legacy infrastructure and high-latency Red Sea routing.
domain_priors:suppression_score(silklink_2026, 0.40).   

% Theater ratio is low (0.15). The "hub" rhetoric is backed by signed, 
% multibillion-dollar binding contracts and lifting of US sanctions.
domain_priors:theater_ratio(silklink_2026, 0.15).       

% Corrected Registry
narrative_ontology:constraint_metric(silklink_2026, extractiveness, 0.25).
narrative_ontology:constraint_metric(silklink_2026, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(silklink_2026, theater_ratio, 0.15).

narrative_ontology:has_sunset_clause(silklink_2026).
% Constraint self-claim (analytical classification)
narrative_ontology:constraint_claim(silklink_2026, scaffold).
narrative_ontology:human_readable(silklink_2026, "SilkLink Syria-Saudi Telecom Project").
narrative_ontology:topic_domain(silklink_2026, "technological/economic").

narrative_ontology:constraint_beneficiary(silklink_2026, stc_group_dominance).
narrative_ontology:constraint_victim(silklink_2026, legacy_intermediary_carriers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SYRIAN CITIZEN (ROPE)
% For individual citizens, SilkLink is a Rope: a coordination tool that
% restores connectivity and economic capacity previously destroyed by conflict.
constraint_indexing:constraint_classification(silklink_2026, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SYRIAN ECONOMY (SCAFFOLD)
% For the recovering state, SilkLink is a Scaffold: an essential temporary 
% structure enabling the build-out of a modern digital economy.
constraint_indexing:constraint_classification(silklink_2026, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))).

% PERSPECTIVE 2: THE REGIONAL DATA MARKET (ROPE)
% For international hyperscalers, the project is a Rope: a new, reliable 
% coordination path to bypass Red Sea congestion and reduce latency.
constraint_indexing:constraint_classification(silklink_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SCAFFOLD)
% Analysts view SilkLink as a Scaffold: a temporary coordination structure
% with a sunset clause, enabling Syria's digital transition at low extraction.
constraint_indexing:constraint_classification(silklink_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

narrative_ontology:interval(silklink_2026, 0, 5).

/* ==========================================================================
   5. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA: 3x2 Matrix)
   ========================================================================== */

% Metrics: extractiveness (Modeling the transition from aid to investment)
narrative_ontology:measurement(sl_ex_t0, silklink_2026, extractiveness, 0, 0.05). % Pre-deal (May 2025)
narrative_ontology:measurement(sl_ex_t2, silklink_2026, extractiveness, 2, 0.15). 
narrative_ontology:measurement(sl_ex_t5, silklink_2026, extractiveness, 5, 0.25). % Signing (Feb 2026)

% Metrics: theater_ratio (Reflecting shift from "promises" to "contracts")
narrative_ontology:measurement(sl_tr_t0, silklink_2026, theater_ratio, 0, 0.55). % High "visionary" rhetoric
narrative_ontology:measurement(sl_tr_t2, silklink_2026, theater_ratio, 2, 0.30).
narrative_ontology:measurement(sl_tr_t5, silklink_2026, theater_ratio, 5, 0.15). % Validated by STC/Flynas

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
