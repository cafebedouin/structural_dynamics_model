% ============================================================================
% CONSTRAINT STORY: open_source_commons
% ============================================================================
:- module(constraint_genuine_community, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* 1. NARRATIVE CONTEXT */
/**
 * constraint_id: open_source_commons
 * human_readable: The Mutual Garden
 * domain: social/technological
 * SUMMARY: A high-trust coordination environment where value is shared. 
 * KEY AGENTS: Contributor (Powerless/Mobile), The Project (Institutional/Beneficiary).
 */

/* 2. BASE PROPERTIES */
narrative_ontology:constraint_claim(open_source_commons, rope).

domain_priors:base_extractiveness(open_source_commons, 0.05). % Near Mountain-low extraction
domain_priors:suppression_score(open_source_commons, 0.10).   % High exit options
domain_priors:theater_ratio(open_source_commons, 0.15).       % Low performativity

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(open_source_commons, extractiveness, 0.05).
narrative_ontology:constraint_metric(open_source_commons, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(open_source_commons, theater_ratio, 0.15).

/* 3. INDEXED CLASSIFICATIONS */
% PERSPECTIVE 1: THE CONTRIBUTOR (ROPE)
constraint_indexing:constraint_classification(open_source_commons, rope, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(global))).

/* 5. GENERATIVE COMMENTARY */
/**
 * LOGIC RATIONALE: 
 * This is a "Pure Rope." Because extraction is near zero and theater is low, 
 * the Subject and Beneficiary agree on the classification. There is no 
 * Perspectival Gap because the constraint serves the agent as much as the group.
 */

/* 7. INTEGRATION HOOKS */
narrative_ontology:interval(open_source_commons, 0, 10).

/* 8. TEMPORAL DATA (Flat line, no drift) */
narrative_ontology:measurement(osc_tr_t0, open_source_commons, theater_ratio, 0, 0.15).
narrative_ontology:measurement(osc_tr_t10, open_source_commons, theater_ratio, 10, 0.15).
narrative_ontology:measurement(osc_ex_t0, open_source_commons, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(osc_ex_t10, open_source_commons, base_extractiveness, 10, 0.05).

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (open_source_commons)
% ============================================================================
constraint_beneficiary(open_source_commons, open_source_contributors).
constraint_victim(open_source_commons, none).

% --- Analytical perspective classification ---
% chi = 0.05 * 1.15 (analytical) * 1.2 (global) = 0.069
% Classification: scaffold
constraint_indexing:constraint_classification(open_source_commons, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PROJECT MAINTAINER (ROPE)
% Institutional actors use the commons as coordination infrastructure.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

omega_variable(
    omega_oss_sustainability,
    "Can open-source commons survive the 'Maintainer Burnout' problem without introducing extractive funding mechanisms?",
    "Analysis of long-term sustainability models (sponsorships, foundations, cooperatives) vs. maintainer attrition rates.",
    "If sustainable: Remains a pure Rope. If burnout prevails: Drifts toward a Scaffold requiring external support.",
    confidence_without_resolution(medium)
).
