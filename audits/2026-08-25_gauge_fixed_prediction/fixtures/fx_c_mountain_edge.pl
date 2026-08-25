% FIXTURE (scratch overlay corpus — NEVER prolog/testsets/).
% Built 2026-08-25 for audits/2026-08-25_gauge_fixed_prediction/.
% Purpose: ATTEMPT the empty cell (c) of the gauge_fixed prediction —
%   local dr_type in {mountain,snare} AND classify_from_restricted =/= dr_type
%   AND gauge_fixed = false.
% Runs through the SAME probe script and the SAME code path as the live leg.
:- module(constraint_fx_c_mountain_edge, []).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    domain_priors:emerges_naturally/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

domain_priors:base_extractiveness(fx_c_mountain_edge, 0.20).
domain_priors:suppression_score(fx_c_mountain_edge, 0.04).
domain_priors:theater_ratio(fx_c_mountain_edge, 0.02).

narrative_ontology:constraint_metric(fx_c_mountain_edge, extractiveness, 0.20).
narrative_ontology:constraint_metric(fx_c_mountain_edge, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(fx_c_mountain_edge, theater_ratio, 0.02).

narrative_ontology:human_readable(fx_c_mountain_edge, "cell-(c) attempt fixture").
narrative_ontology:topic_domain(fx_c_mountain_edge, "fixture/gauge_fixed_prediction").
domain_priors:emerges_naturally(fx_c_mountain_edge).
narrative_ontology:constraint_claim(fx_c_mountain_edge, mountain).
