% FIXTURE (scratch overlay corpus — NEVER prolog/testsets/).
% Built 2026-08-25 for audits/2026-08-25_gauge_fixed_prediction/.
% Purpose: ATTEMPT the empty cell (c) of the gauge_fixed prediction —
%   local dr_type in {mountain,snare} AND classify_from_restricted =/= dr_type
%   AND gauge_fixed = false.
% Runs through the SAME probe script and the SAME code path as the live leg.
:- module(constraint_fx_c_snare_high, []).
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

domain_priors:base_extractiveness(fx_c_snare_high, 0.75).
domain_priors:suppression_score(fx_c_snare_high, 0.85).
domain_priors:theater_ratio(fx_c_snare_high, 0.10).

narrative_ontology:constraint_metric(fx_c_snare_high, extractiveness, 0.75).
narrative_ontology:constraint_metric(fx_c_snare_high, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(fx_c_snare_high, theater_ratio, 0.10).

narrative_ontology:human_readable(fx_c_snare_high, "cell-(c) attempt fixture").
narrative_ontology:topic_domain(fx_c_snare_high, "fixture/gauge_fixed_prediction").
domain_priors:requires_active_enforcement(fx_c_snare_high).
narrative_ontology:constraint_beneficiary(fx_c_snare_high, enforcing_authority).
narrative_ontology:constraint_beneficiary(fx_c_snare_high, licensed_incumbents).
narrative_ontology:constraint_victim(fx_c_snare_high, excluded_entrants).
narrative_ontology:constraint_victim(fx_c_snare_high, unlicensed_practitioners).
narrative_ontology:constraint_claim(fx_c_snare_high, rope).
narrative_ontology:coordination_type(fx_c_snare_high, resource_allocation).
