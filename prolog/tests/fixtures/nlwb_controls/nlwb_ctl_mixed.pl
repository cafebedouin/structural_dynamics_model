% nlwb_ctl_mixed.pl — OQ-66 agency-gate fixture: THE FILTER PASSES AGENTS.
%
% One REGISTERED non-agent beneficiary plus one UNLISTED (agent-kind) one:
%   raw      natural_law_without_beneficiary — FALSE
%   filtered natural_law_without_beneficiary — FALSE (the agent co-beneficiary survives)
% => no flip.
%
% Mirrors the humane_treatment_standard__absolute_prohibition "moot by inertness"
% case recorded in the two-gate block at narrative_ontology.pl: a registry entry
% releases nothing while an agent co-beneficiary remains. Proves the filter can
% CLEAR as well as flag — a filter that dropped the whole constraint on any
% registered value would pass the flip test and fail here.
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(nlwb_ctl_mixed, 0.80).
domain_priors:emerges_naturally(nlwb_ctl_mixed).

narrative_ontology:constraint_metric(nlwb_ctl_mixed, extractiveness, 0.80).
narrative_ontology:constraint_metric(nlwb_ctl_mixed, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nlwb_ctl_mixed, theater_ratio, 0.40).

narrative_ontology:coordination_type(nlwb_ctl_mixed, extractive_coordination).

narrative_ontology:constraint_beneficiary(nlwb_ctl_mixed, entropic_universe_hypothesis).
narrative_ontology:constraint_beneficiary(nlwb_ctl_mixed, nlwb_ctl_extracting_guild).

narrative_ontology:constraint_claim(nlwb_ctl_mixed, snare).
narrative_ontology:human_readable(nlwb_ctl_mixed,
    "OQ-66 control: registered + unlisted beneficiaries — filter passes the agent, NO flip").
