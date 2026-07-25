% nlwb_ctl_agent_only.pl — OQ-66 agency-gate fixture: THE TWO-SIDED ARM.
%
% Identical metrics to nlwb_ctl_nonagent_only; the ONLY difference is that its
% sole beneficiary is UNLISTED (default-agent, per the two-gate principle's
% fail-open-to-status-quo default). So:
%   raw      natural_law_without_beneficiary — FALSE
%   filtered natural_law_without_beneficiary — FALSE (the value survives the filter)
% => no flip.
%
% This is what makes the flip at nlwb_ctl_nonagent_only attributable to REGISTRY
% MEMBERSHIP rather than to the predicate swap itself. Without this arm, a flip
% could equally be an artifact of the abolish/assertz redefinition.
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(nlwb_ctl_agent_only, 0.80).
domain_priors:emerges_naturally(nlwb_ctl_agent_only).

narrative_ontology:constraint_metric(nlwb_ctl_agent_only, extractiveness, 0.80).
narrative_ontology:constraint_metric(nlwb_ctl_agent_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nlwb_ctl_agent_only, theater_ratio, 0.40).

narrative_ontology:coordination_type(nlwb_ctl_agent_only, extractive_coordination).

% Unlisted value => agent-kind by default. Deliberately NOT in the registry.
narrative_ontology:constraint_beneficiary(nlwb_ctl_agent_only, nlwb_ctl_extracting_guild).

narrative_ontology:constraint_claim(nlwb_ctl_agent_only, snare).
narrative_ontology:human_readable(nlwb_ctl_agent_only,
    "OQ-66 control: unlisted (agent-kind) beneficiary only — same metrics, NO flip").
