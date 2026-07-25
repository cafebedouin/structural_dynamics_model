% nlwb_ctl_no_beneficiary.pl — OQ-66 agency-gate fixture: THE BASE CASE.
%
% No beneficiary fact at all:
%   raw      natural_law_without_beneficiary — TRUE
%   filtered natural_law_without_beneficiary — TRUE
% => no flip, and snare is blocked in BOTH arms.
%
% Guards the direction the other three cannot: that the swap does not change the
% behaviour of constraints the registry never touches. A filter implemented as
% "always true" would flip nlwb_ctl_nonagent_only correctly and still be wrong;
% this fixture plus nlwb_ctl_agent_only pin both ends.
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(nlwb_ctl_no_beneficiary, 0.80).
domain_priors:emerges_naturally(nlwb_ctl_no_beneficiary).

narrative_ontology:constraint_metric(nlwb_ctl_no_beneficiary, extractiveness, 0.80).
narrative_ontology:constraint_metric(nlwb_ctl_no_beneficiary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nlwb_ctl_no_beneficiary, theater_ratio, 0.40).

narrative_ontology:coordination_type(nlwb_ctl_no_beneficiary, extractive_coordination).

% NO narrative_ontology:constraint_beneficiary/2 fact — that is the point.

narrative_ontology:constraint_claim(nlwb_ctl_no_beneficiary, mountain).
narrative_ontology:human_readable(nlwb_ctl_no_beneficiary,
    "OQ-66 control: no beneficiary at all — nlwb TRUE in both arms, snare blocked both ways").
