% nlwb_ctl_nonagent_only.pl — OQ-66 agency-gate fixture: THE FLIP.
%
% Sole beneficiary is a REGISTERED non_agent_beneficiary/1 value, so:
%   raw      natural_law_without_beneficiary — FALSE (a beneficiary fact exists)
%   filtered natural_law_without_beneficiary — TRUE  (agent_beneficiary/2 drops it)
% Metrics open the snare gate with MARGIN against the three config floors
% (snare_chi_floor 0.66 / snare_epsilon_floor 0.46 / snare_suppression_floor 0.60),
% so the raw arm classifies snare and the filtered arm is snare-blocked.
%
% This fixture asserts the FILTER'S SEMANTICS (a registered value is invisible to
% agent_beneficiary/2), not the absence of divergence. It uses the REAL registry
% value entropic_universe_hypothesis — the registry is never mutated for the test.
%
% Metrics are identical across all four nlwb_ctl_* fixtures; the ONLY variable is
% beneficiary composition. That is what makes nlwb_ctl_agent_only a two-sided
% control rather than a second positive.
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(nlwb_ctl_nonagent_only, 0.80).
domain_priors:emerges_naturally(nlwb_ctl_nonagent_only).
% NO domain_priors:requires_active_enforcement/1 — nlwb requires its absence.

narrative_ontology:constraint_metric(nlwb_ctl_nonagent_only, extractiveness, 0.80).
narrative_ontology:constraint_metric(nlwb_ctl_nonagent_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nlwb_ctl_nonagent_only, theater_ratio, 0.40).

% OQ-60: synthetic constraints needing scorable purity must AUTHOR coordination_type
% (absent -> no Boltzmann floor -> purity unknown, not a number).
narrative_ontology:coordination_type(nlwb_ctl_nonagent_only, extractive_coordination).

% The registered proposition-kind value — the whole point of this fixture.
narrative_ontology:constraint_beneficiary(nlwb_ctl_nonagent_only, entropic_universe_hypothesis).

narrative_ontology:constraint_claim(nlwb_ctl_nonagent_only, snare).
narrative_ontology:human_readable(nlwb_ctl_nonagent_only,
    "OQ-66 control: registered non-agent beneficiary ONLY — raw nlwb false, filtered nlwb true (the flip)").
