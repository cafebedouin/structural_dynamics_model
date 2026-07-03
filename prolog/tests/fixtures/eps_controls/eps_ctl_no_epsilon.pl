% eps_ctl_no_epsilon.pl — Control P fixture (OQ-205 spec §6, riskiest shape b):
% NO ε at any of the three sites. Asserts the U1 get_true_metric fallback is
% DEAD, not rerouted: through the real load path this constraint must read
% ε = unknown (never a fabricated 0.0), and it belongs to the no-ε census
% bucket (not loud-null — loud-null requires an authored ε).
:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:human_readable/2.

narrative_ontology:constraint_metric(eps_ctl_no_epsilon, suppression_requirement, 0.5).
narrative_ontology:human_readable(eps_ctl_no_epsilon, "Control P: no-epsilon fixture (fallback-dead witness)").
