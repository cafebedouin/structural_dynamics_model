% eps_ctl_missing.pl — Control P fixture (OQ-205 spec §6): dual authored ε,
% NO epsilon_provenance — must surface in the missing-provenance (loud-null)
% census AT this constraint. Designed to trip the checker; that is its job.
:- multifile
    domain_priors:base_extractiveness/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(eps_ctl_missing, 0.48).
narrative_ontology:constraint_metric(eps_ctl_missing, extractiveness, 0.48).
narrative_ontology:constraint_metric(eps_ctl_missing, suppression_requirement, 0.5).
narrative_ontology:human_readable(eps_ctl_missing, "Control P: provenance-less dual-epsilon fixture").
