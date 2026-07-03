% eps_ctl_drifted.pl — Control P fixture (OQ-205 spec §6): epsilon_provenance
% ValueAsWritten (0.71) != the live constraint_metric value (0.51) — must
% raise the three-site drift error AT this constraint. Designed to trip the
% checker; that is its job.
:- multifile
    domain_priors:base_extractiveness/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(eps_ctl_drifted, 0.51).
narrative_ontology:constraint_metric(eps_ctl_drifted, extractiveness, 0.51).
narrative_ontology:constraint_metric(eps_ctl_drifted, suppression_requirement, 0.5).
narrative_ontology:epsilon_provenance(eps_ctl_drifted, 0.71, human, none, hand_authored).
narrative_ontology:human_readable(eps_ctl_drifted, "Control P: drifted ValueAsWritten fixture").
