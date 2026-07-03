% eps_ctl_clean.pl — Control P fixture (OQ-205 spec §6): the TWO-SIDED arm.
% Three ε sites equal + a matching epsilon_provenance fact — must pass the
% gate flag-free (no drift, NOT in the loud-null census). Loaded through the
% REAL path: corpus_loader consult via corpus_path overlay (fresh process —
% the corpus_loaded/0 guard silently ignores in-process overlay-after-load).
:- multifile
    domain_priors:base_extractiveness/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2.

domain_priors:base_extractiveness(eps_ctl_clean, 0.62).
narrative_ontology:constraint_metric(eps_ctl_clean, extractiveness, 0.62).
narrative_ontology:constraint_metric(eps_ctl_clean, suppression_requirement, 0.5).
narrative_ontology:epsilon_provenance(eps_ctl_clean, 0.62, human, none, hand_authored).
narrative_ontology:human_readable(eps_ctl_clean, "Control P: fully-provenanced clean fixture").
