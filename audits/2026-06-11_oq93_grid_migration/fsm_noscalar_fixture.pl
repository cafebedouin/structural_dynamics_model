% FSM number/1-guard positive control (post-merge witness, 2026-06-11):
% satisfies every FSM gate EXCEPT it authors NO suppression scalar — so
% get_raw_suppression returns the OQ-44 `unknown` sentinel at the clause.
:- module(constraint_fsm_noscalar_fixture, []).
:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- multifile domain_priors:base_extractiveness/2, domain_priors:emerges_naturally/1,
             narrative_ontology:constraint_metric/3, narrative_ontology:constraint_beneficiary/2,
             narrative_ontology:interval/3.
domain_priors:base_extractiveness(fsm_noscalar_fixture, 0.20).
narrative_ontology:constraint_metric(fsm_noscalar_fixture, extractiveness, 0.20).
domain_priors:emerges_naturally(fsm_noscalar_fixture).
narrative_ontology:constraint_beneficiary(fsm_noscalar_fixture, landed_gentry).
narrative_ontology:interval(fsm_noscalar_fixture, 0, 10).
