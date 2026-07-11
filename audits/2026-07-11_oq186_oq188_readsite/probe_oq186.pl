/* OQ-186 A/B probe (pre-registered: PREREG.md Block 5, committed 57159a36).

   Topic A: 3 synthetic slices of one fact — same victim, same beneficiary,
   eps 0.68/0.69/0.68 (0.69 keeps delta-eps=0.01 inside the 0.02 margin;
   0.70 sits float-exactly ON the boundary).
   Topic B: 3 synthetic pairwise-distinct-agent extractive constraints.

   Each synthetic constraint authors one constraint_metric so phantom_subject/1
   passes (the guard is an existence test). Run from prolog/:
     swipl -g "[stack], ['../audits/2026-07-11_oq186_oq188_readsite/probe_oq186.pl'], run_oq186_probe, halt" -t "halt(1)"
*/

:- use_module(probe_harness).

oq186_fixture([
    % Topic A — co-authored slices (shared victim AND beneficiary, near-identical eps)
    narrative_ontology:constraint_metric(oq186_a1, extractiveness, 0.68),
    narrative_ontology:constraint_metric(oq186_a2, extractiveness, 0.69),
    narrative_ontology:constraint_metric(oq186_a3, extractiveness, 0.68),
    narrative_ontology:constraint_beneficiary(oq186_a1, oq186_shared_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186_a2, oq186_shared_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186_a3, oq186_shared_beneficiary),
    narrative_ontology:constraint_victim(oq186_a1, oq186_shared_victim),
    narrative_ontology:constraint_victim(oq186_a2, oq186_shared_victim),
    narrative_ontology:constraint_victim(oq186_a3, oq186_shared_victim),
    % Topic B — genuinely distinct extractive constraints (pairwise-distinct agents)
    narrative_ontology:constraint_metric(oq186_b1, extractiveness, 0.72),
    narrative_ontology:constraint_metric(oq186_b2, extractiveness, 0.66),
    narrative_ontology:constraint_metric(oq186_b3, extractiveness, 0.78),
    narrative_ontology:constraint_beneficiary(oq186_b1, oq186_b1_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186_b2, oq186_b2_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186_b3, oq186_b3_beneficiary),
    narrative_ontology:constraint_victim(oq186_b1, oq186_b1_victim),
    narrative_ontology:constraint_victim(oq186_b2, oq186_b2_victim),
    narrative_ontology:constraint_victim(oq186_b3, oq186_b3_victim)
]).

run_oq186_probe :-
    oq186_fixture(Facts),
    probe_harness:with_asserted(Facts, probe_body).

probe_body :-
    constraint_indexing:default_context(Ctx),
    format("== OQ-186 A/B probe (context: ~w) ==~n", [Ctx]),
    forall(member(C, [oq186_a1, oq186_a2, oq186_a3, oq186_b1, oq186_b2, oq186_b3]),
           ( drl_purity_network:constraint_neighbors(C, Ctx, Ns),
             format("~w neighbors: ~w~n", [C, Ns]) )).
