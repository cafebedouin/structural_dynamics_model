/* OQ-186 regression: the contamination network forms identical-looking
   shared-agent edges for co-authored slices of one fact and for genuinely
   distinct constraints — node independence is NOT expressible Prolog-side
   (deduplicate_neighbors keeps one edge per pair, so "shares both sides"
   cannot ride an edge label) and is carried at the Python read site
   (shared/independence.py). These tests pin the Prolog half of that
   division of labor: the edges DO form for slices (so the read-site caveat
   has something to caveat), do NOT form for distinct-agent topics, and the
   OQ-95 phantom guard is unaffected by the fixture idiom.

   Fixture mirrors the witnessed A/B probe
   (audits/2026-07-11_oq186_oq188_readsite/probe_oq186.{pl,log}; PREREG
   Block 5, commit 57159a36). Each synthetic constraint authors one
   constraint_metric so phantom_subject/1 passes — WITHOUT it these tests
   would measure the phantom filter, not edge formation (the
   slice_clique_fires positive control exists to catch exactly that).

   Run: cd prolog && swipl -g "[stack], [tests/test_oq186_common_cause_clique], run_tests, halt" -t "halt(1)"
*/

:- begin_tests(oq186_common_cause_clique).

% Topic A — three co-authored slices: same beneficiary, same victim,
% near-identical eps. Topic B — three pairwise-distinct-agent extractive
% constraints. Namespaced agents so no collision with any loaded corpus.
fixture_facts([
    narrative_ontology:constraint_metric(oq186t_a1, extractiveness, 0.68),
    narrative_ontology:constraint_metric(oq186t_a2, extractiveness, 0.69),
    narrative_ontology:constraint_metric(oq186t_a3, extractiveness, 0.68),
    narrative_ontology:constraint_beneficiary(oq186t_a1, oq186t_shared_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186t_a2, oq186t_shared_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186t_a3, oq186t_shared_beneficiary),
    narrative_ontology:constraint_victim(oq186t_a1, oq186t_shared_victim),
    narrative_ontology:constraint_victim(oq186t_a2, oq186t_shared_victim),
    narrative_ontology:constraint_victim(oq186t_a3, oq186t_shared_victim),
    narrative_ontology:constraint_metric(oq186t_b1, extractiveness, 0.72),
    narrative_ontology:constraint_metric(oq186t_b2, extractiveness, 0.66),
    narrative_ontology:constraint_metric(oq186t_b3, extractiveness, 0.78),
    narrative_ontology:constraint_beneficiary(oq186t_b1, oq186t_b1_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186t_b2, oq186t_b2_beneficiary),
    narrative_ontology:constraint_beneficiary(oq186t_b3, oq186t_b3_beneficiary),
    narrative_ontology:constraint_victim(oq186t_b1, oq186t_b1_victim),
    narrative_ontology:constraint_victim(oq186t_b2, oq186t_b2_victim),
    narrative_ontology:constraint_victim(oq186t_b3, oq186t_b3_victim)
]).

setup_fixture :-
    fixture_facts(Facts),
    forall(member(F, Facts), assertz(F)),
    cache_registry:clear_all_caches.

cleanup_fixture :-
    fixture_facts(Facts),
    forall(member(F, Facts), retractall(F)),
    cache_registry:clear_all_caches.

% (a) Positive control: every topic-A slice sees BOTH other slices as
% shared-agent neighbors — the full 3-clique. This is what proves the
% B-topic emptiness below is a discrimination result, not a dead probe
% (and that the fixture's constraint_metric facts kept the members past
% the phantom filter).
test(slice_clique_fires,
     [ setup(setup_fixture), cleanup(cleanup_fixture) ]) :-
    constraint_indexing:default_context(Ctx),
    forall(member(C-Others,
                  [ oq186t_a1-[oq186t_a2, oq186t_a3],
                    oq186t_a2-[oq186t_a1, oq186t_a3],
                    oq186t_a3-[oq186t_a1, oq186t_a2] ]),
           ( drl_purity_network:constraint_neighbors(C, Ctx, Ns),
             forall(member(O, Others), memberchk(neighbor(O, _, _), Ns)) )).

% (b) Genuinely distinct topic: pairwise-distinct agents form no agent edges.
test(distinct_topic_silent,
     [ setup(setup_fixture), cleanup(cleanup_fixture) ]) :-
    constraint_indexing:default_context(Ctx),
    forall(member(C, [oq186t_b1, oq186t_b2, oq186t_b3]),
           ( drl_purity_network:constraint_neighbors(C, Ctx, Ns),
             Ns == [] )).

% (c) The OQ-95 phantom guard still holds under this fixture idiom: a
% zero-fact atom acquires no neighbors even while the fixture is asserted.
test(phantom_guard_still_holds,
     [ setup(setup_fixture), cleanup(cleanup_fixture) ]) :-
    constraint_indexing:default_context(Ctx),
    drl_purity_network:constraint_neighbors(
        oq186t_phantom__does_not_exist, Ctx, Ns),
    Ns == [].

:- end_tests(oq186_common_cause_clique).
