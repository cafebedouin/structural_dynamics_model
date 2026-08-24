% OQ-356 refined discriminator. The throw needs effective_purity to SUCCEED with a
% non-number. A purity_score that FAILS makes the conjunct fail, the member is
% skipped, and no arithmetic happens — safe. Conflating the two is exactly the
% error the OQ-60 sweep made, and that the first version of this probe repeated.
:- use_module(library(lists)).

run_gcprobe2 :-
    corpus_loader:load_all_testsets,
    giant_component_analysis:all_corpus_constraints(Cs),
    length(Cs, NC),
    constraint_indexing:default_context(Ctx),
    giant_component_analysis:precompute_all_edges(Cs, Ctx),
    config:param(network_coupling_threshold, T),
    giant_component_analysis:edges_at_threshold(T, Edges),
    giant_component_analysis:build_adjacency_facts(Edges),
    giant_component_analysis:compute_components(Cs, Comps),
    (   Comps = [component(GCSize, GCMembers)|_] -> true ; GCSize = 0, GCMembers = [] ),
    % (a) purity_score SUCCEEDS with a non-number  -> the dangerous class
    findall(C, ( member(C, GCMembers),
                 catch(purity_scoring:purity_score(C, P), _, fail), \+ number(P) ), SuccUnk),
    length(SuccUnk, NSuccUnk),
    % (b) purity_score FAILS outright -> safe (conjunct fails, member skipped)
    findall(C, ( member(C, GCMembers),
                 \+ catch(purity_scoring:purity_score(C, _), _, fail) ), FailP),
    length(FailP, NFailP),
    % (c) what count_by_action_band ACTUALLY sees: effective_purity succeeding non-numeric
    findall(C, ( member(C, GCMembers),
                 catch(drl_purity_network:effective_purity(C, Ctx, EP, _), _, fail),
                 \+ number(EP) ), EffUnk),
    length(EffUnk, NEffUnk),
    format('GCPROBE2 ~w ~w ~w ~w ~w~n', [NC, GCSize, NSuccUnk, NFailP, NEffUnk]).
