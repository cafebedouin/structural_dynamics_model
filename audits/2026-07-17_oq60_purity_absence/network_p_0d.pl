% 0d — R3 falsifier: P(network contains >=1 unknown) per leg.
% Builds the undirected constraint_neighbors graph, finds connected components,
% joins each against the census `unknown` set (disposition column of census_<leg>.tsv),
% and reports the per-component and giant-component unknown incidence.
%
% Run per leg (fresh process):
%   cd prolog && swipl -g "consult('<audit>/network_p_0d.pl'), run(testsets, '<audit>/census_testsets.tsv'), halt" -t "halt(1)"

:- use_module(library(lists)).
:- use_module(library(ugraphs)).

leg_setup(Leg) :-
    ( Leg == testsets -> true
    ; retractall(config:param(corpus_path, _)), asserta(config:param(corpus_path, Leg)) ),
    corpus_loader:load_all_testsets,
    cache_registry:clear_all_caches.

% Read the census TSV: collect constraints with disposition == unknown (col 18).
read_unknown_set(TsvFile, UnkSet) :-
    open(TsvFile, read, S),
    read_string(S, _, Str), close(S),
    split_string(Str, "\n", "", Lines),
    findall(Id,
        ( member(L, Lines), L \= "",
          split_string(L, "\t", "", Fs),
          nth0(0, Fs, IdS), IdS \= "constraint",
          nth0(17, Fs, "unknown"),
          atom_string(Id, IdS) ),
        Ids),
    sort(Ids, UnkSet).

corpus_ids(IDs) :- findall(C, corpus_loader:corpus_constraint(C), I0), sort(I0, IDs).

% Undirected edges from constraint_neighbors under the default context.
build_edges(IDs, Edges) :-
    constraint_indexing:default_context(Ctx),
    findall(A-B,
        ( member(A, IDs),
          catch(drl_purity_network:constraint_neighbors(A, Ctx, Ns), _, Ns=[]),
          member(neighbor(B, _, _), Ns),
          member(B, IDs)
        ),
        Dir),
    findall(B-A, member(A-B, Dir), Rev),
    append(Dir, Rev, All),
    sort(All, Edges).

run(Leg, Tsv) :-
    leg_setup(Leg),
    corpus_ids(IDs), length(IDs, NC),
    read_unknown_set(Tsv, Unk), length(Unk, NU),
    build_edges(IDs, Edges),
    vertices_edges_to_ugraph(IDs, Edges, G),
    % connected components
    findall(Comp, ugraph_component(G, Comp), Comps0),
    sort(Comps0, Comps),
    length(Comps, NComp),
    % per-component unknown incidence (dedup: a component with k unknowns counts once)
    findall(Comp, (member(Comp, Comps), once((member(M, Comp), memberchk(M, Unk)))), UnkComps0),
    sort(UnkComps0, UnkComps),
    length(UnkComps, NUnkComp),
    % constraints living in an unknown-containing component
    findall(M, (member(Comp, UnkComps), member(M, Comp)), InUnkComp0),
    sort(InUnkComp0, InUnkComp), length(InUnkComp, NInUnk),
    % giant component (largest)
    findall(Sz-Comp, (member(Comp, Comps), length(Comp, Sz)), Pairs),
    keysort(Pairs, Sorted), last(Sorted, GSz-Giant),
    ( (member(M, Giant), memberchk(M, Unk)) -> GiantHasUnk = yes ; GiantHasUnk = no ),
    ( NComp > 0 -> PComp is NUnkComp / NComp ; PComp = 0 ),
    format(user_error, '~n[0d:~w] constraints=~w unknown=~w components=~w~n', [Leg, NC, NU, NComp]),
    format(user_error, '[0d:~w] components_with_>=1_unknown=~w  P(component has unknown)=~4f~n', [Leg, NUnkComp, PComp]),
    format(user_error, '[0d:~w] constraints_in_unknown_components=~w/~w (~4f)~n', [Leg, NInUnk, NC, NInUnk/NC]),
    format(user_error, '[0d:~w] giant_component_size=~w  giant_contains_unknown=~w~n', [Leg, GSz, GiantHasUnk]).

% one connected component reachable from an unvisited vertex
ugraph_component(G, Comp) :-
    vertices(G, Vs),
    member(V, Vs),
    reachable(V, G, R), sort(R, Comp),
    % canonical: emit each component once (V is its minimum)
    Comp = [Min|_], V == Min.
