% ============================================================================
% PRODUCT-SITE COHOMOLOGY EXPORT
% ============================================================================
% Runs the first full product-site cohomology computation across the
% 156-context curated product site and exports results to JSON.
%
% Run (from repo root):
%   cd prolog && swipl -g "[stack], [product_site_export], run_product_export, halt" -t "halt(1)"
%
% Output:
%   ../outputs/product_site_orbits.json
%
% Context key format: "{P}_{T}_{E}_{S}"
%   e.g. "powerless_biographical_trapped_local"
% ============================================================================

:- module(product_site_export, [run_product_export/0, run_product_export_to/1]).

:- use_module(covering_analysis).
:- use_module(constraint_indexing).
:- use_module(drl_core).
:- use_module(domain_priors).
:- use_module(data_repair).
:- use_module(library(lists)).

%% run_product_export/0
%  Main entry point. Delegates to run_product_export_to/1 with default path.
run_product_export :-
    run_product_export_to('../outputs/product_site_orbits.json').

%% run_product_export_to(+OutPath)
%  Loads corpus, computes product-site orbits, writes JSON to OutPath.
%  Used by alt_power_transform_test.py to direct output to per-variant files.
run_product_export_to(OutPath) :-
    format(user_error, '[product_export] Loading corpus...~n', []),
    corpus_loader:load_all_testsets,
    covering_analysis:all_corpus_constraints(Cs),
    length(Cs, NTotal),

    % Always use product site for export (site_mode config controls analysis
    % predicates; this export is explicitly product-site only)
    constraint_indexing:site_contexts_product(Ctxs),
    length(Ctxs, NSite),
    format(user_error, '[product_export] ~w constraints x ~w contexts~n', [NTotal, NSite]),

    % Build context keys once (stable order from site_contexts/1)
    maplist(context_key, Ctxs, CtxKeys),

    % Run and time
    get_time(T0),
    setup_call_cleanup(
        open(OutPath, write, S),
        write_product_json(S, Cs, Ctxs, CtxKeys),
        close(S)
    ),
    get_time(T1),
    Elapsed is T1 - T0,
    PerConstraint is 1000 * Elapsed / NTotal,
    format(user_error, '[product_export] Done in ~1fs (~1fms/constraint)~n',
           [Elapsed, PerConstraint]),
    format(user_error, '[product_export] Written to ~w~n', [OutPath]).

/* ================================================================
   JSON OUTPUT
   ================================================================ */

write_product_json(S, Cs, Ctxs, CtxKeys) :-
    format(S, '{~n', []),
    write_entries(S, Cs, Ctxs, CtxKeys),
    format(S, '~n}~n', []).

write_entries(_, [], _, _).
write_entries(S, [C], Ctxs, CtxKeys) :- !,
    write_one_entry(S, C, Ctxs, CtxKeys, false).
write_entries(S, [C|Rest], Ctxs, CtxKeys) :-
    write_one_entry(S, C, Ctxs, CtxKeys, true),
    !,
    write_entries(S, Rest, Ctxs, CtxKeys).

write_one_entry(S, C, Ctxs, CtxKeys, Comma) :-
    % Classify at all site contexts
    maplist(type_at(C), Ctxs, Types),
    % Compute H0/H1
    sort(Types, UniqueTypes),
    (   UniqueTypes = [_]
    ->  H0 = 1, H1 = 0
    ;   H0 = 0, count_disagreeing_pairs(Types, H1)
    ),
    % Write entry
    format(S, '  "~w": {~n', [C]),
    format(S, '    "h0": ~w,~n', [H0]),
    format(S, '    "h1": ~w,~n', [H1]),
    format(S, '    "orbit_signature": [', []),
    write_string_list(S, UniqueTypes),
    format(S, '],~n', []),
    format(S, '    "contexts": {~n', []),
    write_contexts(S, CtxKeys, Types),
    format(S, '    }~n', []),
    (Comma == true -> format(S, '  },~n', []) ; format(S, '  }~n', [])).

/* ================================================================
   HELPERS
   ================================================================ */

%% type_at(+C, +Ctx, -Type)
%  Evaluates the DR presheaf at a single site context.
type_at(C, Ctx, Type) :-
    (   drl_core:dr_type(C, Ctx, T)
    ->  Type = T
    ;   Type = unknown
    ).

%% context_key(+ContextTerm, -Key)
%  Converts context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))
%  to an atom key "P_T_E_S".
context_key(
    context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S)),
    Key
) :-
    atomic_list_concat([P, T, E, S], '_', Key).

%% count_disagreeing_pairs(+TypeVector, -Count)
%  Counts unordered pairs (i,j) with i < j where TypeVector[i] \= TypeVector[j].
%  O(n²) without findall overhead.
count_disagreeing_pairs([], 0).
count_disagreeing_pairs([_], 0).
count_disagreeing_pairs([T|Rest], Count) :-
    count_differs(T, Rest, N),
    count_disagreeing_pairs(Rest, RestCount),
    Count is N + RestCount.

count_differs(_, [], 0).
count_differs(T, [H|Rest], N) :-
    count_differs(T, Rest, N0),
    (T \= H -> N is N0 + 1 ; N = N0).

%% write_string_list(+Stream, +List)
%  Writes a JSON string array body (no brackets): "a", "b", ...
write_string_list(_, []).
write_string_list(S, [X]) :- !,
    format(S, '"~w"', [X]).
write_string_list(S, [X|Xs]) :-
    format(S, '"~w", ', [X]),
    write_string_list(S, Xs).

%% write_contexts(+Stream, +Keys, +Types)
%  Writes context key→type pairs as JSON object body (no braces).
write_contexts(_, [], []).
write_contexts(S, [K], [V]) :- !,
    format(S, '      "~w": "~w"~n', [K, V]).
write_contexts(S, [K|Ks], [V|Vs]) :-
    format(S, '      "~w": "~w",~n', [K, V]),
    write_contexts(S, Ks, Vs).
