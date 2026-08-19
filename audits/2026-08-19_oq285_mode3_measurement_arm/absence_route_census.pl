% OQ-285 step 2 — census of the ROUTES by which a seat reaches `unknown` at the
% H1 read site. Read-only (asserts one synthetic control seat, then retracts it).
%
% Routes (read off the code, not the OQ text):
%   a  dr_type_for_stakeholder/3 FAILS  -> seat_type_token/3 if-then-else -> unknown
%   b  dr_type_with_d/4 clause 1 body FAILS -> clause 2 catch-all -> literal unknown
%   c  clause 1 body SUCCEEDS and yields unknown (classify_from_metrics terminal
%      catch-all, or signature integration returning unknown)
%   r  a real (non-unknown) type
%
% Load chain: stack.pl + corpus. Run with corpus_path overlaid via asserta.

:- use_module(library(lists)).

first_clause_body(C, Ctx, D, FinalType) :-
    constraint_indexing:valid_context(Ctx),
    drl_core:base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, Chi),
    drl_core:get_raw_suppression(C, Supp),
    drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, MetricType),
    signature_detection:integrate_signature_with_modal(C, MetricType, FinalType).

% Sub-diagnosis of a route-b failure: name the first failing conjunct.
b_reason(C, Ctx, D, Reason) :-
    (   \+ constraint_indexing:valid_context(Ctx) -> Reason = invalid_context
    ;   \+ drl_core:base_extractiveness(C, _) -> Reason = no_base_extractiveness
    ;   \+ ( drl_core:base_extractiveness(C,_),
             constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, _) )
        -> Reason = no_chi
    ;   \+ drl_core:get_raw_suppression(C, _) -> Reason = no_suppression
    ;   drl_core:get_raw_suppression(C, S), \+ number(S)
        -> Reason = nonnumeric_suppression      % OQ-44 fail-close inside classify
    ;   Reason = other
    ).

seat_route(C, N, Route) :-
    (   \+ stakeholder_seats:derive_directionality_for_stakeholder(C, N, _)
    ->  Route = a(no_directionality)
    ;   stakeholder_seats:stakeholder_context(C, N, Ctx0)
    ->  once(stakeholder_seats:derive_directionality_for_stakeholder(C, N, D)),
        (   once(first_clause_body(C, Ctx0, D, FT))
        ->  ( FT == unknown -> Route = c ; Route = r(FT) )
        ;   once(b_reason(C, Ctx0, D, R)), Route = b(R)
        )
    ;   Route = a(no_context)
    ).

% All non-excluded agent seats (the H1 vector domain) across the corpus.
census(Counts) :-
    findall(Route,
        ( corpus_loader:corpus_constraint(C),
          stakeholder_seats:stakeholder_agent_seats(C, Ns),
          member(N, Ns),
          seat_route(C, N, Route) ),
        Routes),
    msort(Routes, Sorted),
    tally(Sorted, Counts).

tally([], []).
tally([X|Xs], [X-K|Rest]) :-
    collect_same(X, Xs, K0, Tail), K is K0 + 1,
    tally(Tail, Rest).
collect_same(X, [Y|Ys], K, Tail) :- X == Y, !, collect_same(X, Ys, K0, Tail), K is K0+1.
collect_same(_, L, 0, L).

% Coarse buckets for the brief's binary.
bucket(a(_), route_a_derivation_FAILS).
bucket(b(_), route_b_clause2_catchall_unknown).
bucket(c,    route_c_cascade_literal_unknown).
bucket(r(_), route_r_real_type).

summarise :-
    census(Counts),
    format("~n=== ROUTE TALLY (fine) ===~n", []),
    forall(member(K-V, Counts), format("  ~w~t~45| ~d~n", [K, V])),
    findall(B-V, (member(K-V, Counts), bucket(K, B)), Bs),
    keysort(Bs, BsS), sum_buckets(BsS, Sums),
    format("~n=== ROUTE TALLY (coarse) ===~n", []),
    forall(member(K-V, Sums), format("  ~w~t~45| ~d~n", [K, V])),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C),
                           stakeholder_seats:stakeholder_agent_seats(C, Ns),
                           member(_, Ns) ), TotSeats),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("~n  constraints=~d  agent_seats=~d~n", [NC, TotSeats]).

sum_buckets([], []).
sum_buckets([K-V|T], [K-S|R]) :- take_key(K, T, V, S, Rest), sum_buckets(Rest, R).
take_key(K, [K2-V2|T], Acc, S, Rest) :- K == K2, !, Acc2 is Acc+V2, take_key(K, T, Acc2, S, Rest).
take_key(_, L, Acc, Acc, L).
