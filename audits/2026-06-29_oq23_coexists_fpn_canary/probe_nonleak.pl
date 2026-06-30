% Deliverable 2: categorize WHY each eligible coexists_with pair does NOT leak.
% Mechanism model predicts exactly two reasons:
%   (a) not_coupled         — no affects_constraint edge (side channel absent)
%   (b) donor_zero_strength — coupled, but lower-purity donor's dr_type has
%                             contamination strength 0 (mountain/unknown)
% Anything else => unexplained (a hole in my mechanism model — must surface).
:- initialization((catch(run,E,(print_message(error,E),halt(2))), halt(0))).

tcfc(X) :- X = test_coexists_fpn_canary.

run :-
    ( getenv('CORPUS', CDir), CDir \== '' ->
        retractall(config:param(corpus_path, _)),
        asserta(config:param(corpus_path, CDir)) ; true ),
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    test_coexists_fpn_canary:copresent_pairs(coexists_with, Pairs),
    findall(P, (member(P, Pairs), test_coexists_fpn_canary:pair_eligible(P)), Elig),
    length(Elig, NE),
    findall(P, (member(P, Elig), test_coexists_fpn_canary:pair_leak(P, Ctx, _)), Leaked),
    length(Leaked, NL),
    subtract(Elig, Leaked, NonLeak),
    length(NonLeak, NNL),
    format("~n== eligible=~w leaked=~w non-leaking-eligible=~w ==~n", [NE, NL, NNL]),
    findall(R, (member(P, NonLeak), reason(P, Ctx, R)), Reasons),
    % tallies
    aggregate_all(count, member(not_coupled, Reasons), NC),
    findall(DT, member(donor_zero_strength(DT), Reasons), DTs0), msort(DTs0, DTs),
    aggregate_all(count, member(donor_zero_strength(_), Reasons), NZ),
    aggregate_all(count, member(unexplained(_,_,_), Reasons), NU),
    format("  not_coupled            : ~w~n", [NC]),
    format("  donor_zero_strength    : ~w  (donor types: ~w)~n", [NZ, DTs]),
    format("  unexplained            : ~w~n", [NU]),
    ( NU =:= 0
    ->  format("  => mechanism model COMPLETE: every non-leaking eligible pair explained~n")
    ;   format("  !!! mechanism model INCOMPLETE — unexplained pairs:~n"),
        forall((member(P, NonLeak), reason(P, Ctx, unexplained(DT,S,D))),
               format("    ~w donorType=~w strength=~w ~w~n", [P, DT, S, D])) ).

reason(A-B, Ctx, Reason) :-
    ( \+ test_coexists_fpn_canary:pair_coupled(A-B, Ctx)
    ->  Reason = not_coupled
    ;   purity_scoring:purity_score(A, PA),
        purity_scoring:purity_score(B, PB),
        ( PA < PB -> Donor = A ; Donor = B ),
        ( drl_core:dr_type(Donor, Ctx, DT) -> true ; DT = unknown ),
        ( drl_purity_network:type_contamination_strength(DT, S) -> true ; S = 0.0 ),
        ( S =:= 0.0 -> Reason = donor_zero_strength(DT)
        ;              Reason = unexplained(DT, S, donor(Donor)) )
    ).
