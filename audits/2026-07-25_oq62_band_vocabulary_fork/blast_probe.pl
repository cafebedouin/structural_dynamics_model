% Exact blast radius of the OQ-62 guard at fpn_report.
% Reproduces run_fpn_report's row construction verbatim, then asks: among the
% rows that survive the `IP >= 0.0` filter, how many band from an ABSENCE token
% (-1.0 sentinel or `unknown`) rather than from a measured purity value?

tok(V, non_number) :- \+ number(V), !.
tok(V, negative)   :- V < 0.0, !.
tok(_, value).

main :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    findall(C, (narrative_ontology:constraint_claim(C, _), \+ is_list(C)), Raw),
    sort(Raw, Cs),
    drl_fpn:fpn_run(Cs, Ctx, R),
    format("fpn_run: ~w~n", [R]),

    % verbatim copy of the row filter at fpn_report.pl:47-55
    findall(row(C, IP, OH, FP),
        (   member(C, Cs),
            fpn_report:fpn_intrinsic_safe(C, IP),
            IP >= 0.0,
            fpn_report:one_hop_ep_safe(C, Ctx, OH),
            fpn_report:fpn_ep_safe(C, Ctx, FP)
        ), Rows),
    length(Rows, NRows),
    format("rows surviving IP>=0.0 filter: ~w~n", [NRows]),

    findall(T, (member(row(_,_,OH,_), Rows), tok(OH, T)), OHTs),
    findall(T, (member(row(_,_,_,FP), Rows), tok(FP, T)), FPTs),
    msort(OHTs, OHS), clumped(OHS, OHC),
    msort(FPTs, FPS), clumped(FPS, FPC),
    format("one_hop EP token mix: ~w~n", [OHC]),
    format("fpn     EP token mix: ~w~n", [FPC]),

    % What each absence-bearing row bands TODAY (pre-guard) vs after the guard.
    forall(
        (   member(row(C, _, OH, FP), Rows),
            ( \+ number(OH) ; OH < 0.0 ; \+ number(FP) ; FP < 0.0 )
        ),
        (   fpn_report:purity_zone(OH, Z1),
            fpn_report:purity_zone(FP, Z2),
            ( Z1 \= Z2 -> M = 'MIGRATION-ROW' ; M = '.' ),
            format("  ~w~n    OH=~w -> ~w | FP=~w -> ~w   ~w~n", [C, OH, Z1, FP, Z2, M])
        )),

    % Migration count today (the number report_zone_migrations/1 prints).
    findall(C, (member(row(C,_,OH,FP), Rows),
                fpn_report:purity_zone(OH, Z1), fpn_report:purity_zone(FP, Z2),
                Z1 \= Z2), Migs),
    length(Migs, NM),
    format("zone migrations TODAY: ~w~n", [NM]).
