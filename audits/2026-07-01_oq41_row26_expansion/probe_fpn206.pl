% OQ-41 row-26 expansion — drl_fpn.pl:206 Immunity=0.5 (compute-failed fallback, LIVE).
% Fires when fpn_type_cache(C,Ctx,_) is absent, i.e. dr_type FAILED during precompute AND the
% constraint reached the Neighbors\=[] , IP>=0 branch.  pred-success is NOT a firing witness here
% (fpn_compute_ep succeeds either way), so drl_fpn.pl:206 is patched with a firing marker
% ( assertz(user:tw_fpn206_fired(C)) ) for this run.  Two witnesses:
%   LIVE   : natural fpn_run over the corpus — how many distinct constraints fire :206.
%   CONTROL: force a type-cache miss on a neighboured IP>=0 constraint -> :206 MUST fire (probe wiring).
:- dynamic tw_fpn206_fired/1.
:- initialization(main).

main :-
    [stack],
    ( getenv('CORPUS_OVERLAY', Dir), Dir \== ''
    -> retractall(config:param(corpus_path, _)),
       asserta(config:param(corpus_path, Dir)),
       format("OVERLAY corpus_path=~w~n", [Dir])
    ;  true ),
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    retractall(tw_fpn206_fired(_)),
    % ---- LIVE: natural fpn_run over the live corpus ----
    ( catch(drl_fpn:fpn_run(Ctx, R), E, (R = threw(E))) -> true ; R = failed ),
    ( setof(C, tw_fpn206_fired(C), Fired) -> length(Fired, NF) ; NF = 0, Fired = [] ),
    format("LIVE fpn:206 natural fires: ~w distinct constraints  result=~w~n", [NF, R]),
    ( NF =< 12 -> format("  fired: ~w~n", [Fired]) ; true ),
    % ---- POSITIVE CONTROL: force type-cache miss on a neighboured IP>=0 constraint ----
    retractall(tw_fpn206_fired(_)),
    ( ( drl_fpn:fpn_neighbors_cache(Cc, Ctx, [_|_]),
        drl_fpn:fpn_intrinsic(Cc, IP), IP >= 0.0,
        drl_fpn:fpn_type_cache(Cc, Ctx, _) )
    -> retractall(drl_fpn:fpn_type_cache(Cc, Ctx, _)),
       catch(drl_fpn:fpn_compute_ep(Cc, Ctx, _), _, true),
       ( tw_fpn206_fired(Cc)
       -> format("POSITIVE CONTROL: forced type-cache miss on ~w -> fpn:206 FIRED (probe detects)~n", [Cc])
       ;  format("POSITIVE CONTROL: marker ABSENT after forced miss on ~w -- probe MIS-WIRED, ESCALATE~n", [Cc]) )
    ;  format("POSITIVE CONTROL: no neighboured IP>=0 typed constraint available -- control vacuous, ESCALATE~n") ),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
