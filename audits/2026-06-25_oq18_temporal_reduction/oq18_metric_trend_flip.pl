% OQ-18: does metric_trend/3's endpoint reduction flip a SERIALIZED cs_verdict?
% metric_trend gates cs_verdict(C, scaffold_suppression_escalating) on
%   metric_trend(C, suppression_requirement, increasing)  ==  endpoint Delta > 0.05.
% cs_verdict is serialized (json_report.pl:570 "cs_verdicts"). Faithful bucket:
% LSQ-fitted total change over the span (slope * span) vs the same 0.05 cut.
% FLIP = endpoint says increasing but faithful-fitted does not (verdict would vanish).
% Live control: a non-monotone series where endpoint=increasing, LSQ-fit=falling.
:- initialization(main).
corpus_dir(Dir) :- ( current_prolog_flag(argv,[D|_]) -> Dir=D ; Dir=testsets ).

series(C, M, Sorted) :-
    findall(T-V, narrative_ontology:measurement(_, C, M, T, V), Ps),
    Ps = [_,_|_], msort(Ps, Sorted).
endpoint_delta(S, D) :- S = [_-V1|_], last(S, _-V2), D is V2-V1.
fitted_total(S, F) :- S=[T1-_|_], last(S,T2-_), Span is T2-T1, drl_composition:linear_slope(S, Sl), F is Sl*Span.
bucket(Delta, increasing) :- Delta > 0.05, !.
bucket(Delta, decreasing) :- Delta < -0.05, !.
bucket(_, stable).

main :-
    corpus_dir(Dir),
    use_module(stack),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    format('~n===== ~w =====~n',[Dir]),
    % live control
    Ctrl = [1-0.50, 2-0.90, 3-0.90, 4-0.55, 5-0.56],
    endpoint_delta(Ctrl, CD), fitted_total(Ctrl, CF), bucket(CD,CB), bucket(CF,CFB),
    ( CB == increasing, CFB \== increasing
    -> format('CONTROL: endpoint Delta=~4f (~w) vs LSQ-fit=~4f (~w) -> FLIP detected (control live)~n',[CD,CB,CF,CFB])
    ;  format('CONTROL FAILED: ~w/~w no flip~n',[CB,CFB]) ),
    % real serialized verdicts gated by metric_trend
    findall(C, ( corpus_loader:corpus_constraint(C),
                 cs_pattern_detection:cs_verdict(C, scaffold_suppression_escalating) ), Cs0),
    sort(Cs0, Cs), length(Cs, N),
    foldl(check, Cs, 0-0, Flip-Seen),
    format('serialized scaffold_suppression_escalating verdicts: ~w  examined=~w  FLIPPED=~w~n',[N,Seen,Flip]),
    halt.
main :- writeln('PROBE FAILED'), halt(1).

check(C, FIn-SIn, FOut-SOut) :-
    SOut is SIn + 1,
    ( series(C, suppression_requirement, S)
    -> endpoint_delta(S, ED), fitted_total(S, FT), bucket(ED, EB), bucket(FT, FB),
       ( EB == increasing, FB \== increasing
       -> FOut is FIn + 1,
          format('  [FLIPPED] ~w  endpoint Delta=~4f(~w) LSQ-fit=~4f(~w)~n',[C,ED,EB,FT,FB])
       ;  FOut = FIn )
    ;  FOut = FIn ).   % <3 pts: metric_trend still fires on 2-pt endpoint; non_mono needs 3 -> note below
