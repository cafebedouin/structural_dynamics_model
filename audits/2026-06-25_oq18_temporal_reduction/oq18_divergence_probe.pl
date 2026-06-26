% OQ-18: does endpoint drift_velocity diverge from least-squares slope on the
% live corpus AT ALL? (read-only). If max|endpoint - lsq| ~ 0 corpus-wide, then
% for these data shapes the two velocity definitions coincide and NO gate verdict
% can flip under an LSQ-faithful drift_velocity -- 0-flips would be structural,
% not a missed look. KEY ALGEBRA: for 3 evenly-spaced points, lsq slope == endpoint
% slope exactly, so divergence requires >=4 points or uneven spacing.
% Working control: an unevenly-spaced series where endpoint and lsq MUST differ
% in sign (proves the comparator detects divergence -- not a dead zero).

:- initialization(main).
corpus_dir(Dir) :- ( current_prolog_flag(argv,[D|_]) -> Dir=D ; Dir=testsets ).

ep_rate(Series, Re) :- Series=[T1-V1|_], last(Series, T2-V2), D is T2-T1, D>0, Re is (V2-V1)/D.
lsq_rate(Series, Rf) :- drl_composition:linear_slope(Series, Rf).

series(C, Sorted) :-
    findall(T-V, narrative_ontology:measurement(_, C, base_extractiveness, T, V), Ps),
    Ps = [_,_|_], msort(Ps, Sorted).

main :-
    corpus_dir(Dir),
    use_module(stack),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    format('~n===== ~w =====~n',[Dir]),

    % working control: uneven spacing 1,2,100 -> endpoint and lsq differ in sign
    CtrlS = [1-0.10, 2-0.90, 100-0.15],
    ep_rate(CtrlS, CE), lsq_rate(CtrlS, CF),
    ( abs(CE-CF) > 1.0e-6
    -> format('CONTROL (uneven): endpoint=~6f lsq=~6f  DIFFER (comparator live)~n',[CE,CF])
    ;  format('CONTROL FAILED: endpoint=~6f lsq=~6f identical -- comparator dead~n',[CE,CF]) ),

    % point-count histogram + divergence census over corpus
    findall(N-Div,
        ( corpus_loader:corpus_constraint(C),
          series(C, S), length(S, N),
          ep_rate(S, Re), lsq_rate(S, Rf), Div is abs(Re-Rf) ),
        Rows),
    length(Rows, NSeries),
    findall(N, member(N-_, Rows), Ns),
    msort(Ns, NsS), histogram(NsS, Hist),
    findall(D, member(_-D, Rows), Divs),
    ( Divs=[] -> MaxDiv=0.0 ; max_list(Divs, MaxDiv) ),
    include([D]>>(D>1.0e-6), Divs, NonZero), length(NonZero, NNZ),
    format('base_extractiveness series (>=2 pts): ~w~n', [NSeries]),
    format('point-count histogram (N=count): ~w~n', [Hist]),
    format('series where |endpoint - lsq| > 1e-6 : ~w~n', [NNZ]),
    format('max |endpoint - lsq| over corpus     : ~6f~n', [MaxDiv]),
    halt.
main :- writeln('PROBE FAILED'), halt(1).

histogram([], []).
histogram([X|Xs], [X-C|R]) :- partition([Y]>>(Y==X), [X|Xs], Eq, Rest), length(Eq, C), histogram(Rest, R).
