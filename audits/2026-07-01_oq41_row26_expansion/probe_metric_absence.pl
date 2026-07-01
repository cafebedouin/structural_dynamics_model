% OQ-41 row-26 expansion — metric-absence sites (covering:490, gap:120, omega1:102 + sibs).
% Three witnesses per metric: (1) reject-guard pre-test (does the metric-presence guard admit a
% bogus no-data atom?); (2) would-fire count over the LIVE corpus (genuine metric-absence);
% (3) must-fire control for covering:490 — classify_at_interpolated/4 SUCCEEDS on a metric-absent
% atom, which for an `... -> true ; X=Default` guard is a firing witness (BaseEps can only bind via
% the default branch).  Run with an overlaid corpus_path to sweep testsets/ and kernel_v1.
:- initialization(main).

count_corpus(N) :- findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, N).

absent_count(MetricName, N, Sample) :-
    findall(C, ( corpus_loader:corpus_constraint(C),
                 \+ narrative_ontology:constraint_metric(C, MetricName, _) ), L),
    length(L, N),
    ( L = [Sample|_] -> true ; Sample = none ).

main :-
    [stack],
    % optional corpus overlay (asserta so it wins over config.pl's default first clause)
    ( getenv('CORPUS_OVERLAY', Dir), Dir \== ''
    -> retractall(config:param(corpus_path, _)),
       asserta(config:param(corpus_path, Dir)),
       format("OVERLAY corpus_path=~w~n", [Dir])
    ;  true ),
    corpus_loader:ensure_corpus_loaded,
    count_corpus(NTot),
    format("CORPUS-SIZE ~w~n", [NTot]),
    config:param(extractiveness_metric_name, ExtName),
    config:param(suppression_metric_name, SuppName),
    config:param(theater_metric_name, TheaterName),

    % (1) reject-guard pre-test — bogus no-data atom the guard must NOT admit
    BogusC = tw_bogus_no_data_atom_xyz,
    ( narrative_ontology:constraint_metric(BogusC, ExtName, _)
    -> format("PRE-TEST guard(~w): SUCCEEDS on bogus -> else-branch DEAD~n", [ExtName])
    ;  format("PRE-TEST guard(~w): REJECTS bogus -> reject-guard (else reachable-if-called)~n", [ExtName]) ),

    % (2) would-fire counts over the live corpus
    absent_count(ExtName, NExt, SExt),
    format("WOULD-FIRE ext(0.5)     : ~w / ~w lack authored ~w  (sample: ~w)~n", [NExt, NTot, ExtName, SExt]),
    absent_count(SuppName, NSupp, SSupp),
    format("WOULD-FIRE supp(0)      : ~w / ~w lack authored ~w  (sample: ~w)~n", [NSupp, NTot, SuppName, SSupp]),
    absent_count(TheaterName, NTh, STh),
    format("WOULD-FIRE theater(0.0) : ~w / ~w lack authored ~w  (sample: ~w)~n", [NTh, NTot, TheaterName, STh]),

    % (3) must-fire control for covering:490 (the only metric-absence site reachable-if-called
    % from another module).  Drive on a real metric-absent atom if one exists, else the bogus atom.
    ( absent_count(ExtName, _, AbsC), AbsC \== none -> true ; AbsC = BogusC ),
    ( catch(covering_analysis:classify_at_interpolated(AbsC, 2.0, 1.0, T490), E490, (T490 = threw(E490)))
    -> ( T490 = threw(_)
       -> format("MUST-FIRE covering:490  : THREW on ~w (~w) -- ESCALATE~n", [AbsC, T490])
       ;  format("MUST-FIRE covering:490  : classify_at_interpolated(~w,2.0,1.0)=~w  (success => BaseEps=0.5 default fired)~n", [AbsC, T490]) )
    ;  format("MUST-FIRE covering:490  : FAILED on ~w -- probe mis-wired, ESCALATE~n", [AbsC]) ),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
