% Read-only probe: re-witness §8 of the omega-resolver memo.
% For every constraint the engine labels `extraction_blindness`, check whether
% the MIRROR condition also holds in the same constraint's authored seats:
% an extractive-typed seat at HIGHER power than a functional-typed seat.
% Power convention (report_generator.pl:242): HIGHER d = LOWER power.
%   extraction_blindness fires when extractive d > functional d (extractive lower-power).
%   mirror holds when         extractive d < functional d (extractive higher-power).
% If both hold, the same seats co-license the headline AND its exact inverse.
% No writes. Corpus selected by first argv atom ('live' or a corpus_path dir).
:- initialization(main).
:- [stack].
:- use_module(report_generator).
:- use_module(narrative_ontology).

select_corpus(live) :- !.
select_corpus(Dir) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Dir)).

% extraction_blindness fires for C?  (reuse engine's own detector)
eb_constraint(C) :-
    report_generator:detect_gap_pattern(C, gap(extraction_blindness, _, _)).

% mirror condition over C's authored seat readings
mirror(C) :-
    findall(R, report_generator:seat_type_reading(C, R), Rs),
    member(reading(De, _, ExtT, _),  Rs), report_generator:gap_extractive_type(ExtT),
    member(reading(Df, _, FuncT, _), Rs), report_generator:gap_functional_type(FuncT),
    De < Df, !.            % extractive seat is HIGHER power than functional seat

distinct_types(C, N) :-
    findall(T, ( report_generator:seat_type_reading(C, reading(_,_,T,_)) ), Ts),
    sort(Ts, U), length(U, N).

main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [CorpusAtom|_] -> true ; CorpusAtom = live ),
    select_corpus(CorpusAtom),
    corpus_loader:ensure_corpus_loaded,
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    length(Cs, Total),
    findall(C, (member(C, Cs), eb_constraint(C)), EBs),
    length(EBs, NEB),
    findall(C, (member(C, EBs), mirror(C)), Mirrored),
    length(Mirrored, NMirror),
    findall(N, (member(C, EBs), distinct_types(C, N)), Ns),
    ( Ns = [] -> Avg = 0.0 ; sum_list(Ns, S), length(Ns, L), Avg is S / L ),
    ( NEB =:= 0 -> Pct = 0.0 ; Pct is 100.0 * NMirror / NEB ),
    format("~n=== corpus=~w total=~w extraction_blindness=~w mirror_also=~w (~1f%%) avg_distinct_types=~2f ===~n",
           [CorpusAtom, Total, NEB, NMirror, Pct, Avg]),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
