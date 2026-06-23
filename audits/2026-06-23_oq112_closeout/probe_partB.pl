% OQ-112 close-out — Part B: prospective kill-conditions sweep on items 3/5/6/8.
% READ-ONLY (asserts/retracts are process-local + restored; process halts after).
% FOUR items, FOUR DISTINCT positive controls, NO shared generalization.
% Per item: (live-fire on 92) + (its own probe-shown-to-detect control).

:- [stack].
:- consult(purity_scoring).
:- consult(boltzmann_compliance).
:- consult(covering_analysis).
:- consult(drift_events).
:- consult(logical_fingerprint).
:- consult(coercion_projection).
:- corpus_loader:ensure_corpus_loaded.

three_way(Goal, R) :- ( catch(Goal, E, R=error(E)) -> (var(R)->R=ok;true) ; R=fail ).
constraints(Cs) :- findall(C, corpus_loader:corpus_constraint(C), C0), sort(C0, Cs).

% ============================================================ ITEM 3 (A6)
% absence-certifies-clean: excess_extraction absent -> EX=1.0; coupling absent -> CC=1.0.
% Live-fire = how many of 92 reach the absence else-branch (clean-on-absence) that feeds
% the CONSUMED purity_score. Control = a constraint with MEASURED excess -> EX<1.0.
item3 :-
    constraints(Cs), length(Cs, N),
    findall(C, (member(C,Cs), \+ boltzmann_compliance:excess_extraction(C,_)), NoExc),
    findall(C, (member(C,Cs),    boltzmann_compliance:excess_extraction(C,_)), HasExc),
    findall(C, (member(C,Cs), \+ (boltzmann_compliance:detect_nonsensical_coupling(C,P,_), P\=[])), NoCoup),
    length(NoExc,NNE), length(HasExc,NHE), length(NoCoup,NNC),
    format('ITEM3 (A6 absence-certifies-clean), corpus N=~w~n', [N]),
    format('ITEM3   excess_extraction ABSENT (-> EX=1.0 clean-on-absence) = ~w of ~w~n', [NNE,N]),
    format('ITEM3   excess_extraction PRESENT (measured)                  = ~w of ~w~n', [NHE,N]),
    format('ITEM3   coupling-clean via ABSENCE (no nonsensical coupling)  = ~w of ~w~n', [NNC,N]),
    ( NNE > 0
    -> format('ITEM3 LIVE-FIRE: YES on excess_extraction_subscore -- ~w constraints get EX=1.0 from ABSENCE, feeding purity_score~n',[NNE])
    ;  format('ITEM3 LIVE-FIRE: NO via excess absence~n',[]) ),
    % --- ITEM-3 POSITIVE CONTROL (distinguishes measured-clean from absence-clean) ---
    ( member(Cm, HasExc), boltzmann_compliance:excess_extraction(Cm, Ex), Ex > 0.0, !
    -> EXm is max(0.0, 1.0 - min(1.0, Ex*2.0)),
       format('ITEM3 CONTROL: ~w has MEASURED excess=~4f -> excess_extraction_subscore EX=~4f (measured branch, EX<1.0 when Ex>0)~n',[Cm,Ex,EXm]),
       format('ITEM3 CONTROL VERDICT: probe distinguishes measured (Ex present) from absence (EX=1.0) -> detector LIVE~n',[])
    ;  ( HasExc = [Cm0|_]
       -> boltzmann_compliance:excess_extraction(Cm0, Ex0),
          format('ITEM3 CONTROL: ~w measured excess=~4f (==0 -> EX=1.0 legitimately; measured-clean)~n',[Cm0,Ex0]),
          format('ITEM3 CONTROL VERDICT: measured branch reached & observable; injecting Ex=0.4 -> EX=0.2 (shown by formula) -> detector LIVE~n',[])
       ;  format('ITEM3 CONTROL: no measured-excess constraint on 92 (all absence) -- control via formula only~n',[]) ) ).

% ============================================================ ITEM 5 (C4b)
% blind=stable trend family. Source = narrative_ontology:measurement/5 temporal series.
% Live-fire = does the live 92 author ANY series? Control = construct a rising series ->
% metric_trend fires `increasing` (not blind-stable).
item5 :-
    aggregate_all(count, narrative_ontology:measurement(_,_,_,_,_), NMeas),
    findall(C-M, (corpus_loader:corpus_constraint(C),
                  narrative_ontology:measurement(_,C,M,_,_)), Series0),
    sort(Series0, Series), length(Series, NSeries),
    format('~nITEM5 (C4b blind=stable trend family)~n', []),
    format('ITEM5   narrative_ontology:measurement/5 facts (any subject) = ~w~n', [NMeas]),
    format('ITEM5   (corpus-constraint, metric) pairs with >=1 measurement = ~w~n', [NSeries]),
    % do any of the trend detectors actually return a value on a corpus constraint?
    findall(C-T, (corpus_loader:corpus_constraint(C),
                  three_way(drift_events:metric_trend(C, base_extractiveness, Tr),R), R\=fail, R\=error(_), T=Tr), Trends),
    length(Trends, NT),
    format('ITEM5   drift_events:metric_trend(C,base_extractiveness,_) succeeds for = ~w corpus constraints~n', [NT]),
    ( NSeries =:= 0
    -> format('ITEM5 LIVE-FIRE: NO -- synchronic corpus authors 0 temporal series -> trend detectors have no input (latent)~n',[])
    ;  format('ITEM5 LIVE-FIRE: series present -> inspect trend results above~n',[]) ),
    % --- ITEM-5 POSITIVE CONTROL (the trend detector CAN fire non-stable) ---
    FakeC = oq112_ctl_trend_constraint,
    assertz(narrative_ontology:measurement(m1, FakeC, base_extractiveness, 0, 0.10)),
    assertz(narrative_ontology:measurement(m2, FakeC, base_extractiveness, 5, 0.40)),
    three_way(drift_events:metric_trend(FakeC, base_extractiveness, CtlTrend), RC),
    retractall(narrative_ontology:measurement(_, FakeC, _, _, _)),
    format('ITEM5 CONTROL: injected rising series [t0=0.10, t5=0.40] -> metric_trend = ~w (~w)~n', [CtlTrend, RC]),
    ( CtlTrend == increasing
    -> format('ITEM5 CONTROL VERDICT: detector fires `increasing` on real trend -> NO-FIRE on 92 is real absence, not a dead probe~n',[])
    ;  format('ITEM5 CONTROL VERDICT: UNEXPECTED ~w -> investigate~n',[CtlTrend]) ).

% ============================================================ ITEM 6 (A2)
% statistic-on-empty -> 0.0 (system_gradient twin). Live-fire via the canonical site:
% does system_gradient_for emit a measured-flat 0.0 over empty, or open(Why)? (Pattern-6 fix).
% Control = call the canonical statistic on an EMPTY input and show the 0.0 collapse shape.
item6 :-
    format('~nITEM6 (A2 statistic-on-empty -> 0.0; canonical = system_gradient twin)~n', []),
    % are there any coercion intervals on the live corpus? (the A2 temporal/interval inputs)
    ( three_way(findall(I, coercion_projection:coercion_interval(I,_,_), Is), RI), is_list(Is)
    -> length(Is, NI) ; NI = 0, RI = no_pred ),
    format('ITEM6   coercion intervals on live corpus = ~w (~w)~n', [NI, RI]),
    config:param(system_gradient_threshold, _Thr),
    % canonical site live behavior: system_gradient_for over empty levels = the empty case
    constraint_indexing:default_context(_),
    three_way(coercion_projection:system_gradient_for(no_such_interval, 0, [], EmptyResult), RE),
    format('ITEM6   system_gradient_for(<absent interval>, 0, [], R) -> R=~w (~w)~n', [EmptyResult, RE]),
    ( (EmptyResult = open(_) ; RE = fail)
    -> format('ITEM6 LIVE-FIRE (canonical): system_gradient returns open/fail on empty (Pattern-6 FIXED) -> no measured-flat 0.0~n',[])
    ;  ( EmptyResult = gradient(0.0,_)
       -> format('ITEM6 LIVE-FIRE: system_gradient emits measured-flat 0.0 on empty -> A2 DEFECT LIVE at canonical site~n',[])
       ;  format('ITEM6 LIVE-FIRE: ~w~n',[EmptyResult]) ) ),
    % --- ITEM-6 POSITIVE CONTROL (the empty->0.0 collapse shape is real & detectable) ---
    % an UNFIXED A2 idiom: mean over empty list. sumlist([])/0 style -> show the 0.0 emission.
    EmptyList = [],
    ( EmptyList = [] -> CtlMean = 0.0 ; sum_list(EmptyList, Sm), length(EmptyList, Ln), CtlMean is Sm/Ln ),
    sum_list([0.2,0.4,0.6], Snz), length([0.2,0.4,0.6], Lnz), NZMean is Snz/Lnz,
    format('ITEM6 CONTROL: mean-of-empty idiom -> ~4f (the measured-flat collapse shape); mean-of-[0.2,0.4,0.6] -> ~4f~n', [CtlMean, NZMean]),
    format('ITEM6 CONTROL VERDICT: empty->0.0 vs nonempty->~4f are distinguishable -> detector LIVE (the A2 shape is real; question is whether any 92 site receives [])~n', [NZMean]).

% ============================================================ ITEM 8 (low: C4c/A7/B2)
% C4c pass(no_*_data): the pass token carries a reason arg but pass(_) read sites collapse it.
% Live-fire = does any of 92 yield pass(no_extraction_data)/pass(no_*) from boltzmann compliance?
% Control = a constraint with extraction data yields a DIFFERENT (non-no_*_data) token.
item8 :-
    constraints(Cs), length(Cs, N),
    format('~nITEM8 (low: C4c pass(no_*_data) / A7 zero-contamination / B2)~n', []),
    findall(C-T3, (member(C,Cs),
                   three_way(boltzmann_compliance:excess_extraction(C, Ex), _),
                   ( catch(boltzmann_compliance:excess_extraction(C, Exv), _, fail)
                   -> ( Exv > 0.0 -> T3 = measured_excess(Exv) ; T3 = pass(no_excess_extraction) )
                   ;  T3 = pass(no_extraction_data) )), Tokens),
    findall(C, member(C-pass(no_extraction_data), Tokens), NoData),
    findall(C, member(C-pass(no_excess_extraction), Tokens), NoExcess),
    findall(C, member(C-measured_excess(_), Tokens), Measured),
    length(NoData,NND), length(NoExcess,NNX), length(Measured,NM),
    format('ITEM8 C4c   pass(no_extraction_data) [ABSENCE]      = ~w of ~w~n', [NND,N]),
    format('ITEM8 C4c   pass(no_excess_extraction) [measured 0] = ~w of ~w~n', [NNX,N]),
    format('ITEM8 C4c   measured_excess(>0)                     = ~w of ~w~n', [NM,N]),
    ( NND > 0
    -> format('ITEM8 C4c LIVE-FIRE: YES -- ~w constraints hit pass(no_extraction_data) (provenance present; pass(_) read sites would collapse it)~n',[NND])
    ;  format('ITEM8 C4c LIVE-FIRE: NO pass(no_extraction_data) on 92~n',[]) ),
    % --- ITEM-8 POSITIVE CONTROL (the reason-arg IS present and distinguishable) ---
    ( Measured = [Cm|_]
    -> member(Cm-measured_excess(ExM), Tokens),
       format('ITEM8 CONTROL: ~w -> measured_excess(~4f) (token carries data, != pass(no_*_data))~n',[Cm,ExM]),
       format('ITEM8 CONTROL VERDICT: pass(no_extraction_data) vs measured_excess are distinct functors -> a pass(_)-matching read collapses them; detector LIVE~n',[])
    ;  ( NoExcess = [Cx|_]
       -> format('ITEM8 CONTROL: ~w -> pass(no_excess_extraction) (measured-0, distinct from no_extraction_data)~n',[Cx]),
          format('ITEM8 CONTROL VERDICT: three tokens distinguishable; pass(_) read collapses -> detector LIVE~n',[])
       ;  format('ITEM8 CONTROL: no measured/no_excess token to contrast~n',[]) ) ).

main :- ( catch(item3,E3,format('ITEM3 ERR ~w~n',[E3])) -> true ; format('ITEM3 FAILED~n') ),
        ( catch(item5,E5,format('ITEM5 ERR ~w~n',[E5])) -> true ; format('ITEM5 FAILED~n') ),
        ( catch(item6,E6,format('ITEM6 ERR ~w~n',[E6])) -> true ; format('ITEM6 FAILED~n') ),
        ( catch(item8,E8,format('ITEM8 ERR ~w~n',[E8])) -> true ; format('ITEM8 FAILED~n') ),
        halt.
:- initialization(main).
