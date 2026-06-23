% OQ-112 close-out — Part B v2: LIVE REACHABILITY through consumers (not guard predicates).
% The v1 sweep counted guard-predicate firings; this checks whether the absence-branch value
% reaches a CONSUMED output for the 6 absence constraints. READ-ONLY.

:- [stack].
:- consult(purity_scoring).
:- consult(boltzmann_compliance).
:- consult(drl_boltzmann_analysis).
:- consult(drift_events).
:- corpus_loader:ensure_corpus_loaded.

three_way(Goal, R) :- ( catch(Goal, E, R=error(E)) -> (var(R)->R=ok;true) ; R=fail ).

six([actinide_replenishment_mechanism_contradictions,digital_money_legitimacy_contradictions,
     knowledge_legitimacy_biomedicine_contradictions,performance_legitimacy_contradictions,
     polaris_document_status_contradictions,visual_evidentiary_authority_contradictions]).

main :-
    six(Six),
    % --- ITEM 3 reachability: does the EX=1.0 absence-branch reach a consumer? ---
    format('=== ITEM 3 (A6) LIVE REACHABILITY ===~n',[]),
    % (a) via purity_score/2 (json field): epistemic gate short-circuits the 6 -> -1.0 -> null
    forall(member(C,Six),
        ( three_way(purity_scoring:purity_score(C,PS),_),
          three_way(purity_scoring:epistemic_access_check(C,EA),_),
          format('  ~w: purity_score=~w (epistemic_access=~w)~n',[C,PS,EA]) )),
    % (b) via drl_boltzmann_analysis:purity_deficit (bypasses epistemic gate). Does EX=1.0
    %     suppress an excess_extraction_deficit (absence reads as "no deficit")?
    ( member(C1,Six),
      ( three_way(drl_boltzmann_analysis:purity_deficit(C1, excess_extraction_deficit(Sc)),Rd) ; true ), !
    -> format('  purity_deficit(~w, excess_extraction_deficit(_)) -> ~w (deficit fires? ~w)~n',
              [C1, Rd, (Rd==fail -> 'NO (EX>=0.85 from absence -> reads clean)' ; Rd)])
    ; true ),
    % (c) is drl_boltzmann_analysis reform/deficit emitted to per-constraint json? (checked in py)
    format('  NOTE: purity_deficit/reform_urgency are NOT per-constraint json fields (py check); aggregate use checked in py.~n',[]),

    % --- ITEM 8 (C4c) reachability: are the 6 boltzmann_compliant via pass(no_extraction_data)? ---
    format('~n=== ITEM 8 (C4c) LIVE REACHABILITY ===~n',[]),
    forall(member(C,Six),
        ( three_way(boltzmann_compliance:boltzmann_compliant(C, Comp),_),
          format('  ~w: boltzmann_compliant = ~w~n',[C,Comp]) )),
    % positive control: a normal constraint's compliance for contrast
    three_way(boltzmann_compliance:boltzmann_compliant(actinide_replenishment_mechanism_flat_control, CompN),_),
    format('  CONTROL actinide_..._flat_control: boltzmann_compliant = ~w~n',[CompN]),

    % --- ITEM 5 (C4b) refinement: is `stable` measured or blind? ---
    format('~n=== ITEM 5 (C4b) stable-is-measured-not-blind ===~n',[]),
    findall(C-Tr, (corpus_loader:corpus_constraint(C),
                   three_way(drift_events:metric_trend(C,base_extractiveness,Tr0),R), R==ok, Tr=Tr0), Trends),
    findall(C, member(C-stable, Trends), Stables), length(Stables, NStable),
    findall(C, member(C-increasing, Trends), Incs), length(Incs, NInc),
    findall(C, member(C-decreasing, Trends), Decs), length(Decs, NDec),
    length(Trends, NTr),
    format('  metric_trend(base_extractiveness) over corpus: total=~w stable=~w increasing=~w decreasing=~w~n',
           [NTr,NStable,NInc,NDec]),
    % for every `stable`, confirm metric_delta SUCCEEDS (measured small delta), not absence
    findall(C, (member(C-stable, Trends), three_way(drift_events:metric_delta(C,base_extractiveness,_,_,_),Rd2), Rd2==ok), MeasuredStable),
    length(MeasuredStable, NMS),
    format('  of ~w stable, metric_delta SUCCEEDS (measured small-delta, NOT blind) for ~w~n',[NStable,NMS]),
    ( NMS =:= NStable
    -> format('  ITEM5 VERDICT: every `stable` is a MEASURED small-delta (metric_delta requires non-empty series + T2>T1) -> drift_events:92 is SOUND, no blind=stable on 92~n',[])
    ;  format('  ITEM5 VERDICT: ~w stable lack a measured delta -> BLIND=STABLE candidate, investigate~n',[NStable-NMS]) ),
    halt.

:- initialization(main).
