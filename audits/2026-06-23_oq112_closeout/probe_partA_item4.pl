% OQ-112 close-out — Part A bite-check, item 4 (A3 maxent-local sentinel). READ-ONLY.
% Field: maxent profile/LL values. Bite iff a maxent-local accessor returns the `unknown`
% sentinel for a claim-bearing constraint on 92 (else-branch reached). Round-3 WA showed 0.
% Re-witness on 92 + sentinel-firing positive control (theater retract reproduces a firing).

:- [stack].
:- consult(maxent_classifier).
:- corpus_loader:ensure_corpus_loaded.

main :-
    config:param(theater_metric_name, TheaterName),
    constraint_indexing:default_context(Ctx),
    % --- B: claim-bearing constraints lacking each non-sentinel source ---
    findall(C, (corpus_loader:corpus_constraint(C), narrative_ontology:constraint_claim(C,_),
                \+ drl_core:base_extractiveness(C,_)), AbsEps),
    findall(C, (corpus_loader:corpus_constraint(C), narrative_ontology:constraint_claim(C,_),
                \+ constraint_indexing:extractiveness_for_agent(C,Ctx,_)), AbsChi),
    findall(C, (corpus_loader:corpus_constraint(C), narrative_ontology:constraint_claim(C,_),
                \+ narrative_ontology:constraint_metric(C,TheaterName,_)), AbsTh),
    length(AbsEps,NE), length(AbsChi,NC), length(AbsTh,NT),
    format('ITEM4 claim+corpus lacking base_extractiveness   = ~w: ~w~n',[NE,AbsEps]),
    format('ITEM4 claim+corpus lacking extractiveness_for_agent = ~w: ~w~n',[NC,AbsChi]),
    format('ITEM4 claim+corpus lacking theater(~w)            = ~w: ~w~n',[TheaterName,NT,AbsTh]),
    % --- direct sentinel scan: does any accessor return `unknown` for a claim constraint? ---
    findall(C, (corpus_loader:corpus_constraint(C), narrative_ontology:constraint_claim(C,_),
                maxent_classifier:get_constraint_metrics(C, E, S, T),
                (E==unknown ; S==unknown ; T==unknown)), Sentinels),
    length(Sentinels, NS),
    format('ITEM4 claim constraints where get_constraint_metrics returns a sentinel = ~w: ~w~n',[NS,Sentinels]),
    ( NS =:= 0
    -> format('ITEM4 VERDICT: 0 sentinels on 92 -> else-branches unreached -> genuine values -> NO LIVE BITE (latent)~n',[])
    ;  format('ITEM4 VERDICT: LIVE BITE -- sentinel reaches a consumer on 92~n',[]) ),
    % --- POSITIVE CONTROL: the sentinel scan CAN detect a firing (overlay theater-absence) ---
    format('~n=== ITEM 4 POSITIVE CONTROL (sentinel-firing detector) ===~n',[]),
    ( corpus_loader:corpus_constraint(Cc),
      narrative_ontology:constraint_metric(Cc, TheaterName, _), !
    -> maxent_classifier:get_constraint_metrics(Cc, _, _, Tbefore),
       findall(V, narrative_ontology:constraint_metric(Cc, TheaterName, V), Vs),
       retractall(narrative_ontology:constraint_metric(Cc, TheaterName, _)),
       maxent_classifier:get_constraint_metrics(Cc, _, _, Tafter),
       % restore
       forall(member(V,Vs), assertz(narrative_ontology:constraint_metric(Cc, TheaterName, V))),
       format('PC: ~w theater before=~w  after-retract=~w~n',[Cc,Tbefore,Tafter]),
       ( Tafter == unknown
       -> format('PC VERDICT: else-branch FIRED (returns `unknown` sentinel) -> detector LIVE, latency real~n',[])
       ;  format('PC VERDICT: after-retract=~w (NOT unknown) -> investigate~n',[Tafter]) )
    ;  format('PC: no theater-bearing constraint to perturb~n',[]) ),
    halt.

:- initialization(main).
