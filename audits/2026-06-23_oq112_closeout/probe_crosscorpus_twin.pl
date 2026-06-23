% OQ-112 close-out — CROSS-CORPUS check on the testsets_haiku twin (960, a LIVE leg).
% Tests whether the absence-gates that were masked UPSTREAM on the sparse 92 fire LIVE on a
% denser corpus -- i.e. is "latent" a 92-fact or an engine-fact?  READ-ONLY.
%
% Overlay discipline (CLAUDE.md): retractall(corpus_path) THEN asserta (plain assertz is
% silently appended after the default and ignored). Clear caches before classification.

:- [stack].
:- consult(purity_scoring).
:- consult(boltzmann_compliance).
:- consult(drift_events).

three_way(Goal, R) :- ( catch(Goal, E, R=error(E)) -> (var(R)->R=ok;true) ; R=fail ).

main :-
    % --- overlay testsets_haiku (asserta after retractall; witness the count) ---
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format('TWIN testsets_haiku LIVE=~w (overlay-took witness; expect 960, not 44/92)~n', [N]),
    ( N >= 900 -> true ; format('TWIN WARNING: count ~w != ~960 -> overlay may NOT have taken; HALT-WORTHY~n',[N]) ),
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),

    % re-key tripwire: filename == in-file constraint_metric subject? (post-de-leak twins: expect yes)
    findall(C, (member(C,Cs), \+ narrative_ontology:constraint_metric(C,_,_)), NoMetric),
    length(NoMetric, NNM),
    format('TWIN constraints with NO constraint_metric (filename!=subject re-key risk) = ~w~n', [NNM]),

    % ===== ITEM 3 (A6): does excess-absence reach a NON-NULL purity_score (false-clean)? =====
    % the 92 masking: epistemic_access_check=false short-circuits to -1.0. On the twin, count
    % constraints that PASS epistemic AND lack excess_extraction -> EX=1.0 reaches purity_score.
    findall(C, (member(C,Cs), three_way(purity_scoring:epistemic_access_check(C,true),Re), Re==ok), EpiOk),
    length(EpiOk, NEpi),
    findall(C, (member(C,EpiOk), \+ boltzmann_compliance:excess_extraction(C,_)), EpiOkNoExc),
    length(EpiOkNoExc, NEONE),
    format('~nITEM3 TWIN: epistemic_access=true = ~w of ~w~n', [NEpi,N]),
    format('ITEM3 TWIN: epistemic=true AND excess_extraction ABSENT (EX=1.0 reaches non-null purity_score) = ~w~n', [NEONE]),
    ( NEONE > 0
    -> format('ITEM3 TWIN LIVE-FIRE: YES -- ~w twin constraints get a NON-NULL purity_score whose excess-subscore=1.0 came from ABSENCE -> A6 BITES on the twin~n', [NEONE]),
       ( EpiOkNoExc = [S3|_] -> three_way(purity_scoring:purity_score(S3,PS3),_),
         format('ITEM3 TWIN sample: ~w purity_score=~w (non-null, EX=1.0 from absence baked in)~n',[S3,PS3]) ; true )
    ;  format('ITEM3 TWIN LIVE-FIRE: NO -- every excess-absent twin constraint also fails epistemic gate (same masking as 92)~n', []) ),

    % ===== ITEM 8 (C4c): boltzmann_compliant=compliant with pass(no_extraction_data)? =====
    findall(C-Comp, (member(C,Cs), three_way(boltzmann_compliance:boltzmann_compliant(C,Comp0),Rc), Rc==ok, Comp=Comp0), Comps),
    findall(C, (member(C-compliant(_),Comps)), Compliant),
    length(Compliant, NCompliant),
    % of the compliant, how many had NO excess_extraction data (T3 = pass(no_extraction_data))?
    findall(C, (member(C,Compliant), \+ boltzmann_compliance:excess_extraction(C,_)), CompliantNoExc),
    length(CompliantNoExc, NCNE),
    format('~nITEM8 TWIN: boltzmann_compliant=compliant = ~w of ~w~n', [NCompliant,N]),
    format('ITEM8 TWIN: compliant AND excess_extraction ABSENT (compliance rode pass(no_extraction_data)) = ~w~n', [NCNE]),
    ( NCNE > 0
    -> format('ITEM8 TWIN C4c LIVE-FIRE: YES -- ~w twin constraints certified compliant with NO extraction data (pass(no_extraction_data) collapsed to clean) -> C4c BITES on the twin~n', [NCNE])
    ;  format('ITEM8 TWIN C4c LIVE-FIRE: NO~n', []) ),

    % ===== ITEM 5 (C4b): any temporal series on the twin? =====
    aggregate_all(count, narrative_ontology:measurement(_,_,_,_,_), NMeas),
    format('~nITEM5 TWIN: measurement/5 facts = ~w~n', [NMeas]),
    halt.

:- initialization(main).
