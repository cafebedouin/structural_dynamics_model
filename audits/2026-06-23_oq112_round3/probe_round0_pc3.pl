% OQ-112 Round 3 — PC3 corrected. base_extractiveness is a RULE over
% constraint_metric(C, extractiveness_metric_name, V); retract the underlying FACT.
% Confirms eps-absence BLOCKS dr_type (is_X calls base_extractiveness first), so
% eps/chi-absent constraints drop at dr_type — theater is the unique residual whose
% absence reaches the arith while the constraint stays classifiable.

:- [stack].
:- use_module(probe_harness).
:- corpus_loader:ensure_corpus_loaded.

probe :-
    config:param(extractiveness_metric_name, ExtName),
    constraint_indexing:default_context(Ctx),
    once(( corpus_loader:corpus_constraint(C0),
           narrative_ontology:constraint_claim(C0, _),
           narrative_ontology:constraint_metric(C0, ExtName, _),
           drl_core:dr_type(C0, Ctx, T0) )),
    format('PC3 constraint C0 = ~w (baseline dr_type = ~w, ext_metric = ~w)~n', [C0, T0, ExtName]),
    probe_harness:with_retracted(
        [narrative_ontology:constraint_metric(C0, ExtName, _)],
        ( ( catch(drl_core:base_extractiveness(C0, BE), _, (BE = err)) -> true ; BE = absent ),
          format('PC3: base_extractiveness(eps-fact retracted ~w) -> ~w~n', [C0, BE]),
          maxent_classifier:get_constraint_metrics(C0, E1, S1, Th1),
          format('PC3: get_constraint_metrics: eps=~w supp=~w theater=~w (maxent fabricates eps if absent)~n', [E1, S1, Th1]),
          ( catch(drl_core:dr_type(C0, Ctx, T1), Edt, (T1 = error(Edt))) -> true ; T1 = failed ),
          format('PC3: dr_type(eps-absent ~w) -> ~w~n', [C0, T1]),
          format('PC3 verdict: eps-absence ~w dr_type~n',
                 [(T1 == failed -> 'BLOCKS (eps/chi drop at dr_type; theater is the unique residual)' ; 'does NOT block — REVISIT')])
        )).

:- (catch(probe, E, (format('PROBE ERROR: ~w~n', [E]), fail)) -> true ; format('PROBE FAILED~n')), halt.
:- halt(1).
