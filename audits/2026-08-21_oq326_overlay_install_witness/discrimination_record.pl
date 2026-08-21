% OQ-326 discrimination record — naturally-arising, BOTH directions.
% POSITIVE: the OQ-302 preregistration's own template (rule-bearing + static) —
%           a real defective specification nobody authored to be caught.
% NEGATIVE: a real historically-clean overlay (a1_probe.pl:87, 2026-06-07).
positive :-
    format("~n=== POSITIVE: OQ-302 prereg's own template, via with_retracted/2 ===~n"),
    (   catch(probe_harness:with_retracted(
                [boltzmann_compliance:boltzmann_invariant_mountain(_,_)], true), E, true)
    ->  (   subsumes_term(error(probe_overlay_partial(_,_),_), E)
        ->  format("POSITIVE OK: clause-3 throw ~q~n",[E])
        ;   nonvar(E)
        ->  format("POSITIVE INCONCLUSIVE: threw the WRONG term ~q~n",[E]), halt(1)
        ;   format("POSITIVE FAILED: no throw~n"), halt(1) )
    ;   format("POSITIVE FAILED: goal failed silently~n"), halt(1) ).

negative :-
    format("~n=== NEGATIVE: a1_probe.pl:87's overlay, must DECLINE and INSTALL ===~n"),
    config:param(extractiveness_metric_name, ExtName),
    once(( corpus_loader:corpus_constraint(C),
           narrative_ontology:constraint_metric(C, ExtName, _) )),
    (   catch(probe_harness:with_overlay(
                [narrative_ontology:constraint_metric(C, ExtName, _)],
                [narrative_ontology:constraint_metric(C, ExtName, 0.75)],
                Report,
                narrative_ontology:constraint_metric(C, ExtName, 0.75)), E2, true)
    ->  (   var(E2)
        ->  format("NEGATIVE OK: declined and installed on ~w (metric ~w)~n",[C,ExtName]),
            format("  install witness: ~q~n",[Report])
        ;   format("NEGATIVE FAILED: threw ~q~n",[E2]), halt(1) )
    ;   format("NEGATIVE FAILED: overlay goal failed~n"), halt(1) ).

run_dr :- positive, negative, format("~nBOTH DIRECTIONS OK~n").
