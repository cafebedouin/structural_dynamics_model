% Campaign 2 (OQ-138 CI-rope limb) — neutron_star vs superheavy verdict-path dump at HEAD.
% Read-only probe: loads live testsets, dumps type/base/cap/alerts/grade/eps per seat.
% Positive control: superheavy_decay_reading must match its post-FCR-9 state (FCR9_FINDINGS.md).
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

seat(neutron_star_bombardment_reading).
seat(superheavy_decay_reading).

gv(G, V, Fallback) :- ( catch(G, _, fail) -> true ; V = Fallback ).

dump(C) :-
    constraint_indexing:default_context(Ctx),
    gv(drl_core:metric_based_type_indexed(C, Ctx, MT), MT, err),
    gv(drl_core:dr_type(C, Ctx, DT), DT, err),
    gv(narrative_ontology:constraint_claim(C, Claim), Claim, none),
    gv(narrative_ontology:constraint_metric(C, extractiveness, Eps), Eps, none),
    findall(S, signature_detection:constraint_signature(C, S), Sigs),
    ( catch(signature_detection:fcr_routed(C), _, fail) -> Routed = yes ; Routed = no ),
    gv(signature_detection:signature_grade(C, G), G, none),
    gv(signature_detection:signature_severity(C, Sv), Sv, none),
    gv(boltzmann_compliance:boltzmann_compliant(C, BC), BC, err),
    gv(boltzmann_compliance:excess_extraction(C, XX), XX, none),
    (   catch(diagnostic_summary:diagnostic_summary(C, Summary), _, fail)
    ->  diagnostic_summary:diagnostic_verdict(Summary, Base),
        (   catch(diagnostic_summary:verdict_join(C, Summary,
                    verdict_join(J, B2, Cap, Alerts, GridProv, _, SG)), _, fail)
        ->  true
        ;   J = err, B2 = err, Cap = err, Alerts = err, GridProv = err, SG = err )
    ;   Base = err, J = err, B2 = err, Cap = err, Alerts = err, GridProv = err, SG = err ),
    format("~n=== ~w ===~n", [C]),
    format("  claim=~w  eps(extractiveness)=~w~n", [Claim, Eps]),
    format("  metric_type=~w  dr_type=~w~n", [MT, DT]),
    format("  signatures=~w  fcr_routed=~w~n", [Sigs, Routed]),
    format("  signature_grade=~w  signature_severity=~w~n", [G, Sv]),
    format("  boltzmann=~w  excess_extraction=~w~n", [BC, XX]),
    format("  base_verdict=~w  join_verdict=~w (base_in_join=~w) cap=~w sig_grade=~w~n",
           [Base, J, B2, Cap, SG]),
    format("  grid_prov=~w~n  alerts=~w~n", [GridProv, Alerts]).

main :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, testsets)),
    corpus_loader:load_all_testsets,
    forall(seat(C), dump(C)),
    halt.
main :- write('PROBE FAIL'), nl, halt(1).
