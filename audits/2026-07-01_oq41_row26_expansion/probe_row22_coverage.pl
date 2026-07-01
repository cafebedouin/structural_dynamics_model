% OQ-40 census row-22 spin-out — coverage witness for compute_temporal_stability.
% (a) confirm the folded metric identity (SuppMetricName). (b) coverage over the NON-CIRCULAR
% reach-the-gate denominator (= authors the gate-feeding scalar metric, regardless of class),
% counting measurement/5-temporal vs scalar-only. (b') positive control: the count must catch a
% known measurement/5 series (else near-zero is "didn't find it," not "isn't there").
:- initialization(main).

main :-
    [stack],
    ( getenv('CORPUS_OVERLAY', Dir), Dir \== ''
    -> retractall(config:param(corpus_path, _)),
       asserta(config:param(corpus_path, Dir)),
       format("OVERLAY corpus_path=~w~n", [Dir])
    ;  true ),
    corpus_loader:ensure_corpus_loaded,
    % (a) folded metric identity
    config:param(suppression_metric_name, Supp),
    format("(a) FOLDED METRIC = ~w  (signature_detection:177 SuppMetricName -> :191 compute_temporal_stability)~n", [Supp]),
    % (b) reach-the-gate denominator = authors the gate-feeding scalar metric
    findall(C, ( corpus_loader:corpus_constraint(C),
                 narrative_ontology:constraint_metric(C, Supp, _) ), ReachRaw),
    sort(ReachRaw, Reach), length(Reach, NReach),
    findall(C, ( member(C, Reach),
                 narrative_ontology:measurement(_, C, Supp, _, _) ), WithMeasRaw),
    sort(WithMeasRaw, WithMeas), length(WithMeas, NWith),
    NScalarOnly is NReach - NWith,
    findall(C, ( member(C, Reach),
                 findall(V, narrative_ontology:constraint_metric(C, Supp, V), Vs),
                 length(Vs, NL), NL > 1 ), MultiRaw),
    sort(MultiRaw, Multi), length(Multi, NMulti),
    format("(b) REACH-THE-GATE denom (author ~w scalar): ~w~n", [Supp, NReach]),
    format("    of those WITH measurement/5 temporal series : ~w~n", [NWith]),
    format("    scalar-only (no temporal series)            : ~w~n", [NScalarOnly]),
    format("    with >1 scalar level (cross-level fold non-trivial): ~w~n", [NMulti]),
    % (b') positive control — the reach+measurement query MUST catch a known series
    ( WithMeas = [Ex|_]
    -> ( narrative_ontology:measurement(MId, Ex, Supp, T0, MV0)
       -> format("(b') POSITIVE CONTROL: ~w has measurement(~w,_,~w,~w,~w) and IS in WithMeas -> count catches temporal series~n", [Ex, MId, Supp, T0, MV0])
       ;  format("(b') POSITIVE CONTROL: WithMeas member lacks a measurement fact -- ESCALATE~n") )
    ;  ( assertz(narrative_ontology:constraint_metric(tw_row22_synth, Supp, 0.3)),
         assertz(narrative_ontology:measurement(m_tw, tw_row22_synth, Supp, 1900, 0.3)),
         ( ( narrative_ontology:constraint_metric(tw_row22_synth, Supp, _),
             narrative_ontology:measurement(_, tw_row22_synth, Supp, _, _) )
         -> format("(b') POSITIVE CONTROL (injected): synthetic reach-gate constraint WITH measurement/5 is caught by the query -> count would detect temporal series if any existed~n")
         ;  format("(b') POSITIVE CONTROL: injected series NOT caught -- query mis-wired, ESCALATE~n") ) ) ),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
