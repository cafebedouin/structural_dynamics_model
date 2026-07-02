/* B5 — Cross-leg NL population probe (generalizes b1_nl404_probe.pl).
 *
 * Same pre-8b5a34b8 has_viable_alternatives swap + Sig-UNBOUND cascade sweep,
 * parameterized over corpus. Controls: PRE (unknown) / MID (false) dispatch
 * controls per run; the aggregate control value differs per corpus —
 * kernel_v1 has a RECORDED count (26, liveness matrix 2026-06-10); the twins
 * and live testsets are fresh measurements (no recorded expectation).
 *
 * Run: cd prolog && swipl -g "[stack], consult('<this>'), b5_run('<corpus>', '<outfile>'), halt" -t "halt(1)"
 */

b5_run(CorpusPath, OutFile) :-
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, CorpusPath)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorp),
    format(user_error, '[b5:~w] corpus_constraint=~w~n', [CorpusPath, NCorp]),

    % PRE dispatch control (HEAD fallback = unknown)
    ( corpus_loader:corpus_constraint(C0),
      \+ ( narrative_ontology:affects_constraint(I0, C0),
           narrative_ontology:intent_viable_alternative(I0, _, _) )
    -> true ; throw(b5_no_control_constraint) ),
    signature_detection:has_viable_alternatives(C0, PreVal),
    format(user_error, '[b5:~w] PRE control (~w): ~w (expect unknown)~n',
           [CorpusPath, C0, PreVal]),
    ( PreVal == unknown -> true ; throw(b5_pre_control_failed(PreVal)) ),

    aggregate_all(count, narrative_ontology:intent_viable_alternative(_, _, _), NIVA),
    format(user_error, '[b5:~w] intent_viable_alternative facts=~w~n', [CorpusPath, NIVA]),

    % SWAP to pre-8b5a34b8 semantics
    abolish(signature_detection:has_viable_alternatives/2),
    assertz((signature_detection:has_viable_alternatives(C, true) :-
                narrative_ontology:affects_constraint(I, C),
                narrative_ontology:intent_viable_alternative(I, _, _), !)),
    assertz(signature_detection:has_viable_alternatives(_, false)),

    % MID dispatch control
    signature_detection:has_viable_alternatives(C0, MidVal),
    format(user_error, '[b5:~w] MID control (~w): ~w (expect false)~n',
           [CorpusPath, C0, MidVal]),
    ( MidVal == false -> true ; throw(b5_mid_control_failed(MidVal)) ),

    % Sig-UNBOUND cascade sweep
    findall(C-Sig,
            ( corpus_loader:corpus_constraint(C),
              once(signature_detection:constraint_signature(C, Sig)) ),
            Pairs),
    findall(C, member(C-natural_law, Pairs), NLs),
    length(NLs, NNL),
    length(Pairs, NAll),
    format(user_error, '[b5:~w] swept=~w  natural_law=~w~n', [CorpusPath, NAll, NNL]),

    open(OutFile, write, S),
    forall(member(C, NLs), format(S, '~w~n', [C])),
    close(S),
    format(user_error, '[b5:~w] member list -> ~w~n', [CorpusPath, OutFile]).
