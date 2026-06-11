/* Probe C — D2 static path (get_raw_suppression else-branch) census, LIVE corpus.
   Same call path as the census: drl_core:get_raw_suppression/2 itself.
   Positive control: in-denominator-shaped synthetic (authored extractiveness,
   NO authored suppression) pushed through the same path must be flagged.       */

:- [stack].

run_probe_c :-
    get_time(T0), format_time(atom(TS), '%FT%T%z', T0),
    format("=== Probe C: get_raw_suppression else-branch census, live corpus ===~nas-of: ~w~n", [TS]),
    corpus_loader:load_all_testsets,
    config:param(corpus_path, CP),
    corpus_loader:resolve_corpus_dir(CP, Abs),
    format("resolved corpus dir: ~w~n", [Abs]),
    config:param(extractiveness_metric_name, ExtName),
    config:param(suppression_metric_name, SuppName),
    format("extractiveness_metric_name=~w  suppression_metric_name=~w~n",
           [ExtName, SuppName]),
    % ---- control (this process, before the census) ----
    assertz(narrative_ontology:constraint_metric(oq33ctrl_d2, ExtName, 0.4)),
    covering_analysis:all_corpus_constraints(ACs1),
    (   member(oq33ctrl_d2, ACs1)
    ->  format("CONTROL in-denominator: YES (member of all_corpus_constraints)~n")
    ;   format("CONTROL in-denominator: FAILED — control never entered the denominator~n"),
        halt(3)
    ),
    (   drl_core:get_raw_suppression(oq33ctrl_d2, V0), V0 == 0,
        \+ narrative_ontology:constraint_metric(oq33ctrl_d2, SuppName, _)
    ->  format("CONTROL else-branch: FLAGGED (get_raw_suppression=~w, no authored ~w fact)~n",
               [V0, SuppName])
    ;   format("CONTROL else-branch: FAILED — probe would not flag a true absence~n"),
        halt(3)
    ),
    retractall(narrative_ontology:constraint_metric(oq33ctrl_d2, _, _)),
    covering_analysis:all_corpus_constraints(ACs2),
    (   \+ member(oq33ctrl_d2, ACs2)
    ->  format("control retracted; denominator clean~n")
    ;   format("CONTROL RETRACTION FAILED~n"), halt(3)
    ),
    % ---- census, same call path ----
    covering_analysis:all_corpus_constraints(ACs), length(ACs, NAC),
    findall(C, corpus_loader:corpus_constraint(C), Corpus), length(Corpus, NCorpus),
    findall(C-V,
        ( member(C, ACs),
          drl_core:get_raw_suppression(C, V),
          \+ narrative_ontology:constraint_metric(C, SuppName, _) ),
        ElseHits),
    length(ElseHits, NElse),
    findall(C, ( member(C-_, ElseHits), member(C, Corpus) ), CorpusElse),
    length(CorpusElse, NCorpusElse),
    format("~nall_corpus_constraints denominator : ~w~n", [NAC]),
    format("corpus_constraint/1 count          : ~w~n", [NCorpus]),
    format("else-branch hits (all denominators): ~w  ~w~n", [NElse, ElseHits]),
    format("else-branch hits within corpus_constraint/1: ~w  ~w~n",
           [NCorpusElse, CorpusElse]).
