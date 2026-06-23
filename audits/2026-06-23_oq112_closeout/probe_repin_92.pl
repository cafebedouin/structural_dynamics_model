% OQ-112 close-out — re-pin 92 (READ-ONLY, self-witnessing).
% Emits corpus_constraint membership (not just a count) + N distinct constraint_claim
% subjects + non-corpus demo check. Negative control (bad corpus_path -> corpus_empty)
% is run separately (probe_repin_negctl.pl) so a throw here does not mask the pin.

:- [stack].
:- corpus_loader:ensure_corpus_loaded.

main :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorpus),
    format('LIVE=~w~n', [NCorpus]),
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    forall(member(C, Cs), format('~w~n', [C])),
    findall(S, narrative_ontology:constraint_claim(S, _), Claims0),
    sort(Claims0, Claims),
    length(Claims, NClaims),
    format('# DISTINCT_CLAIM_SUBJECTS=~w~n', [NClaims]),
    findall(S, (member(S, Claims), \+ corpus_loader:corpus_constraint(S)), NonCorpus),
    length(NonCorpus, NNon),
    format('# CLAIM_SUBJECTS_NOT_IN_CORPUS=~w: ~w~n', [NNon, NonCorpus]),
    halt.

:- initialization(main).
