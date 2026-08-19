% Does the BOUND call signature_grade(C, correction) agree with the unbound-then-filter
% form on the live corpus? Pattern 7 says the bound form is OVER-PERMISSIVE; a disagreement
% is the hazard firing, an agreement is a corpus-scoped benign verdict.
sg_probe :-
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    length(Cs, N),
    findall(C, (member(C,Cs), signature_detection:signature_grade(C, correction)), Bound),
    findall(C, (member(C,Cs), once(signature_detection:signature_grade(C, G)), G == correction), Unbound),
    length(Bound, NB), length(Unbound, NU),
    sort(Bound, SB), sort(Unbound, SU),
    subtract(SB, SU, OnlyBound), subtract(SU, SB, OnlyUnbound),
    length(OnlyBound, NOB), length(OnlyUnbound, NOU),
    format("corpus=~w bound=~w unbound_filtered=~w only_bound=~w only_unbound=~w~n",
           [N, NB, NU, NOB, NOU]),
    ( OnlyBound == [] -> true ; format("  ONLY-BOUND: ~q~n", [OnlyBound]) ),
    ( OnlyUnbound == [] -> true ; format("  ONLY-UNBOUND: ~q~n", [OnlyUnbound]) ),
    % POSITIVE CONTROL: the same comparison on a predicate whose bound form IS known
    % over-permissive would differ. Use commentary — clause 3's atom, whose cut IS skipped
    % under a bound `correction` query — to show the probe can separate the two forms.
    findall(C, (member(C,Cs), signature_detection:signature_grade(C, commentary)), BC),
    findall(C, (member(C,Cs), once(signature_detection:signature_grade(C, G2)), G2 == commentary), UC),
    length(BC, NBC), length(UC, NUC),
    format("control(commentary): bound=~w unbound_filtered=~w~n", [NBC, NUC]).
