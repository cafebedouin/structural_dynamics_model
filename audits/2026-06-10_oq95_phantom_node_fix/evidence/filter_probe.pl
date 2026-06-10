% OQ-95 probe: does constraint_claim/2 separate real nodes from phantom
% affects_constraint endpoints? Read-only; run from prolog/.
:- [stack].
:- corpus_loader:load_all_testsets.

probe :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorpus),
    config:param(extractiveness_metric_name, ExtName),
    findall(C, (narrative_ontology:constraint_metric(C, ExtName, _), atom(C)), CsRaw),
    sort(CsRaw, Enumerated), length(Enumerated, NEnum),
    % affects_constraint endpoint atoms
    findall(X, (narrative_ontology:affects_constraint(A,B), member(X,[A,B])), Es0),
    sort(Es0, Endpoints), length(Endpoints, NEnd),
    partition([X]>>(narrative_ontology:constraint_claim(X,_)), Endpoints, WithClaim, NoClaim),
    length(WithClaim, NWc), length(NoClaim, NNc),
    partition([X]>>(memberchk(X, Enumerated)), Endpoints, _InEnum, NotInEnum),
    length(NotInEnum, NNie),
    format("corpus=~w enumerated=~w aff_endpoints=~w with_claim=~w no_claim=~w not_in_enumerated=~w~n",
           [NCorpus, NEnum, NEnd, NWc, NNc, NNie]),
    format("no_claim_list=~w~n", [NoClaim]),
    sort(NotInEnum, SNotInEnum), sort(NoClaim, SNoClaim),
    (   SNotInEnum == SNoClaim
    ->  format("claim_test_matches_enumeration_gap=YES~n")
    ;   subtract(SNotInEnum, SNoClaim, OnlyEnumGap),
        subtract(SNoClaim, SNotInEnum, OnlyClaimGap),
        format("claim_test_matches_enumeration_gap=NO only_enum_gap=~w only_claim_gap=~w~n",
               [OnlyEnumGap, OnlyClaimGap])
    ),
    % positive control 1: every enumerated corpus node has a claim
    findall(C, (member(C, Enumerated), \+ narrative_ontology:constraint_claim(C,_)), EnumNoClaim),
    format("enumerated_without_claim=~w~n", [EnumNoClaim]),
    % positive control 2: claim test fires on a known-real endpoint
    (   WithClaim = [W|_] -> format("claim_fires_on_real_example=~w~n", [W])
    ;   format("claim_fires_on_real_example=NONE~n") ),
    % demo constraint: does the existence test admit engine demos?
    (   narrative_ontology:constraint_claim(catholic_church_1200,_) -> D=yes ; D=no ),
    format("catholic_church_1200_has_claim=~w~n", [D]),
    % phantom traversability witness: a phantom has neighbors via reverse edges
    (   NoClaim = [Ph|_],
        constraint_indexing:default_context(Ctx),
        drl_purity_network:constraint_neighbors(Ph, Ctx, PhN)
    ->  length(PhN, NPhN), format("phantom_~w_neighbor_count=~w~n", [Ph, NPhN])
    ;   format("phantom_neighbor_probe=FAILED~n") ).

:- probe, halt.
:- halt(1).
