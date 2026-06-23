% OQ-112 Round 3 — Round 0 probe (READ-ONLY).
% Re-witnesses on the pinned 92 corpus:
%   A  re-pin 92 (corpus_constraint membership count + constraint_claim count)
%   B  firing re-witness per non-sentinel source {base_extractiveness(eps),
%      extractiveness_for_agent(chi), constraint_metric(theater)}: which
%      constraint_claim-bearing constraints (the maxent driver enumeration) lack it?
%   C  per-absent-source: does dr_type still classify it? (the silent-drop predictor:
%      eps/chi block dr_type's is_X first; theater does NOT — so theater is the live
%      item-4 residual that flows with a fabricated 0.0)
%   D  positive control: get_constraint_metrics on an absent-source constraint shows
%      the fabricated 0.0 (else-branch FIRED), and it flows into continuous_log_likelihood
%   E  Commit-2 mechanism: of constraints that dr_type DOES classify, do any lack theater?
%      Such a constraint is collected into compute_type_profile's theater findall with a
%      fabricated 0.0 today (item-4 live); post-Commit-1 it would carry `unknown`.

:- [stack].
:- corpus_loader:ensure_corpus_loaded.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

probe :-
    config:param(suppression_metric_name, SuppName),
    config:param(theater_metric_name, TheaterName),
    constraint_indexing:default_context(Ctx),
    format('theater_metric_name = ~w ; default_context = ~w~n~n', [TheaterName, Ctx]),

    % ---- A: re-pin 92 ----
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorpus),
    findall(C, narrative_ontology:constraint_claim(C, _), Claims0),
    sort(Claims0, Claims),
    length(Claims, NClaims),
    format('A: corpus_constraint=~w ; distinct constraint_claim subjects=~w~n', [NCorpus, NClaims]),
    % how many claim-subjects are NOT corpus members (engine demos)?
    findall(C, (member(C, Claims), \+ corpus_loader:corpus_constraint(C)), NonCorpus),
    length(NonCorpus, NNon),
    format('A: claim-subjects not in corpus (engine demos) = ~w: ~w~n~n', [NNon, NonCorpus]),

    % ---- B: firing re-witness — claim-bearing constraints lacking each source ----
    findall(C, (corpus_loader:corpus_constraint(C),
                narrative_ontology:constraint_claim(C, _),
                \+ drl_core:base_extractiveness(C, _)), AbsEps),
    findall(C, (corpus_loader:corpus_constraint(C),
                narrative_ontology:constraint_claim(C, _),
                \+ constraint_indexing:extractiveness_for_agent(C, Ctx, _)), AbsChi),
    findall(C, (corpus_loader:corpus_constraint(C),
                narrative_ontology:constraint_claim(C, _),
                \+ narrative_ontology:constraint_metric(C, TheaterName, _)), AbsTheater),
    length(AbsEps, NE), length(AbsChi, NC), length(AbsTheater, NT),
    format('B: claim+corpus lacking base_extractiveness = ~w: ~w~n', [NE, AbsEps]),
    format('B: claim+corpus lacking extractiveness_for_agent = ~w: ~w~n', [NC, AbsChi]),
    format('B: claim+corpus lacking theater(~w) = ~w: ~w~n~n', [TheaterName, NT, AbsTheater]),

    % ---- C: does absence block dr_type? (per source, sample first if any) ----
    ( AbsEps = [Ce|_] -> three_way(drl_core:dr_type(Ce, Ctx, Te), Re),
        format('C: dr_type(absent-eps ~w) -> ~w (Type=~w)~n', [Ce, Re, Te]) ; format('C: no absent-eps claim-constraint~n') ),
    ( AbsChi = [Cc|_] -> three_way(drl_core:dr_type(Cc, Ctx, Tc), Rc),
        format('C: dr_type(absent-chi ~w) -> ~w (Type=~w)~n', [Cc, Rc, Tc]) ; format('C: no absent-chi claim-constraint~n') ),
    ( AbsTheater = [Ct|_] -> three_way(drl_core:dr_type(Ct, Ctx, Tt), Rt),
        format('C: dr_type(absent-theater ~w) -> ~w (Type=~w)~n~n', [Ct, Rt, Tt]) ; format('C: no absent-theater claim-constraint~n~n') ),

    % ---- D: positive control — fabricated 0.0 flows through get_constraint_metrics ----
    ( AbsTheater = [Cd|_]
    ->  maxent_classifier:get_constraint_metrics(Cd, EpsD, SuppD, TheaterD),
        format('D: get_constraint_metrics(absent-theater ~w): eps=~w supp=~w theater=~w~n', [Cd, EpsD, SuppD, TheaterD]),
        format('D verdict: theater else-branch ~w~n~n',
               [(TheaterD == 0.0 -> 'FIRED (fabricated 0.0 flows into arith)' ; 'not fired')])
    ;   ( AbsEps = [Cd|_]
        ->  maxent_classifier:get_constraint_metrics(Cd, EpsD, SuppD, TheaterD),
            format('D: get_constraint_metrics(absent-eps ~w): eps=~w supp=~w theater=~w~n~n', [Cd, EpsD, SuppD, TheaterD])
        ;   format('D: no live absent-source claim-constraint to trace~n~n') )
    ),

    % ---- E: Commit-2 mechanism — classifiable constraints lacking theater ----
    % these are collected into compute_type_profile(_,_,_,theater)'s findall WITH a value
    % (0.0 today, `unknown` post-Commit-1). If empty on 92 -> Commit-2 latent.
    findall(C-T, (corpus_loader:corpus_constraint(C),
                  narrative_ontology:constraint_claim(C, _),
                  drl_core:dr_type(C, Ctx, T),
                  \+ narrative_ontology:constraint_metric(C, TheaterName, _)), ClassifiableNoTheater),
    length(ClassifiableNoTheater, NCNT),
    format('E: classifiable(dr_type succeeds) BUT lacking theater = ~w~n', [NCNT]),
    format('E: ~w~n', [ClassifiableNoTheater]),
    format('E verdict: Commit-2 findall theater-drop/throw is ~w on 92~n',
           [(NCNT > 0 -> 'LIVE (a classifiable constraint enters the theater findall metric-absent)' ; 'LATENT (no classifiable-but-theater-absent constraint)')]).

:- (catch(probe, E, (format('PROBE ERROR: ~w~n', [E]), fail)) -> true ; format('PROBE FAILED~n')), halt.
:- halt(1).
