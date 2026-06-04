% Probe 2 — Counterfactual: retract tuple-T mountain classifications, re-run signatures.
% Tuple T = (agent_power(analytical), exit_options(analytical)); time_horizon/spatial_scope free.
% Run from prolog/:  swipl -g "[stack], corpus_loader:load_all_testsets, consult('../outputs/fnl_probe2_counterfactual.pl'), run_probe2, halt" -t "halt(1)"
% Output: ../outputs/fnl_probe2_counterfactual.jsonl  {"id", "before", "after"}
% Discipline (swipl_load_path_and_probe_gotchas.md §4): findall facts FIRST, then retract.
% One-shot process: no restore needed (process exits; no files modified).
% constraint_signature/2 always called with Sig unbound (lock cuts respected).

run_probe2 :-
    setof(C, M^V^(narrative_ontology:constraint_metric(C, M, V), atom(C)), Cs0),
    exclude(==(catholic_church_1200), Cs0, Cs),
    length(Cs, N),
    format(user_error, "probe2: n_constraints=~w~n", [N]),

    % --- Phase A: baseline sweep ---
    findall(C-Sig, (member(C, Cs), sig_of(C, Sig)), Baseline),
    length(Baseline, NB),
    format(user_error, "probe2: baseline swept ~w~n", [NB]),

    % --- Phase B: retract tuple-T mountain facts (findall first, then retract) ---
    findall(
        constraint_indexing:constraint_classification(C, mountain,
            context(agent_power(analytical), time_horizon(TH),
                    exit_options(analytical), spatial_scope(SS))),
        constraint_indexing:constraint_classification(C, mountain,
            context(agent_power(analytical), time_horizon(TH),
                    exit_options(analytical), spatial_scope(SS))),
        Facts),
    length(Facts, NF),
    format(user_error, "probe2: tuple-T mountain facts found=~w~n", [NF]),
    forall(member(F, Facts), retract(F)),
    % verify none remain
    (   constraint_indexing:constraint_classification(_, mountain,
            context(agent_power(analytical), _, exit_options(analytical), _))
    ->  format(user_error, "probe2: RETRACT INCOMPLETE — aborting~n", []), halt(1)
    ;   format(user_error, "probe2: retract verified complete~n", [])
    ),

    % --- Clear memo caches (boltzmann grid is metric-driven, but clear regardless) ---
    retractall(boltzmann_compliance:cached_classification(_, _, _)),
    retractall(boltzmann_compliance:cached_coupling(_, _)),
    format(user_error, "probe2: caches cleared~n", []),

    % --- Sensitivity control FIRST (pre-named prediction: false_ci_rope) ---
    sig_of(abrahamic_covenant__land_promise_constraint, CtrlSig),
    format(user_error, "probe2: SENSITIVITY CONTROL abrahamic_covenant__land_promise_constraint after-retract signature=~w (predicted: false_ci_rope)~n", [CtrlSig]),
    (   CtrlSig == false_ci_rope
    ->  format(user_error, "probe2: sensitivity control PASS~n", [])
    ;   format(user_error, "probe2: sensitivity control FAIL — corpus diff untrusted~n", []), halt(1)
    ),

    % --- Phase C: post-retract sweep ---
    open('../outputs/fnl_probe2_counterfactual.jsonl', write, S),
    forall(member(C-Before, Baseline),
           ( sig_of(C, After),
             format(S, '{"id": "~w", "before": "~w", "after": "~w"}~n', [C, Before, After]) )),
    close(S),
    format(user_error, "probe2: done~n", []).

sig_of(C, Sig) :-
    (   signature_detection:constraint_signature(C, Sig0)
    ->  Sig = Sig0
    ;   Sig = none
    ).
