% Probe 1 + 1b — FNL claim-source / FCR appearance-source attribution.
% Run from prolog/:  swipl -g "[stack], corpus_loader:load_all_testsets, consult('../outputs/fnl_probe1_attribution.pl'), run_probe1, halt" -t "halt(1)"
% Output: ../outputs/fnl_probe1_attribution.jsonl (one line per corpus constraint).
% NOTE: constraint_signature/2 is called with Sig UNBOUND (clause order respected);
% never with the signature atom bound (bound-probe bypasses lock cuts — ISSUES.md:1150).

run_probe1 :-
    setof(C, M^V^(narrative_ontology:constraint_metric(C, M, V), atom(C)), Cs0),
    exclude(==(catholic_church_1200), Cs0, Cs),
    length(Cs, N),
    format(user_error, "probe1: n_constraints=~w~n", [N]),

    % --- Positive controls, BEFORE the sweep ---
    % Control A (source-1 dispatch): a claimed-mountain constraint must return
    % explicit_mountain_claim from claimed_natural/2 (proves the probe sees source 1).
    (   narrative_ontology:constraint_claim(Ctrl, mountain), atom(Ctrl)
    ->  (   signature_detection:claimed_natural(Ctrl, CtrlClaim)
        ->  format(user_error, "control_A: ~w claimed_natural=~w (expect explicit_mountain_claim)~n",
                   [Ctrl, CtrlClaim])
        ;   format(user_error, "control_A: FAILED — claimed_natural/2 has no solution for ~w~n", [Ctrl])
        )
    ;   format(user_error, "control_A: FAILED — no claimed-mountain constraint found~n", [])
    ),

    % --- Sweep ---
    open('../outputs/fnl_probe1_attribution.jsonl', write, S),
    forall(member(C, Cs), report_one(S, C)),
    close(S),
    format(user_error, "probe1: done~n", []).

report_one(S, C) :-
    (   signature_detection:constraint_signature(C, Sig)
    ->  true
    ;   Sig = none
    ),
    (   Sig == false_natural_law,
        signature_detection:false_natural_law(C, fnl_evidence(Claim, _, _, _, _))
    ->  true
    ;   Claim = na
    ),
    (   Sig == false_ci_rope,
        signature_detection:false_ci_rope(C, fcr_evidence(App, _, _, _, _, _))
    ->  true
    ;   App = na
    ),
    format(S, '{"id": "~w", "signature": "~w", "fnl_claim_source": "~w", "fcr_appearance": "~w"}~n',
           [C, Sig, Claim, App]).
