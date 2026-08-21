% ============================================================================
% OQ-112 item 7 — wm_token/3 + wm_emit/3 four-state control
%
% Forces every state the helper can route and pastes the resulting JSON token.
% A control that only forces the happy path proves nothing about the collapse
% this fix exists to break (the introduced instrument is itself a claim).
%
%   STATE 1  genuine measured 0.0   -> float 0.000000   (was 0.0; unchanged)
%   STATE 1b genuine nonzero float  -> float 0.400000   (float passes through)
%   STATE 2  absent (no maxent dist)-> null             (was 0.0)
%   STATE 3  errored (producer throws) -> "errored"     (was 0.0)
%   STATE 4  succeed-with-unbound-M -> "errored"        (defensive guard)
%
% States 1-3 run the REAL shipped wm_token via a maxent_dist/3 overlay
% (probe_harness:with_overlay — snapshot/restore + cache clear).
%
% STATE 4 is structurally unreachable through the real producer: it is STATIC
% (cannot be extended at runtime) and its only success path runs
% extract_chain_probs/3, whose terminal `IncompMass is max(0.0, ...)` always
% binds Mass or throws. So state 4 is a *defensive* guard against a future
% producer change. We (a) print the producer's static/dynamic property, (b)
% paste the shipped wm_token clause so the guard subterm is visible, and (c)
% run a guard-decision control: the exact guard goal with Tok and M unbound
% must bind Tok=errored. (b)+(c) together exercise the branch's logic; the
% reader can diff the control goal against the pasted shipped clause.
%
% Run:  swipl -q -l stack.pl -l json_report.pl \
%             -g "consult('../audits/2026-06-22_oq112_round2/probe_item7_wm_token_controls.pl'), run, halt" \
%             -t "halt(1)"
% ============================================================================

:- use_module(probe_harness).

emit_to_string(Key, Tok, Str) :-
    with_output_to(string(Str), ( current_output(S), wm_emit(S, Key, Tok) )).

run :-
    measurement_layer:wasserstein_contexts([Ctx1|_]),
    format("=== producer property ===~n"),
    (   predicate_property(measurement_layer:wasserstein_incomparable_mass(_,_,_), dynamic)
    ->  writeln('  producer wasserstein_incomparable_mass/3: DYNAMIC')
    ;   writeln('  producer wasserstein_incomparable_mass/3: STATIC (state 4 unreachable via real producer; guard is defensive)')
    ),

    format("~n=== shipped wm_token/3 clause (guard subterm must match the state-4 control goal) ===~n"),
    (   clause(wm_token(_C,_Ctx,_Tok), Body)
    ->  portray_clause((wm_token('C','Ctx','Tok') :- Body))
    ;   writeln('  !! NO CLAUSE FOR wm_token/3 — fix did not load')
    ),

    % ---- STATE 1: genuine measured 0.0 (all mass on chain types) ----
    format("~n=== STATE 1: genuine measured 0.0 ===~n"),
    probe_harness:with_overlay(
        [],
        [
         reach_undeclared(retrofit('2026-08-21', "with_overlay/3 with an EMPTY template list: no declared query shape (OQ-326 clause 4')"),
           maxent_classifier:maxent_dist(probe_zero, Ctx1,
            [mountain-1.0, rope-0.0, tangled_rope-0.0, snare-0.0]))
        ],
        ( wm_token(probe_zero, Ctx1, T1), emit_to_string(u1, T1, J1) )),
    format("  token=~q  json=~w   (expect: float, 0.000000)~n", [T1, J1]),

    % ---- STATE 1b: genuine nonzero float (0.4 mass off-chain) ----
    format("~n=== STATE 1b: genuine nonzero float ===~n"),
    probe_harness:with_overlay(
        [],
        [
         reach_undeclared(retrofit('2026-08-21', "with_overlay/3 with an EMPTY template list: no declared query shape (OQ-326 clause 4')"),
           maxent_classifier:maxent_dist(probe_nz, Ctx1,
            [mountain-0.6, scaffold-0.4]))
        ],
        ( wm_token(probe_nz, Ctx1, T1b), emit_to_string(u1, T1b, J1b) )),
    format("  token=~q  json=~w   (expect: float, 0.400000)~n", [T1b, J1b]),

    % ---- STATE 2: absent — no maxent_dist for this constraint ----
    format("~n=== STATE 2: absent (producer fails) ===~n"),
    ( wm_token(probe_absent_never_asserted, Ctx1, T2) -> true ; T2 = '<wm_token failed>' ),
    emit_to_string(u1, T2, J2),
    format("  token=~q  json=~w   (expect: absent, null)~n", [T2, J2]),

    % ---- STATE 3: errored — producer throws (non-numeric prob) ----
    format("~n=== STATE 3: errored (producer throws) ===~n"),
    probe_harness:with_overlay(
        [],
        [
         reach_undeclared(retrofit('2026-08-21', "with_overlay/3 with an EMPTY template list: no declared query shape (OQ-326 clause 4')"),
           maxent_classifier:maxent_dist(probe_err, Ctx1, [mountain-not_a_number]))
        ],
        ( wm_token(probe_err, Ctx1, T3), emit_to_string(u1, T3, J3) )),
    format("  token=~q  json=~w   (expect: errored, \"errored\")~n", [T3, J3]),

    % ---- STATE 4: succeed-with-unbound-M — guard-decision control ----
    format("~n=== STATE 4: succeed-with-unbound-M (defensive guard control) ===~n"),
    format("  unreachability witness: producer STATIC + extract_chain_probs ends in `is/2` (binds or throws)~n"),
    % The guard subterm copied verbatim from the shipped clause above; with the
    % producer-success branch entered (catch recovery did NOT fire, so Tok is
    % unbound) and M unbound, it must bind Tok=errored — NOT Tok=M (a JSON hole).
    ( ( nonvar(GuardTok) -> true ; var(GuardM) -> GuardTok = errored ; GuardTok = GuardM )
      -> true ; GuardTok = '<guard failed>' ),
    emit_to_string(u1, GuardTok, J4),
    format("  guard goal with Tok,M both unbound -> token=~q  json=~w   (expect: errored, \"errored\")~n",
           [GuardTok, J4]),

    % ---- Verdict ----
    format("~n=== VERDICT ===~n"),
    (   T1 == 0.0, number(T1b), abs(T1b - 0.4) < 1.0e-9,
        T2 == absent, T3 == errored, GuardTok == errored,
        J1 == "\"u1\": 0.000000",
        J1b == "\"u1\": 0.400000",
        J2 == "\"u1\": null",
        J3 == "\"u1\": \"errored\"",
        J4 == "\"u1\": \"errored\""
    ->  writeln('  ALL FOUR STATES ROUTED CORRECTLY — PASS')
    ;   writeln('  *** MISMATCH — FAIL ***')
    ).
