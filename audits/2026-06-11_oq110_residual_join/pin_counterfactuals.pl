/* OQ-110 §1.3 — ε-pinned and supp-pinned counterfactuals on EVERY backed flip.

   PRE-REGISTERED criterion (plan, pinned at approval; wrongly-specified =>
   halt-and-escalate, never inline-amend):
     A backed flip (T1->T2) is ε-explained iff the ε-pinned counterfactual —
     the base_extractiveness measurement at T2 overlaid with its T1 value, all
     other inputs as authored — run through classify_at_time/5 at T2 does NOT
     produce the type change. A flip that survives ε-pinning is ε-unexplained
     (residual). On every backed flip the supp-pinned twin also runs
     (suppression_requirement at T2 overlaid with its T1 value); the residual
     set is bucketed: supp-explained vs genuinely unexplained. Both pins run
     on every backed flip including d≈0 ones.

   Implementation notes (resolved against code before pinning):
   - ε/supp are NOT arguments to classify_at_time/5; they enter via
     narrative_ontology:measurement/5 (drl_composition.pl:236, :217). Pinning
     = overlaying the measurement fact at T2 with the T1-READ value (the snap
     value), via probe_harness:with_overlay/3 (caches auto-cleared, restore
     verified). Never argument surgery.
   - "Does NOT produce the type change" is implemented as PinnedType \== To.
     The full pinned type is recorded, so a third-type outcome (neither From
     nor To) stays visible and separable downstream.
   - Backed flips guarantee eps measurements exist at T1 and T2. Suppression
     may be backed via the static-marker scalar (no series); the supp pin then
     asserts a measurement at T2 carrying the T1-read value — same value down
     the same math, provably a no-op, run anyway per the criterion.

   Per-process positive controls (pre-registered):
   - OVERLAY-TOOK-EFFECT: inside every overlay, the T2 fact is read back and
     must carry exactly the pin value, exactly once.
   - IDENTITY PIN (two-sided): the first enumerated flip is re-pinned with its
     ORIGINAL T2 value; the flip must SURVIVE (overlay path itself does not
     perturb classification).
   - EXPECTED-VANISH: adjunctification_of_university_teaching powerless flip
     t=10->20 (d_eps=0.14; 1.1 control C showed eps@20 removal reverts the
     type) must be ε-explained. Failure = halt-and-investigate.
   - Any classify_at_time failure under a pin is recorded as FAIL and the run
     exits nonzero (errored is not a verdict).

   Output: ../outputs/oq110_pin_results.tsv
     C, CtxLabel, T1, T2, From, To, E1, E2, Su1, Su2, EpsPinType, SuppPinType

   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq110_residual_join/pin_counterfactuals.pl'), run, halt" -t "halt(1)"
*/

:- [stack].
:- corpus_loader:ensure_corpus_loaded.
:- use_module(probe_harness).

%% backed_flip(-C, -Ctx, -Label, -T1, -T2, -From, -To, -E1, -E2, -Su1, -Su2)
%  Adjacent backed->backed type changes — exactly temporal_residual's flip set.
backed_flip(C, Ctx, Label, T1, T2, From, To, E1, E2, Su1, Su2) :-
    corpus_loader:corpus_constraint(C),
    temporal_residual:residual_contexts(Ctxs),
    member(Ctx, Ctxs),
    temporal_residual:context_label(Ctx, Label),
    temporal_residual:snapshot_seq(C, Ctx, Seq),
    append(_, [state(T1, From, _, E1, Su1, _, true),
               state(T2, To,   _, E2, Su2, _, true) | _], Seq),
    From \== To.

%% pin_classify(+C, +Ctx, +T2, +Metric, +PinVal, -OutType)
%  Overlay measurement(Metric)@T2 with PinVal, verify the overlay took effect
%  (exactly one fact, carrying PinVal), classify at T2.
pin_classify(C, Ctx, T2, Metric, PinVal, OutType) :-
    probe_harness:with_overlay(
        [narrative_ontology:measurement(_, C, Metric, T2, _)],
        [narrative_ontology:measurement(oq110_pin, C, Metric, T2, PinVal)],
        ( findall(V, narrative_ontology:measurement(_, C, Metric, T2, V), Vs),
          (   Vs = [Vr], Vr =:= PinVal
          ->  true
          ;   format("HALT: overlay-took-effect failed for ~w ~w@~w: ~w~n",
                     [C, Metric, T2, Vs]),
              throw(oq110_overlay_ineffective(C, Metric, T2))
          ),
          (   catch(drl_composition:classify_at_time(C, T2, Ctx, OutType0, _),
                    Err,
                    ( print_message(error, Err), fail ))
          ->  OutType = OutType0
          ;   OutType = 'FAIL'
          )
        )).

run :-
    findall(f(C, Ctx, Label, T1, T2, From, To, E1, E2, Su1, Su2),
            backed_flip(C, Ctx, Label, T1, T2, From, To, E1, E2, Su1, Su2),
            Flips),
    length(Flips, NF),
    format("enumerated ~w backed flips in-process~n", [NF]),

    % --- two-sided control 1: identity pin on the first flip must SURVIVE ---
    Flips = [f(C0, Ctx0, L0, T10, T20, From0, To0, _, E20, _, _) | _],
    pin_classify(C0, Ctx0, T20, base_extractiveness, E20, IdType),
    format("identity-pin control: ~w ~w t=~w->~w ~w->~w, eps@~w re-pinned to own value ~w => type ~w~n",
           [C0, L0, T10, T20, From0, To0, T20, E20, IdType]),
    (   IdType == To0
    ->  format("IDENTITY-PIN CONTROL PASS (flip survives identity pin)~n")
    ;   format("IDENTITY-PIN CONTROL FAIL: overlay path perturbs classification~n"),
        throw(oq110_identity_pin_failed)
    ),

    % --- run both pins on every backed flip ---
    open('../outputs/oq110_pin_results.tsv', write, S),
    format(S, "constraint\tcontext\tt1\tt2\tfrom\tto\teps1\teps2\tsupp1\tsupp2\teps_pin_type\tsupp_pin_type~n", []),
    forall(member(f(C, Ctx, Label, T1, T2, From, To, E1, E2, Su1, Su2), Flips),
           ( pin_classify(C, Ctx, T2, base_extractiveness, E1, EpsPinT),
             pin_classify(C, Ctx, T2, suppression_requirement, Su1, SuppPinT),
             format(S, "~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n",
                    [C, Label, T1, T2, From, To, E1, E2, Su1, Su2, EpsPinT, SuppPinT])
           )),
    close(S),

    % --- control 2: expected-vanish (adjunctification powerless t=10->20) ---
    (   member(f(adjunctification_of_university_teaching, CtxEV,
                 'powerless/biographical/trapped/local',
                 10, 20, tangled_rope, snare, EV1, _, _, _), Flips)
    ->  pin_classify(adjunctification_of_university_teaching,
                     CtxEV, 20, base_extractiveness, EV1, EVType),
        (   EVType \== snare
        ->  format("EXPECTED-VANISH CONTROL PASS: eps-pin reverts adjunctification t=20 to ~w~n", [EVType])
        ;   format("EXPECTED-VANISH CONTROL FAIL: known eps-driven flip survives eps-pin~n"),
            throw(oq110_expected_vanish_failed)
        )
    ;   format("EXPECTED-VANISH CONTROL FAIL: control flip not in enumeration~n"),
        throw(oq110_control_flip_missing)
    ),
    format("pin run complete: ~w flips x 2 pins; results in outputs/oq110_pin_results.tsv~n", [NF]).
