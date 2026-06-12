/* OQ-110 §1.1 — Backed end-to-end semantic verification (pre-Phase-B plumbing).

   Trace under test: drl_composition:classify_at_time/5 -> snap(D,Backed,...)
   (Backed = EpsBacked /\ SuppBacked, drl_composition.pl:256) ->
   temporal_residual:scan_transitions/3 (both endpoints Backed=true else
   fabrication_adjacent, temporal_residual.pl:70-85) ->
   json_report:write_temporal_residual/2 -> pipeline_output.json.

   Per-process positive controls (pre-registered, plan §1.1):
   A. agenda_conditioning, powerless ctx: suppression series authored at
      T={0,20,40,50}, ABSENT at T={10,30} with scalar present and no static
      marker -> SuppBacked=false (OQ-105 grid-misalignment bucket). A known
      type-change spanning an unbacked endpoint must be EXCLUDED from flips
      and counted in fabrication_adjacent_transitions.
   B. adjunctification_of_university_teaching, powerless ctx: known backed
      flip t=10->20 tangled_rope->snare must be PRESENT in flips with deltas
      matching the serialized JSON (d_eps 0.14 / d_supp 0.04 / d_theater 0.11).
   C. Manipulation control (proves in THIS process that the Backed bit gates
      the bucketing, i.e. the probe would flag a regression): retract the
      eps measurement at t=20 via probe_harness:with_retracted (caches
      auto-cleared, restore verified) -> the t=10->20 flip must LEAVE flips;
      post-restore it must return.

   Substrate: live corpus; manifest = outputs/pipeline_output.json
   (pipeline_run_at 2026-06-12T00:59:49Z, code_commit c22ec561, code_dirty false,
   n_constraints 62).

   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq110_residual_join/backed_semantic_probe.pl'), run, halt" -t "halt(1)"
*/

:- [stack].
:- corpus_loader:ensure_corpus_loaded.
:- use_module(probe_harness).

ctx_by_power(P, Ctx) :-
    constraint_indexing:site_contexts_canonical(C4),
    member(Ctx, C4),
    Ctx = context(agent_power(P), _, _, _), !.

print_seq(C, Ctx) :-
    temporal_residual:snapshot_seq(C, Ctx, Seq),
    forall(member(state(T,Ty,D,E,Su,Th,B), Seq),
           format("    t=~w type=~w backed=~w eps=~w supp=~w theater=~w d=~w~n",
                  [T,Ty,B,E,Su,Th,D])).

% --- CONTROL A: fab_adjacent transition witnessed EXCLUDED ---
control_a :-
    C = agenda_conditioning,
    ctx_by_power(powerless, Ctx),
    format("~n=== CONTROL A: fab_adjacent EXCLUDED (~w, powerless ctx) ===~n", [C]),
    forall(member(T, [10, 30]),
           ( ( narrative_ontology:measurement(_, C, suppression_requirement, T, _)
             -> format("    t=~w: UNEXPECTED suppression measurement present~n", [T]), fail
             ;  format("    t=~w: no suppression measurement", [T]) ),
             ( narrative_ontology:constraint_metric(C, suppression_requirement, SV)
             -> format("; scalar ~w present", [SV])
             ;  format("; NO scalar") ),
             ( narrative_ontology:suppression_profile(C, static)
             -> format("; static marker PRESENT (unexpected)~n")
             ;  format("; no static marker => SuppBacked=false (OQ-105 misalignment bucket)~n") )
           )),
    print_seq(C, Ctx),
    temporal_residual:residual_for_context(C, Ctx, ctx_residual(NT, NB, Flips, Fab)),
    format("    residual: times_examined=~w backed_times=~w fab_adjacent=~w~n    flips=~w~n",
           [NT, NB, Fab, Flips]),
    temporal_residual:snapshot_seq(C, Ctx, Seq),
    (   append(_, [state(T1,Ty1,_,_,_,_,B1), state(T2,Ty2,_,_,_,_,B2)|_], Seq),
        Ty1 \== Ty2, ( B1 == false ; B2 == false )
    ->  format("    witness: type-change ~w->~w at t=~w->~w spans unbacked endpoint (B1=~w, B2=~w)~n",
               [Ty1, Ty2, T1, T2, B1, B2]),
        (   \+ memberchk(flip(T1, T2, _, _, _, _, _), Flips)
        ->  true
        ;   format("    CONTROL A FAIL: transition appears in flips~n"), fail )
    ;   format("    CONTROL A FAIL: no unbacked-adjacent type change in seq (witness missing)~n"),
        fail
    ),
    (   Flips == [], Fab >= 1
    ->  format("    CONTROL A PASS: transition excluded from flips; fab_adjacent=~w~n", [Fab])
    ;   format("    CONTROL A FAIL: expected flips=[] and fab_adjacent>=1~n"), fail
    ).

% --- CONTROL B: backed flip witnessed PRESENT with deltas ---
control_b :-
    C = adjunctification_of_university_teaching,
    ctx_by_power(powerless, Ctx),
    format("~n=== CONTROL B: backed flip PRESENT (~w, powerless ctx) ===~n", [C]),
    print_seq(C, Ctx),
    temporal_residual:residual_for_context(C, Ctx, ctx_residual(NT, NB, Flips, Fab)),
    format("    residual: times_examined=~w backed_times=~w fab_adjacent=~w~n    flips=~w~n",
           [NT, NB, Fab, Flips]),
    (   memberchk(flip(10, 20, tangled_rope, snare, DE, DS, DT), Flips)
    ->  format("    flip(10,20,tangled_rope,snare): d_eps=~w d_supp=~w d_theater=~w~n",
               [DE, DS, DT]),
        (   abs(DE - 0.14) < 0.005, abs(DS - 0.04) < 0.005, abs(DT - 0.11) < 0.005
        ->  format("    CONTROL B PASS: present, deltas match serialized JSON (0.14/0.04/0.11)~n")
        ;   format("    CONTROL B FAIL: deltas do not match JSON~n"), fail )
    ;   format("    CONTROL B FAIL: expected flip not found~n"), fail
    ).

% --- CONTROL C: Backed bit gates bucketing (manipulation + restore witness) ---
control_c :-
    C = adjunctification_of_university_teaching,
    ctx_by_power(powerless, Ctx),
    format("~n=== CONTROL C: retract eps@20 -> flip(10,20) must leave flips ===~n"),
    probe_harness:with_retracted(
        [narrative_ontology:measurement(_, C, base_extractiveness, 20, _)],
        ( temporal_residual:residual_for_context(C, Ctx, ctx_residual(_, NB1, Flips1, Fab1)),
          format("    under retraction: backed_times=~w fab_adjacent=~w~n    flips=~w~n",
                 [NB1, Fab1, Flips1]),
          (   \+ memberchk(flip(10, 20, _, _, _, _, _), Flips1)
          ->  format("    in-overlay PASS: flip(10,20) excluded from flips~n")
          ;   format("    CONTROL C FAIL: flip(10,20) survives eps@20 retraction~n"), fail )
        )),
    temporal_residual:residual_for_context(C, Ctx, ctx_residual(_, NB2, Flips2, _)),
    (   memberchk(flip(10, 20, tangled_rope, snare, _, _, _), Flips2)
    ->  format("    RESTORE WITNESS: flip(10,20) present again post-restore (backed_times=~w)~n", [NB2]),
        format("    CONTROL C PASS~n")
    ;   format("    CONTROL C FAIL: flip absent after restore~n"), fail
    ).

run :-
    format("OQ-110 1.1 Backed end-to-end semantic verification (pre-Phase-B plumbing)~n"),
    control_a,
    control_b,
    control_c,
    format("~nALL CONTROLS PASS~n").
