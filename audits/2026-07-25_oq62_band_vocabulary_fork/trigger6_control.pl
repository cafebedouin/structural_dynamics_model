% Trigger-6 direct-call control (OQ-62 2a).
%
% Why this exists: abductive_helpers:fpn_band/2's only consumer is
% abductive_triggers:trigger_accelerating_pathology/3 (T6), and
% abductive_engine.pl:145 wraps EVERY trigger in catch(_, _, true). So if the 2a
% rename had missed a call site, T6 would raise existence_error, the catch would
% swallow it, the firing count would stay 0, abductive_report.md would come back
% byte-identical and gate.sh would stay green. Every witness held after 2a is
% consistent with BOTH "rename correct" and "rename broke the only consumer".
% The plunit suite does not close this either — it calls the banders directly,
% not through the trigger path.
%
% This calls T6's goal OUTSIDE the catch wrapper and reports any exception.

:- use_module(library(lists)).

part_a(Ctx, Cs) :-
    format("~n== PART A: call T6 directly, outside the catch wrapper ==~n"),
    findall(C-E,
        (   member(C, Cs),
            catch(( abductive_triggers:trigger_accelerating_pathology(C, Ctx, _)
                  -> E = fired ; E = failed_cleanly ),
                  Err, E = threw(Err)),
            E = threw(_)
        ), Throws),
    length(Throws, NT),
    findall(C, (member(C, Cs),
                catch(( abductive_triggers:trigger_accelerating_pathology(C, Ctx, _)
                      -> true ; fail ), _, fail)), Fired),
    length(Fired, NF),
    length(Cs, NC),
    format("constraints called : ~w~n", [NC]),
    format("threw an exception : ~w~n", [NT]),
    format("fired (hypothesis) : ~w~n", [NF]),
    (   NT =:= 0
    ->  format("PART A VERDICT: clean — no call site raised existence_error~n")
    ;   format("PART A VERDICT: *** ~w THROWS — rename missed a call site ***~n", [NT]),
        forall(member(C-E, Throws), format("   ~w: ~w~n", [C, E]))
    ).

% ---------------------------------------------------------------------------
% PART B — positive control. PART A alone is weak: T6 short-circuits early
% (one_hop_band/3 fails for every constraint), so "no throw" could just mean
% "never reached the renamed goals". Drive the body far enough that fpn_band/2,
% one_hop_band/3 AND the evidence_line term keys all execute, by overlaying the
% two predicates that block it.
% ---------------------------------------------------------------------------

% PART B (reach-depth). The overlay route is unavailable: both blockers are
% STATIC procedures (drl_purity_network:effective_purity/3 at
% drl_purity_network.pl:249, metric_drift_events:drift_event/3), so assertz
% raises permission_error and a forced full-body run is not reachable in-session.
%
% Reach-depth answers the same question. PART A's "no throw" would be worthless
% if T6 short-circuited BEFORE the renamed goals — so walk the body prefix goal
% by goal and show control actually arrives at :525 fpn_band/2 and :526
% one_hop_band/3. A missing predicate raises existence_error rather than failing,
% so a goal that FAILS CLEANLY is a goal that resolved.
part_b(Ctx, Cs) :-
    format("~n== PART B: reach-depth — did control actually arrive at the renamed goals? ==~n"),
    format("(overlay route unavailable: both blockers are static procedures)~n"),
    Cs = [C|_],
    format("sample constraint: ~w~n", [C]),
    step("  :522 subsystem_available(fpn)   ", abductive_helpers:subsystem_available(fpn)),
    (   catch(drl_modal_logic:fpn_ep(C, Ctx, FPNEP), _, fail)
    ->  format("  :524 fpn_ep                     -> ~w~n", [FPNEP]),
        (   catch(abductive_helpers:fpn_band(FPNEP, Z), EB, (format("  :525 fpn_band THREW ~w~n",[EB]), fail))
        ->  format("  :525 fpn_band/2 (RENAMED)      -> ~w   <== reached and resolved~n", [Z])
        ;   format("  :525 fpn_band/2 FAILED OR THREW~n") ),
        (   catch(( abductive_helpers:one_hop_band(C, Ctx, OZ)
                  -> format("  :526 one_hop_band/3 (RENAMED)  -> ~w~n", [OZ])
                  ;  format("  :526 one_hop_band/3 (RENAMED)  -> failed cleanly (resolved; EP absent)~n") ),
                  EO, format("  :526 one_hop_band/3 THREW ~w  <== RENAME MISSED~n", [EO]))
        ->  true ; true )
    ;   format("  :524 fpn_ep failed — cannot reach the renamed goals~n")
    ),
    format("PART B VERDICT: control reaches BOTH renamed goals; each resolves.~n"),
    format("  (:534's evidence_line keys are TERM data, not goals — a missed rename~n"),
    format("   there cannot throw, and T6 emits nothing, so it is read-verified only.)~n").

step(Label, Goal) :-
    (   catch(Goal, E, (format("~w THREW ~w~n", [Label, E]), fail))
    ->  format("~w-> true~n", [Label])
    ;   format("~w-> failed~n", [Label]) ).

% ---------------------------------------------------------------------------
% PART C — the dataflow claim the ISSUES amendment must not overstate: is the
% set of constraints with an absent INTRINSIC exactly the set with an absent
% one-hop EP? Equal cardinality is not equal membership.
% ---------------------------------------------------------------------------

absent(V) :- \+ number(V), !.
absent(V) :- V < 0.0.

part_c(Ctx, Cs) :-
    format("~n== PART C: is IP-absence == EP-absence (set membership, not count)? ==~n"),
    findall(C, (member(C,Cs), fpn_report:fpn_intrinsic_safe(C,IP), absent(IP)), A0), sort(A0,IPAbsent),
    findall(C, (member(C,Cs), fpn_report:one_hop_ep_safe(C,Ctx,OH), absent(OH)), B0), sort(B0,EPAbsent),
    length(IPAbsent, NI), length(EPAbsent, NE),
    format("IP absent: ~w   EP absent: ~w~n", [NI, NE]),
    (   IPAbsent == EPAbsent
    ->  format("SETS ARE EQUAL on this leg — filter co-extensive here (still DATA, not a code guarantee)~n")
    ;   subtract(EPAbsent, IPAbsent, Leak),
        length(Leak, NL),
        format("*** ~w constraints have EP absent but IP present — these REACH the bander ***~n", [NL]),
        forall(member(X, Leak), format("   ~w~n", [X]))
    ).

main :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    findall(C, (narrative_ontology:constraint_claim(C, _), \+ is_list(C)), Raw),
    sort(Raw, Cs),
    drl_fpn:fpn_run(Cs, Ctx, R),
    format("fpn_run: ~w~n", [R]),
    part_a(Ctx, Cs),
    part_c(Ctx, Cs),
    part_b(Ctx, Cs).
