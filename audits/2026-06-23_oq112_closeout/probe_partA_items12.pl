% OQ-112 close-out — Part A bite-check, items 1 (C4a) & 2 (maxent completion gate),
% plus the Part-A headline-flip POSITIVE CONTROL. READ-ONLY w.r.t. files.
%
% Faithfully replicates the json_report maxent sequence (retract attempt markers;
% assert classical; maxent_multi_run; assert indexed; maxent_indexed_run) so the
% item-2 void gate is exercised under REAL conditions (stages attempted AND completed)
% -- NOT a vacuous empty gate (maxent_attempted unset would make void_alerts trivially []).

:- consult(stack),
   consult(covering_analysis),
   consult(maxent_classifier),
   consult(dirac_classification),
   consult(diagnostic_summary),
   consult(post_synthesis),
   consult(json_report).

three_way(Goal, R) :- ( catch(Goal, E, R=error(E)) -> (var(R)->R=ok;true) ; R=fail ).

abd_sig(C, Ctx, DetType, A) :-
    ( catch(diagnostic_summary:probe_abductive(C, Ctx, DetType, S),_,S=err) -> true ; S=failed ),
    ( S=agrees->A=agrees ; S=unavailable->A=unavailable ; S=inconclusive->A=inconclusive
    ; S=disagrees(_)->A=disagrees ; A=other ).

join_v(C, JV, VoidPresent) :-
    ( catch(diagnostic_summary:diagnostic_summary(C, Summary),_,fail),
      catch(diagnostic_summary:verdict_join(C, Summary, J),_,fail),
      J = verdict_join(JV0, _, _, Alerts, _, _, _)
    -> JV=JV0,
       ( member(alert(maxent_voided(_),_,_), Alerts) -> VoidPresent=yes ; VoidPresent=no )
    ; JV=none, VoidPresent=no ).

main :-
    corpus_loader:load_all_testsets,
    json_report:load_abductive_data,
    % --- replicate the real maxent stage sequence (item-2 gate live conditions) ---
    retractall(diagnostic_summary:maxent_attempted(_)),
    constraint_indexing:default_context(Ctx),
    measurement_layer:wasserstein_contexts(WCtxs),
    assertz(diagnostic_summary:maxent_attempted(classical)),
    ( catch(maxent_classifier:maxent_multi_run(WCtxs, _),_,fail) -> true ; true ),
    assertz(diagnostic_summary:maxent_attempted(indexed)),
    ( catch(maxent_classifier:maxent_indexed_run(Ctx, _),_,fail) -> true ; true ),
    format('# maxent stages attempted: classical+indexed; run_info present:~n', []),
    ( maxent_classifier:maxent_run_info(Ctx,Nc,_) -> format('#   classical run_info(~w,~w)~n',[Ctx,Nc]) ; format('#   classical run_info ABSENT~n',[]) ),
    ( maxent_classifier:maxent_indexed_run_info(Ctx,Ni,_) -> format('#   indexed run_info(~w,~w)~n',[Ctx,Ni]) ; format('#   indexed run_info ABSENT~n',[]) ),

    % --- ITEM 1 + ITEM 2 field-level over the 92 (baseline) ---
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C-A, (member(C,Cs), abd_sig(C,Ctx,_,A)), AbdRows),
    findall(C, member(C-unavailable, AbdRows), Unavail), length(Unavail, NU),
    findall(C-JV-VP, (member(C,Cs), join_v(C,JV,VP)), JoinRows),
    findall(C, member(C-_-yes, JoinRows), VoidC), length(VoidC, NVoid),
    format('~n=== ITEM 1 (C4a :198/:212/:163) field-level on 92 ===~n', []),
    format('ITEM1 abductive_signal=unavailable count = ~w of 92~n', [NU]),
    format('ITEM1 members: ~w~n', [Unavail]),
    format('~n=== ITEM 2 (maxent completion gate) field-level on 92 ===~n', []),
    format('ITEM2 constraints carrying a maxent_voided alert (normal completed run) = ~w of 92~n', [NVoid]),
    format('ITEM2 void members: ~w~n', [VoidC]),

    % --- PART-A POSITIVE CONTROL: constructed headline-flip is FLAGGED by the comparison ---
    % pick a baseline-green constraint, force an indexed void (retract run_info), recompute.
    format('~n=== PART-A POSITIVE CONTROL (headline-flip detector) ===~n', []),
    ( member(Cg-green-no, JoinRows)
    -> retractall(maxent_classifier:maxent_indexed_run_info(_,_,_)),
       join_v(Cg, JV2, VP2),
       format('PC: baseline join(~w)=green ; after forced indexed void -> join=~w (void_alert=~w)~n', [Cg, JV2, VP2]),
       ( JV2 \== green
       -> format('PC VERDICT: headline FLIP detected by comparison (green -> ~w). Detector is LIVE.~n', [JV2])
       ;  format('PC VERDICT: NO FLIP -> detector did not register the forced void. HALT-WORTHY.~n', []) )
    ;  format('PC: no baseline-green constraint found; trying a non-green baseline cap-removal control~n', []),
       % fallback: show void gate adds an alert where there was none
       ( JoinRows = [Cany-_-_|_]
       -> retractall(maxent_classifier:maxent_indexed_run_info(_,_,_)),
          join_v(Cany, JVx, VPx),
          format('PC(fallback): ~w after forced void -> join=~w void_alert=~w~n', [Cany, JVx, VPx])
       ;  true )
    ),
    halt.

:- initialization(main).
