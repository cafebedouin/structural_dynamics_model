% OQ-138 FNL conversion — PER-CONTEXT consumer-state probe (Verification step 4).
% Extends the fcr9_consumer_state.pl template per the operator's Commit-2 condition:
% fnl_routed/1 is default-context-keyed while the :925 overwrite was orbit-wide, so the
% probe must show the consumer state AT EVERY CONTEXT — the named case is
% organization_floor_c0 @ institutional (routes tangled_rope->scaffold there while its
% default-context type is unknown). Per seat: default metric->dr_type, vic, fnl_routed,
% seat_overrides, grade/sev, probe_signature signal (probe_signature is a DEFAULT-context
% evaluator — diagnostic_summary.pl:91-97; per-context rows witness dr_type + the
% PER-CONTEXT maxent boost state instead, since apply_signature_override fires inside
% maxent_classify_one at all 4 Wasserstein contexts).
% boost_state discriminates by dist==raw identity; BOOST-CONTROL is the positive control
% (an unconverted override signature must read `boosted`, else the identity check never
% could have flagged a boost).
% Run:  CORPUS_DIR=<leg> [FNL_ABLATION=1] swipl -q -g true -t halt \
%         ../audits/2026-07-02_oq138_fnl_evidence/fnl_consumer_state.pl
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).
nvic(C,N):-findall(V,narrative_ontology:constraint_victim(C,V),L),sort(L,Ls),length(Ls,N).
g(G,R):-(catch(G,_,fail)->R=yes;R=no).
boost_state(C,Ctx,S) :-
    (   catch(maxent_classifier:maxent_distribution_raw(C,Ctx,R),_,fail)
    ->  (   catch(maxent_classifier:maxent_distribution(C,Ctx,D),_,fail)
        ->  ( D == R -> S = no_boost ; S = boosted )
        ;   S = no_dist )
    ;   S = no_raw ).
ctx_label(Ctx,L) :- ( compound(Ctx) -> arg(1,Ctx,L) ; L = Ctx ).
main :-
  getenv('CORPUS_DIR',Dir),
  (   getenv('FNL_ABLATION','1')
  ->  retractall(config:param(false_natural_law_override_enabled,_)),
      asserta(config:param(false_natural_law_override_enabled,1)),
      format("MODE: ABLATION (lever=1, legacy overwrite)~n")
  ;   format("MODE: route (lever=0, committed default)~n")
  ),
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,Dir)),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(DCtx),
  measurement_layer:wasserstein_contexts(WCtxs),
  maxent_classifier:maxent_run(DCtx,_),
  maxent_classifier:maxent_multi_run(WCtxs,_),
  % positive control: an UNCONVERTED override signature must read `boosted`
  (   ( corpus_loader:corpus_constraint(PC),
        signature_detection:constraint_signature(PC,PSig),   % unbound winner (§1 gotcha)
        memberchk(PSig,[coupling_invariant_rope,natural_law,coordination_scaffold,
                        constructed_low_extraction]) )
  ->  boost_state(PC,DCtx,PBS),
      format("BOOST-CONTROL: ~w sig=~w default-ctx boost=~w (must be boosted)~n",[PC,PSig,PBS])
  ;   format("BOOST-CONTROL: no unconverted override signature on this leg~n")
  ),
  findall(C,(corpus_loader:corpus_constraint(C),
             signature_detection:constraint_signature(C,false_natural_law)),Fs0),
  sort(Fs0,Fs), length(Fs,NF),
  format("~w: ~w FNL cascade-winners~n",[Dir,NF]),
  forall(member(C,Fs),(
    ( drl_core:metric_based_type_indexed(C,DCtx,MT)->true;MT=err ),
    ( drl_core:dr_type(C,DCtx,DT)->true;DT=err ),
    nvic(C,V),
    g(signature_detection:fnl_routed(C),Routed),
    g(abductive_helpers:seat_overrides(C,false_natural_law),SO),
    ( signature_detection:signature_grade(C,G)->true;G=none ),
    ( signature_detection:signature_severity(C,S)->true;S=none ),
    ( catch(diagnostic_summary:probe_signature(C,DT,PS),_,PS=err)->true;PS=err ),
    ( MT==DT -> Tag='' ; Tag='*CHANGED*' ),
    format("~w | default ~w->~w ~w | vic=~w | fnl_routed=~w | seat_overrides=~w | grade/sev=~w/~w | probe_signature=~w~n",
           [C,MT,DT,Tag,V,Routed,SO,G,S,PS]),
    forall(member(Ctx,WCtxs),(
      ( drl_core:dr_type(C,Ctx,CDT)->true;CDT=err ),
      boost_state(C,Ctx,BS),
      ctx_label(Ctx,CL),
      format("    @~w: dr_type=~w | maxent_boost=~w~n",[CL,CDT,BS])
    ))
  )),
  halt.
main :- write('PROBE FAIL'),halt(1).
