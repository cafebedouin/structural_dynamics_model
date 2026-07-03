% OQ-138 FNL conversion generality sweep (Verification step 2) — copied from
% fcr9_generality_sweep.pl (audits/2026-06-21_oq138_fsm_route_conversion/), swapped
% false_ci_rope→false_natural_law, fcr_routed→fnl_routed. THREE must-be-0 invariants:
%   routed-still-tangled_rope = 0  (routed seats actually routed away from the target)
%   routed∩abstain = 0             (no routed seat has dr_type=unknown)
%   routed∩piton = 0               (POSITIVE CONTROL for "FNL has no piton case" — FCR
%                                   discovered its piton-3 via exactly this invariant;
%                                   if FNL ever produces a piton seat this fires)
% Run: CORPUS_DIR=<leg> swipl -q -g true -t halt ../audits/2026-07-02_oq138_fnl_evidence/fnl_generality_sweep.pl
:- initialization(main).
:- [stack].
g(G,R):-(catch(G,_,fail)->R=yes;R=no).
main :-
  getenv('CORPUS_DIR',Dir),
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,Dir)),
  corpus_loader:load_all_testsets,
  constraint_indexing:default_context(Ctx),
  findall(C,(corpus_loader:corpus_constraint(C), signature_detection:constraint_signature(C,false_natural_law)),Fs0),
  sort(Fs0,Fs),
  findall(C,(member(C,Fs), signature_detection:fnl_routed(C)),Routed),
  % routed seats must NOT be tangled_rope (they routed to their metric type)
  findall(C,(member(C,Routed), drl_core:dr_type(C,Ctx,tangled_rope)),RoutedStillTR),
  % no routed seat may be the honest-abstain inert case
  findall(C,(member(C,Routed), drl_core:dr_type(C,Ctx,unknown)),RoutedAbstain),
  % positive control: FNL has no piton refinement — a routed piton seat means that claim broke
  findall(C,(member(C,Routed), g(narrative_ontology:piton_candidate(C),yes)),RoutedPiton),
  length(Fs,NF),length(Routed,NR),length(RoutedStillTR,NRT),length(RoutedAbstain,NRA),length(RoutedPiton,NRP),
  format("~w: ~w FNL-winners | ~w fnl_routed ~w | routed-still-tangled_rope=~w (must 0) | routed-abstain=~w (must 0) | routed-piton=~w (must 0)~n",
         [Dir,NF,NR,Routed,NRT,NRA,NRP]),
  halt.
main :- write('SWEEP FAIL'),halt(1).
