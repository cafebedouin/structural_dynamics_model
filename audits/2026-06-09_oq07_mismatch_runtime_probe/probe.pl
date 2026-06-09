/* OQ-07 runtime probe — does cs_drift_mismatch/2 fire for the hand-traced candidate
   UID 72c8aa61-6909-40a1-83ef-a460510f3b82 (conceptual_emergence_reading)?

   Corpus note (2026-06-09): the live corpus is the post-2026-06-05 rebuild and does NOT
   contain this testset. The UID exists in exactly one archive copy
   (grep -rln over prolog/ --include=*.pl): archives/datasets/kernel_test/ — other archive
   copies of conceptual_emergence_reading.pl carry different per-generation story_uids.
   So this probe overlays corpus_path to that archive (standard retract+asserta idiom,
   cf. python/fcr_ablation.py). Read-only: nothing here writes output files.

   Positive control: corpus-wide findall over cs_drift_mismatch/2 — if the detector fires
   on zero UIDs anywhere, the probe cannot distinguish silent-detector from genuine
   negative and says so instead of reporting a verdict.

   Run: swipl -g "consult('<this file>'), probe, halt" -t "halt(1)"
*/
:- ['/home/scott/bin/structural_dynamics_model/prolog/stack'].
:- use_module('/home/scott/bin/structural_dynamics_model/prolog/cs_drift_mismatch').
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'archives/datasets/kernel_test')).
:- corpus_loader:load_all_testsets.

uid('72c8aa61-6909-40a1-83ef-a460510f3b82').

probe :-
    uid(UID),
    % Load witness: the UID must actually be present after the overlay load.
    (   narrative_ontology:cs_story_uid(C, UID), \+ is_list(C)
    ->  format("LOADED: reading ~w carries UID ~w~n", [C, UID])
    ;   format("LOAD-FAIL: UID absent after load — overlay or archive wrong~n"),
        halt(2)
    ),
    % Positive control: the detector must be live on this corpus.
    findall(U-S, cs_drift_mismatch:cs_drift_mismatch(U, S), All),
    length(All, N),
    format("POSITIVE-CONTROL: cs_drift_mismatch fires on ~w UID(s) corpus-wide~n", [N]),
    (   N =:= 0
    ->  format("  WARNING: zero firings anywhere — cannot distinguish silent-detector from genuine-negative~n")
    ;   true
    ),
    % The OQ-07 question.
    (   member(UID-Src, All)
    ->  format("OQ07-VERDICT: FIRES — source=~w~n", [Src])
    ;   format("OQ07-VERDICT: SILENT for ~w~n", [UID]),
        % Decompose which conjunct blocked it.
        cs_drift_mismatch:cs_any_foreclosed(UID, Traj, AxFc),
        format("  foreclosure-half: trajectory=~w axiom=~w~n", [Traj, AxFc]),
        (   cs_drift_mismatch:cs_is_metric_stable(C)
        ->  format("  metric-stable-half: HOLDS~n")
        ;   format("  metric-stable-half: FAILS~n")
        )
    ).
