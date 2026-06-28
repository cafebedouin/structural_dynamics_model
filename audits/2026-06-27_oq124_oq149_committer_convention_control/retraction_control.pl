% Phase 2 — Field A bidirectional retraction control (pre-registered falsifier).
% Loads ONE twin (corpus_path overlaid via asserta — the silent-fork gotcha) and, for a
% pre-named control cid, reports appears_as_rope/false_ci_rope BEFORE and AFTER retracting
% the authored narrative_ontology:constraint_claim(C, rope) via probe_harness:with_retracted/2
% (snapshot-first, verified restore, cache_registry:clear_all_caches/0).
%
% Predicted (PRE_REGISTRATION):
%   testsets_haiku / human_dignity_ai_governance__techno_optimist_reading  (SOURCE1_ONLY, eps 0.78):
%       appears_as_rope MUST DROP on retraction (no source-2 rescue).
%   testsets_flash / acceptable_risk_for_energy__expected_value_dominant   (BOTH, eps 0.30):
%       appears_as_rope MUST SURVIVE on retraction (low_extraction_profile rescues).
%
% Run (from prolog/):
%   swipl -g "control(testsets_haiku, human_dignity_ai_governance__techno_optimist_reading), halt" \
%         -t "halt(1)" <thisfile>
:- initialization(true).
:- use_module(library(lists)).
:- use_module(probe_harness).

load_twin(Dir) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Dir)),
    consult(stack),
    corpus_loader:load_all_testsets.

rope_states(C, States) :-
    findall(T, signature_detection:appears_as_rope(C, T), Ts0),
    sort(Ts0, States).

fcr_holds(C, Yes) :-
    ( signature_detection:false_ci_rope(C, _) -> Yes = yes ; Yes = no ).

claim_rope_present(C, P) :-
    ( narrative_ontology:constraint_claim(C, rope) -> P = yes ; P = no ).

control(Dir, C) :-
    load_twin(Dir),
    format("~n=== CONTROL ~w / ~w ===~n", [Dir, C]),
    % non-vacuity: confirm the story loaded and the claim is authored
    ( corpus_loader:corpus_constraint(C) -> true
    ; ( format("FATAL: ~w not in corpus~n", [C]), fail ) ),
    claim_rope_present(C, Pre_claim),
    rope_states(C, Pre_states), fcr_holds(C, Pre_fcr),
    format("BEFORE: claim_rope=~w  appears_as_rope=~w  false_ci_rope=~w~n",
           [Pre_claim, Pre_states, Pre_fcr]),
    % retract the authored constraint_claim(C, rope) and re-measure
    probe_harness:with_retracted(
        [narrative_ontology:constraint_claim(C, rope)],
        ( rope_states(C, Post_states), fcr_holds(C, Post_fcr),
          claim_rope_present(C, Post_claim),
          format("AFTER : claim_rope=~w  appears_as_rope=~w  false_ci_rope=~w~n",
                 [Post_claim, Post_states, Post_fcr]),
          ( Post_states == [] -> Verdict = dropped ; Verdict = survived ),
          format("RESULT: appears_as_rope ~w on retraction~n", [Verdict]) )),
    % confirm restore: claim back, states back
    claim_rope_present(C, Restored_claim),
    rope_states(C, Restored_states),
    format("RESTORE: claim_rope=~w  appears_as_rope=~w~n", [Restored_claim, Restored_states]).
