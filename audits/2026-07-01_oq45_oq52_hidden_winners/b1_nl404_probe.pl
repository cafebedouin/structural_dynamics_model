/* B1 — Re-derive the OQ-45 "404" NL population on archives/datasets/original_v6.
 *
 * The natural_law signature is UNSATISFIABLE on HEAD: has_viable_alternatives/2's
 * range is {true, unknown} (commit 8b5a34b8, OQ-113 dead-by-range), and the gate
 * requires HasAlternatives == false. This probe restores the PRE-8b5a34b8 fallback
 * (has_viable_alternatives(_, false)) via the sanctioned abolish+assertz swap
 * (swipl_load_path_and_probe_gotchas.md §3), then sweeps the cascade with Sig
 * UNBOUND (bound-Sig bypasses the cascade — KNOWN_STATE 2026-05-31 gotcha:
 * bound=432 vs unbound=404).
 *
 * Aggregate positive control: count must equal 404 (KNOWN_STATE.md:6568;
 * audits/2026-06-10_signature_liveness_crosscorpus/MATRIX.md). Mismatch = halt.
 * Member-level control lives in B2 (0/404 authored beneficiaries).
 *
 * Run:  cd prolog && swipl -g "[stack], consult('<this file>'), b1_run, halt" -t "halt(1)"
 */

b1_run :-
    % --- corpus overlay (asserta-equivalent: retract default first) ---------
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/original_v6')),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorp),
    format(user_error, '[b1] corpus_constraint=~w (glob expect 3380)~n', [NCorp]),
    ( NCorp =:= 3380 -> true ; throw(b1_load_incomplete(NCorp)) ),

    % --- PRE dispatch control: HEAD fallback returns `unknown` --------------
    ( corpus_loader:corpus_constraint(C0),
      \+ ( narrative_ontology:affects_constraint(I0, C0),
           narrative_ontology:intent_viable_alternative(I0, _, _) )
    -> true ; throw(b1_no_control_constraint) ),
    signature_detection:has_viable_alternatives(C0, PreVal),
    format(user_error, '[b1] PRE control (~w): has_viable_alternatives=~w (expect unknown)~n',
           [C0, PreVal]),
    ( PreVal == unknown -> true ; throw(b1_pre_control_failed(PreVal)) ),

    % --- table population fact (the OQ-43 empty channel, on this corpus) ----
    aggregate_all(count, narrative_ontology:intent_viable_alternative(_, _, _), NIVA),
    format(user_error, '[b1] intent_viable_alternative facts corpus-wide=~w~n', [NIVA]),

    % --- SWAP to pre-8b5a34b8 semantics (§3 abolish+assertz) -----------------
    abolish(signature_detection:has_viable_alternatives/2),
    assertz((signature_detection:has_viable_alternatives(C, true) :-
                narrative_ontology:affects_constraint(I, C),
                narrative_ontology:intent_viable_alternative(I, _, _), !)),
    assertz(signature_detection:has_viable_alternatives(_, false)),

    % --- MID dispatch control: swap visible at the consumer path ------------
    signature_detection:has_viable_alternatives(C0, MidVal),
    format(user_error, '[b1] MID control (~w): has_viable_alternatives=~w (expect false)~n',
           [C0, MidVal]),
    ( MidVal == false -> true ; throw(b1_mid_control_failed(MidVal)) ),

    % --- Unbound-Sig cascade sweep, post-filter natural_law -----------------
    findall(C-Sig,
            ( corpus_loader:corpus_constraint(C),
              once(signature_detection:constraint_signature(C, Sig)) ),
            Pairs),
    findall(C, member(C-natural_law, Pairs), NLs),
    length(NLs, NNL),
    length(Pairs, NAll),
    format(user_error, '[b1] swept=~w  natural_law=~w (aggregate control expects 404)~n',
           [NAll, NNL]),

    % --- persist the member list (absolute path; writes are cwd-relative) ---
    open('/home/scott/bin/structural_dynamics_model/audits/2026-07-01_oq45_oq52_hidden_winners/b1_nl404_members.txt',
         write, S),
    forall(member(C, NLs), format(S, '~w~n', [C])),
    close(S),
    ( NNL =:= 404
    -> format(user_error, '[b1] AGGREGATE CONTROL PASS (404)~n', [])
    ;  format(user_error, '[b1] AGGREGATE CONTROL FAIL (~w != 404) — halt and diagnose~n', [NNL]),
       throw(b1_aggregate_control_failed(NNL))
    ).
