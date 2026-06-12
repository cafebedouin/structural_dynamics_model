/* OQ-109 B3 — unanimity-guard candidate adjudication (pinned criterion, operator 2026-06-12).

   OLD guard: only_mountain_classifications/1 — authored table, >=1 cell, all mountain.
   Candidate A: pre-signature computed unanimity — drl_core:metric_based_type_indexed/3
     (classify_from_metrics, NO signature integration) yields mountain at ALL canonical
     contexts. Semantic descendant of "all seats agree mountain."
   Candidate B: natural_law_signature(get_constraint_profile) — metric-profile predicate
     already used inside the signature layer (determine_pure_subtype).

   Criterion: (1) extension preservation over the live corpus (old vs candidate, per story);
   (2) seam positive control — synthetic NL-profile story with ZERO authored cells: OLD
   false (the seam), candidate must be TRUE; (3) reentrancy witness handled at install time
   (layering grep + pipeline termination). If exactly one candidate passes 1+2 → settled;
   both pass → prefer A; both pass but disagree live → escalate with the disagreement set.

   Run from prolog/:
     swipl -g "consult('../audits/2026-06-11_oq109_phase_b/unanimity_adjudication_probe.pl'), run, halt" -t "halt(1)"
*/

:- [stack].
:- corpus_loader:ensure_corpus_loaded.
:- use_module(probe_harness).

old_guard(C) :- signature_detection:only_mountain_classifications(C).

cand_a(C) :-
    constraint_indexing:site_contexts_canonical(C4),
    forall(member(Ctx, C4),
           drl_core:metric_based_type_indexed(C, Ctx, mountain)).

cand_b(C) :-
    signature_detection:get_constraint_profile(C, P),
    signature_detection:natural_law_signature(P).

verdict(G, C, V) :- ( catch(call(G, C), E, (print_message(error, E), fail)) -> V = true ; V = false ).

run :-
    % --- Test 1: extension over the live corpus ---
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, N),
    format("TEST 1 — extension over ~w live constraints~n", [N]),
    findall(C-Old-A-B,
            ( member(C, Cs),
              verdict(old_guard, C, Old),
              verdict(cand_a, C, A),
              verdict(cand_b, C, B) ),
            Rows),
    include([_-true-_-_]>>true, Rows, OldTrue),
    include([_-_-true-_]>>true, Rows, ATrue),
    include([_-_-_-true]>>true, Rows, BTrue),
    length(OldTrue, NOld), length(ATrue, NA), length(BTrue, NB),
    format("  old guard true-set: ~w  ~w~n", [NOld, OldTrue]),
    format("  candidate A true-set: ~w  ~w~n", [NA, ATrue]),
    format("  candidate B true-set: ~w  ~w~n", [NB, BTrue]),
    include([_-Old-A-_]>>(Old \== A), Rows, DisOA),
    include([_-Old-_-B]>>(Old \== B), Rows, DisOB),
    include([_-_-A-B]>>(A \== B), Rows, DisAB),
    format("  old vs A disagreements: ~w~n", [DisOA]),
    format("  old vs B disagreements: ~w~n", [DisOB]),
    format("  A vs B disagreements: ~w~n", [DisAB]),

    % --- Test 2: seam positive control ---
    format("~nTEST 2 — seam positive control (synthetic NL story, zero authored cells)~n"),
    config:param(extractiveness_metric_name, EpsName),
    config:param(suppression_metric_name, SuppName),
    probe_harness:with_asserted(
        [ narrative_ontology:constraint_metric(oq109_seam_nl, EpsName, 0.03),
          narrative_ontology:constraint_metric(oq109_seam_nl, SuppName, 0.04),
          narrative_ontology:constraint_metric(oq109_seam_nl, accessibility_collapse, 0.92),
          narrative_ontology:constraint_metric(oq109_seam_nl, resistance, 0.05),
          narrative_ontology:constraint_metric(oq109_seam_nl, theater_ratio, 0.05),
          narrative_ontology:constraint_claim(oq109_seam_nl, mountain),
          domain_priors:emerges_naturally(oq109_seam_nl) ],
        ( verdict(old_guard, oq109_seam_nl, OldS),
          verdict(cand_a, oq109_seam_nl, AS),
          verdict(cand_b, oq109_seam_nl, BS),
          format("  old=~w (false = the seam)  A=~w  B=~w  (candidate must be true)~n",
                 [OldS, AS, BS]),
          % per-context detail for A
          constraint_indexing:site_contexts_canonical(C4),
          forall(member(Ctx, C4),
                 ( ( drl_core:metric_based_type_indexed(oq109_seam_nl, Ctx, T) -> true ; T = 'FAIL' ),
                   temporal_residual:context_label(Ctx, L),
                   format("    A detail ~w -> ~w~n", [L, T]) )),
          ( signature_detection:get_constraint_profile(oq109_seam_nl, Prof)
          -> format("    B profile: ~w~n", [Prof]) ; format("    B profile: FAIL~n") )
        )),
    format("~ndone~n").
