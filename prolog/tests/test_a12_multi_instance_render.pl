% ============================================================================
% TEST: A12 multi-instance render branch (json_report.pl:write_per_constraint_entry/4)
% ============================================================================
% OQ-21. The "A12" branch fires when a constraint name C carries more than one
% cs_story_uid. It picks ONE instance as canonical and renders that instance's
% UID-keyed fields (cs_drift_terminal, cs_axiom_foreclosed, cs_reference_frame,
% cs_drift_moment/cs_drift_gap, cs_drift_unacknowledged). Selection is by
% standard order of UID atoms (@<-maximal), NOT by cs_created_at.
%
% WHY @< AND NOT RECENCY (operator ruling, 2026-06-25): instances of one name
% are parallel draws, not versions (determinism frontier) — there is no
% canonical-latest. The only live consumer (orbit_operator's committer
% terminal-projection orbit) needs a deterministic, stable canonical, which @<
% supplies. recency-as-canonical is incoherent with the model's own semantics.
%
% HISTORY: the branch shipped with a dead clause —
%   aggregate_all(max(T-U), (member(U,UIDs), cs_created_at(U,T)), max(_-UID))
% evaluated T-U ARITHMETICALLY, threw on atom UIDs (type_error evaluable), was
% swallowed by catch/3, and ALWAYS fell through to the @< path. "Verified by
% manual dual-consult" read the comment's intent; the code never executed it.
% This positive control is what surfaced that (the discipline: a branch is not
% witnessed until it runs). The dead clause is now removed; @< is the sole
% selector and these tests pin it.
%
% TEST STRUCTURE (operator-specified):
%   t1 — @<-max selection, recency-PIN, and BUNDLE COHERENCE. The @<-max UID is
%        given the OLDER timestamp; assert it still wins (recency is dead) and
%        that TWO distinguishable fields (cs_reference_frame + cs_drift_moment)
%        both come from that one winner (no field leaks from the losing
%        instance). If a future change makes recency work, t1 goes RED.
%   t2 — NON-VACUOUS INVERSION. Same @<-max UID atom as t1 but the field values
%        are swapped between instances; the expected output flips accordingly,
%        proving the assertion tracks the selected UID's value, not a constant.
%   t3 — NO-TIMESTAMP ROBUSTNESS. Neither instance has cs_created_at; @< still
%        selects deterministically and does not throw.
%   t4 — COUNT-1 COMPLEMENT. A single-instance name renders cs_instance_count 1
%        via the UIDs=[UID] path — proving count-1 is genuinely reachable and
%        distinct from the multi-instance branch.
%
% SCOPE (what the green asserts): the branch reads its named contract correctly
% — uid-collision-on-name, @<-order selection, instance-field bundle coherence.
% It does NOT certify firing on real pipeline data: the live corpus is
% single-instance-per-name by construction, so the branch fires zero times in
% the pipeline today. That half (OQ-21 b) is OQ-17-gated and not claimed here.
%
% NOTE ON THE DEADNESS PIN'S REACH: t1 catches a switch to WORKING recency
% selection (red). It cannot catch re-introduction of the *throwing* arithmetic
% form, because that form is behaviorally identical to @< (it always falls
% through) — there is no output difference to assert. The in-code comment guards
% that misleading-but-harmless reintroduction; this test guards the wrong-output
% one. Together they defend @< from both sides.
% ============================================================================

:- ensure_loaded('../stack').
:- ensure_loaded('../json_report').
:- use_module('../narrative_ontology').
:- use_module(library(plunit)).

% --- fixture helpers -------------------------------------------------------
% Assert/retract a multi-instance fixture for a synthetic constraint name.
% All target predicates are multifile+dynamic (narrative_ontology.pl), and the
% UIDs are synthetic, so there is no real-corpus mutation and no stale cache
% (cs_reference_frame / cs_drift_state are direct fact reads; the computed
% UID-keyed predicates simply fail->null on synthetic UIDs).

a12_assert_instance(C, UID, TS, RF, Moment) :-
    assertz(narrative_ontology:cs_story_uid(C, UID)),
    ( TS == none -> true ; assertz(narrative_ontology:cs_created_at(UID, TS)) ),
    assertz(narrative_ontology:cs_reference_frame(UID, RF)),
    assertz(narrative_ontology:cs_drift_state(UID, Moment, gap(declining, substantial, false))).

a12_cleanup(C) :-
    retractall(narrative_ontology:cs_story_uid(C, _)),
    forall(member(UID, ['a12_uid_aaa','a12_uid_zzz','a12_uid_solo']),
           ( retractall(narrative_ontology:cs_created_at(UID, _)),
             retractall(narrative_ontology:cs_reference_frame(UID, _)),
             retractall(narrative_ontology:cs_drift_state(UID, _, _)) )).

% Render the full entry to a string (drives the identical pipeline render path).
a12_render(C, Out) :-
    with_output_to(string(Out),
        ( current_output(S),
          write_per_constraint_entry(S, C, false, context([],[],[],[])) )).

% --- tests -----------------------------------------------------------------

:- begin_tests(a12_multi_instance_render).

% t1: @<-max wins over recency; two fields track the one winner (bundle coherence).
% UIDs: a12_uid_zzz is @<-maximal. It is given the OLDER timestamp and the
% values ref_hi / mom_hi. The @<-minimal a12_uid_aaa gets the NEWER timestamp and
% ref_lo / mom_lo. @< must win -> ref_hi + mom_hi rendered, ref_lo/mom_lo absent.
test(t1_at_max_wins_over_recency_bundle_coherent,
     [ setup(( a12_assert_instance(a12_c1, 'a12_uid_aaa', '2099-01-01T00:00:00Z', ref_lo, mom_lo),
               a12_assert_instance(a12_c1, 'a12_uid_zzz', '2000-01-01T00:00:00Z', ref_hi, mom_hi) )),
       cleanup(a12_cleanup(a12_c1)) ]) :-
    a12_render(a12_c1, Out),
    % branch fired
    assertion(sub_string(Out, _, _, _, "\"cs_instance_count\": 2")),
    % winner is the @<-max UID (a12_uid_zzz), despite its OLDER timestamp
    assertion(sub_string(Out, _, _, _, "\"cs_reference_frame\": \"ref_hi\"")),
    % bundle coherence: a SECOND field also comes from the same winner
    assertion(sub_string(Out, _, _, _, "\"cs_drift_moment\": \"mom_hi\"")),
    % no field leaked from the losing instance
    assertion(\+ sub_string(Out, _, _, _, "ref_lo")),
    assertion(\+ sub_string(Out, _, _, _, "mom_lo")).

% t2: inversion — same @<-max UID atom, values swapped between instances.
% Now a12_uid_zzz (still @<-max) carries ref_lo/mom_lo and a12_uid_aaa carries
% ref_hi/mom_hi. Expected output flips to ref_lo/mom_lo: proves the assertion
% tracks the selected UID's VALUE, not a fixed string (non-vacuous).
test(t2_inversion_tracks_selected_uid,
     [ setup(( a12_assert_instance(a12_c2, 'a12_uid_aaa', '2000-01-01T00:00:00Z', ref_hi, mom_hi),
               a12_assert_instance(a12_c2, 'a12_uid_zzz', '2099-01-01T00:00:00Z', ref_lo, mom_lo) )),
       cleanup(a12_cleanup(a12_c2)) ]) :-
    a12_render(a12_c2, Out),
    assertion(sub_string(Out, _, _, _, "\"cs_instance_count\": 2")),
    % @<-max (a12_uid_zzz) now holds ref_lo/mom_lo -> those are selected
    assertion(sub_string(Out, _, _, _, "\"cs_reference_frame\": \"ref_lo\"")),
    assertion(sub_string(Out, _, _, _, "\"cs_drift_moment\": \"mom_lo\"")),
    % the OTHER instance's values must NOT appear
    assertion(\+ sub_string(Out, _, _, _, "ref_hi")),
    assertion(\+ sub_string(Out, _, _, _, "mom_hi")).

% t3: no timestamps anywhere — @< still selects deterministically, no throw.
test(t3_no_timestamp_at_order_robust,
     [ setup(( a12_assert_instance(a12_c3, 'a12_uid_aaa', none, ref_lo, mom_lo),
               a12_assert_instance(a12_c3, 'a12_uid_zzz', none, ref_hi, mom_hi) )),
       cleanup(a12_cleanup(a12_c3)) ]) :-
    a12_render(a12_c3, Out),
    assertion(sub_string(Out, _, _, _, "\"cs_instance_count\": 2")),
    assertion(sub_string(Out, _, _, _, "\"cs_reference_frame\": \"ref_hi\"")),
    assertion(\+ sub_string(Out, _, _, _, "ref_lo")).

% t4: single-instance complement — count 1 via the UIDs=[UID] path.
test(t4_single_instance_count_1,
     [ setup(a12_assert_instance(a12_c4, 'a12_uid_solo', '2026-06-25T00:00:00Z', ref_solo, mom_solo)),
       cleanup(a12_cleanup(a12_c4)) ]) :-
    a12_render(a12_c4, Out),
    assertion(sub_string(Out, _, _, _, "\"cs_instance_count\": 1")),
    assertion(sub_string(Out, _, _, _, "\"cs_reference_frame\": \"ref_solo\"")).

:- end_tests(a12_multi_instance_render).
