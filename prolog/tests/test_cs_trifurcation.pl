% ============================================================================
% TEST: cs_reading_trifurcation/3 — within-kernel A/B/C router (OQ-55)
% ============================================================================
% Positive controls, one constructed kernel per branch, all inputs within-kernel:
%
%   real_closure  (forecloses edge)         → type_b_structure
%   licensed_plurality (coexists_with edge)  → type_c_ambiguity
%   untyped + unacknowledged drift           → type_a_drift   (the ONLY computed verdict)
%   untyped + no drift                       → unknown        (Pattern 5 fail-closed)
%   singleton (<2 readings)                  → no verdict (predicate FAILS)
%
% TYPE A TWO-TWIN (the operator constraint): tk_drift and tk_nodrift both hold
% obstruction status FIXED at `untyped` (2 readings, no authored edge) and vary
% ONLY the drift signal. This proves the DRIFT signal is the discriminator — that
% Type A fires on untyped+drift and does NOT fire on untyped+no-drift — so "fires
% on Type A" is not byte-identical to "fires on any untyped kernel."
%
% INPUT-BOUNDARY CONTROL: cross_kernel_facts_do_not_leak asserts a foreclosure
% edge + drift in an UNRELATED kernel and shows tk_drift's verdict is unchanged —
% the within-kernel boundary made into a test, not just a trace.
%
% Facts are synthetic (asserted here), not corpus-loaded: the router reads only
% per-kernel/per-member-reading facts, so the controls are hermetic.
% ============================================================================

:- use_module(cs_trifurcation).
:- use_module(narrative_ontology).
:- use_module(library(plunit)).

:- multifile narrative_ontology:cs_kernel_id/2.
:- multifile narrative_ontology:cs_story_uid/2.
:- multifile narrative_ontology:cs_reading_relation/3.
:- multifile narrative_ontology:cs_drift_state/3.
:- multifile narrative_ontology:cs_axiom/3.
:- multifile narrative_ontology:cs_axiom_grounding/3.

% ---------------------------------------------------------------------------
% BRANCH FIXTURES (constructed; within-kernel only)
% ---------------------------------------------------------------------------

% --- real_closure → Type B (edge only; no computed axiom foreclosure) ---
narrative_ontology:cs_kernel_id(rc_a, tk_closure).
narrative_ontology:cs_kernel_id(rc_b, tk_closure).
narrative_ontology:cs_story_uid(rc_a, 'uid-rc-a').
narrative_ontology:cs_story_uid(rc_b, 'uid-rc-b').
narrative_ontology:cs_reading_relation('uid-rc-a', rc_b, forecloses).

% --- real_closure → Type B (CONFIRMED by a member's computed foreclosure) ---
narrative_ontology:cs_kernel_id(rcc_a, tk_closure_confirmed).
narrative_ontology:cs_kernel_id(rcc_b, tk_closure_confirmed).
narrative_ontology:cs_story_uid(rcc_a, 'uid-rcc-a').
narrative_ontology:cs_story_uid(rcc_b, 'uid-rcc-b').
narrative_ontology:cs_reading_relation('uid-rcc-a', rcc_b, forecloses).
narrative_ontology:cs_axiom('uid-rcc-a', premise, ax_rcc).
narrative_ontology:cs_axiom_grounding('uid-rcc-a', ax_rcc, empirically_contingent).
narrative_ontology:cs_drift_state('uid-rcc-a', m_rcc, gap(axiom_overriding, substantial, false)).

% --- licensed_plurality → Type C ---
narrative_ontology:cs_kernel_id(rp_a, tk_plurality).
narrative_ontology:cs_kernel_id(rp_b, tk_plurality).
narrative_ontology:cs_story_uid(rp_a, 'uid-rp-a').
narrative_ontology:cs_story_uid(rp_b, 'uid-rp-b').
narrative_ontology:cs_reading_relation('uid-rp-a', rp_b, coexists_with).

% --- untyped + unacknowledged drift → Type A ---
narrative_ontology:cs_kernel_id(rd_a, tk_drift).
narrative_ontology:cs_kernel_id(rd_b, tk_drift).
narrative_ontology:cs_story_uid(rd_a, 'uid-rd-a').
narrative_ontology:cs_story_uid(rd_b, 'uid-rd-b').
narrative_ontology:cs_drift_state('uid-rd-a', m_rd, gap(authority_erosion, substantial, false)).

% --- untyped + NO drift → unknown (twin of tk_drift; obstruction held at untyped) ---
narrative_ontology:cs_kernel_id(rn_a, tk_nodrift).
narrative_ontology:cs_kernel_id(rn_b, tk_nodrift).
narrative_ontology:cs_story_uid(rn_a, 'uid-rn-a').
narrative_ontology:cs_story_uid(rn_b, 'uid-rn-b').
% A drift that must NOT count: acknowledged (true) AND stable+minor — the twin
% carries a drift_state, so "has a drift_state row" is not the discriminator.
narrative_ontology:cs_drift_state('uid-rn-a', m_rn, gap(stable, minor, true)).

% --- singleton → no verdict ---
narrative_ontology:cs_kernel_id(rs_a, tk_singleton).
narrative_ontology:cs_story_uid(rs_a, 'uid-rs-a').

% --- unrelated foreclosure kernel, coexisting in the SAME fact base, used by the
%     input-boundary control: its forecloses edge must not leak into tk_drift. ---
narrative_ontology:cs_kernel_id(xk_a, tk_other).
narrative_ontology:cs_kernel_id(xk_b, tk_other).
narrative_ontology:cs_story_uid(xk_a, 'uid-xk-a').
narrative_ontology:cs_story_uid(xk_b, 'uid-xk-b').
narrative_ontology:cs_reading_relation('uid-xk-a', xk_b, forecloses).

% ---------------------------------------------------------------------------
% TESTS
% ---------------------------------------------------------------------------

:- begin_tests(cs_trifurcation).

% --- Type B: real_closure, edge only ---
test(real_closure_type_b) :-
    cs_trifurcation:cs_reading_trifurcation(tk_closure, Type, Prov),
    Type == type_b_structure,
    Prov = provenance(scope(within_kernel), obstruction(real_closure),
                      diagnostic(axiom_foreclosed(edge_only))).

% --- Type B: real_closure, confirmed by computed member foreclosure ---
test(real_closure_type_b_confirmed) :-
    cs_trifurcation:cs_reading_trifurcation(tk_closure_confirmed, Type, Prov),
    Type == type_b_structure,
    Prov = provenance(scope(within_kernel), obstruction(real_closure),
                      diagnostic(axiom_foreclosed(confirmed))).

% --- Type C: licensed_plurality ---
test(licensed_plurality_type_c) :-
    cs_trifurcation:cs_reading_trifurcation(tk_plurality, Type, Prov),
    Type == type_c_ambiguity,
    Prov = provenance(scope(within_kernel), obstruction(licensed_plurality),
                      diagnostic(coexist_edge)).

% --- Type A: untyped + unacknowledged drift (the sole COMPUTED verdict) ---
test(untyped_drift_type_a) :-
    cs_trifurcation:cs_reading_trifurcation(tk_drift, Type, Prov),
    Type == type_a_drift,
    Prov = provenance(scope(within_kernel), obstruction(untyped),
                      diagnostic(drift_unacknowledged)).

% --- unknown: untyped + no drift (Pattern 5 fail-closed; NOT a default type) ---
test(untyped_nodrift_unknown) :-
    cs_trifurcation:cs_reading_trifurcation(tk_nodrift, Type, Prov),
    Type == unknown,
    Prov = provenance(scope(within_kernel), obstruction(untyped),
                      diagnostic(no_drift_signal_fail_closed)).

% --- negative control: singleton yields NO verdict (predicate fails) ---
test(singleton_no_verdict, [fail]) :-
    cs_trifurcation:cs_reading_trifurcation(tk_singleton, _, _).

% --- TYPE A TWO-TWIN: drift is the discriminator, obstruction held at untyped ---
% Both kernels are `untyped` (verified), and only the drift signal differs, yet
% they route to different types. If obstruction status were silently doing the
% work, both would route identically.
test(type_a_discriminator_is_drift) :-
    cs_kernel_registry:cs_kernel_obstruction_status(tk_drift, untyped),
    cs_kernel_registry:cs_kernel_obstruction_status(tk_nodrift, untyped),
    cs_trifurcation:cs_reading_trifurcation(tk_drift, type_a_drift, _),
    cs_trifurcation:cs_reading_trifurcation(tk_nodrift, unknown, _).

% --- INPUT-BOUNDARY CONTROL: cross-kernel facts do not leak into a verdict ---
% An unrelated kernel (tk_other) carries a forecloses edge in the SAME fact base.
% tk_drift still routes Type A and tk_other routes Type B simultaneously — the
% router keys every input per-kernel; a forecloses edge in one kernel does not
% migrate the verdict of another.
test(cross_kernel_facts_do_not_leak) :-
    cs_trifurcation:cs_reading_trifurcation(tk_drift, type_a_drift, _),
    cs_trifurcation:cs_reading_trifurcation(tk_other, type_b_structure, _).

:- end_tests(cs_trifurcation).
