% ============================================================================
% PROBE HARNESS — safe in-session fact overlays for counterfactual probes
% ============================================================================
% Encapsulates the overlay discipline documented in
% docs/technical/swipl_load_path_and_probe_gotchas.md (§2 choicepoint-deferred
% cleanup, §3 dispatch controls, §4 per-item restore verification, §7 stale
% memo caches) as one tested utility, so each probe no longer re-implements it
% from the reading.
%
% Usage — a FACT table, which is the only thing this harness can overlay:
%   ?- probe_harness:with_retracted(
%          [narrative_ontology:constraint_metric(my_constraint, extractiveness, _)],
%          my_probe_goal).
%
% COUNTEREXAMPLE — do NOT copy this shape (OQ-302/OQ-326, 2026-08-19). The example that
% stood here until 2026-08-19 was:
%
%       [constraint_indexing:constraint_classification(_, mountain,
%            context(agent_power(analytical), _, exit_options(analytical), _))]
%
% constraint_classification/3 is RULE-BEARING — constraint_instances.pl defines six clauses
% with bodies — so with the first argument UNBOUND this template matches a rule clause,
% which snapshot/2 does not collect and apply_overlay/2 does not retract. The overlay is
% then PARTIAL and warn_if_rule_clauses/1 says so with a *warning*, not an error. Copying
% this form is how the hazard documented below propagates: the file warned about the shape
% in its scope-limits section and demonstrated it in its usage section.
% Census of every committed probe: audits/2026-08-19_oq302_bound_false_repair/
% overlay_template_census.md — this example was the only occurrence of the unsafe form,
% and no probe copied it.
%
%   with_retracted(+Templates, :Goal)   — retract matching FACTS, run, restore
%   with_asserted(+Facts, :Goal)        — assert facts, run, remove
%   with_overlay(+Templates, +Facts, :Goal) — both
%
% Guarantees:
%   - Snapshot is taken BEFORE any retract (findall-first, §4).
%   - Goal runs under once/1 inside setup_call_cleanup/3, so restore runs
%     even on Goal failure or exception, and is never deferred behind
%     choicepoints (§2).
%   - cache_registry:clear_all_caches/0 runs after the overlay is applied AND
%     after restore (§7) — the goal never reads pre-overlay memos, and
%     post-restore callers never read overlay-era memos.
%   - Restore is VERIFIED against the snapshot (multiset equality); a failed
%     restore throws probe_restore_failed/2 rather than silently corrupting
%     subsequent measurements (§4).
%
% Scope limits (deliberate):
%   - FACT overlays only. Templates match clauses whose body is `true`;
%     RULE clauses matching the template are left untouched and a warning is
%     printed (restoring rule-derived solutions as facts would corrupt the
%     program — e.g. constraint_instances.pl defines constraint_classification
%     CLAUSES with bodies).
%   - Templates and Facts must be module-qualified (M:Fact). Unqualified
%     terms throw immediately — defaulting the module would be a silent
%     wrong-table overlay.
% ============================================================================

:- module(probe_harness, [
    with_retracted/2,
    with_asserted/2,
    with_overlay/3
]).

:- use_module(cache_registry).

:- meta_predicate with_retracted(+, 0).
:- meta_predicate with_asserted(+, 0).
:- meta_predicate with_overlay(+, +, 0).

with_retracted(Templates, Goal) :-
    with_overlay(Templates, [], Goal).

with_asserted(Facts, Goal) :-
    with_overlay([], Facts, Goal).

with_overlay(Templates, Facts, Goal) :-
    must_be(list, Templates),
    must_be(list, Facts),
    maplist(check_qualified, Templates),
    maplist(check_qualified, Facts),
    maplist(warn_if_rule_clauses, Templates),
    snapshot(Templates, Snapshot),
    setup_call_cleanup(
        apply_overlay(Snapshot, Facts),
        once(Goal),
        restore_overlay(Templates, Snapshot, Facts)
    ).

check_qualified(M:T) :-
    atom(M), callable(T), !.
check_qualified(Other) :-
    throw(error(type_error(module_qualified_fact, Other),
                probe_harness_requires_module_qualification)).

%% warn_if_rule_clauses(+Template)
%  A template that also matches RULE clauses is only partially overlaid —
%  say so loudly rather than let the probe read a half-removed predicate.
warn_if_rule_clauses(M:T) :-
    (   catch(clause(M:T, Body), _, fail),
        Body \== true
    ->  print_message(warning,
            probe_harness_template_matches_rules(M:T))
    ;   true
    ).

%% snapshot(+Templates, -Snapshot)
%  Bound copies of every FACT (body == true) matching any template,
%  collected before anything is retracted.
snapshot(Templates, Snapshot) :-
    findall(M:Inst,
            ( member(M:T, Templates),
              copy_term(T, Inst),
              catch(clause(M:Inst, true), _, fail)
            ),
            Snapshot).

apply_overlay(Snapshot, Facts) :-
    % Robust retract: a snapshot entry duplicated by OVERLAPPING templates
    % must not fail setup midway (partial overlay with no cleanup). The
    % inconsistency such overlap creates is caught loudly by verify_restore.
    forall(member(M:F, Snapshot),
           ( retract(M:F) -> true ; true )),
    forall(member(M:F, Facts), assertz(M:F)),
    cache_registry:clear_all_caches.

restore_overlay(Templates, Snapshot, Facts) :-
    forall(member(M:F, Facts),
           ( retract(M:F) -> true ; true )),
    forall(member(M:F, Snapshot), assertz(M:F)),
    cache_registry:clear_all_caches,
    verify_restore(Templates, Snapshot).

%% verify_restore(+Templates, +Snapshot)
%  Post-restore fact multiset must equal the snapshot. A clean-looking but
%  wrong restore silently corrupts every subsequent measurement (§4) —
%  fail loud instead.
verify_restore(Templates, Snapshot) :-
    snapshot(Templates, Post),
    msort(Snapshot, S1),
    msort(Post, S2),
    (   S1 == S2
    ->  true
    ;   throw(error(probe_restore_failed(expected(S1), got(S2)),
                    probe_harness_restore_verification))
    ).
