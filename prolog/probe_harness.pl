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
% then PARTIAL and warn_if_rule_clauses/1 said so with a *warning*, not an error. Copying
% this form is how the hazard documented below propagates: the file warned about the shape
% in its scope-limits section and demonstrated it in its usage section.
% Census of every committed probe: audits/2026-08-19_oq302_bound_false_repair/
% overlay_template_census.md — this example was the only occurrence of the unsafe form,
% and no probe copied it.
%
% ---------------------------------------------------------------------------
% INSTALL WITNESS (OQ-326, 2026-08-21) — the harness now proves it overlaid something
% ---------------------------------------------------------------------------
% Until 2026-08-21 this harness verified RESTORE and nothing verified INSTALL. A template
% that matched nothing retracted nothing, warned at most, and the asserted Facts landed
% AFTER the existing clauses; the "counterfactual" arm then measured the UNMUTATED program
% and both arms came back identical. Byte-identity — the thing a clean-vs-edited pair
% reports as success — was produced by the overlay never having been installed.
%
% Six checks now run BEFORE any mutation, in this ORDER (load-bearing, see below):
%
%   2. resolvable    template is a defined predicate    probe_overlay_unresolvable/2
%   3. partial       template matches a RULE clause     probe_overlay_partial/2
%   1. non-empty     per-template snapshot has facts    probe_overlay_empty/1
%   4. reachable     replacement not shadowed           probe_overlay_shadowed/3
%   4'. decidable    some template covers this fact     probe_overlay_reach_undecidable/1
%   5. mutable       target is dynamic                  probe_overlay_immutable/1
%
% WHY THIS ORDER. Check 2 is unconditionally a defect and reports first. Check 3 is a MORE
% SPECIFIC diagnosis of an empty snapshot than emptiness is, so it must pre-empt check 1 or
% the most informative throw is masked by the least — boltzmann_invariant_mountain/2 has two
% clauses, BOTH rules, so its fact snapshot is zero and a 1-before-3 order would diagnose it
% `empty` when `partial` is the truth. Check 5 is LAST, and its position is a FREE choice,
% not forced: clause/2 does NOT raise on static USER predicates under protect_static_code =
% false (the SWI default). 5-early would spend check 3's only naturally-arising positive
% (boltzmann is the sole known rule-bearing template and it is static) to buy nothing.
%
% ALL CHECKS ARE PRE-INSTALL. setup_call_cleanup/3 registers Cleanup only once Setup has
% SUCCEEDED, so a throw from Setup means Cleanup never runs. A check that threw after
% apply_overlay/2 would leave the program permanently overlaid for the rest of the session —
% the direction that manufactures results. This is not hypothetical: it is live at
% audits/2026-06-11_oq109_phase_b/unanimity_adjudication_probe.pl:66, which asserts into a
% STATIC target and leaks the facts asserted before the failing one (that site is exactly
% what check 5 converts into a pre-mutation refusal).
%
% ---------------------------------------------------------------------------
% ESCAPE VOCABULARY — declare the exception at the call site, never globally
% ---------------------------------------------------------------------------
% Wrap the template or fact IN PLACE, inside the same list:
%
%   expect_empty(Reason, M:T)      suppresses check 1  for that TEMPLATE
%   allow_partial(Reason, M:T)     suppresses check 3  for that TEMPLATE
%   allow_shadowed(Reason, M:F)    suppresses check 4  for that FACT
%   reach_undeclared(Reason, M:F)  suppresses check 4' for that FACT
%
% Reason MUST be retrofit(Date, Text) or authored(Text) — anything else is a type error, so
% the provenance bit cannot be dropped. A retrofit carries its date in-file and is greppable:
%
%   expect_empty(retrofit('2026-08-21', "oq35 null control: with_retracted([], ...)"), M:T)
%
% A retrofitted wrapper textually indistinguishable from one written at commit time
% manufactures a green-looking probe whose greenness was installed after the fact — OQ-326's
% own class, moved up one level from the harness to the audit record. The reason must be
% DERIVABLE FROM THE COMMITTED ARTIFACT (a syntactic declaration, or an in-probe assertion),
% never from reconstructed intent. "It looks like they meant it" is the assumption move this
% check exists to prohibit: no wrapper — mint an OQ instead.
%
% COMPOSITION — read this before concluding a wrapper is broken:
%   - Each wrapper suppresses ITS OWN clause and no other. A deliberately partial overlay
%     needs allow_partial AND expect_empty: suppressing 3 lets 1 fire on the same template,
%     by design — two declarations for two distinct facts.
%   - NO combination of wrappers can clear check 2 or check 5. A static rule-bearing target
%     will consume allow_partial, then expect_empty, and still throw probe_overlay_immutable.
%     That is a tested property, not a surprise.
%   - reach_undeclared is legal ONLY for a fact that NO template covers. Attaching it to a
%     template-covered fact is a TYPE ERROR, not a silent bypass — otherwise it becomes the
%     global escape that would defeat the whole check.
%   - reach_undeclared, NOT allow_shadowed, is the migration for bare with_asserted/2 sites.
%     allow_shadowed means "I checked and accept the shadowing"; those sites never had a
%     check to accept.
%
%   with_retracted(+Templates, :Goal)          — retract matching FACTS, run, restore
%   with_asserted(+Facts, :Goal)               — assert facts, run, remove
%   with_overlay(+Templates, +Facts, :Goal)    — both
%   with_overlay(+Templates, +Facts, -Report, :Goal) — both, plus a pasteable install witness
%
% Report shape:
%   overlay_report(RetractedN, AssertedN, PerTemplate, PerFactReach)
%     PerTemplate   = [t(M:T, NFacts), ...]
%     PerFactReach  = [reach(M:F, checked(Template)) | reach(M:F, declared_gap), ...]
%   PerFactReach lets a read site tell a CHECKED reachability from a DECLARED gap, which is
%   the distinction check 4/4' exists to make. (The OQ-326 plan specified a 3-argument
%   report; a 3-arg term cannot carry the per-fact split its own residue requires, so the
%   term carries four. Additive — nothing in the plan's report contract is dropped.)
%
% Guarantees:
%   - All six install checks run BEFORE the single mutation point; nothing throws past it.
%   - Snapshot is taken BEFORE any retract (findall-first, §4), PER TEMPLATE.
%   - Goal runs under once/1 inside setup_call_cleanup/3, so restore runs even on Goal
%     failure or exception, and is never deferred behind choicepoints (§2).
%   - cache_registry:clear_all_caches/0 runs after the overlay is applied AND after restore
%     (§7).
%   - Restore is VERIFIED against the snapshot (multiset equality); a failed restore throws
%     probe_restore_failed/2 (§4).
%   - rule_clauses/2 uses copy_term and NEVER binds the caller's template. The predicate it
%     replaced (warn_if_rule_clauses/1) called clause/2 on the caller's own term inside an
%     if-then-else condition, so when the warning fired the template came back BOUND TO THE
%     RULE HEAD and snapshot/2 then collected only facts unifying with that head. The
%     documented mechanism was "retracts nothing"; the real one ALSO silently narrowed the
%     retract side.
%
% Scope limits (deliberate):
%   - FACT overlays only. Templates match clauses whose body is `true`; a template matching
%     a RULE clause now THROWS (check 3) rather than warning — print_message(warning, ...)
%     was removed, not kept alongside, because OQ-96 established that channel is filtered and
%     check 3 requires a DECLARED decision.
%   - Templates and Facts must be module-qualified (M:Fact). Unqualified terms throw
%     immediately — defaulting the module would be a silent wrong-table overlay.
%   - STRUCTURAL INSTALL IS NOT SEMANTIC EFFECT. These checks prove the clauses moved and
%     that the replacement is reachable at the declared query shape. They do NOT prove the
%     OBSERVABLE changed. A probe still owes its own semantic assertion inside the overlay.
%   - Check 4 is currently UNREACHABLE given checks 1 and 3 in the ruled order plus snapshot
%     completeness: check 3 has removed rule clauses, check 1 has removed empty snapshots, so
%     what reaches check 4 is a fact-only predicate whose template-matching facts were ALL
%     snapshotted, and nothing template-shaped survives. Read that as "unreachable GIVEN the
%     current order", NEVER as "unnecessary" — reorder the checks and it becomes reachable
%     again. The property is pinned by a test (tests/test_probe_harness.pl: narrow the
%     snapshot artificially and check 4 MUST fire), so a regression in snapshot completeness
%     or check ordering has a named signal instead of silently disabling the guard.
% ============================================================================

:- module(probe_harness, [
    with_retracted/2,
    with_asserted/2,
    with_overlay/3,
    with_overlay/4,
    % --- exported for the install-witness suite, not for probe authors ---
    % These three are the internals the guard-property tests must reach:
    % check 4's unreachability is a consequence of check ORDER plus snapshot
    % completeness, so pinning it means handing shadow_survivors/3 a
    % deliberately narrowed snapshot. Exported as ACCESSORS rather than reached
    % by module-qualified bypass, per the OQ-68 write-ownership ruling
    % (gate row `module bounds`). A probe should call with_overlay/3 or /4.
    rule_clauses/2,
    snapshot_one/2,
    shadow_survivors/3
]).

:- use_module(cache_registry).

:- meta_predicate with_retracted(+, 0).
:- meta_predicate with_asserted(+, 0).
:- meta_predicate with_overlay(+, +, 0).
:- meta_predicate with_overlay(+, +, -, 0).

with_retracted(Templates, Goal) :-
    with_overlay(Templates, [], _, Goal).

with_asserted(Facts, Goal) :-
    with_overlay([], Facts, _, Goal).

with_overlay(Templates, Facts, Goal) :-
    with_overlay(Templates, Facts, _, Goal).

with_overlay(Templates, Facts, Report, Goal) :-
    must_be(list, Templates),
    must_be(list, Facts),
    maplist(unwrap(template), Templates, TSubs, TEsc),
    maplist(unwrap(fact),     Facts,     FSubs, FEsc),
    maplist(check_qualified, TSubs),
    maplist(check_qualified, FSubs),
    preflight(TSubs, TEsc, FSubs, FEsc, Snaps, Reach),
    flatten_snaps(Snaps, Snapshot),
    length(Snapshot, RetractedN),
    length(FSubs, AssertedN),
    findall(t(T, N), (member(t(T, Is), Snaps), length(Is, N)), PerTemplate),
    Report = overlay_report(RetractedN, AssertedN, PerTemplate, Reach),
    setup_call_cleanup(
        apply_overlay(Snapshot, FSubs),
        once(Goal),
        restore_overlay(TSubs, Snapshot, FSubs)
    ).

% ---------------------------------------------------------------------------
% Wrapper handling. EVERY walker over Templates/Facts goes through unwrap/4 —
% apply_overlay/2 and restore_overlay/3 included, or they would try to
% retract(expect_empty(...)).
% ---------------------------------------------------------------------------
%  Escapes NEST: allow_partial(R1, expect_empty(R2, M:T)) applies both to the
%  same subject. Returns the accumulated LIST, because a deliberately partial
%  overlay genuinely needs two declarations for two distinct facts.
unwrap(Side, Term, Subject, Escapes) :-
    unwrap_(Side, Term, Subject, [], Escapes).

unwrap_(Side, Term, Subject, Acc, Escapes) :-
    (   escape_term(Term, Name, Reason, Inner)
    ->  check_reason(Reason, Term),
        check_escape_side(Side, Name, Term),
        E =.. [Name, Reason],
        unwrap_(Side, Inner, Subject, [E|Acc], Escapes)
    ;   Subject = Term, Escapes = Acc
    ).

escape_term(expect_empty(R, S),     expect_empty,     R, S).
escape_term(allow_partial(R, S),    allow_partial,    R, S).
escape_term(allow_shadowed(R, S),   allow_shadowed,   R, S).
escape_term(reach_undeclared(R, S), reach_undeclared, R, S).

check_reason(retrofit(D, T), _) :- atom(D), text_like(T), !.
check_reason(authored(T),    _) :- text_like(T), !.
check_reason(R, Term) :-
    throw(error(type_error(probe_overlay_reason, R),
                probe_harness_reason_must_be_retrofit_or_authored(Term))).

text_like(T) :- ( atom(T) ; string(T) ), !.

check_escape_side(template, N, _) :- memberchk(N, [expect_empty, allow_partial]), !.
check_escape_side(fact,     N, _) :- memberchk(N, [allow_shadowed, reach_undeclared]), !.
check_escape_side(Side, N, Term) :-
    throw(error(type_error(probe_overlay_escape_side(Side), N),
                probe_harness_escape_on_wrong_side(Term))).

%  NOTE memberchk/2 would be WRONG here: with E unbound it binds the first
%  element and never backtracks, so a nested escape pair would report only one
%  of its two members.
has_escape(EscapeLists, Name, Idx) :-
    nth0(Idx, EscapeLists, Es),
    member(E, Es), functor(E, Name, 1), !.

check_qualified(M:T) :-
    atom(M), callable(T), !.
check_qualified(Other) :-
    throw(error(type_error(module_qualified_fact, Other),
                probe_harness_requires_module_qualification)).

% ---------------------------------------------------------------------------
% PREFLIGHT — ruled order 2 -> 3 -> 1 -> 4/4' -> 5, over the UNMUTATED database.
% Check 2 applies to TEMPLATES ONLY: an undefined template silently retracts
% nothing, but an undefined ASSERT target is the ordinary fixture-planting
% idiom and assertz creates it dynamic. Check 5 applies to BOTH sides — a
% static assert target dies inside apply_overlay/2 on the very permission_error
% check 5 exists to prevent — but on the assert side it must guard on DEFINED
% first, since predicate_property/2 fails for an undefined predicate and would
% otherwise read "undefined" as "static".
% ---------------------------------------------------------------------------
preflight(TSubs, TEsc, FSubs, FEsc, Snaps, Reach) :-
    maplist(check_resolvable, TSubs),                     % clause 2, templates only
    check_no_rules(TSubs, TEsc),                          % clause 3 (pre-empts clause 1)
    maplist(snapshot_one, TSubs, Snaps),
    check_non_empty(TSubs, TEsc, Snaps),                  % clause 1
    check_reachable(TSubs, Snaps, FSubs, FEsc, Reach),    % clauses 4 and 4', per FACT
    maplist(check_mutable_template, TSubs),               % clause 5, retract side
    maplist(check_mutable_fact, FSubs).                   % clause 5, assert side

% --- clause 2 -------------------------------------------------------------
check_resolvable(M:T) :-
    (   catch(predicate_property(M:T, defined), _, fail)
    ->  true
    ;   functor(T, N, _),
        (   current_predicate(M:N/A2)
        ->  Cause = arity_mismatch(A2)
        ;   Cause = undefined
        ),
        throw(error(probe_overlay_unresolvable(M:T, Cause),
                    probe_harness_install_check))
    ).

% --- clause 3 -------------------------------------------------------------
check_no_rules(Subs, Escapes) :-
    forall(nth0(I, Subs, M:T),
           (   has_escape(Escapes, allow_partial, I)
           ->  true
           ;   rule_clauses(M:T, Heads),
               (   Heads == []
               ->  true
               ;   Heads = [H|_],
                   throw(error(probe_overlay_partial(M:T, H),
                               probe_harness_install_check))
               )
           )).

%% rule_clauses(+Template, -Heads)
%  copy_term FIRST: never bind the caller's template (the OQ-326 binding leak).
rule_clauses(M:T, Heads) :-
    findall(H,
            ( copy_term(T, C),
              catch(clause(M:C, B), _, fail),
              B \== true,
              copy_term(C, H)
            ),
            Heads).

% --- snapshot, per template ----------------------------------------------
snapshot_one(M:T, t(M:T, Insts)) :-
    findall(M:Inst,
            ( copy_term(T, Inst),
              catch(clause(M:Inst, true), _, fail)
            ),
            Insts).

flatten_snaps(Snaps, Flat) :-
    findall(I, (member(t(_, Is), Snaps), member(I, Is)), Flat).

% --- clause 1 -------------------------------------------------------------
check_non_empty(Subs, Escapes, Snaps) :-
    forall(nth0(I, Subs, M:T),
           (   has_escape(Escapes, expect_empty, I)
           ->  true
           ;   nth0(I, Snaps, t(_, Insts)),
               (   Insts == []
               ->  throw(error(probe_overlay_empty(M:T),
                               probe_harness_install_check))
               ;   true
               )
           )).

% --- clauses 4 and 4', per FACT ------------------------------------------
% 4  : the fact IS covered by a template -> is the replacement reachable by the
%      query the probe will actually run? Tested at TEMPLATE shape, because the
%      template is the declared query shape; testing at fact shape answers a
%      narrower question in the PERMISSIVE direction (a false green on a
%      reachability check).
% 4' : NO template covers the fact -> there is no declared query shape, so
%      reachability is UNDECIDABLE. Fail closed rather than return a verdict
%      with no ground. A bare with_asserted/2 is the case where that is every
%      fact — not a special case in the code, a consequence of Templates = [].
check_reachable(TSubs, Snaps, FSubs, FEsc, Reach) :-
    findall(R,
            ( nth0(I, FSubs, M:F),
              reach_one(M:F, I, TSubs, Snaps, FEsc, R)
            ),
            Reach).

reach_one(M:F, I, TSubs, Snaps, FEsc, reach(M:F, Status)) :-
    (   covering_template(M:F, TSubs, Snaps, M:T, Snap)
    ->  (   has_escape(FEsc, reach_undeclared, I)
        ->  throw(error(type_error(probe_overlay_reach_undeclared_on_covered_fact, M:F),
                        probe_harness_reach_undeclared_is_per_uncovered_fact_only))
        ;   has_escape(FEsc, allow_shadowed, I)
        ->  Status = checked(T)
        ;   shadow_survivors(M:T, Snap, Surv),
            (   Surv == []
            ->  Status = checked(T)
            ;   Surv = [S|_],
                throw(error(probe_overlay_shadowed(M:F, M:T, S),
                            probe_harness_install_check))
            )
        )
    ;   has_escape(FEsc, reach_undeclared, I)
    ->  Status = declared_gap
    ;   throw(error(probe_overlay_reach_undecidable(M:F),
                    probe_harness_install_check))
    ).

covering_template(M:F, TSubs, Snaps, M:T, Snap) :-
    nth0(J, TSubs, M:T),
    \+ \+ F = T,
    nth0(J, Snaps, t(_, Snap)),
    !.

%% shadow_survivors(+Template, +Snapshot, -Survivors)
%  Clauses currently matching the template MINUS the snapshot about to be
%  retracted. assertz appends last, so a survivor at TEMPLATE shape wins the
%  query. Exposed (module-qualified) so the suite can pin the guard property
%  by handing it a deliberately NARROWED snapshot — see the test
%  `clause4_fires_on_narrowed_snapshot`.
shadow_survivors(M:T, Snap, Surv) :-
    findall(M:I,
            ( copy_term(T, I),
              catch(clause(M:I, true), _, fail)
            ),
            Cur),
    foldl(remove_first, Snap, Cur, Surv).

remove_first(X, L0, L) :- ( selectchk(X, L0, L1) -> L = L1 ; L = L0 ).

% --- clause 5 -------------------------------------------------------------
check_mutable_template(M:T) :-
    (   catch(predicate_property(M:T, (dynamic)), _, fail)
    ->  true
    ;   throw(error(probe_overlay_immutable(M:T), probe_harness_install_check))
    ).

% An UNDEFINED assert target is legal: assertz/1 creates it as dynamic. Guard on
% defined FIRST or this check re-creates, through clause 5, the very defect that
% keeping clause 2 off the assert side was meant to avoid.
check_mutable_fact(M:F) :-
    (   \+ catch(predicate_property(M:F, defined), _, fail)
    ->  true
    ;   catch(predicate_property(M:F, (dynamic)), _, fail)
    ->  true
    ;   throw(error(probe_overlay_immutable(M:F), probe_harness_install_check))
    ).

% ---------------------------------------------------------------------------
% THE SINGLE MUTATION POINT. Every verdict is already reached before this runs,
% and nothing after it can raise.
% ---------------------------------------------------------------------------
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
    maplist(snapshot_one, Templates, Snaps),
    flatten_snaps(Snaps, Post),
    msort(Snapshot, S1),
    msort(Post, S2),
    (   S1 == S2
    ->  true
    ;   throw(error(probe_restore_failed(expected(S1), got(S2)),
                    probe_harness_restore_verification))
    ).
