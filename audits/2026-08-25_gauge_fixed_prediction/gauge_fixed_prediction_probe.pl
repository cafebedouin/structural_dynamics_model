% ============================================================================
% gauge_fixed_prediction_probe.pl
%
% Runs the never-executed prediction pre-registered in the source comment at
% constraint_indexing.pl:798-799:
%
%   "the set of constraints where classify_from_restricted differs from
%    dr_type/3 should match the set with gauge_fixed = true."
%
% Emits ONE ROW PER (constraint, context) PAIR UNCONDITIONALLY. No filtering,
% no aggregation inside the probe -- the TSV is the raw artifact and every
% count in the writeup is derived from it, never from a loop counter.
%
% Run from prolog/:
%   swipl -g "['../audits/2026-08-25_gauge_fixed_prediction/gauge_fixed_prediction_probe.pl'], run, halt" -t "halt(1)"
%
% Design notes (each one is a discipline requirement, not decoration):
%
%  * Every one of the three observables is called with the OUTPUT ARGUMENT
%    UNBOUND and wrapped in once/1. A bound selecting argument would bypass
%    clause-order cuts (Build Discipline Pattern 7) -- gauge_fixed/3 in
%    particular is cut-ordered, so gauge_fixed(C,Ctx,true) called with `true`
%    bound answers a different question than gauge_fixed(C,Ctx,GF).
%
%  * Failure binds an explicit `failed` token. `unknown` is kept DISTINCT from
%    `failed`: dr_type/3 has a catch-all clause binding `unknown`, so the two
%    are genuinely different events and collapsing them would be the
%    absence-satisfies-the-gate defect.
%
%  * The context set is obtained by calling constraint_indexing:site_contexts/1
%    -- literally the same predicate gauge_fixed/3 consults at
%    dirac_classification.pl:215. This is identity of the domain, not equality
%    of two independently-built lists.
%
%  * The FIRING-DOMAIN check is computed, not read off the source: the probe
%    asserts at runtime that every gauge_fixed=true row carries
%    dr_type in {mountain, snare}, and reports the observed local-type
%    distribution of the true rows.
% ============================================================================

:- use_module(library(lists)).

run :-
    ensure_loaded('../prolog/stack'),
    corpus_loader:load_all_testsets,

    % --- the measurement site, taken from the same predicate gauge_fixed uses
    constraint_indexing:site_contexts(Contexts),
    length(Contexts, NCtx),

    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    length(Cs, NC),

    ExpectedRows is NC * NCtx,
    format(user_error, '[probe] members=~w contexts=~w expected_rows=~w~n',
           [NC, NCtx, ExpectedRows]),

    open('../audits/2026-08-25_gauge_fixed_prediction/rows.tsv', write, S),
    format(S, 'constraint\tkind\tctx_idx\tagent_power\tdr_type\trestricted_type\tgauge_fixed\tagreement~n', []),
    forall(
        ( member(C, Cs),
          nth1(I, Contexts, Ctx) ),
        emit_row(S, C, I, Ctx)
    ),
    close(S),

    % --- row-count reconciliation, derived from the ARTIFACT, not the loop
    count_lines('../audits/2026-08-25_gauge_fixed_prediction/rows.tsv', NLines),
    NRows is NLines - 1,
    format(user_error, '[probe] rows_written=~w expected=~w delta=~w~n',
           [NRows, ExpectedRows, NRows - ExpectedRows]),
    (   NRows =:= ExpectedRows
    ->  format(user_error, '[probe] ROW COUNT OK~n', [])
    ;   Sign is sign(NRows - ExpectedRows),
        format(user_error, '[probe] ROW COUNT MISMATCH sign=~w~n', [Sign])
    ),

    % --- computed firing-domain check
    firing_domain_check(Cs, Contexts),
    format(user_error, '[probe] done~n', []).

emit_row(S, C, I, Ctx) :-
    Ctx = context(agent_power(P), _, _, _),
    (   corpus_loader:corpus_member_kind(C, K0) -> Kind = K0 ; Kind = kind_failed ),
    obs(drl_core:dr_type(C, Ctx, X1), X1, DrT),
    obs(constraint_indexing:classify_from_restricted(C, Ctx, X2), X2, RestT),
    obs(dirac_classification:gauge_fixed(C, Ctx, X3), X3, GF),
    agreement(DrT, RestT, Agr),
    format(S, '~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n',
           [C, Kind, I, P, DrT, RestT, GF, Agr]).

%% obs(+Goal, -Out, -Value)
%  once/1 + catch/3; failure and exception both bind `failed`, kept distinct
%  from any token the predicate itself can return.
obs(Goal, Out, Value) :-
    (   catch(once(Goal), _E, fail)
    ->  Value = Out
    ;   Value = failed
    ).

%% agreement(+DrType, +RestrictedType, -Atom)
%  Five atoms, no arithmetic possible on any of them.
%
%  The fifth, `stratum_indeterminate`, is a DEVIATION from the plan's four and
%  is forced by the substrate: restricted_classify/7's catch-all clause
%  (constraint_indexing.pl:975) returns `indeterminate`, which is a non-answer
%  from the restricted side, not a type. Scoring `indeterminate` against a real
%  dr_type as "disagree" would let a non-answer satisfy the prediction --
%  exactly the absence-satisfies-the-gate shape. It gets its own stratum and
%  the writeup evaluates the equality BOTH ways.
agreement(failed, _, stratum_failed) :- !.
agreement(_, failed, stratum_failed) :- !.
agreement(unknown, _, stratum_unknown) :- !.
agreement(_, unknown, stratum_unknown) :- !.
agreement(_, indeterminate, stratum_indeterminate) :- !.
agreement(T, T, agree) :- !.
agreement(_, _, disagree).

%% firing_domain_check(+Cs, +Contexts)
%  The one load-bearing check, computed rather than read off the source.
firing_domain_check(Cs, Contexts) :-
    findall(DrT,
        ( member(C, Cs), member(Ctx, Contexts),
          obs(dirac_classification:gauge_fixed(C, Ctx, G), G, true),
          obs(drl_core:dr_type(C, Ctx, X), X, DrT) ),
        Types),
    msort(Types, Sorted),
    clumped(Sorted, Counts),
    length(Types, NTrue),
    format(user_error, '[probe] gauge_fixed=true rows: ~w~n', [NTrue]),
    format(user_error, '[probe] local dr_type distribution over true rows: ~w~n', [Counts]),
    (   forall(member(T, Types), memberchk(T, [mountain, snare]))
    ->  format(user_error, '[probe] FIRING DOMAIN: every true row is mountain|snare -> EXACTLY {mountain,snare}~n', [])
    ;   format(user_error, '[probe] FIRING DOMAIN: SUPERSET or OTHER -- see distribution above~n', [])
    ).

count_lines(File, N) :-
    setup_call_cleanup(
        open(File, read, S),
        count_lines_(S, 0, N),
        close(S)).
count_lines_(S, A, N) :-
    read_line_to_string(S, L),
    (   L == end_of_file
    ->  N = A
    ;   A1 is A + 1, count_lines_(S, A1, N)
    ).
