% ============================================================================
% FINGERPRINT REPORT — Corpus-Wide Logical Fingerprint Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -g "[fingerprint_report], halt."
%
% Bulk-loads all testsets, discovers constraints, runs fingerprint analysis,
% and outputs a markdown report to stdout.
% ============================================================================

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(logical_fingerprint).
:- use_module(data_repair).
:- use_module(domain_priors).

:- use_module(library(lists)).

:- initialization(run_fingerprint_report).

%% run_fingerprint_report
%  Main entry point. Loads corpus, analyzes fingerprints, outputs markdown.
run_fingerprint_report :-
    load_all_testsets,
    format('# Logical Fingerprint Report~n~n'),
    format('*Generated: corpus-wide structural fingerprint analysis*~n~n'),

    % Discover all constraints
    findall(C, logical_fingerprint:known_constraint(C), CRaw),
    sort(CRaw, Constraints),
    length(Constraints, NConstraints),

    % Get all shift patterns
    logical_fingerprint:all_shift_patterns(Patterns),
    length(Patterns, NPatterns),

    % Summary
    format('## Summary~n~n'),
    format('- **Constraints analyzed**: ~w~n', [NConstraints]),
    format('- **Distinct shift patterns**: ~w~n~n', [NPatterns]),

    % Shift pattern families
    format('## Shift Pattern Families~n~n'),
    format('Each family groups constraints with identical perspectival shift — '),
    format('same classification at each power level.~n~n'),
    forall(
        member(Pattern, Patterns),
        report_shift_family(Pattern)
    ),

    % Scope differentiation
    format('## Scope Differentiation~n~n'),
    format('Constraints where changing scope (local vs national vs global) '),
    format('changes the classification type, holding power/time/exit constant.~n~n'),
    findall(C2, (member(C2, Constraints), scope_changes_type(C2)), ScopeSensitive),
    length(ScopeSensitive, NScopeSensitive),
    format('- **Scope-sensitive constraints**: ~w / ~w~n~n', [NScopeSensitive, NConstraints]),
    (   ScopeSensitive \= []
    ->  forall(
            member(SC, ScopeSensitive),
            report_scope_differentiation(SC)
        )
    ;   format('No constraints showed scope-induced type changes.~n~n')
    ),

    % Zone distribution
    format('## Metric Zone Distribution~n~n'),
    report_zone_distribution(Constraints),

    format('---~n'),
    format('*End of fingerprint report*~n').

%% load_all_testsets
%  Bulk-loads all .pl files from testsets/ directory.
load_all_testsets :-
    expand_file_name('testsets/*.pl', Files),
    length(Files, N),
    format(user_error, 'Loading ~w testset files...~n', [N]),
    forall(
        member(F, Files),
        ( catch(consult(F), E,
                format(user_error, 'Warning: failed to load ~w: ~w~n', [F, E]))
        )
    ),
    format(user_error, 'Testset loading complete.~n', []).

%% report_shift_family(+Pattern)
%  Reports one shift pattern family with its members.
report_shift_family(Pattern) :-
    Pattern = shift(Pw, Mod, Inst, An),
    logical_fingerprint:shift_family(Pattern, Members),
    length(Members, Count),
    format('### `shift(~w, ~w, ~w, ~w)` — ~w constraints~n~n',
           [Pw, Mod, Inst, An, Count]),
    % List all members (no truncation — conflict_map.py parses these)
    forall(member(M, Members), format('- `~w`~n', [M])),
    format('~n').

%% scope_changes_type(+Constraint)
%  True if varying scope while holding power=moderate, time=biographical,
%  exit=mobile produces different classification types.
scope_changes_type(C) :-
    scope_context(local, CtxLocal),
    scope_context(national, CtxNat),
    scope_context(global, CtxGlobal),
    catch(drl_core:dr_type(C, CtxLocal, TLocal), _, fail),
    catch(drl_core:dr_type(C, CtxNat, TNat), _, fail),
    catch(drl_core:dr_type(C, CtxGlobal, TGlobal), _, fail),
    \+ (TLocal == TNat, TNat == TGlobal).

%% scope_context(+Scope, -Context)
%  Standard context varying only scope, holding power=moderate.
scope_context(Scope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(Scope))).

%% report_scope_differentiation(+Constraint)
%  Shows how scope changes classification for a constraint.
report_scope_differentiation(C) :-
    scope_context(local, CtxL),
    scope_context(national, CtxN),
    scope_context(global, CtxG),
    (catch(drl_core:dr_type(C, CtxL, TL), _, TL = error) ; TL = error),
    (catch(drl_core:dr_type(C, CtxN, TN), _, TN = error) ; TN = error),
    (catch(drl_core:dr_type(C, CtxG, TG), _, TG = error) ; TG = error),
    format('- `~w`: local=~w, national=~w, global=~w~n', [C, TL, TN, TG]).

%% report_zone_distribution(+Constraints)
%  Summarizes how constraints distribute across extraction/suppression zones.
report_zone_distribution(Constraints) :-
    findall(EZ,
            (member(C, Constraints),
             logical_fingerprint:fingerprint_zone(C, zone(EZ, _)),
             EZ \= unknown),
            EZones),
    msort(EZones, SortedEZ),
    format('### Extraction Zones~n~n'),
    report_counts(SortedEZ),
    format('~n'),
    findall(SZ,
            (member(C, Constraints),
             logical_fingerprint:fingerprint_zone(C, zone(_, SZ)),
             SZ \= unknown),
            SZones),
    msort(SZones, SortedSZ),
    format('### Suppression Zones~n~n'),
    report_counts(SortedSZ),
    format('~n').

%% report_counts(+SortedList)
%  Counts consecutive runs in a sorted list and reports them.
report_counts([]).
report_counts([H|T]) :-
    count_run(H, T, Count, Rest),
    format('| ~w | ~w |~n', [H, Count]),
    report_counts(Rest).

count_run(_, [], 1, []).
count_run(H, [H|T], N, Rest) :-
    !, count_run(H, T, N1, Rest), N is N1 + 1.
count_run(_, List, 1, List).
