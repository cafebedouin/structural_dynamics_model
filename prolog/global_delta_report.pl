% ============================================================================
% GLOBAL DELTA REPORT — Structural Derivation vs Legacy Canonical Baseline
% ============================================================================
% Compares derive_directionality/3 (v5.0 structural derivation) against
% canonical_d_for_power/2 (legacy canonical fallback) across the entire
% corpus, for each standard context.
%
% Usage: cd prolog && swipl -l stack.pl -l global_delta_report.pl -g run_delta_report -t halt
% ============================================================================

:- module(global_delta_report, [run_delta_report/0]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(drl_core).
:- use_module(corpus_loader).

% Standard contexts to test (matching drl_core:standard_context/1)
std_ctx(powerless,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

std_ctx(moderate,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

std_ctx(institutional,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

std_ctx(analytical,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% ── Discover all constraints with extractiveness data ─────────────────────

all_constraints(Cs) :-
    findall(C,
        (   config:param(extractiveness_metric_name, ExtName),
            narrative_ontology:constraint_metric(C, ExtName, _)
        ),
        CsRaw),
    sort(CsRaw, Cs).

% ── Core comparison logic ─────────────────────────────────────────────────

%% compare_one(+Constraint, +PowerLabel, +Context, -Result)
%  Result = delta(Constraint, PowerLabel, D_actual, D_canonical, F_actual, F_canonical,
%                 Chi_actual, Chi_canonical, Type_actual, Type_canonical, HasBenef, HasVict)
%  or no_delta if d values are identical.
compare_one(C, PowerLabel, Ctx, Result) :-
    Ctx = context(agent_power(Power), _, _, spatial_scope(Scope)),

    % Resolve coalition power (same for both paths)
    constraint_indexing:resolve_coalition_power(Power, C, ResolvedPower),
    Ctx = context(_, T, E, S),
    ResolvedCtx = context(agent_power(ResolvedPower), T, E, S),

    % Actual: derive_directionality (override > structural > canonical)
    constraint_indexing:derive_directionality(C, ResolvedCtx, D_actual),

    % Legacy: canonical_d_for_power only
    constraint_indexing:canonical_d_for_power(ResolvedPower, D_canonical),

    % Check for structural data
    (narrative_ontology:constraint_beneficiary(C, _) -> HasBenef = yes ; HasBenef = no),
    (narrative_ontology:constraint_victim(C, _) -> HasVict = yes ; HasVict = no),

    % Check for override
    (constraint_indexing:directionality_override(C, ResolvedPower, _) -> HasOverride = yes ; HasOverride = no),

    % Compare d values
    DeltaD is abs(D_actual - D_canonical),
    (   DeltaD > 0.0001
    ->  % Compute both f(d) and chi
        constraint_indexing:sigmoid_f(D_actual, F_actual),
        constraint_indexing:sigmoid_f(D_canonical, F_canonical),
        config:param(extractiveness_metric_name, ExtName),
        narrative_ontology:constraint_metric(C, ExtName, BaseEps),
        constraint_indexing:scope_modifier(Scope, ScopeMod),
        Chi_actual is BaseEps * F_actual * ScopeMod,
        Chi_canonical is BaseEps * F_canonical * ScopeMod,

        % Classify both
        drl_core:get_raw_suppression(C, Supp),
        (drl_core:classify_from_metrics(C, BaseEps, Chi_actual, Supp, Ctx, Type_actual_raw)
        -> Type_actual = Type_actual_raw ; Type_actual = unknown),
        (drl_core:classify_from_metrics(C, BaseEps, Chi_canonical, Supp, Ctx, Type_canonical_raw)
        -> Type_canonical = Type_canonical_raw ; Type_canonical = unknown),

        Result = delta(C, PowerLabel, ResolvedPower,
                       D_actual, D_canonical,
                       F_actual, F_canonical,
                       Chi_actual, Chi_canonical,
                       Type_actual, Type_canonical,
                       HasBenef, HasVict, HasOverride, BaseEps, Supp)
    ;   Result = no_delta
    ).

% ── Main entry point ──────────────────────────────────────────────────────

run_delta_report :-
    load_all_testsets,
    all_constraints(Cs),
    length(Cs, NC),
    format(user_error, '[delta] Scanning ~w constraints across 4 standard contexts...~n', [NC]),

    % Collect all deltas
    findall(Delta,
        (   member(C, Cs),
            std_ctx(Label, Ctx),
            catch(compare_one(C, Label, Ctx, Delta), _, fail),
            Delta \= no_delta
        ),
        Deltas),
    length(Deltas, ND),

    % Count classification shifts
    include(is_type_shift, Deltas, Shifts),
    length(Shifts, NS),

    % Count d-only shifts (same classification)
    include(is_d_only_shift, Deltas, DOnly),
    length(DOnly, NDOnly),

    % Count by source
    include(has_override, Deltas, WithOverride),
    length(WithOverride, NOverride),
    include(is_structural_only, Deltas, StructOnly),
    length(StructOnly, NStruct),

    % ── Header ──
    nl,
    writeln('================================================================================'),
    writeln('GLOBAL DELTA REPORT: Structural Derivation vs Legacy Canonical Baseline'),
    writeln('================================================================================'),
    format('Constraints scanned:     ~w~n', [NC]),
    format('Context x Constraint:    ~w~n', [ND]),
    format('  d-value deltas found:  ~w~n', [ND]),
    format('  Classification shifts: ~w~n', [NS]),
    format('  d-shift, same type:    ~w~n', [NDOnly]),
    format('  From override:         ~w~n', [NOverride]),
    format('  From structural:       ~w~n', [NStruct]),
    nl,

    % ── Classification Shifts ──
    (   NS > 0
    ->  writeln('================================================================================'),
        writeln('CLASSIFICATION SHIFTS (type changed due to structural derivation)'),
        writeln('================================================================================'),
        nl,
        format('~w~t~48|~w~t~60|~w~t~72|~w~t~80|~w~t~88|~w~t~98|~w~t~108|~w~n',
               ['Constraint', 'Perspective', 'Resolved', 'D_act', 'D_can',
                'Type_act', 'Type_can', 'Source']),
        format('~`-t~108|~n'),
        forall(member(S, Shifts), print_shift(S)),
        nl
    ;   writeln('================================================================================'),
        writeln('NO CLASSIFICATION SHIFTS — all 727 classifications preserved'),
        writeln('================================================================================'),
        nl
    ),

    % ── D-Value Deltas (same type) ── Summary by perspective
    writeln('================================================================================'),
    writeln('D-VALUE DELTA SUMMARY BY PERSPECTIVE'),
    writeln('================================================================================'),
    nl,
    forall(std_ctx(Label, _),
        (   include(matches_perspective(Label), Deltas, PDeltas),
            length(PDeltas, PCount),
            (PCount > 0 ->
                include(is_type_shift, PDeltas, PShifts),
                length(PShifts, PSCount),
                format('  ~w: ~w d-deltas, ~w classification shifts~n', [Label, PCount, PSCount])
            ;   format('  ~w: 0 d-deltas~n', [Label])
            )
        )
    ),
    nl,

    % ── Chi Delta Histogram ──
    writeln('================================================================================'),
    writeln('CHI DELTA DISTRIBUTION (|chi_actual - chi_canonical|)'),
    writeln('================================================================================'),
    nl,
    findall(AbsDelta,
        (member(D, Deltas), D = delta(_, _, _, _, _, _, _, ChiA, ChiC, _, _, _, _, _, _, _),
         AbsDelta is abs(ChiA - ChiC)),
        ChiDeltas),
    chi_histogram(ChiDeltas),
    nl,

    % ── Top 30 largest chi deltas ──
    writeln('================================================================================'),
    writeln('TOP 30 LARGEST CHI DELTAS'),
    writeln('================================================================================'),
    nl,
    findall(abs_delta(AbsD, Rec),
        (member(Rec, Deltas),
         Rec = delta(_, _, _, _, _, _, _, ChiA2, ChiC2, _, _, _, _, _, _, _),
         AbsD is abs(ChiA2 - ChiC2)),
        AbsDeltaList),
    sort(1, @>=, AbsDeltaList, Sorted),
    length(Sorted, SortedLen),
    Top is min(30, SortedLen),
    format('~w~t~42|~w~t~54|~w~t~62|~w~t~70|~w~t~80|~w~t~90|~w~t~100|~w~t~110|~w~n',
           ['Constraint', 'Perspective', 'D_act', 'D_can', 'Chi_act', 'Chi_can',
            'Type_act', 'Type_can', 'eps']),
    format('~`-t~110|~n'),
    print_top_n(Sorted, Top, 0),
    nl,

    % ── Structural data coverage ──
    writeln('================================================================================'),
    writeln('STRUCTURAL DATA COVERAGE'),
    writeln('================================================================================'),
    nl,
    count_structural_data(Cs),
    nl,

    writeln('================================================================================'),
    writeln('END OF GLOBAL DELTA REPORT'),
    writeln('================================================================================'),
    nl.

% ── Helpers ───────────────────────────────────────────────────────────────

is_type_shift(delta(_, _, _, _, _, _, _, _, _, TypeA, TypeC, _, _, _, _, _)) :-
    TypeA \= TypeC.

is_d_only_shift(delta(_, _, _, _, _, _, _, _, _, TypeA, TypeC, _, _, _, _, _)) :-
    TypeA == TypeC.

has_override(delta(_, _, _, _, _, _, _, _, _, _, _, _, _, yes, _, _)).

is_structural_only(delta(_, _, _, _, _, _, _, _, _, _, _, _, _, no, _, _)).

matches_perspective(Label, delta(_, Label, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).

print_shift(delta(C, Label, Resolved, DA, DC, _FA, _FC, _ChiA, _ChiC, TypeA, TypeC, _B, _V, HasOvr, _Eps, _Supp)) :-
    (HasOvr = yes -> Src = override ; Src = structural),
    format('~w~t~48|~w~t~60|~w~t~72|~4f~t~80|~4f~t~88|~w~t~98|~w~t~108|~w~n',
           [C, Label, Resolved, DA, DC, TypeA, TypeC, Src]).

print_top_n(_, Max, Max) :- !.
print_top_n([], _, _) :- !.
print_top_n([abs_delta(_, Rec)|Rest], Max, I) :-
    Rec = delta(C, Label, _, DA, DC, _, _, ChiA, ChiC, TypeA, TypeC, _, _, _, Eps, _),
    (TypeA \= TypeC -> Mark = ' ***' ; Mark = ''),
    format('~w~t~42|~w~t~54|~4f~t~62|~4f~t~70|~4f~t~80|~4f~t~90|~w~t~100|~w~w~n',
           [C, Label, DA, DC, ChiA, ChiC, TypeA, TypeC, Mark]),
    I1 is I + 1,
    print_top_n(Rest, Max, I1).

chi_histogram(Deltas) :-
    include(in_bin(0.00, 0.01), Deltas, B1), length(B1, N1),
    include(in_bin(0.01, 0.05), Deltas, B2), length(B2, N2),
    include(in_bin(0.05, 0.10), Deltas, B3), length(B3, N3),
    include(in_bin(0.10, 0.20), Deltas, B4), length(B4, N4),
    include(in_bin(0.20, 0.50), Deltas, B5), length(B5, N5),
    include(in_bin(0.50, 9.99), Deltas, B6), length(B6, N6),
    format('  [0.00, 0.01): ~w~n', [N1]),
    format('  [0.01, 0.05): ~w~n', [N2]),
    format('  [0.05, 0.10): ~w~n', [N3]),
    format('  [0.10, 0.20): ~w~n', [N4]),
    format('  [0.20, 0.50): ~w~n', [N5]),
    format('  [0.50, +∞):   ~w~n', [N6]).

in_bin(Lo, Hi, V) :- V >= Lo, V < Hi.

count_structural_data(Cs) :-
    include(has_beneficiary, Cs, WithB),
    include(has_victim, Cs, WithV),
    include(has_either, Cs, WithEither),
    include(has_both, Cs, WithBoth),
    include(has_override_any, Cs, WithOverride),
    length(WithB, NB),
    length(WithV, NV),
    length(WithEither, NE),
    length(WithBoth, NBoth),
    length(WithOverride, NOverride),
    length(Cs, NC),
    NNeither is NC - NE,
    format('  Total constraints:           ~w~n', [NC]),
    format('  With beneficiary data:       ~w~n', [NB]),
    format('  With victim data:            ~w~n', [NV]),
    format('  With either (derivation):    ~w  (structural derivation fires)~n', [NE]),
    format('  With both:                   ~w~n', [NBoth]),
    format('  With directionality_override:~w~n', [NOverride]),
    format('  With neither (canonical):    ~w  (canonical fallback only)~n', [NNeither]).

has_beneficiary(C) :- narrative_ontology:constraint_beneficiary(C, _), !.
has_victim(C) :- narrative_ontology:constraint_victim(C, _), !.
has_either(C) :- (has_beneficiary(C) ; has_victim(C)), !.
has_both(C) :- has_beneficiary(C), has_victim(C), !.
has_override_any(C) :- constraint_indexing:directionality_override(C, _, _), !.
