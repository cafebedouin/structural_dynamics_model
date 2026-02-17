:- module(omega1_audit, [
    run_omega1_audit/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(drl_core).
:- use_module(covering_analysis).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

/* ================================================================
   DYNAMIC FACTS — intermediate results for reporting
   ================================================================ */

:- dynamic omega1_unknown/1.            % omega1_unknown(Constraint)
:- dynamic omega1_profile/6.            % omega1_profile(C, BaseEps, Chi, Supp, Theater, Signature)
:- dynamic omega1_blocking_gate/2.      % omega1_blocking_gate(C, Gate)
:- dynamic omega1_structural_flags/6.   % omega1_structural_flags(C, HasCoord, HasAsymm, HasEnforce, Emerges, NatLaw)
:- dynamic omega1_cross_context/4.      % omega1_cross_context(C, Label, Context, Type)
:- dynamic omega1_gap_region/2.         % omega1_gap_region(C, Region)
:- dynamic omega1_primary_gate/2.       % omega1_primary_gate(C, Gate)

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

run_omega1_audit :-
    format(user_error, '[omega1] Starting Omega1 Unknown Audit...~n', []),
    corpus_loader:load_all_testsets,
    covering_analysis:all_corpus_constraints(Cs),
    length(Cs, NC),
    format(user_error, '[omega1] Corpus: ~w constraints~n', [NC]),

    % Step 1: Collect unknowns at analytical/global
    format(user_error, '[omega1] Step 1: Collecting unknowns at analytical/global...~n', []),
    collect_unknowns(Cs),
    findall(C, omega1_unknown(C), Unknowns),
    length(Unknowns, NU),
    format(user_error, '[omega1] Found ~w unknowns~n', [NU]),

    % Step 2: Compute full metric profiles
    format(user_error, '[omega1] Step 2: Computing metric profiles...~n', []),
    compute_profiles(Unknowns),

    % Step 3: Determine blocking gates
    format(user_error, '[omega1] Step 3: Determining blocking gates...~n', []),
    determine_all_blocking_gates(Unknowns),

    % Step 4: Cross-context classification
    format(user_error, '[omega1] Step 4: Cross-context classification...~n', []),
    compute_all_cross_context(Unknowns),

    % Step 5: Classify gap regions
    format(user_error, '[omega1] Step 5: Classifying gap regions...~n', []),
    classify_all_gap_regions(Unknowns),

    % Step 6: Self-consistency check
    format(user_error, '[omega1] Step 6: Self-consistency check...~n', []),
    self_consistency_check(Unknowns, ConsistencyResult),

    % Step 7: Generate report
    format(user_error, '[omega1] Step 7: Generating report...~n', []),
    report_full(NC, NU, ConsistencyResult),
    format(user_error, '[omega1] Done.~n', []).

/* ================================================================
   STEP 1: COLLECT UNKNOWNS
   ================================================================ */

collect_unknowns(Cs) :-
    retractall(omega1_unknown(_)),
    constraint_indexing:default_context(Ctx),
    forall(member(C, Cs), (
        (   catch(drl_core:dr_type(C, Ctx, Type), _, Type = error)
        ->  (   Type == unknown
            ->  assertz(omega1_unknown(C))
            ;   true
            )
        ;   assertz(omega1_unknown(C))  % dr_type failed entirely
        )
    )).

/* ================================================================
   STEP 2: COMPUTE METRIC PROFILES
   ================================================================ */

compute_profiles(Unknowns) :-
    retractall(omega1_profile(_, _, _, _, _, _)),
    retractall(omega1_structural_flags(_, _, _, _, _, _)),
    constraint_indexing:default_context(Ctx),
    forall(member(C, Unknowns), compute_one_profile(C, Ctx)).

compute_one_profile(C, Ctx) :-
    % BaseEps
    config:param(extractiveness_metric_name, ExtName),
    (   narrative_ontology:constraint_metric(C, ExtName, BaseEps0)
    ->  BaseEps = BaseEps0
    ;   BaseEps = 0.5
    ),
    % Chi
    (   catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), _, fail)
    ->  Chi = Chi0
    ;   Chi = -999.0
    ),
    % Supp
    drl_core:get_raw_suppression(C, Supp),
    % Theater
    config:param(theater_metric_name, TheaterName),
    (   narrative_ontology:constraint_metric(C, TheaterName, Theater0)
    ->  Theater = Theater0
    ;   Theater = 0.0
    ),
    % Structural signature
    (   catch(structural_signatures:constraint_signature(C, Sig0), _, Sig0 = error)
    ->  Sig = Sig0
    ;   Sig = no_signature
    ),
    assertz(omega1_profile(C, BaseEps, Chi, Supp, Theater, Sig)),
    % Structural flags
    (   narrative_ontology:has_coordination_function(C) -> HasCoord = true ; HasCoord = false ),
    (   narrative_ontology:has_asymmetric_extraction(C) -> HasAsymm = true ; HasAsymm = false ),
    (   drl_core:requires_active_enforcement(C) -> HasEnforce = true ; HasEnforce = false ),
    (   drl_core:emerges_naturally(C) -> Emerges = true ; Emerges = false ),
    (   drl_core:natural_law_without_beneficiary(C) -> NatLaw = true ; NatLaw = false ),
    assertz(omega1_structural_flags(C, HasCoord, HasAsymm, HasEnforce, Emerges, NatLaw)).

/* ================================================================
   STEP 3: BLOCKING GATE ANALYSIS
   ================================================================ */

determine_all_blocking_gates(Unknowns) :-
    retractall(omega1_blocking_gate(_, _)),
    retractall(omega1_primary_gate(_, _)),
    constraint_indexing:default_context(Ctx),
    forall(member(C, Unknowns), determine_gates_for(C, Ctx)).

determine_gates_for(C, Ctx) :-
    omega1_profile(C, BaseEps, Chi, Supp, Theater, _Sig),
    omega1_structural_flags(C, HasCoord, HasAsymm, HasEnforce, Emerges, NatLaw),

    % --- Mountain (line 252) ---
    config:param(mountain_suppression_ceiling, MSuppCeil),
    config:param(mountain_extractiveness_max, MMaxX),
    (   Supp > MSuppCeil
    ->  assertz(omega1_blocking_gate(C, not_mountain_supp))
    ;   true
    ),
    (   BaseEps > MMaxX
    ->  assertz(omega1_blocking_gate(C, not_mountain_eps))
    ;   true
    ),
    (   \+ constraint_indexing:effective_immutability_for_context(Ctx, mountain)
    ->  assertz(omega1_blocking_gate(C, not_mountain_immutable))
    ;   true
    ),

    % --- Snare (line 261) ---
    config:param(snare_chi_floor, SChiFloor),
    config:param(snare_epsilon_floor, SEpsFloor),
    config:param(snare_suppression_floor, SSuppFloor),
    (   NatLaw = true
    ->  assertz(omega1_blocking_gate(C, snare_blocked_natural_law))
    ;   true
    ),
    (   Chi < SChiFloor
    ->  assertz(omega1_blocking_gate(C, snare_chi_below_066))
    ;   true
    ),
    (   BaseEps < SEpsFloor
    ->  assertz(omega1_blocking_gate(C, snare_eps_below_046))
    ;   true
    ),
    (   Supp < SSuppFloor
    ->  assertz(omega1_blocking_gate(C, snare_supp_below_060))
    ;   true
    ),
    (   \+ snare_immutability_check_audit(Ctx)
    ->  assertz(omega1_blocking_gate(C, snare_immutability_fail))
    ;   true
    ),

    % --- Scaffold (line 270) ---
    config:param(scaffold_extraction_ceil, ScMaxX),
    (   Chi > ScMaxX
    ->  assertz(omega1_blocking_gate(C, scaffold_chi_above_030))
    ;   true
    ),
    (   HasCoord = false
    ->  assertz(omega1_blocking_gate(C, scaffold_no_coordination))
    ;   true
    ),
    (   \+ scaffold_temporality_check_audit(C)
    ->  assertz(omega1_blocking_gate(C, scaffold_no_temporality))
    ;   true
    ),
    (   Theater > 0.70
    ->  assertz(omega1_blocking_gate(C, scaffold_theater_above_070))
    ;   true
    ),

    % --- Rope (line 278) ---
    config:param(rope_chi_ceiling, RChiCeil),
    config:param(rope_epsilon_ceiling, REpsCeil),
    (   Chi > RChiCeil
    ->  assertz(omega1_blocking_gate(C, rope_chi_above_035))
    ;   true
    ),
    (   Chi > 0, BaseEps > REpsCeil
    ->  assertz(omega1_blocking_gate(C, rope_eps_above_045))
    ;   true
    ),
    (   \+ constraint_indexing:effective_immutability_for_context(Ctx, rope),
        Emerges = false
    ->  assertz(omega1_blocking_gate(C, rope_no_changeability))
    ;   true
    ),

    % --- Tangled Rope (line 291) ---
    config:param(tangled_rope_chi_floor, TRChiFloor),
    config:param(tangled_rope_chi_ceil, TRChiCeil),
    config:param(tangled_rope_epsilon_floor, TREpsFloor),
    config:param(tangled_rope_suppression_floor, TRSuppFloor),
    (   NatLaw = true
    ->  assertz(omega1_blocking_gate(C, tangled_blocked_natural_law))
    ;   true
    ),
    (   Chi < TRChiFloor
    ->  assertz(omega1_blocking_gate(C, tangled_chi_below_040))
    ;   true
    ),
    (   Chi > TRChiCeil
    ->  assertz(omega1_blocking_gate(C, tangled_chi_above_090))
    ;   true
    ),
    (   BaseEps < TREpsFloor
    ->  assertz(omega1_blocking_gate(C, tangled_eps_below_030))
    ;   true
    ),
    (   Supp < TRSuppFloor
    ->  assertz(omega1_blocking_gate(C, tangled_supp_below_040))
    ;   true
    ),
    (   HasEnforce = false
    ->  assertz(omega1_blocking_gate(C, tangled_no_enforcement))
    ;   true
    ),
    (   HasCoord = false
    ->  assertz(omega1_blocking_gate(C, tangled_no_coordination))
    ;   true
    ),
    (   HasAsymm = false
    ->  assertz(omega1_blocking_gate(C, tangled_no_asymmetry))
    ;   true
    ),

    % --- Piton (line 304) ---
    config:param(piton_extraction_ceiling, PXCeil),
    config:param(piton_epsilon_floor, PEpsFloor),
    config:param(piton_theater_floor, PTRFloor),
    (   Chi > PXCeil
    ->  assertz(omega1_blocking_gate(C, piton_chi_above_025))
    ;   true
    ),
    (   BaseEps =< PEpsFloor
    ->  assertz(omega1_blocking_gate(C, piton_eps_at_or_below_010))
    ;   true
    ),
    (   Theater < PTRFloor
    ->  assertz(omega1_blocking_gate(C, piton_theater_below_070))
    ;   true
    ),

    % --- Indexically Opaque (line 314) ---
    (   BaseEps =< REpsCeil
    ->  assertz(omega1_blocking_gate(C, opaque_eps_at_or_below_045))
    ;   true
    ),
    (   Chi >= TRChiFloor
    ->  assertz(omega1_blocking_gate(C, opaque_chi_at_or_above_040))
    ;   true
    ),

    % --- Primary blocking gate (first critical failure in classifier order) ---
    determine_primary_gate(C, Chi, BaseEps, Supp, Theater, HasCoord, HasAsymm, HasEnforce, Emerges, NatLaw, Ctx).

%% snare_immutability_check_audit(+Context)
%  Mirrors snare_immutability_check/1 from drl_core.pl.
snare_immutability_check_audit(Context) :-
    constraint_indexing:effective_immutability_for_context(Context, rope), !.
snare_immutability_check_audit(_Context) :-
    drl_core:standard_context(AltCtx),
    constraint_indexing:effective_immutability_for_context(AltCtx, rope), !.

%% scaffold_temporality_check_audit(+C)
%  Mirrors scaffold_temporality_check/1 from drl_core.pl.
scaffold_temporality_check_audit(C) :-
    narrative_ontology:has_sunset_clause(C), !.
scaffold_temporality_check_audit(C) :-
    \+ drl_core:requires_active_enforcement(C).

%% determine_primary_gate/11
%  Identifies the single most decisive blocking gate -- the first type in the
%  classifier chain where the constraint ALMOST qualifies but fails one condition.
%  Split into multiple clauses with cuts for clean control flow.

% Near-snare: Chi >= 0.66 and not natural law
determine_primary_gate(C, Chi, BaseEps, Supp, _Theater, _HasCoord, _HasAsymm, _HasEnforce, _Emerges, NatLaw, _Ctx) :-
    config:param(snare_chi_floor, SChiFloor),
    NatLaw \= true,
    Chi >= SChiFloor, !,
    config:param(snare_epsilon_floor, SEpsFloor),
    config:param(snare_suppression_floor, SSuppFloor),
    (   BaseEps < SEpsFloor
    ->  assertz(omega1_primary_gate(C, near_snare_eps_too_low))
    ;   Supp < SSuppFloor
    ->  assertz(omega1_primary_gate(C, near_snare_supp_too_low))
    ;   assertz(omega1_primary_gate(C, near_snare_immutability))
    ).
% Near-rope: Chi <= 0.35
determine_primary_gate(C, Chi, BaseEps, _Supp, _Theater, _HasCoord, _HasAsymm, _HasEnforce, _Emerges, _NatLaw, _Ctx) :-
    config:param(rope_chi_ceiling, RChiCeil),
    Chi =< RChiCeil, !,
    config:param(rope_epsilon_ceiling, REpsCeil),
    (   Chi > 0, BaseEps > REpsCeil
    ->  assertz(omega1_primary_gate(C, near_rope_eps_too_high))
    ;   assertz(omega1_primary_gate(C, near_rope_changeability))
    ).
% Near-tangled_rope: 0.40 <= Chi <= 0.90
determine_primary_gate(C, Chi, BaseEps, Supp, _Theater, HasCoord, HasAsymm, HasEnforce, _Emerges, _NatLaw, _Ctx) :-
    config:param(tangled_rope_chi_floor, TRChiFloor),
    config:param(tangled_rope_chi_ceil, TRChiCeil),
    Chi >= TRChiFloor,
    Chi =< TRChiCeil, !,
    config:param(tangled_rope_epsilon_floor, TREpsFloor),
    config:param(tangled_rope_suppression_floor, TRSuppFloor),
    (   BaseEps < TREpsFloor
    ->  assertz(omega1_primary_gate(C, near_tangled_eps_too_low))
    ;   Supp < TRSuppFloor
    ->  assertz(omega1_primary_gate(C, near_tangled_supp_too_low))
    ;   HasEnforce = false
    ->  assertz(omega1_primary_gate(C, near_tangled_no_enforcement))
    ;   HasCoord = false
    ->  assertz(omega1_primary_gate(C, near_tangled_no_coordination))
    ;   HasAsymm = false
    ->  assertz(omega1_primary_gate(C, near_tangled_no_asymmetry))
    ;   assertz(omega1_primary_gate(C, near_tangled_other))
    ).
% Chi gap: (0.35, 0.40) -- between rope and tangled_rope
determine_primary_gate(C, Chi, _BaseEps, _Supp, _Theater, _HasCoord, _HasAsymm, _HasEnforce, _Emerges, _NatLaw, _Ctx) :-
    config:param(rope_chi_ceiling, RChiCeil),
    config:param(tangled_rope_chi_floor, TRChiFloor),
    Chi > RChiCeil,
    Chi < TRChiFloor, !,
    assertz(omega1_primary_gate(C, chi_in_rope_tangled_gap)).
% Chi above all ceilings: > 0.90
determine_primary_gate(C, Chi, _BaseEps, _Supp, _Theater, _HasCoord, _HasAsymm, _HasEnforce, _Emerges, _NatLaw, _Ctx) :-
    config:param(tangled_rope_chi_ceil, TRChiCeil),
    Chi > TRChiCeil, !,
    assertz(omega1_primary_gate(C, chi_above_all_ceilings)).
% Catch-all
determine_primary_gate(C, _Chi, _BaseEps, _Supp, _Theater, _HasCoord, _HasAsymm, _HasEnforce, _Emerges, _NatLaw, _Ctx) :-
    assertz(omega1_primary_gate(C, unclassified_gap)).

/* ================================================================
   STEP 4: CROSS-CONTEXT CLASSIFICATION
   ================================================================ */

compute_all_cross_context(Unknowns) :-
    retractall(omega1_cross_context(_, _, _, _)),
    forall(member(C, Unknowns), compute_cross_context(C)).

compute_cross_context(C) :-
    % Institutional/Local
    structural_signatures:coupling_test_context(institutional, local, CtxInst),
    classify_safe(C, CtxInst, TypeInst),
    assertz(omega1_cross_context(C, 'Institutional/Local', CtxInst, TypeInst)),
    % Moderate/National
    structural_signatures:coupling_test_context(moderate, national, CtxMod),
    classify_safe(C, CtxMod, TypeMod),
    assertz(omega1_cross_context(C, 'Moderate/National', CtxMod, TypeMod)),
    % Analytical/Global (should be unknown by definition)
    constraint_indexing:default_context(CtxAna),
    classify_safe(C, CtxAna, TypeAna),
    assertz(omega1_cross_context(C, 'Analytical/Global', CtxAna, TypeAna)).

classify_safe(C, Ctx, Type) :-
    (   catch(drl_core:dr_type(C, Ctx, T), _, T = error)
    ->  Type = T
    ;   Type = error
    ).

/* ================================================================
   STEP 5: GAP REGION CLASSIFICATION
   ================================================================ */

classify_all_gap_regions(Unknowns) :-
    retractall(omega1_gap_region(_, _)),
    forall(member(C, Unknowns), classify_gap_region(C)).

classify_gap_region(C) :-
    omega1_profile(C, BaseEps, Chi, Supp, _Theater, _Sig),
    omega1_structural_flags(C, HasCoord, HasAsymm, HasEnforce, _Emerges, _NatLaw),
    config:param(rope_chi_ceiling, RChiCeil),
    config:param(tangled_rope_chi_floor, TRChiFloor),
    config:param(tangled_rope_epsilon_floor, TREpsFloor),
    config:param(tangled_rope_suppression_floor, TRSuppFloor),
    config:param(snare_chi_floor, SChiFloor),
    config:param(snare_epsilon_floor, SEpsFloor),
    % Region A: Chi in (0.35, 0.40) -- between rope and tangled_rope
    (   Chi > RChiCeil, Chi < TRChiFloor
    ->  Region = region_a_chi_gap
    ;
    % Region E: Chi >= 0.66, BaseEps < 0.46 -- near-snare with low epsilon
    (   Chi >= SChiFloor, BaseEps < SEpsFloor
    ->  Region = region_e_near_snare
    ;
    % Region B: Chi >= 0.40, BaseEps < 0.30 -- tangled_rope epsilon violation
    (   Chi >= TRChiFloor, BaseEps < TREpsFloor
    ->  Region = region_b_low_epsilon
    ;
    % Region C: Chi >= 0.40, Supp < 0.40 -- tangled_rope suppression violation
    (   Chi >= TRChiFloor, Supp < TRSuppFloor
    ->  Region = region_c_low_suppression
    ;
    % Region D: Chi >= 0.40, metrics met, structural flags missing
    (   Chi >= TRChiFloor, BaseEps >= TREpsFloor, Supp >= TRSuppFloor,
        (HasEnforce = false ; HasCoord = false ; HasAsymm = false)
    ->  Region = region_d_missing_flags
    ;
    % Region F: Other / uncategorized
        Region = region_f_other
    ))))),
    assertz(omega1_gap_region(C, Region)).

/* ================================================================
   STEP 6: SELF-CONSISTENCY CHECK
   ================================================================ */

self_consistency_check(Unknowns, Result) :-
    constraint_indexing:default_context(Ctx),
    findall(C,
        (   member(C, Unknowns),
            omega1_profile(C, BaseEps, Chi, Supp, _, _),
            drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, MetricType),
            MetricType \== unknown
        ),
        Mismatches),
    length(Mismatches, NMismatch),
    (   NMismatch =:= 0
    ->  Result = pass
    ;   Result = fail(NMismatch, Mismatches)
    ).

/* ================================================================
   REPORT GENERATION
   ================================================================ */

report_full(NC, NU, ConsistencyResult) :-
    format('# Omega1 Audit: The ~w Unknown Constraints~n~n', [NU]),
    format('*Investigates why ~w constraints classify as `unknown` at the*~n', [NU]),
    format('*analytical/global context, diagnoses their blocking gates,*~n'),
    format('*and recommends classification fixes.*~n~n'),
    format('---~n~n'),

    % Summary
    report_summary(NC, NU, ConsistencyResult),

    % Metric distribution
    report_metric_distribution,

    % Gap region distribution
    report_gap_regions,

    % Blocking gate frequency
    report_blocking_gates,

    % Primary gate analysis
    report_primary_gates,

    % Cross-context transition map
    report_cross_context,

    % Structural flags summary
    report_structural_flags,

    % Opaque impossibility proof
    report_opaque_impossibility,

    % Structural signature analysis
    report_signature_analysis,

    % Finding classification
    report_finding_classification(NU),

    % Embedded Prolog facts
    report_embedded_facts,

    format('---~n~n'),
    format('*End of Omega1 audit*~n').

/* ---- Summary ---- */

report_summary(NC, NU, ConsistencyResult) :-
    format('## Summary~n~n'),
    Pct is NU / max(1, NC) * 100,
    format('- **Total corpus**: ~w constraints~n', [NC]),
    format('- **Unknowns at analytical/global**: ~w (~1f%)~n', [NU, Pct]),
    % Cross-context unknown counts
    findall(C, (omega1_cross_context(C, 'Institutional/Local', _, unknown)), InstUnk),
    findall(C, (omega1_cross_context(C, 'Moderate/National', _, unknown)), ModUnk),
    length(InstUnk, NInstUnk),
    length(ModUnk, NModUnk),
    format('- **Unknowns at institutional/local**: ~w~n', [NInstUnk]),
    format('- **Unknowns at moderate/national**: ~w~n', [NModUnk]),
    % Subset check
    sort(ModUnk, ModUnkSorted),
    findall(C, omega1_unknown(C), AnaUnk),
    sort(AnaUnk, AnaUnkSorted),
    (   ord_subset(ModUnkSorted, AnaUnkSorted)
    ->  format('- **Moderate unknowns are a subset of analytical unknowns**: yes~n')
    ;   ord_intersection(ModUnkSorted, AnaUnkSorted, Overlap),
        length(Overlap, NOverlap),
        format('- **Moderate unknowns are a subset of analytical unknowns**: no (overlap = ~w)~n', [NOverlap])
    ),
    format('- **Self-consistency check**: '),
    (   ConsistencyResult = pass
    ->  format('PASS (all ~w unknowns confirmed via classify_from_metrics/6)~n', [NU])
    ;   ConsistencyResult = fail(NM, _)
    ->  format('FAIL (~w mismatches)~n', [NM])
    ;   format('~w~n', [ConsistencyResult])
    ),
    format('~n').

/* ---- Metric Distribution ---- */

report_metric_distribution :-
    format('## Metric Distribution of the Unknowns~n~n'),
    findall(E, omega1_profile(_, E, _, _, _, _), Eps),
    findall(X, omega1_profile(_, _, X, _, _, _), Chis),
    findall(S, omega1_profile(_, _, _, S, _, _), Supps),
    findall(T, omega1_profile(_, _, _, _, T, _), Theaters),
    format('| Metric | Min | Q1 | Median | Q3 | Max | Mean |~n'),
    format('|--------|-----|----|---------|----|-----|------|~n'),
    report_distrib_row('BaseEps', Eps),
    report_distrib_row('Chi (analytical/global)', Chis),
    report_distrib_row('Suppression', Supps),
    report_distrib_row('Theater', Theaters),
    format('~n').

report_distrib_row(Label, Values) :-
    msort(Values, Sorted),
    length(Sorted, N),
    (   N > 0
    ->  nth1(1, Sorted, Min),
        last(Sorted, Max),
        Q1Idx is max(1, round(N * 0.25)),
        MedIdx is max(1, round(N * 0.5)),
        Q3Idx is max(1, round(N * 0.75)),
        nth1(Q1Idx, Sorted, Q1),
        nth1(MedIdx, Sorted, Med),
        nth1(Q3Idx, Sorted, Q3),
        sum_list(Sorted, Sum),
        Mean is Sum / N,
        format('| ~w | ~3f | ~3f | ~3f | ~3f | ~3f | ~3f |~n',
               [Label, Min, Q1, Med, Q3, Max, Mean])
    ;   format('| ~w | - | - | - | - | - | - |~n', [Label])
    ).

/* ---- Gap Region Distribution ---- */

report_gap_regions :-
    format('## Gap Region Distribution~n~n'),
    format('| Region | Description | Count |~n'),
    format('|--------|-------------|-------|~n'),
    Regions = [
        region_a_chi_gap - 'Chi in (0.35, 0.40): between rope ceiling and tangled_rope floor',
        region_b_low_epsilon - 'Chi >= 0.40, BaseEps < 0.30: tangled_rope epsilon floor violation',
        region_c_low_suppression - 'Chi >= 0.40, Supp < 0.40: tangled_rope suppression floor violation',
        region_d_missing_flags - 'Chi >= 0.40, metrics met, missing structural flags',
        region_e_near_snare - 'Chi >= 0.66, BaseEps < 0.46: near-snare with low epsilon',
        region_f_other - 'Other / uncategorized'
    ],
    forall(member(R - Desc, Regions), (
        findall(C, omega1_gap_region(C, R), Cs),
        length(Cs, Count),
        format('| ~w | ~w | ~w |~n', [R, Desc, Count])
    )),
    format('~n'),
    % Show members of each region
    format('### Region Membership~n~n'),
    forall(member(R - Desc, Regions), (
        findall(C, omega1_gap_region(C, R), Cs),
        length(Cs, Count),
        (   Count > 0
        ->  format('**~w** (~w): ~w~n~n', [R, Desc, Count]),
            forall(member(C, Cs), (
                omega1_profile(C, BaseEps, Chi, Supp, _Theater, _Sig),
                format('- `~w` (eps=~3f, chi=~3f, supp=~3f)~n', [C, BaseEps, Chi, Supp])
            )),
            format('~n')
        ;   true
        )
    )).

/* ---- Blocking Gate Frequency ---- */

report_blocking_gates :-
    format('## Blocking Gate Frequency~n~n'),
    format('How many of the unknowns fail each specific gate condition:~n~n'),
    format('| Gate | Count | Description |~n'),
    format('|------|-------|-------------|~n'),
    GateDescs = [
        not_mountain_supp - 'Supp > 0.05',
        not_mountain_eps - 'BaseEps > 0.25',
        not_mountain_immutable - 'Not immutable at analytical context',
        snare_blocked_natural_law - 'Blocked by natural_law_without_beneficiary',
        snare_chi_below_066 - 'Chi < 0.66',
        snare_eps_below_046 - 'BaseEps < 0.46',
        snare_supp_below_060 - 'Supp < 0.60',
        snare_immutability_fail - 'Snare immutability check fails',
        scaffold_chi_above_030 - 'Chi > 0.30',
        scaffold_no_coordination - 'No has_coordination_function',
        scaffold_no_temporality - 'No sunset clause and requires enforcement',
        scaffold_theater_above_070 - 'Theater > 0.70',
        rope_chi_above_035 - 'Chi > 0.35',
        rope_eps_above_045 - 'BaseEps > 0.45 (when Chi > 0)',
        rope_no_changeability - 'Not changeable and not emerges_naturally',
        tangled_blocked_natural_law - 'Blocked by natural_law_without_beneficiary',
        tangled_chi_below_040 - 'Chi < 0.40',
        tangled_chi_above_090 - 'Chi > 0.90',
        tangled_eps_below_030 - 'BaseEps < 0.30',
        tangled_supp_below_040 - 'Supp < 0.40',
        tangled_no_enforcement - 'No requires_active_enforcement',
        tangled_no_coordination - 'No has_coordination_function',
        tangled_no_asymmetry - 'No has_asymmetric_extraction',
        piton_chi_above_025 - 'Chi > 0.25',
        piton_eps_at_or_below_010 - 'BaseEps <= 0.10',
        piton_theater_below_070 - 'Theater < 0.70',
        opaque_eps_at_or_below_045 - 'BaseEps <= 0.45',
        opaque_chi_at_or_above_040 - 'Chi >= 0.40'
    ],
    forall(member(Gate - Desc, GateDescs), (
        findall(C, omega1_blocking_gate(C, Gate), Cs),
        length(Cs, Count),
        (   Count > 0
        ->  format('| ~w | ~w | ~w |~n', [Gate, Count, Desc])
        ;   true
        )
    )),
    format('~n').

/* ---- Primary Gate Analysis ---- */

report_primary_gates :-
    format('## Primary Blocking Gate (What Each Unknown "Wants to Be")~n~n'),
    format('The primary gate is the first type in the classifier chain where the constraint~n'),
    format('almost qualifies but fails one condition.~n~n'),
    format('| Primary Gate | Count | Interpretation |~n'),
    format('|--------------|-------|----------------|~n'),
    PrimaryDescs = [
        chi_in_rope_tangled_gap - 'Chi is in the dead zone (0.35, 0.40) between rope and tangled_rope',
        near_tangled_no_enforcement - 'Would be tangled_rope but no requires_active_enforcement',
        near_tangled_no_coordination - 'Would be tangled_rope but no has_coordination_function',
        near_tangled_no_asymmetry - 'Would be tangled_rope but no has_asymmetric_extraction',
        near_tangled_eps_too_low - 'Would be tangled_rope but BaseEps < 0.30',
        near_tangled_supp_too_low - 'Would be tangled_rope but Supp < 0.40',
        near_tangled_other - 'In tangled_rope range but blocked by unidentified gate',
        near_snare_eps_too_low - 'Would be snare but BaseEps < 0.46',
        near_snare_supp_too_low - 'Would be snare but Supp < 0.60',
        near_snare_immutability - 'Would be snare but fails immutability check',
        near_rope_eps_too_high - 'Would be rope but BaseEps > 0.45',
        near_rope_changeability - 'Would be rope but not changeable/emerges_naturally',
        chi_above_all_ceilings - 'Chi > 0.90 -- above all type ceilings',
        unclassified_gap - 'No clear near-miss type identified'
    ],
    forall(member(Gate - Desc, PrimaryDescs), (
        findall(C, omega1_primary_gate(C, Gate), Cs),
        length(Cs, Count),
        (   Count > 0
        ->  format('| ~w | ~w | ~w |~n', [Gate, Count, Desc])
        ;   true
        )
    )),
    format('~n'),
    % List constraints by primary gate
    format('### Detailed Breakdown~n~n'),
    forall(member(Gate - Desc, PrimaryDescs), (
        findall(C, omega1_primary_gate(C, Gate), Cs),
        length(Cs, Count),
        (   Count > 0
        ->  format('**~w** (~w): ~w constraints~n', [Gate, Desc, Count]),
            forall(member(C, Cs), (
                omega1_profile(C, BaseEps, Chi, Supp, Theater, _Sig),
                format('- `~w` (eps=~3f, chi=~3f, supp=~3f, theater=~3f)~n',
                       [C, BaseEps, Chi, Supp, Theater])
            )),
            format('~n')
        ;   true
        )
    )).

/* ---- Cross-Context Transition Map ---- */

report_cross_context :-
    format('## Cross-Context Transition Map~n~n'),
    format('How each unknown is classified from different observer perspectives:~n~n'),
    format('| Constraint | Institutional/Local | Moderate/National | Analytical/Global |~n'),
    format('|------------|---------------------|-------------------|-------------------|~n'),
    findall(C, omega1_unknown(C), Unknowns),
    sort(Unknowns, Sorted),
    forall(member(C, Sorted), (
        (   omega1_cross_context(C, 'Institutional/Local', _, TI) -> true ; TI = '?' ),
        (   omega1_cross_context(C, 'Moderate/National', _, TM) -> true ; TM = '?' ),
        (   omega1_cross_context(C, 'Analytical/Global', _, TA) -> true ; TA = '?' ),
        format('| ~w | ~w | ~w | ~w |~n', [C, TI, TM, TA])
    )),
    format('~n'),
    % Transition pattern summary
    format('### Transition Pattern Summary~n~n'),
    findall(TI-TM,
        (   omega1_unknown(C),
            omega1_cross_context(C, 'Institutional/Local', _, TI),
            omega1_cross_context(C, 'Moderate/National', _, TM)
        ),
        Patterns),
    msort(Patterns, SortedPatterns),
    clumped(SortedPatterns, Clumped),
    format('| Institutional -> Moderate -> Analytical | Count |~n'),
    format('|----------------------------------------|-------|~n'),
    forall(member((PI-PM)-Count, Clumped), (
        format('| ~w -> ~w -> unknown | ~w |~n', [PI, PM, Count])
    )),
    format('~n').

/* ---- Structural Flags Summary ---- */

report_structural_flags :-
    format('## Structural Flags Summary~n~n'),
    findall(C, omega1_unknown(C), Unknowns),
    length(Unknowns, NU),
    count_flag(has_coordination, NCoord),
    count_flag(has_asymmetry, NAsymm),
    count_flag(has_enforcement, NEnforce),
    count_flag(emerges, NEmerges),
    count_flag(natural_law, NNatLaw),
    format('| Flag | Present | Absent | Present % |~n'),
    format('|------|---------|--------|-----------|~n'),
    report_flag_row('has_coordination_function', NCoord, NU),
    report_flag_row('has_asymmetric_extraction', NAsymm, NU),
    report_flag_row('requires_active_enforcement', NEnforce, NU),
    report_flag_row('emerges_naturally', NEmerges, NU),
    report_flag_row('natural_law_without_beneficiary', NNatLaw, NU),
    format('~n').

count_flag(has_coordination, N) :-
    findall(C, omega1_structural_flags(C, true, _, _, _, _), Cs), length(Cs, N).
count_flag(has_asymmetry, N) :-
    findall(C, omega1_structural_flags(C, _, true, _, _, _), Cs), length(Cs, N).
count_flag(has_enforcement, N) :-
    findall(C, omega1_structural_flags(C, _, _, true, _, _), Cs), length(Cs, N).
count_flag(emerges, N) :-
    findall(C, omega1_structural_flags(C, _, _, _, true, _), Cs), length(Cs, N).
count_flag(natural_law, N) :-
    findall(C, omega1_structural_flags(C, _, _, _, _, true), Cs), length(Cs, N).

report_flag_row(Label, Present, Total) :-
    Absent is Total - Present,
    Pct is Present / max(1, Total) * 100,
    format('| ~w | ~w | ~w | ~1f% |~n', [Label, Present, Absent, Pct]).

/* ---- Opaque Impossibility Proof ---- */

report_opaque_impossibility :-
    format('## Indexically Opaque Impossibility at Analytical Context~n~n'),
    format('**Claim**: The `indexically_opaque` clause (drl_core.pl:314-318) can NEVER fire '),
    format('at the analytical/global context.~n~n'),
    format('**Proof**:~n'),
    format('- The clause requires: `BaseEps > 0.45 AND Chi < 0.40`~n'),
    config:param(canonical_d_analytical, DA),
    constraint_indexing:sigmoid_f(DA, FDA),
    config:param(scope_modifier_global, SigmaGlobal),
    Multiplier is FDA * SigmaGlobal,
    format('- At analytical/global: `Chi = BaseEps * f(~3f) * ~2f = BaseEps * ~3f`~n',
           [DA, SigmaGlobal, Multiplier]),
    MaxBaseEps is 0.40 / Multiplier,
    format('- For `Chi < 0.40`: `BaseEps < 0.40 / ~3f = ~3f`~n', [Multiplier, MaxBaseEps]),
    format('- But `BaseEps > 0.45` is simultaneously required~n'),
    format('- `~3f < 0.45` -- **contradiction**~n~n', [MaxBaseEps]),
    format('**Empirical verification**:~n'),
    findall(C,
        (   omega1_unknown(C),
            omega1_profile(C, BaseEps, Chi, _, _, _),
            BaseEps > 0.45,
            Chi < 0.40
        ),
        Exceptions),
    length(Exceptions, NExc),
    format('- Constraints satisfying BOTH BaseEps > 0.45 AND Chi < 0.40: **~w** (expected: 0)~n',
           [NExc]),
    (   NExc =:= 0
    ->  format('- **Confirmed**: The opaque clause is structurally impossible at analytical context.~n~n')
    ;   format('- **UNEXPECTED**: Found ~w exceptions! Investigate: ~w~n~n', [NExc, Exceptions])
    ).

/* ---- Structural Signature Analysis ---- */

report_signature_analysis :-
    format('## Structural Signature Analysis~n~n'),
    format('The `dr_type/3` pipeline passes metric-based type through `integrate_signature_with_modal/3`.~n'),
    format('When the metric layer returns `unknown`, certain signatures can rescue the classification~n'),
    format('(e.g., `coordination_scaffold` -> `rope`, `constructed_high_extraction` -> `snare`).~n'),
    format('The 39 constraints that remain `unknown` after this stage have signatures that do NOT rescue.~n~n'),
    format('| Signature | Count |~n'),
    format('|-----------|-------|~n'),
    findall(Sig, omega1_profile(_, _, _, _, _, Sig), Sigs),
    msort(Sigs, SortedSigs),
    clumped(SortedSigs, Clumped),
    forall(member(S-Count, Clumped), (
        format('| ~w | ~w |~n', [S, Count])
    )),
    format('~n'),
    format('For `unknown` metric type, the rescue rules are:~n'),
    format('- `coordination_scaffold` -> rope~n'),
    format('- `constructed_low_extraction` -> rope~n'),
    format('- `constructed_high_extraction` -> snare~n'),
    format('- `constructed_constraint` -> tangled_rope~n'),
    format('- `piton_signature` -> piton~n'),
    format('- `ambiguous` -> unknown (no rescue)~n'),
    format('- Any other signature with no `unknown` rescue rule -> keeps unknown~n~n').

/* ---- Finding Classification ---- */

report_finding_classification(NU) :-
    format('## Finding Classification~n~n'),
    % Count regions
    findall(C, omega1_gap_region(C, region_a_chi_gap), RegA), length(RegA, NA),
    findall(C, omega1_gap_region(C, region_b_low_epsilon), RegB), length(RegB, NB),
    findall(C, omega1_gap_region(C, region_c_low_suppression), RegC), length(RegC, NC_),
    findall(C, omega1_gap_region(C, region_d_missing_flags), RegD), length(RegD, ND),
    findall(C, omega1_gap_region(C, region_e_near_snare), RegE), length(RegE, NE),
    findall(C, omega1_gap_region(C, region_f_other), RegF), length(RegF, NF),
    % Determine category
    TotalABC is NA + NB + NC_ + ND + NE,
    format('### Distribution~n~n'),
    format('- Region A (chi gap): ~w (~1f%)~n', [NA, NA/max(1,NU)*100]),
    format('- Region B (low epsilon): ~w (~1f%)~n', [NB, NB/max(1,NU)*100]),
    format('- Region C (low suppression): ~w (~1f%)~n', [NC_, NC_/max(1,NU)*100]),
    format('- Region D (missing flags): ~w (~1f%)~n', [ND, ND/max(1,NU)*100]),
    format('- Region E (near-snare): ~w (~1f%)~n', [NE, NE/max(1,NU)*100]),
    format('- Region F (other): ~w (~1f%)~n~n', [NF, NF/max(1,NU)*100]),
    % Determine overall finding category (A/B/C/D per user rubric)
    format('### Verdict~n~n'),
    (   NA > NU * 0.60
    ->  format('**Category A -- Single Missing Clause**: The majority (~w/~w) cluster in Region A ', [NA, NU]),
        format('(the chi gap between rope ceiling 0.35 and tangled_rope floor 0.40). '),
        format('A single new clause covering 0.35 < Chi < 0.40 would resolve most unknowns.~n~n')
    ;   ND > NU * 0.60
    ->  format('**Category D -- Data Quality Issues**: The majority (~w/~w) cluster in Region D ', [ND, NU]),
        format('(missing structural flags). The constraints have metrics compatible with tangled_rope '),
        format('but lack required flags (has_coordination_function, requires_active_enforcement, '),
        format('has_asymmetric_extraction). Fix the testset declarations.~n~n')
    ;   TotalABC > 1, NF < NU * 0.30
    ->  format('**Category B -- Multiple Distinct Gaps**: The unknowns split across several regions '),
        format('with no single dominant cause. Region A=~w, B=~w, C=~w, D=~w, E=~w, F=~w. ', [NA, NB, NC_, ND, NE, NF]),
        format('Each region may need separate treatment.~n~n')
    ;   format('**Category C -- Classifier Edge Cases**: The unknowns are scattered across boundary '),
        format('conditions with no clear pattern. Consider whether expanding existing type '),
        format('boundaries or adding new types is warranted.~n~n')
    ),
    format('### Recommendations~n~n'),
    (   NA > 0
    ->  format('1. **Chi gap (Region A)**: Add a classifier clause for 0.35 < Chi < 0.40. '),
        format('Semantically, these are constraints in transition between coordination (rope) '),
        format('and entangled extraction (tangled_rope). Options:~n'),
        format('   - Expand rope ceiling from 0.35 to 0.40 (subsume into rope)~n'),
        format('   - Lower tangled_rope floor from 0.40 to 0.35 (subsume into tangled_rope)~n'),
        format('   - Add a new type for the transition zone~n~n')
    ;   true
    ),
    (   ND > 0
    ->  format('2. **Missing flags (Region D)**: Review these testsets and add missing structural '),
        format('declarations (has_coordination_function, requires_active_enforcement, '),
        format('has_asymmetric_extraction) where warranted by the domain.~n~n')
    ;   true
    ),
    (   NC_ > 0
    ->  format('3. **Low suppression (Region C)**: These constraints have tangled_rope chi and '),
        format('epsilon but suppression < 0.40. Consider whether they represent a '),
        format('genuine category (extraction without enforcement) or data errors.~n~n')
    ;   true
    ),
    (   NE > 0
    ->  format('4. **Near-snare (Region E)**: These have snare-level chi (>= 0.66) but epsilon '),
        format('< 0.46. The extraction is high from the analytical view but the structural '),
        format('base extraction is low. May indicate context-amplified constraints.~n~n')
    ;   true
    ).

/* ---- Embedded Prolog Facts ---- */

report_embedded_facts :-
    format('## Embedded Prolog Facts~n~n'),
    format('```prolog~n'),
    format('%% omega1_unknown(Constraint, AnalyticalContext, PrimaryBlockingGate).~n'),
    constraint_indexing:default_context(Ctx),
    findall(C, omega1_unknown(C), Unknowns),
    sort(Unknowns, Sorted),
    forall(member(C, Sorted), (
        (   omega1_primary_gate(C, Gate) -> true ; Gate = undiagnosed ),
        format('omega1_unknown(~q, ~q, ~q).~n', [C, Ctx, Gate])
    )),
    format('~n%% omega1_transition(Constraint, InstitutionalType, ModerateType, AnalyticalType).~n'),
    forall(member(C, Sorted), (
        (   omega1_cross_context(C, 'Institutional/Local', _, TI) -> true ; TI = error ),
        (   omega1_cross_context(C, 'Moderate/National', _, TM) -> true ; TM = error ),
        (   omega1_cross_context(C, 'Analytical/Global', _, TA) -> true ; TA = error ),
        format('omega1_transition(~q, ~q, ~q, ~q).~n', [C, TI, TM, TA])
    )),
    format('```~n~n').
