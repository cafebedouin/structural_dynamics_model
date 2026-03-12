% ============================================================================
% GROTHENDIECK COHOMOLOGY — Cech Cohomological Invariants (v1.0)
% ============================================================================
% Computes cohomological invariants of the DR presheaf over the site of
% observer positions. Where the Dirac module computes orbits (how types vary
% across contexts), this module computes cohomological invariants (how the
% *failure* of global consistency is structured).
%
% Mathematical basis:
%   The 4 standard contexts form an open cover U = {U_1, ..., U_4} of the
%   observer space. The DR presheaf F assigns to each U_i a type from Omega.
%   Cech cohomology of F with respect to U:
%
%     H^0(U, F) = global sections = constraints where F is constant on U
%     H^1(U, F) = obstruction to gluing = measure of perspectival fracture
%
%   On a discrete 4-point site, H^0 counts constraints with singleton orbits.
%   H^1 is approximated by counting disagreeing context-pairs (0..6).
%
% Rigor:
%   H^0 computation: STRICT — literal global sections of a presheaf on a
%     finite cover. This IS the definition of Cech H^0.
%   H^1 proxy: STRUCTURAL — the disagreeing-pairs count is a combinatorial
%     descent-failure count on the poset site. It measures how many
%     restriction morphisms fail to preserve the type. This is NOT formal
%     Cech H^1 (which requires ker(delta^1)/im(delta^0) and is trivially 0
%     on a discrete site). On the linear poset with Alexandrov topology,
%     it is a well-motivated obstruction measure with genuine diagnostic value.
%   Descent condition: STRICT — descent holds iff H^1 = 0, which is exactly
%     the condition that local sections glue to a global section.
%
% This module is DIAGNOSTIC ONLY. It does not modify any classification,
% purity score, or existing pipeline output.
%
% Standalone run:
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl \
%     -l dirac_classification.pl -l grothendieck_cohomology.pl \
%     -g "grothendieck_cohomology:cohomology_selftest, halt."
% ============================================================================

:- module(grothendieck_cohomology, [
    % Configuration
    cohomology_enabled/0,

    % Core API
    cohomological_obstruction/3,    % cohomological_obstruction(C, H0, H1)
    descent_status/2,               % descent_status(C, Status)

    % Corpus-level analysis
    corpus_cohomology/1,            % corpus_cohomology(Summary)

    % Contextuality fraction (Abramsky-Brandenburger)
    constraint_contextuality/2,     % constraint_contextuality(C, CF)
    contextuality_fraction/1,       % contextuality_fraction(CF)
    contextuality_by_type/1,        % contextuality_by_type(TypeCFs)
    contextuality_summary/1,        % contextuality_summary(Summary)

    % Cache management
    cohomology_cleanup/0,

    % Standalone validation
    cohomology_selftest/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(dirac_classification).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(covering_analysis).
:- use_module(library(lists)).

/* ================================================================
   DYNAMIC FACTS
   ================================================================ */

:- dynamic cached_obstruction/3.     % cached_obstruction(C, H0, H1)
:- dynamic cohomology_run_info/5.    % cohomology_run_info(Timestamp, N, H0Total, H1Total, H1Mean)

/* ================================================================
   CONFIGURATION
   ================================================================ */

cohomology_enabled :-
    config:param(cohomology_enabled, 1).

/* ================================================================
   CLEANUP
   ================================================================ */

cohomology_cleanup :-
    retractall(cached_obstruction(_, _, _)),
    retractall(cohomology_run_info(_, _, _, _, _)).

/* ================================================================
   CANONICAL CONTEXT ORDER

   The orbit vector must enumerate contexts in a fixed canonical
   order so that positional comparisons are meaningful. This order
   matches dirac_classification:standard_context/1 clause order
   and drl_core:standard_context/1 clause order.
   ================================================================ */

ordered_contexts([
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local)),
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national)),
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national)),
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))
]).

/* ================================================================
   ORBIT VECTOR — fixed-order type evaluation
   ================================================================ */

%% orbit_vector(+Constraint, -TypeVector)
%  Returns the 4-element list of types in canonical context order.
%  TypeVector = [T_powerless, T_moderate, T_institutional, T_analytical]
orbit_vector(C, TypeVector) :-
    ordered_contexts(Contexts),
    maplist(type_at_context(C), Contexts, TypeVector).

%% type_at_context(+C, +Context, -Type)
%  Evaluates the presheaf at a single context. Falls back to unknown
%  if classification fails (should not happen for well-formed constraints).
type_at_context(C, Ctx, Type) :-
    (   drl_core:dr_type(C, Ctx, T)
    ->  Type = T
    ;   Type = unknown
    ).

/* ================================================================
   CECH COHOMOLOGY COMPUTATION
   ================================================================ */

% Categorical: Cech cohomology on presheaf [H0: STRICT, H1: STRUCTURAL]
%% cohomological_obstruction(+Constraint, -H0, -H1)
%  H0 = 1 if the constraint has a global section (all contexts agree on type).
%  H0 = 0 otherwise.
%  H1 = number of disagreeing context-pairs (0..6). Range reflects C(4,2) = 6.
%  H0 and H1 are complementary: H0=1 implies H1=0; H1>0 implies H0=0.
cohomological_obstruction(C, H0, H1) :-
    (   cached_obstruction(C, CH0, CH1)
    ->  H0 = CH0, H1 = CH1
    ;   orbit_vector(C, TypeVector),
        sort(TypeVector, UniqueTypes),
        (   UniqueTypes = [_]
        ->  H0 = 1, H1 = 0
        ;   H0 = 0,
            count_disagreeing_pairs(TypeVector, H1)
        ),
        assertz(cached_obstruction(C, H0, H1))
    ).

% Categorical: Cech 1-cocycle proxy — counts disagreeing pairs in the orbit vector
%% count_disagreeing_pairs(+TypeVector, -Count)
%  For a 4-element vector, counts unordered pairs (i,j) with i < j
%  where TypeVector[i] \= TypeVector[j]. Range: 0..6.
count_disagreeing_pairs(TypeVector, Count) :-
    findall(1,
        (   nth1(I, TypeVector, T1),
            nth1(J, TypeVector, T2),
            I < J,
            T1 \= T2
        ),
        Ones),
    length(Ones, Count).

/* ================================================================
   DESCENT CONDITION
   ================================================================ */

% Categorical: Descent condition [STRICT] — tests whether local data glues to global
%% descent_status(+Constraint, -Status)
%  Status = descends(Type) if H0=1 (global section exists, constraint is
%    classified as Type from every context).
%  Status = fails_descent(H1, UniqueTypes) if H0=0 (gluing fails, H1
%    measures the degree of failure, UniqueTypes lists the distinct types).
descent_status(C, descends(Type)) :-
    cohomological_obstruction(C, 1, 0),
    orbit_vector(C, [Type|_]), !.
descent_status(C, fails_descent(H1, UniqueTypes)) :-
    cohomological_obstruction(C, 0, H1),
    orbit_vector(C, TypeVector),
    sort(TypeVector, UniqueTypes).

/* ================================================================
   CORPUS-LEVEL COHOMOLOGY
   ================================================================ */

% Categorical: Corpus-level Cech cohomology summary
%% corpus_cohomology(-Summary)
%  Computes H0, H1 for all corpus constraints and aggregates.
corpus_cohomology(Summary) :-
    cohomology_cleanup,
    corpus_loader:load_all_testsets,
    covering_analysis:all_corpus_constraints(Constraints),
    length(Constraints, N),
    format(user_error, '[cohomology] Computing for ~w constraints...~n', [N]),

    % Compute H0, H1 for all constraints
    maplist(force_obstruction, Constraints),

    % Aggregate H0
    findall(C, cached_obstruction(C, 1, 0), H0Cs),
    length(H0Cs, H0Total),

    % H0 by type — which types have global sections
    findall(Type-Cnt,
        (   member(Type, [mountain, rope, tangled_rope, snare, scaffold,
                          piton, naturalized, unknown]),
            findall(C, (
                cached_obstruction(C, 1, 0),
                orbit_vector(C, [Type|_])
            ), TCs),
            length(TCs, Cnt),
            Cnt > 0
        ),
        TypeCounts),

    % H1 aggregation (over constraints with H0=0)
    findall(H1, (cached_obstruction(_, 0, H1), H1 > 0), H1List),
    (   H1List \= []
    ->  sum_list(H1List, H1Total),
        length(H1List, NH1),
        H1Mean is H1Total / NH1
    ;   H1Total = 0, H1Mean = 0.0
    ),

    % H1 distribution: how many constraints at each H1 value
    findall(V-Cnt,
        (   member(V, [0, 1, 2, 3, 4, 5, 6]),
            (   V =:= 0
            ->  Cnt = H0Total
            ;   findall(C, cached_obstruction(C, 0, V), VCs),
                length(VCs, Cnt)
            )
        ),
        H1Dist),

    % Descent rate
    (   N > 0 -> Rate is H0Total / N ; Rate = 0.0 ),

    % Maximum obstruction constraints (H1=6)
    findall(C, cached_obstruction(C, 0, 6), MaxCs),

    % H1 by analytical-context type
    findall(AnalType-MeanH1,
        (   member(AnalType, [mountain, rope, tangled_rope, snare, scaffold,
                               piton, naturalized, unknown]),
            findall(H1Val,
                (   cached_obstruction(Cx, _, H1Val),
                    analytical_type(Cx, AnalType)
                ),
                TypeH1s),
            TypeH1s \= [],
            sum_list(TypeH1s, SumH1),
            length(TypeH1s, NType),
            MeanH1 is SumH1 / NType
        ),
        H1ByType),

    get_time(Timestamp),
    assertz(cohomology_run_info(Timestamp, N, H0Total, H1Total, H1Mean)),

    Summary = cohomology_summary(
        corpus_size(N),
        h0_total(H0Total),
        h0_by_type(TypeCounts),
        h1_total(H1Total),
        h1_mean(H1Mean),
        h1_distribution(H1Dist),
        descent_rate(Rate),
        max_obstruction(MaxCs),
        h1_by_analytical_type(H1ByType)
    ),

    format(user_error, '[cohomology] Complete: |H0|=~w, sum(H1)=~w, descent_rate=~4f~n',
           [H0Total, H1Total, Rate]).

%% force_obstruction(+C)
%  Forces computation and caching of cohomological obstruction.
force_obstruction(C) :-
    cohomological_obstruction(C, _, _).

%% analytical_type(+Constraint, -Type)
%  Returns the type at the analytical context.
analytical_type(C, Type) :-
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, Type).

/* ================================================================
   CONTEXTUALITY FRACTION (Abramsky-Brandenburger)

   The contextuality fraction measures the proportion of an empirical
   model that cannot be explained by any non-contextual (global) hidden
   variable assignment. On this 4-point site:

     - A constraint is contextual iff H0 = 0 (no global section).
     - Binary CF (corpus) = |{C : H0(C)=0}| / N = 1 - descent_rate.
     - Graded CF (per-constraint) = H1 / 6 (fraction of C(4,2) pairs
       that disagree). By Theorem 2's gap, H1 in {0,3,4,5,6}, so
       the graded CF takes values in {0, 0.5, 0.667, 0.833, 1.0}.

   The graded corpus mean reflects the gap structure: the absence of
   H1 in {1,2} means low-contextuality constraints do not exist —
   contextuality, when present, is always at least half-maximal.
   ================================================================ */

%% constraint_contextuality(+Constraint, -CF)
%  Per-constraint contextuality fraction: CF = H1 / 6.
%  CF = 0.0 means non-contextual (global section exists).
%  CF = 1.0 means maximally contextual (all 6 pairs disagree).
%  Works without corpus precomputation.
constraint_contextuality(C, CF) :-
    cohomological_obstruction(C, _, H1),
    CF is H1 / 6.

%% contextuality_fraction(-CF)
%  Corpus-level binary contextuality fraction: CF = 1 - descent_rate.
%  Requires cached_obstruction/3 to be populated (calls corpus_cohomology
%  if cache is empty).
contextuality_fraction(CF) :-
    ensure_corpus_computed,
    findall(C, cached_obstruction(C, _, _), AllCs),
    length(AllCs, N),
    (   N > 0
    ->  findall(C, cached_obstruction(C, 0, _), CtxCs),
        length(CtxCs, NCtx),
        CF is NCtx / N
    ;   CF = 0.0
    ).

%% contextuality_by_type(-TypeCFs)
%  Binary contextuality fraction broken down by analytical-perspective type.
%  TypeCFs = [Type-CF, ...] for each type with at least one constraint.
contextuality_by_type(TypeCFs) :-
    ensure_corpus_computed,
    findall(AnalType-CF,
        (   member(AnalType, [mountain, rope, tangled_rope, snare, scaffold,
                               piton, naturalized, unknown]),
            findall(C, (cached_obstruction(C, _, _), analytical_type(C, AnalType)), AllOfType),
            AllOfType \= [],
            length(AllOfType, NType),
            findall(C, (cached_obstruction(C, 0, _), analytical_type(C, AnalType)), CtxOfType),
            length(CtxOfType, NCtxType),
            CF is NCtxType / NType
        ),
        TypeCFs).

%% contextuality_summary(-Summary)
%  Aggregate contextuality metrics.
%  Summary = cf_summary(corpus_cf(CF), graded_mean(Mean),
%                       by_type(TypeCFs), distribution(Dist))
%  graded_mean = mean of H1/6 across the corpus — the more theoretically
%  interesting number, as its discrete support reflects the H1 gap.
contextuality_summary(Summary) :-
    contextuality_fraction(CF),
    contextuality_by_type(TypeCFs),
    % Graded mean: mean(H1_i / 6) over all constraints
    findall(H1, cached_obstruction(_, _, H1), AllH1),
    length(AllH1, N),
    (   N > 0
    ->  sum_list(AllH1, SumH1),
        Mean is SumH1 / (6 * N)
    ;   Mean = 0.0
    ),
    % Distribution: count of constraints at each graded CF level
    findall(GradedCF-Cnt,
        (   member(H1Val, [0, 3, 4, 5, 6]),
            GradedCF is H1Val / 6,
            findall(C, cached_obstruction(C, _, H1Val), Cs),
            length(Cs, Cnt),
            Cnt > 0
        ),
        Dist),
    Summary = cf_summary(
        corpus_cf(CF),
        graded_mean(Mean),
        by_type(TypeCFs),
        distribution(Dist)
    ).

%% ensure_corpus_computed
%  Populates the cached_obstruction/3 cache if it is empty.
ensure_corpus_computed :-
    cached_obstruction(_, _, _), !.
ensure_corpus_computed :-
    corpus_cohomology(_).

/* ================================================================
   SELFTEST
   ================================================================ */

cohomology_selftest :-
    format('~n=== Grothendieck Cohomology Selftest ===~n~n'),
    corpus_loader:load_all_testsets,

    corpus_cohomology(Summary),
    Summary = cohomology_summary(
        corpus_size(N),
        h0_total(H0Total),
        h0_by_type(TypeCounts),
        h1_total(H1Total),
        h1_mean(H1Mean),
        h1_distribution(H1Dist),
        descent_rate(Rate),
        max_obstruction(MaxCs),
        h1_by_analytical_type(H1ByType)
    ),

    format('--- Corpus Summary ---~n'),
    format('  Corpus size:    ~w~n', [N]),
    format('  |H0| (global):  ~w~n', [H0Total]),
    H0Fail is N - H0Total,
    format('  |H1>0| (fail):  ~w~n', [H0Fail]),
    format('  Sum(H1):        ~w~n', [H1Total]),
    format('  Mean H1:        ~4f~n', [H1Mean]),
    format('  Descent rate:   ~4f~n~n', [Rate]),

    format('--- H0 by Preserved Type ---~n'),
    forall(member(T-Cnt, TypeCounts),
           format('  ~w: ~w~n', [T, Cnt])),

    format('~n--- H1 Distribution ---~n'),
    forall(member(V-Cnt, H1Dist),
           format('  H1=~w: ~w constraints~n', [V, Cnt])),

    format('~n--- Mean H1 by Analytical Type ---~n'),
    forall(member(AT-MH1, H1ByType),
           format('  ~w: ~4f~n', [AT, MH1])),

    format('~n--- Max Obstruction (H1=6) ---~n'),
    length(MaxCs, NMax),
    format('  ~w constraints with maximal obstruction~n', [NMax]),
    forall(member(MC, MaxCs),
           (   orbit_vector(MC, OV),
               format('    ~w: ~w~n', [MC, OV])
           )),

    % Sample constraints at each H1 level
    format('~n--- Sample Orbits by H1 Level ---~n'),
    forall(
        (member(V, [0, 1, 2, 3, 4, 5, 6]),
         findall(C, cached_obstruction(C, _, V), AllAtV),
         AllAtV \= []),
        (   AllAtV = [First|_],
            orbit_vector(First, SampleOV),
            format('  H1=~w: ~w  orbit=~w~n', [V, First, SampleOV])
        )
    ),

    % Correlation: H1 vs purity_score
    format('~n--- H1 vs Purity Score Correlation ---~n'),
    forall(
        member(V, [0, 1, 2, 3, 4, 5, 6]),
        (   findall(P,
                (cached_obstruction(C, _, V),
                 purity_scoring:purity_score(C, P),
                 P >= 0.0),  % Exclude -1.0 sentinel
                Purities),
            (   Purities \= []
            ->  sum_list(Purities, SumP),
                length(Purities, NP),
                MeanP is SumP / NP,
                format('  H1=~w: mean_purity=~4f (n=~w)~n', [V, MeanP, NP])
            ;   format('  H1=~w: no purity data~n', [V])
            )
        )
    ),

    % Contextuality fraction
    format('~n--- Contextuality Fraction (AB) ---~n'),
    contextuality_summary(CFSummary),
    CFSummary = cf_summary(
        corpus_cf(CorpusCF),
        graded_mean(GradedMean),
        by_type(TypeCFs),
        distribution(CFDist)
    ),
    format('  Corpus CF (binary):  ~4f~n', [CorpusCF]),
    format('  Graded mean (H1/6): ~4f~n', [GradedMean]),
    format('  Cross-check: 1 - descent_rate = ~4f~n', [1.0 - Rate]),
    format('~n  CF by analytical type:~n'),
    forall(member(AT2-TCF, TypeCFs),
           format('    ~w: ~4f~n', [AT2, TCF])),
    format('~n  Graded CF distribution:~n'),
    forall(member(GCF-GCnt, CFDist),
           format('    CF=~4f: ~w constraints~n', [GCF, GCnt])),

    format('~n=== Selftest Complete ===~n').
