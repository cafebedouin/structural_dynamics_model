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
    obstruction_from_vector/3,      % obstruction_from_vector(TypeVector, H0, H1) — pure, synthetic-testable
    is_real_type/1,                 % is_real_type(T) — OQ-51 N/A filter (T \== unknown)
    descent_status/2,               % descent_status(C, Status)

    % Corpus-level analysis
    corpus_cohomology/1,            % corpus_cohomology(Summary)

    % Contextuality fraction (Abramsky-Brandenburger)
    constraint_contextuality/2,     % constraint_contextuality(C, CF)
    contextuality_fraction/1,       % contextuality_fraction(CF)
    contextuality_by_type/1,        % contextuality_by_type(TypeCFs)
    contextuality_summary/1,        % contextuality_summary(Summary)

    % Power-chain monotonicity
    extraction_rank/2,              % extraction_rank(Type, Rank)
    orbit_monotonicity/2,           % orbit_monotonicity(C, Status)
    transition_boundaries/2,        % transition_boundaries(C, Boundaries)
    corpus_monotonicity/1,          % corpus_monotonicity(Summary)

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

% Central cache-invalidation surface (cache_registry.pl).
:- multifile cache_registry:clear_hook/0.
cache_registry:clear_hook :- cohomology_cleanup.

/* ================================================================
   CANONICAL CONTEXT ORDER

   The orbit vector enumerates contexts in a fixed canonical order so
   that positional comparisons are meaningful. ordered_contexts/1
   delegates to constraint_indexing:site_contexts/1, which defaults
   to the 4 canonical contexts and can be switched to the product
   site by changing that predicate.

   H¹ = count of disagreeing context-pairs across the observer site.
   GENERAL LAW (OQ-195 resolved 2026-07-02, docs/h1_gap_spectrum_general_n.md):
     for n REAL seats the reachable spectrum is H(n) — min nonzero = n-1
     ({1..n-2} forbidden at every n), exact band decomposition by largest
     agreement bloc, inter-band gap iff n >= j+3+C(j+1,2); with more real
     seats than type tokens (n > 7) the top truncates (Theorem D there).
   On the canonical 4-point site: range 0..6 = C(4,2);
     H¹ ∈ {0, 3, 4, 5, 6} — the n=4 instance of the law.
   On the 156-point product site: range 0..C(156,2) = 0..12,090.
     Minimum nonzero H¹ = 3,380 (26 minority × 130 majority contexts).
     Classification is quantized in power-level blocks of ~39 contexts.
   The binary distinction (H¹ = 0 vs H¹ > 0) is site-invariant in the
   chimera-era corpus where measured (zero crossings across 3,301
   constraints — pre-reset regime figure).
   ================================================================ */

ordered_contexts(Contexts) :-
    constraint_indexing:site_contexts(Contexts).

/* ================================================================
   ORBIT VECTOR — fixed-order type evaluation
   ================================================================ */

%% orbit_vector(+Constraint, -TypeVector)
%  Returns the 4-element list of types in canonical context order.
%  TypeVector = [T_powerless, T_moderate, T_institutional, T_analytical]
%
%  OQ-27 (signature-resolved orbit disclosure): the per-context type here is the
%  SIGNATURE-RESOLVED `dr_type`, NOT the raw metric type. Outer chain:
%    cohomological_obstruction -> orbit_vector -> type_at_context -> drl_core:dr_type.
%  Inside dr_type: metric_based_type_indexed (raw classify_from_metrics) THEN
%    signature_detection:integrate_signature_with_modal.
%  The raw classify_from_metrics type is only the intermediate fed to the signature
%  layer; H¹ counts disagreements over the POST-signature orbit. So H¹ = 0 means the
%  resolved orbit is a global section — the raw per-context metric types may still be
%  maximally heterogeneous (the signature is the cover story, Theorem 1). Reproduce H¹
%  over signature-resolved dr_type, never raw classify_from_metrics. (Paper: v7 §Thm 7;
%  v6.13.1 amendment.)
orbit_vector(C, TypeVector) :-
    ordered_contexts(Contexts),
    maplist(type_at_context(C), Contexts, TypeVector).

%% type_at_context(+C, +Context, -Type)
%  Evaluates the presheaf at a single context. Falls back to unknown
%  if classification fails (should not happen for well-formed constraints).
%  Type is the signature-resolved dr_type (see orbit_vector/2 above, OQ-27).
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
%  H1 = number of disagreeing context-pairs (0..6 on the canonical 4-seat site).
%  GENERAL-N LAW (OQ-195 RESOLVED 2026-07-02): the reachable spectrum for n real
%  seats is H(n) of docs/h1_gap_spectrum_general_n.md — min nonzero = n-1, so
%  {1..n-2} is forbidden at every cardinality (n=4 -> {0,3,4,5,6}; n=3 -> {0,2,3};
%  n=2 -> {0,1}); proven for all n, machine-verified n<=40, engine-witnessed by
%  tests/test_h1_spectrum.pl. A value outside H(n_real) is a bug witness.
%  H0 and H1 are complementary: H0=1 implies H1=0; H1>0 implies H0=0.
%  OQ-51 N/A rule (2026-06-25): `unknown` is N/A — an untypeable seat that can
%  neither agree nor disagree. Fewer than two REAL seats can form no pair, so the
%  obstruction is UNDETERMINED, serialized as H0 = null, H1 = null (the single
%  source of truth for "H1 is N/A"). This is the canonical-path twin of the
%  cs_kernel_registry build; it kills the Pattern-5 trap where an all-`unknown`
%  constraint read genuine_sheaf by absence. Mirrors cs_kernel_registry:is_real_type/1.
cohomological_obstruction(C, H0, H1) :-
    (   cached_obstruction(C, CH0, CH1)
    ->  H0 = CH0, H1 = CH1
    ;   orbit_vector(C, TypeVector),
        obstruction_from_vector(TypeVector, H0, H1),
        assertz(cached_obstruction(C, H0, H1))
    ).

%% obstruction_from_vector(+TypeVector, -H0, -H1)
%  Pure H0/H1 over a type vector under the OQ-51 N/A rule (no corpus, no cache —
%  synthetic-testable). <2 real seats ⇒ H0=null, H1=null (UNDETERMINED). Else the
%  real seats either all agree (H0=1, H1=0) or disagree (H0=0, H1=#real pairs).
obstruction_from_vector(TypeVector, H0, H1) :-
    include(is_real_type, TypeVector, RealVector),
    length(RealVector, NReal),
    (   NReal < 2
    ->  H0 = null, H1 = null   % N/A: <2 real seats can neither agree nor disagree
    ;   sort(RealVector, UniqueReal),
        (   UniqueReal = [_]
        ->  H0 = 1, H1 = 0
        ;   H0 = 0,
            count_disagreeing_pairs(RealVector, H1)
        )
    ).

%% is_real_type(+T)  (OQ-51 N/A rule)
%  `unknown` is N/A — not a value that agrees with itself, not a disagreeing type.
%  A real (typeable) seat is anything else. Shared filter; "both-real-different" is
%  the disagreement definition, applied identically wherever pairs are counted.
is_real_type(T) :- T \== unknown.

% Categorical: Cech 1-cocycle proxy — counts disagreeing pairs in the orbit vector
%% count_disagreeing_pairs(+TypeVector, -Count)
%  For a vector of length L, counts unordered pairs (i,j) with i < j where both
%  TypeVector[i] and TypeVector[j] are REAL and differ. The both-real requirement
%  is the OQ-51 N/A rule (an `unknown` on either side contributes to neither agree
%  nor disagree); do NOT refactor back to a bare `T1 \= T2`, which would count an
%  `unknown`-vs-real pair as disagreement and inflate H1.
count_disagreeing_pairs(TypeVector, Count) :-
    findall(1,
        (   nth1(I, TypeVector, T1),
            nth1(J, TypeVector, T2),
            I < J,
            is_real_type(T1), is_real_type(T2),
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
descent_status(C, undetermined) :-
    cohomological_obstruction(C, H0, _),
    H0 == null, !.   % OQ-51: <2 real seats — neither descends nor fails, it is N/A
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
                    number(H1Val),   % OQ-51: skip undetermined (null H1) — sum_list throws on the atom
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
%  Per-constraint contextuality fraction: CF = H1 / comparable_pairs, where
%  comparable_pairs = C(NReal, 2) counts only REAL-REAL seat pairs (the OQ-51
%  N/A rule — `unknown` seats are abstentions, in neither numerator nor
%  denominator). On a fully-real 4-seat orbit this is the old H1/6.
%  FAILS (→ JSON null) when the constraint is undetermined (null H1, <2 real
%  seats) — undetermined is N/A, not CF=0.
%  Works without corpus precomputation.
constraint_contextuality(C, CF) :-
    cohomological_obstruction(C, _, H1),
    number(H1),                       % fail-closed on undetermined (null H1)
    orbit_vector(C, TypeVector),
    include(is_real_type, TypeVector, RealVector),
    length(RealVector, NReal),
    Comparable is NReal * (NReal - 1) // 2,
    Comparable > 0,
    CF is H1 / Comparable.

%% contextuality_fraction(-CF)
%  Corpus-level binary contextuality fraction over DETERMINED constraints only:
%  CF = |{C : H0(C)=0}| / |{C : C determined}|. Undetermined constraints (null
%  H0, <2 real seats — OQ-51 N/A) are excluded from BOTH numerator and
%  denominator; counting them would be the absence-as-presence trap one level up
%  (a vacuous denominator inflation). Equals 1 - descent_rate when descent_rate
%  is itself taken over the determined set.
%  Requires cached_obstruction/3 to be populated (calls corpus_cohomology
%  if cache is empty).
contextuality_fraction(CF) :-
    ensure_corpus_computed,
    findall(C, (cached_obstruction(C, H0, _), number(H0)), AllCs),  % determined only
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
            findall(C, (cached_obstruction(C, H0, _), number(H0), analytical_type(C, AnalType)), AllOfType),  % determined only (OQ-51)
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
    % Graded mean: mean(H1_i / 6) over DETERMINED constraints (OQ-51: skip null
    % H1 — undetermined constraints carry no graded CF, and sum_list throws on the
    % atom). Keeps the corpus-shape /6 normalization (fraction of C(4,2)).
    findall(H1, (cached_obstruction(_, _, H1), number(H1)), AllH1),
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
   POWER-CHAIN MONOTONICITY

   The 4 standard contexts form a linear chain ordered by power:
     U_1(powerless) -> U_2(moderate) -> U_3(institutional) -> U_4(analytical)

   The extraction ordering on types is:
     mountain(0) < rope(1) < tangled_rope(2) < snare(3)

   A constraint's orbit vector [T_1, T_2, T_3, T_4] is monotone along
   the power chain if its extraction rank is non-decreasing (ascending)
   or non-increasing (descending). Non-monotone orbits oscillate —
   a qualitatively different kind of perspectival fracture.

   The transition boundary position (1, 2, or 3) identifies WHERE in
   the chain the type changes. This connects H^1 to the invariant
   analysis finding that institutional power (pi = -0.2) is a phase
   transition point.
   ================================================================ */

%% extraction_rank(+Type, -Rank)
%  Position on the extraction chain. Types outside the chain
%  (scaffold, piton, naturalized, unknown) get rank -1.
%  Deterministic: cuts prevent the catch-all from shadowing known types.
extraction_rank(mountain, 0) :- !.
extraction_rank(rope, 1) :- !.
extraction_rank(tangled_rope, 2) :- !.
extraction_rank(snare, 3) :- !.
extraction_rank(_, -1).

%% orbit_monotonicity(+Constraint, -Status)
%  Analyzes the orbit vector for monotonicity along the power chain.
%  Status is one of:
%    constant(Type)           - all 4 contexts agree (H^0 = 1)
%    monotone_ascending       - extraction rank non-decreasing, >= 1 strict increase
%    monotone_descending      - extraction rank non-increasing, >= 1 strict decrease
%    non_monotone(Reversals)  - direction changes; Reversals = count of sign changes
%    incomparable             - orbit contains types outside the extraction chain
orbit_monotonicity(C, Status) :-
    orbit_vector(C, TypeVector),
    sort(TypeVector, Unique),
    (   Unique = [SingleType]
    ->  Status = constant(SingleType)
    ;   maplist(extraction_rank, TypeVector, Ranks),
        (   member(-1, Ranks)
        ->  Status = incomparable
        ;   adjacent_deltas(Ranks, Deltas),
            classify_deltas(Deltas, Status)
        )
    ).

%% adjacent_deltas(+Ranks, -Deltas)
%  Computes the list of rank differences between adjacent positions.
%  For a 4-element list, returns 3 deltas.
adjacent_deltas([], []).
adjacent_deltas([_], []).
adjacent_deltas([R1, R2 | Rest], [D | Ds]) :-
    D is R2 - R1,
    adjacent_deltas([R2 | Rest], Ds).

%% classify_deltas(+Deltas, -Status)
%  Classifies a delta sequence as ascending, descending, or non-monotone.
classify_deltas(Deltas, monotone_ascending) :-
    forall(member(D, Deltas), D >= 0),
    member(D, Deltas), D > 0, !.
classify_deltas(Deltas, monotone_descending) :-
    forall(member(D, Deltas), D =< 0),
    member(D, Deltas), D < 0, !.
classify_deltas(Deltas, non_monotone(Reversals)) :-
    count_reversals(Deltas, Reversals).

%% count_reversals(+Deltas, -Count)
%  Counts direction changes in the delta sequence.
%  A reversal occurs when consecutive non-zero deltas have opposite signs.
count_reversals(Deltas, Count) :-
    include(\=(0), Deltas, NonZero),
    count_sign_changes(NonZero, 0, Count).

count_sign_changes([], Acc, Acc).
count_sign_changes([_], Acc, Acc).
count_sign_changes([A, B | Rest], Acc, Count) :-
    (   A > 0, B < 0 -> Acc1 is Acc + 1
    ;   A < 0, B > 0 -> Acc1 is Acc + 1
    ;   Acc1 = Acc
    ),
    count_sign_changes([B | Rest], Acc1, Count).

%% transition_boundaries(+Constraint, -Boundaries)
%  Returns boundary positions where the type changes in the orbit vector.
%  Boundaries = [boundary(Pos, TypeLeft, TypeRight), ...]
%  Pos in {1,2,3}: 1 = U1->U2, 2 = U2->U3, 3 = U3->U4.
transition_boundaries(C, Boundaries) :-
    orbit_vector(C, TypeVector),
    find_boundaries(TypeVector, 1, Boundaries).

find_boundaries([_], _, []).
find_boundaries([T1, T2 | Rest], Pos, Boundaries) :-
    (   T1 \= T2
    ->  Boundaries = [boundary(Pos, T1, T2) | MoreBs]
    ;   Boundaries = MoreBs
    ),
    Pos1 is Pos + 1,
    find_boundaries([T2 | Rest], Pos1, MoreBs).

%% corpus_monotonicity(-Summary)
%  Aggregate monotonicity and boundary statistics across the corpus.
corpus_monotonicity(Summary) :-
    ensure_corpus_computed,
    findall(C, cached_obstruction(C, _, _), AllCs),
    classify_all_monotonicity(AllCs, 0, 0, 0, 0, 0, NConst, NAsc, NDesc, NNon, NInc),
    count_boundary_positions(AllCs, N1, N2, N3),
    Summary = monotonicity_summary(
        constant(NConst),
        monotone_ascending(NAsc),
        monotone_descending(NDesc),
        non_monotone(NNon),
        incomparable(NInc),
        boundary_distribution([pos(1, N1), pos(2, N2), pos(3, N3)])
    ).

classify_all_monotonicity([], NC, NA, ND, NN, NI, NC, NA, ND, NN, NI).
classify_all_monotonicity([C | Rest], NC0, NA0, ND0, NN0, NI0, NC, NA, ND, NN, NI) :-
    orbit_monotonicity(C, Status),
    (   Status = constant(_)        -> NC1 is NC0+1, NA1=NA0, ND1=ND0, NN1=NN0, NI1=NI0
    ;   Status = monotone_ascending -> NC1=NC0, NA1 is NA0+1, ND1=ND0, NN1=NN0, NI1=NI0
    ;   Status = monotone_descending-> NC1=NC0, NA1=NA0, ND1 is ND0+1, NN1=NN0, NI1=NI0
    ;   Status = non_monotone(_)    -> NC1=NC0, NA1=NA0, ND1=ND0, NN1 is NN0+1, NI1=NI0
    ;   /* incomparable */             NC1=NC0, NA1=NA0, ND1=ND0, NN1=NN0, NI1 is NI0+1
    ),
    classify_all_monotonicity(Rest, NC1, NA1, ND1, NN1, NI1, NC, NA, ND, NN, NI).

count_boundary_positions(Constraints, N1, N2, N3) :-
    findall(Pos,
        (   member(C, Constraints),
            transition_boundaries(C, Bs),
            member(boundary(Pos, _, _), Bs)
        ),
        AllPositions),
    include(=(1), AllPositions, P1s), length(P1s, N1),
    include(=(2), AllPositions, P2s), length(P2s, N2),
    include(=(3), AllPositions, P3s), length(P3s, N3).

/* ================================================================
   INCOMPARABLE ORBIT DECOMPOSITION
   155 constraints (14.8%) have orbits with types outside the
   extraction chain. This section decomposes them by mechanism,
   position, and residual in-chain monotonicity.
   ================================================================ */

%% incomparable_decomposition(-Summary)
%  Structured breakdown of incomparable orbits by OOC type,
%  position, H1 distribution, and in-chain monotonicity.
incomparable_decomposition(Summary) :-
    ensure_corpus_computed,
    findall(C,
        (cached_obstruction(C, _, _),
         once(orbit_monotonicity(C, Status)),
         Status == incomparable),
        IncompCs),
    length(IncompCs, NTotal),
    count_ooc_by_type(IncompCs, naturalized, NNat),
    count_ooc_by_type(IncompCs, scaffold, NScaff),
    count_ooc_by_type(IncompCs, piton, NPiton),
    count_ooc_by_position(IncompCs, N1, N2, N3, N4),
    incomparable_h1_dist(IncompCs, H1Dist),
    in_chain_monotonicity(IncompCs, ICMSummary),
    Summary = incomparable_decomposition(
        total(NTotal),
        by_type([naturalized-NNat, scaffold-NScaff, piton-NPiton]),
        by_position([u1-N1, u2-N2, u3-N3, u4-N4]),
        h1_distribution(H1Dist),
        in_chain_monotonicity(ICMSummary)
    ).

%% count_ooc_by_type(+Constraints, +Type, -Count)
%  Count constraints whose orbit vector contains the given OOC type.
count_ooc_by_type(Constraints, Type, Count) :-
    include(has_ooc_type(Type), Constraints, Matching),
    length(Matching, Count).

has_ooc_type(Type, C) :-
    orbit_vector(C, Vec),
    member(Type, Vec).

%% count_ooc_by_position(+Constraints, -N1, -N2, -N3, -N4)
%  Count OOC occurrences at each of the 4 canonical positions.
count_ooc_by_position(Constraints, N1, N2, N3, N4) :-
    foldl(acc_ooc_positions, Constraints, 0-0-0-0, N1-N2-N3-N4).

acc_ooc_positions(C, A1-A2-A3-A4, B1-B2-B3-B4) :-
    orbit_vector(C, [T1, T2, T3, T4]),
    (is_ooc(T1) -> B1 is A1+1 ; B1 = A1),
    (is_ooc(T2) -> B2 is A2+1 ; B2 = A2),
    (is_ooc(T3) -> B3 is A3+1 ; B3 = A3),
    (is_ooc(T4) -> B4 is A4+1 ; B4 = A4).

%% is_ooc(+Type)
%  True if Type is outside the extraction chain. Deterministic.
is_ooc(Type) :-
    once(extraction_rank(Type, R)),
    R =:= -1.

%% det_extraction_rank(+Type, -Rank)
%  Deterministic wrapper around extraction_rank/2.
det_extraction_rank(Type, Rank) :-
    once(extraction_rank(Type, Rank)).

%% incomparable_h1_dist(+Constraints, -Distribution)
%  H1 frequency distribution for incomparable subset.
%  Returns sorted list of H1Value-Count pairs.
incomparable_h1_dist(Constraints, Dist) :-
    findall(H1,
        (member(C, Constraints), cached_obstruction(C, _, H1)),
        AllH1),
    msort(AllH1, Sorted),
    clumped(Sorted, Dist).

%% in_chain_monotonicity(+Constraints, -Summary)
%  For each incomparable constraint, mask OOC positions and classify
%  the remaining in-chain ranks for monotonicity.
%  Summary = icm_summary(ascending(Na), descending(Nd),
%                        non_monotone(Nn), too_few(Nt))
in_chain_monotonicity(Constraints, icm_summary(ascending(Na), descending(Nd),
                                               non_monotone(Nn), too_few(Nt))) :-
    foldl(acc_icm, Constraints, 0-0-0-0, Na-Nd-Nn-Nt).

acc_icm(C, A-D-N-F, A2-D2-N2-F2) :-
    orbit_vector(C, Vec),
    maplist(det_extraction_rank, Vec, Ranks),
    include(\=(-1), Ranks, InChainRanks),
    length(InChainRanks, Len),
    (   Len >= 3
    ->  adjacent_deltas(InChainRanks, Deltas),
        (   classify_deltas(Deltas, monotone_ascending)
        ->  A2 is A+1, D2=D, N2=N, F2=F
        ;   classify_deltas(Deltas, monotone_descending)
        ->  A2=A, D2 is D+1, N2=N, F2=F
        ;   A2=A, D2=D, N2 is N+1, F2=F
        )
    ;   A2=A, D2=D, N2=N, F2 is F+1
    ).

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

    % Power-chain monotonicity
    format('~n--- Power-Chain Monotonicity ---~n'),
    corpus_monotonicity(MonoSummary),
    MonoSummary = monotonicity_summary(
        constant(MConst),
        monotone_ascending(MAsc),
        monotone_descending(MDesc),
        non_monotone(MNon),
        incomparable(MInc),
        boundary_distribution([pos(1, B1), pos(2, B2), pos(3, B3)])
    ),
    format('  Constant (H0=1):      ~w~n', [MConst]),
    format('  Monotone ascending:   ~w~n', [MAsc]),
    format('  Monotone descending:  ~w~n', [MDesc]),
    format('  Non-monotone:         ~w~n', [MNon]),
    format('  Incomparable:         ~w~n', [MInc]),
    MTotal is MConst + MAsc + MDesc + MNon + MInc,
    format('  Total (cross-check):  ~w~n', [MTotal]),
    format('~n  Boundary distribution (where type changes occur):~n'),
    format('    Pos 1 (U1->U2, powerless->moderate):        ~w~n', [B1]),
    format('    Pos 2 (U2->U3, moderate->institutional):    ~w~n', [B2]),
    format('    Pos 3 (U3->U4, institutional->analytical):  ~w~n', [B3]),

    % Sample orbits for each monotonicity category
    format('~n  Sample orbits:~n'),
    findall(C, cached_obstruction(C, _, _), AllCached),
    forall(
        member(Cat, [monotone_ascending, monotone_descending]),
        (   (   member(SC, AllCached),
                orbit_monotonicity(SC, Cat)
            ->  orbit_vector(SC, SOV),
                format('    ~w: ~w  orbit=~w~n', [Cat, SC, SOV])
            ;   format('    ~w: (none)~n', [Cat])
            )
        )
    ),
    (   member(NMC, AllCached),
        orbit_monotonicity(NMC, non_monotone(_))
    ->  orbit_vector(NMC, NMOV),
        format('    non_monotone: ~w  orbit=~w~n', [NMC, NMOV])
    ;   format('    non_monotone: (none)~n')
    ),

    % Incomparable orbit decomposition
    format('~n--- Incomparable Orbit Decomposition ---~n'),
    incomparable_decomposition(IncompSummary),
    IncompSummary = incomparable_decomposition(
        total(ITotal),
        by_type(TypeBreakdown),
        by_position(PosBreakdown),
        h1_distribution(IH1Dist),
        in_chain_monotonicity(icm_summary(
            ascending(ICMAsc), descending(ICMDesc),
            non_monotone(ICMNon), too_few(ICMFew)))
    ),
    format('  Total incomparable: ~w~n', [ITotal]),
    format('~n  By OOC type:~n'),
    forall(member(T-Cnt, TypeBreakdown),
           format('    ~w: ~w~n', [T, Cnt])),
    format('~n  By position (OOC count at each U):~n'),
    forall(member(U-Cnt, PosBreakdown),
           format('    ~w: ~w~n', [U, Cnt])),
    format('~n  H1 distribution (incomparable only):~n'),
    forall(member(H1V-H1Cnt, IH1Dist),
           format('    H1=~w: ~w~n', [H1V, H1Cnt])),
    format('~n  In-chain monotonicity (OOC positions masked):~n'),
    format('    ascending:   ~w~n', [ICMAsc]),
    format('    descending:  ~w~n', [ICMDesc]),
    format('    non_monotone: ~w~n', [ICMNon]),
    format('    too_few:     ~w~n', [ICMFew]),

    format('~n=== Selftest Complete ===~n').
