% ============================================================================
% DRL FPN — Stage 8b: Fixed-Point Network Iteration (v5.3)
% Split from drl_modal_logic.pl (v5.3)
% ============================================================================

:- module(drl_fpn, [
    % Fixed-Point Network Iteration
    fpn_run/2,
    fpn_run/3,
    fpn_effective_purity/4,
    fpn_effective_purity/3,
    fpn_cleanup/1,
    states_equivalent/3,

    % Dynamic state — exported for external readers (fpn_report, abductive_engine)
    fpn_ep/3,
    fpn_intrinsic/2,
    fpn_iteration_info/4
]).

:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(drl_purity_network).

% Dynamic state for fixed-point iteration
:- dynamic fpn_ep/3.                % fpn_ep(Constraint, Context, EffPurity)
:- dynamic fpn_intrinsic/2.         % fpn_intrinsic(Constraint, IntrinsicPurity)
:- dynamic fpn_type_cache/3.        % fpn_type_cache(Constraint, Context, Type)
:- dynamic fpn_neighbors_cache/3.   % fpn_neighbors_cache(Constraint, Context, NeighborList)
:- dynamic fpn_iteration_info/4.    % fpn_iteration_info(Context, Iterations, MaxDelta, Converged)

/* ================================================================
   STAGE 8b: FIXED-POINT NETWORK ITERATION (v5.3)

   Extends Stage 8's one-hop purity propagation to multi-hop convergence.
   The single semantic change: neighbor purity reads from the previous
   iteration's effective purity (fpn_ep/3) instead of intrinsic purity
   (purity_score/2). Everything else is structural plumbing for the
   iterative loop.

   Convergence guarantee: The sequence {EP_k(C)} is monotonically
   non-increasing and bounded below by 0.0, so it converges by the
   monotone convergence theorem. No damping factor is needed.

   State vector: fpn_ep(C, Context, EP) — only effective purities vary.
   All other quantities (type, immunity, neighbors, intrinsic purity)
   are invariant across iterations and pre-cached.

   Activation: param(fpn_enabled, 1) to enable. Default is 0 (one-hop).
   ================================================================ */

/* ----------------------------------------------------------------
   8b-A. CLEANUP — Retract all FPN state for a context
   ---------------------------------------------------------------- */

%% fpn_cleanup(+Context)
%  Retracts all fpn_* dynamic facts for the given Context.
%  Also retracts context-free caches (fpn_intrinsic/2) since they are
%  populated per-run and may be stale from a prior corpus load.
fpn_cleanup(Context) :-
    retractall(fpn_ep(_, Context, _)),
    retractall(fpn_type_cache(_, Context, _)),
    retractall(fpn_neighbors_cache(_, Context, _)),
    retractall(fpn_iteration_info(Context, _, _, _)),
    retractall(fpn_intrinsic(_, _)).

/* ----------------------------------------------------------------
   8b-B. PRECOMPUTE — Cache iteration-invariant quantities
   ---------------------------------------------------------------- */

%% fpn_precompute(+Constraints, +Context)
%  Pre-computes and caches all iteration-invariant data:
%  - Intrinsic purity (from purity_scoring:purity_score/2)
%  - Classification type (from drl_core:dr_type/3)
%  - Neighbor graph (from constraint_neighbors/3)
%  - Initial effective purity = intrinsic purity (iteration 0)
fpn_precompute(Constraints, Context) :-
    fpn_precompute_constraints(Constraints, Context).

fpn_precompute_constraints([], _).
fpn_precompute_constraints([C|Cs], Context) :-
    % Cache intrinsic purity
    (   purity_scoring:purity_score(C, IP)
    ->  true
    ;   IP = -1.0
    ),
    assertz(fpn_intrinsic(C, IP)),
    % Cache type
    (   drl_core:dr_type(C, Context, Type)
    ->  assertz(fpn_type_cache(C, Context, Type))
    ;   true
    ),
    % Cache neighbors
    (   drl_purity_network:constraint_neighbors(C, Context, Neighbors)
    ->  assertz(fpn_neighbors_cache(C, Context, Neighbors))
    ;   assertz(fpn_neighbors_cache(C, Context, []))
    ),
    % Initialize EP to intrinsic purity (iteration 0)
    assertz(fpn_ep(C, Context, IP)),
    fpn_precompute_constraints(Cs, Context).

/* ----------------------------------------------------------------
   8b-C. MAIN ENTRY POINTS
   ---------------------------------------------------------------- */

%% fpn_run(+Context, -Result)
%  Convenience: discovers all constraints and delegates to fpn_run/3.
fpn_run(Context, Result) :-
    constraint_indexing:valid_context(Context),
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    fpn_run(Constraints, Context, Result).

%% fpn_run(+Constraints, +Context, -Result)
%  Main entry point for the fixed-point network iterator.
%  Result = fpn_result(Iterations, MaxDelta, Converged, ConstraintCount)
fpn_run(Constraints, Context, fpn_result(Iterations, MaxDelta, Converged, NConstraints)) :-
    constraint_indexing:valid_context(Context),
    fpn_cleanup(Context),
    fpn_precompute(Constraints, Context),
    length(Constraints, NConstraints),
    config:param(fpn_epsilon, Eps),
    config:param(fpn_max_iterations, MaxIter),
    fpn_iterate(Constraints, Context, 1, MaxIter, Eps),
    fpn_iteration_info(Context, Iterations, MaxDelta, Converged).

/* ----------------------------------------------------------------
   8b-D. ITERATION LOOP (Jacobi-style simultaneous update)
   ---------------------------------------------------------------- */

% Categorical: Greatest fixed point computation — Jacobi iteration converging to equilibrium of contamination endofunctor
%% fpn_iterate(+Constraints, +Context, +K, +MaxIter, +Eps)
%  Recursive iteration loop. Each iteration:
%  1. Computes all new EPs from current state (Jacobi: read old, write new)
%  2. Batch-updates all EPs and computes max residual
%  3. Checks convergence or iteration cap
fpn_iterate(Constraints, Context, K, MaxIter, Eps) :-
    % Step 1: Compute all new EPs from current state
    findall(C-NewEP, (
        member(C, Constraints),
        fpn_compute_ep(C, Context, NewEP)
    ), NewValues),
    % Step 2: Batch update and compute max residual
    fpn_jacobi_update(NewValues, Context, MaxDelta),
    % Step 3: Check convergence
    (   MaxDelta < Eps
    ->  assertz(fpn_iteration_info(Context, K, MaxDelta, true))
    ;   K >= MaxIter
    ->  assertz(fpn_iteration_info(Context, K, MaxDelta, false))
    ;   K1 is K + 1,
        fpn_iterate(Constraints, Context, K1, MaxIter, Eps)
    ).

/* ----------------------------------------------------------------
   8b-E. PER-CONSTRAINT EP COMPUTATION
   ---------------------------------------------------------------- */

%% fpn_compute_ep(+C, +Context, -NewEP)
%  Computes one constraint's effective purity from cached state.
%  Mirrors effective_purity/4 but reads neighbor EP from fpn_ep/3.
%  IMPORTANT: Single clause with if-then-else to ensure determinism.
%  findall/3 collects ALL solutions via backtracking, so multiple clauses
%  would produce duplicate entries that corrupt the Jacobi update.
fpn_compute_ep(C, Context, NewEP) :-
    (   fpn_intrinsic(C, IP)
    ->  true
    ;   IP = -1.0
    ),
    (   IP < 0.0
    ->  NewEP = IP                     % Sentinel: no purity data
    ;   (   fpn_neighbors_cache(C, Context, Neighbors)
        ->  true
        ;   Neighbors = []
        ),
        (   Neighbors = []
        ->  NewEP = IP                 % Isolated: no contamination
        ;   (   fpn_type_cache(C, Context, Type)
            ->  drl_purity_network:type_immunity(Type, Immunity)
            ;   Immunity = 0.5         % Default immunity if no type cached
            ),
            fpn_total_contamination(C, IP, Neighbors, Context, TotalContam),
            RawEP is IP - Immunity * TotalContam,
            NewEP is max(0.0, RawEP)
        )
    ).

%% fpn_total_contamination(+C, +MyPurity, +Neighbors, +Context, -Total)
%  Sums contamination from all neighbors, reading their EP from fpn_ep/3.
%  Mirrors compute_total_contamination/6 structurally.
fpn_total_contamination(_, _, [], _, 0.0).
fpn_total_contamination(C, MyPurity, [neighbor(Other, EdgeStrength, _Src)|Rest], Context, Total) :-
    fpn_edge_contamination(MyPurity, Other, EdgeStrength, Context, EdgeContam),
    fpn_total_contamination(C, MyPurity, Rest, Context, RestTotal),
    Total is EdgeContam + RestTotal.

%% fpn_edge_contamination(+MyPurity, +Other, +EdgeStrength, +Context, -Contam)
%  Computes contamination from one neighbor.
%  THE ONLY SEMANTIC CHANGE FROM STAGE 8: reads fpn_ep(Other, Context, OtherEP)
%  instead of purity_scoring:purity_score(Other, OtherPurity).
%  Everything else in Stage 8b exists to make this single substitution iterative.
fpn_edge_contamination(MyPurity, Other, EdgeStrength, Context, Contam) :-
    % === THE CORE CHANGE: read from iteration state, not intrinsic purity ===
    fpn_ep(Other, Context, OtherEP),
    !,
    (   OtherEP < 0.0
    ->  Contam = 0.0                   % Sentinel: no purity data for neighbor
    ;   Delta is max(0.0, MyPurity - OtherEP),
        (   Delta > 0.0
        ->  config:param(purity_attenuation_factor, AttFactor),
            config:param(purity_contamination_cap, Cap),
            (   fpn_type_cache(Other, Context, OtherType)
            ->  drl_purity_network:type_contamination_strength(OtherType, TypeFactor)
            ;   TypeFactor = 0.0
            ),
            Attenuation is EdgeStrength * AttFactor,
            RawContam is Delta * Attenuation * TypeFactor,
            Contam is min(Cap, RawContam)
        ;   Contam = 0.0
        )
    ).
% Fallback: neighbor not in FPN state. Read intrinsic purity from purity_score/2
% to match the one-hop model's behavior. These "external" neighbors are treated as
% constants (their EP never changes across iterations), which is correct since
% they aren't being iterated.
fpn_edge_contamination(MyPurity, Other, EdgeStrength, Context, Contam) :-
    purity_scoring:purity_score(Other, OtherPurity),
    OtherPurity >= 0.0,
    !,
    Delta is max(0.0, MyPurity - OtherPurity),
    (   Delta > 0.0
    ->  config:param(purity_attenuation_factor, AttFactor),
        config:param(purity_contamination_cap, Cap),
        (   drl_core:dr_type(Other, Context, OtherType)
        ->  drl_purity_network:type_contamination_strength(OtherType, TypeFactor)
        ;   TypeFactor = 0.0
        ),
        Attenuation is EdgeStrength * AttFactor,
        RawContam is Delta * Attenuation * TypeFactor,
        Contam is min(Cap, RawContam)
    ;   Contam = 0.0
    ).
fpn_edge_contamination(_, _, _, _, 0.0).

/* ----------------------------------------------------------------
   8b-F. JACOBI UPDATE — Batch state replacement
   ---------------------------------------------------------------- */

%% fpn_jacobi_update(+NewValues, +Context, -MaxDelta)
%  Atomically replaces all fpn_ep/3 facts and computes the maximum
%  absolute change (residual) across all constraints.
fpn_jacobi_update(NewValues, Context, MaxDelta) :-
    fpn_jacobi_update_acc(NewValues, Context, 0.0, MaxDelta).

fpn_jacobi_update_acc([], _, Acc, Acc).
fpn_jacobi_update_acc([C-NewEP|Rest], Context, Acc, MaxDelta) :-
    (   retract(fpn_ep(C, Context, OldEP))
    ->  Delta is abs(NewEP - OldEP)
    ;   Delta = 0.0
    ),
    assertz(fpn_ep(C, Context, NewEP)),
    NewAcc is max(Acc, Delta),
    fpn_jacobi_update_acc(Rest, Context, NewAcc, MaxDelta).

/* ----------------------------------------------------------------
   8b-G. PUBLIC API — fpn_effective_purity
   ---------------------------------------------------------------- */

%% fpn_effective_purity(+C, +Context, -EP, -Components)
%  Public API: returns fixed-point effective purity if FPN is enabled
%  and has been run; otherwise delegates to one-hop effective_purity/4.
%  Components = fpn_components(IP, TotalContam, Iterations, Converged)
%            | purity_components(...) (from one-hop fallback)
fpn_effective_purity(C, Context, EP, Components) :-
    config:param(fpn_enabled, 1),
    fpn_iteration_info(Context, Iters, _MaxDelta, Converged),
    !,
    (   fpn_ep(C, Context, EP)
    ->  fpn_intrinsic(C, IP),
        TotalContam is max(0.0, IP - EP),
        Components = fpn_components(IP, TotalContam, Iters, Converged)
    ;   % Constraint not in FPN state — fall through to one-hop
        drl_purity_network:effective_purity(C, Context, EP, Components)
    ).
fpn_effective_purity(C, Context, EP, Components) :-
    drl_purity_network:effective_purity(C, Context, EP, Components).

%% fpn_effective_purity(+C, -EP, -Components)
%  Backward compat: uses analytical context.
fpn_effective_purity(C, EP, Components) :-
    constraint_indexing:default_context(Ctx),
    fpn_effective_purity(C, Ctx, EP, Components).

/* ----------------------------------------------------------------
   8b-H. CONVERGENCE COMPARISON
   ---------------------------------------------------------------- */

%% states_equivalent(+State1, +State2, +Epsilon)
%  Succeeds if max |EP1(C) - EP2(C)| < Epsilon for all constraints.
%  State1, State2 are lists of C-EP pairs (sorted by constraint).
states_equivalent([], [], _).
states_equivalent([C-EP1|Rest1], [C-EP2|Rest2], Epsilon) :-
    Delta is abs(EP1 - EP2),
    Delta < Epsilon,
    states_equivalent(Rest1, Rest2, Epsilon).
