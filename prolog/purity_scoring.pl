:- module(purity_scoring, [
    purity_score/2,
    factorization_subscore/2,
    scope_invariance_subscore/2,
    coupling_cleanliness_subscore/2,
    excess_extraction_subscore/2
]).

:- use_module(boltzmann_compliance).

/* ================================================================
   PURITY SCORE — v5.1
   ================================================================
   Combines the four Boltzmann structural tests into a single
   scalar in [0, 1]:

     purity_score(C) = 0.30 × factorization
                     + 0.25 × scope_invariance
                     + 0.25 × coupling_cleanliness
                     + 0.20 × (1 - excess_extraction)

   Interpretation (canonical zones — logic_extensions.md §2.3 / purity_zone/2):
     >=0.9 = pristine     (exemplary coordination)
     >=0.7 = sound        (healthy coordination)
     >=0.5 = borderline   (acceptable but watch)
     >=0.3 = contaminated (degrading coordination)
     <0.3  = degraded     (extraction-dominant; reform blocked)

   Two ABSENCE tokens, distinct causes, both serialising to JSON null (OQ-60):
     -1.0    = epistemic-gate-fail sentinel (epistemic_access_check/2 said false)
     unknown = no-data (no authored coordination_type, so no Boltzmann floor)
   Neither is a purity VALUE: never coerce, average, or .get(...,0) them. Note
   `unknown` is an atom and atoms sort BEFORE numbers — guard number/1 before any
   sort/max over purity. Banders fail closed to zone `unknown` on both (OQ-62).

   Use cases:
     - Rank coordination mechanisms by structural soundness
     - Detect drift toward impurity (purity_score decreasing)
     - Compare constraints across domains
     - Identify fragile ropes: purity ∈ [0.5, 0.7] →
       one drift event from tangled_rope
     - Integrate into fingerprint coupling dimension
   ================================================================ */

% Categorical: Naturality health scalar — weighted composite of four naturality test subscores
%% purity_score(+Constraint, -Score)
%  Computes scalar purity score. Returns -1.0 for insufficient data.
purity_score(C, Score) :-
    epistemic_access_check(C, true),
    !,
    factorization_subscore(C, F),
    scope_invariance_subscore(C, SI),
    coupling_cleanliness_subscore(C, CC),
    excess_extraction_subscore(C, EX),
    % OQ-60: a subscore may report `unknown` (no-data) once its producer commit
    % lands. No-data is not perfection — propagate `unknown` rather than feeding
    % it to the weighted sum (which would throw). Distinct from the -1.0
    % epistemic-gate-fail sentinel below (short-circuited by the cut above).
    % Inert until a producer emits `unknown` (Commit 0a is byte-identical).
    (   ( F == unknown ; SI == unknown ; CC == unknown ; EX == unknown )
    ->  Score = unknown
    ;   RawScore is 0.30 * F + 0.25 * SI + 0.25 * CC + 0.20 * EX,
        Score is min(1.0, max(0.0, RawScore))
    ).
purity_score(_, -1.0).  % Sentinel for insufficient epistemic data

%% factorization_subscore(+C, -F)
%  1.0 if Boltzmann-compliant. Decays with coupling score.
factorization_subscore(C, F) :-
    (   cross_index_coupling(C, CouplingScore)
    ->  F is max(0.0, 1.0 - CouplingScore)
    ;   F = unknown  % OQ-60 mech 2: no coupling grid = no data, not a 0.5 neutral
    ).

%% scope_invariance_subscore(+C, -SI)
%  1.0 if scope-invariant. Penalized per extra classification type.
scope_invariance_subscore(C, SI) :-
    scope_invariance_test(C, Result),
    (   Result = invariant
    ->  SI = 1.0
    ;   Result = variant(Types)
    ->  length(Types, N),
        % Penalize 0.25 per extra type beyond unity
        SI is max(0.0, 1.0 - (N - 1) * 0.25)
    ;   Result = no_data
    ->  SI = unknown  % OQ-60 mech 1: empty type list = no data (was variant([]) → 1.25)
    ;   SI = 0.5
    ).

%% coupling_cleanliness_subscore(+C, -CC)
%  1.0 if no nonsensical coupling. Decays with coupling strength.
coupling_cleanliness_subscore(C, CC) :-
    % OQ-60 mech 3: detect_nonsensical_coupling now FAILS on an empty grid
    % (no data) — distinguish that from a measured grid with no coupled pairs.
    (   detect_nonsensical_coupling(C, Pairs, Strength)
    ->  (   Pairs \= []
        ->  CC is max(0.0, 1.0 - Strength)
        ;   CC = 1.0  % Measured grid, no coupling = clean
        )
    ;   CC = unknown  % No grid = no data, not clean
    ).

%% excess_extraction_subscore(+C, -EX)
%  1.0 if no excess extraction. Decays: excess of 0.5 → score 0.0.
excess_extraction_subscore(C, EX) :-
    (   excess_extraction(C, Excess)
    ->  EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))
    ;   EX = unknown  % OQ-60 mech 4: no extraction data = no data, not clean
    ).
