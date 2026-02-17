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

   Interpretation:
     1.0  = perfectly pure (all tests pass, no contamination)
     >0.8 = structurally sound (minor contamination tolerable)
     0.5  = borderline (significant contamination, drift risk)
     <0.3 = contaminated (multiple structural failures)
     -1.0 = inconclusive (insufficient epistemic data)

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
    RawScore is 0.30 * F + 0.25 * SI + 0.25 * CC + 0.20 * EX,
    Score is min(1.0, max(0.0, RawScore)).
purity_score(_, -1.0).  % Sentinel for insufficient epistemic data

%% factorization_subscore(+C, -F)
%  1.0 if Boltzmann-compliant. Decays with coupling score.
factorization_subscore(C, F) :-
    (   cross_index_coupling(C, CouplingScore)
    ->  F is max(0.0, 1.0 - CouplingScore)
    ;   F = 0.5  % Neutral if no data
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
    ;   SI = 0.5
    ).

%% coupling_cleanliness_subscore(+C, -CC)
%  1.0 if no nonsensical coupling. Decays with coupling strength.
coupling_cleanliness_subscore(C, CC) :-
    (   detect_nonsensical_coupling(C, Pairs, Strength),
        Pairs \= []
    ->  CC is max(0.0, 1.0 - Strength)
    ;   CC = 1.0  % No coupling = clean
    ).

%% excess_extraction_subscore(+C, -EX)
%  1.0 if no excess extraction. Decays: excess of 0.5 → score 0.0.
excess_extraction_subscore(C, EX) :-
    (   excess_extraction(C, Excess)
    ->  EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))
    ;   EX = 1.0  % No extraction data = clean
    ).
