% ============================================================================
% SHEAF ANALYSIS — Three-regime partition and block consistency (v1.0)
% ============================================================================
% Implements the two core predicates that compose existing cohomological and
% Arakelov machinery into higher-level diagnostic predicates.
%
% sheaf_status/2: Three-regime partition (genuine_sheaf / fragile_presheaf /
%   manifest_presheaf) based on H¹ and Arakelov height. The binary distinction
%   (H¹=0 vs H¹>0) is site-invariant; the genuine/fragile split is
%   height-dependent and may vary between sites.
%
% block_consistency/2: Checks that the product-site orbit is internally
%   constant within each power-level block. The 3,301-constraint product-site
%   run found 100% block consistency. This predicate monitors for exceptions
%   as the corpus grows or axioms change.
%
% This module is DIAGNOSTIC ONLY. It does not modify any classification,
% purity score, or existing pipeline output.
% ============================================================================

:- module(sheaf_analysis, [
    sheaf_status/2,       % sheaf_status(C, Status)
    block_consistency/2   % block_consistency(C, Result)
]).

:- use_module(grothendieck_cohomology).
:- use_module(arakelov_height).
:- use_module(constraint_indexing).
:- use_module(drl_core).
:- use_module(library(pairs)).

/* ================================================================
   SHEAF STATUS — Three-regime partition
   ================================================================ */

%% sheaf_status(+Constraint, -Status) is det.
%% Classifies a constraint into one of three regimes based on
%% cohomological obstruction and Arakelov height.
%%
%% Status is one of:
%%   genuine_sheaf    — H¹ = 0, Arakelov height below corpus threshold
%%   fragile_presheaf — H¹ = 0, Arakelov height above corpus threshold
%%   manifest_presheaf — H¹ > 0
%%
%% The binary distinction (genuine_sheaf + fragile_presheaf vs. manifest_presheaf)
%% is site-invariant: it produces the same result on the 4-point canonical site
%% and the 156-point product site (confirmed with zero crossings across 3,301
%% constraints). sheaf_status/2 runs on whichever site site_contexts/1 returns.
%%
%% The fragile/genuine sub-partition depends on Arakelov height, which is
%% site-dependent (heights can only increase with more contexts). For the
%% fragile/genuine distinction, use the site appropriate to the analysis.

sheaf_status(C, manifest_presheaf) :-
    grothendieck_cohomology:cohomological_obstruction(C, _, H1),
    H1 > 0, !.
sheaf_status(C, fragile_presheaf) :-
    grothendieck_cohomology:cohomological_obstruction(C, _, 0),
    arakelov_height:arakelov_height(C, H),
    arakelov_height:arakelov_threshold(Thresh),
    H > Thresh, !.
sheaf_status(C, genuine_sheaf) :-
    grothendieck_cohomology:cohomological_obstruction(C, _, 0).

/* ================================================================
   BLOCK CONSISTENCY — Product-site power-level invariant
   ================================================================ */

%% block_consistency(+Constraint, -Result) is det.
%% Checks whether the product-site orbit is internally constant
%% within each power-level block.
%%
%% Result is one of:
%%   all_constant — every power-level block assigns a single type
%%   mixed(Details) — at least one block has internal variation
%%     Details = list of P-constant or P-mixed(Types)
%%
%% The product-site run found 100% block consistency across the corpus.
%% This predicate monitors for future exceptions as the corpus grows
%% or axioms change. A mixed result means a non-power axis is crossing
%% a classification threshold — the per-axis decomposition (Phase 3)
%% was designed to find.
%%
%% NOTE: Always runs on the product site (calls site_contexts_product/1
%% directly, not site_contexts/1). Checking block consistency on the
%% 4-point canonical site is meaningless — each power level has exactly
%% one context there.

block_consistency(C, Result) :-
    constraint_indexing:site_contexts_product(Contexts),
    findall(P-Type,
        (   member(Ctx, Contexts),
            Ctx = context(agent_power(P), _, _, _),
            drl_core:dr_type(C, Ctx, Type)
        ),
        PairList),
    keysort(PairList, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(assess_block, Grouped, BlockResults),
    (   \+ member(_-mixed(_), BlockResults)
    ->  Result = all_constant
    ;   Result = mixed(BlockResults)
    ).

%% assess_block(+P-Types, -P-Status)
%  Determines if all contexts in a power-level block agree on type.
assess_block(P-Types, P-Status) :-
    sort(Types, Unique),
    (   Unique = [_]
    ->  Status = constant
    ;   Status = mixed(Unique)
    ).
