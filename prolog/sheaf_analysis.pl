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
    sheaf_status/2,              % sheaf_status(C, Status)
    sheaf_undetermined_reason/2, % sheaf_undetermined_reason(C, Reason)
    block_consistency/2          % block_consistency(C, Result)
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
%% Classifies a constraint into one of FOUR regimes based on cohomological
%% obstruction and Arakelov height.
%%
%% Status is one of:
%%   genuine_sheaf    — H¹ = 0, Arakelov height below corpus threshold
%%   fragile_presheaf — H¹ = 0, Arakelov height above corpus threshold
%%   manifest_presheaf — H¹ > 0
%%   undetermined     — N/A (OQ-51): EITHER <2 real seats (H¹ = null, the
%%                      obstruction cannot be computed) OR H¹ = 0 but the
%%                      Arakelov height that distinguishes genuine from fragile
%%                      is uncomputable (unauthored ε / missing MaxEnt). NOT
%%                      genuine_sheaf by absence — that was the Pattern-5 trap.
%%
%% The two undetermined ROUTES carry distinct provenance and resolve under
%% different added data (more real seats vs. an authored height); the sibling
%% predicate sheaf_undetermined_reason/2 names which. Do not collapse them.
%%
%% The binary distinction (genuine_sheaf + fragile_presheaf vs. manifest_presheaf)
%% is site-invariant: it produces the same result on the 4-point canonical site
%% and the 156-point product site (confirmed with zero crossings across 3,301
%% constraints). sheaf_status/2 runs on whichever site site_contexts/1 returns.
%%
%% The fragile/genuine sub-partition depends on Arakelov height, which is
%% site-dependent (heights can only increase with more contexts). For the
%% fragile/genuine distinction, use the site appropriate to the analysis.

sheaf_status(C, Status) :-
    grothendieck_cohomology:cohomological_obstruction(C, _, H1),
    (   \+ number(H1)               % H¹ = null → route 1: <2 real seats (N/A)
    ->  Status = undetermined
    ;   H1 > 0
    ->  Status = manifest_presheaf
    ;   % H¹ = 0: genuine vs fragile — but only if the height is computable.
        (   arakelov_height:arakelov_height(C, H)
        ->  (   arakelov_height:arakelov_threshold(Thresh), H > Thresh
            ->  Status = fragile_presheaf
            ;   Status = genuine_sheaf
            )
        ;   Status = undetermined   % route 2: uncomputable height (NOT genuine by absence)
        )
    ).

%% sheaf_undetermined_reason(+Constraint, -Reason) is semidet.
%% Names WHICH undetermined route a constraint took; FAILS when the constraint
%% is determined (genuine/fragile/manifest) so json_report serializes null.
%%   insufficient_seats  — <2 real seats; H¹ = null (route 1)
%%   uncomputable_height — H¹ = 0 but arakelov_height/2 fails (route 2)
%% Carries the provenance bit so the two N/A causes (resolved by seats vs by ε)
%% stay distinguishable in the output — the carry-the-provenance-bit spine.
sheaf_undetermined_reason(C, Reason) :-
    grothendieck_cohomology:cohomological_obstruction(C, _, H1),
    (   \+ number(H1)
    ->  Reason = insufficient_seats
    ;   H1 =:= 0,
        \+ arakelov_height:arakelov_height(C, _)
    ->  Reason = uncomputable_height
    ).

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
