/* ============================================================================
   ARAKELOV HEIGHT — Boundary Complexity Diagnostic
   ============================================================================
   Combines base extractiveness (ε) with MaxEnt classifier uncertainty and
   conditional signature pressure to identify constraints where the engine
   genuinely cannot decide the type.

   Height = ε × (raw_uncertainty + conditional_pressure)

   - raw_uncertainty = 1 - |raw_confidence_margin|
   - conditional_pressure = |raw_margin - post_margin| for conditional overrides
                          = 0 for unconditional overrides (NL, FNL, CI_rope)

   Computed as max across all 4 canonical observer contexts (U1..U4).

   Lifecycle: Pipeline diagnostic only. Requires maxent_run/2 to have been called
   (populating maxent_dist/3 and maxent_dist_raw/3). Not available during
   individual testset execution.
   ============================================================================ */

:- module(arakelov_height, [
    arakelov_height/2,
    arakelov_height_context/2,
    arakelov_height_pair/3,
    raw_confidence_margin/3,
    post_confidence_margin/3,
    signature_pressure/3,
    high_complexity_constraint/1,
    arakelov_threshold/1
]).

:- use_module(maxent_classifier, [maxent_distribution_raw/3, maxent_distribution/3]).
:- use_module(constraint_data, [base_extractiveness/2]).
:- use_module(drl_core, [dr_type/3]).
:- use_module(measurement_layer, [wasserstein_contexts/1]).
:- use_module(signature_detection, [constraint_signature/2]).
:- use_module(logical_fingerprint, [known_constraint/1]).

/* ================================================================
   CONFIDENCE MARGIN PREDICATES
   ================================================================ */

%% raw_confidence_margin(+C, +Context, -Margin)
%  P(claimed_type) - P(rival_type) from the pre-override MaxEnt distribution.
%  Margin > 0 means the raw model agrees with the deterministic classification.
%  Margin < 0 means the raw model favors a rival type.
raw_confidence_margin(C, Context, Margin) :-
    maxent_classifier:maxent_distribution_raw(C, Context, Dist),
    drl_core:dr_type(C, Context, Claimed),
    member(Claimed-Conf, Dist),
    findall(P, (member(T-P, Dist), T \= Claimed), Rivals),
    max_list(Rivals, RivalP),
    Margin is Conf - RivalP.

%% post_confidence_margin(+C, +Context, -Margin)
%  Same as raw_confidence_margin but from the post-override distribution.
post_confidence_margin(C, Context, Margin) :-
    maxent_classifier:maxent_distribution(C, Context, Dist),
    drl_core:dr_type(C, Context, Claimed),
    member(Claimed-Conf, Dist),
    findall(P, (member(T-P, Dist), T \= Claimed), Rivals),
    max_list(Rivals, RivalP),
    Margin is Conf - RivalP.

/* ================================================================
   SIGNATURE PRESSURE
   ================================================================ */

%% signature_pressure(+C, +Context, -Pressure)
%  For conditional overrides (×3 boost): |raw_margin - post_margin|.
%  For unconditional overrides (set to 0.95): 0.0 (not informative).
signature_pressure(C, Context, Pressure) :-
    (   conditional_override(C)
    ->  raw_confidence_margin(C, Context, RM),
        post_confidence_margin(C, Context, PM),
        Pressure is abs(RM - PM)
    ;   Pressure = 0.0
    ).

%% conditional_override(+C)
%  True if the constraint has a conditional (×3 boost) signature override.
%  Unconditional overrides (natural_law, false_natural_law, coupling_invariant_rope)
%  set target probability to 0.95 regardless — their pressure is binary, not diagnostic.
conditional_override(C) :-
    signature_detection:constraint_signature(C, Sig),
    memberchk(Sig, [false_ci_rope,
                     coordination_scaffold,
                     constructed_low_extraction,
                     constructed_high_extraction,
                     constructed_constraint]).

/* ================================================================
   ARAKELOV HEIGHT — Core Predicates
   ================================================================ */

%% arakelov_height_pair(+C, -Height, -MaxContext)
%  Compute boundary complexity as max over all 4 canonical contexts.
%  Returns both the height value and the context that produced the maximum.
%  The max-context is a free diagnostic: tells you where boundary uncertainty lives.
arakelov_height_pair(C, Height, MaxContext) :-
    constraint_data:base_extractiveness(C, Eps),
    measurement_layer:wasserstein_contexts(Ctxs),
    findall(H-Ctx, (
        member(Ctx, Ctxs),
        raw_confidence_margin(C, Ctx, RawMargin),
        signature_pressure(C, Ctx, Pressure),
        RawUncertainty is 1.0 - abs(RawMargin),
        H is Eps * (RawUncertainty + Pressure)
    ), HeightCtxPairs),
    HeightCtxPairs \= [],
    max_member(Height-MaxContext, HeightCtxPairs).

%% arakelov_height(+C, -Height)
%  Boundary complexity scalar for constraint C.
arakelov_height(C, Height) :-
    arakelov_height_pair(C, Height, _).

%% arakelov_height_context(+C, -MaxContext)
%  Which observer context produces the maximum boundary complexity.
arakelov_height_context(C, MaxContext) :-
    arakelov_height_pair(C, _, MaxContext).

/* ================================================================
   THRESHOLD AND HIGH-COMPLEXITY FLAG
   ================================================================ */

%% arakelov_threshold(-Thresh)
%  p75 of non-trivial heights (H > 0.01), memoized via nb_getval/nb_setval.
arakelov_threshold(Thresh) :-
    (   catch(nb_getval(arakelov_threshold_cache, Thresh), _, fail)
    ->  true
    ;   compute_arakelov_threshold(Thresh),
        nb_setval(arakelov_threshold_cache, Thresh)
    ).

compute_arakelov_threshold(Thresh) :-
    findall(H, (
        logical_fingerprint:known_constraint(C),
        arakelov_height(C, H),
        H > 0.01
    ), Heights),
    msort(Heights, Sorted),
    length(Sorted, N),
    (   N > 0
    ->  P75Index is max(1, round(N * 0.75)),
        nth1(P75Index, Sorted, Thresh)
    ;   Thresh = 0.01
    ).

%% high_complexity_constraint(+C)
%  True if C's Arakelov height exceeds the corpus p75 threshold.
high_complexity_constraint(C) :-
    arakelov_height(C, H),
    arakelov_threshold(Thresh),
    H > Thresh.
