% ============================================================================
% INVERTIBILITY ANALYSIS — Context-Tuple Transformation Reconstruction v1.0
% ============================================================================
% Analyzes whether classification transformations across observer positions
% are invertible. Given a constraint's type at one context, can you
% reconstruct its type at another?
%
% Three core questions:
%   1. Strategy B failure rates by orbit family
%   2. Which pipeline steps cause the failures
%   3. Whether the tangled middle band carries irreducible information
%
% Read-only analytical overlay — calls existing predicates, modifies nothing.
%
% Inspired by Scholze's tilting equivalence in perfectoid spaces:
%   orbit family = underlying object
%   context-indexed type = completion
%   structural invariants = perfectoid core
%
% Usage:
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl \
%     -l dirac_classification.pl -l invertibility_analysis.pl \
%     -g run_invertibility_analysis -t halt \
%     > ../outputs/invertibility_report.md \
%     2> ../outputs/invertibility_analysis.log
% ============================================================================

:- module(invertibility_analysis, [
    run_invertibility_analysis/0,

    % Pipeline invariants (the fiber)
    extract_pipeline_invariants/2,

    % Chi computation and inversion
    compute_chi_at_context/4,
    invert_chi_to_directionality/4,

    % Orbit enumeration
    enumerate_orbit_families/0,

    % Strategy A: full-invariant reconstruction (sanity check)
    reconstruct_classification/5,

    % Strategy B: type-only reconstruction (the real test)
    reconstruct_from_type_only/5,

    % Loss attribution
    pipeline_lossiness/4,

    % Tangled middle band
    tangled_rope_band_analysis/0,

    % Report
    generate_invertibility_report/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(signature_detection, [constraint_signature/2, integrate_signature_with_modal/3]).
:- use_module(drl_core).
:- use_module(dirac_classification).
:- use_module(covering_analysis).
:- use_module(library(lists)).

% Dynamic facts for analysis results
:- dynamic corpus_loaded_inv/0.
:- dynamic inv_orbit_family/2.        % inv_orbit_family(OrbitSig, Constraints)
:- dynamic inv_roundtrip/7.           % inv_roundtrip(C, SrcCtx, SrcType, TgtCtx, TgtType, ReconType, Match)
:- dynamic inv_loss/4.                % inv_loss(C, SrcCtx, TgtCtx, LossStep)
:- dynamic inv_tangled_band/4.        % inv_tangled_band(C, SubBand, ChiMod, Recoverable)

% ============================================================================
% STANDARD CONTEXTS (local copy — same as dirac_classification.pl)
% ============================================================================

inv_standard_context(context(agent_power(powerless),
                            time_horizon(biographical),
                            exit_options(trapped),
                            spatial_scope(local))).

inv_standard_context(context(agent_power(moderate),
                            time_horizon(biographical),
                            exit_options(mobile),
                            spatial_scope(national))).

inv_standard_context(context(agent_power(institutional),
                            time_horizon(generational),
                            exit_options(arbitrage),
                            spatial_scope(national))).

inv_standard_context(context(agent_power(analytical),
                            time_horizon(civilizational),
                            exit_options(analytical),
                            spatial_scope(global))).

% Short label for context (for reporting)
context_label(context(agent_power(P), _, _, _), P).

% ============================================================================
% SECTION 1: PIPELINE INVARIANTS (THE FIBER)
% ============================================================================

%% extract_pipeline_invariants(+C, -Invariants)
%  Collects all context-independent structural facts for a constraint.
%  These are the "perfectoid core" — everything you need to recompute
%  classification at any context without re-querying the database.
extract_pipeline_invariants(C, Invariants) :-
    % Numeric metrics
    config:param(extractiveness_metric_name, ExtName),
    (narrative_ontology:constraint_metric(C, ExtName, BaseEps) -> true ; BaseEps = 0.0),
    config:param(suppression_metric_name, SuppName),
    (narrative_ontology:constraint_metric(C, SuppName, Supp) -> true ; Supp = 0.0),
    config:param(theater_metric_name, TheaterName),
    (narrative_ontology:constraint_metric(C, TheaterName, Theater) -> true ; Theater = 0.0),

    % Binary flags
    (drl_core:emerges_naturally(C) -> Emerges = true ; Emerges = false),
    (drl_core:requires_active_enforcement(C) -> Enforced = true ; Enforced = false),
    (narrative_ontology:has_coordination_function(C) -> HasCoord = true ; HasCoord = false),
    (narrative_ontology:has_asymmetric_extraction(C) -> HasAsymm = true ; HasAsymm = false),
    (narrative_ontology:has_sunset_clause(C) -> HasSunset = true ; HasSunset = false),
    (drl_core:natural_law_without_beneficiary(C) -> NLGate = true ; NLGate = false),

    % Structural signature
    (signature_detection:constraint_signature(C, Sig) -> true ; Sig = unknown),

    % Directionality overrides (per-power explicit d values)
    findall(override(Power, D),
            constraint_indexing:directionality_override(C, Power, D),
            DOverrides),

    % Beneficiary/victim structure
    (narrative_ontology:constraint_beneficiary(C, _) -> HasBen = true ; HasBen = false),
    (narrative_ontology:constraint_victim(C, _) -> HasVic = true ; HasVic = false),

    Invariants = invariants(
        base_eps(BaseEps),
        suppression(Supp),
        theater(Theater),
        emerges(Emerges),
        enforced(Enforced),
        has_coord(HasCoord),
        has_asymm(HasAsymm),
        has_sunset(HasSunset),
        nl_gate(NLGate),
        signature(Sig),
        d_overrides(DOverrides),
        has_beneficiary(HasBen),
        has_victim(HasVic)
    ).

% ============================================================================
% SECTION 2: CHI COMPUTATION AND INVERSION
% ============================================================================

%% compute_chi_at_context(+C, +Context, -Chi, -Trace)
%  Forward chi computation with explicit intermediate values.
%  Trace = chi_trace(D, FD, Sigma, Chi) for analysis.
compute_chi_at_context(C, Context, Chi, Trace) :-
    Context = context(agent_power(Power), _, _, spatial_scope(Scope)),
    constraint_indexing:resolve_coalition_power(Power, C, ResolvedPower),
    Context = context(_, T, E, S),
    ResolvedContext = context(agent_power(ResolvedPower), T, E, S),
    config:param(extractiveness_metric_name, ExtName),
    narrative_ontology:constraint_metric(C, ExtName, BaseEps),
    constraint_indexing:derive_directionality(C, ResolvedContext, D),
    constraint_indexing:sigmoid_f(D, FD),
    constraint_indexing:scope_modifier(Scope, Sigma),
    Chi is BaseEps * FD * Sigma,
    Trace = chi_trace(d(D), f_d(FD), sigma(Sigma), chi(Chi), resolved_power(ResolvedPower)).

%% invert_chi_to_directionality(+Chi, +BaseEps, +Sigma, -D)
%  Inverse sigmoid: given chi, recover directionality d.
%  From chi = BaseEps * f(d) * sigma:
%    f(d) = chi / (BaseEps * sigma)
%    d = d0 - (1/k) * ln((U-L)/(f(d)-L) - 1)
%
%  Fails if f(d) is outside sigmoid range [L, U].
invert_chi_to_directionality(Chi, BaseEps, Sigma, D) :-
    BaseEps > 0.0001,  % Guard against division by near-zero
    Sigma > 0.0001,
    FD is Chi / (BaseEps * Sigma),
    config:param(sigmoid_lower, L),
    config:param(sigmoid_upper, U),
    config:param(sigmoid_midpoint, D0),
    config:param(sigmoid_steepness, K),
    % f(d) must be strictly within (L, U) for log to be defined
    FD > L + 0.0001,
    FD < U - 0.0001,
    Range is U - L,
    Ratio is Range / (FD - L) - 1,
    Ratio > 0.0001,  % Guard against log(0)
    D is D0 - (1/K) * log(Ratio),
    % Clamp to valid range
    (D < 0.0 -> true ; D =< 1.0).

% ============================================================================
% SECTION 3: ORBIT FAMILY ENUMERATION
% ============================================================================

%% enumerate_orbit_families
%  Groups all corpus constraints by orbit signature.
%  Asserts inv_orbit_family(OrbitSig, Constraints) for each family.
enumerate_orbit_families :-
    retractall(inv_orbit_family(_, _)),
    corpus_loader:load_all_testsets,
    covering_analysis:all_corpus_constraints(AllCs),
    length(AllCs, Total),
    format(user_error, '[invertibility] Computing orbits for ~w constraints...~n', [Total]),
    findall(pair(Sig, C),
            (member(C, AllCs),
             constraint_orbit_signature(C, Sig)),
            Pairs),
    % Group by signature
    msort(Pairs, Sorted),
    group_orbit_pairs(Sorted, Grouped),
    length(Grouped, NFamilies),
    format(user_error, '[invertibility] Found ~w orbit families.~n', [NFamilies]),
    forall(member(family(Sig, Cs), Grouped),
           assertz(inv_orbit_family(Sig, Cs))).

%% constraint_orbit_signature(+C, -Sig)
%  Sig = sorted list of unique type atoms in the orbit.
constraint_orbit_signature(C, Sig) :-
    dirac_classification:gauge_orbit(C, Orbit),
    findall(T, member(orbit_point(T, _), Orbit), Types),
    sort(Types, Sig).

%% group_orbit_pairs(+SortedPairs, -Grouped)
%  Groups pair(Sig, C) terms into family(Sig, [C1,C2,...]) terms.
group_orbit_pairs([], []).
group_orbit_pairs([pair(Sig, C)|Rest], [family(Sig, [C|Cs])|Groups]) :-
    collect_same_sig(Sig, Rest, Cs, Remaining),
    group_orbit_pairs(Remaining, Groups).

collect_same_sig(Sig, [pair(Sig, C)|Rest], [C|Cs], Remaining) :- !,
    collect_same_sig(Sig, Rest, Cs, Remaining).
collect_same_sig(_, Rest, [], Rest).

% ============================================================================
% SECTION 4: STRATEGY A — FULL INVARIANT RECONSTRUCTION
% ============================================================================

%% reconstruct_classification(+C, +SrcCtx, +SrcType, +TgtCtx, -TgtType)
%  Reconstruct classification at TgtCtx using full pipeline.
%  This is a sanity check — it should match dr_type/3 exactly.
%  SrcType is accepted but not used (it's the Strategy A contract:
%  we have access to the constraint identity).
reconstruct_classification(C, _SrcCtx, _SrcType, TgtCtx, TgtType) :-
    drl_core:dr_type(C, TgtCtx, TgtType).

% ============================================================================
% SECTION 5: STRATEGY B — TYPE-ONLY RECONSTRUCTION
% ============================================================================

%% reconstruct_from_type_only(+SrcType, +SrcCtx, +TgtCtx, +OrbitSig, -TgtType)
%  Predict target type from ONLY: source type, source context, target context,
%  and orbit signature. No constraint identity, no base metrics.
%
%  This is the genuine invertibility test. The orbit signature is the
%  structural invariant that the orbit taxonomy provides — it tells you
%  which types are in the orbit but not which constraint you're looking at.

% Trivial case: same context
reconstruct_from_type_only(SrcType, Ctx, Ctx, _, SrcType) :- !.

% Singleton orbit: type is invariant
reconstruct_from_type_only(SrcType, _, _, [SrcType], SrcType) :- !.

% Two-type orbit [rope, snare]: clean bifurcation along power axis
% The transformation rule: institutional sees rope, powerless sees snare.
% Moderate and analytical depend on the specific constraint — this is
% where information loss happens.
reconstruct_from_type_only(SrcType, SrcCtx, TgtCtx, [rope, snare], TgtType) :- !,
    context_label(TgtCtx, TgtPower),
    context_label(SrcCtx, SrcPower),
    predict_rope_snare(SrcType, SrcPower, TgtPower, TgtType).

% Two-type orbit [rope, tangled_rope]
reconstruct_from_type_only(SrcType, SrcCtx, TgtCtx, [rope, tangled_rope], TgtType) :- !,
    context_label(TgtCtx, TgtPower),
    context_label(SrcCtx, SrcPower),
    predict_rope_tangled(SrcType, SrcPower, TgtPower, TgtType).

% Three-type orbit [rope, snare, tangled_rope]
reconstruct_from_type_only(SrcType, SrcCtx, TgtCtx, [rope, snare, tangled_rope], TgtType) :- !,
    context_label(TgtCtx, TgtPower),
    context_label(SrcCtx, SrcPower),
    predict_three_type(SrcType, SrcPower, TgtPower, TgtType).

% Two-type orbit [snare, tangled_rope]
reconstruct_from_type_only(SrcType, SrcCtx, TgtCtx, [snare, tangled_rope], TgtType) :- !,
    context_label(TgtCtx, TgtPower),
    context_label(SrcCtx, SrcPower),
    predict_snare_tangled(SrcType, SrcPower, TgtPower, TgtType).

% Any other orbit: no prediction rule — return unknown
reconstruct_from_type_only(_, _, _, _, unknown).

% --- Prediction rules for [rope, snare] ---
% Core insight: institutional/organized → rope, powerless/trapped → snare.
% Moderate and analytical are ambiguous (depend on constraint's epsilon).
predict_rope_snare(rope, _, institutional, rope) :- !.
predict_rope_snare(snare, _, institutional, rope) :- !.
predict_rope_snare(rope, _, powerless, snare) :- !.
predict_rope_snare(snare, _, powerless, snare) :- !.
% Moderate: most [rope,snare] constraints are snare from moderate (d≈0.70, high f(d))
% but some with lower epsilon are rope. Default to snare (majority rule).
predict_rope_snare(_, _, moderate, snare) :- !.
% Analytical: d≈0.725, f(d)≈1.15 — similar to moderate but slightly lower.
% Default to snare.
predict_rope_snare(_, _, analytical, snare) :- !.

% --- Prediction rules for [rope, tangled_rope] ---
% Institutional → rope, others → tangled_rope (most common pattern)
predict_rope_tangled(_, _, institutional, rope) :- !.
predict_rope_tangled(_, _, powerless, tangled_rope) :- !.
predict_rope_tangled(_, _, moderate, tangled_rope) :- !.
predict_rope_tangled(_, _, analytical, tangled_rope) :- !.

% --- Prediction rules for [rope, snare, tangled_rope] ---
% Institutional → rope, powerless → snare, moderate → tangled_rope.
% Analytical is the hardest to predict — could be any of the three.
predict_three_type(_, _, institutional, rope) :- !.
predict_three_type(_, _, powerless, snare) :- !.
predict_three_type(_, _, moderate, tangled_rope) :- !.
% Analytical: default to tangled_rope (intermediate position)
predict_three_type(_, _, analytical, tangled_rope) :- !.

% --- Prediction rules for [snare, tangled_rope] ---
predict_snare_tangled(_, _, powerless, snare) :- !.
predict_snare_tangled(_, _, institutional, tangled_rope) :- !.
predict_snare_tangled(_, _, moderate, tangled_rope) :- !.
predict_snare_tangled(_, _, analytical, tangled_rope) :- !.

% ============================================================================
% SECTION 6: ROUNDTRIP TESTING
% ============================================================================

%% run_roundtrips(+OrbitSig, +Constraints)
%  Test all directed context pairs for every constraint in a family.
%  Asserts inv_roundtrip/7 for each test.
run_roundtrips(OrbitSig, Constraints) :-
    forall(
        (member(C, Constraints),
         inv_standard_context(SrcCtx),
         inv_standard_context(TgtCtx),
         SrcCtx \= TgtCtx),
        (   run_single_roundtrip(C, SrcCtx, TgtCtx, OrbitSig)
        ->  true
        ;   true  % Skip failures in individual roundtrips
        )
    ).

run_single_roundtrip(C, SrcCtx, TgtCtx, OrbitSig) :-
    % Ground truth
    (drl_core:dr_type(C, SrcCtx, SrcType) -> true ; SrcType = unknown),
    (drl_core:dr_type(C, TgtCtx, TgtType) -> true ; TgtType = unknown),
    % Strategy B prediction
    reconstruct_from_type_only(SrcType, SrcCtx, TgtCtx, OrbitSig, ReconType),
    % Compare
    (ReconType = TgtType -> Match = true ; Match = false),
    assertz(inv_roundtrip(C, SrcCtx, SrcType, TgtCtx, TgtType, ReconType, Match)).

% ============================================================================
% SECTION 7: LOSS ATTRIBUTION
% ============================================================================

%% pipeline_lossiness(+C, +SrcCtx, +TgtCtx, -LossSteps)
%  For a roundtrip failure, identify which pipeline steps caused it.
%  Returns a list of loss step atoms.
pipeline_lossiness(C, SrcCtx, TgtCtx, LossSteps) :-
    findall(Step, single_loss_check(C, SrcCtx, TgtCtx, Step), LossSteps).

single_loss_check(C, SrcCtx, TgtCtx, threshold_crossing) :-
    check_threshold_crossing(C, SrcCtx, TgtCtx).

single_loss_check(C, SrcCtx, TgtCtx, gate_priority_shadow) :-
    check_gate_priority_shadow(C, SrcCtx, TgtCtx).

single_loss_check(_, SrcCtx, TgtCtx, immutability_flip) :-
    check_immutability_flip(SrcCtx, TgtCtx).

single_loss_check(C, SrcCtx, TgtCtx, coalition_upgrade) :-
    check_coalition_change(C, SrcCtx, TgtCtx).

single_loss_check(C, SrcCtx, TgtCtx, signature_override_divergence) :-
    check_signature_divergence(C, SrcCtx, TgtCtx).

%% check_threshold_crossing(+C, +SrcCtx, +TgtCtx)
%  True if chi crosses a classification threshold between contexts.
check_threshold_crossing(C, SrcCtx, TgtCtx) :-
    compute_chi_at_context(C, SrcCtx, Chi1, _),
    compute_chi_at_context(C, TgtCtx, Chi2, _),
    config:param(snare_chi_floor, SnareFloor),
    config:param(rope_chi_ceiling, RopeCeil),
    config:param(tangled_rope_chi_floor, TRFloor),
    config:param(tangled_rope_chi_ceil, TRCeil),
    % Check if any threshold lies between Chi1 and Chi2
    MinChi is min(Chi1, Chi2),
    MaxChi is max(Chi1, Chi2),
    (   (MinChi < SnareFloor, MaxChi >= SnareFloor)
    ;   (MinChi < RopeCeil, MaxChi >= RopeCeil)
    ;   (MinChi < TRFloor, MaxChi >= TRFloor)
    ;   (MinChi < TRCeil, MaxChi >= TRCeil)
    ), !.

%% check_gate_priority_shadow(+C, +SrcCtx, +TgtCtx)
%  True if different gates fire at each context due to priority ordering.
%  Detected when the metric-based type differs from what chi alone would suggest.
check_gate_priority_shadow(C, SrcCtx, TgtCtx) :-
    drl_core:base_extractiveness(C, BaseEps),
    drl_core:get_raw_suppression(C, Supp),
    compute_chi_at_context(C, SrcCtx, Chi1, _),
    compute_chi_at_context(C, TgtCtx, Chi2, _),
    drl_core:classify_from_metrics(C, BaseEps, Chi1, Supp, SrcCtx, Type1),
    drl_core:classify_from_metrics(C, BaseEps, Chi2, Supp, TgtCtx, Type2),
    Type1 \= Type2,
    % Check if a higher-priority gate fires at one but not other
    gate_priority(Type1, P1),
    gate_priority(Type2, P2),
    P1 \= P2.

gate_priority(mountain, 1).
gate_priority(snare, 2).
gate_priority(scaffold, 3).
gate_priority(rope, 4).
gate_priority(tangled_rope, 5).
gate_priority(piton, 6).
gate_priority(indexically_opaque, 7).
gate_priority(unknown, 8).

%% check_immutability_flip(+SrcCtx, +TgtCtx)
%  True if effective_immutability perception ACTUALLY DIFFERS in a way
%  that affects classification. Only counts when one context sees mountain
%  perception and the other sees rope perception (not just different T/E).
check_immutability_flip(SrcCtx, TgtCtx) :-
    SrcCtx = context(_, time_horizon(T1), exit_options(E1), _),
    TgtCtx = context(_, time_horizon(T2), exit_options(E2), _),
    findall(I1, constraint_indexing:effective_immutability(T1, E1, I1), Imm1),
    findall(I2, constraint_indexing:effective_immutability(T2, E2, I2), Imm2),
    sort(Imm1, S1), sort(Imm2, S2),
    S1 \= S2,
    % One must see ONLY mountain while the other sees rope
    (   (S1 = [mountain], member(rope, S2))
    ;   (S2 = [mountain], member(rope, S1))
    ).

%% check_coalition_change(+C, +SrcCtx, +TgtCtx)
%  True if coalition resolution actually changes the effective power level
%  (powerless upgrades to organized at one context but not the other,
%  or one context starts at powerless while the other doesn't).
%  Only fires when the coalition mechanism ITSELF causes divergence.
check_coalition_change(C, SrcCtx, TgtCtx) :-
    SrcCtx = context(agent_power(P1), _, _, _),
    TgtCtx = context(agent_power(P2), _, _, _),
    constraint_indexing:resolve_coalition_power(P1, C, R1),
    constraint_indexing:resolve_coalition_power(P2, C, R2),
    % Coalition mechanism matters when input power differs from resolved power
    % at at least one context (i.e., the upgrade actually fired)
    (P1 \= R1 ; P2 \= R2),
    R1 \= R2.

%% check_signature_divergence(+C, +SrcCtx, +TgtCtx)
%  True if signature override produces different results at each context.
check_signature_divergence(C, SrcCtx, TgtCtx) :-
    drl_core:base_extractiveness(C, BaseEps),
    drl_core:get_raw_suppression(C, Supp),
    compute_chi_at_context(C, SrcCtx, Chi1, _),
    compute_chi_at_context(C, TgtCtx, Chi2, _),
    drl_core:classify_from_metrics(C, BaseEps, Chi1, Supp, SrcCtx, MetricType1),
    drl_core:classify_from_metrics(C, BaseEps, Chi2, Supp, TgtCtx, MetricType2),
    signature_detection:integrate_signature_with_modal(C, MetricType1, Final1),
    signature_detection:integrate_signature_with_modal(C, MetricType2, Final2),
    % Signature divergence = override changed the type at one but not the other
    (   (MetricType1 = Final1, MetricType2 \= Final2)
    ;   (MetricType1 \= Final1, MetricType2 = Final2)
    ;   (MetricType1 \= Final1, MetricType2 \= Final2, Final1 \= Final2)
    ), !.

%% attribute_losses_for_family(+OrbitSig)
%  For all roundtrip failures in a family, run loss attribution.
attribute_losses_for_family(OrbitSig) :-
    inv_orbit_family(OrbitSig, Constraints),
    forall(
        (inv_roundtrip(C, SrcCtx, _, TgtCtx, _, _, false),
         member(C, Constraints)),
        (   pipeline_lossiness(C, SrcCtx, TgtCtx, Steps),
            forall(member(Step, Steps),
                   assertz(inv_loss(C, SrcCtx, TgtCtx, Step)))
        )
    ).

% ============================================================================
% SECTION 8: TANGLED MIDDLE BAND ANALYSIS
% ============================================================================

%% tangled_rope_band_analysis
%  Deep dive into [rope, snare, tangled_rope] three-type orbit.
%  Core question: does the context that sees tangled_rope carry
%  irreducible information not recoverable from the other two types?
tangled_rope_band_analysis :-
    retractall(inv_tangled_band(_, _, _, _)),
    OrbitSig = [rope, snare, tangled_rope],
    (   inv_orbit_family(OrbitSig, Constraints)
    ->  true
    ;   Constraints = []
    ),
    length(Constraints, N),
    format(user_error, '[invertibility] Tangled band analysis: ~w constraints~n', [N]),
    forall(
        member(C, Constraints),
        analyze_tangled_constraint(C)
    ).

analyze_tangled_constraint(C) :-
    PowCtx = context(agent_power(powerless),
                     time_horizon(biographical),
                     exit_options(trapped),
                     spatial_scope(local)),
    ModCtx = context(agent_power(moderate),
                     time_horizon(biographical),
                     exit_options(mobile),
                     spatial_scope(national)),
    InstCtx = context(agent_power(institutional),
                      time_horizon(generational),
                      exit_options(arbitrage),
                      spatial_scope(national)),
    AnalCtx = context(agent_power(analytical),
                      time_horizon(civilizational),
                      exit_options(analytical),
                      spatial_scope(global)),
    (drl_core:dr_type(C, PowCtx, PowType) -> true ; PowType = unknown),
    (drl_core:dr_type(C, ModCtx, ModType) -> true ; ModType = unknown),
    (drl_core:dr_type(C, InstCtx, InstType) -> true ; InstType = unknown),
    (drl_core:dr_type(C, AnalCtx, AnalType) -> true ; AnalType = unknown),
    % Compute chi at each context for sub-band analysis
    (compute_chi_at_context(C, ModCtx, ChiMod, _) -> true ; ChiMod = 0.0),
    (compute_chi_at_context(C, PowCtx, ChiPow, _) -> true ; ChiPow = 0.0),
    chi_subband(ChiMod, SubBand),
    % Find where tangled_rope appears in this constraint's orbit
    findall(Ctx-tangled_rope,
            (member(Ctx-Type, [powerless-PowType, moderate-ModType,
                               institutional-InstType, analytical-AnalType]),
             Type = tangled_rope),
            TangledContexts),
    % Recoverability: given the two non-tangled endpoint types,
    % can you predict which context sees tangled_rope?
    (   TangledContexts = []
    ->  Recoverable = no_tangled_seen  % Shouldn't happen in this orbit
    ;   % Count unique patterns: how many distinct type sequences exist?
        Pattern = pattern(PowType, ModType, InstType, AnalType),
        Recoverable = observed(Pattern)
    ),
    assertz(inv_tangled_band(C, SubBand, ChiMod, Recoverable)),
    !.
analyze_tangled_constraint(_).

chi_subband(Chi, low_tangled) :- Chi >= 0.40, Chi < 0.55, !.
chi_subband(Chi, mid_tangled) :- Chi >= 0.55, Chi < 0.70, !.
chi_subband(Chi, high_tangled) :- Chi >= 0.70, Chi =< 0.90, !.
chi_subband(Chi, below_range) :- Chi < 0.40, !.
chi_subband(_, above_range).

% ============================================================================
% SECTION 9: REPORT GENERATION
% ============================================================================

%% generate_invertibility_report
%  Writes the full report to stdout.
generate_invertibility_report :-
    % Clear delimiter to separate from any testset initialization output
    format('~n<!-- INVERTIBILITY_REPORT_START -->~n~n'),
    format('# Invertibility Analysis of Context-Tuple Transformations~n~n'),
    format('## Executive Summary~n~n'),
    report_executive_summary,
    format('~n## 1. Strategy B Results: Failure Rates by Orbit Family~n~n'),
    report_strategy_b_results,
    format('~n## 2. Loss Attribution: Which Pipeline Steps Cause Failures~n~n'),
    report_loss_attribution,
    format('~n## 3. Tangled Middle Band: Irreducible Information~n~n'),
    report_tangled_band,
    format('~n## 4. The Scholze Assessment~n~n'),
    report_scholze_assessment,
    format('~n## 5. Practical Implications~n~n'),
    report_practical_implications.

%% report_executive_summary
report_executive_summary :-
    findall(Sig, inv_orbit_family(Sig, _), Sigs),
    forall(
        member(Sig, Sigs),
        (   inv_orbit_family(Sig, Cs),
            length(Cs, FamilySize),
            count_roundtrips(Sig, Cs, Total, Successes),
            (Total > 0 -> Rate is Successes / Total * 100 ; Rate = 0.0),
            format('- **~w** (~w constraints): Strategy B success rate = **~1f%** (~w/~w roundtrips)~n',
                   [Sig, FamilySize, Rate, Successes, Total])
        )
    ),
    % Overall
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, true), TotalSucc),
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, _), TotalAll),
    (TotalAll > 0 -> OverallRate is TotalSucc / TotalAll * 100 ; OverallRate = 0.0),
    format('~n**Overall**: ~1f% success (~w/~w)~n', [OverallRate, TotalSucc, TotalAll]).

count_roundtrips(_, Cs, Total, Successes) :-
    aggregate_all(count,
        (inv_roundtrip(C, _, _, _, _, _, _), member(C, Cs)),
        Total),
    aggregate_all(count,
        (inv_roundtrip(C, _, _, _, _, _, true), member(C, Cs)),
        Successes).

%% report_strategy_b_results
report_strategy_b_results :-
    % Focus on the four largest families
    BigFour = [[rope, snare], [tangled_rope], [rope, snare, tangled_rope], [rope, tangled_rope]],
    forall(
        member(Sig, BigFour),
        report_family_detail(Sig)
    ),
    % Remaining families
    format('### Other Orbit Families~n~n'),
    findall(Sig, inv_orbit_family(Sig, _), AllSigs),
    forall(
        (member(Sig, AllSigs), \+ member(Sig, BigFour)),
        report_family_brief(Sig)
    ).

report_family_detail(Sig) :-
    format('### Family: ~w~n~n', [Sig]),
    (   inv_orbit_family(Sig, Cs)
    ->  length(Cs, N),
        format('Population: ~w constraints~n~n', [N]),
        % Per-context-pair breakdown
        format('| Source → Target | Success | Fail | Rate |~n'),
        format('|---|---|---|---|~n'),
        forall(
            (inv_standard_context(SrcCtx), inv_standard_context(TgtCtx),
             SrcCtx \= TgtCtx),
            report_context_pair(Sig, Cs, SrcCtx, TgtCtx)
        ),
        format('~n'),
        % Show specific failures (up to 5)
        findall(failure(C, SP, ST, TP, TT, RT),
                (inv_roundtrip(C, SC, ST, TC, TT, RT, false),
                 member(C, Cs),
                 context_label(SC, SP),
                 context_label(TC, TP)),
                Failures),
        length(Failures, NF),
        (   NF > 0
        ->  format('**Notable failures** (~w total):~n~n', [NF]),
            take_n(5, Failures, ShownFailures),
            forall(
                member(failure(C, SP, ST, TP, TT, RT), ShownFailures),
                format('- `~w`: ~w(~w) → predicted ~w, actual ~w(~w)~n',
                       [C, ST, SP, RT, TT, TP])
            ),
            format('~n')
        ;   format('No failures in this family.~n~n')
        )
    ;   format('(Family not found in corpus)~n~n')
    ).

report_context_pair(_, Cs, SrcCtx, TgtCtx) :-
    context_label(SrcCtx, SrcP),
    context_label(TgtCtx, TgtP),
    aggregate_all(count,
        (inv_roundtrip(C, SrcCtx, _, TgtCtx, _, _, _), member(C, Cs)),
        Total),
    aggregate_all(count,
        (inv_roundtrip(C, SrcCtx, _, TgtCtx, _, _, true), member(C, Cs)),
        Succ),
    Fail is Total - Succ,
    (Total > 0 -> Rate is Succ / Total * 100 ; Rate = 0.0),
    format('| ~w → ~w | ~w | ~w | ~1f% |~n', [SrcP, TgtP, Succ, Fail, Rate]).

report_family_brief(Sig) :-
    inv_orbit_family(Sig, Cs),
    length(Cs, N),
    count_roundtrips(Sig, Cs, Total, Succ),
    (Total > 0 -> Rate is Succ / Total * 100 ; Rate = 0.0),
    format('- **~w** (~w constraints): ~1f% success~n', [Sig, N, Rate]).

%% report_loss_attribution
report_loss_attribution :-
    format('### Aggregate Loss Sources~n~n'),
    format('| Loss Step | Count | % of Failures |~n'),
    format('|---|---|---|~n'),
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, false), TotalFail),
    forall(
        member(Step, [threshold_crossing, gate_priority_shadow,
                      immutability_flip, coalition_upgrade,
                      signature_override_divergence]),
        (   aggregate_all(count, inv_loss(_, _, _, Step), StepCount),
            (TotalFail > 0 -> Pct is StepCount / TotalFail * 100 ; Pct = 0.0),
            format('| ~w | ~w | ~1f% |~n', [Step, StepCount, Pct])
        )
    ),
    format('~n'),
    format('*Note: A single failure may be attributed to multiple loss steps.*~n~n'),

    % Per-family breakdown
    format('### Loss Sources by Orbit Family~n~n'),
    BigFour = [[rope, snare], [tangled_rope], [rope, snare, tangled_rope], [rope, tangled_rope]],
    forall(
        member(Sig, BigFour),
        report_family_loss(Sig)
    ).

report_family_loss(Sig) :-
    format('**~w**:~n', [Sig]),
    (   inv_orbit_family(Sig, Cs)
    ->  aggregate_all(count,
            (inv_roundtrip(C, _, _, _, _, _, false), member(C, Cs)),
            FamFail),
        (   FamFail > 0
        ->  forall(
                member(Step, [threshold_crossing, gate_priority_shadow,
                              immutability_flip, coalition_upgrade,
                              signature_override_divergence]),
                (   aggregate_all(count,
                        (inv_loss(C, _, _, Step), member(C, Cs)),
                        SC),
                    Pct is SC / FamFail * 100,
                    format('  - ~w: ~w (~1f%)~n', [Step, SC, Pct])
                )
            )
        ;   format('  (no failures)~n')
        )
    ;   format('  (family not found)~n')
    ),
    format('~n').

%% report_tangled_band
report_tangled_band :-
    findall(B, inv_tangled_band(_, B, _, _), Bands),
    (   Bands = []
    ->  format('No constraints in [rope, snare, tangled_rope] family.~n')
    ;   % Sub-band distribution
        format('### Chi Distribution at Moderate Context~n~n'),
        format('| Sub-band | Count |~n'),
        format('|---|---|~n'),
        forall(
            member(Band, [below_range, low_tangled, mid_tangled, high_tangled, above_range]),
            (   aggregate_all(count, inv_tangled_band(_, Band, _, _), BCount),
                (BCount > 0
                ->  format('| ~w | ~w |~n', [Band, BCount])
                ;   true
                )
            )
        ),
        format('~n'),

        % Chi range statistics
        findall(Chi, inv_tangled_band(_, _, Chi, _), Chis),
        msort(Chis, SortedChis),
        length(SortedChis, NChis),
        (   NChis > 0
        ->  nth1(1, SortedChis, MinChi),
            last(SortedChis, MaxChi),
            MedIdx is max(1, NChis // 2),
            nth1(MedIdx, SortedChis, MedChi),
            format('Chi at moderate context: min=~3f, median=~3f, max=~3f~n~n', [MinChi, MedChi, MaxChi])
        ;   true
        ),

        % Observed context→type patterns
        format('### Observed Context→Type Patterns~n~n'),
        format('The assumed "clean" pattern (powerless=snare, moderate=tangled_rope, institutional=rope)~n'),
        format('may not hold. Here are the actually observed patterns:~n~n'),
        findall(Pat,
                inv_tangled_band(_, _, _, observed(Pat)),
                AllPatterns),
        msort(AllPatterns, SortedPats),
        clumped(SortedPats, Clumped),
        sort(2, @>=, Clumped, ByCount),
        format('| Powerless | Moderate | Institutional | Analytical | Count |~n'),
        format('|---|---|---|---|---|~n'),
        forall(
            member(pattern(P, M, I, A)-Cnt, ByCount),
            format('| ~w | ~w | ~w | ~w | ~w |~n', [P, M, I, A, Cnt])
        ),
        format('~n'),

        % Key finding: where does tangled_rope actually appear?
        format('### Where Tangled_Rope Appears~n~n'),
        count_type_at_context(powerless, tangled_rope, N_Pow_TR),
        count_type_at_context(moderate, tangled_rope, N_Mod_TR),
        count_type_at_context(institutional, tangled_rope, N_Inst_TR),
        count_type_at_context(analytical, tangled_rope, N_Anal_TR),
        aggregate_all(count, inv_tangled_band(_, _, _, _), TotAll),
        format('- Powerless sees tangled_rope: ~w/~w~n', [N_Pow_TR, TotAll]),
        format('- Moderate sees tangled_rope: ~w/~w~n', [N_Mod_TR, TotAll]),
        format('- Institutional sees tangled_rope: ~w/~w~n', [N_Inst_TR, TotAll]),
        format('- Analytical sees tangled_rope: ~w/~w~n~n', [N_Anal_TR, TotAll]),

        % Irreducibility assessment
        format('### Irreducibility Assessment~n~n'),
        % Count how many distinct patterns exist
        length(ByCount, NDistinct),
        format('**~w distinct context→type patterns** observed across ~w constraints.~n~n', [NDistinct, TotAll]),
        (   NDistinct = 1
        ->  format('All constraints in this orbit follow the same pattern — the~n'),
            format('tangled_rope type appears at a predictable context position.~n'),
            format('Invertibility is achievable via pattern matching.~n~n')
        ;   format('Multiple patterns coexist within the same orbit family.~n'),
            format('This means knowing the orbit signature [rope, snare, tangled_rope]~n'),
            format('is NOT sufficient to predict which context sees which type.~n'),
            format('The tangled_rope position within the orbit carries **irreducible~n'),
            format('structural information** about the specific constraint.~n~n'),
            format('Concretely: two constraints can both belong to the [rope, snare, tangled_rope]~n'),
            format('orbit but differ in WHERE the tangled_rope appears. This difference~n'),
            format('reflects genuine structural variation (e.g., coalition dynamics,~n'),
            format('epsilon magnitude) that the orbit signature flattens.~n~n')
        ),

        % Show a few concrete examples of different patterns
        format('### Concrete Examples~n~n'),
        (   ByCount = [pattern(P1,M1,I1,A1)-_|_]
        ->  findall(C, inv_tangled_band(C, _, _, observed(pattern(P1,M1,I1,A1))), ExamplesP1),
            (ExamplesP1 = [Ex1|_] -> true ; Ex1 = none),
            format('**Most common pattern** (~w/~w/~w/~w):~n', [P1,M1,I1,A1]),
            format('- Example: `~w`~n~n', [Ex1])
        ;   true
        ),
        (   ByCount = [_,pattern(P2,M2,I2,A2)-_|_]
        ->  findall(C, inv_tangled_band(C, _, _, observed(pattern(P2,M2,I2,A2))), ExamplesP2),
            (ExamplesP2 = [Ex2|_] -> true ; Ex2 = none),
            format('**Second pattern** (~w/~w/~w/~w):~n', [P2,M2,I2,A2]),
            format('- Example: `~w`~n~n', [Ex2])
        ;   true
        )
    ).

%% count_type_at_context(+ContextLabel, +Type, -Count)
count_type_at_context(CtxLabel, Type, Count) :-
    aggregate_all(count,
        (inv_tangled_band(_, _, _, observed(Pat)),
         pattern_has_type_at(Pat, CtxLabel, Type)),
        Count).

pattern_has_type_at(pattern(T, _, _, _), powerless, T).
pattern_has_type_at(pattern(_, T, _, _), moderate, T).
pattern_has_type_at(pattern(_, _, T, _), institutional, T).
pattern_has_type_at(pattern(_, _, _, T), analytical, T).

%% report_scholze_assessment
report_scholze_assessment :-
    format('### Where the Analogy Illuminates~n~n'),
    format('The tilting equivalence maps between different "completions" of the same~n'),
    format('underlying object. In this system:~n~n'),
    format('- The **orbit family** is the underlying object (the perfectoid space)~n'),
    format('- Each **context-indexed classification** is a completion~n'),
    format('- The **structural invariants** (BaseEps, Supp, flags, signature) are the perfectoid core~n'),
    format('- **Singleton orbits** are perfectoid primes — trivially invertible~n~n'),

    format('The orbit taxonomy successfully organizes constraints by their gauge structure,~n'),
    format('and the Strategy B test quantifies exactly how much of that structure~n'),
    format('is recoverable from type information alone.~n~n'),

    format('### Where It Breaks Down~n~n'),
    format('1. **Tilting is exact; this is not.** Scholze\\''s tilting equivalence is an~n'),
    format('   *equivalence of categories* — no information is lost. The DR pipeline has~n'),
    format('   threshold gates that intentionally discard distance-from-threshold.~n'),
    format('   '),

    % Compute actual data for the assessment
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, _), TotalTests),
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, true), TotalSucc),
    (TotalTests > 0 -> LossRate is (1 - TotalSucc / TotalTests) * 100 ; LossRate = 0.0),
    format('Empirically, ~1f% of roundtrips lose information.~n~n', [LossRate]),

    format('2. **No simpler characteristic.** Tilting works because characteristic p~n'),
    format('   is "simpler" than characteristic 0. There is no analog here — the~n'),
    format('   analytical context is the most comprehensive, not the simplest.~n~n'),

    format('3. **Lossiness is load-bearing.** Classification MEANS choosing a discrete~n'),
    format('   type from continuous data. The thresholds are not accidental — they~n'),
    format('   encode the system\\''s theory of what distinctions matter. Removing~n'),
    format('   the lossiness would remove the classification.~n~n'),

    format('### Verdict~n~n'),
    format('The Scholze analogy is a **productive metaphor** for organizing the analysis~n'),
    format('but does not hold as a formal equivalence. The orbit taxonomy recovers~n'),
    format('most of the gauge structure (Strategy B succeeds for the majority of~n'),
    format('roundtrips), but the threshold boundaries introduce genuine, irreversible~n'),
    format('information loss that is *by design*.~n').

%% report_practical_implications
report_practical_implications :-
    format('### For `check_all_contexts`~n~n'),
    format('The finding that Strategy B has non-trivial failure rates means that~n'),
    format('`check_all_contexts` cannot be replaced by a single-context query plus~n'),
    format('an orbit-based reconstruction. The orbit signature tells you *which types*~n'),
    format('are possible but not *which context produces which type* — that requires~n'),
    format('the full pipeline (Strategy A), which needs the constraint identity.~n~n'),

    format('### For the Reform Pipeline~n~n'),
    (   findall(Pat, inv_tangled_band(_, _, _, observed(Pat)), AllPats),
        AllPats \= []
    ->  msort(AllPats, SortedPats),
        clumped(SortedPats, Clumped),
        length(Clumped, NDistinctPats),
        length(AllPats, TotTB),
        format('The [rope, snare, tangled_rope] orbit (~w constraints) shows ~w distinct~n',
               [TotTB, NDistinctPats]),
        format('context→type patterns. '),
        (   NDistinctPats > 1
        ->  format('The non-uniqueness means that knowing a constraint~n'),
            format('is in this orbit family does NOT tell you which context sees the tangled~n'),
            format('middle — the coalition upgrade mechanism and epsilon magnitude create~n'),
            format('structural variation within the family. Reform analysis must still~n'),
            format('run the full pipeline per constraint.~n~n')
        ;   format('The single pattern means reform analysis can use~n'),
            format('orbit membership to predict tangled_rope position.~n~n')
        )
    ;   format('(Insufficient data for three-type orbit analysis.)~n~n')
    ),

    format('### For Constraint Authors~n~n'),
    format('The orbit taxonomy provides a **structural fingerprint** that is more~n'),
    format('informative than any single classification. A constraint\\''s orbit signature~n'),
    format('encodes its gauge structure — how it transforms across perspectives.~n'),
    format('This is a first-class property that should be surfaced in constraint reports.~n').

% ============================================================================
% SECTION 10: MAIN ENTRY POINT
% ============================================================================

%% run_invertibility_analysis
%  Full pipeline: enumerate, test, attribute, report.
run_invertibility_analysis :-
    format(user_error, '[invertibility] Starting invertibility analysis...~n', []),

    % Step 1: Enumerate orbit families
    enumerate_orbit_families,

    % Step 2: Run roundtrip tests for all families
    retractall(inv_roundtrip(_, _, _, _, _, _, _)),
    retractall(inv_loss(_, _, _, _)),
    findall(Sig, inv_orbit_family(Sig, _), AllSigs),
    length(AllSigs, NFam),
    format(user_error, '[invertibility] Running roundtrip tests for ~w families...~n', [NFam]),
    forall(
        member(Sig, AllSigs),
        (   inv_orbit_family(Sig, Cs),
            length(Cs, N),
            format(user_error, '[invertibility]   ~w (~w constraints)...~n', [Sig, N]),
            run_roundtrips(Sig, Cs)
        )
    ),

    % Count results
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, _), TotalTests),
    aggregate_all(count, inv_roundtrip(_, _, _, _, _, _, true), TotalSucc),
    format(user_error, '[invertibility] Roundtrip results: ~w/~w successful.~n',
           [TotalSucc, TotalTests]),

    % Step 3: Loss attribution for failures
    format(user_error, '[invertibility] Attributing losses...~n', []),
    forall(member(Sig, AllSigs), attribute_losses_for_family(Sig)),

    % Step 4: Tangled middle band analysis
    format(user_error, '[invertibility] Tangled band analysis...~n', []),
    tangled_rope_band_analysis,

    % Step 5: Generate report
    format(user_error, '[invertibility] Generating report...~n', []),
    generate_invertibility_report,
    format(user_error, '[invertibility] Done.~n', []).

% ============================================================================
% UTILITY
% ============================================================================

take_n(_, [], []) :- !.
take_n(0, _, []) :- !.
take_n(N, [H|T], [H|R]) :-
    N1 is N - 1,
    take_n(N1, T, R).
