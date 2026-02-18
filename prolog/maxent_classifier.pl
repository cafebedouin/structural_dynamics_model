% ============================================================================
% MAXIMUM ENTROPY SHADOW CLASSIFIER
% ============================================================================
% Diagnostic shadow classifier that runs alongside the deterministic
% classify_from_metrics/6 cascade. Produces probability distributions,
% entropy scores, and disagreement flags — purely as diagnostics.
% The existing deterministic classifier remains the source of truth.
%
% Architecture follows the FPN iterator pattern (drl_modal_logic.pl:1807-2044):
%   1. maxent_cleanup/0    — retract prior run's dynamic facts
%   2. maxent_precompute/2 — compute profiles from corpus
%   3. maxent_run/2        — classify all constraints, store distributions
%   4. Query API           — entropy, confidence, disagreement accessors
%
% Standalone run:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -g "maxent_classifier:maxent_selftest, halt."
% ============================================================================

:- module(maxent_classifier, [
    % Configuration
    maxent_enabled/0,

    % Core API
    maxent_run/2,                    % maxent_run(Context, Summary)
    maxent_multi_run/2,              % maxent_multi_run(Contexts, Summaries)
    maxent_distribution/3,           % maxent_distribution(C, Context, Distribution)
    maxent_distribution_raw/3,       % maxent_distribution_raw(C, Context, Distribution)
    maxent_confidence/3,             % maxent_confidence(C, Context, Confidence)
    maxent_entropy/3,                % maxent_entropy(C, Context, NormalizedEntropy)
    maxent_disagreement/3,           % maxent_disagreement(C, Context, DisagreementInfo)
    maxent_top_type/3,               % maxent_top_type(C, Context, TopType)

    % Batch analysis
    maxent_high_uncertainty/3,       % maxent_high_uncertainty(Context, Threshold, Constraints)
    maxent_disagreements/2,          % maxent_disagreements(Context, DisagreementList)

    % Threshold proximity
    maxent_threshold_proximity/4,    % maxent_threshold_proximity(C, Context, Boundary, Distance)
    maxent_boundary_analysis/3,      % maxent_boundary_analysis(C, Context, Analysis)

    % Indexed MaxEnt (quantum complexity diagnostic)
    maxent_indexed_run/2,            % maxent_indexed_run(Context, Summary)
    maxent_indexed_distribution/3,   % maxent_indexed_distribution(C, Context, Distribution)
    maxent_classical_vs_indexed/4,   % maxent_classical_vs_indexed(C, Context, Classical, Indexed)
    maxent_indexing_divergence/3,    % maxent_indexing_divergence(C, Context, Divergence)
    maxent_indexing_divergences/3,   % maxent_indexing_divergences(Context, Threshold, Divergent)

    % Internals (exposed for testing)
    maxent_type_log_likelihood/5,    % maxent_type_log_likelihood(C, Type, Context, LogL, Details)
    maxent_cleanup/0,
    maxent_precompute/2,             % maxent_precompute(Constraints, Context)
    maxent_selftest/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(covering_analysis).

:- use_module(library(lists)).

/* ================================================================
   DYNAMIC FACTS
   ================================================================ */

:- dynamic maxent_dist/3.           % maxent_dist(Constraint, Context, TypeProbList)
:- dynamic maxent_dist_raw/3.       % maxent_dist_raw(Constraint, Context, TypeProbList) — pre-override
:- dynamic maxent_profile/3.        % maxent_profile(Type, MetricName, params(Mu, Sigma))
:- dynamic maxent_prior/2.          % maxent_prior(Type, Prior)
:- dynamic maxent_run_info/3.       % maxent_run_info(Context, NConstraints, Timestamp)

% Indexed MaxEnt dynamic facts (power-scaled χ instead of raw ε)
:- dynamic maxent_indexed_dist/3.   % maxent_indexed_dist(C, Context, TypeProbList)
:- dynamic maxent_indexed_profile/3. % maxent_indexed_profile(Type, MetricName, params(Mu, Sigma))

/* ================================================================
   CONFIGURATION
   ================================================================ */

maxent_enabled :-
    config:param(maxent_enabled, 1).

%% The six primary types for the shadow classifier.
%  Excludes unknown and indexically_opaque (residual categories).
maxent_type(mountain).
maxent_type(rope).
maxent_type(tangled_rope).
maxent_type(snare).
maxent_type(scaffold).
maxent_type(piton).

maxent_n_types(6).

/* ================================================================
   CLEANUP
   ================================================================ */

maxent_cleanup :-
    retractall(maxent_dist(_, _, _)),
    retractall(maxent_dist_raw(_, _, _)),
    retractall(maxent_profile(_, _, _)),
    retractall(maxent_prior(_, _)),
    retractall(maxent_run_info(_, _, _)),
    retractall(maxent_indexed_dist(_, _, _)),
    retractall(maxent_indexed_profile(_, _, _)).

/* ================================================================
   GAUSSIAN LOG-LIKELIHOOD
   ================================================================
   The MaxEnt principle: given constraints on first and second moments,
   the maximum entropy distribution is Gaussian. Since our continuous
   metrics are bounded [0,1] with known per-type means and std devs,
   Gaussian likelihoods are the principled MaxEnt choice.

   Omit constant -0.5*log(2*pi) since it cancels in normalization.
   ================================================================ */

gaussian_log_likelihood(X, Mu, Sigma, LogL) :-
    Sigma > 1.0e-15,
    Diff is X - Mu,
    LogL is -0.5 * (Diff * Diff) / (Sigma * Sigma) - log(Sigma).
gaussian_log_likelihood(_, _, Sigma, -100.0) :-
    Sigma =< 1.0e-15.

/* ================================================================
   PER-TYPE METRIC PROFILES
   ================================================================
   Empirical profiles derived from corpus data and config thresholds.
   During precompute, these are computed from actual corpus statistics.
   Fallback defaults are provided for robustness.
   ================================================================ */

%% default_profile(?Type, ?MetricName, ?Params)
%  Fallback profiles if corpus is too small to compute empirical stats.
default_profile(mountain,     extractiveness, params(0.09, 0.08)).
default_profile(mountain,     suppression,    params(0.03, 0.02)).
default_profile(mountain,     theater,        params(0.02, 0.05)).
default_profile(rope,         extractiveness, params(0.15, 0.12)).
default_profile(rope,         suppression,    params(0.30, 0.20)).
default_profile(rope,         theater,        params(0.25, 0.15)).
default_profile(tangled_rope, extractiveness, params(0.61, 0.15)).
default_profile(tangled_rope, suppression,    params(0.66, 0.15)).
default_profile(tangled_rope, theater,        params(0.30, 0.15)).
default_profile(snare,        extractiveness, params(0.67, 0.15)).
default_profile(snare,        suppression,    params(0.73, 0.12)).
default_profile(snare,        theater,        params(0.28, 0.18)).
default_profile(scaffold,     extractiveness, params(0.20, 0.12)).
default_profile(scaffold,     suppression,    params(0.38, 0.20)).
default_profile(scaffold,     theater,        params(0.14, 0.12)).
default_profile(piton,        extractiveness, params(0.65, 0.15)).
default_profile(piton,        suppression,    params(0.69, 0.15)).
default_profile(piton,        theater,        params(0.85, 0.08)).

/* ================================================================
   BOOLEAN FEATURE LOG-LIKELIHOOD
   ================================================================
   The deterministic classifier uses boolean predicates as hard gates.
   The shadow classifier treats them as soft log-likelihood terms.

   required:  log(1.0)=0.0 if true,  penalty if false
   forbidden: penalty if true, log(1.0)=0.0 if false
   bonus:     +bonus if true, 0.0 if false
   optional:  0.0 always
   ================================================================ */

%% boolean_spec(?Type, ?Feature, ?Constraint)
%  Specifies how each boolean feature affects each type's likelihood.
boolean_spec(mountain,     emerges_naturally,          required).
boolean_spec(mountain,     requires_active_enforcement, forbidden).
boolean_spec(snare,        natural_law_without_beneficiary, forbidden).
boolean_spec(scaffold,     has_coordination_function,   required).
boolean_spec(rope,         emerges_naturally,          bonus).
boolean_spec(tangled_rope, natural_law_without_beneficiary, forbidden).
boolean_spec(tangled_rope, requires_active_enforcement, required).
boolean_spec(tangled_rope, has_coordination_function,   required).
boolean_spec(tangled_rope, has_asymmetric_extraction,   required).

%% eval_boolean_feature(+Constraint, +Feature, -Value)
%  Evaluates a boolean structural predicate for a constraint.
eval_boolean_feature(C, emerges_naturally, Val) :-
    (drl_core:emerges_naturally(C) -> Val = true ; Val = false).
eval_boolean_feature(C, requires_active_enforcement, Val) :-
    (drl_core:requires_active_enforcement(C) -> Val = true ; Val = false).
eval_boolean_feature(C, has_coordination_function, Val) :-
    (narrative_ontology:has_coordination_function(C) -> Val = true ; Val = false).
eval_boolean_feature(C, has_asymmetric_extraction, Val) :-
    (narrative_ontology:has_asymmetric_extraction(C) -> Val = true ; Val = false).
eval_boolean_feature(C, natural_law_without_beneficiary, Val) :-
    (drl_core:natural_law_without_beneficiary(C) -> Val = true ; Val = false).

%% boolean_log_likelihood(+Constraint, +Type, -LogL)
%  Computes the total boolean contribution to log-likelihood for a type.
boolean_log_likelihood(C, Type, LogL) :-
    config:param(maxent_boolean_penalty, Penalty),
    config:param(maxent_boolean_bonus, Bonus),
    findall(LL,
        (   boolean_spec(Type, Feature, Spec),
            eval_boolean_feature(C, Feature, Val),
            boolean_ll_term(Spec, Val, Penalty, Bonus, LL)
        ),
        LLs),
    sum_list(LLs, LogL),
    !.
boolean_log_likelihood(_, _, 0.0).  % No boolean specs for this type

boolean_ll_term(required,  true,  _Penalty, _Bonus, 0.0).
boolean_ll_term(required,  false, Penalty,  _Bonus, Penalty).
boolean_ll_term(forbidden, true,  Penalty,  _Bonus, Penalty).
boolean_ll_term(forbidden, false, _Penalty, _Bonus, 0.0).
boolean_ll_term(bonus,     true,  _Penalty, Bonus,  Bonus).
boolean_ll_term(bonus,     false, _Penalty, _Bonus, 0.0).

/* ================================================================
   PRIOR LOG-LIKELIHOOD
   ================================================================ */

prior_log_likelihood(Type, PriorLL) :-
    maxent_prior(Type, Prior),
    Prior > 1.0e-15,
    !,
    PriorLL is log(Prior).
prior_log_likelihood(_, -10.0).  % Fallback for missing/zero prior

/* ================================================================
   CONTINUOUS LOG-LIKELIHOOD
   ================================================================
   Evaluates Gaussian log-likelihood across all metrics for a type.
   ================================================================ */

continuous_log_likelihood(C, Type, _Context, ContLL) :-
    get_constraint_metrics(C, Eps, Supp, Theater),
    findall(LL, (
        (   MetricName = extractiveness, X = Eps
        ;   MetricName = suppression, X = Supp
        ;   MetricName = theater, X = Theater
        ),
        maxent_profile(Type, MetricName, params(Mu, Sigma)),
        gaussian_log_likelihood(X, Mu, Sigma, LL)
    ), LLs),
    (   LLs \= []
    ->  sum_list(LLs, ContLL)
    ;   ContLL = 0.0
    ).

%% get_constraint_metrics(+C, -Eps, -Supp, -Theater)
%  Retrieves the three continuous metrics for a constraint.
get_constraint_metrics(C, Eps, Supp, Theater) :-
    (drl_core:base_extractiveness(C, Eps0) -> Eps = Eps0 ; Eps = 0.0),
    (drl_core:get_raw_suppression(C, Supp0) -> Supp = Supp0 ; Supp = 0.0),
    config:param(theater_metric_name, TheaterName),
    (narrative_ontology:constraint_metric(C, TheaterName, Theater0) -> Theater = Theater0 ; Theater = 0.0).

/* ================================================================
   PER-TYPE LOG-LIKELIHOOD (COMBINED)
   ================================================================ */

maxent_type_log_likelihood(C, Type, Context, TotalLogL, Details) :-
    maxent_type(Type),
    continuous_log_likelihood(C, Type, Context, ContLL),
    boolean_log_likelihood(C, Type, BoolLL),
    prior_log_likelihood(Type, PriorLL),
    TotalLogL is ContLL + BoolLL + PriorLL,
    Details = ll_detail(ContLL, BoolLL, PriorLL).

/* ================================================================
   NORMALIZATION VIA LOG-SUM-EXP
   ================================================================
   Computing P(type) = exp(logL_type) / Σ exp(logL_i) directly risks
   overflow/underflow. Subtracting the max log-likelihood first keeps
   all exponentials in [0, 1].
   ================================================================ */

normalize_log_probs(TypeLogLPairs, TypeProbPairs) :-
    maplist(pair_snd, TypeLogLPairs, LogLs),
    max_list(LogLs, MaxLL),
    maplist(shift_and_exp(MaxLL), TypeLogLPairs, TypeExpPairs),
    maplist(pair_snd, TypeExpPairs, Exps),
    sum_list(Exps, Total),
    (   Total > 1.0e-30
    ->  maplist(divide_by(Total), TypeExpPairs, TypeProbPairs)
    ;   % Uniform fallback if all probabilities are zero
        maxent_n_types(N),
        Uniform is 1.0 / N,
        maplist(set_uniform(Uniform), TypeLogLPairs, TypeProbPairs)
    ).

pair_snd(_-V, V).

shift_and_exp(MaxLL, Type-LL, Type-Exp) :-
    Shifted is LL - MaxLL,
    (   Shifted > -500
    ->  Exp is exp(Shifted)
    ;   Exp = 0.0
    ).

divide_by(Total, Type-Exp, Type-Prob) :-
    Prob is Exp / Total.

set_uniform(U, Type-_, Type-U).

/* ================================================================
   SIGNATURE OVERRIDE ADJUSTMENT
   ================================================================
   After computing the metric-based distribution, adjust for
   signature overrides. Unconditional overrides set target type
   probability to override_strength; conditional overrides boost
   by a factor of 3.
   ================================================================ */

apply_signature_override(C, DistIn, DistOut) :-
    (   catch(structural_signatures:constraint_signature(C, Sig), _, fail)
    ->  apply_override_for_sig(Sig, DistIn, DistOut)
    ;   DistOut = DistIn
    ).

%% Unconditional overrides: set target to 0.95, redistribute 0.05
apply_override_for_sig(natural_law, DistIn, DistOut) :-
    !, override_unconditional(mountain, DistIn, DistOut).
apply_override_for_sig(false_natural_law, DistIn, DistOut) :-
    !, override_unconditional(tangled_rope, DistIn, DistOut).
apply_override_for_sig(coupling_invariant_rope, DistIn, DistOut) :-
    !, override_unconditional(rope, DistIn, DistOut).

%% Conditional overrides: boost target by factor of 3
apply_override_for_sig(false_ci_rope, DistIn, DistOut) :-
    !, override_conditional(tangled_rope, 3.0, DistIn, DistOut).
apply_override_for_sig(coordination_scaffold, DistIn, DistOut) :-
    !, override_conditional(rope, 3.0, DistIn, DistOut).
apply_override_for_sig(constructed_low_extraction, DistIn, DistOut) :-
    !, override_conditional(rope, 3.0, DistIn, DistOut).
apply_override_for_sig(constructed_high_extraction, DistIn, DistOut) :-
    !, override_conditional(tangled_rope, 3.0, DistIn, DistOut).
apply_override_for_sig(constructed_constraint, DistIn, DistOut) :-
    !, override_conditional(tangled_rope, 3.0, DistIn, DistOut).

%% No override for other signatures
apply_override_for_sig(_, Dist, Dist).

override_unconditional(TargetType, DistIn, DistOut) :-
    config:param(maxent_signature_override_strength, Strength),
    Remainder is (1.0 - Strength) / 5.0,  % Split among 5 other types
    maplist(set_override_prob(TargetType, Strength, Remainder), DistIn, DistOut).

set_override_prob(TargetType, Strength, _Remainder, TargetType-_, TargetType-Strength) :- !.
set_override_prob(_TargetType, _Strength, Remainder, Type-_, Type-Remainder).

override_conditional(TargetType, Factor, DistIn, DistOut) :-
    maplist(boost_type(TargetType, Factor), DistIn, Boosted),
    maplist(pair_snd, Boosted, Vals),
    sum_list(Vals, Total),
    (   Total > 1.0e-15
    ->  maplist(divide_by(Total), Boosted, DistOut)
    ;   DistOut = DistIn
    ).

boost_type(TargetType, Factor, TargetType-P, TargetType-Boosted) :-
    !, Boosted is P * Factor.
boost_type(_, _, Type-P, Type-P).

/* ================================================================
   SHANNON ENTROPY
   ================================================================ */

shannon_entropy(TypeProbPairs, H) :-
    foldl(entropy_acc, TypeProbPairs, 0.0, H).

entropy_acc(_Type-P, Acc, NewAcc) :-
    (   P > 1.0e-15
    ->  NewAcc is Acc - P * log(P)
    ;   NewAcc = Acc
    ).

/* ================================================================
   PUBLIC QUERY API
   ================================================================ */

% Categorical: Distribution on Omega — probability measure over the type space (structurally analogous to Giry monad image)
%% maxent_distribution(+C, +Context, -Distribution)
maxent_distribution(C, Context, Dist) :-
    maxent_dist(C, Context, Dist).

%% maxent_distribution_raw(+C, +Context, -Distribution)
%  Raw (pre-override) distribution for override impact analysis.
maxent_distribution_raw(C, Context, Dist) :-
    maxent_dist_raw(C, Context, Dist).

%% maxent_entropy(+C, +Context, -HNorm)
maxent_entropy(C, Context, HNorm) :-
    maxent_dist(C, Context, Dist),
    shannon_entropy(Dist, H),
    maxent_n_types(N),
    HMax is log(N),
    (   HMax > 0
    ->  HNorm is H / HMax
    ;   HNorm = 0.0
    ).

%% maxent_confidence(+C, +Context, -Confidence)
maxent_confidence(C, Context, Conf) :-
    maxent_entropy(C, Context, HNorm),
    Conf is 1.0 - HNorm.

%% maxent_top_type(+C, +Context, -TopType)
maxent_top_type(C, Context, TopType) :-
    maxent_dist(C, Context, Dist),
    best_type(Dist, TopType).

best_type(Dist, TopType) :-
    foldl(max_prob, Dist, none-(-1.0), TopType-_).

max_prob(Type-P, _CurType-CurP, Type-P) :- P > CurP, !.
max_prob(_, Cur, Cur).

%% maxent_disagreement(+C, +Context, -DisagreementInfo)
%  Three levels:
%    hard(ShadowType, DetType)     — top types differ
%    soft(ShadowType, DetType, P)  — same top but P < 0.50
%    entropy_flag(HNorm)           — H_norm > threshold
%    none                          — no disagreement
maxent_disagreement(C, Context, Disagreement) :-
    maxent_top_type(C, Context, ShadowType),
    (   drl_core:dr_type(C, Context, DetType) -> true ; DetType = unknown ),
    maxent_entropy(C, Context, HNorm),
    config:param(maxent_uncertainty_threshold, EntropyThresh),
    config:param(maxent_disagreement_prob_threshold, ProbThresh),
    maxent_dist(C, Context, Dist),
    (   member(DetType-DetProb, Dist) -> true ; DetProb = 0.0 ),
    classify_disagreement(ShadowType, DetType, DetProb, HNorm,
                          EntropyThresh, ProbThresh, Disagreement).

residual_type(unknown).
residual_type(indexically_opaque).

%% Residual types (unknown, indexically_opaque) are not in the shadow model.
%  The shadow will always disagree — flag as residual_override, not hard.
classify_disagreement(ShadowType, DetType, _DetProb, _HNorm, _ET, _PT,
                      residual_override(ShadowType, DetType)) :-
    residual_type(DetType), !.
classify_disagreement(ShadowType, DetType, _DetProb, _HNorm, _ET, _PT,
                      hard(ShadowType, DetType)) :-
    ShadowType \= DetType, !.
classify_disagreement(ShadowType, DetType, DetProb, _HNorm, _ET, PT,
                      soft(ShadowType, DetType, DetProb)) :-
    DetProb < PT, !.
classify_disagreement(_ShadowType, _DetType, _DetProb, HNorm, ET, _PT,
                      entropy_flag(HNorm)) :-
    HNorm > ET, !.
classify_disagreement(_, _, _, _, _, _, none).

/* ================================================================
   BATCH ANALYSIS
   ================================================================ */

%% maxent_high_uncertainty(+Context, +Threshold, -Constraints)
maxent_high_uncertainty(Context, Threshold, Constraints) :-
    findall(C-HNorm,
        (   maxent_dist(C, Context, _),
            maxent_entropy(C, Context, HNorm),
            HNorm > Threshold
        ),
        Constraints).

%% maxent_disagreements(+Context, -DisagreementList)
maxent_disagreements(Context, DisagreementList) :-
    findall(C-Disagreement,
        (   maxent_dist(C, Context, _),
            maxent_disagreement(C, Context, Disagreement),
            Disagreement \= none
        ),
        DisagreementList).

/* ================================================================
   THRESHOLD PROXIMITY ANALYSIS
   ================================================================ */

%% threshold_boundary(?BoundaryName, ?MetricName, ?ThresholdValue, ?TypeBelow, ?TypeAbove)
threshold_boundary(snare_epsilon_floor,     extractiveness, Thresh, rope,         snare) :-
    config:param(snare_epsilon_floor, Thresh).
threshold_boundary(rope_epsilon_ceiling,    extractiveness, Thresh, rope,         tangled_rope) :-
    config:param(rope_epsilon_ceiling, Thresh).
threshold_boundary(snare_suppression_floor, suppression,    Thresh, tangled_rope, snare) :-
    config:param(snare_suppression_floor, Thresh).
threshold_boundary(tangled_rope_supp_floor, suppression,    Thresh, rope,         tangled_rope) :-
    config:param(tangled_rope_suppression_floor, Thresh).
threshold_boundary(mountain_supp_ceiling,   suppression,    Thresh, mountain,     rope) :-
    config:param(mountain_suppression_ceiling, Thresh).
threshold_boundary(snare_chi_floor,         chi,            Thresh, tangled_rope, snare) :-
    config:param(snare_chi_floor, Thresh).
threshold_boundary(rope_chi_ceiling,        chi,            Thresh, rope,         tangled_rope) :-
    config:param(rope_chi_ceiling, Thresh).

%% maxent_threshold_proximity(+C, +Context, -Boundary, -Distance)
maxent_threshold_proximity(C, Context, Boundary, Distance) :-
    get_constraint_metrics(C, Eps, Supp, _Theater),
    (   drl_core:dr_type(C, Context, _) -> true ; true ),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    threshold_boundary(Boundary, MetricName, Thresh, _, _),
    metric_value_for_name(MetricName, Eps, Supp, Chi, Val),
    Distance is abs(Val - Thresh).

metric_value_for_name(extractiveness, Eps, _Supp, _Chi, Eps).
metric_value_for_name(suppression,    _Eps, Supp, _Chi, Supp).
metric_value_for_name(chi,            _Eps, _Supp, Chi,  Chi).

%% maxent_boundary_analysis(+C, +Context, -Analysis)
%  Returns sorted list of boundary-distance pairs for a constraint.
maxent_boundary_analysis(C, Context, Analysis) :-
    findall(Distance-Boundary,
        maxent_threshold_proximity(C, Context, Boundary, Distance),
        Pairs),
    msort(Pairs, Analysis).

/* ================================================================
   PRECOMPUTE PHASE
   ================================================================
   Follows FPN's fpn_precompute/2 pattern.
   ================================================================ */

%% maxent_precompute(+Constraints, +Context)
maxent_precompute(Constraints, Context) :-
    maxent_cleanup,
    maxent_compute_profiles(Constraints, Context),
    maxent_compute_priors(Constraints),
    maxent_classify_all(Constraints, Context),
    length(Constraints, N),
    get_time(T),
    assertz(maxent_run_info(Context, N, T)).

%% maxent_compute_profiles(+Constraints, +Context)
%  For each type, compute empirical mean and std of (Eps, Supp, Theater)
%  from the loaded corpus. Store as maxent_profile/3 dynamic facts.
maxent_compute_profiles(Constraints, Context) :-
    forall(maxent_type(Type), (
        compute_type_profile(Constraints, Context, Type, extractiveness),
        compute_type_profile(Constraints, Context, Type, suppression),
        compute_type_profile(Constraints, Context, Type, theater)
    )).

compute_type_profile(Constraints, Context, Type, MetricName) :-
    findall(Val,
        (   member(C, Constraints),
            drl_core:dr_type(C, Context, Type),
            metric_value(C, MetricName, Val)
        ),
        Values),
    (   Values = [_,_|_]  % Need at least 2 values for std dev
    ->  length(Values, N),
        sum_list(Values, Sum),
        Mu is Sum / N,
        foldl(sum_sq_diff(Mu), Values, 0.0, SumSqDiff),
        Variance is SumSqDiff / N,
        Sigma is max(0.01, sqrt(Variance)),  % Floor to prevent division by zero
        assertz(maxent_profile(Type, MetricName, params(Mu, Sigma)))
    ;   % Fallback to default profile
        default_profile(Type, MetricName, Params),
        assertz(maxent_profile(Type, MetricName, Params))
    ).

sum_sq_diff(Mu, X, Acc, NewAcc) :-
    NewAcc is Acc + (X - Mu) * (X - Mu).

metric_value(C, extractiveness, Val) :-
    drl_core:base_extractiveness(C, Val).
metric_value(C, suppression, Val) :-
    drl_core:get_raw_suppression(C, Val).
metric_value(C, theater, Val) :-
    config:param(theater_metric_name, TheaterName),
    (narrative_ontology:constraint_metric(C, TheaterName, Val) -> true ; Val = 0.0).

%% maxent_compute_priors(+Constraints)
%  Count constraint types in corpus, normalize to priors.
%  Uses dr_type (deterministic classification) for consistency with profile
%  computation — both are trained on the same classification source.
maxent_compute_priors(Constraints) :-
    config:param(maxent_prior_mode, Mode),
    (   Mode = uniform
    ->  maxent_n_types(N),
        Prior is 1.0 / N,
        forall(maxent_type(Type), assertz(maxent_prior(Type, Prior)))
    ;   % corpus mode (default) — use dr_type for consistency with profiles
        constraint_indexing:default_context(Ctx),
        length(Constraints, Total),
        (   Total > 0
        ->  forall(maxent_type(Type), (
                aggregate_all(count,
                    (member(C, Constraints), drl_core:dr_type(C, Ctx, Type)),
                    Count),
                Prior is max(0.001, Count / Total),  % Floor to prevent log(0)
                assertz(maxent_prior(Type, Prior))
            ))
        ;   maxent_n_types(N),
            Uniform is 1.0 / N,
            forall(maxent_type(Type), assertz(maxent_prior(Type, Uniform)))
        )
    ).

%% maxent_classify_all(+Constraints, +Context)
%  For each constraint, compute the full 6-type distribution.
maxent_classify_all(Constraints, Context) :-
    forall(member(C, Constraints),
        maxent_classify_one(C, Context)
    ).

maxent_classify_one(C, Context) :-
    findall(Type-LogL,
        (   maxent_type(Type),
            maxent_type_log_likelihood(C, Type, Context, LogL, _)
        ),
        TypeLogLPairs),
    (   TypeLogLPairs \= []
    ->  normalize_log_probs(TypeLogLPairs, NormDist),
        assertz(maxent_dist_raw(C, Context, NormDist)),
        apply_signature_override(C, NormDist, FinalDist),
        assertz(maxent_dist(C, Context, FinalDist))
    ;   true
    ).

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

%% maxent_run(+Context, -Summary)
%  Discovers all constraints, runs precompute, returns summary.
maxent_run(Context, Summary) :-
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    length(Constraints, NTotal),
    format(user_error, '[maxent] Found ~w constraints.~n', [NTotal]),

    maxent_precompute(Constraints, Context),

    % Compute summary statistics
    config:param(maxent_uncertainty_threshold, Threshold),
    maxent_high_uncertainty(Context, Threshold, HighUncertainty),
    length(HighUncertainty, NHighUncertainty),

    maxent_disagreements(Context, Disagreements),
    include(is_hard_disagreement, Disagreements, HardDisagreements),
    length(HardDisagreements, NHard),
    include(is_soft_disagreement, Disagreements, SoftDisagreements),
    length(SoftDisagreements, NSoft),

    % Mean entropy
    findall(HN,
        (maxent_dist(C2, Context, _), maxent_entropy(C2, Context, HN)),
        Entropies),
    (   Entropies \= []
    ->  sum_list(Entropies, SumE),
        length(Entropies, NE),
        MeanEntropy is SumE / NE
    ;   MeanEntropy = 0.0
    ),

    Summary = maxent_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft),
    format(user_error, '[maxent] Done. Mean entropy=~4f, flagged=~w, hard_disagree=~w~n',
           [MeanEntropy, NHighUncertainty, NHard]).

is_hard_disagreement(_-hard(_, _)).
is_soft_disagreement(_-soft(_, _, _)).

/* ================================================================
   MULTI-CONTEXT MAXENT (v6.4 — Trajectory Mining Support)
   ================================================================
   Runs MaxEnt at multiple contexts (typically the 4 standard
   contexts from dirac_classification.pl). Stores distributions
   indexed by (Constraint, Context). The existing maxent_dist/3
   dynamic fact already supports multi-context storage; this
   predicate just iterates maxent_precompute across contexts
   without cleaning between them.
   ================================================================ */

%% maxent_multi_run(+Contexts, -Summaries)
%  Runs MaxEnt at each context in Contexts. Each context produces
%  independent profiles and distributions. Results accumulate —
%  distributions from earlier contexts are preserved.
%
%  Usage: maxent_multi_run([Ctx1, Ctx2, Ctx3, Ctx4], Summaries)
%  After: maxent_entropy(C, Ctx1, H1), maxent_entropy(C, Ctx2, H2) etc.
maxent_multi_run(Contexts, Summaries) :-
    % Clean all prior state once
    maxent_cleanup,
    % Discover constraints once
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    length(Constraints, NTotal),
    format(user_error, '[maxent_multi] Found ~w constraints, running ~w contexts.~n',
           [NTotal, Contexts]),
    maxent_multi_run_contexts(Contexts, Constraints, NTotal, Summaries).

maxent_multi_run_contexts([], _, _, []).
maxent_multi_run_contexts([Ctx|Rest], Constraints, NTotal, [Summary|RestSummaries]) :-
    format(user_error, '[maxent_multi] Computing profiles for context ~w...~n', [Ctx]),
    % Compute profiles for this context (don't cleanup between contexts)
    maxent_compute_profiles(Constraints, Ctx),
    maxent_compute_priors(Constraints),
    maxent_classify_all(Constraints, Ctx),
    get_time(T),
    assertz(maxent_run_info(Ctx, NTotal, T)),
    % Compute summary stats for this context
    config:param(maxent_uncertainty_threshold, Threshold),
    maxent_high_uncertainty(Ctx, Threshold, HighUncertainty),
    length(HighUncertainty, NHighUncertainty),
    maxent_disagreements(Ctx, Disagreements),
    include(is_hard_disagreement, Disagreements, HardDisagreements),
    length(HardDisagreements, NHard),
    include(is_soft_disagreement, Disagreements, SoftDisagreements),
    length(SoftDisagreements, NSoft),
    findall(HN,
        (maxent_dist(C2, Ctx, _), maxent_entropy(C2, Ctx, HN)),
        Entropies),
    (   Entropies \= []
    ->  sum_list(Entropies, SumE),
        length(Entropies, NE),
        MeanEntropy is SumE / NE
    ;   MeanEntropy = 0.0
    ),
    Summary = maxent_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft),
    format(user_error, '[maxent_multi] Context ~w done. Mean entropy=~4f, flagged=~w~n',
           [Ctx, MeanEntropy, NHighUncertainty]),
    maxent_multi_run_contexts(Rest, Constraints, NTotal, RestSummaries).

/* ================================================================
   INDEXED MAXENT — POWER-SCALED CLASSICAL ORACLE
   ================================================================
   The classical MaxEnt (above) uses raw ε, σ, τ — observer-independent
   structural metrics. This indexed variant uses power-scaled χ instead
   of raw ε for the extractiveness likelihood, creating a "fully indexed"
   probability distribution.

   Diagnostic purpose: comparing classical vs indexed MaxEnt identifies
   constraints where power-scaling changes the probabilistic picture.
   This operationalizes Yuen's question: "can the classical oracle solve
   the same problem?" When the two distributions diverge, the indexical
   structure is doing non-trivial work.

   Requires: maxent_run/2 must be called first (provides priors).
   ================================================================ */

%% get_constraint_metrics_indexed(+C, +Context, -Chi, -Supp, -Theater)
%  Like get_constraint_metrics/3 but uses power-scaled χ instead of raw ε.
get_constraint_metrics_indexed(C, Context, Chi, Supp, Theater) :-
    (constraint_indexing:extractiveness_for_agent(C, Context, Chi0) -> Chi = Chi0 ; Chi = 0.0),
    (drl_core:get_raw_suppression(C, Supp0) -> Supp = Supp0 ; Supp = 0.0),
    config:param(theater_metric_name, TheaterName),
    (narrative_ontology:constraint_metric(C, TheaterName, Theater0) -> Theater = Theater0 ; Theater = 0.0).

%% metric_value_indexed(+C, +Context, +MetricName, -Val)
%  Retrieves an indexed metric value. Extractiveness uses χ; others unchanged.
metric_value_indexed(C, Context, extractiveness, Val) :-
    (constraint_indexing:extractiveness_for_agent(C, Context, Val) -> true ; Val = 0.0).
metric_value_indexed(C, _, suppression, Val) :-
    drl_core:get_raw_suppression(C, Val).
metric_value_indexed(C, _, theater, Val) :-
    config:param(theater_metric_name, TheaterName),
    (narrative_ontology:constraint_metric(C, TheaterName, Val) -> true ; Val = 0.0).

%% continuous_log_likelihood_indexed(+C, +Type, +Context, -ContLL)
%  Gaussian log-likelihood using indexed profiles and power-scaled metrics.
continuous_log_likelihood_indexed(C, Type, Context, ContLL) :-
    get_constraint_metrics_indexed(C, Context, Chi, Supp, Theater),
    findall(LL, (
        (   MetricName = extractiveness, X = Chi
        ;   MetricName = suppression, X = Supp
        ;   MetricName = theater, X = Theater
        ),
        maxent_indexed_profile(Type, MetricName, params(Mu, Sigma)),
        gaussian_log_likelihood(X, Mu, Sigma, LL)
    ), LLs),
    (   LLs \= []
    ->  sum_list(LLs, ContLL)
    ;   ContLL = 0.0
    ).

%% maxent_type_log_likelihood_indexed(+C, +Type, +Context, -TotalLogL, -Details)
maxent_type_log_likelihood_indexed(C, Type, Context, TotalLogL, Details) :-
    maxent_type(Type),
    continuous_log_likelihood_indexed(C, Type, Context, ContLL),
    boolean_log_likelihood(C, Type, BoolLL),
    prior_log_likelihood(Type, PriorLL),
    TotalLogL is ContLL + BoolLL + PriorLL,
    Details = ll_detail(ContLL, BoolLL, PriorLL).

%% maxent_compute_profiles_indexed(+Constraints, +Context)
%  Compute empirical mean/stddev of indexed metrics for each type.
maxent_compute_profiles_indexed(Constraints, Context) :-
    forall(maxent_type(Type), (
        compute_type_profile_indexed(Constraints, Context, Type, extractiveness),
        compute_type_profile_indexed(Constraints, Context, Type, suppression),
        compute_type_profile_indexed(Constraints, Context, Type, theater)
    )).

compute_type_profile_indexed(Constraints, Context, Type, MetricName) :-
    findall(Val,
        (   member(C, Constraints),
            drl_core:dr_type(C, Context, Type),
            metric_value_indexed(C, Context, MetricName, Val)
        ),
        Values),
    (   Values = [_,_|_]
    ->  length(Values, N),
        sum_list(Values, Sum),
        Mu is Sum / N,
        foldl(sum_sq_diff(Mu), Values, 0.0, SumSqDiff),
        Variance is SumSqDiff / N,
        Sigma is max(0.01, sqrt(Variance)),
        assertz(maxent_indexed_profile(Type, MetricName, params(Mu, Sigma)))
    ;   default_profile(Type, MetricName, Params),
        assertz(maxent_indexed_profile(Type, MetricName, Params))
    ).

%% maxent_classify_all_indexed(+Constraints, +Context)
maxent_classify_all_indexed(Constraints, Context) :-
    forall(member(C, Constraints),
        maxent_classify_one_indexed(C, Context)
    ).

maxent_classify_one_indexed(C, Context) :-
    findall(Type-LogL,
        (   maxent_type(Type),
            maxent_type_log_likelihood_indexed(C, Type, Context, LogL, _)
        ),
        TypeLogLPairs),
    (   TypeLogLPairs \= []
    ->  normalize_log_probs(TypeLogLPairs, NormDist),
        apply_signature_override(C, NormDist, FinalDist),
        assertz(maxent_indexed_dist(C, Context, FinalDist))
    ;   true
    ).

%% maxent_indexed_run(+Context, -Summary)
%  Runs the indexed MaxEnt classifier using power-scaled χ for extraction
%  likelihoods. Requires prior maxent_run/2 call (for priors).
%  Stores distributions in maxent_indexed_dist/3.
maxent_indexed_run(Context, Summary) :-
    (   maxent_prior(_, _)
    ->  true
    ;   format(user_error, '[maxent_indexed] ERROR: Run maxent_run/2 first (priors needed).~n', []),
        fail
    ),
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        atom(C)
    ), RawConstraints),
    sort(RawConstraints, Constraints),
    length(Constraints, NTotal),
    format(user_error, '[maxent_indexed] Found ~w constraints, computing indexed profiles...~n', [NTotal]),

    retractall(maxent_indexed_dist(_, _, _)),
    retractall(maxent_indexed_profile(_, _, _)),
    maxent_compute_profiles_indexed(Constraints, Context),
    maxent_classify_all_indexed(Constraints, Context),

    % Summary statistics
    findall(HN, (
        maxent_indexed_dist(C2, Context, Dist),
        shannon_entropy(Dist, H),
        maxent_n_types(N),
        HMax is log(N),
        (HMax > 0 -> HN is H / HMax ; HN = 0.0)
    ), Entropies),
    (   Entropies \= []
    ->  sum_list(Entropies, SumE), length(Entropies, NE),
        MeanEntropy is SumE / NE
    ;   MeanEntropy = 0.0
    ),
    Summary = maxent_indexed_summary(NTotal, MeanEntropy),
    format(user_error, '[maxent_indexed] Done. Mean indexed entropy=~4f~n', [MeanEntropy]).

%% maxent_indexed_distribution(+C, +Context, -Distribution)
maxent_indexed_distribution(C, Context, Dist) :-
    maxent_indexed_dist(C, Context, Dist).

%% maxent_classical_vs_indexed(+C, +Context, -ClassicalDist, -IndexedDist)
%  Returns both classical (raw ε) and indexed (power-scaled χ) distributions
%  for direct comparison. Both runs must have been completed.
maxent_classical_vs_indexed(C, Context, ClassicalDist, IndexedDist) :-
    maxent_dist(C, Context, ClassicalDist),
    maxent_indexed_dist(C, Context, IndexedDist).

%% maxent_indexing_divergence(+C, +Context, -Divergence)
%  Maximum absolute probability difference between classical and indexed MaxEnt.
%  High divergence = power-scaling changes the probabilistic classification.
%  These constraints should cluster at H^1 > 0 (cohomological obstruction).
maxent_indexing_divergence(C, Context, Divergence) :-
    maxent_dist(C, Context, ClassicalDist),
    maxent_indexed_dist(C, Context, IndexedDist),
    findall(AbsDiff, (
        member(T-PC, ClassicalDist),
        member(T-PI, IndexedDist),
        AbsDiff is abs(PC - PI)
    ), Diffs),
    max_list(Diffs, Divergence).

%% maxent_indexing_divergences(+Context, +Threshold, -Divergent)
%  Batch query: all constraints where classical vs indexed MaxEnt diverge
%  above Threshold. These are the constraints where power-scaling does
%  non-trivial probabilistic work — the "quantum" cases.
maxent_indexing_divergences(Context, Threshold, Divergent) :-
    findall(C-Div, (
        maxent_indexed_dist(C, Context, _),
        maxent_indexing_divergence(C, Context, Div),
        Div > Threshold
    ), Divergent).

/* ================================================================
   SELF-TEST
   ================================================================ */

maxent_selftest :-
    format(user_error, '[maxent] Running self-test...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),
    maxent_run(Context, Summary),
    Summary = maxent_summary(NTotal, MeanEntropy, NHighUncertainty, NHard, NSoft),
    format('MaxEnt Self-Test Results:~n'),
    format('  Constraints:     ~w~n', [NTotal]),
    format('  Mean entropy:    ~4f~n', [MeanEntropy]),
    format('  High uncertainty: ~w~n', [NHighUncertainty]),
    format('  Hard disagreements: ~w~n', [NHard]),
    format('  Soft disagreements: ~w~n', [NSoft]),

    % Sanity checks
    (   NTotal > 0
    ->  format('  [OK] Constraints found~n')
    ;   format('  [FAIL] No constraints found~n')
    ),
    (   MeanEntropy >= 0.0, MeanEntropy =< 1.0
    ->  format('  [OK] Mean entropy in valid range~n')
    ;   format('  [FAIL] Mean entropy out of range~n')
    ),

    % Check a distribution sums to ~1.0
    (   maxent_dist(SampleC, Context, SampleDist)
    ->  maplist(pair_snd, SampleDist, Probs),
        sum_list(Probs, ProbSum),
        (   abs(ProbSum - 1.0) < 0.01
        ->  format('  [OK] Distribution sums to ~4f for ~w~n', [ProbSum, SampleC])
        ;   format('  [FAIL] Distribution sums to ~4f for ~w~n', [ProbSum, SampleC])
        )
    ;   format('  [WARN] No distributions computed~n')
    ),
    format('Self-test complete.~n').
