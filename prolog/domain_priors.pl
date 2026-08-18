:- module(domain_priors, [
    get_prior/3,
    is_known_domain/1,
    flag_novelty/1,
    category_of/2
]).

% RETIRED 2026-08-18 (OQ-296 D3): expected_signature/2, should_be_natural_law/1
% and validate_signature/2 were removed — dead in BOTH senses (0 firings AND 0
% consumers repo-wide). The authored 7-row expectation table is preserved
% verbatim in docs/design/design_gaps.md as a declared-absent capability, so the
% content survives the code. See that entry before re-minting anything similar.

% OQ-96 (2026-06-10; CLOSED 2026-06-11 with the OQ-93 shim retirement):
% `:- use_module(domain_registry).` REMOVED — the module was deleted
% 2026-02-18 (commit e7ae13fb) and the directive warned
% `source_sink 'domain_registry' does not exist` at every load for four
% months, hidden by the universal `grep -v Warning` habit (the
% load_warning_gate now guards that channel). The throw-only
% category_of/2 clause 1 and is_known_domain/1 clause 1 were removed the
% same day (history at category_of/2 below); the grid imputation walk that
% reached them was retired permanently with grid_shim_enabled (OQ-93
% ruling (b)).
:- use_module(drl_core, []).

:- multifile
    drl_core:base_extractiveness/2,
    drl_core:suppression_score/2,
    drl_core:requires_active_enforcement/1,
    drl_core:emerges_naturally/1,
    base_extractiveness/2,
    suppression_score/2,
    requires_active_enforcement/1,
    emerges_naturally/1.

drl_core:base_extractiveness(_, _) :- fail.
drl_core:suppression_score(_, _) :- fail.
drl_core:requires_active_enforcement(_) :- fail.
drl_core:emerges_naturally(_) :- fail.

%% ============================================================================
%% 1. CATEGORY PROFILES
%% ============================================================================
category_profile(physical_natural,    [1.00, 1.00, 0.00, 0.00]).  % Mountain
category_profile(formal_logic,        [0.90, 0.20, 0.10, 0.10]).  % Mountain
category_profile(statutory_formal,    [0.80, 0.50, 0.70, 0.40]).  % Rope
category_profile(election_cycle,      [0.80, 0.80, 0.30, 0.50]).  % Periodic Rope
category_profile(extractive_market,   [0.40, 0.80, 0.68, 0.60]).  % Calibrated Snare
category_profile(narrative_history,   [0.6, 0.7, 0.41, 0.6]).     % Calibrated Rope
category_profile(unknown_novel,       [0.55, 0.73, 0.52, 0.6]).   % Fleet Baseline
category_profile(mandatrophy_collapse,[0.20, 0.95, 0.90, 0.30]).  % Terminal State

%% ============================================================================
%% 2. API DEFINITIONS
%% ============================================================================

% OQ-96: `is_known_domain(ID) :- domain_category(ID, _), !.` removed — same
% dead reference as category_of clause 1 (throw-only since 2026-02-18).
is_known_domain(ID) :- base_extractiveness(ID, _), !.
is_known_domain(ID) :- suppression_score(ID, _), !.
is_known_domain(ID) :- narrative_ontology:constraint_claim(ID, _), !.

%% flag_novelty(+ID)
%  Logs a domain that doesn't match existing priors for later calibration.
flag_novelty(ID) :-
    \+ is_known_domain(ID),
    format('! NOTICE: Novel Domain "~w" detected. Using neutral (0.5) priors.~n', [ID]).
flag_novelty(_).

%% get_prior(+ID, +Metric, -Value)
%  Retrieves the baseline value: first tries domain-specific hooks,
%  then falls back to category profile, then neutral 0.5 default.
get_prior(ID, Metric, Value) :-
    map_metric_to_hook(Metric, Hook),
    call(domain_priors:Hook, ID, Value), !.
get_prior(ID, Metric, Value) :-
    category_of(ID, Cat),
    category_profile(Cat, Vector),
    map_metric_to_vector_pos(Metric, Vector, Value), !.
get_prior(_, _, 0.5).

%% category_of(+ID, -Category)
%  Determines the domain category for a constraint ID.
% OQ-96 (2026-06-10): the registry clause
%   category_of(ID, Cat) :- domain_registry:domain_category(ID, Cat), !.
% was REMOVED. The module was deleted 2026-02-18 (e7ae13fb), making the clause
% THROW-ONLY for four months — it could never succeed, so removing it preserves
% every behavior any caller ever observed and removes only the existence_error.
% Witnessed reaching it on the suite path TWICE before removal: (1) the repair
% imputation walk (Polaris story; that walk was retired permanently
% 2026-06-11 with the grid_shim_enabled flag, OQ-93 ruling (b)), and
% (2) data_validation:is_complete_constraint/1 (suite CHECK 1) — the second
% found only because fixing the first let the suite run further. Fallbacks
% below (constraint_claim -> physical_natural; else unknown_novel) are now the
% whole of category_of/2. is_known_domain/1 clause 1 (domain_category/2,
% same dead reference, unqualified) removed for the same reason.
category_of(ID, physical_natural) :-
    (narrative_ontology:constraint_claim(ID, natural_law) ;
     narrative_ontology:constraint_claim(ID, physical_law)), !.
category_of(_, unknown_novel).

%% ============================================================================
%% 3. INTERNAL HELPERS
%% ============================================================================

map_metric_to_hook(base_extractiveness(_), base_extractiveness).
map_metric_to_hook(extractiveness,         base_extractiveness).
map_metric_to_hook(suppression(_),          suppression_score).
map_metric_to_hook(suppression_requirement, suppression_score).

map_metric_to_vector_pos(accessibility_collapse(_), [A,_,_,_], A).
map_metric_to_vector_pos(stakes_inflation(_),      [_,S,_,_], S).
map_metric_to_vector_pos(suppression(_),           [_,_,U,_], U).
map_metric_to_vector_pos(resistance(_),            [_,_,_,R], R).

infer_category_from_priors(ID, extractive_market) :-
    base_extractiveness(ID, E), E > 0.6, !.
infer_category_from_priors(ID, statutory_formal) :-
    requires_active_enforcement(ID), !.
