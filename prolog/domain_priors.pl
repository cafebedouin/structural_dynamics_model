:- module(domain_priors, [
    get_prior/3,
    is_known_domain/1,
    flag_novelty/1,
    expected_signature/2,
    should_be_natural_law/1,
    validate_signature/2,
    category_of/2
]).

:- use_module(domain_registry).
:- use_module(drl_core).

% 1. Suppress discontiguous warnings by declaring them at the top
:- discontiguous expected_signature/2.
:- discontiguous validate_signature/2.
:- discontiguous infer_category_from_priors/2.
:- discontiguous is_known_domain/1.
:- discontiguous flag_novelty/1.
:- discontiguous get_prior/3.

:- multifile
    drl_core:base_extractiveness/2,
    drl_core:suppression_score/2,
    drl_core:requires_active_enforcement/1,
    drl_core:emerges_naturally/1.

drl_core:base_extractiveness(_, _) :- fail.
drl_core:suppression_score(_, _) :- fail.
drl_core:requires_active_enforcement(_) :- fail.
drl_core:emerges_naturally(_) :- fail.

/**
 * DOMAIN PRIORS MODULE - v3.2.4 Hardened
 * Resolves redefinition warnings and restores missing API procedures.
 */

%% ============================================================================
%% 1. CATEGORY PROFILES
%% ============================================================================
category_profile(physical_natural,    [1.00, 1.00, 0.00, 0.00]).  % Mountain
category_profile(formal_logic,        [0.90, 0.20, 0.10, 0.10]).  % Mountain
category_profile(statutory_formal,    [0.80, 0.50, 0.70, 0.40]).  % Rope
category_profile(election_cycle,       [0.80, 0.80, 0.30, 0.50]). % Periodic Rope
category_profile(extractive_market, [0.40, 0.80, 0.68, 0.60]).    % Calibrated Noose
category_profile(narrative_history,  [0.6, 0.7, 0.41, 0.6]).      % Calibrated Rope
category_profile(unknown_novel,      [0.55, 0.73, 0.52, 0.6]).    % Fleet Baseline
category_profile(mandatrophy_collapse, [0.20, 0.95, 0.90, 0.30]). % Terminal State

%% ============================================================================
%% 2. API DEFINITIONS
%% ============================================================================

is_known_domain(ID) :- domain_category(ID, _), !.
is_known_domain(ID) :- base_extractiveness(ID, _), !.
is_known_domain(ID) :- suppression_score(ID, _), !.
is_known_domain(ID) :- narrative_ontology:constraint_claim(ID, _), !.

% Flag Novelty (Clears v3_1_data_repair warnings)
flag_novelty(ID) :-
    \+ is_known_domain(ID),
    format('! NOTICE: Novel Domain "~w" detected. Using neutral (0.5) priors.~n', [ID]).
flag_novelty(_).

get_prior(ID, Metric, Value) :-
    map_metric_to_hook(Metric, Hook),
    call(domain_priors:Hook, ID, Value), !.

get_prior(ID, Metric, Value) :-
    category_of(ID, Cat),
    category_profile(Cat, Vector),
    map_metric_to_vector_pos(Metric, Vector, Value), !.

get_prior(_, _, 0.5).

category_of(ID, Cat) :- domain_registry:domain_category(ID, Cat), !.
category_of(ID, physical_natural) :-
    (narrative_ontology:constraint_claim(ID, natural_law) ;
     narrative_ontology:constraint_claim(ID, physical_law)), !.
category_of(_, unknown_novel).

% Signature Support (Clears exported-procedure errors)
should_be_natural_law(ID) :-
    category_of(ID, Cat),
    expected_signature(Cat, natural_law).

expected_signature(physical_natural, natural_law).
expected_signature(formal_logic,     natural_law).
expected_signature(election_cycle,   constructed_constraint).
expected_signature(statutory_formal, constructed_constraint).
expected_signature(extractive_market, constructed_constraint).
expected_signature(narrative_history, constructed_constraint).
expected_signature(unknown_novel,    ambiguous).

validate_signature(ID, Detected) :-
    category_of(ID, Cat),
    expected_signature(Cat, Expected),
    ( Detected = Expected
    -> format('[VALIDATION] ✓ ~w: ~w matches ~w~n', [ID, Detected, Cat])
    ;  format('[VALIDATION] ✗ ~w: Expected ~w, got ~w~n', [ID, Expected, Detected])).

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

%% ============================================================================
%% 4. SIGNATURE
%% ============================================================================
expected_signature(physical_natural, natural_law).
expected_signature(formal_logic,     natural_law).
expected_signature(election_cycle,   constructed_constraint).
expected_signature(statutory_formal, constructed_constraint).
expected_signature(extractive_market, constructed_constraint).
expected_signature(narrative_history, constructed_constraint).
expected_signature(unknown_novel,    ambiguous).

validate_signature(ID, Detected) :-
    category_of(ID, Cat),
    expected_signature(Cat, Expected),
    (   Detected = Expected
    ->  format('[VALIDATION] ✓ ~w: ~w matches ~w~n', [ID, Detected, Cat])
    ;   format('[VALIDATION] ✗ ~w: Expected ~w, got ~w~n', [ID, Expected, Detected])
    ).

%% flag_novelty(+ID)
%  Logs a domain that doesn't match existing priors for later calibration.
flag_novelty(ID) :-
    format('  [ALERT] Novel domain detected: ~w. Queuing for calibration.~n', [ID]),
    assertz(attribute(ID, discovery_status, novel)).

%% get_prior(+ID, +Metric, -Value)
%  Retrieves the baseline value based on the domain's category profile.
get_prior(ID, Metric, Value) :-
    category_of(ID, Category),
    category_profile(Category, Vector),
    map_metric_to_vector_pos(Metric, Vector, Value), !.
get_prior(_, _, 0.5). % Default neutral prior
