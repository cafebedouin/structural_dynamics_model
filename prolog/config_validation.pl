:- module(config_validation, [
    validate_config/0,
    validate_config_postcorpus/0
]).

:- use_module(config).
:- use_module(config_schema).
:- discontiguous config_violation/1.

/* ================================================================
   CONFIG VALIDATION — Schema-driven param/2 constraint checking

   All validation is driven by config_schema.pl:
     param_spec(Name, Type, Constraint, Description)
     param_relationship(Name, ViolationGoal, Description)

   Checks performed:
     1. Missing params  — declared in schema but absent from config
     2. Unknown params  — present in config but absent from schema
     3. Type mismatches — value doesn't match declared type
     4. Constraint violations — value outside declared range/set
     5. Relationship violations — cross-param invariants broken

   On failure: prints all violations and halts with exit code 1.

   Adding a new config parameter:
     1. Add param(Name, Default) to config.pl
     2. Add param_spec(Name, Type, Constraint, Desc) to config_schema.pl
     Validation is automatic — no changes needed here.
   ================================================================ */

%% validate_config/0
%  Checks all param/2 facts against the schema.
%  Collects ALL violations, reports them, and halts on failure.
validate_config :-
    findall(Msg, config_violation(Msg), Violations),
    (   Violations == []
    ->  true
    ;   length(Violations, N),
        forall(member(V, Violations),
               print_message(error, format("~w", [V]))),
        format(user_error, "~n~w config violation(s) found. Halting.~n", [N]),
        halt(1)
    ).

%% validate_config_postcorpus/0
%  Post-testset-load safety net. Same checks, with file logging.
%  The Makefile pipeline redirects stderr to /dev/null (2>/dev/null),
%  so violations are written to config_violations.log for visibility.
%  Halts on failure so Make sees the nonzero exit code.
validate_config_postcorpus :-
    findall(Msg, config_violation(Msg), Violations),
    (   Violations == []
    ->  true
    ;   length(Violations, N),
        %% Write to file — survives 2>/dev/null
        open('config_violations.log', write, S),
        format(S, "CONFIG VIOLATIONS DETECTED AFTER CORPUS LOAD (~w):~n", [N]),
        forall(member(V, Violations), format(S, "  ~w~n", [V])),
        close(S),
        %% Also write to stderr (visible in interactive use)
        forall(member(V, Violations),
               print_message(error, format("~w", [V]))),
        format(user_error,
               "~n~w config violation(s) after corpus load. See config_violations.log. Halting.~n",
               [N]),
        halt(1)
    ).

% ============================================================
% 1. Missing param — declared in schema but absent from config
% ============================================================

config_violation(Msg) :-
    config_schema:param_spec(Name, _, _, _),
    \+ config:param(Name, _),
    format(atom(Msg),
           'CONFIG ERROR: required param ~w is missing',
           [Name]).

% ============================================================
% 2. Unknown param — present in config but absent from schema
% ============================================================

config_violation(Msg) :-
    config:param(Name, _),
    \+ config_schema:param_spec(Name, _, _, _),
    format(atom(Msg),
           'CONFIG ERROR: param ~w exists in config but has no schema spec',
           [Name]).

% ============================================================
% 3. Type mismatch
% ============================================================

config_violation(Msg) :-
    config_schema:param_spec(Name, Type, _, _),
    config:param(Name, Value),
    \+ type_ok(Type, Value),
    format(atom(Msg),
           'CONFIG ERROR: param(~w, ~w) has wrong type (expected ~w)',
           [Name, Value, Type]).

type_ok(number, V)  :- number(V).
type_ok(integer, V) :- integer(V).
type_ok(atom, V)    :- atom(V).

% ============================================================
% 4. Constraint violation
% ============================================================

config_violation(Msg) :-
    config_schema:param_spec(Name, Type, Constraint, _),
    Constraint \= any,
    config:param(Name, Value),
    type_ok(Type, Value),
    \+ constraint_ok(Constraint, Value),
    format(atom(Msg),
           'CONFIG ERROR: param(~w, ~w) violates constraint ~w',
           [Name, Value, Constraint]).

constraint_ok(any, _).
constraint_ok(range(Lo, Hi), V) :- V >= Lo, V =< Hi.
constraint_ok(positive, V)      :- V > 0.
constraint_ok(non_positive, V)  :- V =< 0.
constraint_ok(oneof(List), V)   :- member(V, List).

% ============================================================
% 5. Relationship violation
% ============================================================

config_violation(Msg) :-
    config_schema:param_relationship(_, Goal, Description),
    catch(call(Goal), _, fail),
    format(atom(Msg),
           'CONFIG ERROR: relationship violated: ~w',
           [Description]).

% ============================================================
% Fire on module load
% ============================================================

:- initialization(validate_config).

% ============================================================
% Tests
% ============================================================

:- begin_tests(config_validation_tests).

%% TEST 1: Range violation — numeric value outside [0.0, 1.0]
test(catches_range_violation) :-
    config:param(boltzmann_coupling_threshold, Original),
    retractall(config:param(boltzmann_coupling_threshold, _)),
    assertz(config:param(boltzmann_coupling_threshold, 1.5)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(boltzmann_coupling_threshold, _)),
    assertz(config:param(boltzmann_coupling_threshold, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'violates constraint'))).

%% TEST 2: Negative range violation
test(catches_negative_range_violation) :-
    config:param(dependency_coupling_threshold, Original),
    retractall(config:param(dependency_coupling_threshold, _)),
    assertz(config:param(dependency_coupling_threshold, -0.1)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(dependency_coupling_threshold, _)),
    assertz(config:param(dependency_coupling_threshold, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'violates constraint'))).

%% TEST 3: Positive divisor violation
test(catches_positive_violation) :-
    config:param(excess_factor_sigma, Original),
    retractall(config:param(excess_factor_sigma, _)),
    assertz(config:param(excess_factor_sigma, 0.0)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(excess_factor_sigma, _)),
    assertz(config:param(excess_factor_sigma, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'violates constraint'))).

%% TEST 4: Ordering/relationship violation — purity tiers
test(catches_ordering_violation) :-
    config:param(purity_action_sound_floor, Original),
    retractall(config:param(purity_action_sound_floor, _)),
    assertz(config:param(purity_action_sound_floor, 0.10)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(purity_action_sound_floor, _)),
    assertz(config:param(purity_action_sound_floor, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'relationship violated'))).

%% TEST 5: Gaussian consistency violation — floor > peak
test(catches_gaussian_violation) :-
    config:param(excess_factor_floor, OrigFloor),
    config:param(excess_factor_peak, OrigPeak),
    retractall(config:param(excess_factor_floor, _)),
    retractall(config:param(excess_factor_peak, _)),
    assertz(config:param(excess_factor_floor, 0.9)),
    assertz(config:param(excess_factor_peak, 0.5)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(excess_factor_floor, _)),
    retractall(config:param(excess_factor_peak, _)),
    assertz(config:param(excess_factor_floor, OrigFloor)),
    assertz(config:param(excess_factor_peak, OrigPeak)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'relationship violated'))).

%% TEST 6: Valid config passes — no violations
test(valid_config_passes) :-
    findall(_, config_violation(_), Violations),
    length(Violations, 0).

%% TEST 7: Type violation — atom where number expected
test(catches_type_violation) :-
    config:param(sigmoid_steepness, Original),
    retractall(config:param(sigmoid_steepness, _)),
    assertz(config:param(sigmoid_steepness, fast)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(sigmoid_steepness, _)),
    assertz(config:param(sigmoid_steepness, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'wrong type'))).

%% TEST 8: Missing param detection
test(catches_missing_param) :-
    config:param(fpn_epsilon, Original),
    retractall(config:param(fpn_epsilon, _)),
    findall(Msg, config_violation(Msg), Violations),
    assertz(config:param(fpn_epsilon, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'missing'))).

%% TEST 9: Relationship violation — sigmoid ordering
test(catches_sigmoid_ordering_violation) :-
    config:param(sigmoid_lower, OrigL),
    config:param(sigmoid_upper, OrigU),
    retractall(config:param(sigmoid_lower, _)),
    retractall(config:param(sigmoid_upper, _)),
    assertz(config:param(sigmoid_lower, 2.0)),
    assertz(config:param(sigmoid_upper, 0.5)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(sigmoid_lower, _)),
    retractall(config:param(sigmoid_upper, _)),
    assertz(config:param(sigmoid_lower, OrigL)),
    assertz(config:param(sigmoid_upper, OrigU)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'sigmoid'))).

%% TEST 10: Binary flag violation — integer not 0 or 1
test(catches_binary_flag_violation) :-
    config:param(cohomology_enabled, Original),
    retractall(config:param(cohomology_enabled, _)),
    assertz(config:param(cohomology_enabled, 3)),
    findall(Msg, config_violation(Msg), Violations),
    retractall(config:param(cohomology_enabled, _)),
    assertz(config:param(cohomology_enabled, Original)),
    once((member(V, Violations), sub_atom(V, _, _, _, 'violates constraint'))).

:- end_tests(config_validation_tests).
