:- module(data_verification, [
    verify_all/0,
    verify_interval_completeness/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).

/* ============================================================
   1. ENTRY POINTS
   ============================================================ */

%% verify_all
%  Performs a full sweep of the loaded data against the v3.1 schema.
verify_all :-
    format('~n--- [START] v3.1 DATA INTEGRITY VERIFICATION ---~n'),
    (validate_ontology -> format('[OK] Ontology Schema matches.~n') ; format('[FAIL] Schema mismatch.~n')),
    verify_structure,
    verify_measurements,
    verify_intent_logic,
    format('--- [END] Verification Complete ---~n').

/* ============================================================
   2. STRUCTURAL & VECTOR COMPLETENESS
   ============================================================ */

%% verify_structure
%  Ensures every interval has the required 32-point coercion vector.
verify_structure :-
    forall(interval(ID, T0, Tn),
           ( format('Checking Interval: ~w (~w-~w)~n', [ID, T0, Tn]),
             verify_interval_completeness(ID)
           )).

%% verify_interval_completeness(+IntervalID)
%  Checks for the presence of all 4 components across all 4 levels at T0 and Tn.
verify_interval_completeness(ID) :-
    interval(ID, T0, Tn),
    forall(level(L),
           ( verify_vector_at(L, T0, ID),
             verify_vector_at(L, Tn, ID)
           )).

verify_vector_at(Level, Time, ID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components),
           ( measurement(_, _, Metric, Time, _)
           -> true
           ;  format('  [MISSING] ~w for Level: ~w at T: ~w in ~w~n', [Metric, Level, Time, ID]),
              fail
           )).

/* ============================================================
   3. VALUE RANGE VALIDATION
   ============================================================ */

verify_measurements :-
    forall(measurement(ID, _, _, _, Val),
           ( number(Val), Val >= 0.0, Val =< 1.0
           -> true
           ;  format('  [VALUE ERROR] Measurement ~w is outside [0,1] range.~n', [ID]),
              fail
           )).

/* ============================================================
   4. INTENT LOGIC CONSISTENCY
   ============================================================ */

verify_intent_logic :-
    % Ensure every rejected alternative was first defined as viable
    forall(intent_alternative_rejected(I, S, A),
           ( intent_viable_alternative(I, S, A)
           -> true
           ;  format('  [LOGIC ERROR] Alternative ~w rejected in ~w but never listed as viable.~n', [A, I]),
              fail
           )),
    % Ensure main beneficiary has a power change fact
    forall(intent_beneficiary_class(I, C),
           ( intent_power_change(I, C, _)
           -> true
           ;  format('  [MISSING DATA] Beneficiary ~w has no power_change delta in ~w.~n', [C, I]),
              fail
           )).
