% ============================================================================
% TEST SUITE: Individual Belief Battery
% ============================================================================
% Comprehensive tests for belief classification, extraction detection,
% and cascade analysis
% ============================================================================

:- use_module(individual_belief_battery).

:- begin_tests(belief_battery_comprehensive).

% --- TIER 0 TESTS (Universal Mountains) ---

test(tier0_logical_necessity) :-
    belief_classification(logical_necessity, mountain).

test(tier0_mathematical_truth) :-
    belief_classification(mathematical_truth, mountain).

test(tier0_physical_laws) :-
    belief_classification(physical_laws, mountain).

test(tier0_consciousness_substrate) :-
    belief_classification(consciousness_substrate, mountain).

test(tier0_death_finality) :-
    belief_classification(death_finality, mountain).

test(tier0_count_exactly_5) :-
    findall(B, (belief_classification(B, mountain), 
                \+ member(B, [mortality_acceptance])), Mountains),
    length(Mountains, 5).

% --- TIER 1 TESTS (Individual Practice) ---

test(tier1_agency_is_rope) :-
    belief_classification(meaningful_agency, rope).

test(tier1_personal_change_is_rope) :-
    belief_classification(personal_change, rope).

test(tier1_life_meaning_is_rope) :-
    belief_classification(life_meaning, rope),
    \+ belief_classification(life_meaning, mountain).

test(tier1_mortality_acceptance_is_mountain) :-
    belief_classification(mortality_acceptance, mountain).

test(tier1_count_at_least_35) :-
    findall(B, (belief_classification(B, Type), 
                Type \= mountain, 
                B \= mortality_acceptance), Practice),
    length(Practice, Count),
    Count >= 35.

% --- DEPENDENCY TESTS ---

test(mortality_depends_on_death_finality) :-
    belief_classification(death_finality, mountain),
    belief_classification(mortality_acceptance, mountain).

test(death_finality_depends_on_substrate) :-
    belief_classification(consciousness_substrate, mountain),
    belief_classification(death_finality, mountain).

% --- EXTRACTION DETECTION TESTS ---

test(eternal_soul_is_type_1_error) :-
    type_1_error(eternal_soul).

test(cosmic_purpose_is_type_1_error) :-
    type_1_error(cosmic_purpose).

test(natural_hierarchy_is_type_1_error) :-
    type_1_error(natural_hierarchy).

test(work_as_dignity_is_type_1_error) :-
    type_1_error(work_as_dignity).

test(family_sacred_is_type_1_error) :-
    type_1_error(family_sacred).

test(at_least_5_type_1_errors) :-
    findall(E, type_1_error(E), Errors),
    length(Errors, Count),
    Count >= 5.

% --- INSTITUTIONAL CLAIM TESTS ---

test(eternal_soul_claimed_as_mountain) :-
    institutional_claim(eternal_soul, mountain).

test(cosmic_purpose_claimed_as_mountain) :-
    institutional_claim(cosmic_purpose, mountain).

test(work_as_dignity_claimed_as_mountain) :-
    institutional_claim(work_as_dignity, mountain).

% --- CONFLICT DETECTION TESTS ---

test(eternal_soul_conflict) :-
    % Institutional claims it's Mountain
    institutional_claim(eternal_soul, mountain),
    % You don't accept it as Mountain
    \+ belief_classification(eternal_soul, mountain),
    % Therefore: Type I Error
    type_1_error(eternal_soul).

test(cosmic_meaning_conflict) :-
    % You classify as Rope (constructed)
    belief_classification(cosmic_meaning, rope),
    % Institution claims as Mountain
    institutional_claim(cosmic_purpose, mountain),
    % This is Type I Error
    type_1_error(cosmic_purpose).

% --- CONTEXT TESTS ---

test(my_context_exists) :-
    my_context(Ctx),
    Ctx = context(_, _, _, _).

test(my_context_has_moderate_power) :-
    my_context(context(agent_power(individual_moderate), _, _, _)).

test(my_context_biographical_timeframe) :-
    my_context(context(_, time_horizon(biographical), _, _)).

test(my_context_mobile_exit) :-
    my_context(context(_, _, exit_options(mobile), _)).

test(my_context_national_scope) :-
    my_context(context(_, _, _, spatial_scope(national))).

% --- EXPLANATION TESTS ---

test(explanations_exist) :-
    belief_explanation(mortality_acceptance, _),
    belief_explanation(meaningful_agency, _),
    belief_explanation(cosmic_meaning, _).

test(all_tier0_have_explanations) :-
    forall(
        (belief_classification(B, mountain), 
         \+ member(B, [mortality_acceptance])),
        belief_explanation(B, _)
    ).

% --- CASCADE TESTS ---

test(eternal_soul_cascade_accept) :-
    cascade_analysis(eternal_soul, accept).

test(eternal_soul_cascade_reject) :-
    cascade_analysis(eternal_soul, reject).

test(meaningful_agency_cascade_accept) :-
    cascade_analysis(meaningful_agency, accept).

test(meaningful_agency_cascade_reject) :-
    cascade_analysis(meaningful_agency, reject).

% --- CONSISTENCY TESTS ---

test(no_belief_is_both_mountain_and_rope) :-
    \+ (belief_classification(B, mountain),
        belief_classification(B, rope),
        B \= mortality_acceptance,  % This one is special (derives from T5)
        B \= death_finality).       % This one derives from T4

test(mortality_acceptance_consistent) :-
    % Should be Mountain because death_finality is Mountain
    belief_classification(death_finality, mountain),
    belief_classification(mortality_acceptance, mountain).

test(no_mountains_claimed_as_ropes) :-
    % Your Mountains shouldn't be institutional Ropes
    \+ (belief_classification(B, mountain),
        institutional_claim(B, rope)).

% --- COMPLETENESS TESTS ---

test(all_sections_represented) :-
    % Check that we have beliefs from all 13 sections
    belief_classification(meaningful_agency, _),    % Section 1
    belief_classification(life_meaning, _),         % Section 2
    belief_classification(family_obligations, _),   % Section 3
    belief_classification(work_value, _),           % Section 4
    belief_classification(belief_norms, _),         % Section 5
    belief_classification(power_structures_natural, _), % Section 6
    belief_classification(mortality_acceptance, _), % Section 7
    belief_classification(trust_default, _),        % Section 8
    belief_classification(social_conformity, _),    % Section 9
    belief_classification(consumption_level, _),    % Section 10
    belief_classification(specialization, _),       % Section 11
    belief_classification(electoral_participation, _), % Section 12
    belief_classification(optimism_pessimism, _).   % Section 13

% --- REGRESSION TESTS ---

test(gita_insight_encoded) :-
    % The key insight from Gita analysis should be encoded
    % Eternal soul is Snare, not Mountain
    type_1_error(eternal_soul),
    institutional_claim(eternal_soul, mountain),
    \+ belief_classification(eternal_soul, mountain).

test(mortality_urgency_encoded) :-
    % Accepting mortality should imply urgency
    belief_classification(mortality_acceptance, mountain),
    belief_explanation(mortality_acceptance, Exp),
    sub_string(Exp, _, _, _, 'urgency').

test(constructed_meaning_encoded) :-
    % Life meaning should be constructed, not discovered
    belief_classification(life_meaning, rope),
    belief_explanation(life_meaning, Exp),
    sub_string(Exp, _, _, _, 'constructed').

% --- UTILITY FUNCTION TESTS ---

test(belief_count_works) :-
    belief_count(mountain),
    belief_count(rope),
    belief_count(varies).

% --- INTEGRATION TESTS ---

test(all_type_1_errors_have_institutional_claims) :-
    forall(
        type_1_error(B),
        institutional_claim(B, _)
    ).

test(all_type_1_errors_rejected_as_mountains) :-
    forall(
        type_1_error(B),
        \+ belief_classification(B, mountain)
    ).

:- end_tests(belief_battery_comprehensive).

% --- RUN ALL TESTS ---

run_all_tests :-
    format('~n╔══════════════════════════════════════════════════════════════╗~n'),
    format('║        INDIVIDUAL BELIEF BATTERY TEST SUITE                 ║~n'),
    format('╚══════════════════════════════════════════════════════════════╝~n~n'),
    run_tests(belief_battery_comprehensive),
    format('~n').

% Quick test command
:- initialization(run_all_tests).
