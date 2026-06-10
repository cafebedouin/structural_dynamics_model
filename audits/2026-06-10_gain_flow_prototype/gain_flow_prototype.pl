% ============================================================================
% gain_flow_prototype.pl — OQ-92 step-2 prototype: authored gain-flow + fixing-cost
% reads against the eight-control battery. Prototype-only predicates; NO production
% file is touched. Run from prolog/:
%   swipl -g run_gain_flow_prototype -t halt ../audits/2026-06-10_gain_flow_prototype/gain_flow_prototype.pl
% Pre-registration + verdict mapping: PREREGISTRATION.md (same dir; committed before the run).
% Cloned from audits/2026-06-09_capture_axis_cut_control/capturer_cut_control.pl.
% ============================================================================

:- [stack].
:- use_module(stakeholder_seats).
:- use_module(library(lists)).

% Prototype-only authored surface (would be schema fields in step 3).
:- dynamic stakeholder_gain_flow/2.   % stakeholder_gain_flow(C, SeatName | diffuse); ABSENT = third value
:- dynamic fixing_cost_class/2.       % fixing_cost_class(C, cheap | prohibitive)

% --- The reads under test (tri-valued provenance design, OQ-92 Rulings block) ---
seat_captures(C, Name) :-
    stakeholder_gain_flow(C, Name),
    Name \== diffuse,
    stakeholder_seats:role_of(C, Name, _).   % authored gain must name an EXISTING seat

uncaptured(C) :-
    stakeholder_gain_flow(C, diffuse).        % positive authored negative, never NAF

piton_candidate(C)   :- uncaptured(C), fixing_cost_class(C, prohibitive).
transient_neglect(C) :- uncaptured(C), fixing_cost_class(C, cheap).
% captured + ANY fixing_cost_class stays snare-flavored (no demotion read exists);
% absent gain_flow -> none of the above fires (fail-closed).

% --- fact construction (06-09 pattern: signature-layer hold-out) -------------
assert_common_metrics(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.75)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.65)),
    assertz(narrative_ontology:constraint_metric(C, theater_ratio, 0.20)).
    % NB: accessibility_collapse + resistance deliberately omitted -> signature=unknown

% seat: constraint_stakeholder(C, Name, Role, Power, T, E, S)
seat(C, Name, Role, Power) :-
    assertz(narrative_ontology:constraint_stakeholder(C, Name, Role, Power, biographical, mobile, national)).

build_cases :-
    % 1. cap_a — genuine capturer (06-09 case (a) + authored surface)
    assert_common_metrics(cap_a),
    seat(cap_a, payer_a, payer, powerless),
    seat(cap_a, capturer_a, beneficiary, institutional),
    assertz(narrative_ontology:constraint_beneficiary(cap_a, capturer_a)),
    assertz(stakeholder_gain_flow(cap_a, capturer_a)),
    assertz(fixing_cost_class(cap_a, prohibitive)),
    % 2. mild_b — the 06-09 false-positive case, now authored diffuse
    assert_common_metrics(mild_b),
    seat(mild_b, payer_b, payer, powerless),
    seat(mild_b, bystander_b, beneficiary, institutional),
    assertz(stakeholder_gain_flow(mild_b, diffuse)),
    assertz(fixing_cost_class(mild_b, prohibitive)),
    % 3. dmv_c — DMV easy case
    assert_common_metrics(dmv_c),
    seat(dmv_c, payer_c, payer, powerless),
    seat(dmv_c, excluded_c, excluded, powerless),
    assertz(stakeholder_gain_flow(dmv_c, diffuse)),
    assertz(fixing_cost_class(dmv_c, prohibitive)),
    % 4. dmv_designed — designed-but-uncaptured with agenda_setter
    assert_common_metrics(dmv_designed),
    seat(dmv_designed, payer_d, payer, powerless),
    seat(dmv_designed, admin_d, agenda_setter, institutional),
    assertz(stakeholder_gain_flow(dmv_designed, diffuse)),
    assertz(fixing_cost_class(dmv_designed, prohibitive)),
    % 5. cheap_fix_e — transient neglect: identical to 4 except fixing_cost class
    assert_common_metrics(cheap_fix_e),
    seat(cheap_fix_e, payer_e, payer, powerless),
    seat(cheap_fix_e, admin_e, agenda_setter, institutional),
    assertz(stakeholder_gain_flow(cheap_fix_e, diffuse)),
    assertz(fixing_cost_class(cheap_fix_e, cheap)),
    % 6. captured_cheap_f — the fourth cell: captured + cheap, must NOT demote
    assert_common_metrics(captured_cheap_f),
    seat(captured_cheap_f, payer_f, payer, powerless),
    seat(captured_cheap_f, capturer_f, beneficiary, institutional),
    assertz(narrative_ontology:constraint_beneficiary(captured_cheap_f, capturer_f)),
    assertz(stakeholder_gain_flow(captured_cheap_f, capturer_f)),
    assertz(fixing_cost_class(captured_cheap_f, cheap)),
    % 7. absent_g — structural twin of mild_b, NO authored surface (fail-closed register)
    assert_common_metrics(absent_g),
    seat(absent_g, payer_g, payer, powerless),
    seat(absent_g, bystander_g, beneficiary, institutional),
    % 8. malformed_h — gain_flow names a NONEXISTENT seat (decided absorption default)
    assert_common_metrics(malformed_h),
    seat(malformed_h, payer_h, payer, powerless),
    seat(malformed_h, capturer_h, beneficiary, institutional),
    assertz(stakeholder_gain_flow(malformed_h, ghost_seat_h)),
    assertz(fixing_cost_class(malformed_h, prohibitive)).

% --- reporting -----------------------------------------------------------------
% per-seat engine type: interference check only (signature held out -> raw metric type)
report_seat(C, Name) :-
    stakeholder_seats:role_of(C, Name, Role),
    ( stakeholder_seats:dr_type_for_stakeholder(C, Name, Final) -> true ; Final = '<none>' ),
    format("    seat=~w role=~w final_type=~w~n", [Name, Role, Final]).

authored_gain(C, G)  :- ( stakeholder_gain_flow(C, G0)  -> G = G0 ; G = 'ABSENT' ).
authored_cost(C, F)  :- ( fixing_cost_class(C, F0)      -> F = F0 ; F = 'ABSENT' ).
bool(Goal, true)  :- call(Goal), !.
bool(_,    false).

report_case(C) :-
    format("CASE ~w~n", [C]),
    findall(N, narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _), Names0),
    sort(Names0, Names),
    forall(member(N, Names), report_seat(C, N)),
    authored_gain(C, G), authored_cost(C, F),
    findall(N, seat_captures(C, N), Caps),
    bool(uncaptured(C), U),
    bool(piton_candidate(C), P),
    bool(transient_neglect(C), T),
    ( (Caps \== [] ; U == true ; P == true ; T == true) -> Fires = yes ; Fires = no ),
    format("  authored: gain_flow=~w fixing_cost=~w~n", [G, F]),
    format("  RESULT ~w | seat_captures=~w uncaptured=~w piton_candidate=~w transient_neglect=~w | fires_any=~w~n~n",
           [C, Caps, U, P, T, Fires]).

run_gain_flow_prototype :-
    build_cases,
    cache_registry:clear_all_caches,
    nl,
    format("==== OQ-92 step-2 gain-flow prototype: eight-control battery ====~n~n", []),
    forall(member(C, [cap_a, mild_b, dmv_c, dmv_designed, cheap_fix_e,
                      captured_cheap_f, absent_g, malformed_h]),
           report_case(C)),
    format("==== verdict mapping: PREREGISTRATION.md (pairs 2<->7 and 1<->8; case 5 is the fixing_cost load-bearing test) ====~n", []).
