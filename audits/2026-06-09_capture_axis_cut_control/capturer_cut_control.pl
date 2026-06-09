% ============================================================================
% capturer_cut_control.pl — Step 1 discriminating control (plan jaunty-juggling-wozniak)
% Tests has_computed_capturer/1 against 4 pre-registered seat-sets.
% Run from prolog/:  swipl -g run_capturer_control -t halt capturer_cut_control.pl
% Pre-registration + verdict mapping: audits/2026-06-09_capture_axis_cut_control/PREREGISTRATION.md
% ============================================================================

:- [stack].
:- use_module(stakeholder_seats).
:- use_module(library(lists)).

% --- favorable: the seat does NOT read extraction-against-itself, nor honest-unknown ---
favorable(T) :- member(T, [rope, mountain, scaffold, naturalized]).

% --- The candidate cut under test ---
has_computed_capturer(C) :-
    stakeholder_seats:role_of(C, Name, R),
    stakeholder_seats:beneficiary_side(R),
    stakeholder_seats:dr_type_for_stakeholder(C, Name, Type),
    favorable(Type), !.

% --- raw metric type for a seat (signature layer NOT integrated) — for mechanism transparency ---
raw_metric_type(C, Name, MetricType) :-
    stakeholder_seats:stakeholder_context(C, Name, Ctx),
    stakeholder_seats:derive_directionality_for_stakeholder(C, Name, D),
    drl_core:base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, Chi),
    drl_core:get_raw_suppression(C, Supp),
    drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, MetricType).

% --- fact construction ---------------------------------------------------------
assert_common_metrics(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.75)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.65)),
    assertz(narrative_ontology:constraint_metric(C, theater_ratio, 0.20)).
    % NB: accessibility_collapse + resistance deliberately omitted -> signature=unknown

% seat: constraint_stakeholder(C, Name, Role, Power, T, E, S)
seat(C, Name, Role, Power) :-
    assertz(narrative_ontology:constraint_stakeholder(C, Name, Role, Power, biographical, mobile, national)).

build_cases :-
    % (a) genuine capturer
    assert_common_metrics(cap_a),
    seat(cap_a, payer_a, payer, powerless),
    seat(cap_a, capturer_a, beneficiary, institutional),
    assertz(narrative_ontology:constraint_beneficiary(cap_a, capturer_a)),
    % (b) mild-favorable non-capturer (no constraint_beneficiary)
    assert_common_metrics(mild_b),
    seat(mild_b, payer_b, payer, powerless),
    seat(mild_b, bystander_b, beneficiary, institutional),
    % (c) DMV easy case: payer + excluded only, NO beneficiary-side seat
    assert_common_metrics(dmv_c),
    seat(dmv_c, payer_c, payer, powerless),
    seat(dmv_c, excluded_c, excluded, powerless),
    % (d) realistic DMV: agenda_setter present, no constraint_beneficiary
    assert_common_metrics(dmv_designed),
    seat(dmv_designed, payer_d, payer, powerless),
    seat(dmv_designed, admin_d, agenda_setter, institutional).

% --- reporting -----------------------------------------------------------------
report_seat(C, Name) :-
    stakeholder_seats:role_of(C, Name, Role),
    ( stakeholder_seats:beneficiary_side(Role) -> Cand = candidate ; Cand = 'not-candidate' ),
    ( raw_metric_type(C, Name, Raw) -> true ; Raw = '<none>' ),
    ( stakeholder_seats:dr_type_for_stakeholder(C, Name, Final) -> true ; Final = '<none>' ),
    ( favorable(Final) -> Fav = favorable ; Fav = unfavorable ),
    format("    seat=~w role=~w | ~w | raw_metric=~w final_type=~w (~w)~n",
           [Name, Role, Cand, Raw, Final, Fav]).

candidate_seats(C, Cands) :-
    findall(N, ( stakeholder_seats:role_of(C, N, R), stakeholder_seats:beneficiary_side(R) ), Cs),
    sort(Cs, Cands).

report_case(C) :-
    format("CASE ~w~n", [C]),
    findall(N, narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _), Names0),
    sort(Names0, Names),
    forall(member(N, Names), report_seat(C, N)),
    candidate_seats(C, Cands),
    ( has_computed_capturer(C) -> Cut = true ; Cut = false ),
    format("  RESULT ~w | candidate_set=~w | cut=~w~n~n", [C, Cands, Cut]).

run_capturer_control :-
    build_cases,
    cache_registry:clear_all_caches,
    nl,
    format("==== Step 1 capturer-cut discriminating control ====~n~n", []),
    forall(member(C, [cap_a, mild_b, dmv_c, dmv_designed]), report_case(C)),
    format("==== verdict mapping: see PREREGISTRATION.md (cut true on (a)&(b) => Outcome 2 HALT) ====~n", []).
