% RETIRED PROTOTYPE (OQ-151 close, 2026-08-09). Archived for provenance; the
% untracked original prolog/probe_mc_cases.pl is deleted in the same change.
% TWO DEFECTS (why this was never adopted):
%   1. Gates on the POWER-gauge H1=0 (cohomological_obstruction/3, line 16) —
%      the empty-chair question lives on the seat frame, not the observer orbit.
%   2. NO is_real_type filter on the excluded set (line 20): an excluded seat
%      deriving 'unknown' counts as dissent — the witnessed 4/5-false-positive
%      trap (ISSUES OQ-151). The adopted detector (stakeholder_seats:
%      empty_chair_state/2, commit e07fba7b) fixes both: seat-frame room,
%      typed chairs only, excluded_untyped fail-open.
:- module(probe_mc_cases, [run/0]).
:- use_module(stakeholder_seats).
:- use_module(grothendieck_cohomology).

role_types(C, Role, Ts) :-
    findall(T, ( narrative_ontology:constraint_stakeholder(C, Nm, Role, _,_,_,_),
                 stakeholder_seats:dr_type_for_stakeholder(C, Nm, T) ), T0),
    sort(T0, Ts).

run :-
    findall(C, ( narrative_ontology:cs_kernel_id(C,_),
                 once(narrative_ontology:constraint_stakeholder(C,_,excluded,_,_,_,_)) ), Cs0),
    sort(Cs0, Cs),
    forall(
      ( member(C, Cs),
        grothendieck_cohomology:cohomological_obstruction(C, _, 0),   % gauge says global section
        role_types(C, excluded, Et), Et \== [],
        role_types(C, beneficiary, Bt), role_types(C, payer, Pt), role_types(C, observer, Ot),
        append([Bt,Pt,Ot], Inc0), sort(Inc0, Inc),
        member(Ev, Et), \+ member(Ev, Inc)        % excluded sees a type no included role sees
      ),
      ( ( narrative_ontology:constraint_claim(C, CT) -> true ; CT = '?' ),
        aggregate_all(count, narrative_ontology:omega_variable(C,_,_), NOmega),
        format("~n[~w]  claimed=~w  omegas=~w~n  beneficiary=~w payer=~w observer=~w  EXCLUDED=~w~n",
               [C, CT, NOmega, Bt, Pt, Ot, Et]) )
    ).
