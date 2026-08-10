% RETIRED PROTOTYPE (OQ-151 close, 2026-08-09). Archived for provenance; the
% untracked original prolog/probe_seat_sweep.pl is deleted in the same change.
% Defect/limitation: corpus-pooled per-role type histograms (no per-story
% pairing) — the pooled read inherits story typing (the 2026-08-08 pooled-H1
% lesson) and cannot ask the per-story dissent question. Superseded by
% role_type_set/3 + role_type_sets/2 (per-story, set-valued) and the
% empty_chair_state/2 detector (commit e07fba7b).
:- module(probe_seat_sweep, [run/0]).
:- use_module(stakeholder_seats).
:- use_module(grothendieck_cohomology).

stk_constraints(Cs) :-
    findall(C, ( narrative_ontology:cs_kernel_id(C,_),
                 once(narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_)) ), Cs0),
    sort(Cs0, Cs).

% every (role-seat) occurrence across the corpus -> its type (fail-open: unknown kept)
role_seat_type(Role, Type) :-
    narrative_ontology:constraint_stakeholder(C, Nm, Role, _,_,_,_),
    ( stakeholder_seats:dr_type_for_stakeholder(C, Nm, Type) -> true ; Type = unknown ).

hist(Role) :-
    findall(T, role_seat_type(Role, T), Ts),
    length(Ts, N),
    msort(Ts, S),
    setof(X, member(X, S), Types),
    format("~n~w  (n=~w seats):~n", [Role, N]),
    forall(member(X, Types),
           ( aggregate_all(count, member(X, S), K),
             P is round(1000.0*K/N)/10.0,
             format("    ~w8|~w  ~w~3+(~w%)~n", [X, K, '', P]) )),
    % extraction lean = snare+tangled_rope+piton
    aggregate_all(count, (member(X,S), memberchk(X,[snare,tangled_rope,piton])), Ext),
    aggregate_all(count, (member(X,S), memberchk(X,[rope,naturalized,scaffold,mountain])), Ben),
    EP is round(1000.0*Ext/N)/10.0, BP is round(1000.0*Ben/N)/10.0,
    format("    => extractive(snare/tangled/piton)=~w%  benign(rope/nat/scaffold/mtn)=~w%~n", [EP, BP]).

% canonical analytical context (universal, the synthetic external view) over stk constraints
analytical_hist :-
    stk_constraints(Cs),
    findall(T, ( member(C, Cs),
                 ( drl_core:dr_type(C, context(agent_power(analytical), time_horizon(immediate),
                                                exit_options(analytical), spatial_scope(global)), T) -> true ; T = unknown ) ),
            Ts),
    length(Ts, N), msort(Ts, S), setof(X, member(X,S), Types),
    format("~nANALYTICAL canonical-context (n=~w constraints):~n", [N]),
    forall(member(X, Types),
           ( aggregate_all(count, member(X,S), K), P is round(1000.0*K/N)/10.0,
             format("    ~w  ~w (~w%)~n", [X, K, P]) )).

run :-
    forall(member(R, [beneficiary, payer, excluded, observer]), hist(R)),
    catch(analytical_hist, _, true).
