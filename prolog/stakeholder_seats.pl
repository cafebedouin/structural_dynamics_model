% ============================================================================
% STAKEHOLDER SEATS — per-(C,Name) observer layer (OQ-83 Phase A step 3)
% ============================================================================
% Consumes the stakeholder facts compiled from stakeholders[]/six_questions
% (narrative_ontology:constraint_stakeholder/7 etc., step 2) and gives each
% NAMED stakeholder its own d, χ, and computed type — escaping the atom-keyed
% collapse (A2: two same-power opposed agents were one coordinate).
%
% No forked bodies (Pattern 2): χ and classification run through the canonical
% d-parameterized variants — constraint_indexing:extractiveness_for_agent_d/4
% and drl_core:dr_type_with_d/4 — added in the same pass.
%
% Role->d base values are config params (stakeholder_role_d_*), a DECLARED,
% fitness-chosen seat (see config.pl comment). PRIMARY role only feeds d
% (declared simplification; secondary_role feeds contention + the compiler's
% beneficiary/victim derivation, not d).
%
% Commentary-grade outputs (R3): consensus_provenance/2, zombie_piton_-
% crosscheck/2, seat_perceived_vs_real/4 annotate; NOTHING here overrides
% classification — an authored absence (excluded seats, missing genealogy)
% never drives a type.
% ============================================================================

:- module(stakeholder_seats, [
    stakeholder_context/3,
    derive_directionality_for_stakeholder/3,
    stakeholder_d_override/3,
    dr_type_for_stakeholder/3,
    chi_for_stakeholder/3,
    in_contention/3,
    seat_perceived_vs_real/4,
    consensus_provenance/2,
    zombie_piton_crosscheck/2
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(drl_core).

% Per-(C,Name) override — probe surface (mirrors directionality_override/3 one
% level finer). Dynamic; nothing in the corpus asserts it.
:- dynamic stakeholder_d_override/3.

%% stakeholder_context(+C, +Name, -Context)
%  Deterministic projection: a stakeholder's authored (P,T,E,S) -> context/4.
%  The context is the engine's measurement coordinate; the Name is the seat.
stakeholder_context(C, Name, context(agent_power(P), time_horizon(T),
                                     exit_options(E), spatial_scope(S))) :-
    narrative_ontology:constraint_stakeholder(C, Name, _Role, P, T, E, S).

%% derive_directionality_for_stakeholder(+C, +Name, -D)
%  Precedence: per-(C,Name) override -> role-d (config) + exit modulation,
%  clamped -> canonical power fallback (malformed role only).
derive_directionality_for_stakeholder(C, Name, D) :-
    narrative_ontology:constraint_stakeholder(C, Name, Role, Power, _, Exit, _),
    (   stakeholder_d_override(C, Name, D)
    ->  true
    ;   role_base_d(Role, BaseD)
    ->  constraint_indexing:exit_modulation(Exit, Mod),
        D0 is BaseD + Mod,
        D is max(0.0, min(1.0, D0))
    ;   constraint_indexing:canonical_d_for_power(Power, D)
    ).

% Role->d dispatch (the declared-seat params; config.pl is source of truth).
role_base_d(agenda_setter, D) :- config:param(stakeholder_role_d_agenda_setter, D).
role_base_d(beneficiary,   D) :- config:param(stakeholder_role_d_beneficiary, D).
role_base_d(payer,         D) :- config:param(stakeholder_role_d_payer, D).
role_base_d(excluded,      D) :- config:param(stakeholder_role_d_excluded, D).
role_base_d(observer,      D) :- config:param(stakeholder_role_d_observer, D).

%% chi_for_stakeholder(+C, +Name, -Chi)
chi_for_stakeholder(C, Name, Chi) :-
    stakeholder_context(C, Name, Ctx),
    derive_directionality_for_stakeholder(C, Name, D),
    constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, Chi).

%% dr_type_for_stakeholder(+C, +Name, -Type)
%  The seat's computed type. Excluded seats get a type like any seat (it is a
%  reading); nothing consumes it as an override (R3).
dr_type_for_stakeholder(C, Name, Type) :-
    stakeholder_context(C, Name, Ctx),
    derive_directionality_for_stakeholder(C, Name, D),
    drl_core:dr_type_with_d(C, Ctx, D, Type).

%% in_contention(+C, ?N1, ?N2)
%  Contention is a RELATION between seats, derived — never authored (operator
%  ruling 2026-06-07: no contender role; this predicate is its computed
%  counterpart). Two distinct agent stakeholders at the SAME power atom, one
%  on the beneficiary side (agenda_setter or beneficiary, primary or
%  secondary), the other a payer. Excluded/observer are not contention parties.
in_contention(C, N1, N2) :-
    narrative_ontology:constraint_stakeholder(C, N1, _, Power, _, _, _),
    narrative_ontology:constraint_stakeholder(C, N2, _, Power, _, _, _),
    N1 \= N2,
    \+ narrative_ontology:stakeholder_non_agent(C, N1),
    \+ narrative_ontology:stakeholder_non_agent(C, N2),
    \+ \+ ( role_of(C, N1, R1), beneficiary_side(R1) ),
    \+ \+ role_of(C, N2, payer).

role_of(C, N, R) :- narrative_ontology:constraint_stakeholder(C, N, R, _, _, _, _).
role_of(C, N, R) :- narrative_ontology:stakeholder_secondary_role(C, N, R).

beneficiary_side(agenda_setter).
beneficiary_side(beneficiary).

%% seat_perceived_vs_real(+C, +Name, -Perceived, -Computed)
%  R1 keep: the seat-level perceived/structural diff is COMPUTED, never
%  authored. Perceived immutability from the Axiom-2 (TIME x EXIT) table;
%  Computed from the per-stakeholder path. Perceived=immutable with an
%  extractive Computed is the seat-level false mountain.
seat_perceived_vs_real(C, Name, Perceived, Computed) :-
    narrative_ontology:constraint_stakeholder(C, Name, _, _, T, E, _),
    (   constraint_indexing:effective_immutability(T, E, mountain)
    ->  Perceived = immutable
    ;   Perceived = changeable
    ),
    dr_type_for_stakeholder(C, Name, Computed).

%% consensus_provenance(+C, -Verdict)
%  R3 consumer (commentary-grade ONLY): did unanimity arise because the
%  reading is situation-fixed, or because the dissenting seats were never in
%  the room? Unanimous computed types across non-excluded agent seats + a
%  non-empty excluded set => manufactured-consensus candidate, naming the
%  absent seats. Never feeds classification.
consensus_provenance(C, Verdict) :-
    findall(N, ( narrative_ontology:constraint_stakeholder(C, N, R, _, _, _, _),
                 R \= excluded,
                 \+ narrative_ontology:stakeholder_non_agent(C, N) ), Ns),
    Ns \= [],
    findall(T, ( member(N, Ns), dr_type_for_stakeholder(C, N, T) ), Ts),
    sort(Ts, UniqueTypes),
    findall(X, narrative_ontology:constraint_stakeholder(C, X, excluded, _, _, _, _), Excl),
    (   UniqueTypes = [_], Excl \= []
    ->  Verdict = manufactured_consensus_candidate(Excl)
    ;   UniqueTypes = [_]
    ->  Verdict = unanimous_no_excluded_seats
    ;   Verdict = plural(UniqueTypes)
    ).

%% zombie_piton_crosscheck(+C, -Verdict)
%  R5 mismatch x computed piton, four-cell verdict (commentary-grade).
%  The authored half reads ONLY the two atoms (mismatch-only consumption —
%  the founding-problem narrative is never read); the computed half is the
%  engine's dead-coordination detector at the default analytical context.
zombie_piton_crosscheck(C, Verdict) :-
    (   narrative_ontology:founding_problem_status(C, dead),
        narrative_ontology:disappearance_verdict(C, world_rearranges)
    ->  Authored = zombie
    ;   Authored = no_zombie_flag
    ),
    (   drl_core:dr_type(C, piton)
    ->  Computed = piton
    ;   Computed = not_piton
    ),
    crosscheck_verdict(Authored, Computed, Verdict).

crosscheck_verdict(zombie,         piton,     corroborated_zombie).
crosscheck_verdict(zombie,         not_piton, authored_zombie_uncorroborated).
crosscheck_verdict(no_zombie_flag, piton,     computed_piton_unflagged).
crosscheck_verdict(no_zombie_flag, not_piton, neither).
