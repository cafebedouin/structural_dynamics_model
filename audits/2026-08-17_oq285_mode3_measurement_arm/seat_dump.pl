% seat_dump.pl — OQ-285 Phase 1B probe. READ-ONLY: emits one TSV row per
% authored constraint_stakeholder/7 fact on the loaded corpus. Writes nothing
% into outputs/; the only write is the TSV named on the command line.
%
% Run (from prolog/):
%   swipl -g "[stack], corpus_loader:load_all_testsets, \
%     ['../audits/2026-08-17_oq285_mode3_measurement_arm/seat_dump'], \
%     dump_seats('<abs path>.tsv'), halt" -t "halt(1)"
%
% Columns:
%   cid  seat  role  power  time  exit  scope  agent_seat  d  chi
%   type_token  raw_derivation  perceived  signature  imm_hole
%
%   agent_seat     : true iff the seat is in stakeholder_agent_seats/2 (the H¹ domain)
%   type_token     : seat_type_token/3 — the kernel-facing token (failure -> unknown)
%   raw_derivation : ok | FAILS — whether dr_type_for_stakeholder/3 succeeded.
%                    This is the group (i) vs (ii)/(iii)/(iv) discriminator.
%   perceived      : seat_perceived_vs_real/4's Perceived (immutable|changeable)
%   imm_hole       : true iff (time,exit) has NO effective_immutability/3 row
%                    (the Phase-4 item-2 table hole)

:- use_module(library(lists)).

seat_row(C, Row) :-
    narrative_ontology:constraint_stakeholder(C, N, Role, P, T, E, S),
    ( stakeholder_seats:stakeholder_agent_seats(C, Ns), memberchk(N, Ns)
    -> Agent = true ; Agent = false ),
    ( stakeholder_seats:derive_directionality_for_stakeholder(C, N, D0)
    -> D = D0 ; D = na ),
    ( stakeholder_seats:chi_for_stakeholder(C, N, Chi0)
    -> Chi = Chi0 ; Chi = na ),
    ( stakeholder_seats:dr_type_for_stakeholder(C, N, RawT)
    -> Raw = ok, RawType = RawT ; Raw = fails, RawType = none ),
    % PRE-signature modal type at this seat: the same classify_from_metrics/6 call
    % dr_type_with_d/4 makes, stopping before integrate_signature_with_modal/3.
    ( seat_metric_type(C, N, MT0) -> MT = MT0 ; MT = fails ),
    stakeholder_seats:seat_type_token(C, N, Tok),
    ( stakeholder_seats:seat_perceived_vs_real(C, N, Perc, _)
    -> true ; Perc = na ),
    ( signature_detection:constraint_signature(C, Sig) -> true ; Sig = none ),
    ( constraint_indexing:effective_immutability(T, E, _) -> Hole = false ; Hole = true ),
    Row = row(C, N, Role, P, T, E, S, Agent, D, Chi, Tok, Raw, RawType, MT, Perc, Sig, Hole).

%% seat_metric_type(+C, +N, -MetricType)
%  The PRE-signature half of dr_type_with_d/4 (drl_core.pl:500-508), reproduced
%  here up to but NOT including integrate_signature_with_modal/3. Same
%  predicates, same argument order — the point of the probe is to measure what
%  the signature layer changes, so the two halves must be separable.
seat_metric_type(C, N, MetricType) :-
    stakeholder_seats:stakeholder_context(C, N, Ctx),
    stakeholder_seats:derive_directionality_for_stakeholder(C, N, D),
    constraint_indexing:valid_context(Ctx),
    drl_core:base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, Chi),
    drl_core:get_raw_suppression(C, Supp),
    drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, MetricType).

dump_seats(File) :-
    setup_call_cleanup(
        open(File, write, Out),
        (   format(Out, "cid\tseat\trole\tpower\ttime\texit\tscope\tagent_seat\td\tchi\ttype_token\traw_derivation\traw_type\tmetric_type\tperceived\tsignature\timm_hole~n", []),
            forall(( corpus_loader:corpus_constraint(C), seat_row(C, R) ),
                   write_row(Out, R))
        ),
        close(Out)),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C2), seat_row(C2, _) ), NRows),
    format(user_error, "[seat_dump] ~w rows written to ~w~n", [NRows, File]).

write_row(Out, row(C,N,Role,P,T,E,S,Agent,D,Chi,Tok,Raw,RawType,MT,Perc,Sig,Hole)) :-
    fmt_num(D, DS), fmt_num(Chi, ChiS),
    format(Out, "~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n",
           [C,N,Role,P,T,E,S,Agent,DS,ChiS,Tok,Raw,RawType,MT,Perc,Sig,Hole]).

fmt_num(na, na) :- !.
fmt_num(V, S) :- format(atom(S), "~6f", [V]).
