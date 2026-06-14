% OQ-122 fix-witness: enumerate the PROPOSED gate's cell, the overdetermination
% residue (type_1 surviving with FSM disabled), the overlap, and whether anything
% already flags the social cases. One run.
:- initialization(main).
:- [stack].
:- use_module(probe_harness).

mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
eps(C, E)   :- ( domain_priors:base_extractiveness(C, E) -> true ; E = na ).
supp(C, S)  :- ( drl_core:get_raw_suppression(C, S0), number(S0) -> S = S0 ; S = na ).
nben(C, N)  :- findall(B, narrative_ontology:agent_beneficiary(C, B), L), sort(L, Ls), length(Ls, N).
nvic(C, N)  :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L, Ls), length(Ls, N).
fsm(C, F)   :- ( signature_detection:false_summit_mountain(C, _) -> F = 'FSM' ; F = '.' ).
sig(C, S)   :- ( signature_detection:constraint_signature(C, S) -> true ; S = none ).

t1(C, Ctx)  :- drl_core:dr_claim_mismatch(C, Ctx, type_1_false_summit, _).
t1count(C, N) :- ( setof(Ctx, t1(C, Ctx), L) -> length(L, N) ; N = 0 ).

% per-seat dr_type, abbreviated power-class -> type
seatseq(C, Seq) :-
    findall(P-T,
      ( drl_core:standard_context(Ctx), Ctx = context(agent_power(P), _, _, _),
        ( catch(drl_core:dr_type(C, Ctx, T0), _, fail) -> T = T0 ; T = '<f>' ) ),
      Seq).

eps_le(C) :- eps(C, E), number(E), E =< 0.25.
supp_le(C) :- supp(C, S), number(S), S =< 0.05.

main :-
    corpus_loader:ensure_corpus_loaded,

    format("~n==== (1) PROPOSED gate cell  {mountain, eps<=0.25, supp<=0.05, victim!=[]} ====~n"),
    findall(C, (mountain(C), eps_le(C), supp_le(C), nvic(C, V), V > 0), P1), sort(P1, C1),
    ( C1 == [] -> format("  EMPTY — victim-gated FSM fires on NOTHING in this corpus~n") ; true ),
    forall(member(C, C1), ( eps(C,E),supp(C,S),nvic(C,NV),nben(C,NB),fsm(C,F),
        format("  ~w~t~46|eps=~w supp=~w vic=~w ben=~w ~w~n",[C,E,S,NV,NB,F]) )),

    format("~n==== (2) CURRENT FSM-firing set (what RED-caps today) + victim count ====~n"),
    findall(C, (mountain(C), fsm(C,'FSM')), P2), sort(P2, C2),
    forall(member(C, C2), ( eps(C,E),supp(C,S),nvic(C,NV),nben(C,NB),t1count(C,T),
        format("  ~w~t~46|eps=~w supp=~w vic=~w ben=~w type1=~w~n",[C,E,S,NV,NB,T]) )),
    format("  -> victim-gate would EXEMPT every member with vic=0 (FSM stops firing there)~n"),

    format("~n==== (3) OVERDETERMINATION: type_1 with beneficiaries retracted corpus-wide (FSM off) ====~n"),
    format("  (only low-eps mountain claims shown: eps<=0.25 — the clean-control region)~n"),
    findall(C, (mountain(C), eps_le(C)), P3), sort(P3, C3),
    format("  BASELINE:~n"),
    forall(member(C, C3), ( t1count(C,T), seatseq(C,Sq), fsm(C,F), nvic(C,NV),
        format("    ~w~t~48|type1=~w vic=~w ~w  ~w~n",[C,T,NV,F,Sq]) )),
    probe_harness:with_retracted([ narrative_ontology:constraint_beneficiary(_,_) ],
      ( format("  WITH ALL BENEFICIARIES RETRACTED (no FSM, no benef-signature can fire):~n"),
        forall(member(C, C3), ( t1count(C,T), seatseq(C,Sq),
            format("    ~w~t~48|type1=~w  ~w~n",[C,T,Sq]) )) )),

    format("~n==== (4) Do the social cell-cases already get flagged? (signature + type_1 + seats) ====~n"),
    forall(member(C, [scale_ceiling_c0, demographic_resource_allocation, demographic_skill_mismatch_c0]),
      ( sig(C,Sg), t1count(C,T), seatseq(C,Sq), nvic(C,NV),
        format("  ~w~t~46|sig=~w type1=~w vic=~w~n      ~w~n",[C,Sg,T,NV,Sq]) )),
    halt.
main :- format("FIX-WITNESS FAILED~n"), halt(1).
