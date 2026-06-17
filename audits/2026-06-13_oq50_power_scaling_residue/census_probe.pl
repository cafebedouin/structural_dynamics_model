% OQ-50 power-scaling residue — Phase 1 census probe (read-only, no engine change).
% Enumerates every mountain-claimer on a corpus selected by CORPUS_DIR.
% Per constraint emits: eps, victim/beneficiary/agent_beneficiary counts, the six
% natural_law_signature profile fields with per-field pass/fail, constraint_signature,
% is_mountain (metric, pre-signature) at the 4 standard seats, dr_type (final) at the 4
% seats, whether type_1_false_summit fires (and seat count), and a partition label.
%
% Run per corpus:
%   CORPUS_DIR=testsets       swipl -q -g true -t halt census_probe.pl
%   CORPUS_DIR=testsets_flash swipl -q -g true -t halt census_probe.pl
%   CORPUS_DIR=testsets_haiku swipl -q -g true -t halt census_probe.pl
:- initialization(main).
:- [stack].
:- use_module(cache_registry).

% ---- field accessors -------------------------------------------------------
mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
eps(C, E)   :- ( drl_core:base_extractiveness(C, E0), number(E0) -> E = E0 ; E = na ).
nvic(C, N)  :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L, Ls), length(Ls, N).
nben(C, N)  :- findall(B, narrative_ontology:constraint_beneficiary(C, B), L), sort(L, Ls), length(Ls, N).
nagb(C, N)  :- findall(B, narrative_ontology:agent_beneficiary(C, B), L), sort(L, Ls), length(Ls, N).
sig(C, S)   :- ( signature_detection:constraint_signature(C, S0) -> S = S0 ; S = none ).

% per-seat is_mountain (metric, pre-signature) and dr_type (final), powerless..analytical
seat_power(context(agent_power(P),_,_,_), P).
ismtn_seq(C, Seq) :-
    findall(P-M,
      ( drl_core:standard_context(Ctx), seat_power(Ctx, P),
        ( drl_core:is_mountain(C, Ctx, M0) -> M = M0 ; M = '<f>' ) ),
      Seq).
drtype_seq(C, Seq) :-
    findall(P-T,
      ( drl_core:standard_context(Ctx), seat_power(Ctx, P),
        ( catch(drl_core:dr_type(C, Ctx, T0), _, fail) -> T = T0 ; T = '<f>' ) ),
      Seq).

t1(C, Ctx) :- drl_core:dr_claim_mismatch(C, Ctx, type_1_false_summit, _).
t1count(C, N) :- ( setof(Ctx, t1(C, Ctx), L) -> length(L, N) ; N = 0 ).

% ---- natural_law profile with per-field pass/fail --------------------------
% Built via the same get_constraint_profile/2 path constraint_signature uses.
nl_profile(C, profile(AC,Su,Re,BC,HA,TS,_),
           [ac(AC,ACp), su(Su,Sup), re(Re,Rep), bc(BC,BCp), ha(HA,HAp), ts(TS,TSp)],
           AllPass) :-
    signature_detection:get_constraint_profile(C,
        profile(AC,Su,Re,BC,HA,TS,CS)),
    config:param(natural_law_collapse_min, CMin),
    config:param(natural_law_suppression_max, SMax),
    config:param(natural_law_resistance_max, RMax),
    ( (number(AC), AC >= CMin)       -> ACp = pass ; ACp = fail ),
    ( (number(Su), Su =< SMax)       -> Sup = pass ; Sup = fail ),
    ( (number(Re), Re =< RMax)       -> Rep = pass ; Rep = fail ),
    ( BC == 0                        -> BCp = pass ; BCp = fail ),
    ( HA == false                    -> HAp = pass ; HAp = fail ),
    ( TS == stable                   -> TSp = pass ; TSp = fail ),
    ( foldl([X,A0,A1]>>(arg(2,X,pass)->A1=A0;A1=fail), [ac(AC,ACp),su(Su,Sup),re(Re,Rep),bc(BC,BCp),ha(HA,HAp),ts(TS,TSp)], pass, AllPass) -> true ; AllPass = fail ),
    ignore(CS = CS).

% ---- partition label -------------------------------------------------------
% restored : dr_type=mountain at ALL 4 seats
% residue  : not restored, vic=0, dr_type departs mountain at a mid seat (moderate/institutional)
% victim   : nvic > 0
% other    : anything else (reported explicitly, not silently bucketed)
mid_departs(Seq) :-
    ( member(moderate-T, Seq),     T \= mountain
    ; member(institutional-T, Seq), T \= mountain ).
all_mtn(Seq) :- forall(member(_-T, Seq), T == mountain).

partition(C, DrSeq, Label) :-
    nvic(C, NV),
    ( NV > 0          -> Label = victim
    ; all_mtn(DrSeq)  -> Label = restored
    ; mid_departs(DrSeq) -> Label = residue
    ; Label = other ).

% ---- main ------------------------------------------------------------------
emit(C) :-
    eps(C,E), nvic(C,NV), nben(C,NB), nagb(C,NAB),
    sig(C,Sg), ismtn_seq(C,IM), drtype_seq(C,DR), t1count(C,T1),
    ( nl_profile(C, _, Fields, AllPass) -> true ; Fields = noprofile, AllPass = na ),
    partition(C, DR, Label),
    format("CONS ~w~n", [C]),
    format("  eps=~w vic=~w ben=~w agt_ben=~w sig=~w~n", [E,NV,NB,NAB,Sg]),
    format("  nl_profile=~w  nl_all_pass=~w~n", [Fields, AllPass]),
    format("  is_mountain=~w~n", [IM]),
    format("  dr_type    =~w~n", [DR]),
    format("  type1_fires=~w  partition=~w~n", [T1, Label]).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    cache_registry:clear_all_caches,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, NCorpus),
    findall(C, (member(C,Cs), mountain(C)), Ms0), sort(Ms0, Ms),
    length(Ms, NMtn),
    format("~n======== OQ-50 CENSUS  corpus=~w  loaded=~w  mountain_claimers=~w ========~n",
           [Dir, NCorpus, NMtn]),
    forall(member(C, Ms), emit(C)),

    % ---- positive controls --------------------------------------------------
    format("~n======== POSITIVE CONTROLS ========~n"),
    % Control 1: at least one restored-at-all-seats mountain whose metric is_mountain
    % degrades at a mid seat (proves restoration is detectable by the probe).
    findall(C,
      ( member(C, Ms), drtype_seq(C, DR), all_mtn(DR),
        ismtn_seq(C, IM), mid_departs(IM) ), Ctrl1),
    ( Ctrl1 \= []
    ->  format("CONTROL 1 PASS — restoration detected (is_mountain degrades mid-seat, dr_type=mountain all seats): ~w~n", [Ctrl1])
    ;   format("CONTROL 1 EMPTY — NO restored-with-mid-degradation case in ~w (report absence against this, do not assert restoration elsewhere)~n", [Dir]) ),

    % Control 2: a victim-bearing claimer whose dr_type departs mountain (firing path).
    findall(C,
      ( member(C, Ms), nvic(C,NV), NV>0, drtype_seq(C,DR), \+ all_mtn(DR) ), Ctrl2),
    ( Ctrl2 \= []
    ->  format("CONTROL 2 PASS — victim-bearing claimer departs mountain (firing path live): ~w~n", [Ctrl2])
    ;   format("CONTROL 2 EMPTY — no victim-bearing departing claimer in ~w~n", [Dir]) ),

    % Control 3 (STOP-AND-REPORT): any mountain-claimer with is_mountain=mountain at
    % a MID seat falsifies the drl_core.pl:605-612 universal-degradation premise.
    findall(C-IM,
      ( member(C, Ms), ismtn_seq(C, IM),
        ( member(moderate-mountain, IM) ; member(institutional-mountain, IM) ) ), Ctrl3),
    ( Ctrl3 == []
    ->  format("CONTROL 3 PASS — NO mountain-claimer holds is_mountain=mountain at moderate/institutional (premise intact)~n")
    ;   format("CONTROL 3 *** PREMISE FALSIFIED *** mid-seat is_mountain=mountain found — HALT, do not proceed to Phase 2:~n~w~n", [Ctrl3]) ),
    halt.
main :- format("CENSUS PROBE FAILED~n"), halt(1).
