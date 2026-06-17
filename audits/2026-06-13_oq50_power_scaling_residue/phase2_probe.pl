% OQ-50 Phase 2 — evidence-only analysis probe (read-only, no engine change).
%   (a) Does ANY constraint pass natural_law_signature on this corpus? (witness the
%       dead restoration path) + per-field failure census over mountain-claimers.
%   (b) radiative/actinide: failing natural_law condition set when BENEFICIARY-FREE.
%   (c) residue victim-independence: mid-seat dr_type by victim presence.
%   (d) UNIT POSITIVE CONTROL: a synthetic profile with HasAlternatives=false PASSES
%       natural_law_signature and the override yields mountain — proving the
%       restoration LOGIC is live, so the corpus-wide absence is real (not a dead probe).
:- initialization(main).
:- [stack].
:- use_module(probe_harness).
:- use_module(cache_registry).

mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
nvic(C, N)  :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L, Ls), length(Ls, N).
seat_power(context(agent_power(P),_,_,_), P).
drtype_seq(C, Seq) :-
    findall(P-T,
      ( drl_core:standard_context(Ctx), seat_power(Ctx, P),
        ( catch(drl_core:dr_type(C, Ctx, T0), _, fail) -> T = T0 ; T = '<f>' ) ), Seq).

% per-field pass against config thresholds (number-guarded)
fld_pass(ac, AC) :- config:param(natural_law_collapse_min, M), number(AC), AC >= M.
fld_pass(su, Su) :- config:param(natural_law_suppression_max, M), number(Su), Su =< M.
fld_pass(re, Re) :- config:param(natural_law_resistance_max, M), number(Re), Re =< M.
fld_pass(bc, BC) :- BC == 0.
fld_pass(ha, HA) :- HA == false.
fld_pass(ts, TS) :- TS == stable.

profile_fail_set(C, FailSet) :-
    signature_detection:get_constraint_profile(C, profile(AC,Su,Re,BC,HA,TS,_)),
    findall(F, ( member(F-V, [ac-AC, su-Su, re-Re, bc-BC, ha-HA, ts-TS]),
                 \+ fld_pass(F, V) ), FailSet).

nl_passes(C) :-
    signature_detection:get_constraint_profile(C, P),
    signature_detection:natural_law_signature(P).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    cache_registry:clear_all_caches,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C, (member(C,Cs), mountain(C)), Ms0), sort(Ms0, Ms),

    format("~n======== OQ-50 PHASE 2  corpus=~w ========~n", [Dir]),

    % (a) natural_law_signature passes corpus-wide?
    findall(C, (member(C,Cs), nl_passes(C)), NLpass),
    length(NLpass, NNL),
    format("~n(a) natural_law_signature PASSES corpus-wide: ~w  -> ~w~n", [NNL, NLpass]),

    % per-field failure tally over mountain-claimers
    format("    per-field FAIL counts over ~w mountain-claimers:~n", [Ms]),
    forall(member(F, [ac,su,re,bc,ha,ts]),
      ( findall(C, (member(C,Ms), profile_fail_set(C,FS), memberchk(F,FS)), L),
        length(L,N), format("      ~w fails: ~w~n", [F, N]) )),

    % mountain-claimers that fail ONLY on {bc, ha} (the restoration-blockers if
    % power-scaling were the only other issue) and that pass ac/su/re/ts:
    findall(C, (member(C,Ms), profile_fail_set(C,FS),
                subtract(FS,[bc,ha],[])), OnlyBcHa),
    format("    fail ONLY on subset of {bc,ha} (pristine on ac/su/re/ts): ~w~n", [OnlyBcHa]),

    % (b) radiative/actinide failing set, baseline and beneficiary-free
    format("~n(b) radiative/actinide failing natural_law conditions:~n"),
    forall(member(C, [radiative_levitation_stratification,
                      actinide_replenishment_mechanism_flat_control]),
      ( ( member(C, Cs)
        ->  profile_fail_set(C, FSb),
            format("    ~w  baseline fail-set = ~w~n", [C, FSb])
        ;   format("    ~w  NOT IN THIS CORPUS~n", [C]) ))),
    ( ( memberchk(radiative_levitation_stratification, Cs)
      ; memberchk(actinide_replenishment_mechanism_flat_control, Cs) )
    -> cache_registry:clear_all_caches,
       probe_harness:with_retracted([ narrative_ontology:constraint_beneficiary(_,_) ],
         ( format("    -- BENEFICIARY-FREE (all constraint_beneficiary retracted) --~n"),
           forall(member(C, [radiative_levitation_stratification,
                             actinide_replenishment_mechanism_flat_control]),
             ( ( member(C, Cs)
               -> profile_fail_set(C, FSf),
                  format("    ~w  beneficiary-free fail-set = ~w~n", [C, FSf])
               ; true ))) )),
       cache_registry:clear_all_caches
    ; true ),

    % (c) residue victim-independence: mid-seat dr_type grouped by victim presence
    format("~n(c) mid-seat (moderate/institutional) dr_type by victim presence:~n"),
    forall(member(C, Ms),
      ( drtype_seq(C, Seq), nvic(C,NV),
        ( member(moderate-Mt, Seq) -> true ; Mt='?' ),
        ( member(institutional-It, Seq) -> true ; It='?' ),
        ( NV>0 -> Tag=victim ; Tag=novictim ),
        format("    ~w~t~58|vic=~w(~w) mod=~w inst=~w~n", [C,NV,Tag,Mt,It]) )),

    % (d) UNIT POSITIVE CONTROL: restoration logic is live
    format("~n(d) UNIT POSITIVE CONTROL — restoration logic on a synthetic passing profile:~n"),
    SynthPass = profile(0.92, 0.02, 0.04, 0, false, stable, _),
    ( signature_detection:natural_law_signature(SynthPass)
    -> format("    natural_law_signature(HA=false pristine) = PASS~n")
    ;  format("    natural_law_signature(HA=false pristine) = *** FAIL (probe broken) ***~n") ),
    ( signature_detection:resolve_modal_signature_conflict(rope, natural_law, R)
    -> format("    override resolve(rope, natural_law) -> ~w (expect mountain)~n", [R])
    ;  format("    override resolve FAILED~n") ),
    SynthUnknown = profile(0.92, 0.02, 0.04, 0, unknown, stable, _),
    ( signature_detection:natural_law_signature(SynthUnknown)
    -> format("    natural_law_signature(HA=unknown pristine) = PASS (unexpected)~n")
    ;  format("    natural_law_signature(HA=unknown pristine) = FAIL <- this is the corpus-wide blocker~n") ),
    halt.
main :- format("PHASE2 PROBE FAILED~n"), halt(1).
