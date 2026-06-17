% OQ-50 Phase 3 — JOINT WITNESS over the two physics controls (radiative/actinide).
% Two legs, both keyed on the AUTHORED no_viable_alternatives flag:
%   HA leg   = config:param(oq50_alt_authoring,1)  + authored no_viable_alternatives(C)
%              -> has_viable_alternatives(C,false)  -> clears the `ha` natural_law condition
%   BENEF leg= constraint_beneficiary(C,_) retracted (the funded-science / OQ-122 beneficiary
%              ruling, witnessed by overlay) -> agent_beneficiary empty -> clears `bc`==0
% natural_law_signature needs BOTH cleared (radiative also passes ac/su/re/ts already).
% Restoration fires natural_law -> mountain at all 4 seats -> type_1 stops -> GREEN.
%
% Cells (over each of radiative/actinide):
%   {HA off, BENEF off} = baseline (witnessed RED elsewhere; re-shown here)
%   {HA on,  BENEF off} = HA-only leg
%   {HA off, BENEF on}  = BENEF-only leg
%   {HA on,  BENEF on}  = both
% Read-off: GREEN only in the both cell ⇒ both-required.
%
% Then the delta-exactly-two confirmation: with BOTH legs on but no_viable_alternatives
% authored ONLY for the two physics cases, enumerate the corpus-wide natural_law-pass set —
% expect EXACTLY {radiative, actinide} (authored discrimination, not metric sweep).
:- initialization(main).
:- [stack].
:- use_module(probe_harness).
:- use_module(cache_registry).

physics2([radiative_levitation_stratification,
          actinide_replenishment_mechanism_flat_control]).

set_ha(V) :- retractall(config:param(oq50_alt_authoring,_)),
             asserta(config:param(oq50_alt_authoring, V)).

seat_power(context(agent_power(P),_,_,_), P).
drseq(C, Seq) :- findall(P-T,
    ( drl_core:standard_context(Ctx), seat_power(Ctx,P),
      ( catch(drl_core:dr_type(C,Ctx,T0),_,fail) -> T=T0 ; T='<f>' ) ), Seq).
t1(C,Ctx) :- drl_core:dr_claim_mismatch(C,Ctx,type_1_false_summit,_).
t1n(C,N) :- ( setof(Ctx,t1(C,Ctx),L) -> length(L,N) ; N=0 ).
sig(C,S) :- ( signature_detection:constraint_signature(C,S0) -> S=S0 ; S=none ).
ha(C,V) :- ( signature_detection:has_viable_alternatives(C,V0) -> V=V0 ; V='<f>' ).
nlpass(C) :- signature_detection:get_constraint_profile(C,P),
             signature_detection:natural_law_signature(P).

verdict(C,Tag) :- t1n(C,N), ( N==0 -> V='GREEN' ; V='RED' ),
    drseq(C,Seq), sig(C,S), ha(C,HA),
    format("    [~w] ~w  type1=~w ~w  ha=~w sig=~w~n      ~w~n",[V,C,N,Tag,HA,S,Seq]).

cell(HAv, Benef, Tag) :-
    cache_registry:clear_all_caches,
    set_ha(HAv),
    physics2(P2),
    ( Benef == retracted
    ->  probe_harness:with_retracted(
          [ narrative_ontology:constraint_beneficiary(radiative_levitation_stratification,_),
            narrative_ontology:constraint_beneficiary(actinide_replenishment_mechanism_flat_control,_) ],
          ( cache_registry:clear_all_caches,
            forall(member(C,P2), verdict(C,Tag)) ))
    ;   forall(member(C,P2), verdict(C,Tag)) ),
    cache_registry:clear_all_caches.

main :-
    % small corpus is enough — the two controls live there
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets)),
    corpus_loader:load_all_testsets,
    physics2(P2),

    format("~n================ OQ-50 JOINT WITNESS (corpus=testsets) ================~n"),
    format("Authoring no_viable_alternatives for: ~w~n", [P2]),
    % author the discriminator (overlay) for the whole run
    forall(member(C,P2), assertz(narrative_ontology:no_viable_alternatives(C))),

    format("~n-- CELL {HA off, BENEF off}  (baseline) --~n"),
    cell(0, present, '{HA off,BENEF off}'),
    format("~n-- CELL {HA on,  BENEF off}  (HA leg only) --~n"),
    cell(1, present, '{HA on, BENEF off}'),
    format("~n-- CELL {HA off, BENEF on}   (BENEF leg only) --~n"),
    cell(0, retracted, '{HA off,BENEF on}'),
    format("~n-- CELL {HA on,  BENEF on}   (both) --~n"),
    cell(1, retracted, '{HA on, BENEF on}'),

    % delta-exactly-two under authored discrimination + both legs
    format("~n-- DELTA-EXACTLY-TWO (both legs on; no_viable_alternatives authored only for the 2) --~n"),
    cache_registry:clear_all_caches, set_ha(1),
    probe_harness:with_retracted(
      [ narrative_ontology:constraint_beneficiary(radiative_levitation_stratification,_),
        narrative_ontology:constraint_beneficiary(actinide_replenishment_mechanism_flat_control,_) ],
      ( cache_registry:clear_all_caches,
        findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0,Cs),
        findall(C, (member(C,Cs), nlpass(C)), NL), sort(NL,NLs),
        format("    natural_law-pass set = ~w~n", [NLs]),
        sort(P2, P2s),
        ( NLs == P2s -> format("    => EXACTLY {radiative,actinide} — delta-exactly-two HOLDS~n")
        ; format("    => set differs from the two physics cases — INSPECT~n") ) )),
    halt.
main :- format("JOINT WITNESS FAILED~n"), halt(1).
