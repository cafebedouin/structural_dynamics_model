% OQ-356 V3b follow-on — is the purity_contamination_cap sweep inert at the
% VALUE level, or only at the BAND level, on a degenerate leg?
%
% Two-sided by construction: the probe reports BOTH the number of members whose
% effective purity MOVES between cap=0.10 and cap=1.00 and the max |delta|. A
% zero mover-count is only interpretable because the same pass reports the max
% RawContam actually observed on the edges — if that max is <= 0.10 the cap
% provably cannot bind anywhere in the swept range, which is a MEASUREMENT, not
% a failure to look.
% (run via -g)

set_cap(V) :-
    retractall(config:param(purity_contamination_cap, _)),
    asserta(config:param(purity_contamination_cap, V)).

eps_at(Cap, Cs, Ctx, EPs) :-
    set_cap(Cap),
    findall(C-EP,
            ( member(C, Cs),
              catch(drl_purity_network:effective_purity(C, Ctx, EP, _), _, fail),
              number(EP) ),
            EPs).

main :-
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    length(Cs0, NAll),
    format("corpus members: ~w~n", [NAll]),
    eps_at(0.10, Cs0, Ctx, Lo),
    eps_at(1.00, Cs0, Ctx, Hi),
    length(Lo, NLo), length(Hi, NHi),
    format("scorable at cap=0.10: ~w   at cap=1.00: ~w~n", [NLo, NHi]),
    findall(D,
            ( member(C-A, Lo), memberchk(C-B, Hi), D is abs(A-B), D > 0.0 ),
            Ds),
    length(Ds, NMoved),
    ( Ds == [] -> MaxD = 0.0 ; max_list(Ds, MaxD) ),
    format("members whose EFFECTIVE PURITY moves between cap 0.10 and 1.00: ~w  (max |delta| = ~6f)~n",
           [NMoved, MaxD]),
    % --- the interpretability control: what is the largest RawContam on any edge? ---
    set_cap(1.00),   % uncapped, so RawContam is observable directly
    config:param(purity_attenuation_factor, Att),
    findall(RC,
            ( member(C, Cs0),
              purity_scoring:purity_score(C, MyP), number(MyP), MyP >= 0.0,
              drl_purity_network:constraint_neighbors(C, Ctx, Ns),
              member(neighbor(Other, ES, _), Ns),
              \+ ( narrative_ontology:cs_kernel_id(C, K),
                   narrative_ontology:cs_kernel_id(Other, K) ),
              purity_scoring:purity_score(Other, OP), number(OP), OP >= 0.0,
              Delta is max(0.0, MyP - OP), Delta > 0.0,
              ( drl_core:dr_type(Other, Ctx, OT)
              -> drl_purity_network:type_contamination_strength(OT, TF) ; TF = 0.0 ),
              RC is Delta * (ES * Att) * TF
            ),
            RCs),
    length(RCs, NEdges),
    ( RCs == [] -> MaxRC = 0.0 ; max_list(RCs, MaxRC) ),
    format("contaminating edges examined: ~w   max RawContam observed: ~6f~n", [NEdges, MaxRC]),
    ( MaxRC =< 0.10
    -> format("=> the cap CANNOT BIND anywhere in the swept range 0.10..1.00 (max RawContam <= lowest cap).~n"),
       format("   The ten-row collapse sweep is INERT BY DATA on this leg, not by a stale cache.~n")
    ;  format("=> the cap CAN bind (max RawContam ~6f > 0.10): flatness is NOT explained by non-binding.~n", [MaxRC]) ),
    % --- positive control: the probe CAN see a moved EP when one exists ---
    ( RCs \== [], MaxRC > 0.0
    -> format("control: contamination is non-zero somewhere (max RawContam ~6f > 0), so the~n", [MaxRC]),
       format("         zero-mover reading above is a measurement of the CAP, not of an empty network.~n")
    ;  format("control FAILED: no contaminating edge at all — a zero-mover reading is UNINTERPRETABLE.~n") ).
