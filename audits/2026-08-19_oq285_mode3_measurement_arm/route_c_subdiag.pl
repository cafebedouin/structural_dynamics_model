% OQ-285 — sub-diagnosis of route c: did the CASCADE fall through unmatched,
% or did the signature layer REWRITE a real metric type to `unknown`?
subdiag :-
    findall(Tag,
      ( corpus_loader:corpus_constraint(C),
        stakeholder_seats:stakeholder_agent_seats(C, Ns), member(N, Ns),
        stakeholder_seats:stakeholder_context(C, N, Ctx),
        once(stakeholder_seats:derive_directionality_for_stakeholder(C, N, D)),
        once(( constraint_indexing:valid_context(Ctx),
               drl_core:base_extractiveness(C, BE),
               constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, Chi),
               drl_core:get_raw_suppression(C, Supp),
               drl_core:classify_from_metrics(C, BE, Chi, Supp, Ctx, MT),
               signature_detection:integrate_signature_with_modal(C, MT, FT) )),
        FT == unknown,
        ( MT == unknown -> Tag = cascade_fell_through
        ; Tag = signature_rewrote(MT) )
      ), Tags),
    msort(Tags, S), tally(S, T),
    format("~n=== route c sub-diagnosis ===~n", []),
    forall(member(K-V,T), format("  ~w~t~40| ~d~n",[K,V])),
    % zero-seat stories: are they the *_contradictions axiom meta-files?
    findall(C2, ( corpus_loader:corpus_constraint(C2),
                  stakeholder_seats:stakeholder_agent_seats(C2, []) ), Zs),
    length(Zs, NZ),
    aggregate_all(count, ( member(Z, Zs), atom_concat(_, '_contradictions', Z) ), NContra),
    format("~n=== zero-agent-seat stories ===~n  total~t~40| ~d~n  of which *_contradictions~t~40| ~d~n",
           [NZ, NContra]).
