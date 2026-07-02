% OQ-137 Phase-5 empirical totality probe: per candidate reading predicate,
% solution-count distribution over its natural domain.
% Output: SWEEP <label> <domain> n=<N> zero=<Z> one=<O> multi=<M>
%         SWEEP_ZERO/SWEEP_MULTI <label> <key> (first 5 each)

sweep :-
    corpus_loader:load_all_testsets,
    forall(spec(Label, Domain, Template), probe_one(Label, Domain, Template)).

domain_keys(constraint, Keys) :-
    findall([C], corpus_loader:corpus_constraint(C), Keys).
domain_keys(seat, Keys) :-
    findall([C,N], ( corpus_loader:corpus_constraint(C),
                     narrative_ontology:constraint_stakeholder(C,N,_,_,_,_,_) ), Keys).
domain_keys(uid, Keys) :-
    findall([U], ( corpus_loader:corpus_constraint(C),
                   narrative_ontology:cs_story_uid(C, U) ), Keys0),
    sort(Keys0, Keys).
domain_keys(drift_uid, Keys) :-
    findall([U], ( corpus_loader:corpus_constraint(C),
                   narrative_ontology:cs_story_uid(C, U),
                   narrative_ontology:cs_drift_state(U, _, _) ), Keys0),
    sort(Keys0, Keys).
domain_keys(kernel, Keys) :-
    findall([K], ( corpus_loader:corpus_constraint(C),
                   narrative_ontology:cs_kernel_id(C, K) ), Keys0),
    sort(Keys0, Keys).

probe_one(Label, Domain, Template) :-
    domain_keys(Domain, Keys),
    length(Keys, N),
    findall(Key-Cnt,
            ( member(Key, Keys),
              copy_term(Template, Key-Goal),
              aggregate_all(count, catch(Goal, _, fail), Cnt) ),
            Counts),
    partition_counts(Counts, Zs, Os, Ms),
    length(Zs, NZ), length(Os, NO), length(Ms, NM),
    (   NZ + NO + NM =:= N
    ->  true
    ;   format('SWEEP_BROKEN ~w partitions ~w+~w+~w != n ~w~n', [Label, NZ, NO, NM, N])
    ),
    format('SWEEP ~w ~w n=~w zero=~w one=~w multi=~w~n', [Label, Domain, N, NZ, NO, NM]),
    emit_examples('SWEEP_ZERO', Label, Zs),
    emit_examples('SWEEP_MULTI', Label, Ms).

partition_counts([], [], [], []).
partition_counts([K-0|T], [K|Zs], Os, Ms) :- !, partition_counts(T, Zs, Os, Ms).
partition_counts([K-1|T], Zs, [K|Os], Ms) :- !, partition_counts(T, Zs, Os, Ms).
partition_counts([K-C|T], Zs, Os, [K-C|Ms]) :- partition_counts(T, Zs, Os, Ms).

emit_examples(Tag, Label, L) :-
    length(L, Len),
    ( Len =< 5 -> Show = L ; length(Show, 5), append(Show, _, L) ),
    forall(member(X, Show), format('~w ~w ~w~n', [Tag, Label, X])).

% ---- Positive controls on the sweep itself (the 2026-07-02 vacuous-pass
% lesson: an unparenthesized M:G template made copy_term fail silently and
% every row read 0/0/0). ctl_planted_fail must read zero=n; ctl_planted_multi
% must read multi=n — else the sweep is not measuring.
spec(ctl_planted_fail,  constraint, [C]-( ctl_never(C) )).
spec(ctl_planted_multi, constraint, [C]-( ctl_two(C,_) )).
ctl_never(_) :- fail.
ctl_two(_, a).
ctl_two(_, b).

% ---- Family A: stakeholder_seats -------------------------------------------
spec(stakeholder_context,  seat, [C,N]-( stakeholder_seats:stakeholder_context(C,N,_) )).
spec(derive_d,             seat, [C,N]-( stakeholder_seats:derive_directionality_for_stakeholder(C,N,_) )).
spec(dr_type_seat,         seat, [C,N]-( stakeholder_seats:dr_type_for_stakeholder(C,N,_) )).
spec(chi_seat,             seat, [C,N]-( stakeholder_seats:chi_for_stakeholder(C,N,_) )).
spec(power_witness_map,    constraint, [C]-( stakeholder_seats:power_witness_map(C,_) )).
spec(extraction_reading,   constraint, [C]-( stakeholder_seats:extraction_reading(C,_) )).
% ---- Family B: signature_detection ------------------------------------------
spec(signature_confidence, constraint,
     [C]-( signature_detection:constraint_signature(C,S),
           signature_detection:signature_confidence(C,S,_) )).
spec(explain_signature,    constraint,
     [C]-( signature_detection:constraint_signature(C,S),
           signature_detection:explain_signature(C,S,_) )).
spec(false_natural_law,    constraint, [C]-( signature_detection:false_natural_law(C,_) )).
spec(false_summit_mountain,constraint, [C]-( signature_detection:false_summit_mountain(C,_) )).
spec(coupling_invariant_rope, constraint, [C]-( signature_detection:coupling_invariant_rope(C,_) )).
spec(false_ci_rope,        constraint, [C]-( signature_detection:false_ci_rope(C,_) )).
spec(structural_purity,    constraint, [C]-( signature_detection:structural_purity(C,_) )).
spec(has_viable_alternatives, constraint, [C]-( signature_detection:has_viable_alternatives(C,_) )).
spec(has_metric_persp_variance, constraint, [C]-( signature_detection:has_metric_perspectival_variance(C) )).
spec(level_gradient_divergence, constraint, [C]-( signature_detection:level_gradient_divergence(C,_) )).
spec(get_constraint_profile, constraint, [C]-( signature_detection:get_constraint_profile(C,_) )).
% ---- Family C: cs_* ----------------------------------------------------------
spec(cs_has_fields,        constraint, [C]-( cs_pattern_detection:cs_has_fields(C) )).
spec(cs_pattern,           constraint, [C]-( cs_pattern_detection:cs_pattern(C,_,_) )).
spec(cs_verdict,           constraint, [C]-( cs_pattern_detection:cs_verdict(C,_) )).
spec(cs_naturalized_mountain, constraint, [C]-( cs_pattern_detection:cs_naturalized_mountain(C) )).
spec(cs_authority_masking, constraint, [C]-( cs_pattern_detection:cs_authority_masking(C,_,_) )).
spec(cs_cover_story_active, constraint, [C]-( cs_pattern_detection:cs_cover_story_active(C,_) )).
spec(cs_displaced_beneficiary, constraint, [C]-( cs_pattern_detection:cs_displaced_beneficiary(C) )).
spec(cs_grounding_mismatch, constraint, [C]-( cs_pattern_detection:cs_grounding_mismatch(C,_,_) )).
spec(cs_has_axioms,        constraint, [C]-( cs_axiom_engine:cs_has_axioms(C) )).
spec(cs_axiom_inconsistent, constraint, [C]-( cs_axiom_engine:cs_axiom_inconsistent(C,_) )).
spec(cs_axiom_foreclosed,  uid, [U]-( cs_axiom_engine:cs_axiom_foreclosed(U,_) )).
spec(cs_drift_unacknowledged, uid, [U]-( cs_pattern_detection:cs_drift_unacknowledged(U,_) )).
spec(cs_drift_trajectory_all, uid, [U]-( cs_drift_engine:cs_drift_trajectory(U,_,_) )).
spec(cs_drift_trajectory_ondomain, drift_uid, [U]-( cs_drift_engine:cs_drift_trajectory(U,_,_) )).
spec(cs_kernel_coverage,   kernel, [K]-( cs_kernel_registry:cs_kernel_coverage(K,_) )).
spec(cs_kernel_obstruction_status, kernel, [K]-( cs_kernel_registry:cs_kernel_obstruction_status(K,_) )).
spec(cs_reading_trifurcation, kernel, [K]-( cs_trifurcation:cs_reading_trifurcation(K,_,_) )).
spec(cs_kernel_divergence, kernel, [K]-( cs_kernel_registry:cs_kernel_divergence(K,_,_,_) )).
