% Reachability probe for the OQ-62 guard blast radius at abductive_triggers:525.
% Question: can `unknown` or a negative sentinel actually reach fpn_zone/2 there?
% A bare count of 0 is "didn't look" unless the precompute ran — so fpn_run/3
% executes first and its success count is the positive control.

classify(Vs, N, NonNum, Neg) :-
    length(Vs, N),
    include([X]>>(\+ number(X)), Vs, A), length(A, NonNum),
    include([X]>>(number(X), X < 0.0), Vs, B), length(B, Neg).

main :-
    corpus_loader:ensure_corpus_loaded,
    % Mirror fpn_report:run_fpn_report exactly — the previous run used the atom
    % `default` and discovered via corpus_constraint/1, landing off the authored
    % grid and reading every result as absent (OQ-178 dual).
    constraint_indexing:default_context(Ctx),
    format("context = ~w~n", [Ctx]),
    findall(C, (narrative_ontology:constraint_claim(C, _), \+ is_list(C)), Raw),
    sort(Raw, Cs),
    length(Cs, NC),
    format("corpus N=~w~n", [NC]),

    % ---- precompute the probe previously skipped -------------------------
    (   catch(drl_fpn:fpn_run(Cs, Ctx, R), E1, (format("fpn_run THREW ~w~n", [E1]), fail))
    ->  format("fpn_run: ~w~n", [R])
    ;   format("fpn_run: FAILED~n")
    ),

    % ---- positive control: the precomputed store must be non-empty -------
    findall(V, (member(C, Cs), catch(drl_fpn:fpn_ep(C, Ctx, V), _, fail)), DVs),
    classify(DVs, DN, DNN, DNG),
    format("CONTROL drl_fpn:fpn_ep       succeeded=~w  (must be >0 or probe is blind)~n", [DN]),
    format("RESULT  drl_fpn:fpn_ep       non_number=~w negative=~w~n", [DNN, DNG]),

    % ---- the qualification the call site actually uses --------------------
    (   current_predicate(drl_modal_logic:fpn_ep/3)
    ->  format("drl_modal_logic:fpn_ep/3 DEFINED~n")
    ;   format("drl_modal_logic:fpn_ep/3 UNDEFINED  <-- call site is dead\n")
    ),
    findall(V2, (member(C, Cs), catch(drl_modal_logic:fpn_ep(C, Ctx, V2), _, fail)), MVs),
    classify(MVs, MN, MNN, MNG),
    format("RESULT  drl_modal_logic:fpn_ep succeeded=~w non_number=~w negative=~w~n", [MN, MNN, MNG]),

    (   current_predicate(drl_modal_logic:effective_purity/3)
    ->  format("drl_modal_logic:effective_purity/3 DEFINED~n")
    ;   format("drl_modal_logic:effective_purity/3 UNDEFINED  <-- one_hop_zone is dead~n")
    ),
    findall(V3, (member(C, Cs), catch(drl_modal_logic:effective_purity(C, Ctx, V3), _, fail)), EVs),
    classify(EVs, EN, ENN, ENG),
    format("RESULT  drl_modal_logic:effective_purity succeeded=~w non_number=~w negative=~w~n", [EN, ENN, ENG]),

    % ---- the 4-arity purity-network form fpn_report/GCA actually call -----
    findall(V4, (member(C, Cs), catch(drl_purity_network:effective_purity(C, Ctx, V4, _), _, fail)), PVs),
    classify(PVs, PN, PNN, PNG),
    format("RESULT  drl_purity_network:effective_purity/4 succeeded=~w non_number=~w negative=~w~n", [PN, PNN, PNG]),

    % ---- what fpn_report's own safe accessors yield -----------------------
    findall(IP, (member(C, Cs), fpn_report:fpn_intrinsic_safe(C, IP)), IPs),
    classify(IPs, IN, INN, ING),
    format("RESULT  fpn_report:fpn_intrinsic_safe succeeded=~w non_number=~w negative=~w~n", [IN, INN, ING]),
    findall(OH, (member(C, Cs), fpn_report:one_hop_ep_safe(C, Ctx, OH)), OHs),
    classify(OHs, OHN, OHNN, OHNG),
    format("RESULT  fpn_report:one_hop_ep_safe    succeeded=~w non_number=~w negative=~w~n", [OHN, OHNN, OHNG]),
    findall(FP, (member(C, Cs), fpn_report:fpn_ep_safe(C, Ctx, FP)), FPs),
    classify(FPs, FPN, FPNN, FPNG),
    format("RESULT  fpn_report:fpn_ep_safe        succeeded=~w non_number=~w negative=~w~n", [FPN, FPNN, FPNG]).
