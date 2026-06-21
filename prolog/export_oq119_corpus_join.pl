% Export three-axis JOIN RECORDS for EVERY constraint in a given corpus subdir.
% Generalizes export_oq119_join_records.pl (which hardcodes a story list) so it can run
% over each OQ-119 run-tag draw dir. Committer axis (obstruction/divergence) computes at
% the kernel level within the loaded draw (each draw holds the full reading set per kernel).
%
% Usage: swipl -q -g "main('testsets/oq119_withheld_d1','/abs/out.json')" export_oq119_corpus_join.pl
:- use_module(library(http/json)).
:- use_module(diagnostic_summary).

main(CorpusSub, OutFile) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, CorpusSub)),
    [stack],
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, N),
    format(user_error, 'loaded ~w constraints from ~w~n', [N, CorpusSub]),
    findall(C-Rec, ( member(C, Cs), join_record(C, Rec) ), Pairs),
    dict_pairs(Out, records, Pairs),
    setup_call_cleanup(open(OutFile, write, S), json_write(S, Out, [width(0)]), close(S)),
    format(user_error, 'wrote ~w records -> ~w~n', [N, OutFile]),
    halt(0).
main(_, _) :- format(user_error, 'EXPORT FAILED~n', []), halt(1).

join_record(C, _{observer:Obs, temporal:Temp, axiom:Ax}) :-
    observer_axis(C, Obs), temporal_axis(C, Temp), axiom_axis(C, Ax).

observer_axis(C, _{powerless:P, moderate:M, institutional:I, analytical:A}) :-
    seat_chi(C, powerless, P), seat_chi(C, moderate, M),
    seat_chi(C, institutional, I), seat_chi(C, analytical, A).
seat_chi(C, Power, Chi) :-
    logical_fingerprint:standard_context_for_power(Power, Ctx),
    ( catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), _, fail)
    -> Chi is round(Chi0*10000)/10000 ; Chi = null ).

temporal_axis(C, Dict) :-
    Metrics = [base_extractiveness, suppression_requirement, theater_ratio],
    findall(M-MRec, ( member(M, Metrics), once(metric_temporal(C, M, MRec)) ), Ps),
    dict_pairs(Dict, t, Ps).
metric_temporal(C, M, _{n_points:NP, mean_rate:MR, slope_sign:SS}) :-
    findall(T-V, narrative_ontology:measurement(_, C, M, T, V), Ps0), sort(Ps0, Ps),
    length(Ps, NP), NP >= 1,
    ( compute_rates(Ps, Rates), Rates \= []
    ->  findall(R, member(rate(_,_,R), Rates), Rs), sum_list(Rs, Sum), length(Rs, NR),
        MR is round((Sum/NR)*100000)/100000,
        ( MR > 0.0 -> SS = 1 ; MR < 0.0 -> SS = -1 ; SS = 0 )
    ;   MR = 0.0, SS = 0 ).
metric_temporal(_, _, _{n_points:0, mean_rate:0.0, slope_sign:0}).
compute_rates([], []).
compute_rates([_], []).
compute_rates([T1-V1, T2-V2|R], Rates) :-
    D is T2-T1,
    ( D > 0 -> Rt is (V2-V1)/D, Rates = [rate(T1,T2,Rt)|Rr] ; Rates = Rr ),
    compute_rates([T2-V2|R], Rr).

axiom_axis(C, _{kernel:KStr, obstruction_status:StStr, divergence_scopes:Scopes,
                verdict_joined:VJ, cap:Cap, sig_grade:SG, n_alerts:NA}) :-
    ( narrative_ontology:cs_kernel_id(C, K) -> true ; K = none ), atom_string(K, KStr),
    ( cs_kernel_registry:cs_kernel_obstruction_status(K, St) -> true ; St = none ), atom_string(St, StStr),
    findall(ScStr, ( cs_kernel_registry:cs_kernel_divergence(K, context(_,_,_,spatial_scope(Sc)), U1-_, U2-_),
                     U1 @< U2, atom_string(Sc, ScStr) ), Scopes0),
    sort(Scopes0, Scopes),
    ( catch(diagnostic_summary:diagnostic_summary(C, Summary), _, fail),
      catch(diagnostic_summary:verdict_join(C, Summary, verdict_join(J,_,Cp,Alerts,_,_,Sg)), _, fail)
    ->  atom_string(J, VJ), atom_string(Cp, Cap), atom_string(Sg, SG), length(Alerts, NA)
    ;   VJ = "unknown", Cap = "unknown", SG = "unknown", NA = 0 ).
