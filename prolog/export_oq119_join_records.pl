% Export compact three-axis JOIN RECORDS for a set of roster stories (read-only).
% Used by the OQ-119 join-diff comparator (Phase 1) for its self/cross controls and,
% post-spend-go, for the real fed-vs-withheld diff. Emits one JSON object per story:
%   observer: {powerless,moderate,institutional,analytical} chi
%   temporal: per join-relevant metric -> {n_backed, mean_rate, slope_sign}
%   axiom:    {kernel, obstruction_status, divergence_scopes, verdict_joined, cap,
%              sig_grade, n_alerts}
:- use_module(library(http/json)).
:- initialization((main, halt)).

% diagnostic_summary is not pulled in by [stack]; load it for verdict_join/3.
:- use_module(diagnostic_summary).

% Stories to export. Two readings of two distinct kernels: lets the comparator's
% cross-control compare both same-kernel siblings and cross-kernel pairs.
stories([ acceptable_risk_energy__expected_value_dominant,
          acceptable_risk_energy__catastrophic_tail_dominant,
          westphalia_sovereignty__absolute_non_intervention,
          woman_category__sex_biology_reading ]).

main :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    [stack],
    corpus_loader:load_all_testsets,
    stories(Cs),
    findall(C-Rec, ( member(C, Cs), join_record(C, Rec) ), Pairs),
    dict_pairs(Out, records, Pairs),
    OutFile = '../audits/2026-06-21_oq119_gate0/join_records.json',
    setup_call_cleanup(open(OutFile, write, S),
                       json_write(S, Out, [width(0)]),
                       close(S)),
    format('wrote ~w records to ~w~n', [Pairs, OutFile]).

join_record(C, _{observer:Obs, temporal:Temp, axiom:Ax}) :-
    observer_axis(C, Obs),
    temporal_axis(C, Temp),
    axiom_axis(C, Ax).

% ---- observer ----
observer_axis(C, _{powerless:P, moderate:M, institutional:I, analytical:A}) :-
    seat_chi(C, powerless, P), seat_chi(C, moderate, M),
    seat_chi(C, institutional, I), seat_chi(C, analytical, A).
seat_chi(C, Power, Chi) :-
    logical_fingerprint:standard_context_for_power(Power, Ctx),
    ( catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), _, fail)
    -> Chi is round(Chi0*10000)/10000 ; Chi = null ).

% ---- temporal ----
temporal_axis(C, Dict) :-
    Metrics = [base_extractiveness, suppression_requirement, theater_ratio],
    findall(M-MRec, ( member(M, Metrics), metric_temporal(C, M, MRec) ), Ps),
    dict_pairs(Dict, t, Ps).
metric_temporal(C, M, _{n_points:NP, mean_rate:MR, slope_sign:SS}) :-
    findall(T-V, narrative_ontology:measurement(_, C, M, T, V), Ps0), sort(Ps0, Ps),
    length(Ps, NP), NP >= 1,
    ( compute_rates(Ps, Rates), Rates \= []
    ->  findall(R, member(rate(_,_,R), Rates), Rs), sum_list(Rs, Sum), length(Rs, NR),
        MR is round((Sum/NR)*100000)/100000,
        ( MR > 0.0 -> SS = 1 ; MR < 0.0 -> SS = -1 ; SS = 0 )
    ;   MR = 0.0, SS = 0 ).
compute_rates([], []).
compute_rates([_], []).
compute_rates([T1-V1, T2-V2|R], Rates) :-
    D is T2-T1,
    ( D > 0 -> Rt is (V2-V1)/D, Rates = [rate(T1,T2,Rt)|Rr] ; Rates = Rr ),
    compute_rates([T2-V2|R], Rr).

% ---- axiom ----
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
