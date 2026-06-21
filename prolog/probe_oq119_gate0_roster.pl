% Gate 0 / G0.0 roster probe for OQ-119 (read-only; NO SPEND).
% Loads testsets_haiku and computes, per cs_kernel:
%   - reading count (b: >=2 readings)
%   - axiom-axis obstruction status (real_closure / licensed_plurality / untyped / singleton)
%   - per-reading temporal series presence: >=2 distinct timepoints with Backed=true
%     and a non-zero inter-snapshot rate on the snapshot ε (base_extractiveness)
% Then groups to the JOINT cell: kernels with >=2 readings, axiom non-vacuous,
% and >=2 readings carrying a Backed-non-vacuous temporal series.
% Witness-discipline: every count is printed; no aggregate stands alone.

:- initialization(main).

main :-
    % Overlay the haiku twin per CLAUDE.md Corpus Loading (asserta, NOT assertz).
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    [stack],
    corpus_loader:load_all_testsets,
    ( corpus_loader:corpus_loaded -> true ; (format('FATAL: corpus not loaded~n'), halt(1)) ),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorp),
    format('=== CORPUS LOAD WITNESS ===~n', []),
    format('corpus_constraint/1 count = ~w (expect ~~960 testsets_haiku)~n~n', [NCorp]),

    % All kernels (distinct cs_kernel_id values).
    findall(K, distinct_kernel(K), Ks0), sort(Ks0, Ks),
    length(Ks, NKernels),
    format('=== KERNELS ===~n', []),
    format('distinct cs_kernel_id values = ~w~n~n', [NKernels]),

    % Per-kernel record.
    findall(rec(K, NR, Status, NTempReadings),
            ( member(K, Ks),
              cs_kernel_registry:cs_kernel_coverage(K, NR),
              cs_kernel_registry:cs_kernel_obstruction_status(K, Status),
              count_temporal_readings(K, NTempReadings)
            ),
            Recs),

    % Tallies.
    include([rec(_,NR,_,_)]>>(NR >= 2), Recs, MultiReading),
    length(MultiReading, NMulti),
    format('=== (b) MULTI-READING KERNELS (>=2 readings) ===~n', []),
    format('count = ~w~n~n', [NMulti]),

    include([rec(_,NR,St,_)]>>(NR >= 2, axiom_nonvacuous(St)), Recs, AxiomLive),
    length(AxiomLive, NAxiom),
    format('=== (b)&axiom-nonvacuous (status in real_closure/licensed_plurality) ===~n', []),
    format('count = ~w~n~n', [NAxiom]),

    % Joint cell: >=2 readings, axiom non-vacuous, >=2 temporal-backed readings.
    include([rec(_,NR,St,NT)]>>(NR >= 2, axiom_nonvacuous(St), NT >= 2), Recs, JointCell),
    length(JointCell, NJoint),
    format('=== JOINT CELL (b & axiom-live & >=2 Backed-temporal readings) ===~n', []),
    format('count = ~w~n', [NJoint]),
    forall(member(rec(K,NR,St,NT), JointCell),
           format('  ~w  readings=~w  axiom=~w  backed_temporal_readings=~w~n', [K,NR,St,NT])),
    nl,

    % Looser variant: >=1 temporal-backed reading (in case the strict cut is too tight).
    include([rec(_,NR,St,NT)]>>(NR >= 2, axiom_nonvacuous(St), NT >= 1), Recs, JointCell1),
    length(JointCell1, NJoint1),
    format('=== LOOSER CELL (b & axiom-live & >=1 Backed-temporal reading) ===~n', []),
    format('count = ~w~n~n', [NJoint1]),

    format('=== STATUS DISTRIBUTION over multi-reading kernels ===~n', []),
    status_dist(MultiReading),
    halt(0).

main :- format('PROBE FAILED~n', []), halt(1).

distinct_kernel(K) :- narrative_ontology:cs_kernel_id(_, K).

axiom_nonvacuous(real_closure).
axiom_nonvacuous(licensed_plurality).

% count_temporal_readings(+K, -N): number of readings C of kernel K that carry a
% Backed-non-vacuous temporal series: >=2 distinct timepoints with Backed=true at
% each, and a non-zero rate between two consecutive Backed timepoints.
count_temporal_readings(K, N) :-
    findall(C, ( narrative_ontology:cs_kernel_id(C, K) ), Cs0), sort(Cs0, Cs),
    include(reading_temporal_nonvacuous, Cs, Good),
    length(Good, N).

reading_temporal_nonvacuous(C) :-
    % Backed timepoints under a representative analytical context.
    backed_timepoints(C, TVs),
    length(TVs, NT), NT >= 2,
    nonzero_rate(TVs).

% backed_timepoints(+C, -TVPairs): sorted T-Eps pairs at times where classify_at_time/5
% reports Backed=true (both ε and suppression authored at that time).
backed_timepoints(C, TVs) :-
    logical_fingerprint:standard_context_for_power(analytical, Ctx),
    findall(T, narrative_ontology:measurement(_, C, base_extractiveness, T, _), Ts0),
    sort(Ts0, Ts),
    findall(T-Eps,
            ( member(T, Ts),
              once(drl_composition:classify_at_time(C, T, Ctx, _Type, snap(_,true,Eps,_,_))),
              number(Eps)
            ),
            TVs0),
    sort(TVs0, TVs).

nonzero_rate([T1-V1, T2-V2 | _]) :- D is T2 - T1, D > 0, R is abs((V2 - V1)/D), R > 0.0, !.
nonzero_rate([_|Rest]) :- nonzero_rate(Rest).

status_dist(Recs) :-
    findall(St, member(rec(_,_,St,_), Recs), Sts),
    msort(Sts, Sorted),
    count_runs(Sorted).
count_runs([]).
count_runs([X|Xs]) :-
    partition([Y]>>(Y==X), Xs, Same, Rest),
    length(Same, N0), N is N0+1,
    format('  ~w: ~w~n', [X, N]),
    count_runs(Rest).
