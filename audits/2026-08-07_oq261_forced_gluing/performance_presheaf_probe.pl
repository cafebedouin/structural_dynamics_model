% ============================================================================
% OQ-261 C3 probe — performance presheaf vs topic presheaf (COMPUTE-ONLY)
% ============================================================================
% Run from prolog/:
%   swipl -l ../audits/2026-08-07_oq261_forced_gluing/performance_presheaf_probe.pl \
%         -g "c3, halt" -t "halt(1)"
% Implements PROPOSAL.md v2 + R2 riders exactly:
%   Cell 1  — fiat pooled performance vector, THREE partition variants, pooled
%             sparsity floor (n_real>=6 AND >=4/6 readings contributing) else NULL
%   Cell 2  — discard minimum: smallest reading-subset whose removal makes every
%             context's topic vector glue (H1=0 or null at every context)
%   Cell 3  — control performance sub-vectors per variant (probe-bug license:
%             numeric H at pooled n_real<2)
%   Cell 4  — control pooled stakeholder vector
%   Comparator — ALL real_closure kernels: pooled non-excluded-agent-seat vector
%             (mechanical proxy), normalized floor, NULL reported as NULL
%   Fiat proxy read — same mechanical read on fiat (proxy-vs-hand comparison,
%             rider 2 branch rule)
%   MC tokens — consensus_provenance re-report, same run
% Seat typing: stakeholder_seats:stakeholder_type_vector/2 zipped with
% stakeholder_agent_seats/2 (same-length maplist; kernel-facing tokens only —
% the census-facing `untyped` never enters a vector). Gluing read: pure
% grothendieck_cohomology:obstruction_from_vector/3. No writes.
% ============================================================================
:- [stack].
:- corpus_loader:load_all_testsets.

fiat_kernel(fiat_efficacy_kernel).
fiat_readings([empirical_precedent_reading, scholarship_reading,
               truth_procedure_reading, predictive_synthesis_reading,
               empathy_simulation_reading, utopian_fiction_reading]).
control_cid(fiat_efficacy_kernel_flat_control).

% ---- frozen partition variants (PROPOSAL v2, verbatim lists) ---------------
perf_frozen([competitive_debaters_running_fiat, opposing_debaters_forced_to_engage_analogy,
             coaching_programs_teaching_activism_framing, debate_institutions_claiming_civic_relevance,
             judges_and_tournament_administrators, academic_debate_community,
             competitive_debaters_without_research_access, debate_theory_analysts,
             declaring_debaters, competitive_debate_circuit, opposing_debaters_forced_into_frame,
             debate_theory_observers, policy_debate_theorists, competitive_debate_coaches,
             student_debaters, competitive_debate_participants, debate_coaches_and_programs,
             competitive_debaters, debate_coaches, debate_league_administrators,
             debate_theorists, novice_debaters_and_students]).
perf_restrictive([competitive_debaters_running_fiat, opposing_debaters_forced_to_engage_analogy,
                  judges_and_tournament_administrators, competitive_debaters_without_research_access,
                  declaring_debaters, opposing_debaters_forced_into_frame, student_debaters,
                  competitive_debate_participants, competitive_debaters,
                  novice_debaters_and_students]).

perf_seat(frozen, S)      :- perf_frozen(L), memberchk(S, L).
perf_seat(inclusive, S)   :- perf_frozen(L), memberchk(S, L).
perf_seat(inclusive, interdisciplinary_synthesis_researchers).
perf_seat(inclusive, analytical_observer).
perf_seat(restrictive, S) :- perf_restrictive(L), memberchk(S, L).

% ---- seat tokens (name-token pairs, kernel-facing) -------------------------
seat_tokens(C, Pairs) :-
    stakeholder_seats:stakeholder_agent_seats(C, Ns),
    stakeholder_seats:stakeholder_type_vector(C, Vec),
    pairs_keys_values(Pairs, Ns, Vec).

real_count(Vec, N) :-
    include(grothendieck_cohomology:is_real_type, Vec, R), length(R, N).

% ---- Cell 1: fiat pooled performance vector per variant --------------------
cell1(Variant) :-
    fiat_readings(Cids),
    findall(C-Sub,
            ( member(C, Cids), seat_tokens(C, Pairs),
              findall(T, (member(N-T, Pairs), perf_seat(Variant, N)), Sub) ),
            PerReading),
    findall(T, (member(_-Sub, PerReading), member(T, Sub)), Pooled),
    length(Pooled, NPool), real_count(Pooled, NReal),
    aggregate_all(count,
                  ( member(_-Sub2, PerReading), real_count(Sub2, NR2), NR2 >= 1 ),
                  NContrib),
    format("CELL1 variant=~w pooled_n=~w pooled_n_real=~w readings_contributing=~w~n",
           [Variant, NPool, NReal, NContrib]),
    forall(member(C3-Sub3, PerReading),
           ( length(Sub3, NS3), real_count(Sub3, NR3),
             format("  ~w: n=~w n_real=~w tokens=~w~n", [C3, NS3, NR3, Sub3]) )),
    (   ( NReal >= 6, NContrib >= 4 )
    ->  grothendieck_cohomology:obstruction_from_vector(Pooled, H0, H1),
        sort(Pooled, Uniq),
        format("CELL1 variant=~w VERDICT H0=~w H1=~w (floor met) distinct_tokens=~w~n",
               [Variant, H0, H1, Uniq])
    ;   format("CELL1 variant=~w VERDICT NULL (below floor: n_real=~w contrib=~w)~n",
               [Variant, NReal, NContrib])
    ).

% ---- Cell 2: discard minimum over the topic presheaf -----------------------
% Precompute per-context per-reading types once; then search subsets by size.
topic_types(CtxTypes) :-
    fiat_readings(Cids),
    constraint_indexing:site_contexts_product(Ctxs),
    findall(Ctx-Types,
            ( member(Ctx, Ctxs),
              findall(C-T, ( member(C, Cids), once(drl_core:dr_type(C, Ctx, T)) ), Types) ),
            CtxTypes).

all_glue(CtxTypes, Removed) :-
    forall(member(_-Types, CtxTypes),
           ( findall(T, ( member(C-T, Types), \+ memberchk(C, Removed) ), Vec),
             grothendieck_cohomology:obstruction_from_vector(Vec, _, H1),
             ( H1 == null ; H1 =:= 0 ) )).

subset_of_size(0, _, []) :- !.
subset_of_size(N, [X|Xs], [X|S]) :- N1 is N-1, subset_of_size(N1, Xs, S).
subset_of_size(N, [_|Xs], S) :- subset_of_size(N, Xs, S).

cell2 :-
    topic_types(CtxTypes),
    fiat_readings(Cids),
    between(0, 5, K),
    findall(S, ( subset_of_size(K, Cids, S), all_glue(CtxTypes, S) ), Sols),
    Sols \== [],
    !,
    length(Sols, NSol),
    format("CELL2 discard_minimum=~w achieving_subsets=~w~n", [K, NSol]),
    forall(member(S, Sols), format("  remove ~w~n", [S])).
cell2 :- format("CELL2 discard_minimum=NONE (no subset up to 5 glues everything)~n").

% ---- Cells 3+4: control -----------------------------------------------------
cell34 :-
    control_cid(C),
    seat_tokens(C, Pairs),
    forall(member(V, [frozen, inclusive, restrictive]),
           ( findall(T, (member(N-T, Pairs), perf_seat(V, N)), Sub),
             length(Sub, NS), real_count(Sub, NR),
             grothendieck_cohomology:obstruction_from_vector(Sub, H0, H1),
             format("CELL3 variant=~w n=~w n_real=~w H0=~w H1=~w tokens=~w~n",
                    [V, NS, NR, H0, H1, Sub]),
             ( NR < 2, number(H1)
             -> format("CELL3 PROBE-BUG WITNESS: numeric H at n_real<2~n")
             ;  true ) )),
    pairs_values(Pairs, FullVec),
    length(FullVec, NF), real_count(FullVec, NFR),
    grothendieck_cohomology:obstruction_from_vector(FullVec, H0f, H1f),
    format("CELL4 control pooled stakeholder vector n=~w n_real=~w H0=~w H1=~w tokens=~w~n",
           [NF, NFR, H0f, H1f, FullVec]).

% ---- Comparator: all real_closure kernels, mechanical proxy ----------------
comparator :-
    findall(K, ( narrative_ontology:cs_kernel_id(_, K), atom(K) ), Ks0),
    sort(Ks0, Ks),
    format("COMPARATOR (pooled non-excluded-agent-seat vector per real_closure kernel):~n"),
    forall(( member(K, Ks),
             cs_kernel_registry:cs_kernel_obstruction_status(K, real_closure) ),
           comparator_row(K)).

comparator_row(K) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    length(Pairs, NReadings),
    findall(C-Vec, ( member(_-C, Pairs), seat_tokens(C, P), pairs_values(P, Vec) ),
            PerReading),
    findall(T, ( member(_-V, PerReading), member(T, V) ), Pooled),
    length(Pooled, NPool), real_count(Pooled, NReal),
    aggregate_all(count, ( member(_-V2, PerReading), real_count(V2, NR2), NR2 >= 1 ),
                  NContrib),
    MinContrib is (2*NReadings + 2) // 3,     % ceil(2n/3)
    (   ( NReal >= NReadings, NContrib >= MinContrib )
    ->  grothendieck_cohomology:obstruction_from_vector(Pooled, H0, H1),
        ( H1 == 0 -> V = glue ; V = obstruct ),
        format("  ~w: n_readings=~w pooled_n=~w n_real=~w contrib=~w H0=~w H1=~w -> ~w~n",
               [K, NReadings, NPool, NReal, NContrib, H0, H1, V])
    ;   format("  ~w: n_readings=~w pooled_n=~w n_real=~w contrib=~w (floor ~w/~w) -> NULL~n",
               [K, NReadings, NPool, NReal, NContrib, NReadings, MinContrib])
    ).

% ---- MC tokens (same-run re-report) ----------------------------------------
mc_tokens :-
    fiat_readings(Cids), control_cid(Ctl),
    append(Cids, [Ctl], All),
    forall(member(C, All),
           ( stakeholder_seats:consensus_provenance(C, V),
             format("MC ~w: ~w~n", [C, V]) )).

c3 :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("corpus loaded: ~w constraints~n~n", [NC]),
    forall(member(V, [frozen, inclusive, restrictive]), (cell1(V), nl)),
    cell2, nl,
    cell34, nl,
    comparator, nl,
    mc_tokens.
