% ============================================================================
% OQ-261 POST-HOC probe (2026-08-08, operator follow-up AFTER close — NOT part
% of the R2-signed pre-registration; labeled accordingly in WRITEUP residue).
% ============================================================================
% Question: is Cell 1's performance-vector obstruction INDEPENDENT information,
% or a restatement of the family's bloc structure that penetrates ANY sub-vector
% pooled across the six readings (seat types being story-derived)?
% Read: obstruction_from_vector over three pooled seat sets —
%   perf   = V_frozen performance seats (Cell 1's set)
%   topic  = the complement: non-excluded agent seats NOT in V_frozen and not
%            proposal-EXCLUDED (the topic-community seats)
%   all    = every non-excluded agent seat (the comparator row's set)
% Comparable H1 DENSITY (H1 / C(n_real,2)) across the three sets = bloc
% structure penetrates any pooled sub-vector -> Cell 1 carries no independent
% information about performance seats specifically.
% Run from prolog/:
%   swipl -l ../audits/2026-08-07_oq261_forced_gluing/posthoc_symmetric_read_probe.pl \
%         -g "posthoc, halt" -t "halt(1)"
% ============================================================================
:- [stack].
:- corpus_loader:load_all_testsets.

fiat_readings([empirical_precedent_reading, scholarship_reading,
               truth_procedure_reading, predictive_synthesis_reading,
               empathy_simulation_reading, utopian_fiction_reading]).

perf_frozen([competitive_debaters_running_fiat, opposing_debaters_forced_to_engage_analogy,
             coaching_programs_teaching_activism_framing, debate_institutions_claiming_civic_relevance,
             judges_and_tournament_administrators, academic_debate_community,
             competitive_debaters_without_research_access, debate_theory_analysts,
             declaring_debaters, competitive_debate_circuit, opposing_debaters_forced_into_frame,
             debate_theory_observers, policy_debate_theorists, competitive_debate_coaches,
             student_debaters, competitive_debate_participants, debate_coaches_and_programs,
             competitive_debaters, debate_coaches, debate_league_administrators,
             debate_theorists, novice_debaters_and_students]).
proposal_excluded([interdisciplinary_synthesis_researchers, analytical_observer]).

seat_tokens(C, Pairs) :-
    stakeholder_seats:stakeholder_agent_seats(C, Ns),
    stakeholder_seats:stakeholder_type_vector(C, Vec),
    pairs_keys_values(Pairs, Ns, Vec).

in_set(perf, N)  :- perf_frozen(L), memberchk(N, L).
in_set(topic, N) :- \+ (perf_frozen(L), memberchk(N, L)),
                    \+ (proposal_excluded(E), memberchk(N, E)).
in_set(all, _).

pooled(Set, PerReading, Pooled) :-
    fiat_readings(Cids),
    findall(C-Sub,
            ( member(C, Cids), seat_tokens(C, Pairs),
              findall(T, (member(N-T, Pairs), in_set(Set, N)), Sub) ),
            PerReading),
    findall(T, (member(_-S, PerReading), member(T, S)), Pooled).

report(Set) :-
    pooled(Set, PerReading, Pooled),
    length(Pooled, NP),
    include(grothendieck_cohomology:is_real_type, Pooled, Real),
    length(Real, NReal),
    aggregate_all(count,
                  ( member(_-S, PerReading),
                    include(grothendieck_cohomology:is_real_type, S, R2), R2 \== [] ),
                  NContrib),
    grothendieck_cohomology:obstruction_from_vector(Pooled, H0, H1),
    (   number(H1), NReal >= 2
    ->  MaxPairs is NReal * (NReal - 1) // 2,
        Density is H1 / MaxPairs
    ;   MaxPairs = na, Density = na
    ),
    msort(Real, RS), clumped(RS, Hist),
    (   number(Density)
    ->  format("SET ~w: pooled_n=~w n_real=~w contrib=~w H0=~w H1=~w max_pairs=~w density=~4f hist=~w~n",
               [Set, NP, NReal, NContrib, H0, H1, MaxPairs, Density, Hist])
    ;   format("SET ~w: pooled_n=~w n_real=~w contrib=~w H0=~w H1=~w max_pairs=~w density=na hist=~w~n",
               [Set, NP, NReal, NContrib, H0, H1, MaxPairs, Hist])
    ),
    forall(member(C-S, PerReading),
           ( include(grothendieck_cohomology:is_real_type, S, R3), length(R3, NR3),
             length(S, NS3),
             format("  ~w: n=~w n_real=~w~n", [C, NS3, NR3]) )).

posthoc :-
    forall(member(S, [perf, topic, all]), (report(S), nl)).
