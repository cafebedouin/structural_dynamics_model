% ============================================================================
% TRAJECTORY MINING REPORT — Structural Family Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%         -l maxent_classifier.pl -l trajectory_mining.pl \
%         -l trajectory_report.pl -g "run_trajectory_report, halt."
%
% Orchestrates trajectory mining prerequisites and outputs a structured
% markdown report with family census, cross-domain isomorphisms,
% Phase A validation, and orbit refinement analysis.
%
% Pattern follows abductive_report.pl exactly.
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(boltzmann_compliance, [cross_index_coupling/2]).
:- use_module(dirac_classification).
:- use_module(maxent_classifier).
:- use_module(drl_lifecycle).
:- use_module(logical_fingerprint).
:- use_module(trajectory_mining).

:- use_module(library(lists)).

%% run_trajectory_report
%  Main entry point. Loads corpus, runs trajectory mining, outputs markdown.
run_trajectory_report :-
    format(user_error, '[trajectory_report] Starting trajectory mining analysis...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Run trajectory mining
    format(user_error, '[trajectory_report] Running trajectory mining...~n', []),
    trajectory_mining:trajectory_run(Context, Summary),
    Summary = trajectory_summary(
        total_constraints(NTrajectories),
        families(NFamilies),
        cross_domain_twins(NTwins),
        anomalies(NSingletons)
    ),

    % Write markdown report
    format('<!-- TRAJECTORY_REPORT_START -->~n'),
    format('# Trajectory Mining Report~n~n'),
    format('*Generated: structural family analysis via trajectory_mining:trajectory_run/2*~n~n'),

    report_summary(NTrajectories, NFamilies, NTwins, NSingletons, Context),
    report_family_census(Context),
    report_cross_domain_isomorphisms(Context),
    report_phase_a_validation(Context),
    report_orbit_refinement(Context),

    format('---~n'),
    format('*End of trajectory mining report*~n'),
    format(user_error, '[trajectory_report] Done.~n', []).

/* ================================================================
   SECTION 1: SUMMARY
   ================================================================ */

report_summary(NTrajectories, NFamilies, NTwins, NSingletons, _Context) :-
    format('## Summary~n~n'),
    format('| Property | Value |~n'),
    format('|----------|-------|~n'),
    format('| **Total trajectories** | ~w |~n', [NTrajectories]),
    format('| **Structural families** | ~w |~n', [NFamilies]),
    format('| **Cross-domain twins** | ~w |~n', [NTwins]),
    format('| **Singletons (anomalies)** | ~w |~n~n', [NSingletons]),

    % Average family size
    (   NFamilies > 0
    ->  AvgSize is NTrajectories / NFamilies,
        format('| **Average family size** | ~1f |~n', [AvgSize])
    ;   true
    ),

    % Largest / smallest families
    findall(Size-FID, (
        trajectory_mining:cluster_members(FID, Members),
        length(Members, Size)
    ), SizePairs),
    (   SizePairs \= []
    ->  msort(SizePairs, Sorted),
        last(Sorted, LargestSize-LargestFID),
        Sorted = [SmallestSize-SmallestFID|_],
        format('| **Largest family** | ~w (ID: ~w) |~n', [LargestSize, LargestFID]),
        format('| **Smallest family** | ~w (ID: ~w) |~n', [SmallestSize, SmallestFID])
    ;   true
    ),
    format('~n').

/* ================================================================
   SECTION 2: FAMILY CENSUS
   ================================================================ */

report_family_census(_Context) :-
    format('## Family Census~n~n'),
    format('Each structural family with member count, representative constraint, and dominant features.~n~n'),

    findall(Size-FID, (
        trajectory_mining:cluster_members(FID, Members),
        length(Members, Size)
    ), SizePairs),
    msort(SizePairs, SortedAsc),
    reverse(SortedAsc, SortedDesc),

    format('| Family ID | Size | Representative | Dominant Shift | Key Feature |~n'),
    format('|-----------|------|---------------|----------------|-------------|~n'),

    forall(member(Size-FID, SortedDesc), (
        trajectory_mining:cluster_members(FID, Members),
        Members = [Rep|_],
        get_family_shift(Members, DomShift),
        get_family_feature(Members, Feature),
        format('| ~w | ~w | ~w | ~w | ~w |~n', [FID, Size, Rep, DomShift, Feature])
    )),
    format('~n').

get_family_shift(Members, DomShift) :-
    findall(Shift, (
        member(C, Members),
        catch(logical_fingerprint:fingerprint_shift(C, Shift), _, fail)
    ), Shifts),
    (   Shifts \= []
    ->  msort(Shifts, Sorted),
        clumped(Sorted, Clumped),
        findall(Count-S, member(S-Count, Clumped), Pairs),
        msort(Pairs, SPairs),
        last(SPairs, _-DomShift)
    ;   DomShift = unknown
    ).

get_family_feature(Members, Feature) :-
    % Determine the most notable feature of this family
    findall(Coup, (
        member(C, Members),
        catch(boltzmann_compliance:cross_index_coupling(C, Coup), _, fail)
    ), Coups),
    (   Coups \= []
    ->  sum_list(Coups, SumC), length(Coups, NC),
        MeanCoup is SumC / NC
    ;   MeanCoup = 0.0
    ),
    findall(DC, (
        member(C, Members),
        catch(drl_lifecycle:scan_constraint_drift(C, DEs), _, (DEs = [])),
        length(DEs, DC)
    ), DriftCounts),
    (   DriftCounts \= []
    ->  sum_list(DriftCounts, SumDC), length(DriftCounts, NDC),
        MeanDrift is SumDC / NDC
    ;   MeanDrift = 0.0
    ),
    (   MeanCoup > 0.5 -> Feature = high_coupling
    ;   MeanDrift > 2.0 -> Feature = active_drift
    ;   MeanCoup < 0.1 -> Feature = low_coupling
    ;   Feature = standard
    ).

/* ================================================================
   SECTION 3: CROSS-DOMAIN ISOMORPHISMS
   ================================================================ */

report_cross_domain_isomorphisms(Context) :-
    format('## Cross-Domain Isomorphisms~n~n'),
    format('Constraint pairs from different domains sharing the same structural family.~n~n'),

    config:param(trajectory_isomorphism_threshold, Thresh),
    trajectory_mining:cross_domain_twins(Context, Thresh, Twins),
    length(Twins, NTwins),

    (   NTwins =:= 0
    ->  format('No cross-domain twins detected at threshold ~2f.~n~n', [Thresh])
    ;   format('Found ~w cross-domain twin pairs (threshold: ~2f).~n~n', [NTwins, Thresh]),
        % Show top 20 by distance (closest first)
        (   NTwins > 20
        ->  length(Top20, 20), append(Top20, _, Twins),
            ShowTwins = Top20
        ;   ShowTwins = Twins
        ),
        format('### Top ~w Cross-Domain Twins~n~n', [NTwins]),
        format('| Constraint 1 | Domain 1 | Constraint 2 | Domain 2 | Distance | Family |~n'),
        format('|--------------|----------|--------------|----------|----------|--------|~n'),
        forall(member(twin(C1, C2, Dist, F), ShowTwins), (
            trajectory_mining:constraint_domain(C1, D1),
            trajectory_mining:constraint_domain(C2, D2),
            format('| ~w | ~w | ~w | ~w | ~4f | ~w |~n', [C1, D1, C2, D2, Dist, F])
        )),
        format('~n')
    ).

/* ================================================================
   SECTION 4: PHASE A VALIDATION (The 6 Genuine Findings)
   ================================================================ */

report_phase_a_validation(Context) :-
    format('## Phase A Validation~n~n'),
    format('Checking whether the 6 genuine abductive findings are correctly grouped.~n~n'),

    FamilyA = [decentralized_infrastructure_rope, fair_use_doctrine,
               noethers_theorem_symmetry, reciprocity_laws_math],
    FamilyB = [moltbook_agent_theater, ulysses_calypso_1904],

    % Family A analysis
    format('### Family A (Preserved-Orbit Rope/Scaffold Ambiguity)~n~n'),
    format('| Constraint | Family ID | Orbit | Coupling | Drift Count |~n'),
    format('|------------|-----------|-------|----------|-------------|~n'),
    forall(member(C, FamilyA), (
        report_constraint_family_row(C, Context)
    )),
    format('~n'),
    check_same_family(FamilyA, 'Family A'),

    % Family B analysis
    format('### Family B (Violated-Orbit Liminal Cases)~n~n'),
    format('| Constraint | Family ID | Orbit | Coupling | Drift Count |~n'),
    format('|------------|-----------|-------|----------|-------------|~n'),
    forall(member(C, FamilyB), (
        report_constraint_family_row(C, Context)
    )),
    format('~n'),
    check_same_family(FamilyB, 'Family B'),

    % Cross-family check
    format('### Cross-Family Separation~n~n'),
    (   FamilyA = [FA1|_], FamilyB = [FB1|_],
        trajectory_mining:family_assignment(FA1, FIDA),
        trajectory_mining:family_assignment(FB1, FIDB)
    ->  (   FIDA \= FIDB
        ->  format('**PASS**: Family A (ID: ~w) and Family B (ID: ~w) are in different structural families.~n~n', [FIDA, FIDB])
        ;   format('**NOTE**: Family A and Family B share the same structural family ID: ~w~n~n', [FIDA])
        )
    ;   format('Could not check — one or more constraints not found in corpus.~n~n')
    ).

report_constraint_family_row(C, _Context) :-
    (   trajectory_mining:family_assignment(C, FID) -> true ; FID = '(not found)' ),
    (   catch(dirac_classification:gauge_orbit(C, OrbitPoints), _, fail)
    ->  findall(T, member(orbit_point(T, _), OrbitPoints), Ts),
        sort(Ts, Orbit)
    ;   Orbit = unknown
    ),
    (   catch(boltzmann_compliance:cross_index_coupling(C, Coup), _, fail)
    ->  true
    ;   Coup = '?'
    ),
    (   catch(drl_lifecycle:scan_constraint_drift(C, DEs), _, (DEs = []))
    ->  length(DEs, DC)
    ;   DC = 0
    ),
    format('| ~w | ~w | ~w | ~w | ~w |~n', [C, FID, Orbit, Coup, DC]).

check_same_family(Members, Label) :-
    findall(FID, (
        member(C, Members),
        trajectory_mining:family_assignment(C, FID)
    ), FIDs),
    sort(FIDs, Unique),
    length(Unique, N),
    (   N =:= 1
    ->  format('**PASS**: All ~w members share family ID ~w.~n~n', [Label, Unique])
    ;   N =:= 0
    ->  format('**SKIP**: No ~w members found in corpus.~n~n', [Label])
    ;   format('**NOTE**: ~w members span ~w families: ~w~n~n', [Label, N, Unique])
    ).

/* ================================================================
   SECTION 5: ORBIT REFINEMENT
   ================================================================ */

report_orbit_refinement(_Context) :-
    format('## Orbit Refinement~n~n'),
    format('How many of the 24 orbit families were split by trajectory mining?~n~n'),

    % For each orbit family, count how many structural families it maps to
    findall(OrbitFamily-FID, (
        trajectory_mining:family_assignment(C, FID),
        catch(dirac_classification:gauge_orbit(C, OrbitPoints), _, fail),
        findall(T, member(orbit_point(T, _), OrbitPoints), Ts),
        sort(Ts, OrbitFamily)
    ), OrbitFamilyPairs),
    msort(OrbitFamilyPairs, SortedPairs),

    % Group by orbit family, count unique structural families
    (   SortedPairs \= []
    ->  group_pairs_by_key(SortedPairs, Grouped),
        findall(orbit_split(OF, NOrbFams, NConstraints), (
            member(OF-FIDs, Grouped),
            sort(FIDs, UniqueFIDs),
            length(UniqueFIDs, NOrbFams),
            length(FIDs, NConstraints)
        ), Splits),
        format('| Orbit Family | Constraints | Structural Families | Split? |~n'),
        format('|--------------|-------------|---------------------|--------|~n'),
        forall(member(orbit_split(OF, NOF, NC), Splits), (
            (NOF > 1 -> Split = 'YES' ; Split = 'no'),
            format('| ~w | ~w | ~w | ~w |~n', [OF, NC, NOF, Split])
        )),
        format('~n'),

        % Summary stats
        include(was_split, Splits, SplitOrbits),
        length(SplitOrbits, NSplit),
        length(Splits, NTotal),
        format('**Summary**: ~w / ~w orbit families were split by trajectory mining.~n', [NSplit, NTotal]),

        % Count total structural families
        findall(FID, trajectory_mining:family_assignment(_, FID), AllFIDs),
        sort(AllFIDs, UniqueFIDs),
        length(UniqueFIDs, NFamilies),
        format('**Total structural families**: ~w (vs ~w orbit families)~n~n', [NFamilies, NTotal])
    ;   format('No orbit data available.~n~n')
    ).

was_split(orbit_split(_, N, _)) :- N > 1.
