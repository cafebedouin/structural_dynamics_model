% ============================================================================
% ORBIT REPORT — Corpus-Wide Gauge Orbit Analysis
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l dirac_classification.pl \
%         -l orbit_report.pl -g "run_orbit_report, halt."
%
% Bulk-loads all testsets, computes gauge_orbit/2 for every constraint,
% and outputs:
%   1. Markdown report to stdout (redirect to orbit_report.md)
%   2. JSON sidecar to ../outputs/orbit_data.json
% ============================================================================

:- use_module(covering_analysis).
:- use_module(dirac_classification).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(domain_priors).
:- use_module(data_repair).

:- use_module(library(lists)).

%% run_orbit_report
%  Main entry point. Loads corpus, computes orbits, outputs markdown + JSON.
run_orbit_report :-
    format(user_error, '[orbit] Starting orbit analysis...~n', []),
    covering_analysis:load_all_testsets,
    covering_analysis:all_corpus_constraints(Constraints),
    length(Constraints, NTotal),
    format(user_error, '[orbit] Computing orbits for ~w constraints...~n', [NTotal]),

    % Compute orbit for each constraint
    compute_all_orbits(Constraints, Orbits),
    length(Orbits, NComputed),
    format(user_error, '[orbit] Computed ~w orbits.~n', [NComputed]),

    % Group by orbit signature
    group_by_signature(Orbits, Families),
    length(Families, NFamilies),
    format(user_error, '[orbit] Found ~w orbit families.~n', [NFamilies]),

    % Write JSON sidecar
    write_orbit_json(Orbits),
    format(user_error, '[orbit] Wrote orbit_data.json~n', []),

    % Write markdown report to stdout (delimiter strips FNL preamble noise)
    format('<!-- ORBIT_REPORT_START -->~n'),
    format('# Gauge Orbit Report~n~n'),
    format('*Generated: corpus-wide orbit signature analysis via dirac_classification:gauge_orbit/2*~n~n'),

    report_summary(NComputed, NFamilies, Families),
    report_family_inventory(Families),
    report_cross_type_analysis(Families),
    report_unknown_orbits(Families),
    report_singleton_vs_multi(Families),

    format('---~n'),
    format('*End of orbit report*~n'),
    format(user_error, '[orbit] Done.~n', []).

/* ================================================================
   ORBIT COMPUTATION
   ================================================================ */

%% compute_all_orbits(+Constraints, -Orbits)
%  Orbits = [orbit(Id, Signature, ContextMap), ...]
%  Signature = sorted list of unique types (e.g., [rope, snare])
%  ContextMap = [ctx(powerless, Type), ctx(moderate, Type), ...]
compute_all_orbits([], []).
compute_all_orbits([C|Cs], Orbits) :-
    (   catch(compute_one_orbit(C, Orbit), _, fail)
    ->  Orbits = [Orbit|Rest]
    ;   Orbits = Rest
    ),
    compute_all_orbits(Cs, Rest).

compute_one_orbit(C, orbit(C, Signature, ContextMap)) :-
    dirac_classification:gauge_orbit(C, OrbitPoints),
    OrbitPoints \= [],
    % Extract signature (sorted unique types)
    findall(T, member(orbit_point(T, _), OrbitPoints), Types),
    sort(Types, Signature),
    % Build context map using agent_power as key
    findall(ctx(Power, Type),
            (member(orbit_point(Type, Ctx), OrbitPoints),
             Ctx = context(agent_power(Power), _, _, _)),
            ContextMap).

/* ================================================================
   GROUPING
   ================================================================ */

%% group_by_signature(+Orbits, -Families)
%  Families = [family(Signature, Members), ...]
%  Members = [orbit(...), ...]
group_by_signature(Orbits, Families) :-
    findall(Sig, member(orbit(_, Sig, _), Orbits), SigsRaw),
    sort(SigsRaw, UniqueSigs),
    findall(family(Sig, Members),
            (member(Sig, UniqueSigs),
             findall(O, (member(O, Orbits), O = orbit(_, Sig, _)), Members)),
            Families).

/* ================================================================
   JSON OUTPUT
   ================================================================ */

%% write_orbit_json(+Orbits)
%  Writes outputs/orbit_data.json with per-constraint orbit data.
write_orbit_json(Orbits) :-
    setup_call_cleanup(
        open('../outputs/orbit_data.json', write, S),
        write_json_stream(S, Orbits),
        close(S)
    ).

write_json_stream(S, Orbits) :-
    format(S, '{~n', []),
    write_json_entries(S, Orbits),
    format(S, '~n}~n', []).

write_json_entries(_, []).
write_json_entries(S, [orbit(Id, Sig, CtxMap)]) :-
    !,  % Last entry — no trailing comma
    write_json_entry(S, Id, Sig, CtxMap, false).
write_json_entries(S, [orbit(Id, Sig, CtxMap)|Rest]) :-
    write_json_entry(S, Id, Sig, CtxMap, true),
    write_json_entries(S, Rest).

write_json_entry(S, Id, Sig, CtxMap, Comma) :-
    format(S, '  "~w": {~n', [Id]),
    % orbit_signature array
    format(S, '    "orbit_signature": [', []),
    write_json_string_list(S, Sig),
    format(S, '],~n', []),
    % contexts object
    format(S, '    "contexts": {~n', []),
    write_json_contexts(S, CtxMap),
    format(S, '    }~n', []),
    (Comma == true -> format(S, '  },~n', []) ; format(S, '  }~n', [])).

write_json_string_list(_, []).
write_json_string_list(S, [X]) :-
    !, format(S, '"~w"', [X]).
write_json_string_list(S, [X|Xs]) :-
    format(S, '"~w", ', [X]),
    write_json_string_list(S, Xs).

write_json_contexts(_, []).
write_json_contexts(S, [ctx(Power, Type)]) :-
    !, format(S, '      "~w": "~w"~n', [Power, Type]).
write_json_contexts(S, [ctx(Power, Type)|Rest]) :-
    format(S, '      "~w": "~w",~n', [Power, Type]),
    write_json_contexts(S, Rest).

/* ================================================================
   MARKDOWN REPORT SECTIONS
   ================================================================ */

%% report_summary(+NConstraints, +NFamilies, +Families)
report_summary(NConstraints, NFamilies, Families) :-
    % Find largest family
    findall(Size-Sig,
            (member(family(Sig, Members), Families), length(Members, Size)),
            SizePairs),
    sort(0, @>=, SizePairs, [LargestSize-LargestSig|_]),
    % Count singletons (single-type orbits)
    findall(Sig, (member(family(Sig, _), Families), Sig = [_]), SingletonSigs),
    length(SingletonSigs, NSingleton),
    % Count multi-type
    NMulti is NFamilies - NSingleton,

    format('## Summary~n~n'),
    format('- **Constraints analyzed**: ~w~n', [NConstraints]),
    format('- **Orbit families**: ~w~n', [NFamilies]),
    format('- **Single-type families** (gauge-invariant): ~w~n', [NSingleton]),
    format('- **Multi-type families** (gauge-variant): ~w~n', [NMulti]),
    format('- **Largest family**: ~w (~w constraints)~n~n', [LargestSig, LargestSize]).

%% report_family_inventory(+Families)
report_family_inventory(Families) :-
    % Sort families by size descending
    findall(Size-family(Sig, Members),
            (member(family(Sig, Members), Families), length(Members, Size)),
            SizePairs),
    sort(0, @>=, SizePairs, Sorted),

    format('## Family Inventory~n~n'),
    format('Families sorted by population (largest first).~n~n'),
    forall(
        member(Size-family(Sig, Members), Sorted),
        report_one_family(Sig, Size, Members)
    ).

report_one_family(Sig, Size, Members) :-
    format('### `~w` — ~w constraints~n~n', [Sig, Size]),
    % Characterize the family
    characterize_family(Sig, CharDesc),
    format('*~w*~n~n', [CharDesc]),
    % List members
    findall(Id, member(orbit(Id, _, _), Members), Ids),
    sort(Ids, SortedIds),
    forall(member(Id, SortedIds), format('- `~w`~n', [Id])),
    format('~n').

%% characterize_family(+Signature, -Description)
characterize_family([mountain], 'Gauge-invariant: immutable restriction from all perspectives') :- !.
characterize_family([rope], 'Gauge-invariant: pure coordination from all perspectives') :- !.
characterize_family([tangled_rope], 'Gauge-invariant: tangled coordination-extraction from all perspectives') :- !.
characterize_family([snare], 'Gauge-invariant: pure extraction from all perspectives') :- !.
characterize_family([scaffold], 'Gauge-invariant: temporary coordination from all perspectives') :- !.
characterize_family([piton], 'Gauge-invariant: ossified coordination from all perspectives') :- !.
characterize_family(Sig, Desc) :-
    (member(unknown, Sig) ; member(indexically_opaque, Sig)),
    !,
    format(atom(Desc), 'Contains unresolved types: ~w', [Sig]).
characterize_family([rope, snare], 'Two-type: coordination/extraction bifurcation along power axis') :- !.
characterize_family([rope, tangled_rope], 'Two-type: pure vs tangled coordination') :- !.
characterize_family([rope, snare, tangled_rope], 'Three-type: full rope-tangled-snare gauge structure with irreducible middle band') :- !.
characterize_family([scaffold, snare], 'Two-type: scaffold/snare transition') :- !.
characterize_family([scaffold, tangled_rope], 'Two-type: scaffold/tangled transition') :- !.
characterize_family(Sig, Desc) :-
    length(Sig, N),
    format(atom(Desc), '~w-type orbit: ~w', [N, Sig]).

%% report_cross_type_analysis(+Families)
report_cross_type_analysis(Families) :-
    format('## Cross-Type Analysis~n~n'),

    % Collect all types that appear across families
    findall(T, (member(family(Sig, _), Families), member(T, Sig)), AllTypes),
    sort(AllTypes, UniqueTypes),
    format('### Types Observed~n~n'),
    forall(
        member(T, UniqueTypes),
        (   findall(Sig, (member(family(Sig, _), Families), member(T, Sig)), Sigs),
            length(Sigs, NF),
            findall(S, (member(family(Sig2, Mems), Families), member(T, Sig2), length(Mems, S)), Sizes),
            sum_list(Sizes, TotalConstraints),
            format('- **~w**: appears in ~w families (~w constraints total)~n', [T, NF, TotalConstraints])
        )
    ),
    format('~n'),

    % Type co-occurrence matrix
    format('### Type Co-occurrence~n~n'),
    format('Which types appear together in multi-type orbits:~n~n'),
    findall(Sig, (member(family(Sig, _), Families), length(Sig, L), L > 1), MultiSigs),
    sort(MultiSigs, UniqueMulti),
    (   UniqueMulti \= []
    ->  forall(
            member(MS, UniqueMulti),
            (   findall(_, (member(family(MS, Mems2), Families), member(_, Mems2)), _),
                member(family(MS, Mems3), Families),
                length(Mems3, Pop),
                format('- ~w (~w constraints)~n', [MS, Pop])
            )
        )
    ;   format('No multi-type orbits found.~n')
    ),
    format('~n').

%% report_unknown_orbits(+Families)
report_unknown_orbits(Families) :-
    format('## Unknown-Containing Orbits~n~n'),
    findall(family(Sig, Members),
            (member(family(Sig, Members), Families),
             (member(unknown, Sig) ; member(indexically_opaque, Sig))),
            UnknownFamilies),
    length(UnknownFamilies, NUnk),
    (   NUnk > 0
    ->  format('~w orbit families contain unresolved types:~n~n', [NUnk]),
        forall(
            member(family(USig, UMembers), UnknownFamilies),
            (   length(UMembers, USize),
                format('### `~w` — ~w constraints~n~n', [USig, USize]),
                findall(Id, member(orbit(Id, _, _), UMembers), Ids),
                sort(Ids, SIds),
                forall(member(Id, SIds), format('- `~w`~n', [Id])),
                format('~n')
            )
        )
    ;   format('No orbits contain unknown or indexically_opaque types.~n~n')
    ).

%% report_singleton_vs_multi(+Families)
report_singleton_vs_multi(Families) :-
    format('## Gauge Invariance Summary~n~n'),
    findall(N, (member(family([_], Members), Families), length(Members, N)), SingleCounts),
    findall(N, (member(family(Sig, Members), Families), length(Sig, L), L > 1, length(Members, N)), MultiCounts),
    sum_list(SingleCounts, TotalSingle),
    sum_list(MultiCounts, TotalMulti),
    Total is TotalSingle + TotalMulti,
    (   Total > 0
    ->  PctSingle is TotalSingle * 100.0 / Total,
        PctMulti is TotalMulti * 100.0 / Total,
        format('- **Gauge-invariant** (single type across all contexts): ~w constraints (~1f%)~n',
               [TotalSingle, PctSingle]),
        format('- **Gauge-variant** (type changes with observer): ~w constraints (~1f%)~n~n',
               [TotalMulti, PctMulti])
    ;   format('No constraints analyzed.~n~n')
    ).
