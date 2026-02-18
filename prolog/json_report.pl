% ============================================================================
% JSON REPORT — Structured Pipeline Output
% ============================================================================
% Standalone script. Run from prolog/ directory:
%   swipl -l stack.pl -l json_report.pl -g "run_json_report, halt."
%
% Bulk-loads all testsets, queries the classification engine, and writes
% outputs/pipeline_output.json with three sections:
%   1. per_constraint — per-constraint classification data
%   2. diagnostic     — corpus-wide summary statistics
%   3. validation     — validation/integrity checks
%
% Design: format/2 manual JSON (matches orbit_report.pl pattern).
% ============================================================================

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(logical_fingerprint).
:- use_module(report_generator).
:- use_module(drl_lifecycle).
:- use_module(corpus_loader).
:- use_module(domain_priors).
:- use_module(data_repair).
:- use_module(covering_analysis).
:- use_module(maxent_classifier).

:- use_module(library(lists)).

/* ================================================================
   ENTRY POINT
   ================================================================ */

%% run_json_report
%  Main entry point. Loads corpus, discovers constraints, writes JSON.
run_json_report :-
    format(user_error, '[json] Starting JSON report generation...~n', []),
    corpus_loader:load_all_testsets,

    % Discover all constraints
    findall(C, logical_fingerprint:known_constraint(C), CRaw),
    sort(CRaw, Constraints),
    length(Constraints, CorpusSize),
    format(user_error, '[json] Found ~w constraints.~n', [CorpusSize]),

    % Precompute MaxEnt distributions for all constraints
    constraint_indexing:default_context(MaxEntCtx),
    maxent_classifier:maxent_precompute(Constraints, MaxEntCtx),
    format(user_error, '[json] MaxEnt precompute done.~n', []),

    % Write JSON
    setup_call_cleanup(
        open('../outputs/pipeline_output.json', write, S),
        write_pipeline_json(S, Constraints, CorpusSize, MaxEntCtx),
        close(S)
    ),
    format(user_error, '[json] Wrote pipeline_output.json (~w constraints)~n', [CorpusSize]).

/* ================================================================
   TIER 2 — SECTION BUILDERS
   ================================================================ */

%% write_pipeline_json(+Stream, +Constraints, +CorpusSize, +MaxEntCtx)
write_pipeline_json(S, Constraints, CorpusSize, MaxEntCtx) :-
    format(S, '{~n', []),

    % Section 1: per_constraint
    format(S, '  "per_constraint": [~n', []),
    write_per_constraint_array(S, Constraints, MaxEntCtx),
    format(S, '  ],~n', []),

    % Section 2: diagnostic
    format(S, '  "diagnostic": ', []),
    write_diagnostic_object(S, Constraints, CorpusSize),
    format(S, ',~n', []),

    % Section 3: validation
    format(S, '  "validation": ', []),
    write_validation_object(S, Constraints),
    format(S, ',~n', []),

    % Section 4: config
    format(S, '  "config": ', []),
    write_config_object(S),
    format(S, ',~n', []),

    % Section 5: type_hierarchy
    format(S, '  "type_hierarchy": ', []),
    write_type_hierarchy_object(S),
    format(S, '~n', []),

    format(S, '}~n', []).

/* ================================================================
   PER-CONSTRAINT ARRAY
   ================================================================ */

%% write_per_constraint_array(+Stream, +Constraints, +MaxEntCtx)
write_per_constraint_array(_, [], _).
write_per_constraint_array(S, [C], MaxEntCtx) :-
    !,
    write_per_constraint_entry(S, C, false, MaxEntCtx).
write_per_constraint_array(S, [C|Rest], MaxEntCtx) :-
    write_per_constraint_entry(S, C, true, MaxEntCtx),
    write_per_constraint_array(S, Rest, MaxEntCtx).

%% write_per_constraint_entry(+Stream, +Constraint, +TrailingComma, +MaxEntCtx)
write_per_constraint_entry(S, C, Comma, MaxEntCtx) :-
    format(S, '    {~n', []),

    % id
    format(S, '      "id": ', []),
    write_json_string(S, C),
    format(S, ',~n', []),

    % human_readable
    (   narrative_ontology:human_readable(C, HumanTitle)
    ->  true
    ;   HumanTitle = null
    ),
    format(S, '      "human_readable": ', []),
    write_json_string(S, HumanTitle),
    format(S, ',~n', []),

    % claimed_type
    (   narrative_ontology:constraint_claim(C, ClaimedType)
    ->  true
    ;   ClaimedType = null
    ),
    format(S, '      "claimed_type": ', []),
    write_json_string(S, ClaimedType),
    format(S, ',~n', []),

    % perspectives
    format(S, '      "perspectives": {~n', []),
    write_perspectives(S, C),
    format(S, '      },~n', []),

    % base_extractiveness
    (   catch(drl_core:base_extractiveness(C, BaseEps), _, fail)
    ->  true
    ;   BaseEps = null
    ),
    format(S, '      "base_extractiveness": ', []),
    write_json_number(S, BaseEps),
    format(S, ',~n', []),

    % suppression
    (   catch(drl_core:get_raw_suppression(C, Supp), _, fail)
    ->  true
    ;   Supp = null
    ),
    format(S, '      "suppression": ', []),
    write_json_number(S, Supp),
    format(S, ',~n', []),

    % resistance
    (   narrative_ontology:constraint_metric(C, resistance_to_change, Resist)
    ->  true
    ;   Resist = null
    ),
    format(S, '      "resistance": ', []),
    write_json_number(S, Resist),
    format(S, ',~n', []),

    % theater_ratio
    (   config:param(theater_metric_name, TheaterName),
        narrative_ontology:constraint_metric(C, TheaterName, TheaterVal)
    ->  true
    ;   TheaterVal = null
    ),
    format(S, '      "theater_ratio": ', []),
    write_json_number(S, TheaterVal),
    format(S, ',~n', []),

    % signature
    (   catch(drl_core:dr_signature(C, Sig), _, fail)
    ->  true
    ;   Sig = null
    ),
    format(S, '      "signature": ', []),
    write_json_string(S, Sig),
    format(S, ',~n', []),

    % purity_score + purity_band
    (   catch(structural_signatures:purity_score(C, PScore), _, fail),
        PScore \= -1.0
    ->  logical_fingerprint:purity_zone(PScore, PBand)
    ;   PScore = null, PBand = null
    ),
    format(S, '      "purity_score": ', []),
    write_json_number(S, PScore),
    format(S, ',~n', []),
    format(S, '      "purity_band": ', []),
    write_json_string(S, PBand),
    format(S, ',~n', []),

    % coupling
    format(S, '      "coupling": ', []),
    write_coupling_object(S, C),
    format(S, ',~n', []),

    % omegas
    collect_omegas(C, Omegas),
    format(S, '      "omegas": ', []),
    write_omega_array(S, Omegas),
    format(S, ',~n', []),

    % gaps
    findall(gap(GT, TP, TI),
            report_generator:detect_gap_pattern(C, gap(GT, TP, TI)),
            Gaps),
    format(S, '      "gaps": ', []),
    write_gap_array(S, Gaps),
    format(S, ',~n', []),

    % beneficiaries
    findall(B, narrative_ontology:constraint_beneficiary(C, B), Bens),
    sort(Bens, UBens),
    format(S, '      "beneficiaries": ', []),
    write_json_string_array(S, UBens),
    format(S, ',~n', []),

    % victims
    findall(V, narrative_ontology:constraint_victim(C, V), Vics),
    sort(Vics, UVics),
    format(S, '      "victims": ', []),
    write_json_string_array(S, UVics),
    format(S, ',~n', []),

    % emerges_naturally
    (   catch(drl_core:emerges_naturally(C), _, fail)
    ->  EmNat = true
    ;   EmNat = false
    ),
    format(S, '      "emerges_naturally": ~w,~n', [EmNat]),

    % requires_active_enforcement
    (   catch(drl_core:requires_active_enforcement(C), _, fail)
    ->  ReqEnf = true
    ;   ReqEnf = false
    ),
    format(S, '      "requires_active_enforcement": ~w,~n', [ReqEnf]),

    % classifications
    findall(classification(CType, CP, CT, CE, CSc),
            constraint_indexing:constraint_classification(C, CType,
                context(agent_power(CP), time_horizon(CT),
                        exit_options(CE), spatial_scope(CSc))),
            Classifications),
    format(S, '      "classifications": ', []),
    write_classification_array(S, Classifications),
    format(S, ',~n', []),

    % domain (classification type from category_of/2 — NOT the topic domain)
    (   catch(domain_priors:category_of(C, Domain), _, fail)
    ->  true
    ;   Domain = null
    ),
    format(S, '      "domain": ', []),
    write_json_string(S, Domain),
    format(S, ',~n', []),

    % topic_domain (subject area from testset metadata)
    (   narrative_ontology:topic_domain(C, TopicDomain)
    ->  true
    ;   TopicDomain = null
    ),
    format(S, '      "topic_domain": ', []),
    write_json_string(S, TopicDomain),
    format(S, ',~n', []),

    % maxent_probs (6-type probability distribution from shadow classifier)
    format(S, '      "maxent_probs": ', []),
    write_maxent_probs(S, C, MaxEntCtx),
    format(S, ',~n', []),

    % raw_maxent_probs (pre-override distribution for override impact analysis)
    format(S, '      "raw_maxent_probs": ', []),
    write_raw_maxent_probs(S, C, MaxEntCtx),
    format(S, ',~n', []),

    % maxent_entropy (normalized Shannon entropy)
    write_maxent_entropy_field(S, C, MaxEntCtx),

    % maxent_top_type (shadow classifier's top pick)
    write_maxent_top_type_field(S, C, MaxEntCtx),

    % resolution_strategy — deferred
    format(S, '      "resolution_strategy": null~n', []),

    % Close object
    (Comma == true -> format(S, '    },~n', []) ; format(S, '    }~n', [])).

/* ================================================================
   MAXENT FIELDS
   ================================================================ */

%% write_maxent_probs(+Stream, +Constraint, +Context)
write_maxent_probs(S, C, Ctx) :-
    (   maxent_classifier:maxent_distribution(C, Ctx, Dist)
    ->  format(S, '{', []),
        write_maxent_dist_entries(S, Dist),
        format(S, '}', [])
    ;   format(S, 'null', [])
    ).

write_maxent_dist_entries(_, []).
write_maxent_dist_entries(S, [Type-Prob]) :- !,
    format(S, '"~w": ~6f', [Type, Prob]).
write_maxent_dist_entries(S, [Type-Prob|Rest]) :-
    format(S, '"~w": ~6f, ', [Type, Prob]),
    write_maxent_dist_entries(S, Rest).

%% write_raw_maxent_probs(+Stream, +Constraint, +Context)
%  Pre-override distribution for override impact analysis.
write_raw_maxent_probs(S, C, Ctx) :-
    (   maxent_classifier:maxent_distribution_raw(C, Ctx, Dist)
    ->  format(S, '{', []),
        write_maxent_dist_entries(S, Dist),
        format(S, '}', [])
    ;   format(S, 'null', [])
    ).

%% write_maxent_entropy_field(+Stream, +Constraint, +Context)
write_maxent_entropy_field(S, C, Ctx) :-
    (   maxent_classifier:maxent_entropy(C, Ctx, HNorm)
    ->  format(S, '      "maxent_entropy": ~6f,~n', [HNorm])
    ;   format(S, '      "maxent_entropy": null,~n', [])
    ).

%% write_maxent_top_type_field(+Stream, +Constraint, +Context)
write_maxent_top_type_field(S, C, Ctx) :-
    (   maxent_classifier:maxent_top_type(C, Ctx, TopType)
    ->  format(S, '      "maxent_top_type": ', []),
        write_json_string(S, TopType),
        format(S, ',~n', [])
    ;   format(S, '      "maxent_top_type": null,~n', [])
    ).

/* ================================================================
   PERSPECTIVES
   ================================================================ */

%% write_perspectives(+Stream, +Constraint)
write_perspectives(S, C) :-
    write_one_perspective(S, C, powerless, true),
    write_one_perspective(S, C, moderate, true),
    write_one_perspective(S, C, institutional, true),
    write_one_perspective(S, C, analytical, false).

write_one_perspective(S, C, Power, Comma) :-
    logical_fingerprint:standard_context_for_power(Power, Ctx),
    (   catch(drl_core:dr_type(C, Ctx, Type), _, fail)
    ->  true
    ;   Type = null
    ),
    format(S, '        "~w": ', [Power]),
    write_json_string(S, Type),
    (Comma == true -> format(S, ',~n', []) ; format(S, '~n', [])).

/* ================================================================
   COUPLING OBJECT
   ================================================================ */

%% write_coupling_object(+Stream, +Constraint)
write_coupling_object(S, C) :-
    (   catch(logical_fingerprint:fingerprint_coupling(C,
              coupling(Cat, Score, _Pairs, BoltzResult, _Purity)), _, fail)
    ->  % Normalize boltzmann to a simple string
        boltzmann_label(BoltzResult, BoltzLabel),
        format(S, '{~n', []),
        format(S, '        "category": ', []),
        write_json_string(S, Cat),
        format(S, ',~n', []),
        format(S, '        "score": ', []),
        write_json_number(S, Score),
        format(S, ',~n', []),
        format(S, '        "boltzmann": ', []),
        write_json_string(S, BoltzLabel),
        format(S, '~n', []),
        format(S, '      }', [])
    ;   format(S, 'null', [])
    ).

%% boltzmann_label(+Result, -Label)
%  Normalize boltzmann compliance term to a simple atom.
boltzmann_label(compliant, compliant) :- !.
boltzmann_label(non_compliant, non_compliant) :- !.
boltzmann_label(non_compliant(_), non_compliant) :- !.
boltzmann_label(inconclusive(_), inconclusive) :- !.
boltzmann_label(inconclusive, inconclusive) :- !.
boltzmann_label(_, unknown).

/* ================================================================
   OMEGAS
   ================================================================ */

%% collect_omegas(+Constraint, -Omegas)
%  Collects omegas from gaps and from testset-declared omega_variable/3.
%  Returns list of omega(ID, Type, Question, Severity).
collect_omegas(C, Omegas) :-
    % Gap-derived omegas
    findall(omega(OID, OType, Question, Sev),
            (   report_generator:detect_gap_pattern(C, Gap),
                report_generator:omega_from_gap(C, Gap, OID, OType, Question),
                report_generator:omega_severity(OID, Sev)
            ),
            GapOmegas),
    % Testset-declared omegas (omega_variable/3 facts)
    findall(omega(OID, OType, Desc, Sev),
            (   narrative_ontology:omega_variable(OID, OType, Desc),
                omega_for_constraint(OID, C),
                report_generator:omega_severity(OID, Sev)
            ),
            DeclaredOmegas),
    append(GapOmegas, DeclaredOmegas, AllOmegas),
    sort(1, @<, AllOmegas, Omegas).  % Deduplicate by ID

%% omega_for_constraint(+OmegaID, +Constraint)
%  True if OmegaID is associated with Constraint (by naming convention).
omega_for_constraint(OID, C) :-
    atom(OID), atom(C),
    sub_atom(OID, _, _, _, C).

/* ================================================================
   DIAGNOSTIC OBJECT
   ================================================================ */

%% write_diagnostic_object(+Stream, +Constraints, +CorpusSize)
write_diagnostic_object(S, Constraints, CorpusSize) :-
    format(S, '{~n', []),

    % corpus_size
    format(S, '    "corpus_size": ~w,~n', [CorpusSize]),

    % type_distribution
    tally_claimed_types(Constraints, TypeDist),
    format(S, '    "type_distribution": ', []),
    write_json_count_object(S, TypeDist),
    format(S, ',~n', []),

    % purity_summary
    tally_purity_bands(Constraints, PurityDist),
    format(S, '    "purity_summary": ', []),
    write_json_count_object(S, PurityDist),
    format(S, ',~n', []),

    % coupling_summary
    tally_coupling_categories(Constraints, CouplingDist),
    format(S, '    "coupling_summary": ', []),
    write_json_count_object(S, CouplingDist),
    format(S, ',~n', []),

    % boltzmann_summary
    tally_boltzmann(Constraints, BoltzDist),
    format(S, '    "boltzmann_summary": ', []),
    write_json_count_object(S, BoltzDist),
    format(S, ',~n', []),

    % drift_event_counts
    tally_drift_severities(Constraints, DriftDist),
    format(S, '    "drift_event_counts": ', []),
    write_json_count_object(S, DriftDist),
    format(S, ',~n', []),

    % network_stability
    logical_fingerprint:standard_context_for_power(analytical, StabCtx),
    (   catch(drl_lifecycle:network_stability_assessment(StabCtx, StabAssessment), _, fail)
    ->  true
    ;   StabAssessment = null
    ),
    format(S, '    "network_stability": ', []),
    write_json_string(S, StabAssessment),
    format(S, '~n', []),

    format(S, '  }', []).

/* ================================================================
   VALIDATION OBJECT
   ================================================================ */

%% write_validation_object(+Stream, +Constraints)
write_validation_object(S, Constraints) :-
    format(S, '{~n', []),

    % constraints_with_gaps
    findall(C, (member(C, Constraints),
                report_generator:detect_gap_pattern(C, _)),
            GapCs),
    sort(GapCs, UGapCs),
    length(UGapCs, GapCount),
    format(S, '    "constraints_with_gaps": ~w,~n', [GapCount]),

    % omega_count
    findall(OID,
            (   member(C, Constraints),
                report_generator:detect_gap_pattern(C, Gap),
                report_generator:omega_from_gap(C, Gap, OID, _, _)),
            OmegaIDs),
    sort(OmegaIDs, UOmegaIDs),
    length(UOmegaIDs, OmegaCount),
    format(S, '    "omega_count": ~w,~n', [OmegaCount]),

    % omega_by_severity
    tally_omega_severities(UOmegaIDs, OmegaSevDist),
    format(S, '    "omega_by_severity": ', []),
    write_json_count_object(S, OmegaSevDist),
    format(S, ',~n', []),

    % false_mountain_count
    findall(C, (member(C, Constraints),
                catch(structural_signatures:false_natural_law(C, _), _, fail)),
            FalseMs),
    length(FalseMs, FalseMountainCount),
    format(S, '    "false_mountain_count": ~w,~n', [FalseMountainCount]),

    % signature_distribution
    tally_signatures(Constraints, SigDist),
    format(S, '    "signature_distribution": ', []),
    write_json_count_object(S, SigDist),
    format(S, '~n', []),

    format(S, '  }', []).

/* ================================================================
   CONFIG OBJECT
   ================================================================ */

%% write_config_object(+Stream)
%  Emits all config:param/2 facts as a flat JSON object.
write_config_object(S) :-
    findall(Name-Value, config:param(Name, Value), Pairs),
    format(S, '{~n', []),
    write_config_pairs(S, Pairs),
    format(S, '~n  }', []).

write_config_pairs(_, []).
write_config_pairs(S, [Name-Value]) :-
    !,
    format(S, '    "~w": ', [Name]),
    write_json_param_value(S, Value).
write_config_pairs(S, [Name-Value|Rest]) :-
    format(S, '    "~w": ', [Name]),
    write_json_param_value(S, Value),
    format(S, ',~n', []),
    write_config_pairs(S, Rest).

%% write_json_param_value(+Stream, +Value)
%  Numbers as JSON numbers, atoms as JSON strings.
write_json_param_value(S, V) :-
    number(V), !,
    write_json_number(S, V).
write_json_param_value(S, V) :-
    write_json_string(S, V).

/* ================================================================
   TYPE HIERARCHY OBJECT
   ================================================================ */

%% write_type_hierarchy_object(+Stream)
%  Emits type metadata for all 6 constraint types.
write_type_hierarchy_object(S) :-
    Types = [mountain, rope, scaffold, piton, tangled_rope, snare],
    format(S, '{~n', []),
    write_type_hierarchy_entries(S, Types),
    format(S, '~n  }', []).

write_type_hierarchy_entries(_, []).
write_type_hierarchy_entries(S, [Type]) :-
    !,
    write_single_type_entry(S, Type).
write_type_hierarchy_entries(S, [Type|Rest]) :-
    write_single_type_entry(S, Type),
    format(S, ',~n', []),
    write_type_hierarchy_entries(S, Rest).

write_single_type_entry(S, Type) :-
    report_generator:type_severity(Type, Sev),
    report_generator:type_description(Type, Desc),
    report_generator:type_strategy(Type, Strat),
    report_generator:type_color(Type, Color),
    format(S, '    "~w": {', [Type]),
    format(S, '"severity": ~w, ', [Sev]),
    format(S, '"description": ', []),
    write_json_string(S, Desc),
    format(S, ', "strategy": ', []),
    write_json_string(S, Strat),
    format(S, ', "color": ', []),
    write_json_string(S, Color),
    format(S, '}', []).

/* ================================================================
   TALLY HELPERS
   ================================================================ */

%% tally_claimed_types(+Constraints, -Pairs)
%  Returns sorted list of Type-Count pairs.
tally_claimed_types(Constraints, Pairs) :-
    findall(T, (member(C, Constraints),
                narrative_ontology:constraint_claim(C, T)),
            Types),
    msort(Types, Sorted),
    run_length_encode(Sorted, Pairs).

%% tally_purity_bands(+Constraints, -Pairs)
tally_purity_bands(Constraints, Pairs) :-
    findall(Zone,
            (   member(C, Constraints),
                catch(structural_signatures:purity_score(C, PS), _, fail),
                PS \= -1.0,
                logical_fingerprint:purity_zone(PS, Zone)),
            Zones),
    msort(Zones, Sorted),
    run_length_encode(Sorted, Pairs).

%% tally_coupling_categories(+Constraints, -Pairs)
tally_coupling_categories(Constraints, Pairs) :-
    findall(Cat,
            (   member(C, Constraints),
                catch(logical_fingerprint:fingerprint_coupling(C,
                      coupling(Cat, _, _, _, _)), _, fail)),
            Cats),
    msort(Cats, Sorted),
    run_length_encode(Sorted, Pairs).

%% tally_boltzmann(+Constraints, -Pairs)
tally_boltzmann(Constraints, Pairs) :-
    findall(Label,
            (   member(C, Constraints),
                catch(logical_fingerprint:fingerprint_coupling(C,
                      coupling(_, _, _, BResult, _)), _, fail),
                boltzmann_label(BResult, Label)),
            Labels),
    msort(Labels, Sorted),
    run_length_encode(Sorted, Pairs).

%% tally_drift_severities(+Constraints, -Pairs)
tally_drift_severities(Constraints, Pairs) :-
    findall(Sev,
            (   member(C, Constraints),
                drl_lifecycle:drift_event(C, EvType, _),
                drl_lifecycle:drift_severity(C, EvType, Sev)),
            Sevs),
    msort(Sevs, Sorted),
    run_length_encode(Sorted, Pairs).

%% tally_omega_severities(+OmegaIDs, -Pairs)
tally_omega_severities(OmegaIDs, Pairs) :-
    findall(Sev,
            (   member(OID, OmegaIDs),
                report_generator:omega_severity(OID, Sev)),
            Sevs),
    msort(Sevs, Sorted),
    run_length_encode(Sorted, Pairs).

%% tally_signatures(+Constraints, -Pairs)
tally_signatures(Constraints, Pairs) :-
    findall(Sig,
            (   member(C, Constraints),
                catch(drl_core:dr_signature(C, Sig), _, fail)),
            Sigs),
    msort(Sigs, Sorted),
    run_length_encode(Sorted, Pairs).

%% run_length_encode(+SortedList, -Pairs)
%  Converts a sorted list into Key-Count pairs.
run_length_encode([], []).
run_length_encode([H|T], [H-Count|Rest]) :-
    count_run(H, T, Count, Remainder),
    run_length_encode(Remainder, Rest).

count_run(_, [], 1, []).
count_run(H, [H|T], N, Rest) :-
    !, count_run(H, T, N1, Rest), N is N1 + 1.
count_run(_, List, 1, List).

/* ================================================================
   TIER 3 — JSON PRIMITIVES
   ================================================================ */

%% json_escape_string(+Value, -Escaped)
%  Escapes special characters in atom/string values for JSON strings.
json_escape_string(Value, Escaped) :-
    (atom(Value) -> atom_chars(Value, Chars) ; string_chars(Value, Chars)),
    escape_chars(Chars, EscChars),
    atom_chars(Escaped, EscChars).

escape_chars([], []).
escape_chars(['\\' | T], ['\\', '\\' | Rest]) :- !, escape_chars(T, Rest).
escape_chars(['"'  | T], ['\\', '"'  | Rest]) :- !, escape_chars(T, Rest).
escape_chars(['\n' | T], ['\\', 'n'  | Rest]) :- !, escape_chars(T, Rest).
escape_chars(['\t' | T], ['\\', 't'  | Rest]) :- !, escape_chars(T, Rest).
escape_chars([C    | T], [C          | Rest]) :- escape_chars(T, Rest).

%% write_json_string(+Stream, +Value)
%  Writes "value" or null. Handles atoms, strings, and compound terms.
write_json_string(S, null) :- !, format(S, 'null', []).
write_json_string(S, Value) :-
    (atom(Value) ; string(Value)),
    !,
    json_escape_string(Value, Escaped),
    format(S, '"~w"', [Escaped]).
write_json_string(S, Value) :-
    term_to_atom(Value, Atom),
    json_escape_string(Atom, Escaped),
    format(S, '"~w"', [Escaped]).

%% write_json_number(+Stream, +Value)
%  Writes a number or null.
write_json_number(S, null)    :- !, format(S, 'null', []).
write_json_number(S, unknown) :- !, format(S, 'null', []).
write_json_number(S, V) :-
    integer(V), !, format(S, '~w', [V]).
write_json_number(S, V) :-
    float(V), !, format(S, '~6f', [V]).
write_json_number(S, V) :-
    number(V), !, format(S, '~w', [V]).
write_json_number(S, _) :-
    format(S, 'null', []).

%% write_json_string_array(+Stream, +List)
%  Writes ["a", "b"] or [].
write_json_string_array(S, []) :- !, format(S, '[]', []).
write_json_string_array(S, Items) :-
    format(S, '[', []),
    write_string_items(S, Items),
    format(S, ']', []).

write_string_items(_, []).
write_string_items(S, [X]) :-
    !, write_json_string(S, X).
write_string_items(S, [X|Xs]) :-
    write_json_string(S, X),
    format(S, ', ', []),
    write_string_items(S, Xs).

%% write_json_count_object(+Stream, +Pairs)
%  Writes {"key": count, ...} from Key-Count pairs.
write_json_count_object(S, []) :- !, format(S, '{}', []).
write_json_count_object(S, Pairs) :-
    format(S, '{', []),
    write_count_entries(S, Pairs),
    format(S, '}', []).

write_count_entries(_, []).
write_count_entries(S, [K-V]) :-
    !, format(S, '"~w": ~w', [K, V]).
write_count_entries(S, [K-V|Rest]) :-
    format(S, '"~w": ~w, ', [K, V]),
    write_count_entries(S, Rest).

%% write_omega_array(+Stream, +Omegas)
%  Writes [{id, type, question, severity}, ...].
write_omega_array(S, []) :- !, format(S, '[]', []).
write_omega_array(S, Omegas) :-
    format(S, '[~n', []),
    write_omega_items(S, Omegas),
    format(S, '~n      ]', []).

write_omega_items(_, []).
write_omega_items(S, [omega(ID, Type, Question, Sev)]) :-
    !,
    format(S, '        {', []),
    format(S, '"id": ', []),
    write_json_string(S, ID),
    format(S, ', "type": ', []),
    write_json_string(S, Type),
    format(S, ', "question": ', []),
    write_json_string(S, Question),
    format(S, ', "severity": ', []),
    write_json_string(S, Sev),
    format(S, '}', []).
write_omega_items(S, [omega(ID, Type, Question, Sev)|Rest]) :-
    format(S, '        {', []),
    format(S, '"id": ', []),
    write_json_string(S, ID),
    format(S, ', "type": ', []),
    write_json_string(S, Type),
    format(S, ', "question": ', []),
    write_json_string(S, Question),
    format(S, ', "severity": ', []),
    write_json_string(S, Sev),
    format(S, '},~n', []),
    write_omega_items(S, Rest).

%% write_gap_array(+Stream, +Gaps)
%  Writes [{gap_type, powerless_type, institutional_type}, ...].
write_gap_array(S, []) :- !, format(S, '[]', []).
write_gap_array(S, Gaps) :-
    format(S, '[~n', []),
    write_gap_items(S, Gaps),
    format(S, '~n      ]', []).

write_gap_items(_, []).
write_gap_items(S, [gap(GT, TP, TI)]) :-
    !,
    format(S, '        {"gap_type": ', []),
    write_json_string(S, GT),
    format(S, ', "powerless_type": ', []),
    write_json_string(S, TP),
    format(S, ', "institutional_type": ', []),
    write_json_string(S, TI),
    format(S, '}', []).
write_gap_items(S, [gap(GT, TP, TI)|Rest]) :-
    format(S, '        {"gap_type": ', []),
    write_json_string(S, GT),
    format(S, ', "powerless_type": ', []),
    write_json_string(S, TP),
    format(S, ', "institutional_type": ', []),
    write_json_string(S, TI),
    format(S, '},~n', []),
    write_gap_items(S, Rest).

/* ================================================================
   CLASSIFICATION ARRAY
   ================================================================ */

%% write_classification_array(+Stream, +Classifications)
%  Writes [{"type": ..., "context": {...}}, ...].
write_classification_array(S, []) :- !, format(S, '[]', []).
write_classification_array(S, Cls) :-
    format(S, '[~n', []),
    write_classification_items(S, Cls),
    format(S, '~n      ]', []).

write_classification_items(_, []).
write_classification_items(S, [classification(Type, P, T, E, Sc)]) :-
    !,
    write_single_classification(S, Type, P, T, E, Sc).
write_classification_items(S, [classification(Type, P, T, E, Sc)|Rest]) :-
    write_single_classification(S, Type, P, T, E, Sc),
    format(S, ',~n', []),
    write_classification_items(S, Rest).

write_single_classification(S, Type, P, T, E, Sc) :-
    format(S, '        {"type": ', []),
    write_json_string(S, Type),
    format(S, ', "context": {"agent_power": ', []),
    write_json_string(S, P),
    format(S, ', "time_horizon": ', []),
    write_json_string(S, T),
    format(S, ', "exit_options": ', []),
    write_json_string(S, E),
    format(S, ', "spatial_scope": ', []),
    write_json_string(S, Sc),
    format(S, '}}', []).
