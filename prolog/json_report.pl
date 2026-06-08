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
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(signature_detection, [false_natural_law/2]).
:- use_module(cs_pattern_detection, [cs_pattern/3, cs_verdict/2, cs_has_fields/1,
                                     cs_grounding_mismatch/3]).
:- use_module(temporal_residual, [residual_report/2]).  % Type-A observer residual (OQ-83; category-B)
:- use_module(logical_fingerprint).
:- use_module(report_generator).
:- use_module(drl_lifecycle).
:- use_module(corpus_loader).
:- use_module(domain_priors).
:- use_module(data_repair).
:- use_module(covering_analysis).
:- use_module(maxent_classifier).
:- use_module(grothendieck_cohomology).
:- use_module(measurement_layer).
:- use_module(sheaf_analysis, []).  % sheaf_status/2, called module-qualified
:- use_module(diagnostic_summary).
:- use_module(post_synthesis).
:- use_module(drl_fpn, [fpn_ep/3, fpn_intrinsic/2]).
:- use_module(drl_purity_network, [constraint_neighbors/3]).

:- use_module(library(lists)).
:- use_module(library(http/json)).

:- dynamic abd_triggers/2.  % abd_triggers(ConstraintID, TriggerList)

/* ================================================================
   ENTRY POINT
   ================================================================ */

%% run_json_report
%  Main entry point. Loads corpus, discovers constraints, writes JSON.
run_json_report :-
    format(user_error, '[json] Starting JSON report generation...~n', []),
    corpus_loader:load_all_testsets,

    % Enumerate the corpus from the authoritative membership registry
    % (corpus_loader:corpus_constraint/1, asserted per loaded testset file).
    % Previously this used logical_fingerprint:known_constraint/1, which
    % unions constraint_metric/claim/classification facts and therefore
    % picked up engine-resident DEMO constraints (catholic_church_1200 via
    % its constraint_classification clauses in constraint_instances.pl) —
    % the source of the per_constraint=1107 vs manifest n_constraints=1106
    % denominator drift (OQ-70 Probe 0).
    findall(C, corpus_loader:corpus_constraint(C), CRaw),
    sort(CRaw, Constraints),
    length(Constraints, CorpusSize),
    format(user_error, '[json] Found ~w constraints.~n', [CorpusSize]),

    % Precompute MaxEnt at all 4 Wasserstein contexts (includes analytical = default)
    constraint_indexing:default_context(MaxEntCtx),
    measurement_layer:wasserstein_contexts(WCtxs),
    catch(maxent_classifier:maxent_multi_run(WCtxs, _WMSummaries), _, true),
    format(user_error, '[json] MaxEnt multi-context (Wasserstein) done.~n', []),

    % Run indexed MaxEnt (uses power-scaled χ; profiles from multi-run's last context)
    catch(maxent_classifier:maxent_indexed_run(MaxEntCtx, _IndexedSummary), _, true),
    format(user_error, '[json] MaxEnt indexed run done.~n', []),

    % Run FPN if enabled (provides fpn_ep/3 state for diagnostic_summary probes)
    (   config:param(fpn_enabled, 1)
    ->  catch(drl_fpn:fpn_run(MaxEntCtx, _FPNSummary), _, true),
        format(user_error, '[json] FPN iteration done.~n', [])
    ;   true
    ),

    % Precompute cohomology for all constraints
    catch(grothendieck_cohomology:corpus_cohomology(_), _, true),
    format(user_error, '[json] Cohomology precompute done.~n', []),

    % Load abductive data from abductive_data.json (produced by abductive_report)
    load_abductive_data,

    % Write JSON — to the RAW filename. run_pipeline.py is the single writer
    % of the canonical pipeline_output.json (it reads the raw file, prepends
    % the provenance manifest, and writes the canonical artifact). A direct
    % re-run of this export therefore cannot clobber a manifest-bearing
    % pipeline_output.json (swipl_load_path_and_probe_gotchas.md §5).
    setup_call_cleanup(
        open('../outputs/pipeline_output.raw.json', write, S),
        write_pipeline_json(S, Constraints, CorpusSize, MaxEntCtx),
        close(S)
    ),
    format(user_error, '[json] Wrote pipeline_output.raw.json (~w constraints); manifest injection + canonical rename happen in run_pipeline.py~n', [CorpusSize]).

/* ================================================================
   ABDUCTIVE DATA LOADER
   ================================================================
   Reads outputs/abductive_data.json and asserts abd_triggers/2 facts.
   Called before JSON generation so diagnostic_summary can query triggers.
   ================================================================ */

load_abductive_data :-
    AbdFile = '../outputs/abductive_data.json',
    (   catch(load_abductive_file(AbdFile, N), E,
              (format(user_error, '[json] Abductive data load failed: ~w~n', [E]),
               N = 0))
    ->  format(user_error, '[json] Loaded abductive triggers for ~w constraints.~n', [N])
    ;   format(user_error, '[json] No abductive data loaded.~n', [])
    ).

load_abductive_file(File, N) :-
    setup_call_cleanup(
        open(File, read, S),
        json_read_dict(S, Dict),
        close(S)
    ),
    PerConstraint = Dict.get(per_constraint),
    dict_pairs(PerConstraint, _, Pairs),
    retractall(abd_triggers(_, _)),
    assert_abd_pairs(Pairs, 0, N).

assert_abd_pairs([], N, N).
assert_abd_pairs([CAtom-TriggerList|Rest], Acc, N) :-
    atom_string(CID, CAtom),
    maplist(json_trigger_to_term, TriggerList, Triggers),
    assert(abd_triggers(CID, Triggers)),
    Acc1 is Acc + 1,
    assert_abd_pairs(Rest, Acc1, N).

json_trigger_to_term(Dict, trigger(Class, Conf, Anomaly, Cat)) :-
    atom_string(Class, Dict.get(trigger_class)),
    Conf = Dict.get(confidence),
    atom_string(Anomaly, Dict.get(anomaly_type)),
    atom_string(Cat, Dict.get(category)).

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

    % perspective_chi (per-perspective Chi decomposition)
    format(S, '      "perspective_chi": {~n', []),
    write_perspective_chi(S, C),
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
    (   catch(purity_scoring:purity_score(C, PScore), _, fail),
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

    % contamination_network (FPN topology)
    format(S, '      "contamination_network": ', []),
    write_contamination_network(S, C, MaxEntCtx),
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

    % maxent_indexed (indexed-mode distribution using power-scaled χ)
    format(S, '      "maxent_indexed": ', []),
    write_maxent_indexed(S, C, MaxEntCtx),
    format(S, ',~n', []),

    % maxent_divergence (classical vs indexed TV distance)
    format(S, '      "maxent_divergence": ', []),
    write_maxent_divergence(S, C, MaxEntCtx),
    format(S, ',~n', []),

    % h1_band (cohomological obstruction — perspectival fracture measure)
    (   catch(grothendieck_cohomology:cohomological_obstruction(C, _H0, H1), _, fail)
    ->  format(S, '      "h1_band": ~w,~n', [H1])
    ;   format(S, '      "h1_band": null,~n', [])
    ),

    % sheaf_status (discrete gluing regime: genuine_sheaf / fragile_presheaf / manifest_presheaf)
    (   catch(sheaf_analysis:sheaf_status(C, SheafStatus), _, fail)
    ->  format(S, '      "sheaf_status": "~w",~n', [SheafStatus])
    ;   format(S, '      "sheaf_status": null,~n', [])
    ),

    % wasserstein transport (continuous complement to H1)
    (   catch(measurement_layer:wasserstein_transport_profile(C, WProfile), _, fail)
    ->  WProfile = transport_profile(edge(u1_u2, W12), edge(u2_u3, W23), edge(u3_u4, W34)),
        WTotal is W12 + W23 + W34,
        format(S, '      "wasserstein_profile": {"u1_u2": ~6f, "u2_u3": ~6f, "u3_u4": ~6f},~n', [W12, W23, W34]),
        format(S, '      "wasserstein_total_fracture": ~6f,~n', [WTotal]),
        measurement_layer:wasserstein_contexts([WCtx1, WCtx2, WCtx3, WCtx4]),
        (catch(measurement_layer:wasserstein_incomparable_mass(C, WCtx1, WM1), _, (WM1 = 0.0)) -> true ; WM1 = 0.0),
        (catch(measurement_layer:wasserstein_incomparable_mass(C, WCtx2, WM2), _, (WM2 = 0.0)) -> true ; WM2 = 0.0),
        (catch(measurement_layer:wasserstein_incomparable_mass(C, WCtx3, WM3), _, (WM3 = 0.0)) -> true ; WM3 = 0.0),
        (catch(measurement_layer:wasserstein_incomparable_mass(C, WCtx4, WM4), _, (WM4 = 0.0)) -> true ; WM4 = 0.0),
        format(S, '      "wasserstein_incomparable_mass": {"u1": ~6f, "u2": ~6f, "u3": ~6f, "u4": ~6f},~n', [WM1, WM2, WM3, WM4])
    ;   format(S, '      "wasserstein_profile": null,~n', []),
        format(S, '      "wasserstein_total_fracture": null,~n', []),
        format(S, '      "wasserstein_incomparable_mass": null,~n', [])
    ),

    % contextuality_fraction (per-constraint: H1/6, Abramsky-Brandenburger)
    (   catch(grothendieck_cohomology:constraint_contextuality(C, CxFrac), _, fail)
    ->  format(S, '      "contextuality_fraction": ~6f,~n', [CxFrac])
    ;   format(S, '      "contextuality_fraction": null,~n', [])
    ),

    % orbit_monotonicity (power-chain extraction monotonicity)
    (   catch(grothendieck_cohomology:orbit_monotonicity(C, MonoStatus), _, fail)
    ->  mono_status_to_string(MonoStatus, MonoStr),
        format(S, '      "orbit_monotonicity": "~w",~n', [MonoStr])
    ;   format(S, '      "orbit_monotonicity": null,~n', [])
    ),

    % arakelov_height (boundary complexity diagnostic)
    (   catch(arakelov_height:arakelov_height_pair(C, ArakH, ArakCtx), _, fail)
    ->  ArakCtx = context(agent_power(ArakPower), _, _, _),
        (   catch(arakelov_height:signature_pressure(C, ArakCtx, ArakSP), _, (ArakSP = 0.0))
        ->  true
        ;   ArakSP = 0.0
        ),
        format(S, '      "arakelov_height": ~6f,~n', [ArakH]),
        format(S, '      "arakelov_height_context": "~w",~n', [ArakPower]),
        format(S, '      "signature_pressure": ~6f,~n', [ArakSP])
    ;   format(S, '      "arakelov_height": null,~n', []),
        format(S, '      "arakelov_height_context": null,~n', []),
        format(S, '      "signature_pressure": null,~n', [])
    ),

    % transition_boundaries (where type-switching occurs in orbit)
    (   catch(grothendieck_cohomology:transition_boundaries(C, TBounds), _, fail)
    ->  format(S, '      "transition_boundaries": [', []),
        write_boundary_array(S, TBounds),
        format(S, '],~n', [])
    ;   format(S, '      "transition_boundaries": [],~n', [])
    ),

    % drift_events (per-constraint structural drift indicators)
    findall(drift(DType, DSeverity), (
        catch(drl_lifecycle:scan_constraint_drift(C, DriftEvents), _, (DriftEvents = [])),
        member(drift(DType, _Evidence, DSeverity), DriftEvents)
    ), Drifts),
    format(S, '      "drift_events": [', []),
    write_drift_array(S, Drifts),
    format(S, '],~n', []),

    % drift_trajectory: emit for all measurement-bearing constraints; absent if no data
    (   narrative_ontology:measurement(_, C, _, _, _)
    ->  format(S, '      "drift_trajectory": ', []),
        write_drift_trajectory(S, C),
        format(S, ',~n', [])
    ;   true
    ),

    % temporal_residual (Type-A observer residual: per-context flip-events; OQ-83).
    % Gated on measurement presence (same as drift_trajectory; absent = no temporal
    % data, distinct from times_examined>0/flips=0). OBSERVER-ONLY — committer drift
    % is emitted separately (cs_drift_* below); reconciliation is an offline join.
    (   narrative_ontology:measurement(_, C, _, _, _)
    ->  format(S, '      "temporal_residual": ', []),
        write_temporal_residual(S, C),
        format(S, ',~n', [])
    ;   true
    ),

    % --- Compute diagnostic summary once (hoisted for T12 access) ---
    (   catch(diagnostic_summary:diagnostic_summary(C, Summary), _, fail)
    ->  true
    ;   Summary = none
    ),

    % diagnostic_verdict
    format(S, '      "diagnostic_verdict": ', []),
    write_diagnostic_verdict_from_summary(S, Summary),
    format(S, ',~n', []),

    % post_synthesis_flags (T12)
    (   Summary \= none, config:param(post_synthesis_enabled, 1)
    ->  post_synthesis:post_synthesis_check(C, Summary, PSFlags)
    ;   PSFlags = []
    ),
    format(S, '      "post_synthesis_flags": ', []),
    write_post_synthesis_flags(S, PSFlags),
    format(S, ',~n', []),

    % resolution_strategy — deferred
    format(S, '      "resolution_strategy": null,~n', []),

    % cs_pattern fields
    (   catch(cs_pattern_detection:cs_has_fields(C), _, fail)
    ->  catch(cs_pattern_detection:cs_pattern(C, CsPat, CsSignals), _,
              (CsPat = null, CsSignals = [])),
        format(S, '      "cs_pattern": ', []),
        write_json_string(S, CsPat),
        format(S, ',~n', []),
        format(S, '      "cs_pattern_signals": ', []),
        write_json_string_array(S, CsSignals),
        format(S, ',~n', []),
        findall(V, catch(cs_pattern_detection:cs_verdict(C, V), _, fail), Verdicts),
        format(S, '      "cs_verdicts": ', []),
        write_json_string_array(S, Verdicts),
        format(S, ',~n', [])
    ;   format(S, '      "cs_pattern": null,~n', []),
        format(S, '      "cs_pattern_signals": [],~n', []),
        format(S, '      "cs_verdicts": [],~n', [])
    ),
    % cs UID-keyed fields: look up all UIDs for this reading, pick latest instance
    findall(U, catch(narrative_ontology:cs_story_uid(C, U), _, fail), UIDs),
    (   UIDs = []
    ->  format(S, '      "cs_instance_count": 0,~n', []),
        format(S, '      "cs_drift_terminal": null,~n', []),
        format(S, '      "cs_axiom_foreclosed": null,~n', []),
        format(S, '      "cs_drift_unacknowledged": false~n', [])
    ;   length(UIDs, NInst),
        format(S, '      "cs_instance_count": ~w,~n', [NInst]),
        % Pick latest instance by created_at, UID-tiebroken; fallback to @< UID ordering
        (   NInst > 1
        ->  (   catch(aggregate_all(max(T-U),
                          (member(U, UIDs),
                           narrative_ontology:cs_created_at(U, T)),
                          max(_-UID)),
                _, fail)
            ->  true
            ;   msort(UIDs, Sorted), last(Sorted, UID)  % @< fallback: no timestamps
            )
        ;   UIDs = [UID]
        ),
        % cs_drift_terminal (UID-keyed)
        (   catch(cs_drift_engine:cs_drift_trajectory(UID, _, Terminal), _, fail)
        ->  format(S, '      "cs_drift_terminal": "~w",~n', [Terminal])
        ;   format(S, '      "cs_drift_terminal": null,~n', [])
        ),
        % cs_axiom_foreclosed (UID-keyed; first matching atom)
        (   catch(cs_axiom_engine:cs_axiom_foreclosed(UID, AxAtom), _, fail)
        ->  format(S, '      "cs_axiom_foreclosed": "~w",~n', [AxAtom])
        ;   format(S, '      "cs_axiom_foreclosed": null,~n', [])
        ),
        % cs_drift_unacknowledged (UID-keyed)
        (   catch(cs_pattern_detection:cs_drift_unacknowledged(UID, _), _, fail)
        ->  format(S, '      "cs_drift_unacknowledged": true~n', [])
        ;   format(S, '      "cs_drift_unacknowledged": false~n', [])
        )
    ),

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
   INDEXED MAXENT + DIVERGENCE
   ================================================================ */

%% write_maxent_indexed(+Stream, +Constraint, +Context)
%  Writes the indexed-mode MaxEnt distribution object (power-scaled χ).
write_maxent_indexed(S, C, Ctx) :-
    (   maxent_classifier:maxent_indexed_distribution(C, Ctx, Dist)
    ->  % Compute normalized entropy inline (shannon_entropy not exported)
        foldl(idx_entropy_acc, Dist, 0.0, RawH),
        HMax is log(6),
        (HMax > 0 -> HNorm is RawH / HMax ; HNorm = 0.0),
        % Find top type by probability
        msort_by_prob_desc(Dist, [TopType-TopProb|_]),
        format(S, '{~n', []),
        format(S, '        "context": "analytical",~n', []),
        format(S, '        "distribution": {', []),
        write_maxent_dist_entries(S, Dist),
        format(S, '},~n', []),
        format(S, '        "entropy": ', []),
        write_json_number(S, HNorm),
        format(S, ',~n', []),
        format(S, '        "top_type": ', []),
        write_json_string(S, TopType),
        format(S, ',~n', []),
        format(S, '        "top_prob": ', []),
        write_json_number(S, TopProb),
        format(S, '~n', []),
        format(S, '      }', [])
    ;   format(S, 'null', [])
    ).

idx_entropy_acc(_Type-P, Acc, NewAcc) :-
    (   P > 1.0e-15
    ->  NewAcc is Acc - P * log(P)
    ;   NewAcc = Acc
    ).

msort_by_prob_desc(Dist, Sorted) :-
    maplist(flip_pair, Dist, Flipped),
    msort(Flipped, SortedAsc),
    reverse(SortedAsc, SortedDesc),
    maplist(flip_pair, SortedDesc, Sorted).

flip_pair(A-B, B-A).

%% write_maxent_divergence(+Stream, +Constraint, +Context)
%  Writes the total variation distance between classical and indexed MaxEnt.
write_maxent_divergence(S, C, Ctx) :-
    (   maxent_classifier:maxent_classical_vs_indexed(C, Ctx, Classical, Indexed)
    ->  % Total variation distance: TV = 0.5 × Σ|P(x) - Q(x)|
        findall(AbsDiff, (
            member(T-PC, Classical),
            member(T-PI, Indexed),
            AbsDiff is abs(PC - PI)
        ), Diffs),
        sum_list(Diffs, SumDiffs),
        TV is SumDiffs / 2.0,
        % Interpretation thresholds
        (   TV < 0.01 -> Interp = near_zero
        ;   TV =< 0.10 -> Interp = moderate
        ;   Interp = large
        ),
        format(S, '{~n', []),
        format(S, '        "total_variation": ', []),
        write_json_number(S, TV),
        format(S, ',~n', []),
        format(S, '        "interpretation": ', []),
        write_json_string(S, Interp),
        format(S, '~n', []),
        format(S, '      }', [])
    ;   format(S, 'null', [])
    ).

/* ================================================================
   DRIFT EVENTS ARRAY
   ================================================================ */

%% write_drift_array(+Stream, +Drifts)
%  Writes drift event objects: [{"type": ..., "severity": ...}, ...].
write_drift_array(_, []).
write_drift_array(S, [drift(DType, DSev)]) :-
    !,
    format(S, '{"type": "~w", "severity": "~w"}', [DType, DSev]).
write_drift_array(S, [drift(DType, DSev)|Rest]) :-
    format(S, '{"type": "~w", "severity": "~w"}, ', [DType, DSev]),
    write_drift_array(S, Rest).

%% write_drift_trajectory(+Stream, +Constraint)
%  Emits full ordered measurement series per metric, per-interval rates, and
%  per-interval accelerations. Source: direct findall on measurement/5 only —
%  no collapsed predicates (metric_delta, drift_velocity, etc.) used.
write_drift_trajectory(S, C) :-
    findall(Metric, narrative_ontology:measurement(_, C, Metric, _, _), AllM0),
    sort(AllM0, Metrics),
    format(S, '{~n', []),
    write_metric_series_list(S, C, Metrics),
    format(S, '      }', []).

write_metric_series_list(_, _, []).
write_metric_series_list(S, C, [M]) :- !,
    write_one_metric_series(S, C, M, false).
write_metric_series_list(S, C, [M|Rest]) :-
    write_one_metric_series(S, C, M, true),
    write_metric_series_list(S, C, Rest).

write_one_metric_series(S, C, Metric, Comma) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    sort(Pairs, Sorted),
    compute_rates(Sorted, Rates),
    compute_accelerations(Rates, Accels),
    format(S, '        "~w": {"series": [', [Metric]),
    write_timepoint_array(S, Sorted),
    format(S, '], "per_interval_rate": [', []),
    write_rate_list(S, Rates),
    format(S, '], "per_interval_acceleration": [', []),
    write_accel_list(S, Accels),
    format(S, ']}', []),
    (Comma == true -> format(S, ',~n', []) ; format(S, '~n', [])).

%% compute_rates(+Sorted, -Rates)
%  Sorted = [T-V, ...] ordered by T. Zero-span intervals (D=0) are skipped.
compute_rates([], []).
compute_rates([_], []).
compute_rates([T1-V1, T2-V2 | Rest], Rates) :-
    D is T2 - T1,
    (   D > 0
    ->  R is (V2 - V1) / D,
        Rates = [rate(T1, T2, R) | RestRates]
    ;   Rates = RestRates   % duplicate timepoint: skip interval
    ),
    compute_rates([T2-V2 | Rest], RestRates).

%% compute_accelerations(+Rates, -Accels)
%  N rates -> N-1 accelerations. Span = Tc - Ta across both intervals.
compute_accelerations([], []).
compute_accelerations([_], []).
compute_accelerations([rate(Ta, _, R1), rate(Tb, Tc, R2) | Rest], [acc(Tb, Acc) | Accs]) :-
    Span is Tc - Ta,
    (Span > 0 -> Acc is (R2 - R1) / Span ; Acc is 0.0),
    compute_accelerations([rate(Tb, Tc, R2) | Rest], Accs).

write_timepoint_array(_, []).
write_timepoint_array(S, [T-V]) :- !,
    format(S, '{"t": ~w, "v": ~4f}', [T, V]).
write_timepoint_array(S, [T-V|Rest]) :-
    format(S, '{"t": ~w, "v": ~4f}, ', [T, V]),
    write_timepoint_array(S, Rest).

write_rate_list(_, []).
write_rate_list(S, [rate(T1, T2, R)]) :- !,
    format(S, '{"t1": ~w, "t2": ~w, "rate": ~6f}', [T1, T2, R]).
write_rate_list(S, [rate(T1, T2, R) | Rest]) :-
    format(S, '{"t1": ~w, "t2": ~w, "rate": ~6f}, ', [T1, T2, R]),
    write_rate_list(S, Rest).

write_accel_list(_, []).
write_accel_list(S, [acc(T, A)]) :- !,
    format(S, '{"t": ~w, "acc": ~6f}', [T, A]).
write_accel_list(S, [acc(T, A) | Rest]) :-
    format(S, '{"t": ~w, "acc": ~6f}, ', [T, A]),
    write_accel_list(S, Rest).

%% write_temporal_residual(+Stream, +Constraint)
%  Type-A observer residual: per-context ran-witness (times_examined,
%  backed_times) + flip composition (real flips vs fabrication_adjacent_transitions).
%  Reads temporal_residual only — observer-axis, no cs_ read here.
write_temporal_residual(S, C) :-
    temporal_residual:residual_report(C, Report),
    format(S, '{~n', []),
    write_ctx_residual_list(S, Report),
    format(S, '      }', []).

write_ctx_residual_list(_, []).
write_ctx_residual_list(S, [ctx(Label, Res)]) :- !,
    write_one_ctx_residual(S, Label, Res, false).
write_ctx_residual_list(S, [ctx(Label, Res) | Rest]) :-
    write_one_ctx_residual(S, Label, Res, true),
    write_ctx_residual_list(S, Rest).

write_one_ctx_residual(S, Label, ctx_residual(NT, NB, Flips, FabAdj), Comma) :-
    length(Flips, NFlips),
    format(S, '        "~w": {"times_examined": ~w, "backed_times": ~w, "flips": ~w, "fabrication_adjacent_transitions": ~w, "flip_events": [',
           [Label, NT, NB, NFlips, FabAdj]),
    write_flip_list(S, Flips),
    format(S, ']}', []),
    (Comma == true -> format(S, ',~n', []) ; format(S, '~n', [])).

write_flip_list(_, []).
write_flip_list(S, [F]) :- !, write_one_flip(S, F).
write_flip_list(S, [F | Rest]) :-
    write_one_flip(S, F), format(S, ', ', []),
    write_flip_list(S, Rest).

write_one_flip(S, flip(T1, T2, Ty1, Ty2, DEps, DSupp, DTheater)) :-
    format(S, '{"t1": ~w, "t2": ~w, "from": "~w", "to": "~w", "d_eps": ', [T1, T2, Ty1, Ty2]),
    write_num_or_null(S, DEps),
    format(S, ', "d_supp": ', []), write_num_or_null(S, DSupp),
    format(S, ', "d_theater": ', []), write_num_or_null(S, DTheater),
    format(S, '}', []).

write_num_or_null(S, V) :- number(V), !, format(S, '~4f', [V]).
write_num_or_null(S, _) :- format(S, 'null', []).

%% mono_status_to_string(+Status, -String)
%  Converts orbit_monotonicity/2 term to a JSON-safe string.
mono_status_to_string(constant(_), constant).
mono_status_to_string(monotone_ascending, monotone_ascending).
mono_status_to_string(monotone_descending, monotone_descending).
mono_status_to_string(non_monotone(_), non_monotone).
mono_status_to_string(incomparable, incomparable).

%% write_boundary_array(+Stream, +Boundaries)
%  Writes transition boundary objects: [{"position": N, "from": T1, "to": T2}, ...].
write_boundary_array(_, []).
write_boundary_array(S, [boundary(Pos, From, To)]) :-
    !,
    format(S, '{"position": ~w, "from": "~w", "to": "~w"}', [Pos, From, To]).
write_boundary_array(S, [boundary(Pos, From, To)|Rest]) :-
    format(S, '{"position": ~w, "from": "~w", "to": "~w"}, ', [Pos, From, To]),
    write_boundary_array(S, Rest).

%% write_type_cf_object(+Stream, +TypeCFs)
%  Writes contextuality-by-type object: {"mountain": 0.03, "rope": 0.38, ...}.
write_type_cf_object(S, TypeCFs) :-
    format(S, '{', []),
    write_type_cf_entries(S, TypeCFs),
    format(S, '}', []).

write_type_cf_entries(_, []).
write_type_cf_entries(S, [Type-CF]) :- !,
    format(S, '"~w": ~6f', [Type, CF]).
write_type_cf_entries(S, [Type-CF|Rest]) :-
    format(S, '"~w": ~6f, ', [Type, CF]),
    write_type_cf_entries(S, Rest).

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
   PERSPECTIVE CHI (per-perspective extractiveness decomposition)
   ================================================================
   Exports Chi = ε × f(d) × σ(S) and its components for each
   perspective. Chi is computed by constraint_indexing:extractiveness_for_agent/3
   during classification but not previously exported to JSON.
   ================================================================ */

%% write_perspective_chi(+Stream, +Constraint)
write_perspective_chi(S, C) :-
    write_one_perspective_chi(S, C, powerless, true),
    write_one_perspective_chi(S, C, moderate, true),
    write_one_perspective_chi(S, C, institutional, true),
    write_one_perspective_chi(S, C, analytical, false).

%% write_one_perspective_chi(+Stream, +Constraint, +Power, +Comma)
%  Exports {chi, epsilon, d, f_d, scope_mod} for a single perspective.
write_one_perspective_chi(S, C, Power, Comma) :-
    logical_fingerprint:standard_context_for_power(Power, Ctx),
    Ctx = context(_, _, _, spatial_scope(Scope)),
    (   catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi), _, fail)
    ->  (catch(drl_core:base_extractiveness(C, Epsilon), _, fail) -> true ; Epsilon = null),
        (catch(constraint_indexing:derive_directionality(C, Ctx, D), _, fail) -> true ; D = null),
        (D \= null -> constraint_indexing:sigmoid_f(D, Fd) ; Fd = null),
        (D \= null -> (catch(constraint_indexing:sigmoid_d1(D, F1d), _, fail) -> true ; F1d = null) ; F1d = null),
        (D \= null -> (catch(constraint_indexing:sigmoid_d2(D, F2d), _, fail) -> true ; F2d = null) ; F2d = null),
        (catch(constraint_indexing:scope_modifier(Scope, ScopeMod), _, fail) -> true ; ScopeMod = null)
    ;   Chi = null, Epsilon = null, D = null, Fd = null, F1d = null, F2d = null, ScopeMod = null
    ),
    format(S, '        "~w": {', [Power]),
    format(S, '"chi": ', []), write_json_number(S, Chi),
    format(S, ', "epsilon": ', []), write_json_number(S, Epsilon),
    format(S, ', "d": ', []), write_json_number(S, D),
    format(S, ', "f_d": ', []), write_json_number(S, Fd),
    format(S, ', "f1_d": ', []), write_json_number(S, F1d),
    format(S, ', "f2_d": ', []), write_json_number(S, F2d),
    format(S, ', "scope_mod": ', []), write_json_number(S, ScopeMod),
    format(S, '}', []),
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
        % Liveness decomposition: which observer index moves the verdict.
        coupling_liveness_fields(C, SV, PV, LiveIndex),
        format(S, '{~n', []),
        format(S, '        "category": ', []),
        write_json_string(S, Cat),
        format(S, ',~n', []),
        format(S, '        "score": ', []),
        write_json_number(S, Score),
        format(S, ',~n', []),
        format(S, '        "boltzmann": ', []),
        write_json_string(S, BoltzLabel),
        format(S, ',~n', []),
        format(S, '        "scope_violations": ', []),
        write_json_number(S, SV),
        format(S, ',~n', []),
        format(S, '        "power_violations": ', []),
        write_json_number(S, PV),
        format(S, ',~n', []),
        format(S, '        "live_index": ', []),
        write_json_string(S, LiveIndex),
        format(S, '~n', []),
        format(S, '      }', [])
    ;   format(S, 'null', [])
    ).

%% coupling_liveness_fields(+C, -ScopeViolations, -PowerViolations, -LiveIndex)
%  Reads boltzmann_compliance:coupling_liveness/3 and derives the live-index
%  label. On failure (no epistemic access / grid not buildable) emits nulls
%  with live_index = inconclusive, so absence is reported AS absence rather
%  than silently defaulting to a "seat-free" (0,0) reading.
coupling_liveness_fields(C, SV, PV, LiveIndex) :-
    (   catch(boltzmann_compliance:coupling_liveness(C, SVc, PVc), _, fail)
    ->  SV = SVc, PV = PVc, live_index_label(SVc, PVc, LiveIndex)
    ;   SV = null, PV = null, LiveIndex = inconclusive
    ).

%% live_index_label(+ScopeViolations, +PowerViolations, -Label)
%  none  — index-invariant on the grid (seat-free w.r.t. observer index;
%          consistent with a Mountain). scope/power/both — which index is live.
live_index_label(0, 0, none)  :- !.
live_index_label(SV, 0, scope) :- SV > 0, !.
live_index_label(0, PV, power) :- PV > 0, !.
live_index_label(_, _, both).

%% boltzmann_label(+Result, -Label)
%  Normalize boltzmann compliance term to a simple atom.
boltzmann_label(compliant, compliant) :- !.
boltzmann_label(compliant(_), compliant) :- !.
boltzmann_label(non_compliant, non_compliant) :- !.
boltzmann_label(non_compliant(_), non_compliant) :- !.
boltzmann_label(non_compliant(_, _), non_compliant) :- !.
boltzmann_label(inconclusive(_), inconclusive) :- !.
boltzmann_label(inconclusive, inconclusive) :- !.
boltzmann_label(_, unknown).

/* ================================================================
   CONTAMINATION NETWORK (FPN TOPOLOGY)
   ================================================================ */

%% write_contamination_network(+Stream, +Constraint, +Context)
%  Writes the contamination_network JSON object: intrinsic/effective purity,
%  propagation delta, and neighbor list with edge metadata.
write_contamination_network(S, C, Context) :-
    % Intrinsic purity: prefer FPN cache, fall back to purity_score/2
    (   catch(fpn_intrinsic(C, IP0), _, fail), IP0 \= -1.0
    ->  IP = IP0
    ;   (   catch(purity_scoring:purity_score(C, IP1), _, fail), IP1 \= -1.0
        ->  IP = IP1
        ;   IP = null
        )
    ),
    % Effective purity: from FPN iteration state
    (   catch(fpn_ep(C, Context, EP0), _, fail), EP0 \= -1.0
    ->  EP = EP0
    ;   EP = IP   % no FPN or no data → effective = intrinsic
    ),
    % Delta = effective - intrinsic (negative = contaminated)
    (   IP \= null, EP \= null
    ->  Delta is EP - IP
    ;   Delta = null
    ),
    % Neighbors
    (   catch(constraint_neighbors(C, Context, Neighbors), _, fail)
    ->  true
    ;   Neighbors = []
    ),
    % Write JSON object
    format(S, '{~n', []),
    format(S, '        "intrinsic_purity": ', []),
    write_json_number(S, IP),
    format(S, ',~n', []),
    format(S, '        "effective_purity": ', []),
    write_json_number(S, EP),
    format(S, ',~n', []),
    format(S, '        "propagation_delta": ', []),
    write_json_number(S, Delta),
    format(S, ',~n', []),
    format(S, '        "neighbors": ', []),
    write_neighbor_array(S, Neighbors, Context),
    format(S, '~n', []),
    format(S, '      }', []).

%% write_neighbor_array(+Stream, +Neighbors, +Context)
%  Writes JSON array of neighbor objects.
write_neighbor_array(S, [], _) :- !, format(S, '[]', []).
write_neighbor_array(S, Neighbors, Context) :-
    format(S, '[~n', []),
    write_neighbor_items(S, Neighbors, Context),
    format(S, '~n        ]', []).

%% write_neighbor_items(+Stream, +Neighbors, +Context)
%  Writes neighbor objects. Last item has no trailing comma.
write_neighbor_items(_, [], _).
write_neighbor_items(S, [neighbor(Other, Str, Src)], Ctx) :-
    !, write_one_neighbor(S, Other, Str, Src, Ctx).
write_neighbor_items(S, [neighbor(Other, Str, Src)|Rest], Ctx) :-
    write_one_neighbor(S, Other, Str, Src, Ctx),
    format(S, ',~n', []),
    write_neighbor_items(S, Rest, Ctx).

%% write_one_neighbor(+Stream, +Other, +Strength, +Source, +Context)
%  Writes a single neighbor JSON object with purity and type.
write_one_neighbor(S, Other, Strength, Source, Context) :-
    % Neighbor's purity (effective if FPN ran, else intrinsic)
    (   catch(fpn_ep(Other, Context, NP0), _, fail), NP0 \= -1.0
    ->  NP = NP0
    ;   (   catch(purity_scoring:purity_score(Other, NP1), _, fail), NP1 \= -1.0
        ->  NP = NP1
        ;   NP = null
        )
    ),
    % Neighbor's classification type
    (   catch(drl_core:dr_type(Other, Context, NType), _, fail)
    ->  true
    ;   NType = null
    ),
    format(S, '          {"constraint_id": ', []),
    write_json_string(S, Other),
    format(S, ', "edge_type": ', []),
    write_json_string(S, Source),
    format(S, ', "edge_strength": ', []),
    write_json_number(S, Strength),
    format(S, ', "neighbor_purity": ', []),
    write_json_number(S, NP),
    format(S, ', "neighbor_type": ', []),
    write_json_string(S, NType),
    format(S, '}', []).

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
                once(report_generator:omega_severity(OID, Sev))
            ),
            GapOmegas),
    % Testset-declared omegas (omega_variable/3 facts)
    findall(omega(OID, OType, Desc, Sev),
            (   narrative_ontology:omega_variable(OID, OType, Desc),
                omega_for_constraint(OID, C),
                once(report_generator:omega_severity(OID, Sev))
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
   DIAGNOSTIC VERDICT (per-constraint)
   ================================================================ */

%% write_diagnostic_verdict(+Stream, +Constraint)
%  Compatibility wrapper — computes summary then serializes.
%  Used by diagnostic_selftest and other callers outside json_report.
write_diagnostic_verdict(S, C) :-
    (   catch(diagnostic_summary:diagnostic_summary(C, Summary), _, fail)
    ->  true
    ;   Summary = none
    ),
    write_diagnostic_verdict_from_summary(S, Summary).

%% write_diagnostic_verdict_from_summary(+Stream, +Summary)
%  Serialize a pre-computed diagnostic_summary term as a JSON object.
write_diagnostic_verdict_from_summary(S, none) :-
    !, format(S, 'null', []).
write_diagnostic_verdict_from_summary(S, Summary) :-
    Summary = diagnostic_summary(
        Verdict, Agreements, ExpConflicts, Rejections,
        Tensions, NAvail, UnavailList
    ),
    format(S, '{~n', []),

    % verdict
    format(S, '        "verdict": ', []),
    write_json_string(S, Verdict),
    format(S, ',~n', []),

    % agreements
    format(S, '        "agreements": ', []),
    write_json_string_array(S, Agreements),
    format(S, ',~n', []),

    % expected_conflicts
    format(S, '        "expected_conflicts": ', []),
    write_expected_conflicts_array(S, ExpConflicts),
    format(S, ',~n', []),

    % convergent_rejections
    format(S, '        "convergent_rejections": ', []),
    write_convergent_rejections_array(S, Rejections),
    format(S, ',~n', []),

    % tensions
    format(S, '        "tensions": ', []),
    write_tensions_array(S, Tensions),
    format(S, ',~n', []),

    % subsystems_available
    format(S, '        "subsystems_available": ~w,~n', [NAvail]),

    % subsystems_unavailable
    format(S, '        "subsystems_unavailable": ', []),
    write_json_string_array(S, UnavailList),
    format(S, '~n', []),

    format(S, '      }', []).

%% write_post_synthesis_flags(+Stream, +Flags)
%  Serialize T12 post-synthesis flags as a JSON array.
%  Each flag is flag(FlagType, DetailPairs) where DetailPairs is Key-Value list.
write_post_synthesis_flags(S, []) :- !, format(S, '[]', []).
write_post_synthesis_flags(S, Flags) :-
    format(S, '[~n', []),
    write_ps_flag_items(S, Flags),
    format(S, '~n      ]', []).

write_ps_flag_items(_, []).
write_ps_flag_items(S, [flag(FlagType, Details)]) :-
    !,
    format(S, '        {"flag_type": ', []),
    write_json_string(S, FlagType),
    format(S, ', "details": {', []),
    write_ps_detail_pairs(S, Details),
    format(S, '}}', []).
write_ps_flag_items(S, [flag(FlagType, Details)|Rest]) :-
    format(S, '        {"flag_type": ', []),
    write_json_string(S, FlagType),
    format(S, ', "details": {', []),
    write_ps_detail_pairs(S, Details),
    format(S, '}},~n', []),
    write_ps_flag_items(S, Rest).

write_ps_detail_pairs(_, []).
write_ps_detail_pairs(S, [K-V]) :-
    !,
    format(S, '"~w": ', [K]),
    write_ps_detail_value(S, V).
write_ps_detail_pairs(S, [K-V|Rest]) :-
    format(S, '"~w": ', [K]),
    write_ps_detail_value(S, V),
    format(S, ', ', []),
    write_ps_detail_pairs(S, Rest).

%% write_ps_detail_value(+Stream, +Value)
%  Serialize a detail value: numbers, atoms as strings, lists as JSON arrays.
write_ps_detail_value(S, V) :- number(V), !, format(S, '~w', [V]).
write_ps_detail_value(S, V) :- is_list(V), !,
    write_json_string_array(S, V).
write_ps_detail_value(S, V) :- atom(V), !,
    write_json_string(S, V).

%% write_expected_conflicts_array(+Stream, +ExpConflicts)
write_expected_conflicts_array(S, []) :- !, format(S, '[]', []).
write_expected_conflicts_array(S, Items) :-
    format(S, '[', []),
    write_ec_items(S, Items),
    format(S, ']', []).

write_ec_items(_, []).
write_ec_items(S, [expected_conflict(Sub, Pattern, Explanation)]) :-
    !,
    format(S, '{"subsystem": ', []),
    write_json_string(S, Sub),
    format(S, ', "pattern": ', []),
    write_json_string(S, Pattern),
    format(S, ', "explanation": ', []),
    write_json_string(S, Explanation),
    format(S, '}', []).
write_ec_items(S, [expected_conflict(Sub, Pattern, Explanation)|Rest]) :-
    format(S, '{"subsystem": ', []),
    write_json_string(S, Sub),
    format(S, ', "pattern": ', []),
    write_json_string(S, Pattern),
    format(S, ', "explanation": ', []),
    write_json_string(S, Explanation),
    format(S, '}, ', []),
    write_ec_items(S, Rest).

%% write_convergent_rejections_array(+Stream, +Rejections)
write_convergent_rejections_array(S, []) :- !, format(S, '[]', []).
write_convergent_rejections_array(S, Items) :-
    format(S, '[', []),
    write_cr_items(S, Items),
    format(S, ']', []).

write_cr_items(_, []).
write_cr_items(S, [convergent_rejection(AltType, _Count, Subs)]) :-
    !,
    length(Subs, NSubs),
    format(S, '{"subsystems": ', []),
    write_json_string_array(S, Subs),
    format(S, ', "alternative_type": ', []),
    write_json_string(S, AltType),
    format(S, ', "evidence": "~w subsystems suggest ~w"', [NSubs, AltType]),
    format(S, '}', []).
write_cr_items(S, [convergent_rejection(AltType, _Count, Subs)|Rest]) :-
    length(Subs, NSubs),
    format(S, '{"subsystems": ', []),
    write_json_string_array(S, Subs),
    format(S, ', "alternative_type": ', []),
    write_json_string(S, AltType),
    format(S, ', "evidence": "~w subsystems suggest ~w"', [NSubs, AltType]),
    format(S, '}, ', []),
    write_cr_items(S, Rest).

%% write_tensions_array(+Stream, +Tensions)
write_tensions_array(S, []) :- !, format(S, '[]', []).
write_tensions_array(S, Items) :-
    format(S, '[', []),
    write_tension_items(S, Items),
    format(S, ']', []).

write_tension_items(_, []).
write_tension_items(S, [tension(Sub, Detail)]) :-
    !,
    term_to_atom(Detail, DetailAtom),
    format(S, '{"subsystem": ', []),
    write_json_string(S, Sub),
    format(S, ', "signal": ', []),
    write_json_string(S, DetailAtom),
    format(S, ', "detail": ', []),
    write_json_string(S, DetailAtom),
    format(S, '}', []).
write_tension_items(S, [tension(Sub, Detail)|Rest]) :-
    term_to_atom(Detail, DetailAtom),
    format(S, '{"subsystem": ', []),
    write_json_string(S, Sub),
    format(S, ', "signal": ', []),
    write_json_string(S, DetailAtom),
    format(S, ', "detail": ', []),
    write_json_string(S, DetailAtom),
    format(S, '}, ', []),
    write_tension_items(S, Rest).

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
    format(S, ',~n', []),

    % corpus_wasserstein_fracture (total W1 across all constraints)
    (   catch(measurement_layer:wasserstein_corpus_fracture(CorpusW1), _, fail)
    ->  format(S, '    "corpus_wasserstein_fracture": ~6f,~n', [CorpusW1])
    ;   format(S, '    "corpus_wasserstein_fracture": null,~n', [])
    ),

    % arakelov_threshold (corpus p75 of non-trivial heights, memoized this run;
    % governs the genuine_sheaf/fragile_presheaf split in per-constraint
    % sheaf_status — recorded so the split is reproducible from this file
    % without recomputing the percentile)
    (   catch(arakelov_height:arakelov_threshold(ArakThresh), _, fail)
    ->  format(S, '    "arakelov_threshold": ~6f,~n', [ArakThresh])
    ;   format(S, '    "arakelov_threshold": null,~n', [])
    ),

    % contextuality (Abramsky-Brandenburger: corpus fraction + by-type breakdown)
    (   catch(grothendieck_cohomology:contextuality_fraction(CorpusCF), _, fail)
    ->  format(S, '    "contextuality": {"corpus_fraction": ~6f, "by_type": ', [CorpusCF]),
        (   catch(grothendieck_cohomology:contextuality_by_type(TypeCFs), _, fail)
        ->  write_type_cf_object(S, TypeCFs)
        ;   format(S, '{}', [])
        ),
        format(S, '},~n', [])
    ;   format(S, '    "contextuality": null,~n', [])
    ),

    % monotonicity (power-chain monotonicity distribution + boundary positions)
    (   catch(grothendieck_cohomology:corpus_monotonicity(CorpusMonoSum), _, fail),
        CorpusMonoSum = monotonicity_summary(
            constant(MNC), monotone_ascending(MNA),
            monotone_descending(MND), non_monotone(MNN), incomparable(MNI),
            boundary_distribution([pos(1, MB1), pos(2, MB2), pos(3, MB3)]))
    ->  format(S, '    "monotonicity": {"constant": ~w, "ascending": ~w, "descending": ~w, "non_monotone": ~w, "incomparable": ~w, "boundary_distribution": {"pos_1": ~w, "pos_2": ~w, "pos_3": ~w}}~n',
               [MNC, MNA, MND, MNN, MNI, MB1, MB2, MB3])
    ;   format(S, '    "monotonicity": null~n', [])
    ),

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
                catch(signature_detection:false_natural_law(C, _), _, fail)),
            FalseMs),
    length(FalseMs, FalseMountainCount),
    format(S, '    "false_mountain_count": ~w,~n', [FalseMountainCount]),

    % signature_distribution
    tally_signatures(Constraints, SigDist),
    format(S, '    "signature_distribution": ', []),
    write_json_count_object(S, SigDist),
    format(S, ',~n', []),

    % cs_pattern_distribution
    tally_cs_patterns(Constraints, CsPatDist, CsFieldsAbsent, CsTotal, CsVerdictDist),
    format(S, '    "cs_pattern_distribution": {~n', []),
    format(S, '      "fields_absent": ~w,~n', [CsFieldsAbsent]),
    format(S, '      "total_with_cs_fields": ~w,~n', [CsTotal]),
    format(S, '      "pattern_counts": ', []),
    write_json_count_object(S, CsPatDist),
    format(S, ',~n', []),
    format(S, '      "cs_verdicts_fired": ', []),
    write_json_count_object(S, CsVerdictDist),
    format(S, '~n    },~n', []),

    % cs_grounding_mismatch count — how many (C, AG, Sig) triples fire.
    % A non-zero count means at least one constraint's asserted authority grounding
    % contradicts its computed structural signature — the primary corpus sanity signal.
    findall(C-AG-Sig, catch(cs_grounding_mismatch(C, AG, Sig), _, fail),
            GmTriples),
    length(GmTriples, GmCount),
    format(S, '    "cs_grounding_mismatch_count": ~w,~n', [GmCount]),

    % CS trifurcation: drift terminal distribution (registered UIDs only)
    findall(T, (narrative_ontology:cs_story_uid(_, U),
                catch(cs_drift_engine:cs_drift_trajectory(U, _, T), _, fail)), AllTerminals),
    msort(AllTerminals, SortedTerminals),
    run_length_encode(SortedTerminals, TerminalPairs),
    format(S, '    "cs_drift_terminal_distribution": ', []),
    write_json_count_object(S, TerminalPairs),
    format(S, ',~n', []),

    % CS trifurcation: distinct UIDs with unacknowledged drift
    % Filter to registered UIDs only (avoids picking up old C-keyed facts during Phase A interregnum)
    findall(U, (narrative_ontology:cs_story_uid(_, U),
                catch(cs_pattern_detection:cs_drift_unacknowledged(U, _), _, fail)), UnackUs),
    sort(UnackUs, UniqueUnackUs),
    length(UniqueUnackUs, UnackCount),
    format(S, '    "cs_drift_unacknowledged_count": ~w,~n', [UnackCount]),

    % CS trifurcation: distinct UIDs with axiom foreclosed
    % Filter to registered UIDs only (avoids picking up old C-keyed facts during Phase A interregnum)
    findall(U, (narrative_ontology:cs_story_uid(_, U),
                catch(cs_axiom_engine:cs_axiom_foreclosed(U, _), _, fail)), FcUs),
    sort(FcUs, UniqueFcUs),
    length(UniqueFcUs, FcCount),
    format(S, '    "cs_axiom_foreclosed_count": ~w,~n', [FcCount]),

    % B3: Kernel-level divergence and axiom conflict statistics
    % K must be bound before calling cs_kernel_divergence to avoid
    % cs_readings_for_kernel collecting all 317 constraints as "readings".
    findall(K, narrative_ontology:cs_kernel_id(_, K), Ks0),
    sort(Ks0, AllKernels),

    findall(K-C1-C2,
        (   member(K, AllKernels),
            catch(cs_kernel_registry:cs_kernel_divergence(K, _, C1, C2), _, fail)
        ),
        DivTuples),
    sort(DivTuples, UniqueDivTuples),
    length(UniqueDivTuples, DivPairCount),
    format(S, '    "cs_kernel_divergence_count": ~w,~n', [DivPairCount]),

    findall(K,
        (   member(K, AllKernels),
            \+ \+ catch(cs_kernel_registry:cs_kernel_divergence(K, _, _, _), _, fail)
        ),
        DivKernels),
    length(DivKernels, DivKernelCount),
    format(S, '    "cs_kernels_with_divergence": ~w,~n', [DivKernelCount]),

    findall(K-C1-C2,
        (   member(K, AllKernels),
            catch(cs_axiom_engine:cs_kernel_axiom_conflict(K, C1, C2, _), _, fail)
        ),
        ConflictTuples),
    sort(ConflictTuples, UniqueConflicts),
    length(UniqueConflicts, ConflictTotal),
    format(S, '    "cs_axiom_conflict_total": ~w,~n', [ConflictTotal]),

    findall(K-(UID1-C1n)-(UID2-C2n),
        (   member(K, AllKernels),
            catch(cs_axiom_engine:cs_kernel_axiom_conflict(K, UID1-C1n, UID2-C2n, _), _, fail),
            (   catch(narrative_ontology:cs_reading_relation(UID1, C2n, forecloses), _, fail)
            ;   catch(narrative_ontology:cs_reading_relation(UID2, C1n, forecloses), _, fail)
            )
        ),
        RealClosureTuples),
    sort(RealClosureTuples, UniqueRC),
    length(UniqueRC, RealClosureCount),
    format(S, '    "cs_axiom_real_closure": ~w,~n', [RealClosureCount]),

    findall(K-(UID1p-C1np)-(UID2p-C2np),
        (   member(K, AllKernels),
            catch(cs_axiom_engine:cs_kernel_axiom_conflict(K, UID1p-C1np, UID2p-C2np, _), _, fail),
            (   catch(narrative_ontology:cs_reading_relation(UID1p, C2np, coexists_with), _, fail)
            ;   catch(narrative_ontology:cs_reading_relation(UID2p, C1np, coexists_with), _, fail)
            )
        ),
        PlurTuples),
    sort(PlurTuples, UniquePlur),
    length(UniquePlur, PlurCount),
    format(S, '    "cs_axiom_licensed_plurality": ~w,~n', [PlurCount]),

    % C3: per-kernel reading comparison (kernels with >= 2 readings only)
    findall(K,
        (   narrative_ontology:cs_kernel_id(_, K),
            cs_kernel_registry:cs_readings_for_kernel(K, Rs),
            length(Rs, L), L >= 2
        ),
        KList0),
    sort(KList0, KList),
    format(S, '    "cs_kernel_comparison": [~n', []),
    write_kernel_comparison_array(S, KList),
    format(S, '    ]~n', []),

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
                catch(purity_scoring:purity_score(C, PS), _, fail),
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
                once(report_generator:omega_severity(OID, Sev))),
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

%% tally_cs_patterns(+Constraints, -PatPairs, -FieldsAbsent, -Total, -VerdictPairs)
tally_cs_patterns(Constraints, PatPairs, FieldsAbsent, Total, VerdictPairs) :-
    findall(Pat,
            (   member(C, Constraints),
                catch(cs_pattern_detection:cs_has_fields(C), _, fail),
                catch(cs_pattern_detection:cs_pattern(C, Pat, _), _, fail)),
            Pats),
    length(Pats, Total),
    length(Constraints, CorpusTotal),
    FieldsAbsent is CorpusTotal - Total,
    msort(Pats, SortedPats),
    run_length_encode(SortedPats, PatPairs),
    findall(V,
            (   member(C, Constraints),
                catch(cs_pattern_detection:cs_verdict(C, V), _, fail)),
            AllVerdicts),
    msort(AllVerdicts, SortedVerdicts),
    run_length_encode(SortedVerdicts, VerdictPairs).

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
   CS KERNEL COMPARISON HELPERS (C3)
   ================================================================ */

%% write_kernel_comparison_array(+Stream, +Kernels)
write_kernel_comparison_array(_, []).
write_kernel_comparison_array(S, [K]) :-
    !, write_kernel_comparison_entry(S, K, false).
write_kernel_comparison_array(S, [K|Ks]) :-
    write_kernel_comparison_entry(S, K, true),
    write_kernel_comparison_array(S, Ks).

%% write_kernel_comparison_entry(+Stream, +K, +Comma)
write_kernel_comparison_entry(S, K, Comma) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Readings),
    length(Readings, RCount),
    findall(R1-R2,
        catch(cs_kernel_registry:cs_kernel_divergence(K, _, R1, R2), _, fail),
        DivPairs0),
    sort(DivPairs0, DivPairs),
    length(DivPairs, NDivPairs),
    findall(R1-R2,
        catch(cs_axiom_engine:cs_kernel_axiom_conflict(K, R1, R2, _), _, fail),
        ConflPairs0),
    sort(ConflPairs0, ConflPairs),
    length(ConflPairs, NConflPairs),
    format(S, '      {~n', []),
    format(S, '        "kernel_id": "~w",~n', [K]),
    format(S, '        "reading_count": ~w,~n', [RCount]),
    format(S, '        "diverging_pair_count": ~w,~n', [NDivPairs]),
    format(S, '        "axiom_conflict_count": ~w,~n', [NConflPairs]),
    format(S, '        "readings": [~n', []),
    write_reading_comparison_list(S, Readings),
    format(S, '        ]~n', []),
    (Comma == true -> format(S, '      },~n', []) ; format(S, '      }~n', [])).

%% write_reading_comparison_list(+Stream, +Readings)
write_reading_comparison_list(_, []).
write_reading_comparison_list(S, [R]) :-
    !, write_reading_comparison_entry(S, R, false).
write_reading_comparison_list(S, [R|Rs]) :-
    write_reading_comparison_entry(S, R, true),
    write_reading_comparison_list(S, Rs).

%% write_reading_comparison_entry(+Stream, +UID-C, +Comma)
%  UID is the story_uid surrogate; C is the reading name (constraint_id).
%  UID-keyed predicates (drift, axiom, mismatch) receive UID; C-keyed (pattern) receive C.
write_reading_comparison_entry(S, UID-C, Comma) :-
    format(S, '          {~n', []),
    format(S, '            "reading_id": "~w",~n', [C]),
    format(S, '            "story_uid": "~w",~n', [UID]),
    (   catch(cs_pattern_detection:cs_pattern(C, Pat, _), _, fail)
    ->  format(S, '            "cs_pattern": "~w",~n', [Pat])
    ;   format(S, '            "cs_pattern": null,~n', [])
    ),
    (   catch(cs_drift_engine:cs_drift_trajectory(UID, _, Term), _, fail)
    ->  format(S, '            "cs_drift_terminal": "~w",~n', [Term])
    ;   format(S, '            "cs_drift_terminal": null,~n', [])
    ),
    (   catch(cs_axiom_engine:cs_axiom_foreclosed(UID, AxAt), _, fail)
    ->  format(S, '            "cs_axiom_foreclosed": "~w",~n', [AxAt])
    ;   format(S, '            "cs_axiom_foreclosed": null,~n', [])
    ),
    (   catch(cs_pattern_detection:cs_drift_unacknowledged(UID, _), _, fail)
    ->  format(S, '            "cs_drift_unacknowledged": true,~n', [])
    ;   format(S, '            "cs_drift_unacknowledged": false,~n', [])
    ),
    (   catch(cs_drift_mismatch:cs_drift_mismatch(UID, _), _, fail)
    ->  format(S, '            "cs_drift_mismatch": true~n', [])
    ;   format(S, '            "cs_drift_mismatch": false~n', [])
    ),
    (Comma == true -> format(S, '          },~n', []) ; format(S, '          }~n', [])).

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
