% ============================================================================
% DIAGNOSTIC SUMMARY — Cross-Subsystem Verdict Synthesis (v1.0)
% ============================================================================
% Queries all available Tier 2 diagnostic subsystems, separates expected
% architectural disagreements from genuine tensions, and produces a
% green/yellow/red traffic-light verdict per constraint.
%
% The verdict is purely observational — it does NOT change any classification.
%
% Architecture:
%   1. diagnostic_summary/2      — main entry: constraint → structured verdict
%   2. 12 probe predicates       — one per subsystem
%   3. expected_conflict_pattern  — catalog of known architectural artifacts
%   4. compute_verdict           — green/yellow/red rules
%   5. diagnostic_selftest/0     — verify catalog coverage
%
% Loaded within the json_report.pl swipl process.
% ============================================================================

:- module(diagnostic_summary, [
    diagnostic_summary/2,
    diagnostic_verdict/2,
    diagnostic_selftest/0
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(signature_detection, [
    constraint_signature/2,
    has_metric_perspectival_variance/1
]).
:- use_module(boltzmann_compliance, [
    boltzmann_compliant/2,
    boltzmann_shadow_audit/2,
    detect_nonsensical_coupling/3,
    excess_extraction/2
]).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(logical_fingerprint).
:- use_module(drl_lifecycle).
:- use_module(grothendieck_cohomology).
:- use_module(maxent_classifier).
:- use_module(dirac_classification).

:- use_module(library(lists)).

% Local copies of override tables (not exported by abductive_engine).
known_override_signature(false_natural_law).
known_override_signature(false_ci_rope).
known_override_signature(coupling_invariant_rope).
known_override_signature(natural_law).
known_override_signature(coordination_scaffold).
known_override_signature(constructed_low_extraction).
known_override_signature(constructed_high_extraction).
known_override_signature(constructed_constraint).

override_target(false_natural_law,           tangled_rope).
override_target(false_ci_rope,               tangled_rope).
override_target(coupling_invariant_rope,     rope).
override_target(natural_law,                 mountain).
override_target(coordination_scaffold,       rope).
override_target(constructed_low_extraction,  rope).
override_target(constructed_high_extraction, tangled_rope).
override_target(constructed_constraint,      tangled_rope).

constructed_type(scaffold).
constructed_type(snare).
constructed_type(tangled_rope).
constructed_type(piton).

/* ================================================================
   SUBSYSTEM AVAILABILITY
   ================================================================ */

ds_subsystem_available(maxent) :-
    catch(maxent_classifier:maxent_run_info(_, _, _), _, fail), !.
ds_subsystem_available(cohomology) :-
    predicate_property(grothendieck_cohomology:cohomological_obstruction(_,_,_), defined), !.
ds_subsystem_available(abductive) :-
    predicate_property(json_report:abd_triggers(_,_), defined), !.
ds_subsystem_available(signature) :- !.
ds_subsystem_available(boltzmann) :- !.
ds_subsystem_available(purity) :- !.
ds_subsystem_available(dirac) :- !.
ds_subsystem_available(fingerprint_voids) :- !.
ds_subsystem_available(drift) :- !.
ds_subsystem_available(context_gap) :- !.
ds_subsystem_available(fcr_gate) :- !.
ds_subsystem_available(gauge_orbit) :- !.

all_subsystems([maxent, cohomology, abductive, signature, boltzmann,
                purity, dirac, fingerprint_voids, drift, context_gap,
                fcr_gate, gauge_orbit]).

/* ================================================================
   MAIN ENTRY POINT
   ================================================================ */

%% diagnostic_summary(+Constraint, -Summary)
%  Returns a structured verdict term for a single constraint.
%  Called inline during JSON serialization (subsystem data already precomputed).
diagnostic_summary(C, Summary) :-
    constraint_indexing:default_context(Ctx),
    (   catch(drl_core:dr_type(C, Ctx, DetType), _, fail)
    ->  true
    ;   DetType = unknown
    ),
    collect_signals(C, Ctx, DetType, Signals),
    classify_all_signals(C, Signals, DetType, Agreements, ExpConflicts, Tensions),
    count_convergent_rejections(Tensions, Rejections, RemainingTensions),
    compute_verdict(ExpConflicts, Rejections, RemainingTensions, Verdict),
    all_subsystems(AllSubs),
    include(ds_subsystem_available, AllSubs, AvailList),
    exclude(ds_subsystem_available, AllSubs, UnavailList),
    length(AvailList, NAvail),
    Summary = diagnostic_summary(
        Verdict,
        Agreements,
        ExpConflicts,
        Rejections,
        RemainingTensions,
        NAvail,
        UnavailList
    ).

%% diagnostic_verdict(+Summary, -Verdict)
diagnostic_verdict(diagnostic_summary(V, _, _, _, _, _, _), V).

/* ================================================================
   SIGNAL COLLECTION
   ================================================================ */

collect_signals(C, Ctx, DetType, Signals) :-
    findall(
        signal(Subsystem, Signal),
        (   all_subsystems(All),
            member(Subsystem, All),
            ds_subsystem_available(Subsystem),
            run_probe(Subsystem, C, Ctx, DetType, Signal)
        ),
        Signals
    ).

run_probe(maxent,           C, Ctx, DetType, Sig) :- probe_maxent(C, Ctx, DetType, Sig).
run_probe(cohomology,       C, _,   DetType, Sig) :- probe_cohomology(C, DetType, Sig).
run_probe(abductive,        C, Ctx, DetType, Sig) :- probe_abductive(C, Ctx, DetType, Sig).
run_probe(signature,        C, _,   DetType, Sig) :- probe_signature(C, DetType, Sig).
run_probe(boltzmann,        C, _,   DetType, Sig) :- probe_boltzmann(C, DetType, Sig).
run_probe(purity,           C, _,   DetType, Sig) :- probe_purity(C, DetType, Sig).
run_probe(dirac,            C, _,   DetType, Sig) :- probe_dirac(C, DetType, Sig).
run_probe(fingerprint_voids,C, _,   DetType, Sig) :- probe_fingerprint_voids(C, DetType, Sig).
run_probe(drift,            C, _,   DetType, Sig) :- probe_drift(C, DetType, Sig).
run_probe(context_gap,      C, Ctx, DetType, Sig) :- probe_context_gap(C, Ctx, DetType, Sig).
run_probe(fcr_gate,         C, _,   DetType, Sig) :- probe_fcr_gate(C, DetType, Sig).
run_probe(gauge_orbit,      C, _,   DetType, Sig) :- probe_gauge_orbit(C, DetType, Sig).

/* ================================================================
   PROBE PREDICATES
   ================================================================ */

%% probe_maxent(+C, +Ctx, +DetType, -Signal)
probe_maxent(C, Ctx, _DetType, Signal) :-
    (   catch(maxent_classifier:maxent_disagreement(C, Ctx, DisInfo), _, fail)
    ->  (   DisInfo = none
        ->  Signal = agrees
        ;   DisInfo = hard(Shadow, Det)
        ->  Signal = disagrees(hard(Shadow, Det))
        ;   DisInfo = soft(Shadow, Det, P)
        ->  Signal = disagrees(soft(Shadow, Det, P))
        ;   DisInfo = entropy_flag(H)
        ->  Signal = disagrees(entropy_flag(H))
        ;   DisInfo = residual_override(Shadow, Det)
        ->  Signal = disagrees(residual_override(Shadow, Det))
        ;   Signal = agrees
        )
    ;   Signal = inconclusive
    ).

%% probe_cohomology(+C, +DetType, -Signal)
probe_cohomology(C, DetType, Signal) :-
    (   catch(grothendieck_cohomology:cohomological_obstruction(C, _H0, H1), _, fail)
    ->  (   catch(grothendieck_cohomology:descent_status(C, Status), _, fail)
        ->  (   Status = descends(DetType)
            ->  Signal = agrees
            ;   Status = descends(_OtherType)
            ->  Signal = disagrees(descent_mismatch(Status))
            ;   Signal = disagrees(fails_descent(H1))
            )
        ;   (   H1 =:= 0
            ->  Signal = agrees
            ;   Signal = disagrees(h1_nonzero(H1))
            )
        )
    ;   Signal = inconclusive
    ).

%% probe_abductive(+C, +Ctx, +DetType, -Signal)
%  Queries abd_triggers/2 facts asserted by json_report's abductive loader.
%  Only genuine (non-artifact) triggers without expected_abductive_trigger
%  matches produce tensions.
probe_abductive(C, _Ctx, DetType, Signal) :-
    (   catch(json_report:abd_triggers(C, Triggers), _, fail),
        Triggers \= []
    ->  (   member(trigger(Class, _Conf, _Anomaly, genuine), Triggers),
            \+ expected_abductive_trigger(Class, DetType)
        ->  Signal = disagrees(abductive_tension(Triggers))
        ;   Signal = agrees
        )
    ;   Signal = agrees
    ).

%% probe_signature(+C, +DetType, -Signal)
probe_signature(C, DetType, Signal) :-
    (   catch(signature_detection:constraint_signature(C, Sig), _, fail)
    ->  (   known_override_signature(Sig)
        ->  (   override_target(Sig, Target),
                Target = DetType
            ->  Signal = agrees_via_override(Sig)
            ;   Signal = disagrees(override_mismatch(Sig, DetType))
            )
        ;   Signal = agrees
        )
    ;   Signal = agrees
    ).

%% probe_boltzmann(+C, +DetType, -Signal)
probe_boltzmann(C, DetType, Signal) :-
    (   catch(boltzmann_compliance:boltzmann_compliant(C, Result), _, fail)
    ->  (   Result = compliant(_)
        ->  Signal = agrees
        ;   Result = non_compliant(Score, Threshold)
        ->  Signal = disagrees(non_compliant(Score, Threshold, DetType))
        ;   Signal = inconclusive
        )
    ;   Signal = inconclusive
    ).

%% probe_purity(+C, +DetType, -Signal)
probe_purity(C, DetType, Signal) :-
    (   catch(purity_scoring:purity_score(C, PScore), _, fail),
        PScore \= -1.0
    ->  logical_fingerprint:purity_zone(PScore, Zone),
        (   (DetType = mountain ; DetType = rope)
        ->  (   (Zone = pristine ; Zone = sound)
            ->  Signal = agrees
            ;   Signal = disagrees(low_purity(PScore, Zone))
            )
        ;   Signal = agrees
        )
    ;   Signal = inconclusive
    ).

%% probe_dirac(+C, +DetType, -Signal)
probe_dirac(C, DetType, Signal) :-
    constraint_indexing:default_context(Ctx),
    (   catch(dirac_classification:dirac_class(C, Ctx, Class), _, fail)
    ->  (   dirac_consistent(Class, DetType)
        ->  Signal = agrees
        ;   Signal = disagrees(dirac_mismatch(Class, DetType))
        )
    ;   Signal = inconclusive
    ).

dirac_consistent(first_class, mountain).
dirac_consistent(first_class, rope).
dirac_consistent(first_class_degenerate, mountain).
dirac_consistent(first_class_degenerate, rope).
dirac_consistent(second_class, snare).
dirac_consistent(second_class, piton).
dirac_consistent(second_class, scaffold).
dirac_consistent(mixed, tangled_rope).
dirac_consistent(mixed, _).
dirac_consistent(undetermined, _).

%% probe_fingerprint_voids(+C, +DetType, -Signal)
probe_fingerprint_voids(C, DetType, Signal) :-
    (   catch(logical_fingerprint:fingerprint_voids(C, Voids), _, fail)
    ->  (   Voids = []
        ->  Signal = agrees
        ;   (DetType = mountain ; DetType = rope)
        ->  (   member(V, Voids), ds_extractive_void(V)
            ->  Signal = disagrees(extractive_voids(Voids))
            ;   Signal = agrees
            )
        ;   Signal = agrees
        )
    ;   Signal = inconclusive
    ).

ds_extractive_void(unaccountable_extraction).
ds_extractive_void(self_sustaining_extraction).
ds_extractive_void(extractive_immutable).
ds_extractive_void(coercion_without_coordination).

%% probe_drift(+C, +DetType, -Signal)
%  Requires ≥3 distinct critical event types to fire tension.
%  Rationale: one repeated critical type is a single measurement system;
%  three independent critical types means convergent drift evidence.
probe_drift(C, _DetType, Signal) :-
    (   catch(drl_lifecycle:scan_constraint_drift(C, Events), _, (Events = []))
    ->  (   Events = []
        ->  Signal = agrees
        ;   findall(ET, member(drift(ET, _, critical), Events), CritTypes),
            sort(CritTypes, UniqueCritTypes),
            length(UniqueCritTypes, NDistinct),
            (   NDistinct >= 3
            ->  Signal = disagrees(critical_drift(Events))
            ;   Signal = agrees
            )
        )
    ;   Signal = agrees
    ).

%% probe_context_gap(+C, +Ctx, +DetType, -Signal)
probe_context_gap(C, Ctx, DetType, Signal) :-
    (   catch(constraint_indexing:classify_from_restricted(C, Ctx, RestrictedType), _, fail)
    ->  (   RestrictedType = DetType
        ->  Signal = agrees
        ;   RestrictedType = indeterminate
        ->  Signal = agrees
        ;   Signal = disagrees(restricted_mismatch(RestrictedType, DetType))
        )
    ;   Signal = inconclusive
    ).

%% probe_fcr_gate(+C, +DetType, -Signal)
probe_fcr_gate(C, DetType, Signal) :-
    (   catch(signature_detection:constraint_signature(C, Sig), _, fail)
    ->  (   Sig = false_ci_rope
        ->  (   catch(signature_detection:has_metric_perspectival_variance(C), _, fail)
            ->  Signal = agrees
            ;   (   DetType = tangled_rope
                ->  Signal = agrees
                ;   Signal = disagrees(fcr_override_mismatch(DetType))
                )
            )
        ;   Signal = agrees
        )
    ;   Signal = agrees
    ).

%% probe_gauge_orbit(+C, +DetType, -Signal)
probe_gauge_orbit(C, _DetType, Signal) :-
    (   catch(dirac_classification:gauge_orbit(C, Orbit), _, fail)
    ->  findall(T, member(orbit_point(T, _), Orbit), Types),
        sort(Types, UniqueTypes),
        (   UniqueTypes = [_]
        ->  Signal = agrees
        ;   Signal = disagrees(multi_type_orbit(UniqueTypes))
        )
    ;   Signal = inconclusive
    ).

/* ================================================================
   SIGNAL CLASSIFICATION
   ================================================================ */

%% classify_all_signals(+C, +Signals, +DetType, -Agreements, -ExpConflicts, -Tensions)
classify_all_signals(C, Signals, DetType, Agreements, ExpConflicts, Tensions) :-
    classify_signals_acc(C, Signals, DetType, [], [], [], Agreements, ExpConflicts, Tensions).

classify_signals_acc(_, [], _, AgAcc, ECAcc, TAcc, Ag, EC, T) :-
    reverse(AgAcc, Ag), reverse(ECAcc, EC), reverse(TAcc, T).
classify_signals_acc(C, [signal(Sub, Signal)|Rest], DetType,
                     AgAcc, ECAcc, TAcc, Ag, EC, T) :-
    (   Signal = agrees
    ->  classify_signals_acc(C, Rest, DetType, [Sub|AgAcc], ECAcc, TAcc, Ag, EC, T)
    ;   Signal = agrees_via_override(_)
    ->  classify_signals_acc(C, Rest, DetType, [Sub|AgAcc], ECAcc, TAcc, Ag, EC, T)
    ;   Signal = inconclusive
    ->  classify_signals_acc(C, Rest, DetType, AgAcc, ECAcc, TAcc, Ag, EC, T)
    ;   Signal = unavailable
    ->  classify_signals_acc(C, Rest, DetType, AgAcc, ECAcc, TAcc, Ag, EC, T)
    ;   Signal = disagrees(Detail)
    ->  (   expected_conflict_pattern(C, Sub, Detail, DetType, Pattern, Explanation)
        ->  EC1 = expected_conflict(Sub, Pattern, Explanation),
            classify_signals_acc(C, Rest, DetType, AgAcc, [EC1|ECAcc], TAcc, Ag, EC, T)
        ;   T1 = tension(Sub, Detail),
            classify_signals_acc(C, Rest, DetType, AgAcc, ECAcc, [T1|TAcc], Ag, EC, T)
        )
    ;   classify_signals_acc(C, Rest, DetType, [Sub|AgAcc], ECAcc, TAcc, Ag, EC, T)
    ).

/* ================================================================
   EXPECTED CONFLICT CATALOG (P1–P10)
   ================================================================
   expected_conflict_pattern(+C, +Subsystem, +Detail, +DetType, -Pattern, -Explanation)
   Each clause matches a known architectural disagreement that is NOT a genuine
   tension. When matched, the signal is classified as expected.
   ================================================================ */

% --- P1: signature_override_artifact ---
% MaxEnt disagrees because signature override forces a type MaxEnt cannot replicate.
expected_conflict_pattern(C, maxent, hard(_Shadow, Det), Det,
    signature_override_artifact,
    'MaxEnt disagrees because signature override forces type') :-
    catch(signature_detection:constraint_signature(C, Sig), _, fail),
    known_override_signature(Sig),
    override_target(Sig, Det).

% --- P2: fcr_gate_deferral ---
% FCR signature present but perspectival variance gate deferred the override.
expected_conflict_pattern(_, fcr_gate, fcr_override_mismatch(_), _,
    fcr_gate_deferral,
    'FCR gate deferred override due to metric perspectival variance').

% --- P3: cohomological_fracture_divergence ---
% H1 > 0 confirms genuine perspectival fracture. Ambiguity is structural.
expected_conflict_pattern(C, maxent, hard(_, _), _,
    cohomological_fracture_divergence,
    'H1 > 0 confirms perspectival fracture; MaxEnt ambiguity is structural') :-
    catch(grothendieck_cohomology:cohomological_obstruction(C, _, H1), _, fail),
    H1 > 0.

expected_conflict_pattern(_, cohomology, h1_nonzero(_), DetType,
    cohomological_fracture_divergence,
    'Perspectival fracture (H1 > 0) expected for constructed/perspectival type') :-
    constructed_type(DetType).

expected_conflict_pattern(_, cohomology, fails_descent(_), DetType,
    cohomological_fracture_divergence,
    'Descent failure expected for constructed/perspectival type') :-
    constructed_type(DetType).

expected_conflict_pattern(_, cohomology, descent_mismatch(_), DetType,
    cohomological_fracture_divergence,
    'Descent yields different type; perspectival difference is structural') :-
    constructed_type(DetType).

% --- P4: natural_law_incidental_beneficiaries ---
% True natural laws can have incidental beneficiaries.
expected_conflict_pattern(C, fingerprint_voids, extractive_voids(_), mountain,
    natural_law_incidental_beneficiaries,
    'True natural laws can have incidental beneficiaries') :-
    catch(signature_detection:constraint_signature(C, natural_law), _, fail).

% --- P5: constructed_non_compliance ---
% Boltzmann non-compliance expected for constructed types with scope-driven coupling.
expected_conflict_pattern(C, boltzmann, non_compliant(_, _, DetType), DetType,
    constructed_non_compliance,
    'Constructed types couple dimensions deliberately; non-compliance is confirmatory') :-
    constructed_type(DetType),
    % Mode-aware: check that coupling violations are scope-driven, not unexpected
    (   catch(boltzmann_compliance:detect_nonsensical_coupling(C, CoupledPairs, _), _, fail)
    ->  (   CoupledPairs = []
        ->  true  % No nonsensical coupling at all — non-compliance from other factors
        ;   % Check all pairs are of expected power_scope form
            forall(
                member(Pair, CoupledPairs),
                Pair = coupled(power_scope, _, _, _)
            )
        )
    ;   true  % Can't check — give benefit of doubt
    ).

% --- P6: fcr_zero_excess_coupling ---
% FCR triggered by coupling structure, not extraction overhead.
expected_conflict_pattern(C, boltzmann, non_compliant(_, _, _), _,
    fcr_zero_excess_coupling,
    'FCR triggered by coupling structure, not extraction overhead') :-
    catch(signature_detection:constraint_signature(C, false_ci_rope), _, fail),
    catch(boltzmann_compliance:excess_extraction(C, Excess), _, fail),
    Excess < 0.05.

% --- P7: pre_post_override_divergence ---
% Any subsystem computing type from metrics sees the pre-override type.
expected_conflict_pattern(C, context_gap, restricted_mismatch(_, DetType), DetType,
    pre_post_override_divergence,
    'Restricted classifier sees pre-override metric-based type') :-
    catch(signature_detection:constraint_signature(C, Sig), _, fail),
    known_override_signature(Sig).

expected_conflict_pattern(C, dirac, dirac_mismatch(_, DetType), DetType,
    pre_post_override_divergence,
    'Dirac class reflects metric-layer type, not override type') :-
    catch(signature_detection:constraint_signature(C, Sig), _, fail),
    known_override_signature(Sig).

% --- P8: tangled_rope_mixed_dirac ---
% Tangled rope is defined as mixed first-class/second-class.
expected_conflict_pattern(_, dirac, dirac_mismatch(mixed, tangled_rope), tangled_rope,
    tangled_rope_mixed_dirac,
    'Tangled rope is mixed first-class/second-class; Dirac mixed is definitional').

% --- P9: residual_type_artifact ---
% Residual types (unknown, indexically_opaque) are not in MaxEnt's 6-type model.
expected_conflict_pattern(_, maxent, residual_override(_, _), _,
    residual_type_artifact,
    'Residual types not in MaxEnt 6-type model; disagreement is mechanistic').

% --- P10: perspectival_orbit_variance ---
% Multi-type orbit IS the perspectival fracture, confirmed by H1 > 0.
expected_conflict_pattern(C, gauge_orbit, multi_type_orbit(_), _,
    perspectival_orbit_variance,
    'Multi-type orbit IS the perspectival fracture') :-
    (   catch(grothendieck_cohomology:cohomological_obstruction(C, _, H1), _, fail),
        H1 > 0
    ->  true
    ;   true  % No cohomology available; accept orbit variance as structural
    ).

% --- P11: fcr_deferred_signature_mismatch ---
% Signature probe sees FCR override target mismatch, but FCR gate deferred
% because perspectival variance exists. The mismatch is architectural.
expected_conflict_pattern(C, signature, override_mismatch(false_ci_rope, _), _,
    fcr_deferred_signature_mismatch,
    'FCR override target mismatch; gate deferred due to perspectival variance') :-
    catch(signature_detection:has_metric_perspectival_variance(C), _, fail).

/* ================================================================
   EXPECTED ABDUCTIVE TRIGGERS
   ================================================================
   expected_abductive_trigger(+TriggerClass, +DetType)
   Trigger classes that are expected for a given det_type and should not
   produce tensions. Initially empty — all genuine triggers are tensions.
   Add entries here as calibration reveals universally-firing triggers.
   ================================================================ */

% (none yet — all genuine triggers produce tensions)
expected_abductive_trigger(_, _) :- fail.

/* ================================================================
   CONVERGENT REJECTIONS
   ================================================================ */

%% count_convergent_rejections(+Tensions, -Rejections, -Remaining)
%  Group tensions that point to the same alternative type (>= 2 subsystems).
count_convergent_rejections(Tensions, Rejections, Remaining) :-
    findall(
        AltType,
        (   member(tension(_, Detail), Tensions),
            alternative_type_from_detail(Detail, AltType)
        ),
        AltTypes
    ),
    msort(AltTypes, Sorted),
    run_length_encode_ds(Sorted, TypeCounts),
    include(convergent_pair, TypeCounts, ConvergentPairs),
    (   ConvergentPairs = []
    ->  Rejections = [], Remaining = Tensions
    ;   findall(
            convergent_rejection(AltT, Count, Subs),
            (   member(AltT-Count, ConvergentPairs),
                findall(Sub,
                    (   member(tension(Sub, Detail), Tensions),
                        alternative_type_from_detail(Detail, AltT)
                    ),
                    Subs)
            ),
            Rejections
        ),
        findall(AltT, member(AltT-_, ConvergentPairs), ConvergentTypes),
        exclude(tension_in_convergent(ConvergentTypes), Tensions, Remaining)
    ).

convergent_pair(_-Count) :- Count >= 2.

tension_in_convergent(ConvergentTypes, tension(_, Detail)) :-
    alternative_type_from_detail(Detail, AltType),
    member(AltType, ConvergentTypes).

alternative_type_from_detail(hard(Shadow, _), Shadow).
alternative_type_from_detail(soft(Shadow, _, _), Shadow).
alternative_type_from_detail(restricted_mismatch(RestType, _), RestType).
alternative_type_from_detail(dirac_mismatch(_, _), unknown).
alternative_type_from_detail(descent_mismatch(descends(T)), T).

run_length_encode_ds([], []).
run_length_encode_ds([H|T], [H-Count|Rest]) :-
    count_run_ds(H, T, Count, Remainder),
    run_length_encode_ds(Remainder, Rest).

count_run_ds(_, [], 1, []).
count_run_ds(H, [H|T], N, Rest) :-
    !, count_run_ds(H, T, N1, Rest), N is N1 + 1.
count_run_ds(_, List, 1, List).

/* ================================================================
   VERDICT COMPUTATION
   ================================================================ */

%% compute_verdict(+ExpConflicts, +Rejections, +Tensions, -Verdict)
%  GREEN:  No tensions, no convergent rejections.
%  YELLOW: Has tensions OR convergent rejections, but no critical mass.
%  RED:    >= 3 tensions, OR convergent rejection with >= 3 subsystems.
compute_verdict(_, Rejections, _, red) :-
    member(convergent_rejection(_, _, Subs), Rejections),
    length(Subs, N), N >= 3, !.
compute_verdict(_, _, Tensions, red) :-
    length(Tensions, N), N >= 3, !.
compute_verdict(_, [_|_], _, yellow) :- !.
compute_verdict(_, _, [_|_], yellow) :- !.
compute_verdict(_, _, _, green).

/* ================================================================
   UTILITIES
   ================================================================ */

%% compute_psi(+C, +Ctx, -Psi)
%  P(snare) / (P(rope) + P(snare) + 0.001) from MaxEnt distribution.
compute_psi(C, Ctx, Psi) :-
    catch(maxent_classifier:maxent_distribution(C, Ctx, Dist), _, fail),
    (   member(snare-PSnare, Dist) -> true ; PSnare = 0.0 ),
    (   member(rope-PRope, Dist) -> true ; PRope = 0.0 ),
    Denom is PRope + PSnare + 0.001,
    Psi is PSnare / Denom.

%% shannon_entropy_raw(+Probs, -H)
%  Shannon entropy on a raw probability list.
shannon_entropy_raw(Probs, H) :-
    foldl(entropy_acc, Probs, 0.0, H).

entropy_acc(P, Acc, NewAcc) :-
    (   P > 0.0
    ->  NewAcc is Acc - P * log(P) / log(2)
    ;   NewAcc = Acc
    ).

/* ================================================================
   SELFTEST
   ================================================================ */

%% diagnostic_selftest/0
%  Verify each expected conflict pattern fires on >= 1 corpus constraint.
diagnostic_selftest :-
    format(user_error, '[diagnostic_selftest] Starting...~n', []),
    all_patterns(Patterns),
    check_patterns(Patterns, 0, 0, Passed, Failed),
    format(user_error, '[diagnostic_selftest] Patterns: ~w passed, ~w failed~n',
           [Passed, Failed]),
    (   Failed > 0
    ->  format(user_error, '[diagnostic_selftest] FAIL~n', [])
    ;   format(user_error, '[diagnostic_selftest] PASS~n', [])
    ).

all_patterns([
    signature_override_artifact,
    fcr_gate_deferral,
    cohomological_fracture_divergence,
    natural_law_incidental_beneficiaries,
    constructed_non_compliance,
    fcr_zero_excess_coupling,
    pre_post_override_divergence,
    tangled_rope_mixed_dirac,
    residual_type_artifact,
    perspectival_orbit_variance,
    fcr_deferred_signature_mismatch
]).

check_patterns([], Passed, Failed, Passed, Failed).
check_patterns([Pat|Rest], PAcc, FAcc, Passed, Failed) :-
    (   pattern_fires_on_corpus(Pat)
    ->  P1 is PAcc + 1,
        check_patterns(Rest, P1, FAcc, Passed, Failed)
    ;   format(user_error, '  [WARN] Pattern ~w never fires on corpus~n', [Pat]),
        F1 is FAcc + 1,
        check_patterns(Rest, PAcc, F1, Passed, Failed)
    ).

%% pattern_fires_on_corpus(+Pattern)
%  True if at least one constraint triggers the given expected conflict pattern.
pattern_fires_on_corpus(Pattern) :-
    logical_fingerprint:known_constraint(C),
    constraint_indexing:default_context(Ctx),
    catch(drl_core:dr_type(C, Ctx, DetType), _, fail),
    collect_signals(C, Ctx, DetType, Signals),
    member(signal(Sub, disagrees(Detail)), Signals),
    expected_conflict_pattern(C, Sub, Detail, DetType, Pattern, _),
    !.
