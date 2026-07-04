% ============================================================================
% ABDUCTIVE HELPERS — Shared Dynamic Facts & Helper Predicates (v6.3)
% ============================================================================
% Extracted from abductive_engine.pl (Phase 6B decomposition).
%
% Centralizes:
%   - Dynamic fact declarations (abd_hypothesis/3, abd_run_info/3)
%   - Override signature tables (also used by diagnostic_summary.pl)
%   - Shared helper predicates used by both abductive_engine and triggers
%
% This module exists to break the circular dependency between the engine
% (which dispatches triggers and asserts hypotheses) and the triggers
% (which query existing hypotheses and use shared helpers).
% ============================================================================

:- module(abductive_helpers, [
    % Dynamic facts
    abd_hypothesis/3,
    abd_run_info/3,

    % Override tables
    known_override_signature/1,
    seat_overrides/2,
    override_target/2,

    % Fingerprint void classification
    extractive_void/1,

    % Zone helpers
    fpn_zone/2,
    one_hop_zone/3,

    % Confidence computation
    compute_confidence/3,

    % Subsystem availability
    subsystem_available/1,
    available_subsystems/1
]).

:- use_module(config).
:- use_module(purity_scoring, [purity_score/2]).
:- use_module(grothendieck_cohomology).
:- use_module(maxent_classifier).

:- use_module(library(lists)).

/* ================================================================
   DYNAMIC FACTS
   ================================================================ */

:- dynamic abd_hypothesis/3.    % abd_hypothesis(Constraint, Context, Hypothesis)
:- dynamic abd_run_info/3.      % abd_run_info(Context, NHypotheses, Timestamp)

/* ================================================================
   OVERRIDE SIGNATURE TABLES
   ================================================================ */

%% known_override_signature(?Signature)
%  Signatures that unconditionally override the metric-based type.
known_override_signature(false_natural_law).
known_override_signature(false_ci_rope).
% false_summit_mountain REMOVED 2026-06-21 (OQ-138): converted from RECLASSIFY to
% ROUTE/COMMENT — it no longer overrides dr_type, so it is no longer an override
% signature. Leaving it here would make probe_signature/3 emit a spurious
% override_mismatch tension (dr_type now = metric type, != the stale override
% target) and keep P1/P7 expected_conflict_pattern arms live with nothing to
% explain. FSM now grades via signature_detection:converted_signature/1 +
% signature_diagnostic_severity/3.
known_override_signature(coupling_invariant_rope).
known_override_signature(natural_law).
known_override_signature(coordination_scaffold).
known_override_signature(constructed_low_extraction).
known_override_signature(constructed_high_extraction).
known_override_signature(constructed_constraint).

%% seat_overrides(+C, +Signature)
%  OQ-138: seat-level "this signature overrides dr_type AT THIS SEAT". Signature-level
%  for every override signature EXCEPT false_ci_rope, which is SEAT-split: it is converted
%  to ROUTE at fcr_routed/1 seats (the FCR-9) and so NO LONGER overrides there, while its
%  piton (OQ-90) and inert seats keep override semantics. The override-artifact consumers
%  (diagnostic_summary probe_signature/3 + the P1/P7 expected_conflict_pattern arms) call
%  THIS instead of known_override_signature/1, so the routed FCR-9 are treated as
%  non-override (their subsystem divergences surface honestly, like FSM) while piton/inert
%  FCR seats are byte-identical. fcr_routed/1 is called module-qualified at runtime (same
%  cycle-avoidance as signature_grade's known_override_signature call — no load cycle).
seat_overrides(C, false_ci_rope) :- !, \+ signature_detection:fcr_routed(C).
seat_overrides(C, constructed_high_extraction) :- !, \+ signature_detection:constructed_routed(C).
% false_natural_law (OQ-138, 2026-07-03): keyed on the LEVER, not on fnl_routed/1 — a
% deliberate departure from the FCR seat-split shape, for two witnessed reasons:
% (1) FNL's conversion is lever-global: at lever=0 NO seat is overwritten anywhere (typed
%     seats route via signature_detection:925; unknown seats ride the OQ-37 abstain, which
%     predates the conversion), so "this seat overrides" is exactly the lever state. A
%     `\+ fnl_routed(C)` shape would mis-read the abstain seats as override-bearing.
% (2) fnl_routed/1 is default-context-keyed while the :925 overwrite was ORBIT-wide:
%     organization_floor_c0 routes tangled_rope->scaffold at the institutional position
%     while its default-context type is unknown (fnl_routed=false) — witnessed in the
%     OQ-138 twin diff (audits/2026-07-02_oq138_fnl_evidence/FNL_CONVERSION_DIFF.md §B).
%     The lever check is orbit-safe by construction; no per-context dispatch needed here.
% FCR keeps its `\+ fcr_routed` shape because its non-routed seats retain a LIVE type-layer
% override (fcr_override_enabled defaults 1); FNL's non-routed seats do not.
seat_overrides(_, false_natural_law) :- !,
    config:param(false_natural_law_override_enabled, 1).
seat_overrides(_, Sig) :- known_override_signature(Sig).

%% override_target(+Signature, -TargetType)
%  The type that a signature override forces.
override_target(false_natural_law,          tangled_rope).
override_target(false_ci_rope,              tangled_rope).
% override_target(false_summit_mountain, ...) REMOVED 2026-06-21 (OQ-138): FSM
% converted to ROUTE/COMMENT, forces no type. See known_override_signature/1 above.
override_target(coupling_invariant_rope,    rope).
override_target(natural_law,                mountain).
override_target(coordination_scaffold,      rope).
override_target(constructed_low_extraction, rope).
override_target(constructed_high_extraction, tangled_rope).
override_target(constructed_constraint,     tangled_rope).

/* ================================================================
   FINGERPRINT VOID CLASSIFICATION
   ================================================================ */

%% extractive_void(?VoidType)
%  Fingerprint voids that indicate extractive structural patterns.
extractive_void(unaccountable_extraction).
extractive_void(self_sustaining_extraction).
extractive_void(extractive_immutable).
extractive_void(coercion_without_coordination).

/* ================================================================
   ZONE HELPERS
   ================================================================ */

%% fpn_zone(+EP, -Zone)
%  Categorizes effective purity into zones (matching fpn_report.pl).
fpn_zone(EP, pure)         :- EP >= 0.80, !.
fpn_zone(EP, clean)        :- EP >= 0.60, !.
fpn_zone(EP, contaminated) :- EP >= 0.40, !.
fpn_zone(EP, compromised)  :- EP >= 0.20, !.
fpn_zone(_,  critical).

%% one_hop_zone(+C, +Context, -Zone)
%  Zone from the standard one-hop effective purity.
one_hop_zone(C, Context, Zone) :-
    catch(drl_modal_logic:effective_purity(C, Context, EP), _, fail),
    fpn_zone(EP, Zone).

/* ================================================================
   CONFIDENCE COMPUTATION
   ================================================================ */

%% compute_confidence(+EvidenceLines, +BaseConfidence, -Confidence)
%  Adjusts base confidence by evidence strength. More evidence lines
%  increase confidence slightly. Capped at 1.0.
compute_confidence(EvidenceLines, Base, Confidence) :-
    length(EvidenceLines, N),
    Bonus is min(0.10, N * 0.02),
    Raw is Base + Bonus,
    Confidence is min(1.0, max(0.0, Raw)).

/* ================================================================
   SUBSYSTEM AVAILABILITY
   ================================================================ */

%% subsystem_available(+Subsystem)
%  Checks whether a subsystem's data is present (has been run).
%  Does NOT check enable flags — only whether dynamic state exists.
subsystem_available(maxent) :-
    catch(maxent_classifier:maxent_run_info(_, _, _), _, fail), !.
subsystem_available(fpn) :-
    catch(drl_modal_logic:fpn_iteration_info(_, _, _, _), _, fail), !.
subsystem_available(dirac) :- !.     % Always available (computed on demand)
subsystem_available(drift) :- !.     % Always available (computed on demand)
subsystem_available(signature) :- !. % Always available (part of core pipeline)
subsystem_available(mismatch) :- !.  % Always available (part of core pipeline)
subsystem_available(fingerprint) :-  % Always available (computed on demand)
    !.
subsystem_available(cohomology) :-
    predicate_property(grothendieck_cohomology:cohomological_obstruction(_,_,_), defined), !.
subsystem_available(indexed_maxent) :-
    catch(maxent_classifier:maxent_indexed_dist(_, _, _), _, fail), !.

%% available_subsystems(-List)
%  Returns list of subsystem atoms that are currently available.
available_subsystems(Subs) :-
    findall(S, (
        member(S, [maxent, indexed_maxent, fpn, dirac, drift, signature, mismatch, fingerprint, cohomology]),
        subsystem_available(S)
    ), Subs).
