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
known_override_signature(coupling_invariant_rope).
known_override_signature(natural_law).
known_override_signature(coordination_scaffold).
known_override_signature(constructed_low_extraction).
known_override_signature(constructed_high_extraction).
known_override_signature(constructed_constraint).

%% override_target(+Signature, -TargetType)
%  The type that a signature override forces.
override_target(false_natural_law,          tangled_rope).
override_target(false_ci_rope,              tangled_rope).
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

%% available_subsystems(-List)
%  Returns list of subsystem atoms that are currently available.
available_subsystems(Subs) :-
    findall(S, (
        member(S, [maxent, fpn, dirac, drift, signature, mismatch, fingerprint, cohomology]),
        subsystem_available(S)
    ), Subs).
