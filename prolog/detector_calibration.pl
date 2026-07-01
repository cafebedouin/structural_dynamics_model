% ============================================================================
% detector_calibration.pl — the apparatus-directed calibration omega (Slice B)
% ============================================================================
% Mints an OPEN omega where the engine's own extraction call is a consequential,
% hard-to-externally-check disagreement with the author — and hands the question
% back. It NEVER closes the question: the corpus carries no ground truth (authored
% type is a seat, not truth — seat theorem), so "is the detector calibrated" is an
% external Ω_E, and "what false-positive rate is acceptable" is an Ω_P value-decision.
%
% Design + rulings (R1–R4): docs/design/detector_calibration_omega_proposal.md.
% This module is the SOLE producer of detector_calibration omegas (R3: a single
% named path, NOT a general engine-minting facility — there is no assert path here
% at all; the omega is COMPUTED live, parallel to report_generator's gap omegas).
%
% STATUS (R4 ruling, operator 2026-07-01): TRACKED-BUT-UNWIRED REFERENCE. Loaded by
% NOTHING in the pipeline; wired into no report. The author↔engine directional-
% disagreement question this computes is carried at the corpus level by OQ-200, NOT
% as a per-constraint firing — because the net-new firings are low-KIND-entropy
% (~90% two directional patterns: false-summit mountain→tangled_rope = OQ-70, and a
% tangled_rope→rope author-over-claims-contestation residual). This file is kept as
% the reference implementation should Ω_E (external calibration) ever get a ground-
% truth answer that makes per-constraint firing worth revisiting. Do NOT wire it
% without reopening R4. Evidence: audits/2026-07-01_oq197_r4_recompute/.
% ============================================================================

:- module(detector_calibration, [
    detector_calibration_due/2,     % (C, Class)  Class ∈ {boundary, severity}
    detector_calibration_fires/1,   % (C) — C has ≥1 qualifying, gated, unguarded seat
    calibration_omega/2             % (C, omega(ID,Type,Question,Severity)) — SOLE producer
]).

:- use_module(library(lists)).
:- use_module(drl_core).
:- use_module(routing_sink).
:- use_module(report_generator).
:- use_module(boltzmann_compliance).
:- use_module(narrative_ontology).

% ---------------------------------------------------------------------------
% Type sets — the engine's OWN extractive/functional axis (report_generator.pl:
% 243-248) MINUS naturalized (R1: naturalized is extractive-by-construction,
% routed to the false_summit apparatus, never counted as the "safe" direction).
% ---------------------------------------------------------------------------
dc_functional(rope).
dc_functional(scaffold).
dc_functional(mountain).
dc_extractive(snare).
dc_extractive(tangled_rope).

% FC1 — boundary crossing, SYMMETRIC (either direction across the line, so the
% omega is not blind to the engine's own under-calling; R1 / Corrections log #4).
fc1_boundary(AT, ET) :- dc_functional(AT), dc_extractive(ET).
fc1_boundary(AT, ET) :- dc_extractive(AT), dc_functional(ET).

% FC2 — within-extraction severity, BOTH directions (tangled_rope ↔ snare: is the
% extraction contested/hedged, or clean/confirmed?). The largest single bucket.
fc2_severity(tangled_rope, snare).
fc2_severity(snare, tangled_rope).

% ---------------------------------------------------------------------------
% A firing seat: a both-speak disagreement matching FC1 or FC2.  Author/Engine
% readings come from routing_sink:seat_diff (the authoritative both-speak source).
% ---------------------------------------------------------------------------
firing_seat(C, Seat, AT, ET, Class) :-
    routing_sink:seat_diff(C, Seat, Author, Engine, _, _, _),
    Author = [AT],                          % author speaks exactly one reading
    nonvar(Engine), Engine \== engine_silent,
    ET = Engine,
    AT \== ET,
    (   fc1_boundary(AT, ET) -> Class = boundary
    ;   fc2_severity(AT, ET) -> Class = severity
    ;   fail
    ).

% ---------------------------------------------------------------------------
% Consequence gate — the two CATEGORICAL limbs only. The scalar theater_ratio
% limb is DROPPED (R1: no honest borrow for the floor; an uncalibrated default is
% worse than the two categorical limbs). Disjunction: consequential if EITHER.
% ---------------------------------------------------------------------------
consequence_gate(C) :- coupling_masked(C), !.
consequence_gate(C) :- no_exit_victim(C).

coupling_masked(C) :-
    catch(boltzmann_compliance:boltzmann_compliant(C, non_compliant(_, _)), _, fail).

no_exit_victim(C) :-
    narrative_ontology:constraint_stakeholder(C, _, payer, _, _, trapped, _), !.

% ---------------------------------------------------------------------------
% Three-branch overlap guard (R4), at constraint granularity: skip C entirely if
% extraction_blindness already covers it (covers branch 1 = cross-seat cover-story
% AND branch 3 = FC2 re-collision, since a skip removes ALL detector_calibration
% firings for C). Branch 2 (naturalized routing) is handled structurally by the
% type-sets — naturalized is in neither dc_functional nor dc_extractive, so no
% transition into or out of it can match FC1/FC2.
% ---------------------------------------------------------------------------
% OQ-197 NOTE (R4 recompute site — behaviour intentionally UNCHANGED here): when
% extraction_blindness is UNDETERMINED (too few operable seats), detect_gap_pattern
% fails, so already_covered/1 is false and detector_calibration fires — treating
% "couldn't tell" as "not covered". That is exactly the R4 net-new inflation mechanism.
% Distinguishing undetermined from genuinely-not-covered here CHANGES R4 counts, so it
% is deferred to the OQ-197 step-5 R4 recompute (HELD on the operator), NOT the wiring
% pass. The three-valued status is available via report_generator:gap_status/2-3 when
% that step runs; report_generator:gap_status(C, undetermined(_)) is the undetermined test.
already_covered(C) :-
    catch(report_generator:detect_gap_pattern(C, gap(extraction_blindness, _, _)), _, fail).

% ---------------------------------------------------------------------------
% Firing predicate.
% ---------------------------------------------------------------------------
detector_calibration_due(C, Class) :-
    firing_seat(C, _Seat, _AT, _ET, Class),
    consequence_gate(C),
    \+ already_covered(C).

detector_calibration_fires(C) :-
    once(detector_calibration_due(C, _)).

% ---------------------------------------------------------------------------
% The omega PAIR — computed, never asserted. Ω_E (hit rate, awaits external data)
% + Ω_P (acceptable FP rate, a value-decision). ID contains C so json_report's
% collect_omegas surfaces it. Severity supplied directly (moderate/low by type),
% matching report_generator:omega_severity's type tiers without depending on it.
% ---------------------------------------------------------------------------
calibration_omega(C, omega(OID, empirical, Question, moderate)) :-
    detector_calibration_fires(C),
    findall(Cl, detector_calibration_due(C, Cl), Cls0), sort(Cls0, Classes),
    format(atom(OID), 'omega_detector_calibration_hitrate_~w', [C]),
    format(atom(Question),
        'Detector calibration — hit rate (Ω_E, awaits external labeled data). Engine and author disagree on this constraint''s extraction type at a consequential, hard-to-externally-check seat (classes: ~w). Does the engine''s extraction call track real hidden extraction better than chance here? NOT resolvable from the corpus — the authored type is a seat, not ground truth (seat theorem). Resolution = an external validation study.',
        [Classes]).
calibration_omega(C, omega(OID, preference, Question, low)) :-
    detector_calibration_fires(C),
    format(atom(OID), 'omega_detector_calibration_fprate_~w', [C]),
    format(atom(Question),
        'Detector calibration — acceptable false-positive rate (Ω_P, a value-decision). Given the cost of a wrong extraction verdict on this constraint, what FP rate is tolerable for acting on the engine''s call? A ruling by those bearing the cost, not a measurement.',
        []).
