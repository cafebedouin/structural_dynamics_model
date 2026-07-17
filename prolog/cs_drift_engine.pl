% ============================================================================
% CS DRIFT ENGINE
% ============================================================================
% Computes t2 terminal attractors from authored t1 gap descriptors.
%
% Architecture: cs_drift_state/3 (authored t1) → cs_terminal_attractor/4
% (theory table) → cs_drift_trajectory/3 (computed t2). t2 is NEVER authored.
% This is the cs_grounding_mismatch pattern: authored claim in, theory-derived
% consequence out.
%
% Gap descriptor: gap(Direction, Magnitude, Acknowledged)
%   Direction ∈ {authority_erosion, codification_collapse, axiom_overriding,
%                practice_drift, revival_pressure, repudiation_pressure, stable}
%   Magnitude ∈ {minor, substantial, severe}
%   Acknowledged ∈ {true, false}
%
% Terminal atoms:
%   stable_pattern   — drift absorbed; system continues under its reference frame
%   husk             — form persists but normative grounding hollowed (Kodashim)
%   extinction       — text and practice both gone; terminal (Hittite/Sumerian)
%   revival          — acknowledged departure + active reconstruction (Hebrew)
%   repudiation      — legitimacy collapses entirely (post-1945 total-war cases)
%   axiom_foreclosure — foundational premise falsified; death-in-progress (Aristotelian
%                       cosmology post-Galileo; unabsorbed empirical refutation)
%
% Acknowledgment principle:
%   Acknowledgment rescues substantial drift to stable_pattern where an
%   institutional medium survives to act on it. It cannot rescue severe drift
%   (gap too large) or codification collapse (medium itself destroyed). For
%   codification collapse acknowledged + being rebuilt, reclassify direction
%   as revival_pressure before authoring — that case is not a valid
%   codification_collapse state.
%
% NOTE: cs_drift_trajectory/3 does NOT depend on cs_drift_unacknowledged/2.
%   cs_drift_unacknowledged/2 is a separate Type-A diagnostic in
%   cs_pattern_detection.pl. Acknowledged drift still gets a computed terminal.
%
% PROVENANCE (OQ-126 Gap 1, 2026-07-02): the Acknowledged bit is AUTHORED —
%   the story author's own call on whether the drift was acknowledged. The
%   terminal this table computes is therefore CONDITIONAL on taking that bit
%   at face value; it is not a settled honor-vs-reabsorb verdict (that verdict
%   is seated and floats — a hostile reader can read the same edit as
%   reabsorption-as-retreat; seat-theorem Cor 3 / design_discipline.md §8,§10).
%   json_report.pl carries this as cs_drift_terminal_basis: "authored_ack"
%   plus a cs_drift_ack_witness object whose confrontation_path "none_exists"
%   sentinel states that NO external confrontation path exists (OQ-107
%   unbuilt) — absence of a path, not a clean probe.
%
% CROSS-REF (2026-07-17, blog/2026-07/no-four-oclock-v8.md; v8 paper §5.9):
%   the Acknowledged bit IS the chosen-vs-adaptive distinction — invariant-as-
%   declared-choice (Euclid stated as an axiom) vs invariant-by-unacknowledged-
%   foreclosure (Euclid mistaken for a mountain). It is authored precisely
%   because no single time-slice probe recovers it: naturalization is a Type-A
%   drift (docs/debugging_philosophy.md), seeable only by frame-fixing and
%   tracing the snapshot series. This table cuts choice/foreclosure ONLY; the
%   mountain/choice boundary is cut elsewhere, by the beneficiary leg
%   (false_natural_law — a present otherwise, checked structurally not remembered).
% ============================================================================

:- module(cs_drift_engine, [cs_drift_trajectory/3]).

:- use_module(narrative_ontology).

%% cs_drift_trajectory(+UID, -Gap, -Terminal)
%  Extends the authored t0→t1 gap vector to a terminal attractor t2.
%  UID is the story_uid surrogate (UUIDv4); Gap is unified with the
%  gap/3 term from cs_drift_state/3.
cs_drift_trajectory(UID, Gap, Terminal) :-
    narrative_ontology:cs_drift_state(UID, _, Gap),
    Gap = gap(Direction, Magnitude, Acknowledged),
    cs_terminal_attractor(Direction, Magnitude, Acknowledged, Terminal).

% ---------------------------------------------------------------------------
% Attractor table (Direction, Magnitude, Acknowledged → Terminal)
% ---------------------------------------------------------------------------

% ROW-DISJOINTNESS (OQ-137 fix, 2026-07-02): the table is consumed as a
% function (one terminal per gap) but its rows used to overlap — (stable,
% minor, _) matched both the stable row and the minor row (duplicate
% stable_pattern, witnessed on 8 corpus UIDs), and (revival_pressure |
% repudiation_pressure, minor, _) matched the minor row AND its pressure row
% with DIFFERENT terminals (order-dependent for once/1 consumers, over-counted
% by aggregates). Guards below make every (Direction, Magnitude, Acknowledged)
% combination match exactly one row, preserving the pre-fix FIRST solution on
% all 42 combinations (before/after enumeration diff in the fix commit).

% No drift → stable regardless
cs_terminal_attractor(stable, _, _, stable_pattern).

% Minor drift → stable regardless (self-corrects under inertia)
cs_terminal_attractor(Direction, minor, _, stable_pattern) :-
    Direction \= stable.

% Authority erosion: medium survives, so acknowledgment can rescue substantial
cs_terminal_attractor(authority_erosion, severe, _, repudiation).         % post-1945 total-war cases
cs_terminal_attractor(authority_erosion, substantial, false, husk).       % Kodashim: form without grounding
cs_terminal_attractor(authority_erosion, substantial, true,  stable_pattern).

% Codification collapse: medium itself destroyed — acknowledgment variable dropped.
% If collapse is acknowledged + being rebuilt, reclassify direction as revival_pressure.
cs_terminal_attractor(codification_collapse, severe,       _, extinction). % definitional: no text + no practice = extinction; Hittite/Sumerian are examples, not proof
cs_terminal_attractor(codification_collapse, substantial,  _, husk).      % substrate hollowed; acknowledged rebuild → reclassify as revival_pressure

% Axiom overriding: falsified-premise death mode
cs_terminal_attractor(axiom_overriding, severe,       _, axiom_foreclosure). % Aristotelian cosmology post-Galileo
cs_terminal_attractor(axiom_overriding, substantial, false, axiom_foreclosure). % empirical refutation unabsorbed
cs_terminal_attractor(axiom_overriding, substantial, true,  stable_pattern).   % acknowledged axiomic update → framework revision

% Practice drift
cs_terminal_attractor(practice_drift, severe,       false, extinction).    % total departure unacknowledged → community forgets the form
cs_terminal_attractor(practice_drift, severe,       true,  revival).       % Hebrew revival: acknowledged departure + active reconstruction (fully grounded)
cs_terminal_attractor(practice_drift, substantial,  false, husk).
cs_terminal_attractor(practice_drift, substantial,  true,  stable_pattern).

% Revival and repudiation pressures (non-minor; a minor pressure gap is
% absorbed by the minor→stable row above — the pre-fix first solution)
cs_terminal_attractor(revival_pressure,      Magnitude, _, revival) :-      % Hebrew revival; also: acknowledged codification rebuild reclassified here
    Magnitude \= minor.
cs_terminal_attractor(repudiation_pressure,  Magnitude, _, repudiation) :-  % Nuremberg-type normative overturning
    Magnitude \= minor.
