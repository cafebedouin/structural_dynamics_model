% ============================================================================
% TYPE METADATA — Constraint Type Descriptions, Strategies & Severity (v6.0)
% ============================================================================
% Extracted from report_generator.pl (Phase 6A decomposition).
% Pure data predicates — no logic, no side effects.
%
% Provides human-readable descriptions, strategic recommendations,
% color coding, and numeric severity ordering for the 6 constraint types.
% ============================================================================

:- module(type_metadata, [
    type_description/2,
    type_strategy/2,
    type_color/2,
    type_severity/2
]).

/* ============================================================================
   TYPE DESCRIPTIONS & STRATEGIES (Updated January 2026 for Tangled Rope)
   ============================================================================ */

%% type_description(?Type, ?Description)
%  Human-readable descriptions for constraint types.
type_description(mountain,
    'Natural constraint - unchangeable given current understanding of reality').
type_description(rope,
    'Pure coordination - low extraction, solves collective action problems').
type_description(tangled_rope,
    'Hybrid coordination/extraction - provides genuine coordination while extracting asymmetrically').
type_description(snare,
    'Pure extraction - minimal coordination benefit, high asymmetric extraction').
type_description(piton,
    'Maintained constraint - low extraction but high suppression costs, should be cut but isn''t').
type_description(scaffold, 'Temporary support - low extraction coordination with a defined sunset clause').

%% type_strategy(?Type, ?Strategy)
%  Strategic recommendations for each constraint type.
type_strategy(mountain,
    'Accept - Work within natural constraints, adapt strategies accordingly').
type_strategy(rope,
    'Maintain - Preserve coordination mechanisms, ensure fair access and participation').
type_strategy(tangled_rope,
    'Reform carefully - Preserve coordination core while cutting extractive elements. Requires surgical separation.').
type_strategy(snare,
    'Cut - Remove extractive constraints, replace with fair alternatives if coordination needed').
type_strategy(piton,
    'Bypass or eliminate - High maintenance cost without value, find alternatives').
type_strategy(scaffold, 'Monitor sunset - Ensure transition mechanisms are in place, remove when no longer needed').

%% type_color(?Type, ?Color)
%  Color coding for visualization and reports.
type_color(mountain, blue).
type_color(rope, green).
type_color(tangled_rope, orange).  % Orange for hybrid nature
type_color(snare, red).
type_color(piton, gray).
type_color(scaffold, yellow).

%% type_severity(?Type, ?Severity)
%  Numeric severity ordering for constraint types (matches conflict_map.py SEVERITY dict).
type_severity(mountain, 0).
type_severity(rope, 1).
type_severity(scaffold, 2).
type_severity(piton, 3).
type_severity(tangled_rope, 4).
type_severity(snare, 5).
