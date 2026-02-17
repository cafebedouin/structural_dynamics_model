% ============================================================================
% DRL LIFECYCLE — Facade (reexports from split modules)
%
% Original monolith split into:
%   drift_events.pl      — Event detection, severity, velocity
%   transition_paths.pl  — Degradation path detection & terminal states
%   network_dynamics.pl  — Network drift, contagion, cascades
%   drift_report.pl      — Unified scan & report generation
%
% All original exports remain accessible via drl_lifecycle:predicate/N.
% ============================================================================

:- module(drl_lifecycle, []).

:- reexport(drift_events).
:- reexport(transition_paths).
:- reexport(network_dynamics).
:- reexport(drift_report).
