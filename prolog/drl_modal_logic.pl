% ============================================================================
% DRL MODAL LOGIC — Convenience wrapper (v4.0+)
% NOT the canonical import path. Import sub-modules directly:
%   :- use_module(drl_composition, [pred/arity, ...]).
%   :- use_module(drl_counterfactual, [pred/arity, ...]).
%   :- use_module(drl_boltzmann_analysis, [pred/arity, ...]).
%   :- use_module(drl_purity_network, [pred/arity, ...]).
%   :- use_module(drl_fpn, [pred/arity, ...]).
% Kept only for backward-compatible module-qualified calls
% (drl_modal_logic:pred) in files that have not been updated.
% ============================================================================

:- module(drl_modal_logic, []).

:- reexport(drl_composition).
:- reexport(drl_counterfactual).
:- reexport(drl_boltzmann_analysis).
:- reexport(drl_purity_network).
:- reexport(drl_fpn).
