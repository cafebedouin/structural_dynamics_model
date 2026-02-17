% ============================================================================
% DRL MODAL LOGIC — Facade (v4.0+)
% Reexports from focused sub-modules. All drl_modal_logic:predicate/N
% calls continue to resolve through this facade.
% ============================================================================

:- module(drl_modal_logic, []).

:- reexport(drl_composition).
:- reexport(drl_counterfactual).
:- reexport(drl_boltzmann_analysis).
:- reexport(drl_purity_network).
:- reexport(drl_fpn).
