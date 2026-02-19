% Convenience wrapper — NOT the canonical import path.
% Import sub-modules directly with selective imports:
%   :- use_module(boltzmann_compliance, [pred/arity, ...]).
%   :- use_module(signature_detection, [pred/arity, ...]).
%   :- use_module(purity_scoring, [pred/arity, ...]).
% Kept only for backward-compatible module-qualified calls
% (structural_signatures:pred) in files that have not been updated.
:- module(structural_signatures, []).

:- reexport(boltzmann_compliance).
:- reexport(signature_detection).
:- reexport(purity_scoring).
