:- module(signature_mapper, [
    map_custom_pillar/3
]).

:- use_module(narrative_ontology).
:- use_module(signature_detection, [constraint_signature/2]).
:- use_module(config).

%% map_custom_pillar(+Constraint, +CustomType, -StandardType)
%  Maps non-standard terminology to v3.1 standard pillars based on metrics.
map_custom_pillar(_C, CustomType, StandardType) :-
    % If already standard, do nothing
    member(CustomType, [mountain, rope, tangled_rope, snare, scaffold, piton]),
    StandardType = CustomType, !.

map_custom_pillar(C, _CustomType, StandardType) :-
    % Analyze structural signature based on current metrics
    signature_detection:constraint_signature(C, Signature),
    resolve_mapping(Signature, StandardType).

% DEAD MAP ENTRIES (OQ-296, confirmed 2026-08-18): neither key is ever produced
% by constraint_signature/2 — natural_law's `HasAlternatives == false` and
% coordination_scaffold's `== true` are both unsatisfiable (signature_detection
% :427, :478). Confirmed at edit time by unbound once/1 census on the live leg:
% both 0. Kept as sockets, not retired — GAP-08 §7 revives both at once. Same
% disposition as the piton_signature row below, different cause.
% Natural Laws and high-stability invariants are Mountains
resolve_mapping(natural_law, mountain).
% Successful voluntary coordination (like decentralized_governance) are Ropes
resolve_mapping(coordination_scaffold, rope).
% A scaffold that has decayed into a liability is a Piton
% superseded by OQ-90 FCR refinement; the piton_signature atom is no longer produced
% from the profile path (dispatch retired 2026-06-11). Left in place — ruled scope.
resolve_mapping(piton_signature, piton).
% Asymmetric, enforced, or extractive rules are Nooses
resolve_mapping(constructed_constraint, snare).
% Fallback for ambiguous data
resolve_mapping(ambiguous, rope).
