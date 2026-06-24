% ====================================================================
% POSITIVE-CONTROL FIXTURE — NOT engine code. DO NOT consult into the live stack.
% Plants a v8 §8 path-b violation: the single forward bridge
% detect_necessity_inheritance reads a committer field OTHER than the influences
% relation (cs_axiom_foreclosed). Bridge-predicate COUNT stays 1, so a count
% check passes — this is the path a count check misses. The reachability guard
% (check_axis_boundary.pl) must fire on the new committer callee.
% Loaded only by check_axis_boundary.py --selftest. (OQ-15 / OQ-135, 2026-06-23.)
% ====================================================================
:- multifile drl_composition:detect_necessity_inheritance/2.
drl_composition:detect_necessity_inheritance(S, S) :-
    narrative_ontology:cs_axiom_foreclosed(S, _).
