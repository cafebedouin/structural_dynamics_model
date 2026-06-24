% ====================================================================
% POSITIVE-CONTROL FIXTURE — NOT engine code. DO NOT consult into the live stack.
% Plants a v8 §8 path-c violation: a read-only seam in observer-verdict module
% drl_core feeds a committer field (cs_kernel_id) into observer computation by a
% route that NEVER touches influences/detect_necessity_inheritance. A count check
% and a per-bridge payload check on influences both pass; the reachability guard
% must fire. Loaded only by check_axis_boundary.py --selftest. (OQ-15, 2026-06-23.)
% ====================================================================
drl_core:axis_control_seam(C, K) :-
    narrative_ontology:cs_kernel_id(C, K).
