% ============================================================================
% TEST: OQ-39 row 14 — scaffold_suppression_escalating commentary verdict
% ============================================================================
% Positive control for the cs_verdict/2 clause added to cs_pattern_detection.pl.
% The clause is COMMENTARY (annotate-only): it records that a scaffold-certified
% constraint has a RISING authored suppression series — contradicting the scaffold
% "suppression declines over time" expectation — WITHOUT reclassifying.
%
% Run from prolog/:
%   swipl -g "[stack], [tests/test_oq39_scaffold_escalation], run_tests, halt" -t "halt(1)"
%
% Three cases (the operator-required set):
%   1. scaffold + RISING suppression  -> verdict FIRES.
%   2. scaffold + FLAT suppression     -> verdict SILENT; dr_type unchanged (scaffold).
%   3. scaffold + RISING + an independent cs_verdict (false_marked_revision)
%      -> cs_verdicts contains BOTH. This is the CUT-REGRESSION control: it proves
%      the new clause's once/1 (no trailing !) and its FIRST placement leave the
%      sibling cs_pattern-gated clauses reachable. A trailing ! on the new clause,
%      or placing it below the family, would drop one of the two verdicts.
% ============================================================================

:- use_module(library(plunit)).

% Synthetic constraints live in narrative_ontology (the engine reads CS/metric/
% measurement facts there). Internal cs_kernel_codification/2 joins through
% cs_story_uid/2, so the dual case needs an identity UID map.
:- multifile narrative_ontology:constraint_metric/3.
:- multifile narrative_ontology:has_coordination_function/1.
:- multifile narrative_ontology:has_sunset_clause/1.
:- multifile narrative_ontology:measurement/5.
:- multifile narrative_ontology:cs_kernel_codification/2.
:- multifile narrative_ontology:cs_authority_grounding/2.
:- multifile narrative_ontology:cs_story_uid/2.

% --- Case 1: scaffold + RISING suppression ---------------------------------
narrative_ontology:constraint_metric(t39_rise, extractiveness, 0.10).
narrative_ontology:constraint_metric(t39_rise, base_extractiveness, 0.10).
narrative_ontology:constraint_metric(t39_rise, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(t39_rise, theater_ratio, 0.10).
narrative_ontology:has_coordination_function(t39_rise).
narrative_ontology:has_sunset_clause(t39_rise).
narrative_ontology:measurement(t39_r1, t39_rise, suppression_requirement, 0, 0.10).
narrative_ontology:measurement(t39_r2, t39_rise, suppression_requirement, 5, 0.40).

% --- Case 2: scaffold + FLAT suppression -----------------------------------
narrative_ontology:constraint_metric(t39_flat, extractiveness, 0.10).
narrative_ontology:constraint_metric(t39_flat, base_extractiveness, 0.10).
narrative_ontology:constraint_metric(t39_flat, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(t39_flat, theater_ratio, 0.10).
narrative_ontology:has_coordination_function(t39_flat).
narrative_ontology:has_sunset_clause(t39_flat).
narrative_ontology:measurement(t39_f1, t39_flat, suppression_requirement, 0, 0.20).
narrative_ontology:measurement(t39_f2, t39_flat, suppression_requirement, 5, 0.20).

% --- Case 3: scaffold + RISING + independent false_marked_revision verdict --
% marked_revision pattern = formalized kernel + expertise authority; the verdict
% fires via the theater_ratio >= 0.60 branch. theater 0.65 stays <= 0.70 so the
% scaffold classification is preserved.
narrative_ontology:cs_story_uid(t39_dual, t39_dual).
narrative_ontology:cs_kernel_codification(t39_dual, formalized).
narrative_ontology:cs_authority_grounding(t39_dual, expertise).
narrative_ontology:constraint_metric(t39_dual, extractiveness, 0.10).
narrative_ontology:constraint_metric(t39_dual, base_extractiveness, 0.10).
narrative_ontology:constraint_metric(t39_dual, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(t39_dual, theater_ratio, 0.65).
narrative_ontology:has_coordination_function(t39_dual).
narrative_ontology:has_sunset_clause(t39_dual).
narrative_ontology:measurement(t39_d1, t39_dual, suppression_requirement, 0, 0.10).
narrative_ontology:measurement(t39_d2, t39_dual, suppression_requirement, 5, 0.40).

verdicts(C, Vs) :-
    findall(V, cs_pattern_detection:cs_verdict(C, V), Vs).

any_scaffold(C) :-
    drl_core:standard_context(Ctx),
    drl_core:dr_type(C, Ctx, scaffold), !.

:- begin_tests(oq39_scaffold_escalation).

% Case 1: fires on rising.
test(rising_fires) :-
    verdicts(t39_rise, Vs),
    memberchk(scaffold_suppression_escalating, Vs).

% Case 1: the substrate really is scaffold (guards against a vacuous pass where
% the constraint never classified scaffold in the first place).
test(rising_is_scaffold) :-
    any_scaffold(t39_rise).

% Case 2: silent on flat — verdict absent.
test(flat_silent) :-
    verdicts(t39_flat, Vs),
    \+ memberchk(scaffold_suppression_escalating, Vs).

% Case 2: commentary did not touch classification — still scaffold.
test(flat_unchanged_dr_type) :-
    any_scaffold(t39_flat).

% Case 3 (cut regression): BOTH verdicts present.
test(dual_carries_both) :-
    verdicts(t39_dual, Vs),
    memberchk(scaffold_suppression_escalating, Vs),
    memberchk(false_marked_revision, Vs).

% Case 3: classification preserved despite theater 0.65.
test(dual_still_scaffold) :-
    any_scaffold(t39_dual).

:- end_tests(oq39_scaffold_escalation).
