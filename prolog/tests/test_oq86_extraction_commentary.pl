% ============================================================================
% test_oq86_extraction_commentary.pl — OQ-86 R3 extraction-reading commentary.
%
% Surfaces the no-authored-victim blindspot: a constraint that COMPUTES an
% extractive constraint-level type but authors no cost-bearer. The reading names
% the beneficiary-side seats (the extractors) and flags the cost-bearer as
% prose-only. R3 commentary — NEVER a classifier input.
%
% DERIVE-DON'T-STUB (OQ-86 plan, operator finding #4): every fixture asserts only
% INPUTS (constraint_metric, requires_active_enforcement, constraint_stakeholder,
% and for the victim-guard negative, constraint_victim). The constraint-level
% type is READ from drl_core:dr_type/2 and asserted extractive as a PRECONDITION
% of each positive/guard test — never hand-asserted. A fixture that fails to
% reach an extractive type is a calibration FINDING (the test fails visibly), not
% a value papered over.
%
% Controls:
%   (a) Positive (predicate): extractive + no victim + two co-equal beneficiary-
%       side seats -> extraction_reading fires; extractors = both co-equals.
%   (b) Channel witness: with_output_to over extraction_reading_line/1 contains
%       the literal "EXTRACTION READING" line (format string + call-site name).
%   (c) Three single-variable negatives, each isolating ONE guard so a dead guard
%       cannot hide: (i) type, (ii) victim, (iii) non-vacuity.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_oq86_extraction_commentary], run_tests, halt" -t "halt(1)"
% ============================================================================

:- multifile
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_beneficiary/2,
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1.

% ---------------------------------------------------------------------------
% Fixture 1 — POSITIVE: extractive, no victim, two co-equal beneficiary-side
% agents (cartel_member_a/b = agenda_setter@institutional) + one observer.
% Metrics calibrated so dr_type derives snare (measured this session: ε=0.82 /
% supp=0.91 + blindspot structure -> snare).
% ---------------------------------------------------------------------------
narrative_ontology:constraint_metric(oq86_positive, extractiveness, 0.82).
narrative_ontology:constraint_metric(oq86_positive, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(oq86_positive, theater_ratio, 0.30).
narrative_ontology:constraint_metric(oq86_positive, accessibility_collapse, 0.70).
narrative_ontology:constraint_metric(oq86_positive, resistance, 0.75).
domain_priors:requires_active_enforcement(oq86_positive).
narrative_ontology:constraint_stakeholder(oq86_positive, cartel_member_a, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:constraint_stakeholder(oq86_positive, cartel_member_b, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:constraint_stakeholder(oq86_positive, trade_press, observer,
    analytical, biographical, analytical, national).

% ---------------------------------------------------------------------------
% Fixture 2 — NEGATIVE (i) TYPE guard: non-extractive metrics, otherwise the
% positive shape. dr_type must derive a NON-extractive type -> silent on guard A.
% ---------------------------------------------------------------------------
narrative_ontology:constraint_metric(oq86_neg_type, extractiveness, 0.12).
narrative_ontology:constraint_metric(oq86_neg_type, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(oq86_neg_type, theater_ratio, 0.10).
narrative_ontology:constraint_metric(oq86_neg_type, accessibility_collapse, 0.10).
narrative_ontology:constraint_metric(oq86_neg_type, resistance, 0.10).
narrative_ontology:constraint_stakeholder(oq86_neg_type, cartel_member_a, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:constraint_stakeholder(oq86_neg_type, cartel_member_b, agenda_setter,
    institutional, generational, arbitrage, national).

% ---------------------------------------------------------------------------
% Fixture 3 — NEGATIVE (ii) VICTIM guard: extractive (same as positive) but an
% authored cost-bearer present -> silent on guard B.
% ---------------------------------------------------------------------------
narrative_ontology:constraint_metric(oq86_neg_victim, extractiveness, 0.82).
narrative_ontology:constraint_metric(oq86_neg_victim, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(oq86_neg_victim, theater_ratio, 0.30).
narrative_ontology:constraint_metric(oq86_neg_victim, accessibility_collapse, 0.70).
narrative_ontology:constraint_metric(oq86_neg_victim, resistance, 0.75).
domain_priors:requires_active_enforcement(oq86_neg_victim).
narrative_ontology:constraint_victim(oq86_neg_victim, captured_suppliers).
narrative_ontology:constraint_stakeholder(oq86_neg_victim, cartel_member_a, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:constraint_stakeholder(oq86_neg_victim, cartel_member_b, agenda_setter,
    institutional, generational, arbitrage, national).

% ---------------------------------------------------------------------------
% Fixture 4 — NEGATIVE (iii) NON-VACUITY guard: extractive, no victim, but NO
% beneficiary-side seat (only a payer + an observer) -> Extractors = [] -> silent.
% ---------------------------------------------------------------------------
narrative_ontology:constraint_metric(oq86_neg_vacuity, extractiveness, 0.82).
narrative_ontology:constraint_metric(oq86_neg_vacuity, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(oq86_neg_vacuity, theater_ratio, 0.30).
narrative_ontology:constraint_metric(oq86_neg_vacuity, accessibility_collapse, 0.70).
narrative_ontology:constraint_metric(oq86_neg_vacuity, resistance, 0.75).
domain_priors:requires_active_enforcement(oq86_neg_vacuity).
narrative_ontology:constraint_stakeholder(oq86_neg_vacuity, squeezed_supplier, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:constraint_stakeholder(oq86_neg_vacuity, trade_press, observer,
    analytical, biographical, analytical, national).

% ---------------------------------------------------------------------------
% Fixture 5 — REGRESSION: the OQ-93 data-repair bridge sentinel must NOT defeat
% guard B. The bridge fabricates constraint_victim(C, inferred_subject) on the
% blindspot metric profile (E>0.46 ∧ S>0.40, no authored victim); guard B reads
% AUTHORED absence, so the reading must STILL fire when the only victim is the
% sentinel. Asserted directly here to simulate the post-repair DB state without
% running scenario_manager. If this test fails, OQ-86 is inert on every real
% report (the literal `\+ constraint_victim` regression).
% ---------------------------------------------------------------------------
narrative_ontology:constraint_metric(oq86_bridge_sentinel, extractiveness, 0.82).
narrative_ontology:constraint_metric(oq86_bridge_sentinel, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(oq86_bridge_sentinel, theater_ratio, 0.30).
narrative_ontology:constraint_metric(oq86_bridge_sentinel, accessibility_collapse, 0.70).
narrative_ontology:constraint_metric(oq86_bridge_sentinel, resistance, 0.75).
domain_priors:requires_active_enforcement(oq86_bridge_sentinel).
narrative_ontology:constraint_victim(oq86_bridge_sentinel, inferred_subject).  % bridge sentinel
narrative_ontology:constraint_stakeholder(oq86_bridge_sentinel, cartel_member_a, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:constraint_stakeholder(oq86_bridge_sentinel, cartel_member_b, agenda_setter,
    institutional, generational, arbitrage, national).

% ===========================================================================

:- begin_tests(oq86_extraction_commentary).

% --- (a) Positive (predicate) — derive the type, then read the reading -------
test(positive_derives_extractive_type) :-
    drl_core:dr_type(oq86_positive, T),
    stakeholder_seats:extractive_type(T).           % checked read, not a stub

test(positive_no_authored_victim) :-
    \+ narrative_ontology:constraint_victim(oq86_positive, _).

test(positive_fires_with_both_coequals) :-
    stakeholder_seats:extraction_reading(oq86_positive,
        extraction(Extractors, cost_bearer_unnamed)),
    Extractors == [cartel_member_a, cartel_member_b].

% --- (b) Channel witness — format string + call-site predicate together -------
test(channel_line_present) :-
    with_output_to(string(S),
        report_generator:extraction_reading_line(oq86_positive)),
    once(sub_string(S, _, _, _, "EXTRACTION READING")),
    once(sub_string(S, _, _, _, "cartel_member_a")).

% --- (c)(i) TYPE guard — non-extractive type, otherwise positive shape --------
test(neg_type_derives_non_extractive) :-
    drl_core:dr_type(oq86_neg_type, T),
    \+ stakeholder_seats:extractive_type(T).        % precondition: guard A's input is off

test(neg_type_silent) :-
    \+ stakeholder_seats:extraction_reading(oq86_neg_type, _).

% --- (c)(ii) VICTIM guard — extractive but a cost-bearer authored -------------
test(neg_victim_still_extractive) :-
    drl_core:dr_type(oq86_neg_victim, T),
    stakeholder_seats:extractive_type(T).           % isolates guard B: type stays on

test(neg_victim_has_authored_victim) :-
    narrative_ontology:constraint_victim(oq86_neg_victim, _).

test(neg_victim_silent) :-
    \+ stakeholder_seats:extraction_reading(oq86_neg_victim, _).

% --- (c)(iii) NON-VACUITY guard — extractive, no victim, no beneficiary seat ---
test(neg_vacuity_still_extractive) :-
    drl_core:dr_type(oq86_neg_vacuity, T),
    stakeholder_seats:extractive_type(T).           % isolates guard C: A,B both pass

test(neg_vacuity_no_authored_victim) :-
    \+ narrative_ontology:constraint_victim(oq86_neg_vacuity, _).

test(neg_vacuity_silent) :-
    \+ stakeholder_seats:extraction_reading(oq86_neg_vacuity, _).

% --- Regression: bridge sentinel does NOT count as an authored victim ---------
test(bridge_sentinel_only_victim) :-
    findall(V, narrative_ontology:constraint_victim(oq86_bridge_sentinel, V), Vs),
    Vs == [inferred_subject].                        % the ONLY victim is the sentinel

test(bridge_sentinel_still_fires) :-
    stakeholder_seats:extraction_reading(oq86_bridge_sentinel,
        extraction(Extractors, cost_bearer_unnamed)),
    Extractors == [cartel_member_a, cartel_member_b].

:- end_tests(oq86_extraction_commentary).
