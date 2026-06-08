% ============================================================================
% CONSTRAINT STORY: extraordinary_measures_duration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extraordinary_measures_duration, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: extraordinary_measures_duration
 *   human_readable: Extraordinary Measures Duration Buffer
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The extraordinary measures duration buffer is the time interval between
 *   when the federal government reaches the statutory debt limit and when it
 *   exhausts all available cash management tools (the 'X Date'). This
 *   duration is mechanically determined by two observable quantities: the
 *   balance in federal retirement funds (primarily the Civil Service
 *   Retirement and Disability Fund and the Postal Service Retiree Health
 *   Benefits Fund) and the monthly net cash flow of the federal government.
 *   Treasury has legal authority to suspend reinvestment in these funds once
 *   the debt limit is reached, converting fund balances into usable cash. The
 *   buffer duration T is approximately B/|C| where B is the fund balance
 *   available for suspension and C is the average monthly cash deficit. This
 *   constraint is presented as a mountain — pure arithmetic with no policy
 *   discretion — but the beneficiary structure (Treasury gains operational
 *   flexibility, congressional negotiators gain negotiation time) creates an
 *   irreducible ambiguity about whether the 'mechanical' framing naturalizes
 *   an institutional arrangement that serves specific interests. The
 *   constraint is downstream of the statutory ceiling vs suspension choice:
 *   the existence of a statutory ceiling (rather than rolling authority)
 *   creates the need for extraordinary measures, and the fund structure
 *   determines how long those measures last.
 *
 * KEY AGENTS:
 *   - Federal Program Beneficiaries: Primary potential victims (powerless/trapped) — Social Security recipients, Medicare beneficiaries, federal contractors who face payment disruption if X Date is reached without resolution
 *   - Federal Employees: Secondary potential victims (moderate/constrained) — Civil service and postal workers whose retirement funds provide the buffer; constrained by employment lock-in
 *   - Treasury Department: Primary beneficiary (institutional/arbitrage) — Gains operational flexibility and time to manage cash flow; the buffer enables core function
 *   - Congressional Negotiators: Primary beneficiaries (institutional/arbitrage) — Both parties gain known deadline for debt limit negotiation; the buffer provides political cover and negotiating structure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — Sees pure arithmetic constraint but must evaluate whether the 'mechanical' framing naturalizes beneficiary structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extraordinary_measures_duration, 0.08).
domain_priors:suppression_score(extraordinary_measures_duration, 0.02).
domain_priors:theater_ratio(extraordinary_measures_duration, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extraordinary_measures_duration, extractiveness, 0.08).
narrative_ontology:constraint_metric(extraordinary_measures_duration, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(extraordinary_measures_duration, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(extraordinary_measures_duration, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(extraordinary_measures_duration, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extraordinary_measures_duration, mountain).
narrative_ontology:human_readable(extraordinary_measures_duration, "Extraordinary Measures Duration Buffer").
narrative_ontology:topic_domain(extraordinary_measures_duration, "public_finance/constitutional_law/political_economy").

domain_priors:emerges_naturally(extraordinary_measures_duration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extraordinary_measures_duration, treasury_department).
narrative_ontology:constraint_beneficiary(extraordinary_measures_duration, congressional_negotiators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL PROGRAM BENEFICIARIES (MOUNTAIN) — Social Security recipients, Medicare beneficiaries, federal contractors. Cannot exit dependency on timely payments. Experience the duration buffer as an immutable arithmetic fact: retirement fund balances plus monthly cash flow determines when payments stop. No alternative exists.
constraint_indexing:constraint_classification(extraordinary_measures_duration, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL EMPLOYEES (MOUNTAIN) — Civil service and postal workers whose retirement funds provide the buffer. Constrained by employment lock-in and pension dependency. See the duration as mechanical: fund balances are observable quantities, cash flow is arithmetic, the buffer is what it is.
constraint_indexing:constraint_classification(extraordinary_measures_duration, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY DEPARTMENT (ROPE) — Primary beneficiary. The duration buffer is a coordination tool: it provides negotiating time between statutory limit and default. Treasury experiences low extraction — the buffer enables their core function (managing federal cash flow) and gives them operational flexibility. The mechanical nature of the calculation is a feature, not a constraint.
constraint_indexing:constraint_classification(extraordinary_measures_duration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL NEGOTIATORS (ROPE) — Beneficiaries of the time buffer for political negotiation. The duration provides a known deadline for debt limit resolution. Low extraction — the buffer serves their coordination need (avoiding default while negotiating fiscal policy). The arithmetic constraint is enabling rather than limiting.
constraint_indexing:constraint_classification(extraordinary_measures_duration, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The duration buffer is pure arithmetic: retirement fund balance B, monthly net cash flow C, buffer duration T = B/|C| when C < 0. This is not policy, not institutional design, not extractive — it is accounting identity. Once the statutory limit is reached, the buffer is mechanically determined by observable quantities. No party can change the arithmetic. The buffer exists because retirement funds are off-budget and Treasury has legal authority to suspend reinvestment — but the duration those suspensions buy is a mathematical consequence of fund size and cash flow rate.
constraint_indexing:constraint_classification(extraordinary_measures_duration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extraordinary_measures_duration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extraordinary_measures_duration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_measures_duration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(extraordinary_measures_duration, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_measures_duration, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(extraordinary_measures_duration, ExtMetricName, E),
    domain_priors:suppression_score(extraordinary_measures_duration, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(extraordinary_measures_duration),
    narrative_ontology:constraint_metric(extraordinary_measures_duration, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(extraordinary_measures_duration, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(extraordinary_measures_duration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The buffer duration is mechanically determined by observable quantities (fund balances, cash flow). Treasury and congressional negotiators benefit from the time buffer, but this benefit appears to be coordination overhead rather than extraction — the buffer serves a genuine function (avoiding immediate default upon reaching the statutory limit) and the beneficiaries do not suppress alternatives or collect rents. The slight extraction reflects that the buffer's existence depends on the statutory ceiling framework (a policy choice) rather than being truly inevitable, and that Treasury has some discretion in the timing and sequencing of extraordinary measures. Suppression (0.02): Negligible. No alternatives are actively suppressed — the arithmetic is what it is once the statutory limit is reached. The low suppression reflects only that the statutory ceiling framework itself (upstream constraint) forecloses rolling authority alternatives. Resistance (0.03): Negligible. No party resists the arithmetic of fund balances and cash flow. The buffer duration is accepted as mechanical by all actors. Accessibility collapse (0.95): Near-total. Once the statutory limit is reached, the buffer duration is determined by observable fund balances and cash flow projections. No alternative calculation method exists — it is accounting identity. Theater ratio (0.05): Very low. The calculation of extraordinary measures duration is functional, not performative. Treasury publishes fund balances and cash flow projections; the X Date estimate is a genuine operational constraint, not theater. The slight theater reflects only the uncertainty in cash flow projections (which introduces some performative hedging in public communications).
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify as mountain or rope — there is no snare or tangled_rope perspective because the constraint's operation does not create identifiable victims under normal conditions. The gap is between those who experience the buffer as pure arithmetic (powerless and moderate agents with no exit) and those who experience it as a coordination tool (institutional beneficiaries with arbitrage options). The analytical observer must evaluate whether this gap reveals false summit dynamics: is the 'pure arithmetic' framing a naturalization of an institutional choice (the statutory ceiling framework) that benefits Treasury and Congress? The omega variables document this irreducible uncertainty. If the buffer duration can be strategically manipulated, or if Treasury uses the buffer period to extract concessions beyond operational necessity, the mountain classification fails and the constraint decomposes into separate stories for different institutional actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury Department and Congressional Negotiators are declared beneficiaries — they gain operational flexibility and negotiation time from the buffer. The engine derives low directionality (d near 0.0) for these institutional actors with arbitrage exit options, producing low or negative effective extraction (they experience the constraint as enabling). Federal Program Beneficiaries and Federal Employees are not declared as victims because they do not bear extraction under normal operation — the buffer prevents payment disruption rather than causing it. They would become victims only if the buffer were exhausted without resolution (X Date breach), which is a failure mode rather than the constraint's normal operation. The analytical observer perspective has no beneficiary/victim status — it evaluates the constraint as pure arithmetic. The critical ambiguity (omega variable) is whether Treasury's operational benefit constitutes coordination overhead (supporting the mountain classification) or extraction (indicating false summit). If extraction is present, the mountain claim naturalizes an institutional arrangement that benefits specific actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that a mechanical calculation can still have a beneficiary structure without becoming extractive. The buffer duration is genuinely arithmetic — fund balance divided by cash flow rate — but the existence of the buffer (rather than immediate default upon reaching the limit) depends on institutional design choices (off-budget retirement funds, Treasury's legal authority to suspend reinvestment, the statutory ceiling framework itself). The mountain classification holds if the beneficiaries' gains are pure coordination overhead. The classification fails (false summit) if the mechanical framing naturalizes extraction. The omega variables document the empirical tests that would resolve this: does Treasury use the buffer extractively? Can the duration be manipulated? Does the buffer create asymmetric congressional negotiating power? The presheaf structure is simpler than the verification bottleneck exemplar because the constraint has fewer distinct perspectives — but the false summit question is structurally identical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_extraction_ambiguity,
    'Does Treasury''s operational benefit from the duration buffer constitute extraction, or is it purely coordination overhead?',
    'Analysis of Treasury''s use of the buffer period: if the time is used solely for cash management and communication with Congress (coordination), extraction is negligible. If Treasury uses the buffer to extract concessions or delay politically costly decisions beyond operational necessity, extraction is present.',
    'If extraction is present, the mountain classification from the analytical perspective is a false summit — the ''pure arithmetic'' framing naturalizes an institutional arrangement that benefits specific actors. If extraction is absent, the mountain holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_extraction_ambiguity, empirical, 'Whether Treasury''s benefit from the buffer constitutes extraction or coordination').

omega_variable(
    congressional_negotiation_asymmetry,
    'Does the duration buffer create asymmetric negotiating advantage for specific congressional factions?',
    'Historical analysis of debt limit negotiations: do factions with preference for smaller government systematically benefit from the deadline pressure the buffer creates? Does the known X Date enable hostage-taking that would be impossible with continuous rolling authority?',
    'If asymmetry exists, congressional negotiators are not uniform beneficiaries — some benefit more than others, and the buffer enables extraction from the disadvantaged faction. This would require decomposition into separate constraint stories for different congressional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_negotiation_asymmetry, empirical, 'Whether the buffer creates asymmetric congressional negotiating power').

omega_variable(
    fund_balance_manipulation,
    'Can Treasury or Congress manipulate the fund balances or cash flow to extend or shorten the buffer strategically?',
    'Legal analysis of Treasury''s discretion in extraordinary measures timing and scope; analysis of congressional ability to affect cash flow through appropriations timing or tax policy changes during the buffer period.',
    'If manipulation is possible, the ''pure arithmetic'' mountain claim is false — the buffer duration is a policy choice disguised as mechanical constraint. If manipulation is legally or practically impossible, the mountain holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fund_balance_manipulation, empirical, 'Whether the buffer duration can be strategically manipulated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extraordinary_measures_duration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(extmeas_theater_2011, extraordinary_measures_duration, theater_ratio, 0, 0.03).
narrative_ontology:measurement(extmeas_theater_2013, extraordinary_measures_duration, theater_ratio, 2, 0.04).
narrative_ontology:measurement(extmeas_theater_2015, extraordinary_measures_duration, theater_ratio, 4, 0.04).
narrative_ontology:measurement(extmeas_theater_2017, extraordinary_measures_duration, theater_ratio, 6, 0.05).
narrative_ontology:measurement(extmeas_theater_2019, extraordinary_measures_duration, theater_ratio, 8, 0.05).
narrative_ontology:measurement(extmeas_theater_2021, extraordinary_measures_duration, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(extmeas_extract_2011, extraordinary_measures_duration, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(extmeas_extract_2013, extraordinary_measures_duration, base_extractiveness, 2, 0.06).
narrative_ontology:measurement(extmeas_extract_2015, extraordinary_measures_duration, base_extractiveness, 4, 0.07).
narrative_ontology:measurement(extmeas_extract_2017, extraordinary_measures_duration, base_extractiveness, 6, 0.08).
narrative_ontology:measurement(extmeas_extract_2019, extraordinary_measures_duration, base_extractiveness, 8, 0.08).
narrative_ontology:measurement(extmeas_extract_2021, extraordinary_measures_duration, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extraordinary_measures_duration, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of statutory_ceiling_vs_suspension. The upstream constraint (the choice of statutory ceiling rather than rolling authority) creates the need for extraordinary measures. This constraint (the duration those measures provide) is mechanically determined by fund balances and cash flow once the upstream choice is made. The two constraints have different extractiveness values: the upstream constraint's extractiveness reflects the political economy of the ceiling framework; this constraint's extractiveness reflects only the coordination overhead (or potential extraction) from the time buffer itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
