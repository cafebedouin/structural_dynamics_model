% ============================================================================
% CONSTRAINT STORY: statutory_ceiling_vs_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_ceiling_vs_suspension, []).

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
 *   constraint_id: statutory_ceiling_vs_suspension
 *   human_readable: Statutory Ceiling vs Suspension Mechanism in Federal Debt Authorization
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The U.S. federal debt limit can be implemented via two distinct statutory
 *   mechanisms: a fixed dollar ceiling (e.g., $41.1 trillion) that remains in
 *   effect until amended, or a suspension that lifts the limit entirely until
 *   a specified date, after which the limit resets to accommodate all
 *   borrowing during the suspension period. This constraint examines whether
 *   the choice between these mechanisms is a neutral legislative drafting
 *   decision (Mountain) or embeds extractive political dynamics. The base
 *   metrics reflect the legal-technical structure: very low extractiveness
 *   (0.08) because the mechanism choice itself extracts minimally from most
 *   agents; very low suppression (0.12) because alternatives to both
 *   mechanisms exist and are not suppressed by the ceiling-vs-suspension
 *   choice; low theater ratio (0.15) because both mechanisms are functional
 *   implementations of the constitutional authorization requirement, though
 *   the ceiling mechanism has accumulated some performative overhead
 *   (extraordinary measures, X-date projections) that the suspension
 *   mechanism eliminates. The constraint is claimed as Mountain with declared
 *   beneficiaries (Treasury operations, Congressional committees) to trigger
 *   False Summit Mountain evaluation — the question is whether the
 *   operational efficiency gains and legislative control preservation are
 *   genuine coordination benefits or whether they mask extraction.
 *
 * KEY AGENTS:
 *   - Individual Taxpayer: Powerless/trapped — no exit from federal fiscal system, negligible extraction from mechanism choice
 *   - Bond Market Participant: Moderate/constrained — can shift to other sovereign debt at cost, mechanism choice is operationally invisible at security level
 *   - Treasury Operations Office: Institutional/arbitrage — primary beneficiary of suspension mechanism (eliminates extraordinary measures choreography), but benefit is operational efficiency rather than extraction; cannot choose which mechanism Congress enacts
 *   - Congressional Appropriations Committees: Institutional/constrained — beneficiary in sense that mechanism choice is within constitutional authority and both preserve legislative control; cannot exit authorization requirement
 *   - Analytical Observer: Analytical/analytical — sees the ceiling-vs-suspension choice as a parameter within the constitutional requirement for legislative authorization of borrowing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_ceiling_vs_suspension, 0.08).
domain_priors:suppression_score(statutory_ceiling_vs_suspension, 0.12).
domain_priors:theater_ratio(statutory_ceiling_vs_suspension, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_ceiling_vs_suspension, mountain).
narrative_ontology:human_readable(statutory_ceiling_vs_suspension, "Statutory Ceiling vs Suspension Mechanism in Federal Debt Authorization").
narrative_ontology:topic_domain(statutory_ceiling_vs_suspension, "public_finance/constitutional_law/political_economy").

domain_priors:emerges_naturally(statutory_ceiling_vs_suspension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_ceiling_vs_suspension, treasury_operations_office).
narrative_ontology:constraint_beneficiary(statutory_ceiling_vs_suspension, congressional_appropriations_committees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL TAXPAYER (MOUNTAIN) — Whether Congress authorizes debt via a fixed dollar ceiling or a time-bounded suspension is a technical legislative choice with no direct extraction from this agent. The taxpayer cannot exit the federal fiscal system and has no power to influence the mechanism choice, but experiences negligible extraction from the choice itself. The constraint is the legal-technical requirement that Congress authorize borrowing in some form — the specific form (ceiling vs suspension) is immaterial to this agent's structural position.
constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BOND MARKET PARTICIPANT (MOUNTAIN) — Treasury debt instruments are fungible regardless of whether authorized via ceiling or suspension. The market participant faces the same credit risk, the same liquidity, the same legal enforceability. The mechanism difference is operationally invisible at the security level. Constrained exit (can shift to other sovereign debt markets at cost) but negligible extraction from the mechanism choice itself.
constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY OPERATIONS OFFICE (MOUNTAIN) — Primary beneficiary of the suspension mechanism (eliminates daily debt-limit monitoring and extraordinary measures choreography during the suspension window) but the benefit is operational efficiency, not extraction. The Treasury cannot choose which mechanism Congress enacts and must operate under either. The constraint is the constitutional requirement that Congress authorize borrowing — the specific statutory form is a parameter within that requirement. Arbitrage exit in the sense that Treasury has discretion over issuance timing and maturity structure, but no exit from the authorization requirement itself.
constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL APPROPRIATIONS COMMITTEES (MOUNTAIN) — Beneficiary in the sense that the mechanism choice is within Congress's constitutional authority and both mechanisms preserve legislative control over borrowing authorization. But the choice between ceiling and suspension is a coordination decision about how to structure that control, not an extraction mechanism. The committees cannot exit the constitutional requirement to authorize borrowing. The constraint is the Article I requirement itself; the ceiling-vs-suspension choice is a degree of freedom within that constraint.
constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The legal-technical difference between a fixed dollar ceiling and a time-bounded suspension is a structural feature of how statutory authorization can be specified. Both mechanisms implement the same constitutional requirement (Congressional control over federal borrowing per Article I Section 8). The choice between them is a legislative drafting decision with operational consequences (ceiling requires periodic adjustment; suspension requires periodic renewal) but negligible extraction. The constraint is the requirement for legislative authorization; the form of that authorization is a parameter. Alternatives (no debt limit, constitutional amendment, premium bond authority) exist but are not suppressed by the ceiling-vs-suspension choice itself — they are suppressed by the broader political economy of fiscal governance.
constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_ceiling_vs_suspension_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_ceiling_vs_suspension, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, ExtMetricName, E),
    domain_priors:suppression_score(statutory_ceiling_vs_suspension, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_ceiling_vs_suspension),
    narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_ceiling_vs_suspension, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_ceiling_vs_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The legal-technical difference between a fixed dollar ceiling and a time-bounded suspension is a structural feature of how statutory authorization can be specified. Both mechanisms implement the same constitutional requirement. The choice between them has operational consequences (ceiling requires periodic adjustment and triggers extraordinary measures; suspension requires periodic renewal but eliminates intra-period monitoring) but does not systematically extract from any agent class. Treasury gains operational efficiency under suspension, but this is a coordination benefit (reduced administrative overhead) rather than extraction. The modest extractiveness reflects that the ceiling mechanism has historically enabled some political leverage dynamics (brinksmanship, policy concessions) that the suspension mechanism reduces, but these dynamics are not intrinsic to the mechanism choice itself — they are features of the broader political economy of fiscal governance. Suppression (0.12): Very low. Alternatives to both mechanisms exist (no statutory debt limit, constitutional amendment requiring balanced budgets, premium bond authority, consol issuance) and are not suppressed by the ceiling-vs-suspension choice. The suppression that does exist is at the level of the authorization requirement itself (Congress must authorize borrowing per Article I Section 8), not at the level of the mechanism choice. Theater ratio (0.15): Low but rising. Both mechanisms are functional implementations of the authorization requirement. The ceiling mechanism has accumulated some performative overhead: Treasury's extraordinary measures (suspending CSRDF/ESF investments, debt issuance suspension periods) and X-date projections are partly theater — the measures are real but the crisis framing is performative when the outcome (Congressional action before default) is structurally overdetermined. The suspension mechanism eliminates this theater by removing the intra-period monitoring requirement. The rising trajectory reflects increasing politicization of debt limit episodes, but the base level remains low because the core function (legislative authorization) is genuine. Accessibility collapse (0.92): Very high. Once the constitutional requirement for legislative authorization is understood, the space of alternatives collapses almost completely. Congress must authorize borrowing in some form; the choice is only how to structure that authorization. Resistance (0.08): Very low. The requirement meets negligible resistance because it is a constitutional mandate, not a policy choice. The mechanism choice (ceiling vs suspension) meets some resistance (political preference for one form over the other) but this is coordination friction, not resistance to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify as Mountain, which is the expected outcome for a genuine structural feature of legislative authorization. The perspectival invariance is the signal: if the constraint were extractive, we would expect divergence between the powerless/trapped perspective (Individual Taxpayer) and the institutional/arbitrage perspectives (Treasury, Congress). The absence of divergence suggests the mechanism choice is structurally neutral. However, the False Summit Mountain detector will evaluate whether the declared beneficiaries (Treasury, Congressional committees) indicate that the 'neutral technical choice' framing is naturalizing a contingent institutional arrangement that benefits specific actors. The three omega variables document the irreducible uncertainties: (1) Is the mechanism choice genuinely neutral or does it embed extractive political dynamics? (2) Does Treasury's efficiency gain from suspension mask reduced legislative oversight? (3) Does the ceiling mechanism enable extractive brinksmanship that the suspension mechanism eliminates? These are empirical questions resolvable through historical analysis of mechanism adoption patterns, fiscal outcomes, and political leverage distributions.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury Operations Office is declared as a beneficiary because suspension eliminates the extraordinary measures choreography and daily debt-limit monitoring that ceiling regimes require. This is an operational efficiency gain. Congressional Appropriations Committees are declared as beneficiaries because both mechanisms preserve legislative control over borrowing authorization, and the choice between them is within Congress's constitutional authority. However, these benefits are coordination gains (reduced administrative overhead, preserved constitutional authority) rather than extraction. The engine will derive low directionality values (d near 0.0) for both beneficiaries, producing low or negative effective extraction (chi). No victims are declared because the mechanism choice itself does not systematically extract from any agent class. The modest base extractiveness (0.08) reflects historical political leverage dynamics under ceiling regimes, but these are not intrinsic to the legal-technical structure of the mechanism choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a candidate False Summit Mountain. The legal-technical structure (fixed dollar ceiling vs time-bounded suspension) is a genuine parameter within the constitutional requirement for legislative authorization of borrowing. Both mechanisms implement the same Article I Section 8 mandate. The choice between them has operational consequences but appears structurally neutral — no agent class is systematically extracted from by the mechanism choice itself. However, the declaration of beneficiaries (Treasury operations, Congressional committees) triggers the False Summit detector because it raises the question: if the choice is truly neutral, why do specific actors benefit? The resolution depends on whether the benefits are coordination gains (operational efficiency, preserved constitutional authority) or extraction (reduced oversight, political leverage). The omega variables document this ambiguity. If historical analysis shows that suspension regimes correlate with reduced legislative scrutiny of Treasury debt management decisions, or that ceiling regimes systematically enable extractive brinksmanship dynamics, the Mountain classification would be false — the constraint would reclassify as Tangled Rope (coordination + extraction) or Snare (pure extraction via political leverage). If outcomes are statistically indistinguishable, the Mountain classification holds and the beneficiaries are genuine coordination beneficiaries rather than extractors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_choice_naturalness,
    'Is the ceiling-vs-suspension distinction a genuine structural feature of legislative authorization (Mountain) or does the choice itself embed extractive political dynamics that benefit specific actors?',
    'Historical analysis of mechanism adoption patterns: do ceiling regimes correlate with different fiscal outcomes, political leverage distributions, or crisis frequencies than suspension regimes? If the choice is neutral, outcomes should be statistically indistinguishable. If extractive, specific actors should systematically benefit from one mechanism over the other.',
    'If the mechanism choice is extractive: reclassify as Tangled Rope (coordination function of authorization + asymmetric extraction from mechanism choice). If neutral: Mountain classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_choice_naturalness, empirical, 'Whether the ceiling-vs-suspension choice is structurally neutral or embeds extraction').

omega_variable(
    beneficiary_structure_ambiguity,
    'Does Treasury''s operational efficiency gain from suspension (eliminating extraordinary measures choreography) constitute a coordination benefit or does it mask extraction by reducing legislative oversight friction?',
    'Comparative analysis of Treasury discretion and debt management outcomes under ceiling vs suspension regimes. If suspension correlates with reduced legislative scrutiny of specific issuance decisions or maturity structure choices, the efficiency gain may be covering extraction. If outcomes are equivalent, the gain is pure coordination.',
    'If Treasury discretion under suspension enables extractive debt management: reclassify Treasury perspective as Rope (beneficiary of reduced oversight). If discretion is neutral: Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether Treasury''s efficiency gain from suspension is coordination or reduced oversight').

omega_variable(
    political_leverage_asymmetry,
    'Does the ceiling mechanism create extractive political leverage (hostage-taking dynamics, extraordinary measures theater) that the suspension mechanism eliminates, or are both mechanisms equally vulnerable to political extraction?',
    'Event study of debt limit episodes: frequency and severity of brinksmanship, market disruption, and policy concessions extracted under ceiling vs suspension regimes. If ceiling episodes systematically produce larger policy concessions or market volatility, the mechanism choice itself is extractive. If equivalent, both are neutral implementations of the authorization requirement.',
    'If ceiling mechanism enables systematic extraction via brinksmanship: the constraint is not Mountain but Snare from the perspective of agents targeted by the extracted concessions. If both mechanisms are equally vulnerable: Mountain holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_leverage_asymmetry, empirical, 'Whether ceiling mechanism enables extractive brinksmanship dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_ceiling_vs_suspension, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1990, statutory_ceiling_vs_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(theater_2000, statutory_ceiling_vs_suspension, theater_ratio, 10, 0.12).
narrative_ontology:measurement(theater_2010, statutory_ceiling_vs_suspension, theater_ratio, 20, 0.15).
narrative_ontology:measurement(theater_2020, statutory_ceiling_vs_suspension, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(extract_1990, statutory_ceiling_vs_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(extract_2000, statutory_ceiling_vs_suspension, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(extract_2010, statutory_ceiling_vs_suspension, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(extract_2020, statutory_ceiling_vs_suspension, base_extractiveness, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_ceiling_vs_suspension, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a component within the broader federal debt authorization system. Upstream constraints include the constitutional requirement for legislative authorization of borrowing (Article I Section 8) and the political economy of fiscal governance. Downstream constraints include Treasury debt management operations and bond market liquidity dynamics. The ceiling-vs-suspension choice is a parameter within the authorization system, not the system itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
