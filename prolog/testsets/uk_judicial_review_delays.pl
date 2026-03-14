% ============================================================================
% CONSTRAINT STORY: uk_judicial_review_delays
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_judicial_review_delays, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_judicial_review_delays
 *   human_readable: UK Judicial Review Delays and Access to Justice
 *   domain: legal/administrative_justice
 *
 * SUMMARY:
 *   UK judicial review delays represent a structural extraction mechanism
 *   disguised as resource scarcity. Applicants challenging government
 *   decisions wait 12-24 months for hearings while the challenged decisions
 *   remain in effect, extracting compliance from applicants and rendering
 *   eventual relief moot in many cases. The constraint is a tangled rope: it
 *   genuinely coordinates the flow of challenges through a finite court
 *   system (rope function), but it does so asymmetrically, benefiting
 *   government defendants and well-resourced repeat players while trapping
 *   powerless applicants (extraction function). The delay has persisted
 *   despite decades of criticism, multiple official reports recommending
 *   reform, and evidence that comparable common-law jurisdictions (Australia,
 *   Canada) process cases 50-75% faster using similar legal frameworks. This
 *   persistence suggests the constraint is sustained not by inherent scarcity
 *   but by political economy factors: government defendants have structurally
 *   weak incentives to fund courts faster (slow review protects their
 *   policies), while applicants lack collective power to demand change. The
 *   constraint exhibits all six DR types depending on perspective: snare for
 *   trapped applicants, rope for government beneficiaries, tangled rope for
 *   legal representatives, scaffold for reform-oriented judicial actors,
 *   piton for the right-to-review doctrine increasingly performative in
 *   practice, and false mountain for observers who naturalize policy choices
 *   as immutable laws.
 *
 * KEY AGENTS:
 *   - Applicants challenging government decisions (powerless/trapped): Primary victims bearing full extraction cost of delayed relief
 *   - Government defendants and public bodies (institutional/arbitrage): Primary beneficiaries extracting compliance during delay window; can exit at will
 *   - Legal representatives and access-to-justice NGOs (moderate/constrained): Mixed position — benefit from billable delay while bearing costs of client dissatisfaction and case overflow
 *   - Judicial office holders and courts reform coalition (organized/mobile): Reform-oriented institutional actors seeing the delay as solvable infrastructure problem with sunset logic
 *   - Judiciary as custodian of rule-of-law doctrine (institutional/arbitrage): Maintains performative commitment to judicial review right while structural barriers suppress its exercise
 *   - Government as funding authority (institutional/arbitrage): Controls court capacity budget; weak incentive to fund faster review of its own decisions; political choice to underfund appears as resource constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_judicial_review_delays, 0.58).
domain_priors:suppression_score(uk_judicial_review_delays, 0.65).
domain_priors:theater_ratio(uk_judicial_review_delays, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_judicial_review_delays, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_judicial_review_delays, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_judicial_review_delays, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_judicial_review_delays, tangled_rope).
narrative_ontology:human_readable(uk_judicial_review_delays, "UK Judicial Review Delays and Access to Justice").
narrative_ontology:topic_domain(uk_judicial_review_delays, "legal/administrative_justice").

domain_priors:requires_active_enforcement(uk_judicial_review_delays).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_judicial_review_delays, government_defendants).
narrative_ontology:constraint_beneficiary(uk_judicial_review_delays, high_volume_administrators).
narrative_ontology:constraint_victim(uk_judicial_review_delays, applicants_challenging_decisions).
narrative_ontology:constraint_victim(uk_judicial_review_delays, access_to_justice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: APPLICANT (SNARE) — Individuals challenging government decisions face 12-24 month delays while waiting for hearing dates. Cannot exit the system if they need justice on the original administrative decision. Extraction is maximal: delays render relief meaningless (decision already implemented, person already harmed). High suppression — applicants cannot bypass judicial review or access faster mechanisms; they are trapped in a queue with no alternatives.
constraint_indexing:constraint_classification(uk_judicial_review_delays, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT DEFENDANTS (ROPE) — Ministry departments, local authorities, and public bodies benefit from delays. Slow review means: (1) challenged decisions remain in effect longer, extracting compliance from applicants; (2) decisions become harder to unwind if eventually quashed (sunk costs, reliance, changed circumstances); (3) political pressure to reverse bad decisions dissipates over time. From the government's perspective, the delay is a coordination mechanism — it manages the flow of challenges and prevents cascade reviews that would destabilize policy implementation. Government can exit at any time by resolving the underlying case; delay works in their favor. Low or negative experienced extraction from this position.
constraint_indexing:constraint_classification(uk_judicial_review_delays, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL REPRESENTATIVES (TANGLED ROPE) — Solicitors and barristers benefit from the billable hours the delay creates; NGOs benefit from high caseload demonstrating unmet justice need (funding lever). But they also bear costs: client dissatisfaction, burnout from managing expectations, opportunity cost of cases that settle or become moot. They experience both coordination function (managing complex litigation) and extraction (the system's delay structure makes their work necessary and lucrative while preventing resolution). Constrained exit — they could leave practice but face reputational and financial costs.
constraint_indexing:constraint_classification(uk_judicial_review_delays, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Judicial office holders, court administrators, rule committees, and law reform bodies see the delay as a temporary coordination failure solvable through institutional change: remote hearings, case management, legal aid reform, and court staffing increases. These are structural fixes with sunset logic — they are not permanent constraints but surmountable infrastructure gaps. The coalition has agency (ability to implement procedural reforms) and sees an exit path. Low theater because the proposed fixes address the root cause (capacity shortage), not symptoms.
constraint_indexing:constraint_classification(uk_judicial_review_delays, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL REVIEW DOCTRINE (PITON) — The principle that all administrative decisions are justiciable appears in case law and constitutional theory as a fundamental rule of law guarantee. Yet the system's actual implementation (delays, cost barriers, permission stage) has become largely performative. Courts perform the right while structural barriers suppress its exercise. The doctrine persists through institutional inertia despite being undermined by the delay mechanism. Theater is moderate-to-low because some applicants do eventually get hearings, so the system maintains appearance of function while delivering access mainly to well-resourced repeat players.
constraint_indexing:constraint_classification(uk_judicial_review_delays, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a civilizational perspective, some judicial review delay may appear inherent to the rule of law: reviewing government decisions requires resources, expertise, and time; no system can process all challenges instantaneously. The delay appears as a natural constraint — a consequence of finite judicial capacity meeting infinite claims. However, the structural data contradicts the mountain classification. The UK's delay is 12-24 months while other common-law systems (Australia, Canada) process cases in 6-9 months using similar legal frameworks. The bottleneck is not inherent to judicial review but to underfunded courts in the UK. The mountain classification naturalizes a policy choice (not funding courts adequately) as a law of nature.
constraint_indexing:constraint_classification(uk_judicial_review_delays, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_judicial_review_delays_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_judicial_review_delays, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_judicial_review_delays, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_judicial_review_delays, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_judicial_review_delays, TR),
    TR >= 0.70.

:- end_tests(uk_judicial_review_delays_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The delay mechanism extracts from applicants in three ways: (1) compliance extraction during waiting period (challenged decisions remain in effect, applicants must continue complying); (2) relief erosion (even successful reviews often come too late to undo implemented decisions, changing circumstances, reliance interests); (3) cost multiplication (extended litigation increases legal bills, client stress, opportunity costs). The extraction is not maximal because some applicants succeed, some cases settle favorably, and the system retains surface legitimacy. Suppression (0.65): Moderate-high. Barriers to exit include: no alternative faster remedy for judicial review (ombudsmen and appeals handle narrower classes of decisions); cost barriers (judicial review requires legal representation, exceeds financial capacity of poor applicants); time barriers (applicants cannot wait 2 years without harm); information barriers (applicants often don't understand judicial review or think it's available to them). Theater ratio (0.48): Moderate. The system performs judicial review — hearings occur, decisions are issued, some are quashed — but increasingly the performance masks implementation that has already occurred. Theater has risen over the interval as delays have lengthened, making the ritual further from functional reality. The bottleneck manifests as court capacity constraints (genuine) but is sustained as a policy choice (government discretion not to fund courts adequately). This hybrid nature — real but chosen — produces moderate rather than high theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint displays a large perspectival gap between the applicant's experience (snare) and the government defendant's experience (rope). For applicants, the delay is pure extraction — harmful and inescapable. For government, the delay is coordination — it manages the flow of challenges in a way that protects policy stability. This gap is not a measurement error; it reflects the constraint's hybrid nature. It is both a coordination mechanism (managing cases through courts) and an extraction mechanism (benefiting powerful repeat players). The gap also emerges between the judicial reform coalition (which sees the constraint as temporary, solvable infrastructure problem — scaffold) and the applicant (which sees it as permanent structural entrapment — snare). This gap is diagnostically significant: the scaffold perspective requires that the power to reform exists and is mobilizing. If reform is blocked (omega variable 5: political inertia), the scaffold collapses back to snare. The mountain perspective (scarcity as natural law) is a false summit — evidence from peer systems shows the delay is not inherent to judicial review but to a specific policy choice regarding court funding.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's effective extraction varies dramatically by structural position. For powerless applicants trapped in the system (d ≈ 0.95 → f(d) ≈ 1.42), experienced extraction is maximal. For institutional government defendants with arbitrage options (d ≈ 0.10 → f(d) ≈ -0.01), experienced extraction is negative (they benefit). The scope modifier (national, σ = 1.0) applies equally across all perspectives. The applicant's high f(d) reflects that a powerless trapped agent experiences high extraction from any extractive mechanism. The government's negative f(d) reflects that a beneficiary with exit options experiences the constraint as beneficial rather than burdensome. Legal representatives occupy the center (d ≈ 0.55 → f(d) ≈ 0.65): constrained but organized, so experienced extraction is moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the apparent scarcity (not enough courts) is a policy choice (not enough court funding) rather than a natural law. The false mountain classification (scarcity appears inevitable) masks the extraction mechanism: by framing the delay as inevitable, stakeholders avoid debate about alternative funding mechanisms, prioritization choices, and the specific beneficiaries of the status quo. Mandatrophy resolution requires distinguishing between (1) genuine resource scarcity (impossible to serve all claims fast), and (2) policy-chosen underfunding (possible to serve claims faster with different budget priorities). The international benchmarking (Australia, Canada process cases faster) demonstrates that the UK delay is not inherent to judicial review but to a specific policy choice. The tangled rope classification captures this precisely: the delay coordinates case flow (rope function) while asymmetrically extracting from powerless applicants (snare function). The constraint would collapse toward pure rope if delays were minimized (faster resolution benefits all parties by reducing uncertainty and cost), revealing that the hybrid nature is sustained by political factors, not functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    underlying_decision_implementation_rate,
    'What proportion of challenged decisions remain substantially unchanged (as implemented) by the time judicial review concludes, rendering relief moot even if successful?',
    'Longitudinal tracking of judicial review outcomes: measure relief granted vs implementation that cannot be reversed; correlation between delay length and mootness rate',
    'If mootness > 40%: delay is the extraction mechanism itself, not a side effect. If mootness < 20%: relief remains meaningful; delay is primarily a coordination/fairness problem, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(underlying_decision_implementation_rate, empirical, 'Proportion of successful reviews where relief is moot due to implementation during delay').

omega_variable(
    repeat_player_asymmetry_magnitude,
    'Do government repeat players and well-resourced defendants navigate the delay asymmetrically compared to one-shot applicants and self-represented parties?',
    'Comparative analysis of outcomes by party type: success rates, delay impact, strategic use of postponements; measurement of legal representation correlation with outcomes',
    'If asymmetry is structural: judicial review is stratified extraction (snare for powerless, rope for institutional). If asymmetry is minimal: delay is a neutral inefficiency affecting all parties equally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repeat_player_asymmetry_magnitude, empirical, 'Magnitude of asymmetric advantage for repeat players in delay navigation').

omega_variable(
    alternative_remedy_substitution_feasibility,
    'Can ombudsman, administrative appeal, or statutory review mechanisms substitute for judicial review as a faster route to decision review?',
    'Comparative analysis of coverage: which government decisions are subject to ombudsman vs judicial review only; outcome rates and speed by mechanism; applicant awareness and access to alternatives',
    'If substitution feasible: judicial review delay affects a narrower population; constraint is more snare-like for those truly trapped. If no substitutes exist: all applicants are trapped; extraction is wider.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_remedy_substitution_feasibility, empirical, 'Whether alternative remedies can substitute for delayed judicial review').

omega_variable(
    court_funding_counterfactual_capacity,
    'How much additional judicial capacity (judges, hearing facilities, staff) would reduce delay to 6-9 months (comparable to other common-law systems)?',
    'Benchmarking analysis: resource ratios in UK courts vs Australia/Canada; case-to-judge ratios; cost modeling of capacity expansion scenarios',
    'If modest investment sufficient: mountain classification false — delay is a policy choice, not a scarcity law. If massive investment required: delay may reflect genuine resource constraints at the societal level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(court_funding_counterfactual_capacity, empirical, 'Investment required to match international peer justice system speeds').

omega_variable(
    political_pressure_and_reform_inertia,
    'Why, despite decades of criticism and multiple official reports calling for reform, have judicial review delays persisted unchanged? What structural factors sustain the constraint despite explicit advocacy for removal?',
    'Policy analysis: track reform proposals, government responses, and implementation barriers; identify beneficiaries of status quo who resist change; analysis of political economy of court funding',
    'If inertia is institutional capture (government resists funding that would speed its own review): constraint is snare dressed as mountain. If inertia is genuine resource competition: constraint reflects real scarcity but is still removable by policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_pressure_and_reform_inertia, conceptual, 'Political economy of reform resistance despite critical consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_judicial_review_delays, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukjr_tr_t0, uk_judicial_review_delays, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ukjr_tr_t10, uk_judicial_review_delays, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ukjr_tr_t20, uk_judicial_review_delays, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ukjr_be_t0, uk_judicial_review_delays, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ukjr_be_t10, uk_judicial_review_delays, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ukjr_be_t20, uk_judicial_review_delays, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_judicial_review_delays, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_judicial_review_delays, rule_of_law_performativity).
narrative_ontology:affects_constraint(uk_judicial_review_delays, administrative_law_access).

% DUAL FORMULATION NOTE:
% UK judicial review delays are structurally linked to the broader constraint of rule-of-law performativity (maintaining the appearance of judicial oversight while structural barriers suppress actual enforcement) and the access-to-justice constraint (preventing ordinary citizens from vindicating legal rights due to cost, time, and information barriers). These three constraints form a family reflecting different scales of the same extraction mechanism: individual access gaps (this constraint), institutional performance gaps (rule-of-law shadow), and systemic justice stratification (access constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_judicial_review_delays, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
