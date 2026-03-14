% ============================================================================
% CONSTRAINT STORY: means_testing_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_means_testing_extraction, []).

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
 *   constraint_id: means_testing_extraction
 *   human_readable: Means Testing Extraction in Welfare Administration
 *   domain: social_policy/economic_governance
 *
 * SUMMARY:
 *   Means testing in welfare administration creates a structural tension
 *   between the legitimate coordination function of targeting limited
 *   resources to those most in need and the extractive mechanism of
 *   bureaucratic verification, benefit phase-out, and stigma-based exclusion.
 *   The constraint exhibits all six DR types from different perspectives,
 *   making it a diagnostic exemplar for how institutional extraction operates
 *   through seemingly neutral administrative design. The same structural
 *   phenomenon — the verification of income and assets to determine benefit
 *   eligibility — appears as an immutable economic law (scarcity requires
 *   rationing), a coordination mechanism (targeting prevents waste), a
 *   temporary problem being solved by universal basic income (sunset
 *   perspective), a degraded principle inverted from universal to
 *   means-tested (piton), a mixed coordination-extraction hybrid (tangled
 *   rope from multiple perspectives), or pure extraction (snare from the
 *   trapped beneficiary). The extractiveness trajectory (0.38 → 0.58)
 *   reflects the accumulation of administrative burden over policy cycles and
 *   the intensification of means-test design as fiscal pressure increases.
 *   The theater ratio trajectory (0.45 → 0.61) reflects the growing
 *   performative justification of means testing as genuine fiscal efficiency
 *   as alternative mechanisms (UBI pilots) emerge to challenge its necessity.
 *
 * KEY AGENTS:
 *   - Low-Income Beneficiaries: Primary victim (powerless/trapped) — bear full cost of verification burden, stigma, and poverty trap effects; cannot exit without abandoning income support
 *   - Marginally-Employed Workers: Secondary victim (moderate/constrained) — face high implicit marginal tax rates and benefit clawback; constrained by cost of formal employment vs. informal economy trade-offs
 *   - Welfare Administration: Primary beneficiary (institutional/arbitrage) — gains administrative clarity, political cover, and operational efficiency through means testing; can modify policy unilaterally
 *   - Fiscal Gatekeeper (Treasury/Budgeting): Secondary beneficiary (institutional/constrained) — benefits from reduced expenditure through targeting and phase-out; constrained by fiscal pressures and political opposition
 *   - Anti-Poverty Coalition: Organized agent (organized/constrained) — advocates for UBI and simplified eligibility; sees means testing as temporary problem with sunset pathway
 *   - Administrative Staff (Case Workers): Intermediate actor (moderate/constrained) — exercise discretionary power in eligibility decisions; may amplify extraction through discriminatory practices or alleviate it through sympathetic interpretation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice (means testing) as inevitable economic law (scarcity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(means_testing_extraction, 0.58).
domain_priors:suppression_score(means_testing_extraction, 0.68).
domain_priors:theater_ratio(means_testing_extraction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(means_testing_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(means_testing_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(means_testing_extraction, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(means_testing_extraction, tangled_rope).
narrative_ontology:human_readable(means_testing_extraction, "Means Testing Extraction in Welfare Administration").
narrative_ontology:topic_domain(means_testing_extraction, "social_policy/economic_governance").

domain_priors:requires_active_enforcement(means_testing_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(means_testing_extraction, administrative_apparatus).
narrative_ontology:constraint_beneficiary(means_testing_extraction, higher_income_taxpayers).
narrative_ontology:constraint_victim(means_testing_extraction, low_income_beneficiaries).
narrative_ontology:constraint_victim(means_testing_extraction, program_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEANS-TESTED RECIPIENT (SNARE) — Trapped in recursive verification cycles. Earning income triggers benefit loss; applying for benefits triggers invasive documentation requirements; non-compliance triggers penalties. The constraint extracts time, dignity, and psychological cost with no meaningful exit path. Maximum suppression through bureaucratic friction and stigma.
constraint_indexing:constraint_classification(means_testing_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALLY-EMPLOYED WORKER (TANGLED ROPE) — Constrained by high implicit marginal tax rates (benefit clawback on earned income). The means test coordinates income verification (genuine coordination function) while simultaneously extracting through phase-out design and administrative burden. Moderate extraction; some agency through informal economy options but constrained by legality and penalties.
constraint_indexing:constraint_classification(means_testing_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE ADMINISTRATION (ROPE) — Experiences means testing as coordination infrastructure: verifying income prevents fraud, targets resources to those most in need, and fulfills legislative intent. Net beneficiary — the institution gains administrative clarity and political cover for resource targeting. Low experienced extraction; institutional power enables arbitrage (can modify policy unilaterally).
constraint_indexing:constraint_classification(means_testing_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL GATEKEEPER (TANGLED ROPE) — Constrained by competing fiscal pressures and political opposition to welfare spending. Means testing serves genuine coordination function (targeting limited funds to greatest need) while enabling extraction through reduction of overall program expenditure. The constraint extracts from future beneficiaries (eligibility tightens over time) while coordinating current fiscal discipline. Institutional power but constrained exit — cannot abandon means testing without political cost.
constraint_indexing:constraint_classification(means_testing_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-POVERTY COALITION (SCAFFOLD) — Organized agents (NGOs, advocacy groups, sympathetic policymakers) see means testing as a temporary coordination failure being solved through universal basic income pilots, simplified eligibility processes, and automatic enrollment. Low effective extraction because the coalition has agency and sees an exit pathway (removing means testing entirely). The sunset is generational — as pilot data accumulates and political consensus shifts, means-test dependent administration is expected to be phased out.
constraint_indexing:constraint_classification(means_testing_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIVERSAL ELIGIBILITY PRINCIPLE (PITON) — The original principle of universal welfare (universality to reduce stigma and maximize take-up) has been inverted into means testing ostensibly for fiscal discipline. However, the performative maintenance of fiscal virtue through means testing persists even as evidence mounts that administrative costs and behavioral distortion reduce net benefit delivery. Theater ratio (0.61) reflects that much of the 'targeting efficiency' justification is narrative cover for extraction — the actual fiscal savings are often less than the administrative cost. The institution maintains the means-test ritual through inertia despite degraded function.
constraint_indexing:constraint_classification(means_testing_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a civilizational perspective, some rationing mechanism is inherent to any resource allocation system with budget constraints. Means testing appears as a natural law: given finite resources and multiple claimants, some discrimination is inevitable. However, the structural data reveals this as false naturalization — unlimited eligibility combined with automatic delivery (universal basic income, universal basic services) represents an alternative coordination mechanism that has not been tried at scale. The mountain classification conceals contingent institutional choices as immutable law.
constraint_indexing:constraint_classification(means_testing_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(means_testing_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(means_testing_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(means_testing_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(means_testing_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(means_testing_extraction, TR),
    TR >= 0.70.

:- end_tests(means_testing_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The means test extracts through multiple mechanisms: (1) benefit phase-out reduces net income for working beneficiaries, (2) administrative burden creates time and psychological cost, (3) stigma and identity effects create self-exclusion, (4) non-compliance penalties impose direct costs. However, extraction is not maximal (0.72+) because the coordination function is genuine — preventing waste and targeting are legitimate administrative goals. The value reflects that extraction is embedded within a genuinely functional system, not a pure predatory mechanism. Suppression (0.68): High. Significant barriers to exit include economic dependency on benefits (trapped), career penalties for benefit receipt (constrained), and internalized shame (identity_locked for some agents). Documentation requirements, repeated applications, and surveillance create continuous friction. The suppression is both structural (bureaucratic barriers) and partially internalized (stigma that persists post-exit). Theater ratio (0.61): Moderate-high. Much of the 'fiscal efficiency' justification for means testing is performative. The narrative of 'targeting to prevent waste' serves real political functions (justifying reduced spending) while obscuring that administrative overhead and behavioral distortion often exceed savings. The theater has increased over time as alternative mechanisms (UBI pilots) emerge, forcing greater rhetorical defense of means testing.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless beneficiary's snare classification and the institutional administrator's rope classification measures the asymmetric extraction built into the system. A trapped agent with no exit options experiences the same structural mechanisms (income verification, benefit targeting) as high extraction because they cannot access the arbitrage opportunities the institutional agent can exercise. The institutional agent experiences coordination because they have power to modify the system. The marginally-employed worker's tangled rope reflects their intermediate position: they benefit from the coordination function (formal income verification enables benefit receipt) but suffer from extraction (phase-out reduces work incentives). The anti-poverty coalition's scaffold reflects organized agency and a visible exit pathway (UBI adoption), which reduces experienced extraction despite the same base constraints. The piton perspective captures the phenomenon of institutional inertia — the original principle of universal eligibility has been inverted into means testing, but the institution continues performing verification theater even as evidence mounts that administrative costs exceed savings. The mountain perspective at the analytical level naturalizes what are contingent institutional choices, which the engine's false summit detector should identify.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of d for each perspective follows from beneficiary/victim status and exit options. Welfare administration and fiscal gatekeepers are beneficiaries with institutional power and exit options (they can modify policy), yielding low d (≈0.15-0.20 for institutional/arbitrage), producing negative or near-zero f(d) — experienced extractiveness is low or neutral because these agents control the mechanism. Trapped beneficiaries are victims with no exit, yielding high d (≈0.95), producing maximum f(d) ≈1.42 — experienced extractiveness is amplified because they bear costs with no escape. Marginally-employed workers are victims with constrained exit (can work informally but risk legality/penalties), yielding moderate-high d (≈0.70-0.80), producing moderate f(d) ≈1.15 — experienced extractiveness is significant because costs are high and exit is costly. Anti-poverty coalition members are organized agents with constrained but real exit (can build alternatives), yielding moderate d (≈0.60), producing moderate f(d) ≈0.85 — experienced extraction is reduced because agency exists. The analytical observer at civilizational scale derives d from the objective structural position (if framing means testing as natural law, they are passive observers, d≈0.70), but the false summit detector identifies the error.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how institutional extraction can be embedded within apparently neutral administrative design. The mandatrophy challenge is: 'Is means testing coordination (targeting efficiency) or extraction (benefit reduction)?'. The answer is perspectival and structural: It functions as coordination for agents with power and arbitrage capacity (welfare administration, fiscal gatekeepers) and as extraction for agents trapped in the system (low-income beneficiaries). The system is genuinely hybrid (Tangled Rope is the correct classification from neutral observation). However, the falsity of the mountain perspective (scarcity requires means testing) reveals that the system's structure is not inevitable — it is a contingent institutional choice among several alternatives (universal basic income, automatic enrollment, simplified universal eligibility). The constraint does not resolve to a single 'true' type; rather, it exemplifies how the same structural mechanism produces different classification outcomes based on agent position, power, and exit options. This is the core insight of deferential realism: constraints are not observer-independent; they are observer-position-relative. The mandatrophy is resolved by acknowledging that all six perspectives are locally accurate descriptions of agents' actual experiences, and the constraint's true nature is the presheaf of all six perspectives together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fraud_prevention_vs_exclusion_trade_off,
    'What level of fraud detection actually justifies the administrative burden and error exclusion (false negatives) created by means testing?',
    'Comparative empirical analysis: fraud rates under means testing vs. simplified/universal eligibility; false negative rates (eligible individuals excluded) vs. false positive rates (ineligible individuals included); total cost of fraud prevention vs. total cost of false negatives',
    'If fraud prevention value < exclusion cost: means testing is net-extractive design. If fraud prevention value > exclusion cost: means testing is legitimate targeting mechanism. If roughly equal: the choice between them is political rather than technical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fraud_prevention_vs_exclusion_trade_off, empirical, 'Fraud prevention justification vs. exclusion cost trade-off').

omega_variable(
    administrative_overhead_recapture,
    'Do fiscal savings from means-test-driven benefit reduction exceed or fall short of the administrative costs incurred in means testing?',
    'Full-cost accounting: administrative apparatus cost (personnel, systems, verification infrastructure) + behavioral costs (reduced take-up due to stigma, application burden, non-compliance penalties) vs. reduced benefit expenditure due to targeting and phase-out',
    'If costs exceed savings: means testing is pure extraction mechanism (Snare). If savings exceed costs: means testing is genuine efficiency mechanism (Rope). If roughly equal: the classification depends on whose behavioral costs are counted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_overhead_recapture, empirical, 'Administrative overhead vs. realized fiscal savings').

omega_variable(
    implicit_marginal_tax_rate_distortion,
    'What is the magnitude of labor supply distortion created by means-test-induced high implicit marginal tax rates (benefit clawback), and does this distortion exceed the coordination benefit of income verification?',
    'Labor supply elasticity estimation; comparison of work behavior under means testing vs. alternative eligibility mechanisms; earnings mobility analysis',
    'If distortion is severe (labor supply collapse, poverty trap amplification): means testing amplifies extraction (Snare dominates). If distortion is modest: means testing is mixed coordination-extraction (Tangled Rope holds). If distortion is reversed by earned-income incentives: means testing is net-beneficial (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_marginal_tax_rate_distortion, empirical, 'Labor supply distortion from marginal tax rates in means testing').

omega_variable(
    stigma_and_internalized_exclusion,
    'To what degree does the stigma of means testing (internalized as shame, unworthiness, or identity as ''welfare dependent'') constitute internalized suppression that persists independent of structural barriers?',
    'Qualitative analysis of beneficiary narratives; comparison of health/psychological outcomes under means testing vs. universal programs; longitudinal tracking of stigma-derived self-exclusion',
    'If internalized suppression is severe: the suppression metric should be classified as partially internalized, carrying greater persistence post-exit. If mild: suppression is primarily structural. If medium: suppression in the measurements is a composite of structural and internalized components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_and_internalized_exclusion, empirical, 'Internalized suppression and identity-based exclusion from means testing').

omega_variable(
    universal_basic_income_sunset_feasibility,
    'Is the scaffold perspective''s sunset (transition to UBI or universal basic services) structurally feasible given fiscal and political constraints, or is it aspirational narrative?',
    'Pilot outcome analysis from UBI trials; fiscal sustainability modeling under various tax and inflation scenarios; political coalition mapping for UBI adoption',
    'If feasible: scaffold classification holds, sunset is real, exit pathway exists. If infeasible: scaffold is aspirational, organized agents are identity-locked to a framing that cannot be realized. The means test persists indefinitely, and the classification reverts to snare/tangled rope from all but analytical positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_basic_income_sunset_feasibility, empirical, 'Feasibility of UBI-based exit from means testing').

omega_variable(
    bureaucratic_discretion_and_power_asymmetry,
    'To what degree do administrative staff (case workers, eligibility officers) exercise discretionary power in ways that amplify extraction or create clientelism?',
    'Case file analysis for discretionary decisions; comparative treatment analysis across sociodemographic groups; administrative appeal rates and success rates',
    'If discretion is high and unevenly distributed: extraction mechanism is amplified by interpersonal power dynamics. The constraint contains an embedded snare (beneficiary-staff dyad) nested within the institutional snare. If discretion is low (rule-based): the constraint is more purely institutional. If discretion is high but evenly distributed: the constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucratic_discretion_and_power_asymmetry, empirical, 'Bureaucratic discretion amplification of extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(means_testing_extraction, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(means_tr_t0, means_testing_extraction, theater_ratio, 0, 0.45).
narrative_ontology:measurement(means_tr_t7, means_testing_extraction, theater_ratio, 7, 0.54).
narrative_ontology:measurement(means_tr_t15, means_testing_extraction, theater_ratio, 15, 0.61).
narrative_ontology:measurement(means_tr_t22, means_testing_extraction, theater_ratio, 22, 0.67).

% Extraction over time
narrative_ontology:measurement(means_be_t0, means_testing_extraction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(means_be_t7, means_testing_extraction, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(means_be_t15, means_testing_extraction, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(means_be_t22, means_testing_extraction, base_extractiveness, 22, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(means_testing_extraction, resource_allocation).
narrative_ontology:affects_constraint(means_testing_extraction, poverty_trap_accumulation).
narrative_ontology:affects_constraint(means_testing_extraction, informal_economy_precarity).
narrative_ontology:affects_constraint(means_testing_extraction, welfare_stigma_internalization).

% DUAL FORMULATION NOTE:
% Means testing represents a constraint family with three structurally distinct claims: (1) income verification as coordination mechanism (ε=0.25, primarily Rope), (2) benefit phase-out as extraction mechanism (ε=0.65, primarily Snare), (3) bureaucratic verification theater as degraded principle (ε=0.40, primarily Piton). This story aggregates all three into a single ε=0.58 Tangled Rope. Downstream constraints represent the actual observed effects of means testing on labor supply (poverty trap), employment strategy (informal economy shift), and identity formation (stigma internalization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(means_testing_extraction, powerless, 0.92).
constraint_indexing:directionality_override(means_testing_extraction, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
