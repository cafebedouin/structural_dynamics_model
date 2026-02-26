% ============================================================================
% CONSTRAINT STORY: automatic_enrollment_defaults
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_automatic_enrollment_defaults, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: automatic_enrollment_defaults
 *   human_readable: Automatic Enrollment Defaults in Retirement Plans
 *   domain: economic/social
 *
 * SUMMARY:
 *   Automatic enrollment in retirement plans is a choice architecture
 *   designed to increase participation by making saving the default option.
 *   It leverages behavioral inertia to overcome procrastination. While highly
 *   effective at boosting enrollment rates, it creates a structural tension.
 *   It serves a genuine coordination function by helping people save who
 *   otherwise wouldn't. Simultaneously, it creates an extractive potential
 *   for financial service providers (via fees on default funds) and employers
 *   (who may benefit from sticky, low default contribution rates that
 *   minimize their matching costs).
 *
 * KEY AGENTS:
 *   - Financially Strained Employee: Primary target (powerless/trapped) — bears cost of contributions and fees with little perceived agency.
 *   - Financial Services Provider: Primary beneficiary (institutional/arbitrage) — benefits from increased assets under management.
 *   - The Employer: Secondary beneficiary (organized/constrained) — benefits from offering a modern plan and potentially lower matching costs.
 *   - The Procrastinating Professional: Beneficiary (moderate/mobile) — benefits from the nudge to overcome personal inertia.
 *   - The Social Policy Architect: Institutional actor (institutional/constrained) — sees the system as a tool to solve a national savings crisis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(automatic_enrollment_defaults, 0.48).
domain_priors:suppression_score(automatic_enrollment_defaults, 0.62).
domain_priors:theater_ratio(automatic_enrollment_defaults, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, 0.48).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(automatic_enrollment_defaults, tangled_rope).
narrative_ontology:human_readable(automatic_enrollment_defaults, "Automatic Enrollment Defaults in Retirement Plans").
narrative_ontology:topic_domain(automatic_enrollment_defaults, "economic/social").

domain_priors:requires_active_enforcement(automatic_enrollment_defaults).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, employees_with_high_inertia).
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, financial_service_providers).
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, employers).
narrative_ontology:constraint_victim(automatic_enrollment_defaults, employees_stuck_at_low_defaults).
narrative_ontology:constraint_victim(automatic_enrollment_defaults, employees_unaware_of_fees).
narrative_ontology:constraint_victim(automatic_enrollment_defaults, low_income_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FINANCIALLY STRAINED EMPLOYEE (SNARE) — Trapped by behavioral inertia and lack of financial literacy. The default contribution, however small, represents immediate financial hardship, while the fees on default funds are a form of pure extraction. Opting out requires overcoming a significant activation energy barrier. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FINANCIAL SERVICES PROVIDER (ROPE) — Benefits from a larger pool of assets under management. From this perspective, the constraint is a pure coordination mechanism that solves the market failure of under-saving, creating a win-win. Exit via arbitrage is high (can offer plans to thousands of employers). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (overcoming procrastination to increase savings rates) and the asymmetric extraction (management fees, employers benefiting from low default rates). The system requires active enforcement by plan administrators and is not a simple convention. This matches the claimed type.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PROCRASTINATING PROFESSIONAL (ROPE) — A median employee who knows they should save but lacks the initiative. For them, the default is a welcome nudge that solves a personal time-inconsistency problem. They have the agency and knowledge to change the default but benefit from the initial push. They perceive no extraction, only coordination. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE SOCIAL POLICY ARCHITECT (SCAFFOLD) — Views auto-enrollment as a temporary support to build a culture of saving. The policy is designed to overcome a specific behavioral failure, with the implicit 'sunset' being the point at which an individual becomes an engaged saver or the social norm of saving is established. The goal is to build a structure that eventually becomes unnecessary.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(automatic_enrollment_defaults_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(automatic_enrollment_defaults, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(automatic_enrollment_defaults_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48): This reflects the value extracted through management fees on default investment vehicles and the potential for employers to suppress wage growth or matching contributions by relying on low, sticky default rates. Suppression (0.62): This score is high not because of physical coercion, but because of the powerful force of behavioral inertia. The alternative (opting out) is simple in theory but requires overcoming a significant psychological barrier, making the default highly coercive in practice. Theater Ratio (0.15): The mechanism is highly functional and does exactly what it is designed to do—increase enrollment. There is very little performative action; the structure itself does the work.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a financial firm, this is a Rope that efficiently gathers assets and solves a social problem. For a low-income employee, it's a Snare that siphons money from their paycheck without clear consent or understanding. For a policy maker, it's a Scaffold to build better savings habits. The analytical view must hold both the coordination and extraction in tension, leading to a Tangled Rope classification. The system's character is fundamentally dependent on the observer's position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries include financial providers (institutional/arbitrage, d≈0.05) and employees who need the nudge (moderate/mobile, d≈0.55). Victims are those trapped by inertia at sub-optimal rates or for whom the contribution is a hardship (powerless/trapped, d≈0.95). This wide spread in directionality (d) across the agent population is what generates the diverse classifications from a single set of base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case where a simple classification would fail. Labeling it a 'Rope' (as behavioral economists might) ignores the extractive fee structure. Labeling it a 'Snare' (as a libertarian might) ignores the genuine, welfare-enhancing coordination it provides for many. The Tangled Rope classification, from the analytical perspective, correctly identifies that it is BOTH a coordination mechanism and an extractive one, resolving the mandatrophy by refusing a simplistic label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    savings_vs_fees,
    'Is the primary structural effect of auto-enrollment the increase in aggregate savings (coordination) or the generation of fees from managed assets (extraction)?',
    'Comparative analysis of net wealth accumulation in auto-enrolled vs. actively-enrolled cohorts, controlling for income and factoring in lifetime fees.',
    'If net wealth is significantly higher, it strengthens the Rope/Scaffold case. If fees consume a large fraction of the gains, it strengthens the Snare/Tangled Rope case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(savings_vs_fees, empirical, 'Whether increased savings outweigh lifetime fees from default funds.').

omega_variable(
    default_rate_suppression,
    'Does the ''stickiness'' of the default contribution rate lead to lower lifetime savings for employees who would have otherwise chosen a higher rate?',
    'Longitudinal study comparing savings trajectories of employees in opt-in vs. opt-out systems with similar demographics.',
    'If auto-enrolled employees cluster at low defaults and save less overall, the system functions as a Snare for a larger population. If it serves as a floor that most exceed, it''s a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_rate_suppression, empirical, 'Whether default rates anchor employees at sub-optimal savings levels.').

omega_variable(
    paternalism_vs_exploitation,
    'Is leveraging behavioral inertia a legitimate paternalistic intervention (Scaffold) or a subtle form of exploitation (Snare)?',
    'This is a conceptual ambiguity, resolvable only by defining a clear ethical boundary for ''nudges'' in public policy.',
    'The classification depends on the ethical framework. A framework prioritizing autonomy would view it as a Snare; one prioritizing welfare outcomes would view it as a Scaffold or Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paternalism_vs_exploitation, conceptual, 'The ethical framing of using behavioral inertia in policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(automatic_enrollment_defaults, 2006, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t2006, automatic_enrollment_defaults, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(auto_tr_t2016, automatic_enrollment_defaults, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(auto_tr_t2026, automatic_enrollment_defaults, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(auto_be_t2006, automatic_enrollment_defaults, base_extractiveness, 2006, 0.35).
narrative_ontology:measurement(auto_be_t2016, automatic_enrollment_defaults, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(auto_be_t2026, automatic_enrollment_defaults, base_extractiveness, 2026, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(automatic_enrollment_defaults, resource_allocation).
narrative_ontology:affects_constraint(automatic_enrollment_defaults, consumer_debt_levels).
narrative_ontology:affects_constraint(automatic_enrollment_defaults, social_security_solvency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
