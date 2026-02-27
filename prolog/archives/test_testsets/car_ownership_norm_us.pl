% ============================================================================
% CONSTRAINT STORY: car_ownership_norm_us
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_car_ownership_norm_us, []).

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
 *   constraint_id: car_ownership_norm_us
 *   human_readable: The Norm of Individual Car Ownership in the US
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint models the socio-economic system in the United States
 *   that normalizes and often necessitates individual car ownership. The
 *   system is maintained by a confluence of interests including the
 *   automotive, fossil fuel, and real estate development industries. It is
 *   enforced through decades of policy choices, such as prioritizing highway
 *   construction over public transportation and enacting zoning laws that
 *   create low-density, car-dependent sprawl. While providing a genuine
 *   coordination function for mobility, it imposes massive financial burdens,
 *   particularly on lower-income households, and systematically suppresses
 *   less extractive alternatives.
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victims (powerless/trapped) - forced into car ownership and debt to access jobs and services.
 *   - Automotive-Industrial Complex: Primary beneficiaries (institutional/arbitrage) - profit from the guaranteed demand for vehicles, fuel, insurance, and loans.
 *   - Middle-Class Suburbanites: Secondary victims/beneficiaries (moderate/mobile) - receive mobility benefits but bear high financial and time costs.
 *   - Urbanist Advocacy Groups: Organized opposition (organized/constrained) - work to build alternatives and dismantle the car-dependent infrastructure.
 *   - Analytical Observer: System-level view (analytical/analytical) - recognizes the dual nature of the system as both coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(car_ownership_norm_us, 0.75).
domain_priors:suppression_score(car_ownership_norm_us, 0.85).
domain_priors:theater_ratio(car_ownership_norm_us, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(car_ownership_norm_us, extractiveness, 0.75).
narrative_ontology:constraint_metric(car_ownership_norm_us, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(car_ownership_norm_us, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(car_ownership_norm_us, tangled_rope).
narrative_ontology:human_readable(car_ownership_norm_us, "The Norm of Individual Car Ownership in the US").
narrative_ontology:topic_domain(car_ownership_norm_us, "economic/social").

domain_priors:requires_active_enforcement(car_ownership_norm_us).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, automotive_manufacturers).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, insurance_companies).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, auto_loan_financiers).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, suburban_developers).
narrative_ontology:constraint_victim(car_ownership_norm_us, low_income_households).
narrative_ontology:constraint_victim(car_ownership_norm_us, non_drivers).
narrative_ontology:constraint_victim(car_ownership_norm_us, urban_pedestrians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME COMMUTER (SNARE) - In car-dependent regions, access to employment is contingent on car ownership, forcing individuals into debt for transportation. Lack of viable public transit creates a trapped condition. d≈0.95, f(d)≈1.42, σ=0.9 -> χ≈1.00. This is a classic debt and mobility trap.
constraint_indexing:constraint_classification(car_ownership_norm_us, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AUTOMOTIVE-INDUSTRIAL COMPLEX (ROPE) - For beneficiaries like car manufacturers and lenders, the system is a pure coordination mechanism that creates and sustains a massive, predictable market. d≈0.05, f(d)≈-0.12, σ=1.0 -> χ≈-0.09. The negative effective extraction indicates a net subsidy from the system.
constraint_indexing:constraint_classification(car_ownership_norm_us, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-CLASS SUBURBAN FAMILY (TANGLED ROPE) - Experiences both the coordination benefits (mobility, access to suburban amenities) and the high extractive costs (payments, insurance, fuel, time). They have some exit options (moving, remote work) but they are costly. d≈0.85, f(d)≈1.15, σ=0.9 -> χ≈0.78.
constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - The system has a genuine coordination function (enabling mass mobility) but is coupled with extremely high, asymmetrically distributed extraction and suppression of alternatives. This is the canonical Tangled Rope structure. d≈0.72, f(d)≈1.15, σ=1.0 -> χ≈0.86.
constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: URBANIST ADVOCACY GROUP (SCAFFOLD) - Views car dependency as a temporary, harmful structure to be replaced. They work to build alternatives (bike lanes, transit funding, zoning reform) with the explicit goal of making the constraint obsolete. Their actions create a sunset clause. d≈0.40, f(d)≈0.40, σ=0.8 -> χ≈0.24.
constraint_indexing:constraint_classification(car_ownership_norm_us, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(car_ownership_norm_us_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(car_ownership_norm_us, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(car_ownership_norm_us, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(car_ownership_norm_us, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(car_ownership_norm_us_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is very high, representing the enormous transfer of wealth from households to the auto-industrial complex via car payments, insurance, fuel, and maintenance, often exceeding 20% of household income. Suppression (0.85) is also very high, as land-use patterns and a lack of investment in alternatives make non-car life functionally impossible in most of the US. Theater Ratio (0.40) reflects the powerful cultural narrative equating car ownership with freedom, success, and adulthood, which helps maintain the norm even when it is economically irrational.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the institutional beneficiaries, the system is a highly effective Rope that coordinates a vast market. For the powerless victim trapped in a food desert without a bus line, it is a Snare that extracts wealth in exchange for basic participation in society. For the analytical observer, the system's dual function is clear, classifying it as a Tangled Rope. This gap explains why political discourse on transportation is so polarized: different actors are describing structurally accurate but irreconcilable experiences of the same system.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (auto industry) have arbitrage exit and benefit, leading to a low 'd' value and a Rope classification with negative effective extraction. Victims (low-income households) are trapped and bear costs, leading to a high 'd' value and a Snare classification with maximal effective extraction. The analytical view averages these effects, resulting in the Tangled Rope classification that captures the system's essential conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a primary example of resolving mandatrophy. To label the system a 'Rope' based on its coordination function is to ignore the coercive, extractive reality for millions. To label it a 'Snare' is to ignore that it does, in fact, coordinate mobility for those who can afford it. The Tangled Rope classification is essential as it correctly identifies the structure as a hybrid, preventing the mislabeling that would occur if only the beneficiary's or the victim's perspective were considered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mobility_price_vs_extraction,
    'Are the high costs of car ownership a fair market price for the mobility provided, or an extractive rent enabled by the suppression of alternatives like public transit and dense housing?',
    'Comparative economic analysis of mobility costs (as % of income) in US cities vs. European/Asian cities with robust public transit and different zoning policies.',
    'If costs are found to be a fair reflection of value provided, the constraint is closer to a Rope. If costs are significantly inflated due to suppressed alternatives, it is a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mobility_price_vs_extraction, empirical, 'Distinguishing fair mobility price from extractive rent.').

omega_variable(
    revealed_preference_vs_coercion,
    'To what extent does car ownership reflect a genuine consumer preference for privacy and autonomy versus a choice coerced by land-use patterns and lack of alternatives?',
    'Analysis of mobility choices in newly developed mixed-use areas with high-quality transit, controlling for income and demographics.',
    'High uptake of alternatives would confirm the coercion hypothesis (Snare/Tangled Rope). Continued dominance of cars would support the preference hypothesis (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revealed_preference_vs_coercion, empirical, 'Disentangling consumer preference from structural coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(car_ownership_norm_us, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(car__tr_t1950, car_ownership_norm_us, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(car__tr_t1990, car_ownership_norm_us, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(car__tr_t2025, car_ownership_norm_us, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(car__be_t1950, car_ownership_norm_us, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(car__be_t1990, car_ownership_norm_us, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(car__be_t2025, car_ownership_norm_us, base_extractiveness, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(car_ownership_norm_us, global_infrastructure).
narrative_ontology:affects_constraint(car_ownership_norm_us, suburban_zoning_laws).
narrative_ontology:affects_constraint(car_ownership_norm_us, fossil_fuel_subsidies).
narrative_ontology:affects_constraint(car_ownership_norm_us, consumer_debt_burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
