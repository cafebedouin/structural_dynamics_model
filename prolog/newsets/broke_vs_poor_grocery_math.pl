% ============================================================================
% CONSTRAINT STORY: broke_vs_poor_grocery_math
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_broke_vs_poor_grocery_math, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: broke_vs_poor_grocery_math
 *   human_readable: The Cognitive Load of Poverty (Grocery Math)
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint models the severe cognitive load imposed by poverty,
 *   using 'grocery math' as a key example. Individuals with highly
 *   constrained budgets must maintain a running mental tally of their
 *   purchases to avoid the high cost of failure (inability to pay) at
 *   checkout. This is not a choice but a mandatory, attention-consuming task.
 *   The constraint extracts a non-financial resource—cognitive
 *   bandwidth—which has cascading effects on long-term planning, education,
 *   and civic engagement.
 *
 * KEY AGENTS:
 *   - Low-Income Individuals: Primary target (powerless/trapped) — bear the full cognitive cost.
 *   - Capital Holders and Employers: Indirect beneficiary (institutional/arbitrage) — benefit from a workforce that is too cognitively taxed for long-term planning or collective action.
 *   - Middle-Class Observer: Unaffected observer (moderate/mobile) — tends to naturalize the constraint as a 'Mountain'.
 *   - Social Worker/Activist: Organized agent (organized/constrained) — attempts to mitigate the extraction while being constrained by the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(broke_vs_poor_grocery_math, 0.75).
domain_priors:suppression_score(broke_vs_poor_grocery_math, 0.8).
domain_priors:theater_ratio(broke_vs_poor_grocery_math, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, extractiveness, 0.75).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(broke_vs_poor_grocery_math, snare).
narrative_ontology:human_readable(broke_vs_poor_grocery_math, "The Cognitive Load of Poverty (Grocery Math)").
narrative_ontology:topic_domain(broke_vs_poor_grocery_math, "economic/social").

domain_priors:requires_active_enforcement(broke_vs_poor_grocery_math).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(broke_vs_poor_grocery_math, capital_holders_and_employers).
narrative_ontology:constraint_victim(broke_vs_poor_grocery_math, low_income_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL (SNARE) — Experiences the constraint as a constant, coercive extraction of cognitive resources. There is no option to not perform the calculation; failure results in immediate material lack and social humiliation. Exit is impossible without a fundamental change in economic status. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.85.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MIDDLE-CLASS OBSERVER (MOUNTAIN) — Lacking direct experience, this observer naturalizes the constraint as an unfortunate but fixed feature of the economic landscape ('the poor will always be with us'). The coercive mechanism is invisible, appearing as an immutable law rather than a contingent state. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMIC BENEFICIARY (ROPE) — From the perspective of capital, this cognitive tax is a feature, not a bug. It appears as a coordination mechanism that ensures labor discipline and reduces the capacity for long-term planning or collective action among the workforce. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.11. The negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ACTIVIST (TANGLED ROPE) — Sees both the severe extraction (the Snare aspect) and attempts to build coordination solutions (the Rope aspect) like benefits counseling, financial literacy programs, and mutual aid. They are entangled in a system that has both coercive and (potentially) cooperative elements. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.51.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (SNARE) — The analyst sees the full structure: high base extraction of a non-financial resource (attention), high suppression of alternatives, and an asymmetric distribution of costs and benefits. The lack of a genuine coordination function for the victim confirms the Snare classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broke_vs_poor_grocery_math_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(broke_vs_poor_grocery_math, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(broke_vs_poor_grocery_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is very high, representing the constant drain on finite cognitive resources, not a direct financial transfer. Suppression (0.80) is also very high because the alternative—not performing the mental calculation—is not viable and leads to immediate, severe consequences. The enforcement mechanism is the hard budget limit at the point of sale. Theater Ratio (0.10) is extremely low; this is a brutally functional and non-performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The individual experiencing the constraint lives inside a Snare. An outside observer with economic security sees a Mountain, naturalizing a social condition as an immutable law. The systemic beneficiary, who profits from a cognitively taxed populace, perceives a Rope—a market 'discipline' that coordinates behavior for their benefit. The analyst, seeing the full structure, confirms the Snare classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear. 'Low_income_individuals' are declared victims with 'trapped' exit options, leading to a derived directionality `d` near 1.0 and maximizing effective extraction (χ). 'Capital_holders_and_employers' are declared beneficiaries with 'arbitrage' exit options, leading to a `d` near 0.0 and a negative χ, indicating they receive a net subsidy from the constraint's existence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of a Snare that is frequently misidentified as a Mountain ('that's just life for poor people') or a matter of personal virtue/failure. By quantifying the extraction of a non-monetary resource (attention) and the high degree of coercion (suppression), the DR framework correctly identifies it as a Snare. It avoids the mandatrophy of blaming the victim by locating the coercive structure in the economic environment, not the individual's character.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_benefit_vs_emergent_property,
    'Is the cognitive tax a functional component that benefits capital by suppressing worker agency, or a non-functional, emergent property of economic inequality?',
    'Comparative economic analysis of societies with different welfare and wage structures, correlating cognitive load metrics with rates of labor organization and political participation.',
    'If functional, it''s a highly stable Snare. If emergent, it''s a Snare that could potentially be resolved through targeted policy without threatening the core economic structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_benefit_vs_emergent_property, conceptual, 'Whether the cognitive tax is a functional feature or an emergent bug of the economic system.').

omega_variable(
    cognitive_vs_financial_extraction,
    'Is the primary harm the direct cognitive load itself, or the poor financial decisions made as a result of decision fatigue?',
    'Longitudinal studies of low-income cohorts, measuring cognitive load against financial outcomes and decision quality over time.',
    'If the harm is direct cognitive load, solutions must focus on reducing complexity (e.g., UBI). If the harm is poor decisions, solutions might focus on decision support tools (which could be another Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_vs_financial_extraction, empirical, 'Distinguishing the direct harm of cognitive load from the indirect harm of poor decision-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(broke_vs_poor_grocery_math, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brok_tr_t0, broke_vs_poor_grocery_math, theater_ratio, 0, 0.1).
narrative_ontology:measurement(brok_tr_t25, broke_vs_poor_grocery_math, theater_ratio, 25, 0.1).
narrative_ontology:measurement(brok_tr_t50, broke_vs_poor_grocery_math, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(brok_be_t0, broke_vs_poor_grocery_math, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(brok_be_t25, broke_vs_poor_grocery_math, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(brok_be_t50, broke_vs_poor_grocery_math, base_extractiveness, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(broke_vs_poor_grocery_math, payday_lending_debt_cycle).
narrative_ontology:affects_constraint(broke_vs_poor_grocery_math, food_deserts_and_nutrition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
