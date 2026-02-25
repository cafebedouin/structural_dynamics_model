% ============================================================================
% CONSTRAINT STORY: brazil_mexico_financial_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_mexico_financial_requirement, []).

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
 *   constraint_id: brazil_mexico_financial_requirement
 *   human_readable: Mexican Financial Proof Requirement for Brazilian Travelers
 *   domain: geopolitical/migration
 *
 * SUMMARY:
 *   In response to pressure from the United States to curb migration flows,
 *   Mexico implemented a rule requiring Brazilian travelers to prove
 *   significant financial solvency (approx. US$1,900). While officially
 *   framed as a standard immigration check to ensure tourists can cover their
 *   expenses, the policy functions as a filter to block potential migrants
 *   who use Mexico as a transit country to the US. This creates a stark
 *   perspectival gap between the actors involved.
 *
 * KEY AGENTS:
 *   - United States Government: Primary beneficiary (institutional/arbitrage) - Achieves migration control goals remotely.
 *   - Aspiring Brazilian Migrants: Primary victims (powerless/trapped) - Face a hard barrier to travel and opportunity.
 *   - Mexican Government: Enforcer and secondary victim (institutional/constrained) - Implements the policy under duress, balancing US relations against other costs.
 *   - Wealthy Brazilian Tourists: Unintended targets (powerful/mobile) - Experience the rule as a minor bureaucratic hurdle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_mexico_financial_requirement, 0.65).
domain_priors:suppression_score(brazil_mexico_financial_requirement, 0.8).
domain_priors:theater_ratio(brazil_mexico_financial_requirement, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, extractiveness, 0.65).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_mexico_financial_requirement, tangled_rope).
narrative_ontology:human_readable(brazil_mexico_financial_requirement, "Mexican Financial Proof Requirement for Brazilian Travelers").
narrative_ontology:topic_domain(brazil_mexico_financial_requirement, "geopolitical/migration").

domain_priors:requires_active_enforcement(brazil_mexico_financial_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, us_government).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, aspiring_brazilian_migrants).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, low_income_brazilian_travelers).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, mexican_tourism_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING MIGRANT (SNARE) — For the individual unable to meet the financial threshold, the rule is an absolute, coercive barrier to entry. They are the direct target of the extraction (of opportunity) and have no legal recourse or alternative. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US GOVERNMENT (ROPE) — As the primary beneficiary, the US sees the rule as a low-cost coordination mechanism to achieve its geopolitical goal of controlling migration flows far from its own border. The extraction is externalized to other actors. d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.09.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: MEXICAN GOVERNMENT (TANGLED ROPE) — As the enforcer, Mexico is constrained by US pressure. It implements a coercive rule (extraction) but also engages in a form of geopolitical coordination. It bears costs (diplomatic friction, lost tourism) but benefits from appeasing a powerful neighbor. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY TOURIST (ROPE) — For an affluent traveler, the requirement is a minor bureaucratic inconvenience, not a barrier. It is experienced as a pure coordination problem (providing paperwork) with negligible extraction. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.42. This value is on the cusp of Tangled Rope, but the low personal cost makes Rope the better classification.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees the full structure: a coordination function (managing international migration) inextricably linked to an asymmetric extractive function (blocking a specific demographic for a third party's benefit). The high suppression and clear victim class confirm the Tangled Rope classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_mexico_financial_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_mexico_financial_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(brazil_mexico_financial_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high because the constraint effectively extracts the opportunity of travel and potential migration from its target group. Suppression (0.80) is high as there are no legal alternatives for those who cannot meet the financial requirement to enter Mexico by air. Theater Ratio (0.60) is significant because the stated purpose (ensuring tourist solvency) masks the primary geopolitical function (migration control).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The US, as the beneficiary, sees a clean coordination tool (Rope). The targeted Brazilian migrant, who is trapped by the rule, experiences it as a pure Snare. The Mexican government, caught between US pressure and the costs of enforcement, perceives a coercive hybrid (Tangled Rope). Finally, the affluent tourist, for whom the rule is a mere formality, sees it as a simple coordination problem (Rope). This demonstrates how a single policy's classification is determined by the observer's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   The US is the clear beneficiary, with arbitrage options in its diplomatic toolkit, leading to a low 'd' value and negative effective extraction (χ). Brazilian travelers are the clear victims, with those targeted for exclusion being trapped, leading to a high 'd' value and high χ. The Mexican government is both a victim of US pressure and a beneficiary of a stable relationship, while being constrained in its options, resulting in an intermediate 'd' value and a Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a clear example of resolving mandatrophy. A naive analysis might label the rule as simple 'immigration policy' (Rope) or 'racist exclusion' (Snare). The Deferential Realism framework, by using indexed perspectives, shows that both are valid experiential realities. The analytical classification of Tangled Rope correctly captures the dual nature of the constraint: it possesses a genuine (if coerced) coordination function while simultaneously operating as a highly extractive mechanism against a specific, powerless population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_pressure_vs_mexican_agency,
    'To what extent is this policy a direct result of US pressure versus an independent Mexican initiative?',
    'Declassification of diplomatic cables; analysis of timelines of US-Mexico migration talks versus policy implementation.',
    'If purely US pressure, Mexico is a constrained victim. If Mexico has its own motives, it becomes a partial beneficiary, altering its directionality and the nature of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_pressure_vs_mexican_agency, empirical, 'Quantifies the degree of external coercion vs internal policy choice for Mexico.').

omega_variable(
    effectiveness_as_deterrent,
    'Does the financial requirement effectively curb irregular migration, or does it merely displace migrants to more dangerous, clandestine routes?',
    'Comparative analysis of migration data through official vs unofficial channels before and after the policy''s implementation.',
    'If it primarily displaces routes, the ''coordination'' function is largely theater, and the constraint is closer to a pure Snare. If it effectively deters, the coordination function is real, supporting the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_as_deterrent, empirical, 'Measures whether the rule achieves its stated coordination goal or simply increases harm.').

omega_variable(
    economic_impact_on_tourism,
    'What is the net economic impact on Mexico''s tourism sector from the loss of Brazilian visitors?',
    'Econometric analysis of tourism revenue from Brazil, controlling for other factors, pre- and post-policy.',
    'A high negative impact solidifies the Mexican government''s status as a victim in the constraint, justifying its ''constrained'' exit options. A negligible impact would suggest Mexico bears little cost for enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_on_tourism, empirical, 'Quantifies the cost borne by Mexico for enforcing the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_mexico_financial_requirement, 2022, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(braz_tr_t0, brazil_mexico_financial_requirement, theater_ratio, 0, 0.55).
narrative_ontology:measurement(braz_tr_t2, brazil_mexico_financial_requirement, theater_ratio, 2, 0.6).
narrative_ontology:measurement(braz_tr_t5, brazil_mexico_financial_requirement, theater_ratio, 5, 0.6).

% Extraction over time
narrative_ontology:measurement(braz_be_t0, brazil_mexico_financial_requirement, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(braz_be_t2, brazil_mexico_financial_requirement, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(braz_be_t5, brazil_mexico_financial_requirement, base_extractiveness, 5, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_mexico_financial_requirement, enforcement_mechanism).
narrative_ontology:affects_constraint(brazil_mexico_financial_requirement, us_mexico_border_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
