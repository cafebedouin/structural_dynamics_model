% ============================================================================
% CONSTRAINT STORY: bor_tax_exemption_nl
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bor_tax_exemption_nl, []).

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
 *   constraint_id: bor_tax_exemption_nl
 *   human_readable: Dutch Business Succession Scheme (BOR)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Dutch Business Succession Scheme (BOR) is a tax regulation designed
 *   to facilitate the transfer of family businesses to the next generation by
 *   providing substantial exemptions on inheritance and gift tax. While
 *   intended to preserve family businesses and promote economic stability,
 *   the scheme also faces scrutiny regarding its fairness and potential
 *   budgetary implications. This constraint presents a mix of coordination
 *   and extraction, benefiting specific groups while imposing costs on
 *   others.
 *
 * KEY AGENTS:
 *   - Family Business Owners: Primary beneficiaries (institutional/arbitrage) – gain tax advantages.
 *   - General Taxpayers: Primary victims (powerless/trapped) – bear the cost of foregone tax revenue.
 *   - Estate Planning Industry: Secondary beneficiaries (powerful/constrained) – benefit from advising on BOR compliance.
 *   - Government Budget: Secondary victim (institutional/constrained) – potential reduction in tax revenue.
 *   - Analytical Observer: Neutral observer (analytical/analytical) – assesses the overall economic and social impacts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bor_tax_exemption_nl, 0.55).
domain_priors:suppression_score(bor_tax_exemption_nl, 0.4).
domain_priors:theater_ratio(bor_tax_exemption_nl, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bor_tax_exemption_nl, extractiveness, 0.55).
narrative_ontology:constraint_metric(bor_tax_exemption_nl, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(bor_tax_exemption_nl, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bor_tax_exemption_nl, tangled_rope).
narrative_ontology:human_readable(bor_tax_exemption_nl, "Dutch Business Succession Scheme (BOR)").
narrative_ontology:topic_domain(bor_tax_exemption_nl, "economic/political").

domain_priors:requires_active_enforcement(bor_tax_exemption_nl).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, family_business_owners).
narrative_ontology:constraint_beneficiary(bor_tax_exemption_nl, estate_planning_industry).
narrative_ontology:constraint_victim(bor_tax_exemption_nl, general_taxpayers).
narrative_ontology:constraint_victim(bor_tax_exemption_nl, government_budget).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Family business owners benefit from reduced tax burden, facilitating smoother generational transitions. They can 'arbitrage' by structuring their business to maximize BOR benefits.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% General taxpayers bear the burden of foregone tax revenue due to the exemption. They are 'trapped' as they have no direct influence on the policy and must compensate for the lost revenue.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The analytical observer sees the BOR as a mixed system: it coordinates succession planning but also extracts from the general tax base. The global scope reflects the awareness of similar schemes in other countries and their effects.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The estate planning industry benefits from advising families on how to structure their business to maximize the BOR benefits but are 'constrained' by the specific regulations. They also depend on the BOR's continued existence for their business model.
constraint_indexing:constraint_classification(bor_tax_exemption_nl, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bor_tax_exemption_nl_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bor_tax_exemption_nl, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bor_tax_exemption_nl, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bor_tax_exemption_nl_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The BOR extracts from the general tax base to provide benefits to family businesses. The exemption reduces government revenue. Suppression (0.40): The general public has limited means to influence this policy and must accept the tax burden. Theater Ratio (0.30): The BOR is mostly functional in its intended goal, but there's a theatrical aspect where some businesses may structure operations specifically to qualify, which may not be the most efficient economic activity.
 *
 * PERSPECTIVAL GAP:
 *   Family business owners see the BOR as a positive tool for succession planning (rope). General taxpayers perceive it as an unfair tax break (snare). The estate planning industry recognizes the opportunities and constraints (tangled rope). The analytical observer sees a system with both coordination and extractive elements (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Family business owners, with their ability to strategically utilize the BOR, experience it as a rope. General taxpayers, lacking a direct means to influence the policy, perceive it as a snare. Estate planning entities benefit from their expertise in navigating BOR regulations, leading to their classification as tangled rope, as they are both benefitted and constrained. The neutral observer analyses the economic consequences of the BOR and sees it as a mix of coordination and extraction, leading to the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The BOR resolves the mandatrophy because it is a classic example of a tangled rope: it has a coordination function (facilitating business succession) but also involves extraction (transferring wealth with reduced taxation). The question is not which of these two functions dominates, but how to calibrate them to balance the benefits of preserving family businesses against the costs of reduced tax revenue and potential inequalities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_impact_assessment,
    'What is the true economic impact of the BOR on job creation and retention?',
    'Conducting a thorough cost-benefit analysis that accounts for both direct and indirect effects, including displacement effects.',
    'If the BOR stimulates significant job creation and retention, it is a rope. If it primarily benefits wealthy families with minimal economic impact, it is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_assessment, empirical, 'Quantify the true economic impact of BOR on job creation and retention.').

omega_variable(
    tax_fairness_perception,
    'How do general taxpayers perceive the fairness of the BOR?',
    'Conducting public opinion surveys to gauge the perception of fairness and equity of the tax exemption.',
    'If perceived as fair, it is a rope. If perceived as unfair and primarily benefiting the wealthy, it is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_fairness_perception, preference, 'Assess general taxpayers perception of the BOR''s fairness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bor_tax_exemption_nl, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bor__tr_t0, bor_tax_exemption_nl, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bor__tr_t5, bor_tax_exemption_nl, theater_ratio, 5, 0.25).
narrative_ontology:measurement(bor__tr_t10, bor_tax_exemption_nl, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(bor__be_t0, bor_tax_exemption_nl, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bor__be_t5, bor_tax_exemption_nl, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(bor__be_t10, bor_tax_exemption_nl, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bor_tax_exemption_nl, inheritance_tax_nl).
narrative_ontology:affects_constraint(bor_tax_exemption_nl, corporate_tax_nl).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
