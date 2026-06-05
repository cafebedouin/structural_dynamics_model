% ============================================================================
% CONSTRAINT STORY: eu_affordable_housing_initiative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_affordable_housing_initiative, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_affordable_housing_initiative
 *   human_readable: EU Affordable Housing Initiative (2025)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The European Commission's plan to promote affordable housing across
 *   member states. The effectiveness of this initiative will vary based on
 *   local conditions and how well it's implemented. The primary goal is to
 *   alleviate the housing crisis, especially for low-income households, but
 *   unintended consequences could occur. The initiative is designed as a
 *   temporary scaffold to address the housing crisis, with a sunset clause to
 *   encourage long-term solutions.
 *
 * KEY AGENTS:
 *   - European Commission: Initiator of the program (organized/mobile).
 *   - National Governments: Implementers of the program (institutional/arbitrage).
 *   - Low-Income Households: Intended beneficiaries (powerless/trapped).
 *   - Construction Sector: Involved in building affordable housing (moderate/constrained).
 *   - Private Landlords: May be affected by the policy (moderate/constrained).
 *   - Taxpayers: Fund the initiative (moderate/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_affordable_housing_initiative, 0.35).
domain_priors:suppression_score(eu_affordable_housing_initiative, 0.25).
domain_priors:theater_ratio(eu_affordable_housing_initiative, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_affordable_housing_initiative, tangled_rope).
narrative_ontology:human_readable(eu_affordable_housing_initiative, "EU Affordable Housing Initiative (2025)").
narrative_ontology:topic_domain(eu_affordable_housing_initiative, "economic/political").

domain_priors:requires_active_enforcement(eu_affordable_housing_initiative).
narrative_ontology:has_sunset_clause(eu_affordable_housing_initiative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, low_income_households).
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, construction_sector).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, private_landlords).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For low-income households in areas with soaring housing costs, the initiative may feel like a snare if insufficient affordable housing is created, leaving them trapped with limited options.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% For national governments, the initiative is intended as a rope, facilitating coordination and resource allocation towards affordable housing. They can arbitrage by directing funds strategically.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The European Commission views the initiative as a scaffold, providing temporary support and coordination to address the affordable housing crisis. Mobile due to policy evolution and future funding decisions.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% The construction sector is a beneficiary, but is constrained by regulations and material costs, experiencing a tangled rope of coordination and extraction through taxes and compliance.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% An analytical observer sees this initiative as a tangled rope, combining coordination efforts with some degree of extraction and potential suppression of alternative housing policies.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_affordable_housing_initiative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(eu_affordable_housing_initiative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate as there are costs involved in the initiative, primarily borne by taxpayers and potentially private landlords. Suppression is low, alternative housing policies are not directly suppressed but might receive less attention. Theater ratio is moderate as the implementation involves bureaucracy and political messaging alongside genuine efforts. The initiative requires active enforcement to ensure compliance and prevent misuse of funds.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from differing experiences. Low-income households may not benefit if the initiative is poorly implemented, leading to a snare. National governments see it as a tool, while the construction sector views it as both an opportunity and a constraint due to regulations. Private landlords may see the initiative as extracting from them if it affects their rental income. The European Commission sees it as a temporary scaffold, while an analytical observer sees a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are low-income households and the construction sector. National governments are implementers, so directionality is more symmetric. Private landlords and taxpayers might see the initiative as extracting from them if it affects their rental income or increases their tax burden. The European Commission's directionality is influenced by its role as the initiator and coordinator of the initiative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint aims to improve coordination through resource allocation, but also involves extraction and potential suppression of alternative policies. It requires careful monitoring to ensure coordination is truly improved and extraction is minimized. The sunset clause is intended to prevent the initiative from becoming a piton, but active enforcement is needed to ensure it remains effective and doesn't become a tool for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    housing_affordability_metric,
    'What metric best captures housing affordability and the initiative''s impact?',
    'Comparative analysis of various affordability indices (e.g., price-to-income ratio, housing wage) before and after the initiative.',
    'Choice of metric influences perceived success/failure of the initiative. Some metrics may show improvement while others do not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_affordability_metric, empirical, 'Metric for measuring housing affordability').

omega_variable(
    implementation_variance,
    'How much does implementation vary across member states, and what factors explain this variance?',
    'Case studies of implementation in different member states, analyzing policy choices, funding allocation, and regulatory frameworks.',
    'High variance may lead to uneven outcomes and questions about the initiative''s overall effectiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_variance, empirical, 'Variance in implementation across member states').

omega_variable(
    long_term_sustainability,
    'Is the initiative financially and politically sustainable in the long term?',
    'Analysis of funding sources, political support, and potential challenges to continued implementation.',
    'Lack of sustainability may undermine the initiative''s long-term impact and create uncertainty for beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_sustainability, conceptual, 'Long-term sustainability of the initiative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_affordable_housing_initiative, 2025, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_a_tr_t2025, eu_affordable_housing_initiative, theater_ratio, 2025, 0.3).
narrative_ontology:measurement(eu_a_tr_t2030, eu_affordable_housing_initiative, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(eu_a_tr_t2035, eu_affordable_housing_initiative, theater_ratio, 2035, 0.5).

% Extraction over time
narrative_ontology:measurement(eu_a_be_t2025, eu_affordable_housing_initiative, base_extractiveness, 2025, 0.25).
narrative_ontology:measurement(eu_a_be_t2030, eu_affordable_housing_initiative, base_extractiveness, 2030, 0.3).
narrative_ontology:measurement(eu_a_be_t2035, eu_affordable_housing_initiative, base_extractiveness, 2035, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_affordable_housing_initiative, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
