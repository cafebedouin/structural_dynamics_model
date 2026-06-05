% ============================================================================
% CONSTRAINT STORY: us_employer_health_insurance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_employer_health_insurance, []).

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
 *   constraint_id: us_employer_health_insurance
 *   human_readable: US Employer-Sponsored Insurance (ESI) System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The US Employer-Sponsored Insurance (ESI) system is a path-dependent
 *   artifact of WWII-era wage freezes that ties essential healthcare access
 *   to specific employment. While providing a mechanism for many Americans to
 *   obtain health insurance, it also creates economic distortions, job lock,
 *   and inequalities in access. The system's design advantages large
 *   employers and insurance companies, while placing burdens on employees,
 *   the unemployed, and small businesses.
 *
 * KEY AGENTS:
 *   - Employees: Primary target (powerless/trapped) — experience job lock due to reliance on ESI.
 *   - Large Employers: Primary beneficiary (institutional/arbitrage) — attract and retain employees, often at lower cost.
 *   - Insurance Companies: Beneficiary (institutional/arbitrage) - manage risk and obtain profits.
 *   - Unemployed: Victims (powerless/trapped) - lack of access to ESI creates vulnerability.
 *   - Small Businesses: Victims (moderate/constrained) - face higher premiums and administrative burdens.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_employer_health_insurance, 0.65).
domain_priors:suppression_score(us_employer_health_insurance, 0.7).
domain_priors:theater_ratio(us_employer_health_insurance, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_employer_health_insurance, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_employer_health_insurance, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_employer_health_insurance, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_employer_health_insurance, tangled_rope).
narrative_ontology:human_readable(us_employer_health_insurance, "US Employer-Sponsored Insurance (ESI) System").
narrative_ontology:topic_domain(us_employer_health_insurance, "economic/social").

domain_priors:requires_active_enforcement(us_employer_health_insurance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, large_employers).
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, insurance_companies).
narrative_ontology:constraint_victim(us_employer_health_insurance, employees).
narrative_ontology:constraint_victim(us_employer_health_insurance, unemployed).
narrative_ontology:constraint_victim(us_employer_health_insurance, small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Employees often experience 'job lock' due to their reliance on employer-sponsored health insurance, making them less likely to switch jobs or start their own businesses, even if better opportunities exist elsewhere. They are trapped within the system.
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Large employers benefit from the ESI system because it allows them to attract and retain employees, often at a lower cost than providing equivalent benefits directly. They can arbitrage across a large pool of employees to negotiate lower rates.
constraint_indexing:constraint_classification(us_employer_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the ESI system as a deeply flawed arrangement that provides essential healthcare access but is also a source of significant economic distortions and social inequalities. It represents a tangled rope because it offers coordination benefits (access to healthcare) alongside asymmetric extraction (job lock, administrative overhead).
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% Small businesses are constrained by the ESI system, as they often face higher premiums and administrative burdens compared to larger employers, making it difficult for them to compete for talent. They experience it as a tangled rope because they must offer insurance to compete, but are at a disadvantage.
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Insurance companies benefit as they administer large populations, managing risk and obtaining profits, while shifting costs and risks when advantageous. Their arbitrage comes from risk management over these groups.
constraint_indexing:constraint_classification(us_employer_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_employer_health_insurance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_employer_health_insurance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_employer_health_insurance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_employer_health_insurance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_employer_health_insurance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The ESI system extracts a significant cost from employees in the form of job lock and limited career mobility. It extracts from small businesses through higher premiums. Suppression (0.70): High. The lack of affordable alternatives to ESI suppresses employee agency and choice. The system actively enforces its role as the dominant mechanism for health insurance access. Theater ratio (0.30): Low-moderate. The system does have genuine coordination elements but these are increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different structural positions of the key actors. Large employers see a coordination mechanism that helps them attract and retain employees. Employees, especially those with pre-existing conditions, see a snare that limits their career options. Analytical observers see a tangled rope that provides access to healthcare but also creates economic distortions and social inequalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the power and exit options of the agents. Employees are powerless and trapped, giving them a high directionality value (close to 1). Large employers are institutional and have arbitrage options, giving them a low directionality value (close to 0). Insurance Companies are institutional and have arbitrage options, giving them a low directionality value (close to 0). Small businesses are moderate and constrained, giving them a moderate directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The ESI system is classified as a tangled rope because it combines elements of coordination (providing healthcare access) with extraction (job lock, administrative overhead, limited choice). A pure coordination mechanism would not create these distortions. A pure extraction mechanism would not provide any benefits to employees.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_solution_viability,
    'How viable are proposed solutions for making healthcare coverage more portable and less tied to employment?',
    'Analysis of the impact of various policy proposals (e.g., universal healthcare, expanded ACA subsidies) on coverage rates, costs, and labor market dynamics.',
    'If viable: the ESI system could transition to a more equitable and efficient model. If not: the ESI system will continue to perpetuate existing inequalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portability_solution_viability, empirical, 'Viability of solutions for healthcare coverage portability').

omega_variable(
    alternative_financing_feasibility,
    'What is the feasibility of alternative healthcare financing mechanisms that do not rely on employer contributions?',
    'Economic modeling of different financing scenarios, including payroll taxes, income taxes, and value-added taxes.',
    'If feasible: the burden of healthcare financing could be shifted away from employers, reducing job lock. If not: employers will continue to play a central role in healthcare financing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_feasibility, empirical, 'Feasibility of alternative healthcare financing mechanisms').

omega_variable(
    political_will_for_reform,
    'What is the level of political will to reform the ESI system, given the entrenched interests of employers, insurers, and employees?',
    'Analysis of legislative proposals, public opinion polls, and lobbying efforts.',
    'If high: comprehensive reform of the ESI system could be possible. If low: incremental changes are more likely, but may not address the fundamental problems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_for_reform, preference, 'Political will to reform the ESI system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_employer_health_insurance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_e_tr_t0, us_employer_health_insurance, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_e_tr_t10, us_employer_health_insurance, theater_ratio, 10, 0.3).
narrative_ontology:measurement(us_e_tr_t20, us_employer_health_insurance, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(us_e_be_t0, us_employer_health_insurance, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(us_e_be_t10, us_employer_health_insurance, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(us_e_be_t20, us_employer_health_insurance, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_employer_health_insurance, resource_allocation).
narrative_ontology:affects_constraint(us_employer_health_insurance, aca_individual_mandate).
narrative_ontology:affects_constraint(us_employer_health_insurance, medicaid_expansion).

% DUAL FORMULATION NOTE:
% The ESI system and related policies (ACA, Medicaid expansion) represent interconnected constraints on healthcare access. The ESI system is the dominant structure; the other policies attempt to mitigate its flaws or provide alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
