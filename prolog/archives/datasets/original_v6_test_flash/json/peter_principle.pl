% ============================================================================
% CONSTRAINT STORY: peter_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peter_principle, []).

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
 *   constraint_id: peter_principle
 *   human_readable: The Peter Principle (Promotion to Incompetence)
 *   domain: organizational/social
 *
 * SUMMARY:
 *   The Peter Principle describes the phenomenon where individuals in a
 *   hierarchy are promoted based on their success in previous roles, rather
 *   than their suitability for the new role. This leads to individuals
 *   eventually being promoted to a position where they are incompetent,
 *   resulting in organizational inefficiency and value extraction. Senior
 *   management may inadvertently perpetuate this effect through promotion
 *   practices that favor loyalty or seniority over competence. The theater
 *   ratio is high because promotions are often based on past performance
 *   rather than future potential, and performance reviews may not accurately
 *   reflect an employee's true capabilities.
 *
 * KEY AGENTS:
 *   - Incompetent Employees: The individuals who have reached their level of incompetence and are extracting value from the organization.
 *   - Senior Management: The decision-makers who are responsible for promotions and are potentially benefiting from the Peter Principle (loyalty, control), even if indirectly.
 *   - Organization: The entity that suffers from the decreased efficiency and innovation caused by the Peter Principle.
 *   - Competent Employees: The employees who are capable but are constrained by the incompetence of their superiors, potentially leading to frustration and turnover.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peter_principle, 0.6).
domain_priors:suppression_score(peter_principle, 0.4).
domain_priors:theater_ratio(peter_principle, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peter_principle, extractiveness, 0.6).
narrative_ontology:constraint_metric(peter_principle, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(peter_principle, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peter_principle, tangled_rope).
narrative_ontology:human_readable(peter_principle, "The Peter Principle (Promotion to Incompetence)").
narrative_ontology:topic_domain(peter_principle, "organizational/social").

domain_priors:requires_active_enforcement(peter_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peter_principle, incompetent_employees).
narrative_ontology:constraint_beneficiary(peter_principle, senior_management).
narrative_ontology:constraint_victim(peter_principle, organization).
narrative_ontology:constraint_victim(peter_principle, competent_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The organization suffers decreased efficiency and innovation, cannot easily remove the incompetent employee, and bears the cost of their salary and inaction.
constraint_indexing:constraint_classification(peter_principle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Competent employees are constrained by the incompetence of their superiors. They may be forced to pick up the slack, or leave the organization altogether. They see the Peter Principle as a degraded promotion system that no longer rewards merit.
constraint_indexing:constraint_classification(peter_principle, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Incompetent employees benefit from the Peter Principle by attaining a higher salary and status, despite their lack of ability. They are now insulated from job loss and have an incentive to maintain the status quo. They see the Peter Principle as a coordination mechanism for career advancement, even if they are not suited for the role.
constraint_indexing:constraint_classification(peter_principle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Senior Management benefits from the Peter Principle by maintaining their position of power and control. However, they are also constrained by the overall inefficiency of the organization. They see the Peter Principle as a mixed system that enables some advancement but also results in extraction from the organization as a whole.
constraint_indexing:constraint_classification(peter_principle, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational/global perspective, the Peter Principle is a tangled rope that extracts from the overall efficiency of organizations but is also a product of promotion systems that are not entirely merit-based. These systems still solve a coordination problem by incentivizing workers to seek higher roles and allowing organizations to move people through the ranks, even if some of those people prove to be incompetent.
constraint_indexing:constraint_classification(peter_principle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peter_principle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peter_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peter_principle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peter_principle, TR),
    TR >= 0.70.

:- end_tests(peter_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The Peter Principle results in a significant extraction of value from the organization, as incompetent employees are being paid a salary without providing commensurate value. Suppression (0.4): There is suppression because of organizational culture, difficulty in demoting people, and lack of objective performance metrics. Theater Ratio (0.75): There is significant theater in that promotion practices often reward performance metrics of the old role rather than aptitude for the new role. Performance reviews may be inflated or based on subjective criteria.
 *
 * PERSPECTIVAL GAP:
 *   The organization, as a whole, is negatively impacted (Snare). Competent employees feel constrained by the incompetence of their superiors and the limited opportunities (Piton). The incompetent employees, now insulated at their level, see this is a coordination and benefit structure (Rope). Senior Management are both beneficiaries and constrained by this practice (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The organization (powerless/trapped) bears the highest cost. The incompetent employees (institutional/arbitrage) benefit. Senior management (institutional/constrained) benefit from loyalty/control, but are constrained by the inefficiencies. The competent employees (moderate/constrained) are also constrained by the negative effects of incompetent management.
 *
 * MANDATROPHY ANALYSIS:
 *   The Peter Principle resolves the Mandatrophy by clarifying that while individual employees might benefit from promotions, the organization and competent employees are negatively affected. By considering different perspectives, the tangled rope designation resolves the tension of viewing the promotion process as purely beneficial or purely detrimental. The organization is extracted from overall, even as senior members make use of it. The high theater ratio indicates that the promotion process is not truly meritocratic, but rather a performance of meritocracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocracy_measurement,
    'How accurately can ''merit'' be measured for promotion decisions?',
    'Statistical analysis of performance reviews vs. objective metrics.',
    'High accuracy: reduced Peter Principle effect. Low accuracy: Peter Principle more prevalent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocracy_measurement, empirical, 'Quantifying the ability to assess and measure merit.').

omega_variable(
    incentive_alignment,
    'How well aligned are individual incentives with organizational goals?',
    'Survey of employee perception of organizational goals and whether their work advances them.',
    'High alignment: less extraction. Low alignment: more extraction and inefficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_alignment, empirical, 'Alignment of individual and organizational incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peter_principle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peter_tr_t0, peter_principle, theater_ratio, 0, 0.5).
narrative_ontology:measurement(peter_tr_t5, peter_principle, theater_ratio, 5, 0.6).
narrative_ontology:measurement(peter_tr_t10, peter_principle, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(peter_be_t0, peter_principle, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(peter_be_t5, peter_principle, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(peter_be_t10, peter_principle, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peter_principle, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
