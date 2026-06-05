% ============================================================================
% CONSTRAINT STORY: non_compete_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_non_compete_agreements, []).

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
 *   constraint_id: non_compete_agreements
 *   human_readable: Non-Compete Agreements (Post-Employment Restraint)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Non-compete agreements are contracts that restrict an employee's ability
 *   to work for a competitor after leaving a company. These agreements aim to
 *   protect trade secrets and customer relationships, but they also limit
 *   employee mobility and can stifle innovation. The effectiveness and
 *   fairness of non-competes are subject to ongoing debate, varying legal
 *   interpretations, and differing impacts on various stakeholders.
 *
 * KEY AGENTS:
 *   - Former Employees: Primary target (powerless/trapped) — bear the costs of reduced job opportunities and wage suppression.
 *   - Former Employers: Primary beneficiary (institutional/arbitrage) — gain protection of trade secrets and reduced competition.
 *   - New Entrant Firms: Secondary actor (moderate/constrained) — constrained by the reduced talent pool, but may benefit from reduced overall competition.
 *   - Incumbent Firms: Organized actor (organized/constrained) - Benefit from limiting the outflow of talent but restricted by the inflow of external experts.
 *   - Analytical Observer: Evaluates overall impact (analytical/analytical) — balances the coordination and extraction aspects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(non_compete_agreements, 0.55).
domain_priors:suppression_score(non_compete_agreements, 0.65).
domain_priors:theater_ratio(non_compete_agreements, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(non_compete_agreements, extractiveness, 0.55).
narrative_ontology:constraint_metric(non_compete_agreements, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(non_compete_agreements, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(non_compete_agreements, tangled_rope).
narrative_ontology:human_readable(non_compete_agreements, "Non-Compete Agreements (Post-Employment Restraint)").
narrative_ontology:topic_domain(non_compete_agreements, "economic/political").

domain_priors:requires_active_enforcement(non_compete_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(non_compete_agreements, former_employers).
narrative_ontology:constraint_beneficiary(non_compete_agreements, incumbent_firms).
narrative_ontology:constraint_victim(non_compete_agreements, former_employees).
narrative_ontology:constraint_victim(non_compete_agreements, new_entrant_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMER EMPLOYEE (SNARE) — Limited mobility due to non-compete; may face legal action. Has limited ability to arbitrage the agreement and is largely trapped. Bears the costs of suppressed wages and career stagnation.
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FORMER EMPLOYER (ROPE) — Benefits from reduced competition, protection of trade secrets, and stability. Arbitrage through defining the scope and duration. Experiences as coordination to protect their investment.
constraint_indexing:constraint_classification(non_compete_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NEW ENTRANT FIRM (TANGLED ROPE) — Limited access to talent pool but may benefit from reduced overall competition. Constrained by needing to work around existing non-competes. Mixed coordination and extraction.
constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INCUMBENT FIRMS (TANGLED ROPE) - Non-competes limit the outflow of talent to competitors, but also restrict the inflow of external expertise. Benefit from reduced competition, but are constrained by limited ability to hire talent constrained by existing agreements. Exhibits both coordination and asymmetric extraction.
constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the overall structure as a hybrid of coordination (protecting trade secrets) and extraction (limiting labor mobility) at a civilizational scale. The effective extraction depends on the enforcement regime and the state of the labor market.
constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(non_compete_agreements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(non_compete_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(non_compete_agreements, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(non_compete_agreements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(non_compete_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Non-competes extract value from former employees by limiting their job options. This extraction is somewhat mitigated by the employee's ability to negotiate the terms of the agreement and the limitations on its enforceability. Suppression (0.65): Moderate-high. The legal enforceability of non-competes significantly suppresses employee mobility, but not entirely due to the constraints of legal interpretation and enforcement costs. Theater Ratio (0.20): Low. Non-competes are primarily functional, with less emphasis on performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The former employee experiences the non-compete as a snare, limiting their job options and potentially depressing wages. The former employer views it as a rope, a necessary mechanism to protect their trade secrets and investments. New entrant firms see a tangled rope - they are limited by non-competes when hiring, but also benefit from reduced competition from larger firms. The incumbent firms also see a tangled rope; they benefit from reducing the flow of workers to competitors, but also face problems attracting workers who are constrained by existing non-competes. The analytical observer sees a tangled rope overall, recognizing both the legitimate need to protect trade secrets and the potential for abuse and suppression of labor mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) values are derived from the structural position of each agent. Former employees are victims with limited exit options, so their d value is high. Former employers are beneficiaries with arbitrage options, so their d value is low. New entrant firms have mixed effects with some constraints to exit options, resulting in intermediate values of d.
 *
 * MANDATROPHY ANALYSIS:
 *   The DR classification prevents mislabeling non-competes as pure extraction by recognizing their coordination function in protecting trade secrets and incentivizing investment. It also avoids mislabeling them as pure coordination by acknowledging their potential to suppress wages and limit labor mobility. The correct classification is Tangled Rope because it includes both the asymmetric extraction and the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_intensity,
    'How aggressively are non-compete agreements enforced?',
    'Legal precedent analysis; tracking litigation rates; surveying employee perceptions of enforcement risk',
    'High enforcement: snare classification strengthened. Low enforcement: tangled rope classification weakens towards rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intensity, empirical, 'Intensity of non-compete enforcement').

omega_variable(
    labor_market_fluidity,
    'How fluid is the labor market within the specified industry?',
    'Tracking job switching rates; analyzing employer concentration; surveying employee mobility options',
    'Low fluidity: snare classification strengthened. High fluidity: tangled rope classification weakens towards rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_fluidity, empirical, 'Fluidity of the labor market').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(non_compete_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(non__tr_t0, non_compete_agreements, theater_ratio, 0, 0.1).
narrative_ontology:measurement(non__tr_t5, non_compete_agreements, theater_ratio, 5, 0.15).
narrative_ontology:measurement(non__tr_t10, non_compete_agreements, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(non__be_t0, non_compete_agreements, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(non__be_t5, non_compete_agreements, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(non__be_t10, non_compete_agreements, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(non_compete_agreements, enforcement_mechanism).
narrative_ontology:affects_constraint(non_compete_agreements, trade_secret_protection).
narrative_ontology:affects_constraint(non_compete_agreements, labor_market_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
