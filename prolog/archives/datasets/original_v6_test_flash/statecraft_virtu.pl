% ============================================================================
% CONSTRAINT STORY: statecraft_virtu
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statecraft_virtu, []).

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
 *   constraint_id: statecraft_virtu
 *   human_readable: Machiavellian Virtù and State Maintenance
 *   domain: political
 *
 * SUMMARY:
 *   Machiavellian virtù is the set of strategic constraints a ruler imposes
 *   to maintain power and order. It involves a mix of coordination
 *   (establishing laws, defending the state) and extraction (suppressing
 *   dissent, extracting resources). Different agents experience this system
 *   differently.
 *
 * KEY AGENTS:
 *   - The Prince: Primary beneficiary (institutional/arbitrage) – benefits from order and stability.
 *   - The Ruling Elite: Secondary beneficiary (institutional/constrained) – benefits from the state's power, but is also subject to its control.
 *   - The Oppressed Citizen: Primary victim (powerless/trapped) – bears the cost of suppression and extraction.
 *   - The Ambitious Noble: Secondary victim (moderate/constrained) – faces risk and uncertainty in the Prince's court
 *   - State Stability: The abstract collective good (institutional/constrained) benefits from the enforcement of constraints.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statecraft_virtu, 0.55).
domain_priors:suppression_score(statecraft_virtu, 0.65).
domain_priors:theater_ratio(statecraft_virtu, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statecraft_virtu, extractiveness, 0.55).
narrative_ontology:constraint_metric(statecraft_virtu, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(statecraft_virtu, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statecraft_virtu, tangled_rope).
narrative_ontology:human_readable(statecraft_virtu, "Machiavellian Virtù and State Maintenance").
narrative_ontology:topic_domain(statecraft_virtu, "political").

domain_priors:requires_active_enforcement(statecraft_virtu).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statecraft_virtu, ruling_elite).
narrative_ontology:constraint_beneficiary(statecraft_virtu, state_stability).
narrative_ontology:constraint_victim(statecraft_virtu, individual_liberty).
narrative_ontology:constraint_victim(statecraft_virtu, political_opposition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The ordinary citizen who experiences the full force of the Prince's virtù. Lacking power and mobility, they are subject to the state's extractive and suppressive measures.
constraint_indexing:constraint_classification(statecraft_virtu, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The noble who must navigate the Prince's court, balancing loyalty and self-interest. They benefit from state stability but are also vulnerable to the Prince's caprice.
constraint_indexing:constraint_classification(statecraft_virtu, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The ruling class that benefits from the Prince's virtù, which maintains order and secures their privileged position. They use the system to their advantage.
constraint_indexing:constraint_classification(statecraft_virtu, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% A detached observer who sees the system as a whole, recognizing its mixed nature of coordination (maintaining order) and extraction (suppressing dissent).
constraint_indexing:constraint_classification(statecraft_virtu, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statecraft_virtu_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statecraft_virtu, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statecraft_virtu, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statecraft_virtu, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statecraft_virtu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The Prince extracts resources and compliance from the population to maintain power. Suppression (0.65): Moderate-high. Dissent is suppressed to maintain order and prevent challenges to the Prince's authority. Theater ratio (0.30): Low. The Prince prioritizes effective action over appearances, focusing on real power rather than theatrical displays.
 *
 * PERSPECTIVAL GAP:
 *   The oppressed citizen experiences the Prince's virtù as a snare, while the ruling elite experience it as a rope. The ambitious noble experiences it as a tangled rope, balancing opportunity and risk. The analytical observer recognizes the mixed nature of the system, seeing it as a tangled rope overall.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ruling_elite, state_stability) have low d-values and negative chi. Victims (individual_liberty, political_opposition) have high d-values and high chi. The Prince, as the enforcer of the system, occupies a unique position, with a moderate d-value reflecting the dual nature of virtù.
 *
 * MANDATROPHY ANALYSIS:
 *   This system is classified as a tangled rope because it combines coordination (maintaining order) and extraction (suppressing dissent). It is not a pure snare because it does provide some benefits to the population (security, stability). It is not a pure rope because it involves significant coercion and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold,
    'At what point does the Prince''s virtù become tyranny?',
    'Historical analysis of successful vs. failed states; correlation between repression and stability',
    'If threshold too low: state is brittle and prone to revolt. If threshold too high: state is inefficient and vulnerable to external threats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold, empirical, 'The balance between repression and stability').

omega_variable(
    alternative_governance_models,
    'Are there alternative models of governance that can achieve the same level of stability with less extraction?',
    'Comparative analysis of different political systems; case studies of states with strong institutions and citizen participation',
    'If alternatives exist: the Prince''s virtù is not necessary and may be harmful. If no alternatives exist: the Prince''s virtù is the lesser of two evils.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_models, conceptual, 'Alternatives to Machiavellian statecraft').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statecraft_virtu, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statecraft_virtu, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t5, statecraft_virtu, theater_ratio, 5, 0.2).
narrative_ontology:measurement(stat_tr_t10, statecraft_virtu, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statecraft_virtu, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(stat_be_t5, statecraft_virtu, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(stat_be_t10, statecraft_virtu, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statecraft_virtu, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
