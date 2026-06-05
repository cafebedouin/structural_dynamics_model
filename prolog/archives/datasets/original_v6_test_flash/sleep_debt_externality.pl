% ============================================================================
% CONSTRAINT STORY: sleep_debt_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sleep_debt_externality, []).

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
 *   constraint_id: sleep_debt_externality
 *   human_readable: The Circadian Siphon
 *   domain: biological/economic/social
 *
 * SUMMARY:
 *   The Circadian Siphon represents the systemic extraction of biological
 *   rest to fuel 24/7 economic and digital activity. This extraction is
 *   facilitated by societal norms, economic pressures, and the design of
 *   digital platforms, leading to widespread sleep deprivation and its
 *   associated health and social costs.
 *
 * KEY AGENTS:
 *   - Shift Workers: Primary target (powerless/trapped) - Bears high extraction due to job requirements.
 *   - General Population: Secondary target (moderate/constrained) - Experiences a subtle extraction due to societal pressures.
 *   - Corporations: Primary beneficiary (institutional/arbitrage) - Benefits from increased worker output.
 *   - Digital Platforms: Secondary beneficiary (institutional/arbitrage) - Benefits from increased user engagement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sleep_debt_externality, 0.65).
domain_priors:suppression_score(sleep_debt_externality, 0.7).
domain_priors:theater_ratio(sleep_debt_externality, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sleep_debt_externality, extractiveness, 0.65).
narrative_ontology:constraint_metric(sleep_debt_externality, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sleep_debt_externality, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sleep_debt_externality, tangled_rope).
narrative_ontology:human_readable(sleep_debt_externality, "The Circadian Siphon").
narrative_ontology:topic_domain(sleep_debt_externality, "biological/economic/social").

domain_priors:requires_active_enforcement(sleep_debt_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sleep_debt_externality, corporations).
narrative_ontology:constraint_beneficiary(sleep_debt_externality, digital_platforms).
narrative_ontology:constraint_victim(sleep_debt_externality, shift_workers).
narrative_ontology:constraint_victim(sleep_debt_externality, general_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Shift workers are often trapped by economic necessity into schedules that disrupt their natural sleep cycles. They experience high extraction with little to no benefit.
constraint_indexing:constraint_classification(sleep_debt_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The general population is subtly coerced into sacrificing sleep due to societal expectations of constant availability and productivity. Benefits are diffuse and hard to quantify, while costs accumulate over time.
constraint_indexing:constraint_classification(sleep_debt_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Corporations benefit directly from extended operating hours and increased worker output, but face the internal cost of decreased worker productivity and increased healthcare expenses related to sleep deprivation. The benefit outweighs the cost, resulting in a classification as Rope.
constraint_indexing:constraint_classification(sleep_debt_externality, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Digital platforms benefit greatly from increased user engagement, which is heavily influenced by 24/7 accessibility. The costs are largely externalized, making the constraint a Rope from the perspective of the platforms.
constraint_indexing:constraint_classification(sleep_debt_externality, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analyical observer sees a tangled rope: a complex interplay of economic incentives, societal pressures, and biological limitations that extracts biological rest to fuel economic activity. Extraction is not total, as individuals can still make choices to prioritize sleep, and the overall system is held together by the coorindation afforded by economic growth.
constraint_indexing:constraint_classification(sleep_debt_externality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sleep_debt_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sleep_debt_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sleep_debt_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sleep_debt_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sleep_debt_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High, reflects the significant portion of biological rest extracted by the system. Suppression (0.70): High, reflecting the societal and economic barriers to resisting the demands of 24/7 activity. Theater Ratio (0.30): Low, representing a small proportion of performative activity in relation to the actual extraction of sleep.
 *
 * PERSPECTIVAL GAP:
 *   Shift workers, trapped in their schedules, experience the constraint as a snare. Corporations and digital platforms, benefiting from increased productivity and engagement, view the constraint as a rope. The general population, facing subtle pressure and societal expectations, experiences the constraint as a tangled rope. The analytical observer sees the aggregate effect as a system extracting biological rest for economic gain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (corporations and digital platforms) have arbitrage exit options and thus lower perceived extraction. Victims (shift workers and general population) have limited exit options due to economic or societal pressures, resulting in higher extraction. Shift workers are powerless with trapped exit options, giving them highest directional cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_agency_vs_systemic_pressure,
    'To what extent can individuals resist the pressure to sacrifice sleep for economic or social demands?',
    'Sociological studies, psychological experiments, and economic analyses examining the determinants of sleep behavior.',
    'If individual agency is high, the constraint is weaker and may be classified as a tangled rope. If systemic pressure dominates, the constraint strengthens towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_agency_vs_systemic_pressure, empirical, 'The balance between individual autonomy and societal pressure in determining sleep patterns.').

omega_variable(
    long_term_health_consequences,
    'What are the long-term health consequences of chronic sleep deprivation on individuals and society?',
    'Epidemiological studies and medical research tracking the health outcomes of populations with different sleep patterns.',
    'If the health consequences are severe, the extractiveness of the constraint is higher, potentially leading to a snare classification. If consequences are mild, the constraint remains a tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_term_health_consequences, empirical, 'The cumulative impact of chronic sleep loss on health and well-being.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sleep_debt_externality, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slee_tr_t0, sleep_debt_externality, theater_ratio, 0, 0.15).
narrative_ontology:measurement(slee_tr_t10, sleep_debt_externality, theater_ratio, 10, 0.22).
narrative_ontology:measurement(slee_tr_t20, sleep_debt_externality, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(slee_be_t0, sleep_debt_externality, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(slee_be_t10, sleep_debt_externality, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(slee_be_t20, sleep_debt_externality, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sleep_debt_externality, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
