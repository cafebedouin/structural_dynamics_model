% ============================================================================
% CONSTRAINT STORY: legitimacy_without_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_without_capacity, []).

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
 *   constraint_id: legitimacy_without_capacity
 *   human_readable: The Sovereign Ghost
 *   domain: political/organizational
 *
 * SUMMARY:
 *   The Sovereign Ghost describes a political or organizational scenario
 *   where an institution retains its formal legitimacy, including the social
 *   and legal 'right to rule,' but has functionally lost the capacity to
 *   provide essential services, maintain security, or ensure social order.
 *   This creates a disconnect between the institution's perceived authority
 *   and its actual effectiveness, leading to a system sustained by inertia,
 *   symbolism, and the self-interest of those who benefit from its continued
 *   existence. The system persists primarily through theater.
 *
 * KEY AGENTS:
 *   - General Population: Primary target (powerless/trapped) — bears the costs of institutional failure and lacks effective exit options.
 *   - Elite Power Holders: Primary beneficiary (institutional/constrained) — maintain power and privilege despite institutional failure.
 *   - Legacy Bureaucracy: Secondary beneficiary (institutional/constrained) — benefits from the status quo and inertia, even as the system degrades.
 *   - Emerging Service Providers: Ambivalent actor (moderate/mobile) — provide alternative services but face regulatory and other constraints.
 *   - Analytical Observer: Sees overall structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_without_capacity, 0.6).
domain_priors:suppression_score(legitimacy_without_capacity, 0.7).
domain_priors:theater_ratio(legitimacy_without_capacity, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_without_capacity, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimacy_without_capacity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_without_capacity, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_without_capacity, piton).
narrative_ontology:human_readable(legitimacy_without_capacity, "The Sovereign Ghost").
narrative_ontology:topic_domain(legitimacy_without_capacity, "political/organizational").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_without_capacity, elite_power_holders).
narrative_ontology:constraint_beneficiary(legitimacy_without_capacity, legacy_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_without_capacity, general_population).
narrative_ontology:constraint_victim(legitimacy_without_capacity, emerging_service_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL POPULATION (SNARE) — Trapped within the failing system; unable to easily exit due to economic or political constraints. Bear the costs of the institution's failure to provide services and security. Extraction is high as they are subject to the authority without receiving commensurate benefits.
constraint_indexing:constraint_classification(legitimacy_without_capacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ELITE POWER HOLDERS (PITON) — Primarily benefit from the inertia of the system, maintaining their positions of power and privilege even as the institution fails. See the system as degraded but maintain it for their own benefit.
constraint_indexing:constraint_classification(legitimacy_without_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGACY BUREAUCRACY (PITON) — Constrained by the existing system and benefiting from the status quo, even as it degrades. They see the system as largely performative, maintaining a facade of functionality.
constraint_indexing:constraint_classification(legitimacy_without_capacity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EMERGING SERVICE PROVIDERS (TANGLED ROPE) — Mobile and able to provide alternative services, but also constrained by the existing system's regulations and lack of support. They experience a mix of coordination and extraction. They benefit through new market access, while suffering from extraction through regulatory overhead.
constraint_indexing:constraint_classification(legitimacy_without_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the overall structure and how different agents are affected. They understand the dual nature of the system: maintaining stability while extracting from the population.
constraint_indexing:constraint_classification(legitimacy_without_capacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_without_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_capacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_without_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_without_capacity, TR),
    TR >= 0.70.

:- end_tests(legitimacy_without_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The institution extracts resources and obedience from the population but fails to provide commensurate benefits. Suppression (0.70): High. The institution actively suppresses alternative service providers and maintains a monopoly on authority. Theater ratio (0.80): Very High. The institution relies heavily on symbolism, rituals, and propaganda to maintain the facade of legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The general population experiences the system as a snare, trapped and bearing the costs of its failures. Elite power holders and legacy bureaucracy see a degraded but still beneficial piton. Emerging service providers experience a mixed bag of opportunities and constraints (tangled rope), while the analytical observer sees the overall picture as one of institutional decay sustained by extraction and theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the beneficiaries and victims declared. Elite power holders and legacy bureaucracy benefit from maintaining the status quo, even as the institution fails. The general population bears the cost. Emerging service providers experience a mix of coordination and extraction. The analytical observer sees the overall structure and how it affects different agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The core ambiguity is whether a system lacking capacity is merely inefficient (a rope needing optimization) or actively extractive (a snare sustained by suppression). The theater_ratio is diagnostic — the high value indicates that the institution is primarily maintained through performative activity. The existence of victims (general population) and beneficiaries (elite power holders) further clarifies the structure as a piton or tangled rope, not a rope. The system can't easily be improved through coordination. There are conflicting interests and deeply rooted systematic dysfunctions, causing the 'sovereign ghost' to remain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_of_failure,
    'At what point does the institution''s failure become irreversible, leading to collapse?',
    'Longitudinal data on service provision, security incidents, and citizen satisfaction',
    'Determines whether the institution can be reformed or must be replaced',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_of_failure, empirical, 'The threshold of institutional failure').

omega_variable(
    alternative_legitimacy_source,
    'What alternative sources of legitimacy could replace the failing institution?',
    'Analysis of social movements, community organizations, and external actors',
    'Indicates potential pathways for regime change or institutional reform',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimacy_source, conceptual, 'Potential sources of alternative legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_without_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_without_capacity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legi_tr_t5, legitimacy_without_capacity, theater_ratio, 5, 0.6).
narrative_ontology:measurement(legi_tr_t10, legitimacy_without_capacity, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_without_capacity, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t5, legitimacy_without_capacity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(legi_be_t10, legitimacy_without_capacity, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_without_capacity, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
