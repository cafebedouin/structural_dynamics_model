% ============================================================================
% CONSTRAINT STORY: constitutional_consecration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_consecration, []).

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
 *   constraint_id: constitutional_consecration
 *   human_readable: The Proposition of Equality as a Binding Sacrifice
 *   domain: political/legal
 *
 * SUMMARY:
 *   Lincoln's Gettysburg Address re-frames the American constitutional
 *   project not as a mere legal compact, but as a "proposition" that all men
 *   are created equal. This reframing elevates the ideal of equality to a
 *   foundational principle, but also introduces a binding sacrifice: the
 *   commitment to this ideal demands ongoing effort, vigilance, and
 *   willingness to confront uncomfortable truths about the nation's past and
 *   present. The degree to which this "proposition" is actually realized
 *   varies significantly depending on one's position in society.
 *
 * KEY AGENTS:
 *   - Disenfranchised Minorities: Primary target (powerless/trapped) – experience the inequality gap directly.
 *   - Political Elites: Primary beneficiary (institutional/arbitrage) – use the equality proposition for legitimacy.
 *   - Unpopular Political Movements: Secondary actors (moderate/constrained) – challenge the elite vision.
 *   - Rhetorical Legitimacy: Beneficiary (institutional/arbitrage). Provides a common discourse for political communication.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_consecration, 0.6).
domain_priors:suppression_score(constitutional_consecration, 0.7).
domain_priors:theater_ratio(constitutional_consecration, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_consecration, extractiveness, 0.6).
narrative_ontology:constraint_metric(constitutional_consecration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_consecration, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_consecration, tangled_rope).
narrative_ontology:human_readable(constitutional_consecration, "The Proposition of Equality as a Binding Sacrifice").
narrative_ontology:topic_domain(constitutional_consecration, "political/legal").

domain_priors:requires_active_enforcement(constitutional_consecration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_consecration, political_elites).
narrative_ontology:constraint_beneficiary(constitutional_consecration, rhetorical_legitimacy).
narrative_ontology:constraint_victim(constitutional_consecration, disenfranchised_minorities).
narrative_ontology:constraint_victim(constitutional_consecration, unpopular_political_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Disenfranchised Minorities (Snare). For groups historically excluded or marginalized (e.g., enslaved people, later African Americans during Jim Crow, women before suffrage) the proposition of equality can feel like a cruel snare. They bear the cost of the ideal without receiving its benefits, and are often actively suppressed for challenging the status quo.
constraint_indexing:constraint_classification(constitutional_consecration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Unpopular Political Movements (Tangled Rope). Groups advocating for radical change (e.g., abolitionists, socialists, civil rights activists) may both benefit from the ideal of equality as a rhetorical tool and be constrained by its selective enforcement and the backlash it provokes. They are constrained because challenging the elite vision invokes severe penalties.
constraint_indexing:constraint_classification(constitutional_consecration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Political Elites (Rope). For those in power, the proposition of equality can serve as a useful tool for maintaining legitimacy and social cohesion. It provides a justification for existing power structures while allowing for incremental reforms that do not fundamentally threaten their position. This is a coordination device to keep the political system stable and maintain power.
constraint_indexing:constraint_classification(constitutional_consecration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: Analytical Observer (Tangled Rope). From a detached perspective, the proposition of equality is neither a pure ideal nor a complete sham. It is a complex and contradictory force, simultaneously inspiring progress and masking injustice. It requires ongoing active enforcement to overcome its defects.
constraint_indexing:constraint_classification(constitutional_consecration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 5: Rhetorical Legitimacy (Rope). The idea of equality provides a common vocabulary and framework for political discourse, enabling communication and negotiation between different groups. The elites are the beneficiaries, and thus see this as a rope device.
constraint_indexing:constraint_classification(constitutional_consecration, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_consecration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_consecration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_consecration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_consecration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_consecration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint extracts from those who are not truly equal, while benefitting the elites who maintain the system. Suppression is high because the system actively works to deny equality to some. Theater is low because there is genuine work to improve equality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives vary based on the agent's position relative to the ideal of equality. Disenfranchised minorities experience it as a snare, while political elites see it as a coordination mechanism. The analytical observer sees the tangled rope, a mix of aspiration and injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by who benefits from and who is harmed by the proposition of equality. Political elites and the existing power structure benefit, while marginalized groups bear the costs of its uneven enforcement. The d value varies based on the agent's ability to exit the system and challenge the status quo.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_equality_achievability,
    'Is true equality realistically achievable, or is it an asymptotic ideal that can only be approached but never fully realized?',
    'Historical analysis of attempts to achieve equality in different societies; philosophical debates about the nature of equality and justice.',
    'If achievable: current disparities represent a failure of implementation. If asymptotic: ongoing efforts to improve equality are always necessary but never sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_equality_achievability, conceptual, 'Whether true equality is achievable or asymptotic.').

omega_variable(
    inherent_human_inequality,
    'To what extent are human beings inherently unequal in terms of abilities, talents, and motivations, and how should these inequalities be addressed by a just society?',
    'Scientific research on human variation; philosophical debates about meritocracy vs. egalitarianism.',
    'If inherent inequalities are significant: some degree of social stratification is inevitable. If inherent inequalities are minimal: greater efforts should be made to eliminate social barriers and promote equal opportunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_human_inequality, empirical, 'The significance of inherent human inequality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_consecration, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_consecration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cons_tr_t50, constitutional_consecration, theater_ratio, 50, 0.3).
narrative_ontology:measurement(cons_tr_t100, constitutional_consecration, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_consecration, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t50, constitutional_consecration, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(cons_be_t100, constitutional_consecration, base_extractiveness, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_consecration, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_consecration, equal_protection_clause).
narrative_ontology:affects_constraint(constitutional_consecration, affirmative_action_policies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
