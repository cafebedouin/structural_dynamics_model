% ============================================================================
% CONSTRAINT STORY: sludge_bureaucratic_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sludge_bureaucratic_friction, []).

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
 *   constraint_id: sludge_bureaucratic_friction
 *   human_readable: Sludge (Intentional Administrative Friction)
 *   domain: political/economic
 *
 * SUMMARY:
 *   "Sludge" describes intentional administrative burdens—excessive
 *   paperwork, long wait times, complex requirements—that create friction,
 *   discouraging access to benefits, rights, or services. This friction acts
 *   as a snare for potential recipients, especially those with limited
 *   resources. Simultaneously, the sludge benefits administrative agencies by
 *   reducing demand and incumbents by reducing competition.
 *
 * KEY AGENTS:
 *   - Benefit Recipients: Primary victim (powerless/trapped) - bear the brunt of administrative burden.
 *   - Small Businesses: Secondary victim (moderate/constrained) - face administrative hurdles that increase costs but also provide a small competitive advantage.
 *   - Administrative Agencies: Primary beneficiary (institutional/arbitrage) - benefit from reduced demand and streamlined operations.
 *   - Incumbent Service Providers: Secondary beneficiary (institutional/constrained) - benefit from reduced competition due to higher entry barriers.
 *   - Analytical Observer: Assesses overall system (analytical/analytical) - observes mixed coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sludge_bureaucratic_friction, 0.65).
domain_priors:suppression_score(sludge_bureaucratic_friction, 0.7).
domain_priors:theater_ratio(sludge_bureaucratic_friction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, extractiveness, 0.65).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sludge_bureaucratic_friction, snare).
narrative_ontology:human_readable(sludge_bureaucratic_friction, "Sludge (Intentional Administrative Friction)").
narrative_ontology:topic_domain(sludge_bureaucratic_friction, "political/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, incumbent_service_providers).
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, administrative_agencies).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, benefit_recipients).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual attempting to access benefits faces significant administrative burdens, often with limited resources or knowledge to navigate the system, leading to a sense of being trapped.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Small businesses attempting to comply with regulations face administrative hurdles and paperwork, which is burdensome but also provides them with a limited competitive advantage over even smaller entities.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Administrative agencies benefit from sludge as it reduces demand and simplifies their workload. They have arbitrage in the sense that they can shift burden elsewhere.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Incumbent service providers benefit from the reduction in competition caused by sludge. However, the system is not actively maintained as a snare. It is a piton because inertia maintains the friction.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a broad perspective, sludge is a tangled rope. It provides some coordination (agencies can more efficiently manage services), but extracts heavily from those who are intended to benefit.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sludge_bureaucratic_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sludge_bureaucratic_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sludge_bureaucratic_friction, TR),
    TR >= 0.70.

:- end_tests(sludge_bureaucratic_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The administrative burden extracts significantly from potential benefit recipients, discouraging access and imposing real costs. Suppression (0.70): High. The complexity and difficulty of navigating the system suppress access and alternatives. Theater Ratio (0.30): Low. Most of the burden is functional in reducing access, not performative. The claimed type is Snare because the primary effect is extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is driven by the differing positions in relation to the sludge. Benefit recipients see only the negative extraction, while administrative agencies experience efficiency gains. Small businesses face a mixed outcome, experiencing increased burden but also limited competition. The analytical observer sees both coordination and extraction but is more strongly weighted toward the extraction as victims are trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's power, exit options, and relationship to the benefit. Benefit recipients are powerless and trapped, so they experience high directionality and thus high extraction. Agencies have some arbitrage (they can alter rules), so they experience low directionality (negative chi, near rope), and incumbents are benefited by less competition (so also experience a positive directionality, near rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a Snare due to the high extractiveness and suppression. The mandatrophy resolution involves distinguishing between genuine optimization needs and intentional discouragement. By considering the impact on different agents, we avoid mislabeling a complex system as a pure coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_optimization_vs_user_burden,
    'To what extent is the administrative complexity due to genuine optimization needs versus intentional discouragement of access?',
    'Cost-benefit analysis of each administrative step, comparing agency efficiency gains with user burden costs.',
    'If mostly optimization: Tangled Rope (lower extractiveness). If mostly discouragement: Snare (higher extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_optimization_vs_user_burden, empirical, 'Is administrative friction due to optimization or discouragement?').

omega_variable(
    alternative_access_cost,
    'What are the costs (time, resources) of accessing the benefit/right/service through alternative channels (e.g., private services, legal aid)?',
    'Market research and surveys to quantify the cost of alternative access options.',
    'High costs: Snare (limited exit). Low costs: Tangled Rope (greater exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_access_cost, empirical, 'Costs of accessing the benefit through alternative channels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sludge_bureaucratic_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slud_tr_t0, sludge_bureaucratic_friction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(slud_tr_t5, sludge_bureaucratic_friction, theater_ratio, 5, 0.35).
narrative_ontology:measurement(slud_tr_t10, sludge_bureaucratic_friction, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(slud_be_t0, sludge_bureaucratic_friction, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(slud_be_t5, sludge_bureaucratic_friction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(slud_be_t10, sludge_bureaucratic_friction, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sludge_bureaucratic_friction, enforcement_mechanism).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, regulatory_capture).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, information_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
