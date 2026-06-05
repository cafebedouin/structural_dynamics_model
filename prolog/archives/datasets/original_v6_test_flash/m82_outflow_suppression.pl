% ============================================================================
% CONSTRAINT STORY: m82_outflow_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_m82_outflow_suppression, []).

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
 *   constraint_id: m82_outflow_suppression
 *   human_readable: M82* Explosive Outflow and Star Formation Suppression
 *   domain: physical/astronomical
 *
 * SUMMARY:
 *   Recent observations of the supermassive black hole (SMBH) M82* revealed
 *   an unprecedented phenomenon: instead of accreting matter, it is violently
 *   expelling it. This outflow suppresses star formation in the immediate
 *   vicinity and regulates the black hole's growth. The constraint manifests
 *   as a tangled rope, balancing between extraction and coordination.
 *
 * KEY AGENTS:
 *   - M82 Galaxy Stability: Primary beneficiary (institutional/arbitrage) - Benefits from regulated star formation and prevents SMBH from overgrowing
 *   - Local Star Formation: Primary victim (powerless/trapped) - Suppressed by the outflow
 *   - Gas Cloud Accretion: Secondary victim (moderate/constrained) - Hindered accretion onto the SMBH
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(m82_outflow_suppression, 0.55).
domain_priors:suppression_score(m82_outflow_suppression, 0.7).
domain_priors:theater_ratio(m82_outflow_suppression, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(m82_outflow_suppression, extractiveness, 0.55).
narrative_ontology:constraint_metric(m82_outflow_suppression, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(m82_outflow_suppression, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(m82_outflow_suppression, tangled_rope).
narrative_ontology:human_readable(m82_outflow_suppression, "M82* Explosive Outflow and Star Formation Suppression").
narrative_ontology:topic_domain(m82_outflow_suppression, "physical/astronomical").

domain_priors:requires_active_enforcement(m82_outflow_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(m82_outflow_suppression, m82_galaxy_stability).
narrative_ontology:constraint_victim(m82_outflow_suppression, local_star_formation).
narrative_ontology:constraint_victim(m82_outflow_suppression, gas_cloud_accretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Local star formation is suppressed by the outflow, limiting the potential for new star birth in the immediate vicinity. They cannot escape the influence of the SMBH.
constraint_indexing:constraint_classification(m82_outflow_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Gas clouds that would normally accrete onto the SMBH are now being pushed away, hindering the black hole's growth but contributing to galactic stability by limiting the SMBH's growth. They are constrained by the SMBH's outflow but benefit from the overall stability.
constraint_indexing:constraint_classification(m82_outflow_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The galaxy as a whole benefits from the outflow, which helps regulate star formation and prevents the SMBH from becoming too large and disruptive. The galaxy has agency and can adjust its behavior (star formation rate) over time to minimize any negative impacts, and benefits from stability.
constraint_indexing:constraint_classification(m82_outflow_suppression, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the M82* outflow represents a complex interplay between the SMBH and its host galaxy, where the outflow acts as both a suppressor of star formation and a regulator of SMBH growth, leading to an overall stabilization of the galaxy. The analytic observer recognizes both the suppression and the coordination aspects.
constraint_indexing:constraint_classification(m82_outflow_suppression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(m82_outflow_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(m82_outflow_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(m82_outflow_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(m82_outflow_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(m82_outflow_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The outflow extracts from local star formation and gas cloud accretion, but the galaxy benefits from the overall regulation. Suppression (0.70): High. The outflow strongly suppresses local star formation and accretion onto the SMBH. Theater Ratio (0.20): Low. The outflow is a genuine physical phenomenon with minimal performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Local star formation sees the constraint as a snare, as it is directly suppressed by the outflow. Gas cloud accretion sees a tangled rope, as they are constrained by the outflow but benefit from the overall galactic stability. The galaxy itself sees a rope, as it benefits from the regulation of star formation and SMBH growth. The Analytical Observer sees the full picture, recognizing the complex interplay between the SMBH and its host galaxy.
 *
 * DIRECTIONALITY LOGIC:
 *   The galaxy as a whole benefits from the outflow, which helps regulate star formation and prevents the SMBH from becoming too large and disruptive. The galaxy has agency and can adjust its behavior (star formation rate) over time to minimize any negative impacts. In contrast, the local star formation experiences a snare, where they are trapped within the confines of the outflow and are directly suppressed, limiting the potential for new star birth in the immediate vicinity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outflow_mechanism,
    'What is the precise mechanism driving the outflow?',
    'Detailed simulations and observations of the SMBH''s environment and outflow dynamics',
    'Understanding the mechanism is crucial for determining the outflow''s long-term impact on the galaxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outflow_mechanism, empirical, 'Mechanism driving outflow').

omega_variable(
    long_term_stability,
    'Will the outflow eventually cease, leading to renewed accretion and star formation?',
    'Long-term monitoring of the SMBH and its host galaxy to observe any changes in outflow activity and star formation rates',
    'The long-term stability of the galaxy depends on the outflow''s sustained activity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_stability, empirical, 'Long-term stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(m82_outflow_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(m82__tr_t0, m82_outflow_suppression, theater_ratio, 0, 0.1).
narrative_ontology:measurement(m82__tr_t5, m82_outflow_suppression, theater_ratio, 5, 0.15).
narrative_ontology:measurement(m82__tr_t10, m82_outflow_suppression, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(m82__be_t0, m82_outflow_suppression, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(m82__be_t5, m82_outflow_suppression, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(m82__be_t10, m82_outflow_suppression, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(m82_outflow_suppression, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
