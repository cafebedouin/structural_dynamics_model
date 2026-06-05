% ============================================================================
% CONSTRAINT STORY: nuclear_order_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_order_2026, []).

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
 *   constraint_id: nuclear_order_2026
 *   human_readable: Post-New START Strategic Ambiguity
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   The expiration of the New START treaty has transitioned the global
 *   nuclear framework from a regulated system of verification and
 *   transparency to a more ambiguous and potentially unstable environment.
 *   This shift increases the risk of miscalculation and escalation, as
 *   nuclear powers have greater freedom to deploy and develop weapons without
 *   constraint. The absence of a formal agreement also weakens international
 *   norms against nuclear proliferation, placing additional burdens on
 *   non-nuclear weapon states. While nuclear weapon states may perceive
 *   short-term gains in strategic flexibility, the long-term implications for
 *   global security are negative.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States: Primary beneficiaries (powerful/constrained) - increased strategic flexibility but also greater risk of miscalculation.
 *   - Non-Nuclear Weapon States: Primary victims (powerless/trapped) - bear the risks of escalation and proliferation.
 *   - International Atomic Energy Agency (IAEA): Institutional actor (institutional/constrained) - monitoring role diminished, but still performing theatrical function.
 *   - Global Security: Abstract collective (powerless/trapped) - bears the ultimate risk of nuclear conflict.
 *   - Analytical Observer: Independent observer (analytical/analytical) - assesses the long-term consequences of strategic ambiguity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_order_2026, 0.5).
domain_priors:suppression_score(nuclear_order_2026, 0.6).
domain_priors:theater_ratio(nuclear_order_2026, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_order_2026, extractiveness, 0.5).
narrative_ontology:constraint_metric(nuclear_order_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(nuclear_order_2026, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_order_2026, tangled_rope).
narrative_ontology:human_readable(nuclear_order_2026, "Post-New START Strategic Ambiguity").
narrative_ontology:topic_domain(nuclear_order_2026, "geopolitical/technological").

domain_priors:requires_active_enforcement(nuclear_order_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_order_2026, nuclear_weapon_states).
narrative_ontology:constraint_victim(nuclear_order_2026, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(nuclear_order_2026, global_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Non-nuclear weapon states are trapped in a global system where nuclear powers have more freedom. They bear the risks of escalation and proliferation with limited ability to influence outcomes.
constraint_indexing:constraint_classification(nuclear_order_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Nuclear weapon states benefit from increased strategic flexibility but are also constrained by the potential for miscalculation and escalation. They gain in immediate power but risk long-term instability.
constraint_indexing:constraint_classification(nuclear_order_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational perspective, the shift represents a move from a rules-based order to a more uncertain environment where the risk of nuclear conflict increases due to a lack of transparency and verification.
constraint_indexing:constraint_classification(nuclear_order_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The IAEA retains a monitoring role, but its verification powers are diminished without the New START treaty. It continues to perform its theatrical function, but with less effective control, which reduces its perceived effectiveness.
constraint_indexing:constraint_classification(nuclear_order_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_order_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_order_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_order_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_order_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_order_2026, TR),
    TR >= 0.70.

:- end_tests(nuclear_order_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): The absence of New START allows nuclear powers to extract greater strategic advantage and autonomy, increasing risks for other states. Suppression (0.60): High, the lack of treaty enforcement suppresses alternative diplomatic solutions, heightening the power of actors with nuclear weapons. Theater ratio (0.40): Some degree of theatrical compliance persists with international inspections. The nuclear weapon states continue to give the appearance of commitment to non-proliferation without substantive checks, so this remains a factor but less than perfect. This represents an increase in overall extractiveness compared to pre-2026 and, consequently, greater overall risk.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states see a more fluid strategic environment with potential benefits, non-nuclear weapon states experience increased vulnerability and risk, and analytical observers perceive a reduction in global stability. The IAEA's power is diminished; it goes from rope to piton, where its function is more theatrical.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states benefit from increased freedom of action, while non-nuclear states bear the brunt of the increased risks. IAEA's role is weakened, even if it nominally continues. Global security, as an abstract entity, is powerless and trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-New START environment cannot be mistaken for a legitimate coordination agreement. While there may be some informal coordination among nuclear powers, the absence of verification mechanisms leads to greater ambiguity and risk. Therefore, it is a Tangled Rope with asymmetric extraction and increasing risk of escalation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    great_power_rivalry_intensity,
    'How intense will the great power rivalry be in the absence of arms control treaties?',
    'Geopolitical risk assessments; military spending trends; frequency of near-miss incidents.',
    'Higher rivalry = increased probability of nuclear conflict (Snare). Lower rivalry = potential for informal arms control (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_rivalry_intensity, empirical, 'Level of tension between great powers impacting nuclear stability.').

omega_variable(
    verification_technology_advancement,
    'How rapidly will new technologies for verification (e.g., satellite surveillance, AI analysis) develop and be deployed?',
    'Technological forecasting; open-source intelligence analysis; expert surveys.',
    'Rapid advancement = increased transparency and reduced strategic ambiguity (Rope). Slow advancement = continued uncertainty and increased risk of miscalculation (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_technology_advancement, empirical, 'Technological impact on verification capabilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_order_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_order_2026, theater_ratio, 0, 0.6).
narrative_ontology:measurement(nucl_tr_t5, nuclear_order_2026, theater_ratio, 5, 0.4).
narrative_ontology:measurement(nucl_tr_t10, nuclear_order_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_order_2026, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nucl_be_t5, nuclear_order_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(nucl_be_t10, nuclear_order_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_order_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_order_2026, non_proliferation_treaty).
narrative_ontology:affects_constraint(nuclear_order_2026, iran_nuclear_deal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
