% ============================================================================
% CONSTRAINT STORY: us_sdf_alliance_abandonment_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sdf_alliance_abandonment_2026, []).

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
 *   constraint_id: us_sdf_alliance_abandonment_2026
 *   human_readable: US Strategic Alliance Abandonment (Syria 2026)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The US strategic alliance with the SDF faces abandonment in 2026 due to
 *   shifting strategic priorities and regional dynamics. This constraint
 *   highlights the tension between short-term geopolitical gains and
 *   long-term alliance credibility. The abandonment creates a snare for the
 *   SDF, benefits some actors while harming others, and reflects underlying
 *   power dynamics in the region.
 *
 * KEY AGENTS:
 *   - Syrian Democratic Forces (SDF): Primary victim (powerless/trapped) - loses US support and faces existential threat.
 *   - US Foreign Policy Establishment (Realist): Beneficiary (institutional/constrained) - prioritizes short-term strategic goals over long-term commitments.
 *   - Turkish State: Beneficiary (powerful/arbitrage) - removes an obstacle to their regional goals.
 *   - US Credibility With Allies: Secondary victim (moderate/constrained) - suffers damage to its reputation as a reliable ally.
 *   - Analytical Observer: Analytical perspective (analytical/analytical) - assesses the overall impact on regional stability and US foreign policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, 0.65).
domain_priors:suppression_score(us_sdf_alliance_abandonment_2026, 0.7).
domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sdf_alliance_abandonment_2026, snare).
narrative_ontology:human_readable(us_sdf_alliance_abandonment_2026, "US Strategic Alliance Abandonment (Syria 2026)").
narrative_ontology:topic_domain(us_sdf_alliance_abandonment_2026, "geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, us_foreign_policy_establishment_realists).
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, turkish_state).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, syrian_democratic_forces).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, us_credibility_with_allies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The SDF is trapped due to their dependence on US support and the existential threat posed by regional adversaries. Their perspective is that of a snare as abandonment leaves them vulnerable.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% The US foreign policy establishment may view the alliance as a piton. The original strategic goals are no longer relevant, but the alliance is maintained due to inertia. They are constrained by domestic political factors.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The Turkish state benefits from the abandonment, as it removes an obstacle to their strategic goals in the region. However, they face constraints due to international pressure, leading to a tangled rope classification.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% US credibility is negatively impacted by the abandonment, leading to a snare classification. Allies are constrained as they are dependent on US commitments but recognize that these commitments may be broken.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer recognizes the mix of strategic calculation and moral hazard in the abandonment, leading to a tangled rope classification. The benefits are localized while the costs are diffused, typical of geopolitical extraction.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sdf_alliance_abandonment_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, TR),
    TR >= 0.70.

:- end_tests(us_sdf_alliance_abandonment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The abandonment significantly extracts resources and security from the SDF. Suppression (0.70): High. The SDF is trapped with few alternatives. Theater Ratio (0.30): Low. The US decision is driven by real strategic calculations, not performative politics. The rationale involves a re-evaluation of strategic priorities and the perceived costs and benefits of maintaining the alliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is significant. The SDF sees a snare, reflecting their vulnerability. The US foreign policy establishment may see a piton, but more likely a tangled rope, as they may misjudge the long-term costs. The Turkish state sees a benefit, while US credibility suffers. The analytical observer attempts to reconcile these perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The SDF, as the primary victim, experiences high extraction. The US foreign policy establishment and the Turkish state, as beneficiaries, experience low or negative extraction. US credibility is harmed due to perceived betrayal.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by recognizing that the different classifications arise from different perspectives. What appears as a rational strategic calculation from the US perspective is experienced as a betrayal from the SDF perspective. The analytical observer attempts to integrate these perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_regional_stability,
    'Will the abandonment lead to greater regional stability or increased conflict?',
    'Monitoring regional power dynamics and incidence of armed conflict.',
    'If stability increases, the abandonment may be seen as a painful but necessary step. If conflict increases, the abandonment may be seen as a strategic blunder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_regional_stability, empirical, 'Impact on regional stability').

omega_variable(
    sdf_viability,
    'Can the SDF survive as an autonomous entity without US support?',
    'Assessing the SDF''s military capabilities and political alliances.',
    'If the SDF collapses, the abandonment may be seen as a death sentence. If the SDF endures, the abandonment may be seen as a catalyst for self-reliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sdf_viability, empirical, 'SDF long-term viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sdf_alliance_abandonment_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_s_tr_t0, us_sdf_alliance_abandonment_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_s_tr_t5, us_sdf_alliance_abandonment_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(us_s_tr_t10, us_sdf_alliance_abandonment_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(us_s_be_t0, us_sdf_alliance_abandonment_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_s_be_t5, us_sdf_alliance_abandonment_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(us_s_be_t10, us_sdf_alliance_abandonment_2026, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sdf_alliance_abandonment_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, us_middle_east_policy).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, nato_cohesion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
