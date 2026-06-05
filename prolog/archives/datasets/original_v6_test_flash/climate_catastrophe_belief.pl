% ============================================================================
% CONSTRAINT STORY: climate_catastrophe_belief
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_catastrophe_belief, []).

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
 *   constraint_id: climate_catastrophe_belief
 *   human_readable: Belief in Inevitable Near-Term Climate Catastrophe
 *   domain: social/political
 *
 * SUMMARY:
 *   Based on polling data (e.g., YouGov/Economist, Feb 2026), a significant
 *   portion of the population believes they will witness catastrophic impacts
 *   from climate change within their lifetimes. This belief, while rooted in
 *   scientific evidence, also functions as a social and political force,
 *   shaping individual behaviors, collective action, and policy decisions.
 *   This creates a complex constraint with both beneficial and detrimental
 *   consequences.
 *
 * KEY AGENTS:
 *   - Average Citizen: Primary victim (powerless/trapped) - Experiences anxiety and fatalism due to the perceived inevitability of climate catastrophe.
 *   - Informed Public: Secondary victim (moderate/constrained) - Constrained by the overwhelming nature of climate change, but also benefits from increased awareness.
 *   - Activist Groups: Primary beneficiary (institutional/arbitrage) - Benefit from increased urgency and engagement, leading to greater support and funding.
 *   - Renewable Energy Sector: Secondary beneficiary (powerful/arbitrage) - Benefits from increased investment and policy support, but is constrained by technological limitations.
 *   - Traditional Media: Institutional actor (institutional/constrained) - Relies on sensationalism to capture audience attention but may also be constrained by journalistic ethics.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) - Acknowledges the reality of climate change but sees the belief in near-term catastrophe as a complex phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_catastrophe_belief, 0.55).
domain_priors:suppression_score(climate_catastrophe_belief, 0.65).
domain_priors:theater_ratio(climate_catastrophe_belief, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_catastrophe_belief, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_catastrophe_belief, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_catastrophe_belief, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_catastrophe_belief, tangled_rope).
narrative_ontology:human_readable(climate_catastrophe_belief, "Belief in Inevitable Near-Term Climate Catastrophe").
narrative_ontology:topic_domain(climate_catastrophe_belief, "social/political").

domain_priors:requires_active_enforcement(climate_catastrophe_belief).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, activist_groups).
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, media_outlets).
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_catastrophe_belief, informed_public_discourse).
narrative_ontology:constraint_victim(climate_catastrophe_belief, economic_productivity).
narrative_ontology:constraint_victim(climate_catastrophe_belief, mental_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Average Citizen (Snare) - Feels trapped by the perceived inevitability of climate catastrophe, leading to anxiety and fatalism. Limited exit options due to lack of personal control over global issues.
constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Informed Public (Tangled Rope) - Constrained by the overwhelming nature of climate change, but also benefits from increased awareness and potential for action. Experiences mixed coordination and extraction.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Activist Groups (Rope) - Benefit from increased urgency and engagement, leading to greater support and funding. Can arbitrage the situation for their organizational goals.
constraint_indexing:constraint_classification(climate_catastrophe_belief, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Renewable Energy Sector (Tangled Rope) - Benefits from increased investment and policy support, but is constrained by technological limitations and political opposition. Experiences mixed coordination and extraction.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 5: Traditional Media (Piton) - Relies on sensationalism and fear-mongering related to climate change to capture audience attention, however their impact is waning due to the rise of social media. Theatrical performance outweighs functional value.
constraint_indexing:constraint_classification(climate_catastrophe_belief, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 6: Analytical Observer (Tangled Rope) - Acknowledges the reality of climate change but sees the belief in near-term catastrophe as a complex phenomenon with both positive and negative consequences. Mixed coordination and extraction.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_catastrophe_belief_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_catastrophe_belief, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_catastrophe_belief, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_catastrophe_belief, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_catastrophe_belief, TR),
    TR >= 0.70.

:- end_tests(climate_catastrophe_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The belief in near-term climate catastrophe extracts a psychological cost from individuals, leading to anxiety and fatalism. It also extracts resources from the economy by diverting them towards often ineffective or performative mitigation efforts. However, it also drives positive changes, such as increased investment in renewable energy. Suppression (0.65): High. Dissenting voices are often suppressed or dismissed as 'climate deniers,' creating an echo chamber effect. The narrative of impending doom dominates the public discourse, making it difficult to have a nuanced conversation about the complexities of climate change. Theater Ratio (0.30): Low. While there is some performative action related to climate change, such as virtue signaling and symbolic gestures, much of the effort is directed towards genuine attempts to mitigate the problem.
 *
 * PERSPECTIVAL GAP:
 *   The average citizen sees a snare, feeling trapped and powerless. Activist groups and the renewable energy sector see a rope, as the belief drives their success. The informed public sees a tangled rope, experiencing a mix of benefits and burdens. Traditional media, constrained by their reliance on capturing audiences, and experiencing decreasing reach, see a piton. The analytical observer recognizes the complex interplay of factors and therefore also sees a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The average citizen (trapped/powerless) experiences the highest effective extraction, while activist groups (arbitrage/institutional) experience coordination benefits. The informed public and renewable energy sector (constrained/moderate and powerful) experience a mix of coordination and extraction. The Analytical Observer sees a balanced tangled rope. Media is extracting value from belief, but losing power to do so as their influence diminishes.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope acknowledges that the belief in near-term climate catastrophe has both positive and negative effects. It is not simply a snare, as it does drive some beneficial changes. It is also not simply a rope, as it extracts a real cost from individuals and society. The mandatrophy is resolved by recognizing the complex interplay of factors and perspectives involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_certainty,
    'To what extent are irreversible climate tipping points truly inevitable?',
    'Advanced climate modeling and long-term observational data.',
    'If inevitable, strengthens the snare. If avoidable, weakens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_certainty, empirical, 'Certainty surrounding irreversible climate tipping points.').

omega_variable(
    policy_effectiveness,
    'How effective are current and proposed climate mitigation policies?',
    'Economic and environmental impact assessments.',
    'If highly effective, shifts the classification toward scaffold. If ineffective, reinforces the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_effectiveness, empirical, 'Effectiveness of current and proposed climate policies.').

omega_variable(
    human_adaptability,
    'What is the capacity of human societies to adapt to climate change?',
    'Social and technological innovation research.',
    'If high adaptability, softens the snare. If low adaptability, hardens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_adaptability, empirical, 'Capacity of human societies to adapt to climate change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_catastrophe_belief, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_catastrophe_belief, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_catastrophe_belief, theater_ratio, 5, 0.3).
narrative_ontology:measurement(clim_tr_t10, climate_catastrophe_belief, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_catastrophe_belief, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t5, climate_catastrophe_belief, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_catastrophe_belief, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_catastrophe_belief, information_standard).
narrative_ontology:affects_constraint(climate_catastrophe_belief, climate_change_mitigation_policies).
narrative_ontology:affects_constraint(climate_catastrophe_belief, renewable_energy_adoption).

% DUAL FORMULATION NOTE:
% Belief in catastrophic outcomes operates on a different level than actual impacts, and has its own set of dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
