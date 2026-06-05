% ============================================================================
% CONSTRAINT STORY: climate_attribution_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_attribution_2026, []).

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
 *   constraint_id: climate_attribution_2026
 *   human_readable: Extreme Weather Attribution Science
 *   domain: scientific/political/economic
 *
 * SUMMARY:
 *   Attribution science quantifies how much human-induced climate change has
 *   altered the risk of specific extreme weather events. This field plays an
 *   increasingly important role in informing policy and public understanding
 *   of climate change impacts. However, the science is inherently complex and
 *   subject to uncertainties, creating a potential for misrepresentation and
 *   undue influence.
 *
 * KEY AGENTS:
 *   - Climate Modelers: Benefit from increased funding and influence (institutional/arbitrage).
 *   - Attribution Scientists: Experience both benefits and constraints (moderate/constrained).
 *   - Public Understanding of Uncertainty: Can be victimized through misrepresentation or oversimplification (powerless/trapped).
 *   - Regions Disproportionately Affected by Climate Change: Bear the brunt of policy decisions based on attribution claims (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_attribution_2026, 0.55).
domain_priors:suppression_score(climate_attribution_2026, 0.4).
domain_priors:theater_ratio(climate_attribution_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_attribution_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_attribution_2026, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_attribution_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_attribution_2026, tangled_rope).
narrative_ontology:human_readable(climate_attribution_2026, "Extreme Weather Attribution Science").
narrative_ontology:topic_domain(climate_attribution_2026, "scientific/political/economic").

domain_priors:requires_active_enforcement(climate_attribution_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_attribution_2026, climate_modelers).
narrative_ontology:constraint_beneficiary(climate_attribution_2026, attribution_scientists).
narrative_ontology:constraint_victim(climate_attribution_2026, public_understanding_of_uncertainty).
narrative_ontology:constraint_victim(climate_attribution_2026, regions_disproportionately_affected_by_climate_change).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% These regions often lack the resources to independently verify or challenge attribution claims, and bear the brunt of policy decisions influenced by these claims. They are trapped in the consequences.
constraint_indexing:constraint_classification(climate_attribution_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Benefit from increased funding and influence as attribution science gains prominence. They can arbitrage their expertise across various research institutions and policy bodies.
constraint_indexing:constraint_classification(climate_attribution_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Sees the complex interplay of scientific advancement, policy influence, and potential for misrepresentation of uncertainties. Acknowledges both the benefits and risks.
constraint_indexing:constraint_classification(climate_attribution_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Experience both benefits (funding, recognition) and constraints (pressure to produce impactful results, potential for bias). Their career prospects are tied to the field's success, making their exit options constrained.
constraint_indexing:constraint_classification(climate_attribution_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The public may not fully grasp the uncertainties inherent in attribution science, leading to oversimplified narratives and potentially misinformed policy preferences. Trapped in the narrative without the tools to critically assess it.
constraint_indexing:constraint_classification(climate_attribution_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_attribution_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_attribution_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_attribution_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_attribution_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The field extracts from public understanding by potentially simplifying complex uncertainties. Funding and recognition flow towards attribution science, potentially at the expense of other climate research areas. Suppression (0.40): Moderate. There is suppression of alternative interpretations or dissenting opinions within the field. The public may be trapped in narratives driven by attribution science. Theater Ratio (0.30): Low. While there is some performative aspect to presenting attribution results for public consumption, the science is generally driven by genuine research efforts.
 *
 * PERSPECTIVAL GAP:
 *   Regions disproportionately affected may see attribution science as a snare if policy responses are inadequate or misdirected due to flawed attribution. Climate modelers and attribution scientists see it as a necessary tool for understanding and addressing climate change. The public may be trapped in a narrative they cannot easily verify or challenge.
 *
 * DIRECTIONALITY LOGIC:
 *   Those who benefit (climate modelers, attribution scientists) have arbitrage exit options and experience lower extraction. Those who bear the costs (public understanding, affected regions) are more constrained or trapped, and experience higher extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by acknowledging the mixed nature of attribution science. It is not purely extractive (snare) as it provides valuable insights and tools for climate action. However, it is also not purely beneficial (rope) as it has the potential to distort public understanding and misdirect policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncertainty_quantification_accuracy,
    'How accurately are uncertainties in climate models and attribution methodologies being quantified and communicated?',
    'Independent audits of uncertainty quantification methods, comparison of different attribution approaches, expert elicitation.',
    'If uncertainties are underestimated, policy decisions may be based on overly confident projections. If overestimated, necessary action may be delayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_quantification_accuracy, empirical, 'Accuracy of uncertainty quantification in attribution science.').

omega_variable(
    influence_of_prior_beliefs,
    'To what extent do prior beliefs and political agendas influence the interpretation and communication of attribution results?',
    'Analysis of scientists'' statements and publications, surveys of public and policymakers, case studies of controversial events.',
    'If strong influence, attribution results may be selectively used to support pre-existing narratives, undermining public trust and effective policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(influence_of_prior_beliefs, conceptual, 'Influence of prior beliefs on attribution science.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_attribution_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_attribution_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_attribution_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(clim_tr_t10, climate_attribution_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_attribution_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t5, climate_attribution_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_attribution_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_attribution_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
