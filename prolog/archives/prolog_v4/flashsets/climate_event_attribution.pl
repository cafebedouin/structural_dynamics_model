% ============================================================================
% CONSTRAINT STORY: climate_event_attribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_event_attribution, []).

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
 *   constraint_id: climate_event_attribution
 *   human_readable: Scientific methodology for attributing extreme weather to climate change
 *   domain: scientific/political
 *
 * SUMMARY:
 *   This constraint concerns the scientific methodology used to attribute
 *   specific extreme weather events (heatwaves, floods, droughts) to
 *   anthropogenic climate change. The process involves complex climate
 *   modeling, statistical analysis, and expert judgment. It has significant
 *   implications for climate policy, public understanding, and even legal
 *   liability. The extraction comes from using imperfect attribution science
 *   to change policy.
 *
 * KEY AGENTS:
 *   - Climate Research Community: The scientists who conduct attribution studies. (moderate/constrained)
 *   - Policy Makers: Those who use the results of attribution studies to make decisions about climate policy. (powerful/constrained)
 *   - Public Understanding of Science: The general public's comprehension of climate science and the role of human influence. (powerless/trapped)
 *   - Fossil Fuel Industry: The industry that produces and sells fossil fuels, and is potentially liable for climate damages. (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_event_attribution, 0.55).
domain_priors:suppression_score(climate_event_attribution, 0.4).
domain_priors:theater_ratio(climate_event_attribution, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_event_attribution, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_event_attribution, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_event_attribution, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_event_attribution, tangled_rope).
narrative_ontology:human_readable(climate_event_attribution, "Scientific methodology for attributing extreme weather to climate change").
narrative_ontology:topic_domain(climate_event_attribution, "scientific/political").

domain_priors:requires_active_enforcement(climate_event_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_event_attribution, climate_research_community).
narrative_ontology:constraint_beneficiary(climate_event_attribution, policy_makers).
narrative_ontology:constraint_victim(climate_event_attribution, public_understanding_of_science).
narrative_ontology:constraint_victim(climate_event_attribution, fossil_fuel_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The general public, lacking the scientific expertise to evaluate attribution studies directly, is vulnerable to misinformation and misinterpretation. They are trapped in a situation where they must rely on experts, but face a complex and often politicized landscape. The extraction comes from the potential for flawed studies or biased interpretations to erode trust in climate science.
constraint_indexing:constraint_classification(climate_event_attribution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Climate scientists both benefit from and are constrained by event attribution methodologies. They gain funding, recognition, and influence, but are also subject to intense scrutiny, potential criticism, and the pressure to produce definitive results. They are constrained by the limitations of current models and data, but can still publish and advance their careers.
constraint_indexing:constraint_classification(climate_event_attribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Policymakers rely on event attribution studies to inform decisions about climate mitigation and adaptation. They are both beneficiaries (having access to scientific evidence) and targets (political pressures related to climate policy). They can also be pressured by the fossil fuel industry.
constraint_indexing:constraint_classification(climate_event_attribution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The fossil fuel industry is negatively impacted by event attribution studies that link extreme weather to climate change, as this can lead to increased regulation, public pressure, and legal liability. While the fossil fuel industry can't entirely avoid the impacts of climate event attribution, they have the resources to constrain the impacts through misinformation and lobbying.
constraint_indexing:constraint_classification(climate_event_attribution, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From an analytical perspective, the methodology serves as a mixed bag. The methodology can be improved to be more accurate, reliable, and consistent, but it also is already a tangled rope.
constraint_indexing:constraint_classification(climate_event_attribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_event_attribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_event_attribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_event_attribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_event_attribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_event_attribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. Attribution studies can lead to significant consequences for the fossil fuel industry and may result in overzealous or misinformed climate policy. Suppression (0.40): Moderate. The politicized nature of climate science creates a significant suppression. Theater ratio (0.30): Moderate-low. While there is public discourse of science, it is overshadowed by genuine scientific endeavor.
 *
 * PERSPECTIVAL GAP:
 *   The research community and policymakers may view the constraint as beneficial, as it provides valuable information for understanding and addressing climate change. However, the fossil fuel industry and members of the public may see it as a snare, as it places blame for extreme events and potentially undermines their interests or understanding. The analytical observer sees a tangled rope, a process with both coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   The climate research community benefits from event attribution through funding and prestige. Policy makers benefit through climate policy. The public has no exit, and thus they are trapped with the information. The fossil fuel industry is negatively impacted by event attribution.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_uncertainty,
    'How accurately can climate models simulate the conditions necessary for attributing extreme events, given limitations in resolution, parameterization, and process understanding?',
    'Improve model resolution and parameterizations; quantify uncertainties through ensemble simulations and multi-model comparisons; develop methods for structural uncertainty reduction.',
    'If uncertainty is high, attribution statements are unreliable, potentially undermining public trust and policy relevance. If uncertainty is low, attribution statements provide a strong basis for action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_uncertainty, empirical, 'Uncertainty regarding climate model simulations').

omega_variable(
    counterfactual_definition,
    'How should the ''counterfactual'' (what would have happened without anthropogenic climate change) be defined and estimated, given inherent uncertainties in reconstructing past climate states?',
    'Develop alternative counterfactual methodologies; compare results across methods; use paleo-climate data to constrain counterfactual estimates.',
    'Different counterfactual definitions can lead to widely varying attribution results, potentially undermining the credibility of the science. Consistent and robust counterfactual methods are crucial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_definition, conceptual, 'The best method for defining and estimating the counterfactual').

omega_variable(
    detection_vs_attribution,
    'To what extent is ''detection'' (identifying a trend) sufficient versus ''attribution'' (quantifying the specific contribution of anthropogenic forcing) for informing policy and legal decisions?',
    'Legal and policy analysis of the requirements for establishing causation; ethical considerations regarding the use of probabilistic evidence in decision-making.',
    'If detection is sufficient, weaker attribution statements can still be valuable. If attribution is required, more rigorous methodologies are needed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(detection_vs_attribution, preference, 'The extent that detection is sufficient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_event_attribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_event_attribution, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_event_attribution, theater_ratio, 5, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_event_attribution, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_event_attribution, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t5, climate_event_attribution, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(clim_be_t10, climate_event_attribution, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_event_attribution, information_standard).
narrative_ontology:affects_constraint(climate_event_attribution, climate_models).
narrative_ontology:affects_constraint(climate_event_attribution, climate_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
