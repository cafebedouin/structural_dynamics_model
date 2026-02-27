% ============================================================================
% CONSTRAINT STORY: climate_policy_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_policy_extraction, []).

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
 *   constraint_id: climate_policy_extraction
 *   human_readable: The Decarbonization Snare (Climate-Identity Collision)
 *   domain: environmental/political
 *
 * SUMMARY:
 *   As federal and international climate policies intensify, a regional
 *   hydrocarbon-based economy interprets decarbonization as a direct threat
 *   to its wealth and identity. This perceived threat manifests as a
 *   'decarbonization snare,' where policies designed to benefit the global
 *   climate extract value and opportunity from a specific regional community.
 *   The collision of climate policy and regional identity creates a
 *   structural tension that requires careful consideration of economic
 *   transition strategies and cultural adaptation.
 *
 * KEY AGENTS:
 *   - Hydrocarbon Economy Workers: Primary victims (powerless/trapped) - face job losses and economic hardship.
 *   - Regional Community Identity: Secondary victims (moderate/constrained) - cultural identity threatened.
 *   - Renewable Energy Sector: Primary beneficiaries (institutional/arbitrage) - benefit from increased investment and market share.
 *   - Climate Policy Advocates: Organized agents (organized/mobile) - see decarbonization as essential but recognize negative impacts.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) - recognizes trade-offs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_policy_extraction, 0.75).
domain_priors:suppression_score(climate_policy_extraction, 0.8).
domain_priors:theater_ratio(climate_policy_extraction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_policy_extraction, extractiveness, 0.75).
narrative_ontology:constraint_metric(climate_policy_extraction, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(climate_policy_extraction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_policy_extraction, tangled_rope).
narrative_ontology:human_readable(climate_policy_extraction, "The Decarbonization Snare (Climate-Identity Collision)").
narrative_ontology:topic_domain(climate_policy_extraction, "environmental/political").

domain_priors:requires_active_enforcement(climate_policy_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_policy_extraction, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_policy_extraction, climate_policy_advocates).
narrative_ontology:constraint_victim(climate_policy_extraction, hydrocarbon_economy_workers).
narrative_ontology:constraint_victim(climate_policy_extraction, regional_community_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Hydrocarbon economy workers face job losses and economic hardship due to decarbonization policies, with limited opportunities for retraining or relocation. Trapped in a declining industry, they perceive decarbonization as a snare.
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective 2: The cultural identity of the region, deeply intertwined with the hydrocarbon industry, is threatened by decarbonization policies. While some adaptation is possible, the loss of a core economic activity and way of life is experienced as a significant extraction.
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: The renewable energy sector benefits from decarbonization policies through increased investment, market share, and political influence. Decarbonization acts as a coordinating force, driving growth and innovation.
constraint_indexing:constraint_classification(climate_policy_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Climate policy advocates see decarbonization as essential for mitigating climate change, but recognize the negative impacts on hydrocarbon-dependent communities. They are mobile in that they can shift focus and resources, but also face constraints in achieving their goals due to political and social resistance. This perspective reflects both coordination and extraction.
constraint_indexing:constraint_classification(climate_policy_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 5: An analytical observer recognizes the trade-offs inherent in decarbonization policies, including the economic and social costs for hydrocarbon-dependent regions. This perspective acknowledges both the necessity of climate action and the distributional consequences.
constraint_indexing:constraint_classification(climate_policy_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_policy_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_policy_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_policy_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_policy_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. Decarbonization policies cause significant economic disruption and job losses in the hydrocarbon region. The impact is magnified by the lack of alternative economic opportunities. Suppression (0.80): High. The region faces limited options for adapting to a post-hydrocarbon economy. Political and social resistance further constrain potential solutions. Theater ratio (0.30): Low. While there is some performative activity (e.g., symbolic investments in green initiatives), the fundamental economic transformation is real and impactful.
 *
 * PERSPECTIVAL GAP:
 *   The regional population directly affected by decarbonization policies experiences it as a loss of livelihood and way of life (snare). Climate policy advocates and the renewable energy sector view it as a necessary step towards a sustainable future (rope). The analytical observer recognizes the inherent trade-offs and the need for careful policy design (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the impact of decarbonization on different stakeholders. Hydrocarbon workers and the regional community face significant economic and social costs (high d). The renewable energy sector and climate policy advocates benefit from the policies (low d). The analytical observer acknowledges both the benefits and the costs (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   The 'decarbonization snare' highlights the complexities of climate policy and the importance of considering distributional consequences. While decarbonization is essential for mitigating climate change, it can create significant hardship for specific communities. The tangled_rope classification underscores the need for policies that promote a just transition and provide support for affected workers and regions. Resolving this mandated trophy requires moving away from a simplistic view of decarbonization as a universal good and recognizing the structural inequalities that it can exacerbate. The enforcement comes from the global pressure to meet climate goals, which forces even unwilling regions to comply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_transition_feasibility,
    'How feasible are alternative economic activities in replacing hydrocarbon-based revenues and employment in the region?',
    'Detailed economic modeling and pilot programs testing alternative industries.',
    'If feasible: Snare mitigated towards Tangled Rope or Scaffold. If not feasible: Snare classification reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_transition_feasibility, empirical, 'Feasibility of alternative economic activities').

omega_variable(
    identity_shift_capacity,
    'To what extent can regional identity adapt to a post-hydrocarbon economy?',
    'Sociological research on community values and narratives. Exploration of cultural initiatives that reframe regional identity.',
    'If adaptable: Snare loosens towards Tangled Rope. If inflexible: Snare classification reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_shift_capacity, conceptual, 'Capacity for regional identity shift').

omega_variable(
    climate_urgency_vs_social_justice,
    'What is the appropriate balance between climate action and social justice concerns in decarbonization policies?',
    'Ethical and political discourse. Public opinion research. Deliberative forums involving affected communities.',
    'Determines the level of acceptable social cost and the political feasibility of decarbonization policies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_urgency_vs_social_justice, preference, 'Balance between climate urgency and social justice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_policy_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_policy_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_policy_extraction, theater_ratio, 5, 0.25).
narrative_ontology:measurement(clim_tr_t10, climate_policy_extraction, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_policy_extraction, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(clim_be_t5, climate_policy_extraction, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(clim_be_t10, climate_policy_extraction, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_policy_extraction, resource_allocation).
narrative_ontology:affects_constraint(climate_policy_extraction, global_carbon_pricing).
narrative_ontology:affects_constraint(climate_policy_extraction, fossil_fuel_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is downstream of global carbon pricing and fossil fuel subsidies. It highlights the localized impacts of broader climate policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
