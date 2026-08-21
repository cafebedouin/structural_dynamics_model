% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity for Reliable Decarbonization
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'baseload necessity' reading of the
 *   broader 'climate mitigation legitimacy' kernel. It asserts that reliable
 *   decarbonization fundamentally requires dispatchable baseload power, which
 *   intermittent renewables cannot provide at scale. This framing channels
 *   policy and investment towards technologies like nuclear and fossil fuels
 *   with CCS, while classifying renewable-only pathways as inadequate. The
 *   claimed type is 'tangled_rope' because it offers a genuine coordination
 *   function (grid stability) but simultaneously extracts resources and
 *   suppresses alternative approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity for Reliable Decarbonization").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'f88ccf0d-9778-4e2c-a697-9670c3c03760').
narrative_ontology:cs_kernel_codification('f88ccf0d-9778-4e2c-a697-9670c3c03760', distributed).
narrative_ontology:cs_authority_grounding('f88ccf0d-9778-4e2c-a697-9670c3c03760', expertise).
narrative_ontology:cs_interpretation_layer_present('f88ccf0d-9778-4e2c-a697-9670c3c03760').
narrative_ontology:cs_reading_relation('f88ccf0d-9778-4e2c-a697-9670c3c03760', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f88ccf0d-9778-4e2c-a697-9670c3c03760', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('f88ccf0d-9778-4e2c-a697-9670c3c03760', climate_mitigation_legitimacy__degrowth_sufficiency_reading, forecloses).
narrative_ontology:cs_axiom('f88ccf0d-9778-4e2c-a697-9670c3c03760', foundational, dispatchability_is_non_negotiable_for_grid_stability).
narrative_ontology:cs_axiom_status(dispatchability_is_non_negotiable_for_grid_stability, holdable).
narrative_ontology:cs_axiom_grounding('f88ccf0d-9778-4e2c-a697-9670c3c03760', dispatchability_is_non_negotiable_for_grid_stability, empirically_contingent).
narrative_ontology:cs_axiom('f88ccf0d-9778-4e2c-a697-9670c3c03760', foundational, renewables_alone_are_insufficient_for_scale_and_reliability).
narrative_ontology:cs_axiom_status(renewables_alone_are_insufficient_for_scale_and_reliability, holdable).
narrative_ontology:cs_axiom_grounding('f88ccf0d-9778-4e2c-a697-9670c3c03760', renewables_alone_are_insufficient_for_scale_and_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('f88ccf0d-9778-4e2c-a697-9670c3c03760', traditional_grid_stability_paradigm).
narrative_ontology:cs_drift_state('f88ccf0d-9778-4e2c-a697-9670c3c03760', contemporary_energy_transition_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f88ccf0d-9778-4e2c-a697-9670c3c03760', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_with_ccs_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_proponents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from policy and investment frameworks that prioritize dispatchable baseload, securing long-term contracts and subsidies for nuclear power plant construction and operation. This reading positions nuclear as an indispensable part of decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Seeks to extend the lifespan of fossil fuel assets by integrating Carbon Capture and Storage (CCS) technology, positioning itself as a provider of dispatchable baseload. This reading provides a justification for continued investment in their infrastructure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_with_ccs_industry, beneficiary,
    institutional, biographical, constrained, global).

% Benefit from the perceived stability and predictability of dispatchable baseload power, which simplifies grid management and reduces the need for complex storage or demand-side management solutions. They advocate for policies that ensure reliable supply.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, beneficiary,
    organized, biographical, constrained, national).

% Bear the cost of policy frameworks that divert investment from purely renewable and storage solutions, facing skepticism and funding challenges for their proposed pathways. They argue that technological advancements make baseload unnecessary.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates, payer,
    organized, generational, constrained, global).

% Their proposals for demand reduction and localized energy systems are often dismissed as unrealistic or insufficient by this reading, which prioritizes large-scale, centralized generation. They face significant ideological and policy barriers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_proponents, payer,
    moderate, civilizational, identity_locked, global).

% Indirectly bear the costs of subsidies and long-term investments in capital-intensive baseload infrastructure, potentially through higher energy prices or taxes, without direct control over energy policy decisions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers, payer,
    powerless, biographical, trapped, national).

% Are responsible for designing energy policy and allocating resources. This reading provides a strong justification for specific policy choices, influencing their decisions towards baseload technologies and away from alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Analyze the technical feasibility and climate impact of various energy pathways, providing data and models that inform the debate. Their findings can either corroborate or challenge the baseload necessity claim.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure a stable, reliable electricity supply during the transition to a decarbonized economy, preventing blackouts and economic disruption by guaranteeing continuous power availability.
% TRANSFER_FUNCTION: Channels public and private investment into dispatchable baseload technologies (e.g., nuclear, fossil with CCS) and away from purely renewable-based infrastructure, transferring risk and cost to taxpayers and consumers while securing profits for baseload providers.
% ABSENT_VOICES: Proponents of purely decentralized, community-owned renewable grids, and those advocating for radical demand reduction, whose models are dismissed as impractical or insufficient by this reading. Their perspectives are often excluded from mainstream policy discussions.
% DISAPPEARANCE_RATIONALE: If the perceived necessity of dispatchable baseload vanished, energy policy would rapidly shift towards accelerated renewable deployment and storage solutions, potentially leading to different grid architectures and investment patterns, fundamentally altering the energy landscape and the economic viability of current energy industries.
% FOUNDING_PROBLEM: The historical challenge of providing continuous, on-demand electricity supply to meet industrial and societal needs, especially as intermittent renewable sources grow and grid stability becomes a concern.
% FOUNDING_PROBLEM_CORROBORATION: Attested by traditional energy engineers, some national grid operators, and proponents of nuclear power, who cite grid stability challenges and the physical limits of energy storage. Renewable advocates and some climate economists contest this, arguing that technological advancements have rendered the problem solvable without traditional baseload.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is driven by the high capital concentration in long-lived baseload assets and the costs imposed on alternative energy pathways. Suppression (0.75) is high due to the active policy and regulatory mechanisms that favor baseload, effectively limiting the viability and funding for purely renewable alternatives. The accessibility collapse (0.80) reflects the strong claim that renewables 'cannot provide at scale', making that alternative seem technically impossible. Resistance (0.70) is significant, stemming from strong advocacy for renewables and degrowth. Theater ratio (0.15) is low, as the claim is rooted in genuine technical and economic arguments, not primarily performance.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (e.g., nuclear industry, grid operators) perceive it as a necessary, pragmatic approach to ensure energy security during decarbonization. Opponents (e.g., renewable advocates, degrowth proponents) view it as a mechanism for incumbent industries to maintain market share, suppressing innovation and delaying a full transition to renewables. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear and fossil fuel with CCS industries, along with grid operators, are clear beneficiaries, as this reading justifies policies that favor their technologies and operational models. Renewable energy advocates, degrowth proponents, and taxpayers are victims, bearing the costs of foregone renewable investment, suppressed alternatives, and potentially higher energy costs. Policy makers act as agenda-setters, influenced by this reading to shape energy policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_of_baseload_claim,
    'Is the claim that renewables cannot provide dispatchable baseload power at scale empirically true, considering advancements in storage and grid management?',
    'Long-term empirical data from grids with high renewable penetration and advanced storage solutions, coupled with independent technical assessments of future technological capabilities.',
    'If empirically disproven, the constraint''s suppression and extractiveness would be reclassified as higher, as its technical justification would be revealed as a cover for rent-seeking. If proven, its classification as a tangled_rope would be reinforced, with the coordination function being more central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validity_of_baseload_claim, empirical, 'Uncertainty regarding the technical necessity of traditional baseload power.').

omega_variable(
    technological_advancement_impact,
    'How rapidly will energy storage and smart grid technologies advance to mitigate the intermittency of renewables, potentially rendering traditional baseload less critical?',
    'Ongoing monitoring of R&D breakthroughs, deployment rates, and cost reductions in energy storage, demand-side management, and grid digitalization.',
    'Faster-than-expected advancements would reduce the perceived necessity of baseload, increasing the effective extractiveness and suppression of this constraint over time. Slower advancements would reinforce its current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_advancement_impact, empirical, 'Impact of future technological progress on the baseload necessity argument.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Is the ''baseload necessity'' a genuine technical constraint or a framing device used by incumbent industries to maintain their position in the energy transition?',
    'Analysis of lobbying efforts, policy influence, and financial flows within the energy sector, alongside independent technical and economic assessments that are not tied to specific industry interests.',
    'If primarily a framing device, the constraint''s extractiveness and suppression would be higher, and its coordination function would be re-evaluated as largely theatrical. If a genuine technical constraint, its current classification as a tangled_rope would be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Ambiguity in the underlying motivation for asserting baseload necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2030, 0.15).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2040, 0.14).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2050, 0.15).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2040, 0.69).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2040, 0.76).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2050, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, energy_market_design).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel, focusing on the necessity of dispatchable baseload power. It is linked to other readings that propose alternative decarbonization pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
