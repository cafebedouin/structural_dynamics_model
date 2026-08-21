% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Climate Mitigation: Portfolio Pragmatism Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio pragmatism' reading of climate
 *   mitigation legitimacy, arguing that optimal decarbonization requires a
 *   technology-neutral approach incorporating both nuclear and renewables. It
 *   is one reading of the broader 'climate_mitigation_legitimacy' kernel.
 *   This reading emphasizes flexibility, regional optimization, and capital
 *   diversification, avoiding a priori exclusion of any low-carbon
 *   technology. The metrics reflect a relatively low-extraction,
 *   low-suppression coordination mechanism, as its primary function is to
 *   enable diverse solutions rather than enforce a narrow one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.35).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.2).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Climate Mitigation: Portfolio Pragmatism Reading").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'ec331de8-6ead-4486-8842-98ed8066d05f').
narrative_ontology:cs_kernel_codification('ec331de8-6ead-4486-8842-98ed8066d05f', distributed).
narrative_ontology:cs_authority_grounding('ec331de8-6ead-4486-8842-98ed8066d05f', expertise).
narrative_ontology:cs_interpretation_layer_present('ec331de8-6ead-4486-8842-98ed8066d05f').
narrative_ontology:cs_reading_relation('ec331de8-6ead-4486-8842-98ed8066d05f', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec331de8-6ead-4486-8842-98ed8066d05f', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec331de8-6ead-4486-8842-98ed8066d05f', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('ec331de8-6ead-4486-8842-98ed8066d05f', foundational, technology_neutrality_for_decarbonization).
narrative_ontology:cs_axiom_status(technology_neutrality_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('ec331de8-6ead-4486-8842-98ed8066d05f', technology_neutrality_for_decarbonization, instrumental).
narrative_ontology:cs_axiom('ec331de8-6ead-4486-8842-98ed8066d05f', foundational, regional_optimization_of_energy_mix).
narrative_ontology:cs_axiom_status(regional_optimization_of_energy_mix, holdable).
narrative_ontology:cs_axiom_grounding('ec331de8-6ead-4486-8842-98ed8066d05f', regional_optimization_of_energy_mix, empirically_contingent).
narrative_ontology:cs_reference_frame('ec331de8-6ead-4486-8842-98ed8066d05f', evidence_based_climate_action).
narrative_ontology:cs_drift_state('ec331de8-6ead-4486-8842-98ed8066d05f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ec331de8-6ead-4486-8842-98ed8066d05f', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_system_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diverse_energy_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing and implementing decarbonization pathways. This reading provides them with maximum flexibility and a wider array of tools, reducing the risk of single-technology failure or cost overruns. They benefit from a pragmatic, evidence-based approach.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_system_planners, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a policy environment that does not arbitrarily exclude viable technologies, allowing them to diversify their portfolios across nuclear, solar, wind, and other low-carbon options based on economic and technical merit. This reduces regulatory risk for their capital.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diverse_energy_investors, beneficiary,
    organized, biographical, mobile, global).

% While not directly paying, they bear the 'cost' of not having renewables exclusively prioritized. This reading requires them to accept nuclear as a legitimate part of the solution, potentially diverting capital or political will from their preferred technologies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_advocates, payer,
    organized, generational, constrained, global).

% Similar to renewable advocates, they bear the 'cost' of not having nuclear exclusively prioritized. This reading requires them to accept renewables as a legitimate part of the solution, potentially diverting capital or political will from their preferred technologies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_advocates, payer,
    organized, generational, constrained, global).

% Provide the scientific basis for decarbonization targets and assess the efficacy of different pathways. This reading aligns with a scientific approach that prioritizes outcomes over specific technological means, but they do not directly benefit or pay.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse technological and economic interests towards the common goal of rapid and reliable decarbonization by allowing for a flexible, regionally optimized mix of low-carbon energy sources.
% TRANSFER_FUNCTION: Transfers political and financial support across a broader range of low-carbon technologies (nuclear, various renewables) rather than concentrating it on a single class, from those who advocate for single-technology primacy to those who prefer a diversified approach.
% ABSENT_VOICES: Those who believe in a 'silver bullet' technology (either exclusively nuclear or exclusively renewable) are implicitly sidelined, as are those who advocate for a 'degrowth' approach that questions the need for large-scale energy infrastructure altogether.
% DISAPPEARANCE_RATIONALE: If this pragmatic, technology-neutral approach vanished, energy policy would likely polarize further, leading to less efficient and potentially slower decarbonization pathways due to ideological lock-in on specific technologies. Investment would become more volatile, and overall climate goals harder to meet.
% FOUNDING_PROBLEM: The problem of achieving rapid, reliable, and cost-effective decarbonization across diverse geographical and economic contexts, avoiding single-point failures or ideological gridlock in energy policy.
% FOUNDING_PROBLEM_CORROBORATION: International energy agencies (IEA, IRENA), intergovernmental panels (IPCC), and independent academic studies consistently corroborate the need for diverse portfolios and pragmatic approaches to meet climate targets, often highlighting the risks of ideological exclusion of viable technologies.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the 'cost' to single-technology advocates of not having their preferred solution exclusively prioritized, but it's not a direct financial extraction. Suppression (0.20) is low because this reading primarily seeks to open options, not close them, though it implicitly suppresses purely ideological arguments against certain technologies. Theater ratio (0.10) is low as the constraint is genuinely focused on achieving decarbonization outcomes. Accessibility collapse is moderate (0.40) because while it opens up options, it still implicitly collapses the option of a single-technology or degrowth-only pathway. Resistance (0.30) is moderate, coming from advocates of more ideologically rigid approaches.
 *
 * PERSPECTIVAL GAP:
 *   Advocates for single-technology solutions (e.g., 'renewable primacy' or 'baseload necessity' readings) would experience this constraint as more extractive, as it forces them to dilute their focus and accept technologies they might oppose. From the perspective of system planners, it's a necessary coordination to achieve the overarching goal. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Energy system planners and diverse energy investors are beneficiaries, as this reading provides them with flexibility and reduces risk. Renewable and nuclear advocates are 'payers' in the sense that they must compromise on their preferred exclusive pathways, bearing the cost of accepting a broader portfolio. Climate scientists act as observers, providing analytical input without direct benefit or cost from this specific policy framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_neutrality_vs_implicit_bias,
    'Is the ''technology-neutral'' stance truly neutral, or does it implicitly favor certain technologies (e.g., large-scale capital projects) due to existing institutional structures or lobbying power?',
    'Detailed analysis of capital allocation patterns and policy incentives under this framework, comparing actual investment outcomes against a truly neutral baseline.',
    'If an implicit bias is found, the constraint''s effective extractiveness for disfavored technologies would be higher, potentially reclassifying it as a Tangled Rope for those specific actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_vs_implicit_bias, empirical, 'Assesses whether the claimed technology neutrality holds in practice.').

omega_variable(
    optimal_mix_definition_ambiguity,
    'What constitutes ''optimal'' decarbonization? Is it purely cost-based, or does it include factors like energy security, social equity, and environmental justice, and how are these weighted?',
    'Policy analysis and stakeholder engagement to explicitly define and weight the criteria for ''optimal'' within the policy framework.',
    'A narrow definition of ''optimal'' (e.g., lowest LCOE only) could lead to outcomes that are extractive for communities bearing environmental burdens, potentially shifting the classification towards a Snare for those communities. A broader definition would reinforce its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_mix_definition_ambiguity, preference, 'Ambiguity in the definition of ''optimal'' decarbonization.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''climate_mitigation_legitimacy'' kernel. The ''portfolio_pragmatism_reading'' emphasizes technology neutrality and diverse solutions. Sibling readings include ''baseload_necessity_reading'', ''renewable_primacy_reading'', and ''degrowth_sufficiency_reading''. How would the classification change if a sibling reading were adopted?',
    'Analyze the structural properties (beneficiaries, victims, extractiveness, suppression) of each sibling reading as separate constraints.',
    'Adopting the ''baseload_necessity_reading'' or ''renewable_primacy_reading'' would likely increase extractiveness and suppression for the excluded technologies, potentially shifting the constraint towards a Tangled Rope or Snare for those specific actors. The ''degrowth_sufficiency_reading'' would fundamentally alter the problem framing, likely leading to a different set of constraints entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is a specific reading of the climate mitigation legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(clim_tr_t50, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(clim_be_t50, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement(clim_su_t50, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'portfolio pragmatism' reading of the 'climate_mitigation_legitimacy' kernel. It is linked to other readings of the same kernel, each representing a distinct structural claim about optimal decarbonization pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
