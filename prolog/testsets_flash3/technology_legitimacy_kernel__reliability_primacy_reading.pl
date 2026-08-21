% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability Primacy Reading of Climate Technology Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'reliability primacy' reading of climate
 *   technology legitimacy, where a technology is deemed legitimate for
 *   climate mitigation if it provides dispatchable, baseload-capable
 *   generation for grid stability. This reading benefits established energy
 *   sectors (e.g., nuclear, fossil with CCS) and grid operators, while
 *   imposing costs and hurdles on intermittent renewables and ratepayers. The
 *   constraint is claimed as a 'tangled_rope' because it genuinely
 *   coordinates grid stability but also extracts from specific parties
 *   through its asymmetric application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.7).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability Primacy Reading of Climate Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, 'd05bed06-a32d-4c35-8e1a-888437b89eb0').
narrative_ontology:cs_kernel_codification('d05bed06-a32d-4c35-8e1a-888437b89eb0', formalized).
narrative_ontology:cs_authority_grounding('d05bed06-a32d-4c35-8e1a-888437b89eb0', expertise).
narrative_ontology:cs_interpretation_layer_present('d05bed06-a32d-4c35-8e1a-888437b89eb0').
narrative_ontology:cs_reading_relation('d05bed06-a32d-4c35-8e1a-888437b89eb0', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d05bed06-a32d-4c35-8e1a-888437b89eb0', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('d05bed06-a32d-4c35-8e1a-888437b89eb0', foundational, grid_stability_is_paramount).
narrative_ontology:cs_axiom_status(grid_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('d05bed06-a32d-4c35-8e1a-888437b89eb0', grid_stability_is_paramount, conventional).
narrative_ontology:cs_axiom('d05bed06-a32d-4c35-8e1a-888437b89eb0', foundational, dispatchability_is_essential_for_baseload).
narrative_ontology:cs_axiom_status(dispatchability_is_essential_for_baseload, holdable).
narrative_ontology:cs_axiom_grounding('d05bed06-a32d-4c35-8e1a-888437b89eb0', dispatchability_is_essential_for_baseload, empirically_contingent).
narrative_ontology:cs_reference_frame('d05bed06-a32d-4c35-8e1a-888437b89eb0', traditional_grid_reliability_paradigm).
narrative_ontology:cs_drift_state('d05bed06-a32d-4c35-8e1a-888437b89eb0', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d05bed06-a32d-4c35-8e1a-888437b89eb0', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_with_ccs_advocates).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, climate_advocates_focused_on_speed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritize grid stability and reliability, viewing dispatchable baseload as essential. They set technical standards and influence policy to favor technologies that meet these criteria, often at the expense of other considerations.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from this reading as nuclear power inherently provides dispatchable, baseload generation. They advocate for policies that emphasize these attributes, securing funding and regulatory support.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, global).

% Advocate for continued use of fossil fuels with carbon capture and storage (CCS) as a dispatchable baseload solution. This reading provides a pathway for their technologies to be considered legitimate for climate mitigation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, fossil_fuel_with_ccs_advocates, beneficiary,
    organized, biographical, constrained, national).

% Face significant hurdles as their technologies (solar, wind) are not inherently dispatchable or baseload. They must invest in costly storage solutions or curtailment, increasing their project costs and reducing competitiveness.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    moderate, biographical, constrained, regional).

% Bear the costs associated with prioritizing dispatchable baseload, including potentially higher electricity prices from more expensive generation or storage solutions, and reduced access to cheaper intermittent renewables.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, immediate, trapped, local).

% Argue that rapid deployment of all available low-carbon technologies, including intermittent renewables, is paramount to meet urgent climate targets. This reading's emphasis on reliability over speed sidelines their preferred solutions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_advocates_focused_on_speed, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates energy policy and investment around a shared understanding of grid stability requirements, ensuring that new generation capacity contributes to a reliable and resilient electricity supply.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and regulatory support to dispatchable, baseload-capable technologies (e.g., nuclear, fossil with CCS) and away from intermittent renewables, imposing additional costs on the latter and on ratepayers.
% ABSENT_VOICES: Climate advocates prioritizing rapid deployment and developers of cost-effective intermittent renewables are marginalized; they would argue for a broader definition of legitimacy that includes speed and cost-effectiveness, even with grid integration challenges.
% DISAPPEARANCE_RATIONALE: If this reliability-primacy reading vanished, energy policy would immediately shift to prioritize other factors (e.g., speed of deployment, cost, environmental impact beyond grid stability). Investment would flow differently, and the energy mix would rapidly change, leading to a significant rearrangement of the energy sector.
% FOUNDING_PROBLEM: The need to ensure continuous, stable, and reliable electricity supply to prevent blackouts and support modern society's energy demands, especially during the transition to low-carbon sources.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and national security agencies consistently attest to the live status of grid stability as a critical problem. While the specific technologies to solve it are debated, the underlying problem of reliability is widely corroborated by independent engineering assessments and historical grid failure analyses.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the increased costs imposed on intermittent renewables and ratepayers, who must either invest in expensive storage or pay for more costly baseload alternatives. Suppression (0.70) is high due to the institutional power of grid operators and traditional energy industries in shaping policy and technical standards, effectively limiting the market access and perceived legitimacy of non-dispatchable technologies. The theater ratio (0.20) is relatively low, as the concern for grid stability is genuine, but there's a performative aspect in how this concern is leveraged to maintain the status quo for certain technologies.
 *
 * PERSPECTIVAL GAP:
 *   Grid operators and the nuclear industry perceive this as a necessary 'rope' for maintaining a stable energy supply, a coordination function. However, intermittent renewable developers and ratepayers experience it as a 'snare' or 'tangled_rope' due to the significant costs and market barriers it imposes, which they view as extractive and unnecessary given alternative grid management solutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators and the nuclear/fossil with CCS industries are beneficiaries (low d) as the constraint aligns with their operational models and secures their market position. Intermittent renewable developers and ratepayers are targets (high d) as they bear the costs of compliance or exclusion. Climate advocates focused on speed are excluded, as their priorities are not directly addressed by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine grid stability concerns as pure extraction. By identifying it as a 'tangled_rope', the framework acknowledges the legitimate coordination function (grid stability) while highlighting the asymmetric extraction (costs on renewables, ratepayers) and the active enforcement required to maintain this specific definition of legitimacy. If it were a 'snare', the coordination story would be pure cover; if a 'rope', the extraction would be negligible. The 'tangled_rope' accurately captures the hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_baseload_capability,
    'Is ''baseload-capable'' an immutable technical requirement or a historically contingent definition that could evolve with grid modernization and storage solutions?',
    'Technological advancements in energy storage and smart grid management, coupled with policy shifts that redefine grid services and compensation mechanisms.',
    'If the definition evolves, intermittent renewables could qualify without costly add-ons, reducing extraction and potentially reclassifying the constraint towards a ''rope'' or ''scaffold'' as it adapts to new technical realities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_baseload_capability, empirical, 'Ambiguity in the technical definition of ''baseload-capable'' and its potential for evolution.').

omega_variable(
    reliability_vs_cost_tradeoff,
    'What is the optimal balance between grid reliability (as defined by this reading) and the cost of electricity for ratepayers, considering the urgency of climate action?',
    'Comprehensive energy system modeling that integrates economic, engineering, and climate impact analyses, followed by public policy debate and democratic decision-making on acceptable tradeoffs.',
    'A shift towards prioritizing cost-effectiveness or speed could reduce the legitimacy of technologies favored by this reading, leading to a reclassification towards a ''snare'' if the reliability justification is found to be disproportionate to the costs imposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliability_vs_cost_tradeoff, preference, 'The irreducible policy tradeoff between grid reliability and other energy system goals.').

omega_variable(
    kernel_reading_precautionary_impact,
    'How would the ''precautionary_reading'' (bounded failure modes) structurally alter the beneficiary/victim set of this ''reliability_primacy_reading''?',
    'Analysis of policy proposals and regulatory frameworks derived from the precautionary reading, specifically examining their impact on nuclear power''s legitimacy and the associated costs/benefits for other technologies.',
    'The precautionary reading would likely shift nuclear power from a beneficiary to a victim (due to long-term waste and accident risks), fundamentally altering the power dynamics and potentially foreclosing this reliability-primacy reading''s current beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_precautionary_impact, conceptual, 'Impact of the precautionary reading on the structural positions within the reliability primacy framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_legitimacy_kernel'. Each reading defines legitimacy differently, leading to distinct beneficiary/victim sets and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
