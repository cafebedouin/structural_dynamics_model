% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Clean Air Act Section 111(d) 'Best System' Limited to Facility-Specific Measures
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents a specific reading of Section 111(d) of the
 *   Clean Air Act, limiting the EPA's authority to regulate greenhouse gas
 *   emissions from existing power plants to measures implementable at
 *   individual facilities (e.g., heat-rate improvements, carbon capture).
 *   This interpretation, often advanced by fossil fuel interests and some
 *   states, prevents the EPA from mandating broader, grid-wide
 *   generation-shifting strategies. The constraint is claimed as a
 *   'tangled_rope' because it provides a coordination function (a clear,
 *   albeit limited, regulatory path) but also involves significant asymmetric
 *   extraction, primarily from climate advocates and renewable energy
 *   developers, while benefiting the coal power sector and states with fossil
 *   fuel interests. This reading effectively sets a regulatory ceiling,
 *   protecting the existing energy infrastructure at the expense of more
 *   aggressive climate action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.75).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) 'Best System' Limited to Facility-Specific Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '93b8143d-7c98-45d3-b080-e94bf0c3ef0f').
narrative_ontology:cs_kernel_codification('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', fixed_text).
narrative_ontology:cs_authority_grounding('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', lineage).
narrative_ontology:cs_interpretation_layer_present('93b8143d-7c98-45d3-b080-e94bf0c3ef0f').
narrative_ontology:cs_reading_relation('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', caa_section_111d_delegation__systemic_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', foundational, epa_authority_limited_to_fence_line).
narrative_ontology:cs_axiom_status(epa_authority_limited_to_fence_line, holdable).
narrative_ontology:cs_axiom_grounding('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', epa_authority_limited_to_fence_line, conventional).
narrative_ontology:cs_axiom('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', foundational, states_retain_primary_energy_mix_control).
narrative_ontology:cs_axiom_status(states_retain_primary_energy_mix_control, holdable).
narrative_ontology:cs_axiom_grounding('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', states_retain_primary_energy_mix_control, conventional).
narrative_ontology:cs_reference_frame('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', traditional_environmental_regulation_scope).
narrative_ontology:cs_drift_state('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('93b8143d-7c98-45d3-b080-e94bf0c3ef0f', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_power_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, states_with_fossil_fuel_interests).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with regulating greenhouse gas emissions under Section 111(d), but constrained by this reading to only facility-level measures. This limits its ability to achieve significant emissions reductions and forces it into a reactive, rather than proactive, regulatory stance.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from this reading by avoiding mandates for generation-shifting or early retirement of coal plants. It can continue operating with minor, facility-specific improvements, preserving its economic model and market share.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_power_sector, beneficiary,
    organized, biographical, mobile, national).

% Preserves their autonomy over energy mix and protects their fossil fuel industries from federal mandates that would force a transition to renewables. This aligns with their economic and political interests.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_with_fossil_fuel_interests, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of delayed and insufficient climate action. This reading creates a regulatory ceiling that prevents the systemic changes they advocate for, leading to continued environmental degradation and increased future climate risks.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, constrained, global).

% Face a less favorable regulatory environment that does not incentivize a rapid transition to renewable energy. This limits their market growth and investment opportunities compared to a reading that would promote generation-shifting.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% The ultimate arbiter of this interpretation, its rulings enforce this constraint on the EPA. Its institutional power ensures the persistence of this reading, shaping the future of environmental regulation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit limited, framework for EPA to regulate greenhouse gas emissions from existing power plants, ensuring some level of federal oversight and consistency across states for facility-level improvements.
% TRANSFER_FUNCTION: Transfers regulatory authority and the burden of significant emissions reductions away from the EPA and onto individual facilities, while preserving economic benefits for the coal power sector and states with fossil fuel interests. The cost of climate inaction is transferred to climate advocates and future generations.
% ABSENT_VOICES: Future generations, who will bear the long-term costs of climate change exacerbated by this limited regulatory approach, are absent from the interpretive process. Scientific consensus on the urgency of climate action is also effectively sidelined.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the EPA would likely pursue more aggressive, systemic emissions reduction strategies, potentially including generation-shifting. This would fundamentally alter the energy landscape, accelerating the retirement of fossil fuel plants and boosting renewable energy development, leading to a significant rearrangement of economic and environmental policy.
% FOUNDING_PROBLEM: The Clean Air Act was established to protect and enhance the quality of the nation's air resources, addressing air pollution from various sources.
% FOUNDING_PROBLEM_CORROBORATION: The EPA and climate advocates attest that the problem of air pollution, particularly greenhouse gas emissions, remains a live and urgent concern. The coal power sector acknowledges the existence of air pollution but disputes the severity and the appropriate regulatory response, often citing economic impacts.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because this reading significantly limits the potential for emissions reductions, imposing a cost on climate goals and future generations. Suppression (0.75) is also high, as this interpretation actively suppresses more ambitious regulatory alternatives and the development of systemic solutions. The theater ratio (0.20) is relatively low, indicating that while the regulatory actions taken under this reading may be insufficient for climate goals, they are genuine facility-level improvements, not mere performance. The constraint requires active enforcement by the Supreme Court and other legal bodies to maintain this narrow interpretation against broader readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the coal power sector, this reading is a reasonable application of statutory limits, preserving state autonomy and economic stability. From the perspective of climate advocates, it is a highly extractive constraint that prioritizes short-term economic interests over long-term environmental imperatives. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The coal power sector and states with fossil fuel interests are clear beneficiaries (low d), as this reading protects their economic and political interests. The EPA, while the nominal agenda-setter, is also a victim (high d) because its statutory mission is constrained. Climate advocates and renewable energy developers are victims (high d) as their goals and market opportunities are suppressed. The Supreme Court acts as an institutional agenda-setter, enforcing this specific interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_interpretation_ambiguity,
    'Is the ''best system of emission reduction'' in Section 111(d) inherently limited to measures implementable at individual facilities, or does it encompass broader, grid-wide generation-shifting strategies?',
    'Further Supreme Court rulings or legislative clarification explicitly defining the scope of ''best system''.',
    'If resolved towards a broader interpretation, this constraint would be reclassified as a snare or piton, as its extractive function would be exposed as lacking statutory basis. If reaffirmed, its mountain-like qualities (as a fixed legal interpretation) would be strengthened, though its extractive nature would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_interpretation_ambiguity, conceptual, 'Ambiguity in the statutory language regarding the scope of EPA''s authority.').

omega_variable(
    economic_impact_of_generation_shifting,
    'What would be the true economic cost and benefit of a systemic, generation-shifting approach to Section 111(d) regulation, compared to the facility-specific approach?',
    'Comprehensive, independent economic modeling and cost-benefit analysis, accounting for both direct energy sector impacts and broader societal costs of climate change.',
    'If systemic shifting proves economically beneficial or neutral, the justification for the facility-specific constraint weakens, exposing its extractive nature. If costs are prohibitive, it might lend credence to the current reading''s ''coordination'' function of avoiding economic disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_generation_shifting, empirical, 'Uncertainty regarding the economic consequences of alternative regulatory approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, epa_regulatory_authority_constraint).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, us_energy_policy_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the broader Section 111(d) delegation kernel. It is linked to the 'systemic_transformation_reading' which represents an alternative, broader interpretation of EPA's authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
