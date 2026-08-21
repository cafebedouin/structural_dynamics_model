% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: State-led Qualitative Development Mandate
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes the state's mandate to shift its economic
 *   development model from raw quantitative growth to 'high-quality
 *   development,' emphasizing innovation, sustainability, and efficiency.
 *   This involves significant state intervention to restructure industries,
 *   diverting resources from traditional sectors to strategic high-tech ones.
 *   The constraint is a reading of the broader 'performance_legitimacy'
 *   kernel, where the state's right to rule is tied to its ability to deliver
 *   economic outcomes, but here, the definition of 'performance' has shifted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.65).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "State-led Qualitative Development Mandate").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '184e58ec-6281-449c-a005-1afc4cdc86e5').
narrative_ontology:cs_kernel_codification('184e58ec-6281-449c-a005-1afc4cdc86e5', formalized).
narrative_ontology:cs_authority_grounding('184e58ec-6281-449c-a005-1afc4cdc86e5', extraction).
narrative_ontology:cs_interpretation_layer_present('184e58ec-6281-449c-a005-1afc4cdc86e5').
narrative_ontology:cs_reading_relation('184e58ec-6281-449c-a005-1afc4cdc86e5', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('184e58ec-6281-449c-a005-1afc4cdc86e5', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_reading_relation('184e58ec-6281-449c-a005-1afc4cdc86e5', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('184e58ec-6281-449c-a005-1afc4cdc86e5', foundational, sustainable_innovation_is_primary_legitimacy_driver).
narrative_ontology:cs_axiom_status(sustainable_innovation_is_primary_legitimacy_driver, holdable).
narrative_ontology:cs_axiom_grounding('184e58ec-6281-449c-a005-1afc4cdc86e5', sustainable_innovation_is_primary_legitimacy_driver, instrumental).
narrative_ontology:cs_axiom('184e58ec-6281-449c-a005-1afc4cdc86e5', foundational, efficiency_over_raw_growth).
narrative_ontology:cs_axiom_status(efficiency_over_raw_growth, holdable).
narrative_ontology:cs_axiom_grounding('184e58ec-6281-449c-a005-1afc4cdc86e5', efficiency_over_raw_growth, conventional).
narrative_ontology:cs_reference_frame('184e58ec-6281-449c-a005-1afc4cdc86e5', sustainable_innovation_driven_economy).
narrative_ontology:cs_drift_state('184e58ec-6281-449c-a005-1afc4cdc86e5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('184e58ec-6281-449c-a005-1afc4cdc86e5', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for formulating and implementing national development strategies, prioritizing innovation, sustainability, and efficiency. They direct state resources and policy support to favored sectors and regions, often at the expense of others.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_development_planners, agenda_setter,
    institutional, generational, constrained, national).

% Receive significant state subsidies, preferential policies, R&D funding, and market access. They are expected to drive innovation and contribute to the 'high-quality' transformation, benefiting from the state's strategic direction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    organized, biographical, mobile, national).

% Comprises state-owned enterprises, venture capital funds, and research institutions aligned with national development goals. They are direct recipients of state capital and policy support, tasked with fostering innovation and industrial upgrading.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, constrained, national).

% Face reduced state support, stricter environmental regulations, and pressure to upgrade or consolidate. They bear the costs of the transition, often experiencing declining profitability and market share as resources are diverted to new industries.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors, payer,
    organized, biographical, constrained, national).

% Historically reliant on land sales and taxes from traditional industries for revenue. They face fiscal pressure as the central government prioritizes qualitative development, limiting their ability to fund local services and infrastructure.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, constrained, regional).

% Workers in traditional industries who lose jobs due to restructuring and automation. They often lack the skills for new sectors and face limited social safety nets, bearing significant personal costs of the transition without a voice in its planning.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, displaced_workers, excluded,
    powerless, immediate, trapped, local).

% Monitor the economic and social impacts of the development strategy, assessing its effectiveness, sustainability, and human rights implications. Their analysis can influence international investment and diplomatic relations.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate national resources, policy, and industrial efforts towards strategic, high-value industries and sustainable growth, moving the economy away from resource-intensive, low-value production and avoiding the 'middle-income trap'.
% TRANSFER_FUNCTION: Transfers capital, policy support, market access, and regulatory burdens from traditional, property-dependent sectors to high-tech, innovation-driven sectors, often resulting in reduced local government revenue and employment in old industries.
% ABSENT_VOICES: Displaced workers, small traditional businesses, and local communities reliant on old industries are largely excluded from the planning process; they would object to the social costs, lack of support during transition, and the top-down nature of the transformation.
% DISAPPEARANCE_RATIONALE: If this mandate vanished, the state's economic policy would likely revert to prioritizing raw GDP growth or short-term stability, leading to different investment patterns, industrial structure, and potentially a return to unsustainable practices.
% FOUNDING_PROBLEM: The nation faced an unsustainable growth model characterized by over-reliance on low-value manufacturing, severe environmental degradation, and a looming 'middle-income trap' due to lack of global competitiveness in advanced industries.
% FOUNDING_PROBLEM_CORROBORATION: International development organizations, independent economists, and environmental scientists corroborate the long-term risks of the old growth model and the necessity of structural transformation, supporting the shifted-function reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine national coordination function (strategic economic transformation) but involves significant asymmetric extraction. Extractiveness is high (0.65) as traditional sectors and local governments bear substantial costs without commensurate benefits. Suppression (0.75) is high due to the state's active enforcement of industrial policies and suppression of resistance from displaced sectors. Theater ratio (0.40) is moderate; while there's genuine effort towards transformation, some aspects of 'innovation' and 'sustainability' can be performative or used to justify top-down control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planners and high-tech sectors, this is a necessary, forward-looking coordination effort for national prosperity. From the perspective of traditional industries and local governments, it is an extractive process that dismantles their existing economic base and revenue streams, enforced by state power. The engine's computation of per-seat types will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State development planners and the high-tech/innovation ecosystem are clear beneficiaries, receiving resources and policy support. Traditional manufacturing sectors and property-dependent local governments are victims, facing divestment and fiscal pressure. Displaced workers are excluded victims, bearing the social costs. International observers provide an analytical perspective. The state's legitimacy is tied to the success of this transformation, making it both an agenda-setter and a beneficiary of the new economic structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a Piton because its mandate (achieving high-quality development) is actively pursued and still considered live, with clear beneficiaries. It avoids being a pure Snare by having a genuine, albeit contested, coordination function for national strategic goals. The ongoing extraction and active enforcement, however, prevent it from being a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qualitative_vs_quantitative_tradeoff,
    'To what extent does prioritizing ''high-quality development'' genuinely lead to long-term benefits that outweigh the short-term costs of lower growth and social disruption?',
    'Longitudinal economic and social impact studies comparing regions/nations that adopted this strategy versus those that maintained a quantitative growth focus, accounting for confounding factors.',
    'If long-term benefits are demonstrably elusive or insufficient, the ''coordination'' aspect of this Tangled Rope would weaken, pushing it closer to a Snare. If benefits are clear, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualitative_vs_quantitative_tradeoff, empirical, 'Empirical validation of the ''high-quality development'' strategy''s net benefits.').

omega_variable(
    legitimacy_grounding_ambiguity,
    'Is the state''s legitimacy truly grounded in delivering ''high-quality development'', or is this framing a post-hoc rationalization for maintaining control and directing resources to favored sectors?',
    'Analysis of public discourse, policy shifts, and elite rhetoric in response to economic performance. If the narrative shifts significantly when ''high-quality'' goals are not met, it supports the grounding claim. If control persists regardless, it suggests a deeper, more extractive grounding.',
    'If the grounding is primarily extractive, the constraint''s effective extraction would be higher, and its classification might shift towards a Snare, as the coordination story would be revealed as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_grounding_ambiguity, conceptual, 'Ambiguity in the true source of state legitimacy for this development model.').

omega_variable(
    social_cost_distribution,
    'Are the social safety nets and retraining programs for displaced workers adequate to mitigate the costs of industrial restructuring, or are these costs disproportionately borne by the powerless?',
    'Detailed social impact assessments, surveys of affected communities, and analysis of government spending on social welfare and labor market programs. Comparison with international best practices.',
    'If social costs are not adequately mitigated, the ''victim'' status of displaced workers is amplified, increasing the overall effective extraction and potentially pushing the constraint towards a Snare due to unaddressed social harms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_cost_distribution, empirical, 'Assessment of the equity of social cost distribution during economic transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
