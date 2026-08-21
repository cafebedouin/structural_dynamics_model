% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule: Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'Stone' land-use rule as a live,
 *   behaviorally enforced prohibition, stemming from a historical disaster.
 *   It is sustained by daily spatial practices and community memory,
 *   effectively preventing development in hazardous areas. This is one
 *   reading of the 'stone_land_use_rule' kernel, focusing on its active,
 *   functional role in ensuring collective safety. The alternative
 *   'commemorative_husk' reading views the rule as a decayed symbol without
 *   behavioral force.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.4).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule: Behavioral Competence Reading").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(stone_land_use_rule__behavioral_competence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'b799a9f5-9a17-4cb4-a189-cdf140282ea4').
narrative_ontology:cs_kernel_codification('b799a9f5-9a17-4cb4-a189-cdf140282ea4', formalized).
narrative_ontology:cs_authority_grounding('b799a9f5-9a17-4cb4-a189-cdf140282ea4', practice).
narrative_ontology:cs_reading_relation('b799a9f5-9a17-4cb4-a189-cdf140282ea4', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('b799a9f5-9a17-4cb4-a189-cdf140282ea4', foundational, disaster_memory_requires_behavioral_compliance).
narrative_ontology:cs_axiom_status(disaster_memory_requires_behavioral_compliance, holdable).
narrative_ontology:cs_axiom_grounding('b799a9f5-9a17-4cb4-a189-cdf140282ea4', disaster_memory_requires_behavioral_compliance, empirically_contingent).
narrative_ontology:cs_axiom('b799a9f5-9a17-4cb4-a189-cdf140282ea4', foundational, collective_safety_overrides_individual_land_rights).
narrative_ontology:cs_axiom_status(collective_safety_overrides_individual_land_rights, holdable).
narrative_ontology:cs_axiom_grounding('b799a9f5-9a17-4cb4-a189-cdf140282ea4', collective_safety_overrides_individual_land_rights, deontological).
narrative_ontology:cs_reference_frame('b799a9f5-9a17-4cb4-a189-cdf140282ea4', post_disaster_reconstruction_consensus).
narrative_ontology:cs_drift_state('b799a9f5-9a17-4cb4-a189-cdf140282ea4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b799a9f5-9a17-4cb4-a189-cdf140282ea4', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, community_residents).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, future_generations).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, land_developers).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, individual_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the safety and stability provided by avoiding hazardous land use. They actively participate in the daily spatial practices that reinforce the rule, internalizing its necessity for collective well-being.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, community_residents, beneficiary,
    moderate, biographical, constrained, local).

% Administers and implicitly enforces the land-use rule through zoning, permitting, and public education. While not codified in modern law, its authority is derived from long-standing community consensus and historical memory of disaster.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, local_governance, agenda_setter,
    institutional, generational, constrained, local).

% Bear the cost of being excluded from developing prime land within the restricted zone. They face economic disincentives and social pressure if they attempt to challenge the rule, making compliance the path of least resistance despite potential profits.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, land_developers, payer,
    powerful, immediate, constrained, regional).

% Experience restrictions on how they can use or develop their property within the designated area. While they accept the rule for collective safety, it represents a direct economic cost in terms of lost development potential or property value.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, individual_landowners, payer,
    moderate, biographical, constrained, local).

% Benefit from the continued safety and resilience of the community, inheriting a landscape free from the risks that led to the original disaster. Their interests are represented by the current community's adherence to the rule.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Study the efficacy of traditional land-use rules in mitigating disaster risk. They provide an external, evidence-based perspective on the rule's continued relevance and effectiveness, corroborating its function.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_risk_analysts, observer,
    analytical, civilizational, analytical, universal).

% Represent a perspective that views the 'Stone' as primarily a historical marker or symbolic warning, rather than an active behavioral prohibition. They would argue for greater flexibility in land use, but their view is currently marginalized by the dominant behavioral competence reading.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, commemorative_husk_advocates, excluded,
    moderate, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates community land-use practices to collectively avoid re-exposure to a historically catastrophic natural hazard, ensuring long-term safety and resilience.
% TRANSFER_FUNCTION: Transfers the right to develop certain land parcels from individual landowners and developers to the collective interest of community safety, preventing future loss of life and property.
% ABSENT_VOICES: Advocates for purely economic development or those who view the rule as an outdated relic are largely excluded from the active enforcement and interpretation of the rule. They would argue for re-evaluating the costs and benefits of the prohibition.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, economic pressures would quickly lead to development in the hazardous zone. This would reintroduce the original risks, leading to future disasters and a fundamental reorganization of the community's safety and economic landscape.
% FOUNDING_PROBLEM: The community experienced a devastating natural disaster (e.g., landslide, flood) that rendered certain land areas extremely hazardous for habitation or development, leading to significant loss of life and property.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, oral traditions, and ongoing disaster risk analyses (from disaster_risk_analysts) consistently corroborate the original hazard and the continued risk, affirming the founding problem as live. The community's sustained compliance also serves as internal corroboration.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that while there are economic costs to restricted land use, these are widely accepted as necessary for collective safety, making the constraint a net benefit for the community. Suppression (0.40) is moderate, as enforcement relies on social norms and daily practice rather than overt coercion, but it is active. Theater ratio (0.05) is very low, indicating the rule is genuinely functional and not merely performative. Accessibility collapse (0.85) is high because alternatives to non-development are effectively foreclosed. Resistance (0.15) is low due to sustained compliance over 78 years.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community residents, the rule is a vital, life-saving coordination mechanism. From the perspective of land developers, it is a restrictive barrier to economic opportunity. The 'behavioral_competence' reading emphasizes the former, while acknowledging the latter as an accepted cost. The 'commemorative_husk' reading would invert this, seeing the economic cost as primary and the safety function as negligible.
 *
 * DIRECTIONALITY LOGIC:
 *   Community residents and future generations are clear beneficiaries, gaining safety and resilience. Land developers and individual landowners are payers, bearing the economic costs of restricted land use. Local governance acts as the agenda-setter, maintaining the rule through implicit and explicit means. The directionality for payers is upward (more target-like) due to the economic costs, but the overall low extractiveness means the effective extraction is still low.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_function,
    'Is the ''Stone'' land-use rule primarily a live behavioral prohibition or a decayed commemorative symbol?',
    'Empirical observation of land-use patterns in the hazardous zone, analysis of development proposals, and ethnographic studies of community decision-making regarding the area. If no development occurs and challenges are consistently rejected, it supports the behavioral reading.',
    'If resolved as a decayed symbol, the constraint''s effective extractiveness would be near zero (no real behavioral cost), and its classification would shift towards Piton or even Mountain (as a natural feature). If resolved as a live prohibition, the current Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_function, empirical, 'The core contest between the behavioral_competence and commemorative_husk readings.').

omega_variable(
    cost_benefit_acceptance_threshold,
    'At what point would the economic costs of the land-use prohibition outweigh the perceived safety benefits for the community, leading to increased resistance?',
    'Economic modeling of alternative land uses, demographic shifts, and changes in community risk perception. Monitoring of public discourse and local governance decisions for signs of increasing challenge to the rule.',
    'If the cost-benefit balance shifts, the constraint''s extractiveness and resistance metrics would rise, potentially shifting its classification towards Tangled Rope or Snare if the coordination function is perceived to fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_acceptance_threshold, empirical, 'The dynamic threshold of community acceptance for the rule''s costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ston_tr_t15, stone_land_use_rule__behavioral_competence, theater_ratio, 15, 0.05).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__behavioral_competence, theater_ratio, 30, 0.05).
narrative_ontology:measurement(ston_tr_t45, stone_land_use_rule__behavioral_competence, theater_ratio, 45, 0.05).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__behavioral_competence, theater_ratio, 60, 0.05).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ston_be_t15, stone_land_use_rule__behavioral_competence, base_extractiveness, 15, 0.13).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__behavioral_competence, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(ston_be_t45, stone_land_use_rule__behavioral_competence, base_extractiveness, 45, 0.14).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ston_su_t15, stone_land_use_rule__behavioral_competence, suppression_requirement, 15, 0.37).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__behavioral_competence, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(ston_su_t45, stone_land_use_rule__behavioral_competence, suppression_requirement, 45, 0.39).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__behavioral_competence, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
