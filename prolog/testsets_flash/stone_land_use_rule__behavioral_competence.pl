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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'Stone Land-Use Rule' as a live,
 *   behaviorally enforced prohibition against building in tsunami-vulnerable
 *   zones, grounded in the community's collective memory of past disasters.
 *   The stone markers are not merely commemorative; they actively shape daily
 *   spatial practice. This reading emphasizes the rule's persistence through
 *   sustained community competence and the acceptance of economic costs
 *   (e.g., foregoing development of prime coastal land) for safety. The
 *   constraint is claimed as a Mountain due to its deep integration into the
 *   community's survival strategy and its near-natural persistence through
 *   social transmission, despite having identifiable beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.25).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, mountain).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/land_use_governance").

domain_priors:emerges_naturally(stone_land_use_rule__behavioral_competence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'e4447f32-f160-4f65-8361-a4599615fdfd').
narrative_ontology:cs_kernel_codification('e4447f32-f160-4f65-8361-a4599615fdfd', formalized).
narrative_ontology:cs_authority_grounding('e4447f32-f160-4f65-8361-a4599615fdfd', practice).
narrative_ontology:cs_interpretation_layer_present('e4447f32-f160-4f65-8361-a4599615fdfd').
narrative_ontology:cs_reading_relation('e4447f32-f160-4f65-8361-a4599615fdfd', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('e4447f32-f160-4f65-8361-a4599615fdfd', foundational, disaster_memory_requires_behavioral_compliance).
narrative_ontology:cs_axiom_status(disaster_memory_requires_behavioral_compliance, holdable).
narrative_ontology:cs_axiom_grounding('e4447f32-f160-4f65-8361-a4599615fdfd', disaster_memory_requires_behavioral_compliance, empirically_contingent).
narrative_ontology:cs_axiom('e4447f32-f160-4f65-8361-a4599615fdfd', foundational, collective_safety_outweighs_individual_economic_gain).
narrative_ontology:cs_axiom_status(collective_safety_outweighs_individual_economic_gain, holdable).
narrative_ontology:cs_axiom_grounding('e4447f32-f160-4f65-8361-a4599615fdfd', collective_safety_outweighs_individual_economic_gain, deontological).
narrative_ontology:cs_reference_frame('e4447f32-f160-4f65-8361-a4599615fdfd', post_tsunami_collective_memory_enforcement).
narrative_ontology:cs_drift_state('e4447f32-f160-4f65-8361-a4599615fdfd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e4447f32-f160-4f65-8361-a4599615fdfd', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_community_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, potential_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of the coastal community who live outside the prohibited zone. They benefit from the safety provided by the rule, which prevents construction in areas vulnerable to tsunamis. Their daily spatial practices reinforce the rule, and their identity is tied to the community's survival through adherence to this memory.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_community_residents, beneficiary,
    powerless, generational, identity_locked, local).

% The local administrative body responsible for land-use planning and disaster preparedness. While they formally uphold the rule, its primary enforcement comes from community practice and memory, not active policing. They benefit from the community's self-regulation and reduced disaster risk.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, local_government, agenda_setter,
    institutional, generational, constrained, local).

% External or internal actors who might seek to develop land within the prohibited zone. They face social pressure and formal rejection of permits, incurring opportunity costs by being unable to utilize prime coastal real estate. Their attempts to challenge the rule are met with community solidarity.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, potential_developers, payer,
    moderate, immediate, mobile, local).

% Elders and community leaders who actively transmit the memory of past disasters and the rationale behind the stone markers. They ensure the rule's persistence through oral tradition, education, and social reinforcement, acting as the primary enforcers of behavioral competence.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_memory_keepers, agenda_setter,
    organized, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use behavior to prevent construction in areas historically vulnerable to tsunamis, ensuring community safety and long-term survival.
% TRANSFER_FUNCTION: Transfers the right to develop certain coastal land from potential developers to the collective safety and memory of the community. It also transfers the burden of remembering and enforcing the rule to community members.
% ABSENT_VOICES: Those who prioritize short-term economic gain from coastal development over long-term disaster resilience are effectively silenced by the community's strong social norms and the local government's adherence to the rule. Their voices are not absent due to coercion, but due to a lack of social legitimacy within the community.
% DISAPPEARANCE_RATIONALE: If the rule vanished, construction would likely resume in vulnerable areas, increasing the community's risk profile. The social fabric built around collective memory and resilience would erode, leading to potential future disaster and loss of life.
% FOUNDING_PROBLEM: The community faced recurrent devastating tsunamis, leading to significant loss of life and property due to settlement in vulnerable coastal areas.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, oral traditions, and geological evidence corroborate the past disasters and the ongoing threat. Disaster preparedness experts and anthropologists studying the community attest to the continued relevance of the rule for safety, independent of the local government or community leaders.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, ExtMetricName, E),
    domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(stone_land_use_rule__behavioral_competence),
    narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary 'cost' is foregone economic opportunity, which the community willingly accepts for safety. Suppression is also low (0.25) as compliance is driven by internalized norms and social pressure, not external coercion. Theater ratio is negligible (0.05) because the rule is genuinely functional and actively shapes behavior, not merely performed. Accessibility collapse is high (0.8) because the community's shared understanding of the risk makes alternative land-use patterns unthinkable. Resistance is low (0.08) due to strong community consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of coastal community residents and disaster memory keepers, the rule is a natural, self-evident truth for survival. From the perspective of potential developers, it represents a 'cost' or 'restriction' on economic activity. The engine's classification will reflect the community's internal framing as a Mountain, while acknowledging the external economic friction.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal community residents are beneficiaries (d near 0.0) as they gain safety and long-term viability. Local government also benefits from reduced disaster risk and community stability. Potential developers are payers (d near 1.0) as they bear the cost of foregone development opportunities. Disaster memory keepers are agenda-setters, actively maintaining the constraint for the community's benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the rule as a 'commemorative husk' (a Piton) by emphasizing its active behavioral enforcement and the live nature of the founding problem. The low theater ratio and high accessibility collapse indicate it is far from an atrophied constraint. The presence of beneficiaries on a claimed Mountain triggers False Summit Mountain detection, which is appropriate given the rule's constructed nature, even if it functions like a natural law for the community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''Stone Land-Use Rule'' a genuine natural law (a Mountain) for this community, or a highly effective social construct that functions like one?',
    'Comparative analysis with other disaster-prone communities lacking such a rule: if similar natural hazards lead to different land-use patterns without the rule, it suggests a social construct. If the rule''s behavioral force diminishes over generations without direct disaster experience, it points to a social construct.',
    'If a pure social construct, its classification as a Mountain might be re-evaluated to a highly stable Rope or even a Tangled Rope if subtle extraction is identified, despite its current low extractiveness. The FSM trigger is designed to flag this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between a genuine natural law and a deeply internalized social construct.').

omega_variable(
    commemorative_husk_vs_behavioral_competence,
    'To what extent do the stone markers function as active behavioral prohibitions versus mere commemorative artifacts?',
    'Ethnographic observation of daily land-use decisions, interviews with younger generations regarding their understanding of the markers, and analysis of permit applications for construction in prohibited zones. If permits are routinely denied and community members actively self-regulate, the behavioral competence reading is strong.',
    'If the markers are found to be primarily commemorative, the constraint would shift towards a Piton (commemorative_husk reading), with higher theater ratio and lower effective suppression, as its behavioral force would have atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commemorative_husk_vs_behavioral_competence, empirical, 'Distinguishing between active behavioral enforcement and symbolic commemoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 1946, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t1946, stone_land_use_rule__behavioral_competence, theater_ratio, 1946, 0.05).
narrative_ontology:measurement(ston_tr_t1965, stone_land_use_rule__behavioral_competence, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(ston_tr_t1985, stone_land_use_rule__behavioral_competence, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(ston_tr_t2005, stone_land_use_rule__behavioral_competence, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(ston_tr_t2024, stone_land_use_rule__behavioral_competence, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(ston_be_t1946, stone_land_use_rule__behavioral_competence, base_extractiveness, 1946, 0.1).
narrative_ontology:measurement(ston_be_t1965, stone_land_use_rule__behavioral_competence, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(ston_be_t1985, stone_land_use_rule__behavioral_competence, base_extractiveness, 1985, 0.13).
narrative_ontology:measurement(ston_be_t2005, stone_land_use_rule__behavioral_competence, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(ston_be_t2024, stone_land_use_rule__behavioral_competence, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t1946, stone_land_use_rule__behavioral_competence, suppression_requirement, 1946, 0.2).
narrative_ontology:measurement(ston_su_t1965, stone_land_use_rule__behavioral_competence, suppression_requirement, 1965, 0.22).
narrative_ontology:measurement(ston_su_t1985, stone_land_use_rule__behavioral_competence, suppression_requirement, 1985, 0.23).
narrative_ontology:measurement(ston_su_t2005, stone_land_use_rule__behavioral_competence, suppression_requirement, 2005, 0.24).
narrative_ontology:measurement(ston_su_t2024, stone_land_use_rule__behavioral_competence, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.08).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral_competence' reading of the 'stone_land_use_rule' kernel, emphasizing its active role in shaping land-use. The 'commemorative_husk' reading (a separate constraint) views the markers as symbolic without behavioral force. Their ε values differ significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
