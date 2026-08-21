% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Ritual (Hybrid Atrophy Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story analyzes a ritual, originally designed to preserve
 *   survival-competence and catastrophe memory, through a 'hybrid atrophy'
 *   reading. Under modernity, its direct adaptive function has atrophied, but
 *   the practice persists as a form of mourning and a mechanism for
 *   collective identity preservation. The constraint is claimed as a Piton,
 *   reflecting its degraded function maintained by inertia and identity-lock,
 *   rather than active utility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.35).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Ritual (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'aefb7bec-a252-4d3a-affb-d83e18b5335f').
narrative_ontology:cs_kernel_codification('aefb7bec-a252-4d3a-affb-d83e18b5335f', implicit).
narrative_ontology:cs_authority_grounding('aefb7bec-a252-4d3a-affb-d83e18b5335f', lineage).
narrative_ontology:cs_interpretation_layer_present('aefb7bec-a252-4d3a-affb-d83e18b5335f').
narrative_ontology:cs_reading_relation('aefb7bec-a252-4d3a-affb-d83e18b5335f', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('aefb7bec-a252-4d3a-affb-d83e18b5335f', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('aefb7bec-a252-4d3a-affb-d83e18b5335f', foundational, ritual_function_evolves).
narrative_ontology:cs_axiom_status(ritual_function_evolves, holdable).
narrative_ontology:cs_axiom_grounding('aefb7bec-a252-4d3a-affb-d83e18b5335f', ritual_function_evolves, empirically_contingent).
narrative_ontology:cs_axiom('aefb7bec-a252-4d3a-affb-d83e18b5335f', foundational, adaptive_function_can_atrophy).
narrative_ontology:cs_axiom_status(adaptive_function_can_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('aefb7bec-a252-4d3a-affb-d83e18b5335f', adaptive_function_can_atrophy, empirically_contingent).
narrative_ontology:cs_reference_frame('aefb7bec-a252-4d3a-affb-d83e18b5335f', original_adaptive_function).
narrative_ontology:cs_drift_state('aefb7bec-a252-4d3a-affb-d83e18b5335f', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aefb7bec-a252-4d3a-affb-d83e18b5335f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and perform the ritual, bearing its time and resource costs. They experience it primarily as a duty to tradition and a means of maintaining group identity, with little direct adaptive payoff for contemporary survival challenges. Exiting means abandoning their cultural and social identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Administer, interpret, and transmit the ritual. They are bound by tradition but also hold authority in shaping its contemporary meaning and practice. They often emphasize the ritual's continuity and symbolic importance, sometimes downplaying the atrophy of its original adaptive function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, community_elders, agenda_setter,
    institutional, generational, constrained, local).

% The collective entity representing past generations who directly benefited from the ritual's original function of preserving survival-competence and collective memory in the face of catastrophe. They are a beneficiary in a historical, non-active sense.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors).

% The collective identity of the group, which is reinforced and sustained by the ritual's continued practice. This identity provides cohesion and a sense of belonging, acting as a diffuse, non-agent beneficiary of the ritual's current form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity, beneficiary,
    analytical, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).

% Study the ritual's evolution, its historical and contemporary functions, and its impact on the community. They analyze the gap between claimed function and observed practice, and the mechanisms of cultural transmission and change.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, cultural_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally, the ritual coordinated collective action and knowledge transmission for survival in the aftermath of catastrophe. Currently, it coordinates collective memory, group cohesion, and the transmission of cultural identity across generations.
% TRANSFER_FUNCTION: Historically, it transferred practical survival knowledge and threat-recognition capacity. In its atrophied form, it transfers symbolic continuity, a sense of shared history, and reinforces in-group identity, from community elders to present-generation practitioners.
% ABSENT_VOICES: Those who have left the community due to the perceived anachronism or burden of the ritual, or younger members who question its relevance and cost in a modern context. Their voices are often marginalized by the authority of tradition and elders.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community's collective identity, sense of historical continuity, and social cohesion would be severely disrupted. While its original survival function is largely obsolete, its role in identity formation is still significant.
% FOUNDING_PROBLEM: To preserve critical survival knowledge, collective trauma memory, and social cohesion across generations following an existential catastrophe, ensuring the group's long-term resilience.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and oral traditions within the community attest to the original survival problem. Cultural anthropologists and some younger practitioners argue that while collective memory remains important, the specific survival-competence aspect of the founding problem is largely 'dead' in modern contexts, leading to the 'contested' status.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` declines over time (from 0.65 to 0.35) because the direct, tangible benefits of the ritual for survival have diminished, while the costs to practitioners remain. The `theater_ratio` increases (from 0.20 to 0.70) as the ritual becomes more performative and symbolic, with less functional output. `Suppression` is low (0.30) because persistence relies on internalized identity and social pressure rather than overt coercion. `Accessibility_collapse` is moderate (0.50) as alternatives exist but are culturally costly. `Resistance` is low (0.25) due to the strong identity-lock on practitioners. The `claimed_type` is Piton because its primary function has atrophied, but it persists due to institutional inertia and the diffuse benefits of identity maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Community elders, who are the agenda-setters, tend to emphasize the ritual's enduring symbolic and identity-preserving functions, often downplaying the loss of its original adaptive utility. Present-generation practitioners, as payers, experience the ritual's costs more acutely relative to its perceived direct benefits, leading to a potential divergence in how they classify the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'in_group_identity' and 'historical_ancestors' are beneficiaries, as the ritual either sustains the collective identity or historically ensured survival. 'Present_generation_practitioners' are the primary victims, bearing the costs of practice without the original adaptive payoff, and are 'identity_locked' due to the deep cultural integration of the ritual. 'Community_elders' act as agenda-setters, maintaining the tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of preserving direct survival-competence has largely atrophied, but the ritual persists by shifting its function to collective mourning and identity preservation. This makes it a classic case of mandatrophy, where the constraint's form outlives its initial purpose, maintained by inertia and new, diffuse benefits. The Piton classification captures this atrophy and the performative nature of its continued existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_primary_driver,
    'Is the ritual''s persistence primarily driven by its residual adaptive function (however diminished), or by its role in identity and mourning?',
    'Longitudinal ethnographic study tracking community resilience outcomes vs. identity cohesion metrics, or a counterfactual analysis of community dissolution rates if the ritual ceased.',
    'If residual adaptive function is still significant, the extractiveness might be lower than estimated, and the classification might lean closer to a degraded Rope. If identity/mourning is the sole driver, the Piton classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_primary_driver, empirical, 'Determines the primary driver of the ritual''s persistence.').

omega_variable(
    identity_lock_strength_measurement,
    'How strong is the ''identity-lock'' on present-generation practitioners, and what specific mechanisms (social, psychological, theological) enforce it?',
    'Sociological surveys, qualitative interviews, and analysis of ex-member narratives to quantify the social and psychological costs of exiting the ritual practice.',
    'A stronger identity-lock would increase the effective suppression and extractiveness experienced by practitioners, potentially pushing the constraint closer to a Snare from their perspective, even if the base extractiveness is low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength_measurement, empirical, 'Measures the strength and mechanisms of identity-based constraint.').

omega_variable(
    mandate_drift_acknowledgment,
    'To what extent do community elders and leaders explicitly acknowledge the atrophy of the ritual''s original survival-competence mandate?',
    'Content analysis of public statements, sermons, and educational materials from community leaders over time, compared with private interviews.',
    'If the atrophy is widely acknowledged, it suggests a more adaptive, self-aware community, potentially leading to reforms that reduce extractiveness. If denied, it reinforces the ''theater'' aspect and the Piton classification, indicating a resistance to change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_drift_acknowledgment, empirical, 'Assesses the community''s awareness of the ritual''s functional shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.65).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.7).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.31).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
