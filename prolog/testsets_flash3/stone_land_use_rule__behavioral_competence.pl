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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint describes the 'stone rule' as a live land-use
 *   prohibition, where daily spatial practice enforces compliance with
 *   historical tsunami high-water marks. This reading emphasizes the rule's
 *   continued behavioral force and its role in disaster risk reduction,
 *   contrasting with a 'commemorative husk' reading where the stones are mere
 *   symbols. The constraint is claimed as a Mountain due to its deep
 *   integration into the natural hazard landscape and its persistence across
 *   generations, with low extractiveness reflecting the accepted costs for
 *   safety.
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
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, mountain).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(stone_land_use_rule__behavioral_competence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'e995539b-5742-422a-b9f6-8edf853ecfd1').
narrative_ontology:cs_kernel_codification('e995539b-5742-422a-b9f6-8edf853ecfd1', formalized).
narrative_ontology:cs_authority_grounding('e995539b-5742-422a-b9f6-8edf853ecfd1', practice).
narrative_ontology:cs_interpretation_layer_present('e995539b-5742-422a-b9f6-8edf853ecfd1').
narrative_ontology:cs_reading_relation('e995539b-5742-422a-b9f6-8edf853ecfd1', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('e995539b-5742-422a-b9f6-8edf853ecfd1', foundational, historical_tsunami_marks_define_safe_zone).
narrative_ontology:cs_axiom_status(historical_tsunami_marks_define_safe_zone, holdable).
narrative_ontology:cs_axiom_grounding('e995539b-5742-422a-b9f6-8edf853ecfd1', historical_tsunami_marks_define_safe_zone, empirically_contingent).
narrative_ontology:cs_axiom('e995539b-5742-422a-b9f6-8edf853ecfd1', foundational, collective_memory_is_active_prohibition).
narrative_ontology:cs_axiom_status(collective_memory_is_active_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('e995539b-5742-422a-b9f6-8edf853ecfd1', collective_memory_is_active_prohibition, conventional).
narrative_ontology:cs_reference_frame('e995539b-5742-422a-b9f6-8edf853ecfd1', community_enforced_tsunami_safety).
narrative_ontology:cs_drift_state('e995539b-5742-422a-b9f6-8edf853ecfd1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e995539b-5742-422a-b9f6-8edf853ecfd1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, developers).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, institutional_memory_preservation).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, disaster_risk_reduction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities live in areas historically devastated by tsunamis. They actively maintain the 'stone rule' through daily practice, ensuring new construction respects the historical high-water mark indicated by the stone markers. They bear the economic cost of building further inland or on higher ground, but benefit from reduced disaster risk.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_communities, beneficiary,
    organized, generational, constrained, local).

% The local government formally recognizes the stone rule in its zoning and building codes, reinforcing the community's practice. It faces pressure to allow development closer to the coast for economic reasons but prioritizes long-term safety based on historical memory. Its role is to codify and support, rather than actively enforce against widespread non-compliance.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, local_government, agenda_setter,
    institutional, generational, constrained, local).

% Developers seeking to build in the area must adhere to the land-use restrictions imposed by the stone rule. This often means higher land acquisition costs or more complex engineering for elevated structures. Their exit options are to build elsewhere or accept the reduced profit margins.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, developers, payer,
    moderate, immediate, constrained, local).

% These agencies study the effectiveness of traditional disaster mitigation strategies like the stone rule. They provide scientific corroboration for the rule's efficacy and advocate for its continued observance, seeing it as a successful example of community-based resilience.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_risk_management_agencies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions across generations to minimize exposure to recurrent tsunami hazards, ensuring collective safety by maintaining a safe distance from the coast.
% TRANSFER_FUNCTION: Transfers potential short-term economic gains from coastal development (e.g., tourism, fishing infrastructure) into long-term community safety and resilience, from developers and individual landowners to the entire coastal community.
% ABSENT_VOICES: Short-sighted economic interests or new residents unfamiliar with the historical context might advocate for relaxing the rule to permit more lucrative coastal development. Their voices are largely marginalized by the strong community consensus and formal government backing.
% DISAPPEARANCE_RATIONALE: If the stone rule vanished overnight, coastal development would likely creep closer to the shore, increasing vulnerability to future tsunamis. The long-term safety of the communities would be severely compromised, leading to predictable disaster cycles.
% FOUNDING_PROBLEM: Recurrent, devastating tsunamis that repeatedly destroyed coastal settlements and caused immense loss of life.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, geological evidence of past tsunami inundation, and ongoing scientific analysis by disaster risk management agencies corroborate the live threat. Community elders and local government officials also attest to the continued relevance of the rule, drawing on oral histories and institutional memory.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The extractiveness is low (0.15) because the costs (building further inland, higher construction costs) are widely accepted as necessary for safety, making the 'extraction' more akin to a coordination cost. Suppression is also low (0.25) because compliance is largely self-enforced through community norms and institutional memory, rather than active coercion. The theater ratio is negligible (0.05) as the rule's function remains directly tied to its stated purpose of safety. Accessibility collapse is high (0.88) because the historical record of tsunamis makes the alternative (building on the coast) clearly catastrophic, leaving few rational alternatives once the hazard is understood. Resistance is low (0.08) due to strong community consensus and the clear, existential threat the rule mitigates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the coastal communities and local government, the stone rule is a vital, almost natural, adaptation to their environment. From the perspective of developers, it's a costly restriction. However, the shared understanding of tsunami risk largely aligns their long-term interests, making the rule's 'mountain' classification robust from most seats. The primary divergence is with the 'commemorative_husk' reading, which would see the rule as a decayed symbol without behavioral force.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal communities are beneficiaries, accepting economic costs for collective safety. The local government acts as an agenda-setter, formalizing and supporting the rule. Developers are payers, bearing the direct economic costs of restricted land use. Disaster risk management agencies are observers, validating the rule's efficacy. All actors, even payers, ultimately benefit from the safety the rule provides, leading to low overall extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_decay_risk,
    'To what extent is the behavioral competence of the stone rule vulnerable to decay over time, particularly with new generations lacking direct experience of tsunamis?',
    'Longitudinal ethnographic studies tracking compliance rates and community knowledge transmission across generations, especially after periods of quiescence in tsunami activity.',
    'If behavioral decay is significant, the constraint''s classification would drift from Mountain towards Piton or Snare, as its functional basis erodes and its persistence relies on inertia or latent coercion rather than active competence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_decay_risk, empirical, 'Risk of the rule''s behavioral force diminishing over time.').

omega_variable(
    commemorative_vs_prohibitive_framing,
    'Is the stone rule primarily understood as a commemorative artifact (a ''husk'') or as an active, binding land-use prohibition (behavioral competence)?',
    'Analysis of local discourse, land-use planning documents, and observed construction practices. If new construction consistently respects the markers, it''s prohibitive. If development encroaches, it''s commemorative.',
    'If primarily commemorative, the constraint''s extractiveness would be near zero (no behavioral cost), and its classification would shift to Piton or even a non-constraint, as its functional role has atrophied. This reading asserts it is prohibitive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commemorative_vs_prohibitive_framing, conceptual, 'Distinguishing between symbolic and active behavioral force of the rule.').

omega_variable(
    natural_vs_constructed_origin,
    'Is the ''naturalness'' of the stone rule (emerges_naturally: true) a genuine reflection of its deep integration with the hazard landscape, or a constructed narrative that benefits coastal communities by externalizing development costs?',
    'Comparative analysis with other disaster-prone regions lacking such rules: do they experience higher costs or greater losses? Examination of the historical process of rule formation: was it a ''natural'' adaptation or a deliberate, contested policy choice?',
    'If primarily constructed, the ''mountain'' classification would be a false summit, reclassifying to Tangled Rope or Snare, as identifiable beneficiaries (coastal communities) would be seen as extracting safety at the expense of developers through a ''natural law'' narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_origin, conceptual, 'Ambiguity between natural adaptation and constructed policy for the stone rule.').


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
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ston_be_t15, stone_land_use_rule__behavioral_competence, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__behavioral_competence, base_extractiveness, 30, 0.13).
narrative_ontology:measurement(ston_be_t45, stone_land_use_rule__behavioral_competence, base_extractiveness, 45, 0.14).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ston_su_t15, stone_land_use_rule__behavioral_competence, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__behavioral_competence, suppression_requirement, 30, 0.23).
narrative_ontology:measurement(ston_su_t45, stone_land_use_rule__behavioral_competence, suppression_requirement, 45, 0.24).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__behavioral_competence, suppression_requirement, 60, 0.25).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral_competence' reading of the 'stone_land_use_rule' kernel. It is structurally distinct from the 'commemorative_husk' reading, which views the stones as symbolic rather than behaviorally binding. Both are linked as part of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
