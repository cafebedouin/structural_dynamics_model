% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Ritual Enforced Group Boundaries via Shared Mourning
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how a shared mourning ritual functions
 *   primarily to enforce group boundaries, fostering in-group cohesion at the
 *   cost of individual autonomy and exclusion of out-group members. It is one
 *   reading of the 'catastrophe_memory_kernel,' emphasizing the ritual's role
 *   in social control and identity demarcation rather than purely symbolic
 *   continuity or trauma processing. The claimed type is 'tangled_rope'
 *   because it serves a genuine coordination function (group cohesion) but
 *   also involves asymmetric extraction (from individuals and out-groups) and
 *   requires active enforcement of its norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Ritual Enforced Group Boundaries via Shared Mourning").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '985215c6-e21f-4e59-911a-8e2c823d0e55').
narrative_ontology:cs_kernel_codification('985215c6-e21f-4e59-911a-8e2c823d0e55', implicit).
narrative_ontology:cs_authority_grounding('985215c6-e21f-4e59-911a-8e2c823d0e55', practice).
narrative_ontology:cs_interpretation_layer_present('985215c6-e21f-4e59-911a-8e2c823d0e55').
narrative_ontology:cs_reading_relation('985215c6-e21f-4e59-911a-8e2c823d0e55', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('985215c6-e21f-4e59-911a-8e2c823d0e55', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('985215c6-e21f-4e59-911a-8e2c823d0e55', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('985215c6-e21f-4e59-911a-8e2c823d0e55', foundational, group_survival_requires_distinct_identity).
narrative_ontology:cs_axiom_status(group_survival_requires_distinct_identity, holdable).
narrative_ontology:cs_axiom_grounding('985215c6-e21f-4e59-911a-8e2c823d0e55', group_survival_requires_distinct_identity, conventional).
narrative_ontology:cs_axiom('985215c6-e21f-4e59-911a-8e2c823d0e55', secondary, ritual_enforces_social_cohesion).
narrative_ontology:cs_axiom_status(ritual_enforces_social_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('985215c6-e21f-4e59-911a-8e2c823d0e55', ritual_enforces_social_cohesion, empirically_contingent).
narrative_ontology:cs_reference_frame('985215c6-e21f-4e59-911a-8e2c823d0e55', collective_identity_preservation).
narrative_ontology:cs_drift_state('985215c6-e21f-4e59-911a-8e2c823d0e55', contemporary_group_dynamics, gap(stable, minor, false)).
narrative_ontology:cs_created_at('985215c6-e21f-4e59-911a-8e2c823d0e55', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, group_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the mourning ritual, ensuring adherence to its forms and meanings. Benefits from the enhanced group cohesion and the solidified authority that results from maintaining clear group boundaries.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, group_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Participate in the shared mourning practice, gaining a strong sense of collective identity, belonging, and solidarity. However, they bear the cost of conforming to ritual norms and suppressing individual expressions that might challenge group boundaries. Their identity is deeply intertwined with the group's practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members, beneficiary,
    organized, biographical, identity_locked, global).

% Represents the individual's capacity for self-determination and independent thought. It bears the cost of conformity, as the ritual's boundary-maintenance function often requires individuals to align their beliefs and behaviors with group norms, limiting personal expression and choice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy, payer,
    powerless, biographical, trapped, local).

% Are explicitly or implicitly excluded from the core mourning practice and the associated group identity. They are often viewed with suspicion or as 'other,' bearing the cost of social distance and potential stigmatization, which reinforces the in-group's boundaries.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members, excluded,
    powerless, biographical, mobile, global).

% Study the ritual's function in maintaining group boundaries, its impact on individual members, and its role in inter-group relations. They analyze the structural dynamics without direct participation or benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, anthropological_observers, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, group_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and social cohesion by establishing clear in-group/out-group boundaries through shared ritualized mourning, ensuring a unified response to perceived external threats or internal fragmentation.
% TRANSFER_FUNCTION: Transfers individual autonomy and the potential for diverse out-group affiliations to the collective, in exchange for group solidarity and the maintenance of leadership authority.
% ABSENT_VOICES: Individuals seeking greater personal autonomy, those advocating for more permeable group boundaries, or members of excluded out-groups are often marginalized or silenced within the group's discourse, as their perspectives directly challenge the boundary-maintenance function of the ritual.
% DISAPPEARANCE_RATIONALE: If the ritual and its enforcement vanished overnight, the group's distinct boundaries would blur, internal cohesion would weaken, and individual members would likely seek alternative affiliations or expressions of identity, leading to a significant reorganization of social structures and power dynamics within and around the group.
% FOUNDING_PROBLEM: To solidify group identity and ensure collective survival in the face of existential threats or internal fragmentation, particularly after a catastrophic event that severely challenged the group's existence and sense of self.
% FOUNDING_PROBLEM_CORROBORATION: Group historians, sociologists, and the current leadership attest to the historical necessity of such rituals for group survival and identity formation. Independent academic analyses often corroborate the founding problem's historical existence and its continued relevance for maintaining group cohesion, even if the current form and intensity of the ritual are debated.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the significant costs borne by individuals in terms of conformity and the exclusion of others. Suppression (0.7) is high due to the social pressure and potential sanctions for non-adherence to ritual norms, which are crucial for maintaining the group's distinct identity. The theater ratio (0.2) is low, indicating that while the ritual has performative elements, its primary function is actively structural and enforced, not merely theatrical. The increasing trend in extractiveness and suppression over the interval suggests a hardening of group boundaries, possibly in response to perceived external threats or internal challenges to cohesion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'group_leadership' and many 'in_group_members,' the ritual is a necessary 'rope' for collective survival and identity. However, from the 'individual_autonomy' and 'out_group_members' seats, it operates as a 'snare' or 'tangled_rope,' imposing significant costs and limitations. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'group_leadership' and 'in_group_members' are beneficiaries, gaining cohesion and authority, placing them at the lower end of the directionality spectrum. 'Individual_autonomy' and 'out_group_members' are targets, bearing the costs of conformity and exclusion, placing them at the higher end. The 'identity_locked' exit option for in-group members further amplifies their effective extraction, as their self-concept is fused with the group's practices.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (solidifying identity after catastrophe) is still considered 'live' by the group, which prevents a clear mandatrophy resolution. However, the increasing extractiveness and suppression suggest that the boundary-maintenance function may be over-performing its original mandate, potentially shifting from necessary cohesion to excessive control. The 'contested' status of the founding problem indicates that external observers or dissenting voices might argue for a re-evaluation of the ritual's current function against its historical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_cost_of_conformity,
    'What is the actual cost to individual autonomy and well-being imposed by the ritual''s conformity requirements, relative to the perceived benefits of group cohesion?',
    'Longitudinal studies tracking individual psychological well-being, rates of internal dissent, and observed instances of suppressed expression within the group, compared to similar groups with less rigid ritual enforcement.',
    'If the cost to individual autonomy is disproportionately high, it would strengthen the classification as a Snare, indicating that the ''coordination'' is primarily a cover for extraction. If costs are low, it would support the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_cost_of_conformity, empirical, 'Assesses the true cost of conformity versus the claimed benefits of cohesion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of individual autonomy primarily structural (external group pressure, social sanctions) or internalized (self-censorship, identity fusion)?',
    'Post-exit suppression trajectory: if individuals who leave the group continue to exhibit self-censorship or difficulty forming new identities, it suggests a significant internalized component. If suppression immediately dissipates, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making exit less effective as a remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual conformity.').

omega_variable(
    coordination_extraction_boundary,
    'Is the ritual''s boundary-maintenance function structurally necessary for genuine group cohesion, or is the enforcement of boundaries primarily a mechanism for leadership to extract conformity?',
    'Comparative analysis of groups that achieve similar levels of cohesion with less rigid boundary-maintenance rituals or more inclusive practices. If cohesion persists without strict enforcement, the functions are separable.',
    'If separable, the boundary enforcement is pure extraction riding on a real coordination function; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the ritual''s coordination and extraction components are structurally separable.').

omega_variable(
    kernel_reading_distinction,
    'How does this ''boundary_maintenance_reading'' structurally differ from sibling readings like ''symbol_continuity_reading'' or ''trauma_encoding_reading''?',
    'Detailed ethnographic studies focusing on the primary function emphasized by group members and leadership, and the specific mechanisms of enforcement and benefit distribution. If the primary focus is on exclusion and conformity, this reading is validated.',
    'If the primary function is found to be symbolic preservation or trauma processing without significant boundary enforcement, this reading''s classification as a Tangled Rope would be challenged, potentially shifting to a Rope or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishes the specific functional emphasis of this reading within the catastrophe memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t6, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(cata_tr_t18, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cata_be_t6, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(cata_be_t18, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t6, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(cata_su_t12, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(cata_su_t18, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several distinct readings of the 'catastrophe_memory_kernel,' each focusing on a different structural function of shared mourning practices. This reading emphasizes boundary enforcement and its associated costs and benefits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
