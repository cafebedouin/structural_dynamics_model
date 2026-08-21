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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe Memory Kernel: Boundary Maintenance Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a reading of a 'catastrophe memory kernel'
 *   where ritualized mourning practices primarily function to enforce group
 *   boundaries and maintain in-group cohesion. The ritual, while ostensibly
 *   commemorating a past trauma, actively shapes identity by defining who
 *   belongs and who does not, often at the expense of individual autonomy and
 *   broader inter-group relations. This reading emphasizes the social and
 *   political functions of memory over its purely commemorative or
 *   therapeutic aspects.
 *
 * KEY AGENTS:
 *   - in_group_members: Primary beneficiary (organized/identity_locked) — benefits from cohesion, pays in conformity
 *   - individual_autonomy: Primary victim (powerless/identity_locked) — bears costs of conformity
 *   - out_group_relations: Secondary victim (powerless/trapped) — bears costs of exclusion
 *   - ritual_leaders: Agenda setter (institutional/constrained) — enforces ritual, gains authority
 *   - dissenting_members: Excluded (moderate/constrained) — marginalized for questioning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe Memory Kernel: Boundary Maintenance Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'c04bf363-4836-4480-ad2e-97ca00b54d93').
narrative_ontology:cs_kernel_codification('c04bf363-4836-4480-ad2e-97ca00b54d93', implicit).
narrative_ontology:cs_authority_grounding('c04bf363-4836-4480-ad2e-97ca00b54d93', practice).
narrative_ontology:cs_interpretation_layer_present('c04bf363-4836-4480-ad2e-97ca00b54d93').
narrative_ontology:cs_reading_relation('c04bf363-4836-4480-ad2e-97ca00b54d93', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c04bf363-4836-4480-ad2e-97ca00b54d93', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c04bf363-4836-4480-ad2e-97ca00b54d93', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('c04bf363-4836-4480-ad2e-97ca00b54d93', foundational, group_cohesion_requires_clear_boundaries).
narrative_ontology:cs_axiom_status(group_cohesion_requires_clear_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('c04bf363-4836-4480-ad2e-97ca00b54d93', group_cohesion_requires_clear_boundaries, conventional).
narrative_ontology:cs_axiom('c04bf363-4836-4480-ad2e-97ca00b54d93', foundational, shared_memory_defines_us_vs_them).
narrative_ontology:cs_axiom_status(shared_memory_defines_us_vs_them, holdable).
narrative_ontology:cs_axiom_grounding('c04bf363-4836-4480-ad2e-97ca00b54d93', shared_memory_defines_us_vs_them, conventional).
narrative_ontology:cs_reference_frame('c04bf363-4836-4480-ad2e-97ca00b54d93', post_catastrophe_group_formation).
narrative_ontology:cs_drift_state('c04bf363-4836-4480-ad2e-97ca00b54d93', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c04bf363-4836-4480-ad2e-97ca00b54d93', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from strong group cohesion, shared identity, and a clear sense of belonging derived from participating in the mourning rituals. They experience the ritual as a source of solidarity and meaning, but also bear the costs of conformity and exclusion of outsiders.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members, beneficiary,
    organized, generational, identity_locked, local).

% The individual's freedom to interpret the past, express dissent, or form relationships outside the prescribed group boundaries is constrained by the ritual's demands for conformity and collective memory. This is an abstract 'agent' representing the cost to individuals.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).

% The potential for harmonious relations with external groups is diminished by rituals that emphasize historical grievances and reinforce an 'us vs. them' mentality. This abstract 'agent' represents the cost to inter-group peace and understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).

% Design, organize, and enforce the mourning practices, ensuring adherence to tradition and the correct transmission of memory. They derive authority and status from their role in maintaining group identity and boundaries.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Members who question the ritual's emphasis on exclusion or its interpretation of history face social pressure, ostracization, or accusations of disloyalty. Their voices are often marginalized to maintain group cohesion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, dissenting_members, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and emotional responses to a past catastrophe, ensuring a shared understanding of group identity and historical narrative, thereby reinforcing in-group solidarity.
% TRANSFER_FUNCTION: Transfers social capital and belonging to conforming in-group members, while transferring social exclusion and pressure for conformity to individuals who deviate, and negative sentiment towards out-groups.
% ABSENT_VOICES: Dissenting members who seek more inclusive interpretations of history or reconciliation with out-groups are often silenced or marginalized. Out-group perspectives on the catastrophe are entirely absent from the ritual's narrative.
% DISAPPEARANCE_RATIONALE: If the ritual and its enforcement vanished, the group's boundaries would soften, individual interpretations of the past would diversify, and the collective identity would become more fluid. This would lead to a significant rearrangement of social structures and inter-group dynamics.
% FOUNDING_PROBLEM: The group faced an existential threat or catastrophe that necessitated strong internal cohesion and clear differentiation from external threats for survival.
% FOUNDING_PROBLEM_CORROBORATION: Ritual leaders and many in-group members attest that the threat to group identity and cohesion remains live, citing ongoing external pressures or historical grievances. External observers might corroborate the historical catastrophe but dispute the necessity of current exclusionary practices.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the cost of maintaining strict group boundaries, including the suppression of individual expression and the perpetuation of inter-group animosity. Suppression (0.70) is high due to the social pressure and potential ostracization for non-conformity, making exit or dissent difficult. Theater ratio (0.20) is low because the ritual is genuinely functional in its boundary-maintenance role, even if that function is extractive. The claimed type is Tangled Rope because it offers a genuine coordination function (group cohesion) but with significant asymmetric extraction (from individual autonomy and out-group relations) and requires active enforcement by ritual leaders.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of in-group members, the ritual is a vital source of identity and solidarity, making it a Rope or even a Mountain of shared heritage. From the perspective of individual autonomy or out-group relations, it is a Snare, trapping individuals in prescribed narratives and perpetuating conflict. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group members are beneficiaries (low d) as they gain cohesion and identity. Individual autonomy and out-group relations are victims (high d) as they bear the costs of conformity and exclusion. Ritual leaders are agenda setters, benefiting from the constraint's operation and holding power to enforce it. Dissenting members are excluded, facing high costs for challenging the established order.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the ritual as pure coordination (Rope) by highlighting the active enforcement and asymmetric extraction involved in boundary maintenance. It also avoids mislabeling it as a pure Snare by acknowledging the genuine, if costly, coordination function of group cohesion. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating a shift in the constraint's primary function over time from pure survival to boundary enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_vs_survival_function,
    'Is the primary function of the ritual still to ensure physical survival (survival_competence_reading), or has it shifted to primarily maintaining group boundaries and identity (boundary_maintenance_reading)?',
    'Empirical analysis of contemporary threats vs. internal group dynamics: if external threats are low but internal conformity pressure is high, it supports the boundary maintenance reading.',
    'If primarily survival, extractiveness might be re-evaluated as a necessary cost of coordination. If primarily boundary maintenance, the current extractiveness is more clearly a cost of social control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_vs_survival_function, empirical, 'Distinguishes between the survival-oriented and boundary-maintenance functions of the ritual.').

omega_variable(
    internalized_suppression_degree,
    'To what extent is the measured suppression structural (social pressure, ostracization) versus internalized (individual belief in the necessity of conformity, identity fusion)?',
    'Post-exit suppression trajectory: if individuals who leave the group continue to self-censor or feel guilt, it indicates internalized suppression. Psychological and sociological studies of former members.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them after formal exit. This would amplify the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_degree, empirical, 'Structural vs. internalized suppression mechanism in ritual conformity.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the ''catastrophe_memory_kernel''. What would change if a sibling reading (e.g., symbol_continuity_reading or trauma_encoding_reading) were adopted as the primary interpretation?',
    'Conceptual analysis of the implications of adopting a different primary reading. For example, if ''symbol_continuity_reading'' were primary, the focus would shift from exclusion to inclusive transmission, potentially lowering extractiveness and suppression.',
    'Adopting a different reading would fundamentally alter the constraint''s declared beneficiaries, victims, and metrics, leading to a different classification. For instance, a ''trauma_encoding_reading'' might emphasize therapeutic benefits, shifting the beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel'. Each reading emphasizes a different structural function and has a unique extractiveness profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
