% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Shared Mourning-Practice as Group Boundary Enforcement
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures the boundary_maintenance_reading of the
 *   catastrophe_memory_kernel: the shared mourning-practice functions as a
 *   boundary-enforcement mechanism. The ritual prescribes specific forms of
 *   grief — liturgy, posture, timing, attire, public performance — that
 *   authenticate membership. Those who perform correctly are inside; those
 *   who cannot or will not are outside. The arrangement coordinates group
 *   cohesion (genuine coordination function) but extracts conformity and
 *   excludes dissenters (asymmetric extraction). The extractiveness has risen
 *   over the interval as the founding survival problem receded and
 *   boundary-maintenance became the dominant function. Suppression has
 *   increased as enforcement shifted from social pressure to formal sanction.
 *   Theater has grown as the ritual's performative aspects have elaborated
 *   beyond its coordination core.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Shared Mourning-Practice as Group Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '1c02be16-4c2d-4155-9435-2543545f93fc').
narrative_ontology:cs_kernel_codification('1c02be16-4c2d-4155-9435-2543545f93fc', implicit).
narrative_ontology:cs_authority_grounding('1c02be16-4c2d-4155-9435-2543545f93fc', practice).
narrative_ontology:cs_interpretation_layer_present('1c02be16-4c2d-4155-9435-2543545f93fc').
narrative_ontology:cs_reading_relation('1c02be16-4c2d-4155-9435-2543545f93fc', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c02be16-4c2d-4155-9435-2543545f93fc', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c02be16-4c2d-4155-9435-2543545f93fc', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('1c02be16-4c2d-4155-9435-2543545f93fc', foundational, boundary_maintenance_authenticates_belonging).
narrative_ontology:cs_axiom_status(boundary_maintenance_authenticates_belonging, holdable).
narrative_ontology:cs_axiom_grounding('1c02be16-4c2d-4155-9435-2543545f93fc', boundary_maintenance_authenticates_belonging, conventional).
narrative_ontology:cs_axiom('1c02be16-4c2d-4155-9435-2543545f93fc', secondary, prescribed_mourning_is_membership_performance).
narrative_ontology:cs_axiom_status(prescribed_mourning_is_membership_performance, holdable).
narrative_ontology:cs_axiom_grounding('1c02be16-4c2d-4155-9435-2543545f93fc', prescribed_mourning_is_membership_performance, conventional).
narrative_ontology:cs_reference_frame('1c02be16-4c2d-4155-9435-2543545f93fc', founding_survival_anchoring).
narrative_ontology:cs_drift_state('1c02be16-4c2d-4155-9435-2543545f93fc', contemporary_institutionalized_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c02be16-4c2d-4155-9435-2543545f93fc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, cohesion_maintainers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_specialists).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, dissenting_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, boundary_crossers).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, non_conformists).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, collective_identity_requires_boundary).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, mourning_authenticates_belonging).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Community leaders and elders who derive authority from curating the mourning ritual. They set the terms of participation, define correct performance, and exclude those who deviate. Their cohesion benefit is real — the ritual knits the group — but it comes at the cost of policing conformity. Exit means losing the legitimacy that comes from guarding the boundary.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, cohesion_maintainers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, cohesion_maintainers, agenda_setter).

% Officiants, cantors, and ritual technicians whose professional standing and livelihood depend on the mourning-practice's centrality. They benefit from the constraint's enforcement — more elaborate prescriptions mean more specialized labor — but their exit is constrained by identity investment and community expectation. They are not the agenda-setters but they maintain the machinery.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_specialists, beneficiary,
    moderate, biographical, constrained, local).

% Members who experience the mourning-practice as coercive — prescribed grief scripts, mandatory attendance, public performance of prescribed affect. They pay through emotional labor, conformity pressure, and the cost of masking dissent. Exit is identity-locked: leaving the ritual means leaving the community, and the community constitutes their social world. They stay and perform.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, dissenting_members, payer,
    moderate, biographical, identity_locked, local).

% Those who marry out, convert, or otherwise cross the group boundary. The mourning-practice marks them as outsiders — they cannot participate authentically, and their exclusion is the boundary's proof. They bear the cost of visible otherness. Exit from the constraint is not an option; the constraint defines them by their exclusion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, boundary_crossers, payer,
    powerless, immediate, trapped, local).

% Members who remain but refuse the prescribed form — they mourn privately, differently, or not at all. They are tolerated only if invisible; visibility brings sanction. They pay through surveillance and the tax of concealment. Their exit is constrained: open dissent means expulsion, concealment means internal fracture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, non_conformists, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, non_conformists, excluded).

% Researchers who study the ritual across communities. They see the structural pattern — boundary-maintenance through prescribed mourning — without being subject to it. Their analytical seat is not neutral; their framing choices (emphasizing cohesion vs. coercion) feed back into community self-understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, comparative_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining a distinct collective identity across generations in the face of assimilation pressure, dispersal, and internal fragmentation. The shared mourning-practice creates a recurrent, embodied occasion where membership is performed, verified, and renewed — a coordination mechanism for 'who belongs' that does not require constant negotiation.
% TRANSFER_FUNCTION: Transfers individual expressive autonomy (how, when, whether to mourn) to the collective prescription. The cost is borne by dissenting_members, boundary_crossers, and non_conformists in the form of emotional labor, conformity pressure, and exclusion. The gain accrues to cohesion_maintainers and ritual_specialists as authority, legitimacy, and structural centrality.
% ABSENT_VOICES: The dead themselves — the putative subjects of the mourning — are structurally absent. Their preferences for how they would be remembered, or whether the living should be bound by prescribed grief, are never consulted. Also absent: assimilated descendants who left entirely and thus have no seat in the current arrangement; their silence is read as validation.
% DISAPPEARANCE_RATIONALE: If the prescribed mourning-practice vanished overnight, the community's primary mechanism for boundary-maintenance would collapse. Cohesion_maintainers would lose their central ritual authority. Dissenting_members and non_conformists would gain expressive freedom but lose the shared reference point that makes their dissent legible. Boundary_crossers would lose the clear marker of their exclusion. The group would either improvise a new boundary-ritual (likely more diffuse, less enforceable) or gradually lose distinctiveness.
% FOUNDING_PROBLEM: After the catastrophe, the surviving community faced dissolution through assimilation, dispersal, and the erosion of transmission. The founding generation instituted the mourning-practice as a portable, repeatable anchor — a way to enact 'we are the people who remember this' without requiring territory, sovereignty, or continuous leadership.
% FOUNDING_PROBLEM_CORROBORATION: Survivor testimonies and early communal records (outside the current beneficiary lineage) attest the founding problem was existential survival of the group as a distinct entity. Current cohesion_maintainers attest the problem remains live (assimilation continues). Comparative scholars and assimilated descendants attest the founding problem is substantially solved — the group persists — and the practice now serves primarily boundary-enforcement rather than survival. No single corroborating voice is accepted by all parties.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).
:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects moderate extraction: the constraint transfers expressive autonomy from individuals to the collective prescription, but the coordination benefit (distinct identity maintenance) is real and valued by many participants. Suppression (0.38) is moderate: enforcement operates through social sanction, exclusion from communal goods, and identity threat rather than physical coercion. Theater ratio (0.22) is low-moderate: the ritual's core coordination function (boundary-maintenance) remains central, but elaborated prescriptions and policing of minor deviations indicate growing performative overhead. Accessibility collapse (0.45) is moderate: alternative mourning forms exist but are socially illegible within the group; leaving the constraint means leaving the group. Resistance (0.35) is moderate: dissent exists but is mostly concealed; open challenges are rare and sanctioned.
 *
 * PERSPECTIVAL GAP:
 *   The cohesion_maintainer seat experiences the constraint as genuine coordination — the ritual solves the real problem of maintaining distinctiveness. The dissenting_member seat experiences it as enforced extraction — the same ritual extracts their expressive freedom. The boundary_crosser seat experiences it as pure exclusion — the ritual's function is to mark them as other. The engine computes these divergent per-seat classifications from the structural data; this commentary documents the structural asymmetry that drives the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Cohesion_maintainers and ritual_specialists are structural beneficiaries (d near 0.0-0.2): they collect authority, legitimacy, and centrality from the constraint's operation. Their exit is constrained — they are invested in the system they maintain. Dissenting_members are identity-locked payers (d near 0.7-0.8): the constraint extracts emotional labor and conformity; their exit would mean losing the community that constitutes their social identity. Boundary_crossers are trapped (d near 1.0): the constraint defines them by exclusion; they cannot exit the constraint's definition of them. Non_conformists are constrained payers with partial exclusion (d near 0.6): they pay through concealment tax and surveillance; exit is possible but costly. Comparative_scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (survival of the group as a distinct entity) is contested: beneficiaries claim it remains live; scholars and assimilated descendants claim it is substantially solved. The constraint persists with rising extractiveness and suppression after the founding problem's acuity has diminished — a classic mandatrophy pattern. However, the coordination function (boundary-maintenance) remains structurally necessary for the group's continued distinctiveness; the constraint is not purely inertial. The classification as tangled_rope (not piton) reflects that the coordination function is still live and the extraction is not purely performative — the boundary is actively maintained because the group still needs a boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_framing,
    'This constraint is one reading (boundary_maintenance_reading) of the contested catastrophe_memory_kernel. How does the kernel''s multi-reading structure affect the classification of this reading?',
    'Analyze sibling readings'' structural profiles. If sibling readings classify differently (e.g., survival_competence_reading as rope, trauma_encoding_reading as snare), the kernel itself hosts a constraint family with divergent extraction profiles. The boundary_maintenance_reading''s classification is then one node in a family, not a standalone verdict.',
    'If the kernel is a constraint family, contamination and coupling analysis must cross reading boundaries. A classification shift in one reading (e.g., survival_competence_reading drifting toward snare under persecution pressure) could propagate to this reading via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Whether the kernel''s multi-reading structure requires family-level analysis rather than single-constraint analysis.').

omega_variable(
    boundary_maintenance_vs_survival_competence,
    'Does the boundary_maintenance_reading foreclose the survival_competence_reading, or do they coexist as complementary functions of the same ritual?',
    'Examine whether communities that center boundary-maintenance in their self-understanding can simultaneously hold that the ritual encodes survival competence. If the ritual''s prescriptions serve both functions without contradiction, they coexist. If boundary-maintenance requires suppressing the survival-competence narrative (e.g., ''we mourn to remember who we are'' vs ''we mourn to remember how to survive''), they may be in tension.',
    'If forecloses: the two readings cannot be held in a single framework; communities must choose. If coexists_with: the ritual is multi-functional and the readings are analytical lenses on the same structure. If influences: boundary-maintenance emphasis may crowd out survival-competence transmission over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_vs_survival_competence, conceptual, 'Structural relationship between boundary_maintenance_reading and survival_competence_reading.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression experienced by dissenting_members and non_conformists primarily structural (external sanction) or internalized (self-policing from identity fusion)?',
    'Post-exit trajectory study: track individuals who leave the community. If suppression symptoms (guilt, anxiety, self-censorship around mourning) persist after structural sanctions are removed, the suppression has an internalized component. If symptoms resolve quickly, suppression was primarily structural.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure (0.38) suggests — the target carries the suppression with them. This would push the constraint toward snare classification for the identity_locked seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Whether the constraint''s suppression operates through external enforcement or internalized identity fusion.').

omega_variable(
    ritual_elaboration_as_extraction,
    'Does the increasing theater_ratio (0.08 to 0.22) reflect genuine coordination elaboration (more precise boundary-maintenance) or extraction elaboration (more opportunities for specialists to capture value)?',
    'Compare ritual prescription complexity over time against boundary-maintenance efficacy. If added prescriptions improve boundary-clarity (fewer ambiguous cases, clearer in/out), it''s coordination. If added prescriptions multiply specialist roles without improving boundary-clarity, it''s extraction.',
    'If extraction elaboration, the constraint is drifting toward snare. If coordination elaboration, the rising theater is the cost of maintaining a more precise boundary in a more complex social environment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_elaboration_as_extraction, empirical, 'Whether ritual elaboration serves coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 70, 0.22).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 80, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 70, 0.42).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 80, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.33).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 70, 0.38).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_memory_kernel. The boundary_maintenance_reading emphasizes the ritual's boundary-enforcement function (D3 tangled_rope). The survival_competence_reading emphasizes adaptive transmission (likely rope or tangled_rope). The symbol_continuity_reading emphasizes identity preservation (likely rope). The trauma_encoding_reading emphasizes warning-function (likely tangled_rope or snare). All four readings share the same ritual substrate but author different ε, beneficiaries, victims, and claimed types. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, moderate, 0.75).
constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
