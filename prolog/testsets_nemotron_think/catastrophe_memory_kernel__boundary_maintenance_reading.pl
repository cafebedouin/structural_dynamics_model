% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Catastrophe Memory Ritual as Boundary Enforcement
 *   domain: religious/collective_memory/ritual
 *
 * SUMMARY:
 *   A community preserves its distinct identity through a prescribed mourning
 *   ritual commemorating a historical catastrophe. The ritual is presented as
 *   sacred remembrance; its boundary-maintenance function is acknowledged
 *   internally but framed as a sacred duty rather than a social technology.
 *   The constraint operates through identity-locked participation — members
 *   cannot opt out without rupturing their belonging — and through the
 *   exclusion of out-group neighbors from the moral community the ritual
 *   constitutes. Extraction is moderate: the ritual genuinely coordinates
 *   collective identity (rope function) but does so by enforcing conformity
 *   on private grief and marking outsiders as Other (snare function).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe Memory Ritual as Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious/collective_memory/ritual").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'd8e0dd42-7579-496d-b6fb-e9be25320270').
narrative_ontology:cs_kernel_codification('d8e0dd42-7579-496d-b6fb-e9be25320270', fixed_text).
narrative_ontology:cs_authority_grounding('d8e0dd42-7579-496d-b6fb-e9be25320270', lineage).
narrative_ontology:cs_interpretation_layer_present('d8e0dd42-7579-496d-b6fb-e9be25320270').
narrative_ontology:cs_reading_relation('d8e0dd42-7579-496d-b6fb-e9be25320270', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8e0dd42-7579-496d-b6fb-e9be25320270', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8e0dd42-7579-496d-b6fb-e9be25320270', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('d8e0dd42-7579-496d-b6fb-e9be25320270', foundational, boundary_maintenance_is_primary_function).
narrative_ontology:cs_axiom_status(boundary_maintenance_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('d8e0dd42-7579-496d-b6fb-e9be25320270', boundary_maintenance_is_primary_function, instrumental).
narrative_ontology:cs_axiom('d8e0dd42-7579-496d-b6fb-e9be25320270', foundational, prescribed_grief_authenticates_membership).
narrative_ontology:cs_axiom_status(prescribed_grief_authenticates_membership, holdable).
narrative_ontology:cs_axiom_grounding('d8e0dd42-7579-496d-b6fb-e9be25320270', prescribed_grief_authenticates_membership, conventional).
narrative_ontology:cs_reference_frame('d8e0dd42-7579-496d-b6fb-e9be25320270', founding_catastrophe_commemoration).
narrative_ontology:cs_drift_state('d8e0dd42-7579-496d-b6fb-e9be25320270', contemporary_institutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8e0dd42-7579-496d-b6fb-e9be25320270', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_dissidents).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, collective_identity_requires_boundary_ritual).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, shared_mourning_creates_usable_past).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, transmit, and enforce the mourning ritual calendar and its prescribed forms. Their authority derives from custodianship of the catastrophe memory. They benefit from the ritual's boundary-maintenance function, which secures their interpretive monopoly and institutional relevance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_authorities, beneficiary).

% Participate in prescribed mourning rites at personal cost (time, emotional labor, conformity). They receive the solidarity and identity-anchoring the ritual provides, but their autonomy is bounded by the requirement to perform grief in the authorized mode. Exit means identity rupture — leaving the ritual is leaving the group.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members, payer,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members, beneficiary).

% Experience the ritual as coerced performance; their private grief does not match the prescribed form but they cannot publicly deviate without social sanction. They bear the full extractive weight (conformity pressure, exclusion threat) without the solidarity benefit. Exit is socially lethal within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_dissidents, payer,
    powerless, immediate, trapped, local).

% Are structurally positioned as the boundary's other — the ritual's enactment marks them as outside the moral community. They have no voice in the ritual's form or timing, and their presence at commemorations is tolerated only as witnesses to the group's distinctness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors, excluded,
    moderate, biographical, mobile, regional).

% Study the ritual as a boundary-maintenance mechanism across multiple communities. They see the structural pattern — how prescribed mourning creates us/them — but hold no stake in any particular instantiation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, anthropologists_of_ritual, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates and maintains a permeable but defensible group boundary through synchronized emotional performance — the shared mourning ritual makes the group visible to itself and others, synchronizes commitment, and provides a recurring signal of membership.
% TRANSFER_FUNCTION: Transfers individual expressive autonomy (how to grieve, when, in what form) to the collective, converting it into boundary capital — the group's ability to recognize its own and exclude others. The ritual calendar extracts compliance; the group receives cohesion.
% ABSENT_VOICES: Those who left the community because the ritual's prescribed grief did not match their experience — their absence is the enforcement mechanism's proof. Also the dead themselves, whose catastrophe is the ritual's raw material but who cannot consent to its instrumentalization.
% DISAPPEARANCE_RATIONALE: If the boundary-maintenance ritual vanished overnight, the group's primary mechanism for signaling distinctness and synchronizing commitment would fail. Within a generation, intermarriage rates would rise, distinctive practices would attenuate, and the group would likely assimilate or fracture into subgroups with competing memory practices.
% FOUNDING_PROBLEM: After the catastrophe, the community faced dissolution through dispersion, conversion, and loss of distinctive practice. The ritual was instituted to anchor a shared past that could survive without territory or sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Communal chronicles and founding charters attest the ritual was instituted explicitly for survival-as-distinct-group. Anthropological literature (e.g., Assmann on cultural memory, Smith on ethnic boundaries) corroborates the boundary-maintenance function from outside the benefiting tradition. The ritual authorities maintain the founding problem is still live; dissident members and comparative scholars argue the existential threat has passed and the ritual now serves institutional self-preservation.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects that the ritual's coordination function is real — the group would likely dissolve without it — but the cost falls asymmetrically on individual expressive autonomy and out-group relations. Suppression (0.52) is structural: the ritual calendar is mandatory, deviation is sanctioned, and the boundary it maintains is actively policed. Theater ratio (0.28) is low-moderate: the sacred framing is not mere performance; participants genuinely experience the ritual as meaningful, but the institutional layer has thickened over time (authorized texts, trained officiants, penalties for innovation). The measurement series shows extraction and theater rising as the existential threat receded, while suppression stabilized — consistent with a coordination mechanism acquiring extractive overlay.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual authority's seat, this is a rope — genuine coordination solving the problem of group survival. From the dissident's seat, it is a snare — enforced performance with no exit. From the in-group member's seat, it is a tangled rope — real solidarity purchased at the cost of autonomy. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the analytical observer's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual authorities sit near the beneficiary end (d ~ 0.15): they control the form, collect institutional legitimacy, and face minimal exit pressure. In-group members are near-symmetric (d ~ 0.45): they pay conformity costs but receive identity anchorage. Individual dissidents are full targets (d ~ 0.85): identity-locked, trapped, bearing costs without benefits. Out-group neighbors are excluded (no d computed — they are not governed by the constraint but constituted by its boundary). The analytical observer is at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (group survival) is contested — the ritual authorities claim it remains live; comparative evidence suggests the existential threat has diminished. The ritual persists with rising theater, suggesting mandatrophy drift: the coordination function has atrophied relative to the extraction, but the constraint is maintained because the authorities benefit and the identity-locked members cannot coordinate exit. This is not a piton (no theatrical maintenance of a dead function — the coordination is still real) but a tangled rope trending toward snare if the threat continues to recede.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the catastrophe_memory_kernel instantiate one constraint with multiple valid readings, or are these four structurally distinct constraints sharing a label?',
    'Apply ε-invariance test: if measuring the constraint via boundary-maintenance observables yields a different ε than via symbol-continuity observables, they are distinct constraints. Compare the extractiveness profiles of each reading''s operationalization.',
    'If distinct constraints, each gets its own story and classification; if one constraint, the readings are perspectival slices and the engine''s per-seat computation captures the divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel label covers one constraint or a constraint family.').

omega_variable(
    boundary_maintenance_vs_symbol_continuity,
    'Is the boundary-maintenance function (this reading) the primary driver of the ritual''s persistence, or is it a downstream effect of the symbol-continuity function (sibling reading)?',
    'Historical analysis of ritual development: did boundary-marking elements appear early (founding) or accrete later? Compare communities that retain the catastrophe memory but dropped boundary strictness — do they maintain the ritual?',
    'If boundary-maintenance is primary, this reading''s ε is the kernel''s ε; if secondary, the sibling reading''s ε governs and this reading''s extraction is overlay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_maintenance_vs_symbol_continuity, empirical, 'Causal priority between sibling readings'' proposed functions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social sanctions, institutional penalties) or internalized (members genuinely believe the prescribed form is the only authentic grief)?',
    'Post-exit suppression trajectory: track dissidents who leave — if prescribed grief performance persists after exit, reclassify as partially internalized. Interview studies comparing current members'' and leavers'' emotional experience of the ritual.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the agent. This would increase extraction for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in identity-locked ritual participation.').

omega_variable(
    reading_relations_structure,
    'Do the sibling readings genuinely coexist, or does boundary_maintenance_reading foreclose trauma_encoding_reading by instrumentalizing the trauma?',
    'Analyze whether a community can simultaneously hold ''the ritual warns against future catastrophe'' (trauma encoding) and ''the ritual marks our boundary'' (boundary maintenance) without contradiction. If the warning function requires universal applicability (trauma as human lesson) while boundary maintenance requires particularism (trauma as our unique marker), they may foreclose.',
    'If forecloses, the readings cannot be held in one framework — the kernel would split into competing traditions. If coexists_with, both remain live interpretive options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relations_structure, conceptual, 'Logical compatibility between boundary_maintenance_reading and trauma_encoding_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmk_bmr_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cmk_bmr_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cmk_bmr_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(cmk_bmr_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(cmk_bmr_tr_t80, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(cmk_bmr_tr_t100, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 100, 0.27).
narrative_ontology:measurement(cmk_bmr_tr_t120, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 120, 0.28).

% Extraction over time
narrative_ontology:measurement(cmk_bmr_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(cmk_bmr_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(cmk_bmr_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(cmk_bmr_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(cmk_bmr_be_t80, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(cmk_bmr_be_t100, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 100, 0.45).
narrative_ontology:measurement(cmk_bmr_be_t120, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 120, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cmk_bmr_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cmk_bmr_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(cmk_bmr_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(cmk_bmr_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement(cmk_bmr_su_t80, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement(cmk_bmr_su_t100, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(cmk_bmr_su_t120, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 120, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four constraint stories linked by affects_constraints. boundary_maintenance_reading (this story) has moderate extraction (0.45) because boundary enforcement requires conformity. symbol_continuity_reading likely has lower extraction (identity preservation without active exclusion). survival_competence_reading may have higher extraction (persecution-survival training demands rigorous compliance). trauma_encoding_reading's extraction depends on whether the warning function is universalized or particularized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, organized, 0.45).
constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, powerless, 0.85).
constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
