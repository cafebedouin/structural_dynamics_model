% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Mourning Practice Reading of Catastrophe Memory Preservation
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the 'mourning_practice_reading' of the
 *   catastrophe_memory_preservation kernel. The reading holds that ritual's
 *   primary function is preserving symbolic continuity and collective
 *   identity after catastrophic loss, without transferring operational
 *   survival competencies. The ritual operates as voluntary
 *   identity-coordination: participants opt in for cohesion, emotional
 *   processing, and intergenerational connection. Extraction is low (0.12) —
 *   mainly the emotional labor of participation and specialist maintenance
 *   costs. Suppression is near-zero (0.05) — non-participation carries no
 *   formal sanction. Theater ratio is moderate (0.25) — some performative
 *   maintenance of forms whose original operational referent is gone, but the
 *   symbolic function remains genuine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.05).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Mourning Practice Reading of Catastrophe Memory Preservation").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '8b71e0be-6eb9-4a27-b831-4329efb8ea2b').
narrative_ontology:cs_kernel_codification('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', distributed).
narrative_ontology:cs_authority_grounding('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', practice).
narrative_ontology:cs_interpretation_layer_present('8b71e0be-6eb9-4a27-b831-4329efb8ea2b').
narrative_ontology:cs_reading_relation('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', foundational, symbolic_continuity_sufficient_for_identity_preservation).
narrative_ontology:cs_axiom_status(symbolic_continuity_sufficient_for_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', symbolic_continuity_sufficient_for_identity_preservation, deontological).
narrative_ontology:cs_axiom('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', secondary, operational_transfer_not_required_for_legitimate_remembrance).
narrative_ontology:cs_axiom_status(operational_transfer_not_required_for_legitimate_remembrance, holdable).
narrative_ontology:cs_axiom_grounding('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', operational_transfer_not_required_for_legitimate_remembrance, conventional).
narrative_ontology:cs_reference_frame('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', communal_mourning_as_identity_anchor).
narrative_ontology:cs_drift_state('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', contemporary_modernity, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8b71e0be-6eb9-4a27-b831-4329efb8ea2b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, practicing_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, descendant_identity_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_specialists).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, ritual_as_symbolic_continuity).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, identity_preservation_without_operational_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily participate in mourning rituals that mark collective loss and affirm group identity. Gain social cohesion, emotional processing, and intergenerational connection. Can opt out without material penalty; participation is identity-affirming rather than coerced.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, practicing_community_members, beneficiary,
    organized, generational, mobile, local).

% Inherit the ritual framework as cultural heritage. Receive symbolic continuity and collective memory without operational burden. Exit means cultural assimilation or identity shift; constrained by social embeddedness but not enforced.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, descendant_identity_groups, beneficiary,
    moderate, generational, constrained, regional).

% Maintain and transmit the ritual forms (clergy, elders, tradition-bearers). Hold authority over correct performance but derive status and livelihood from the role. Can adapt forms within tradition; exit means vocational change with high identity cost but material alternatives exist.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_specialists, agenda_setter,
    organized, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, ritual_specialists, beneficiary).

% Live adjacent to the practicing community but do not share the catastrophe memory or identity framework. Would object if the ritual claimed universal authority or demanded public accommodation beyond the community. Currently neither burdened nor benefited.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, non_participating_outsiders, excluded,
    moderate, immediate, arbitrage, local).

% Analyze the ritual as a case of symbolic continuity without operational transfer. No stake in the ritual's persistence or transformation; provide comparative and theoretical framing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, memory_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared symbolic framework for processing collective trauma and affirming group identity across generations, without requiring operational threat-response capabilities.
% TRANSFER_FUNCTION: Moves emotional labor, narrative authority, and identity affirmation from ritual specialists and participating elders to the broader community and descendant groups; no material resource transfer.
% ABSENT_VOICES: Descendants who reject the identity framework entirely (assimilated individuals, converts to other traditions) are structurally excluded from the ritual conversation; their objection would be that the ritual preserves a victim-identity they wish to transcend.
% DISAPPEARANCE_RATIONALE: If the mourning ritual vanished, the community would lose its primary structured mechanism for collective grief processing and intergenerational identity transmission. Alternative forms would eventually emerge but the specific symbolic continuity would be broken.
% FOUNDING_PROBLEM: How to preserve collective memory of catastrophic loss when the operational survival skills that originally motivated the ritual are no longer needed?
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic records from practicing communities (e.g., Armenian genocide commemorations, Holocaust remembrance rituals, Indigenous mourning ceremonies) attest the founding problem remains live — the catastrophe memory persists as identity core even when operational threat is gone. Scholars outside the benefiting communities (memory studies, anthropology of ritual) corroborate this reading.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores as rope: genuine coordination (identity preservation, grief processing) with minimal coercion and no victim set. Beneficiaries are the practicing community and descendant identity groups who voluntarily participate. No one is forced to participate; exit is mobile for most, constrained only by identity-embeddedness for descendants. The rising theater ratio over time reflects gradual formalization of forms whose operational referent has faded — but the symbolic function remains live and valued by participants.
 *
 * PERSPECTIVAL GAP:
 *   From the participant seat, the ritual is a gift (coordination without cost). From the specialist seat, it is a vocation with identity-locked maintenance costs. From the outsider seat, it is invisible or irrelevant. The engine computes these divergences from the declared power/exit/scope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Practicing community members and descendant groups are beneficiaries (d near 0.0 — constraint subsidizes their identity cohesion). Ritual specialists are dual-positioned: agenda_setters who also benefit (status, livelihood) but with arbitrage-grade exit. Non-participating outsiders are excluded but not victimized — the ritual makes no claim on them. Memory scholars are analytical observers. The engine will compute per-seat effective extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to preserve catastrophe memory without operational transfer) remains live per corroborating ethnography. No mandatrophy: the ritual continues to solve the problem it was adapted for. The theater ratio rise signals some formal drift but not functional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_vs_survival_competence,
    'Does the mourning_practice_reading foreclose the survival_competence_reading, or do they coexist as different communities'' lived framings of the same ritual?',
    'Ethnographic comparison: if practicing communities explicitly reject operational threat-recognition as the ritual''s purpose, forecloses; if different communities hold different framings simultaneously, coexists_with.',
    'If forecloses, the kernel has a logical fracture — one reading''s core premise contradicts the other''s. If coexists_with, the kernel hosts a stable pluralism of framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_vs_survival_competence, conceptual, 'Logical relationship between mourning_practice and survival_competence framings of the same ritual.').

omega_variable(
    theater_as_atrophy_signal,
    'Does the rising theater_ratio (0.15→0.25) signal genuine functional atrophy toward hybrid_atrophy_reading, or is it the inevitable formalization of any long-lived symbolic practice?',
    'Longitudinal comparison with rituals that retain operational referents (e.g., military drills, emergency response rituals) — if their theater ratios also rise, formalization is generic; if only catastrophe-memory rituals rise, atrophy is specific.',
    'If atrophy-specific, the mourning_practice_reading may be a transient phase toward hybrid_atrophy. If generic formalization, the reading is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_as_atrophy_signal, empirical, 'Whether increasing performative maintenance indicates functional loss or ritual maturation.').

omega_variable(
    committer_frame_kernel_membership,
    'Is the catastrophe_memory_preservation kernel a single persisting commitment with three readings, or are these three distinct constraints sharing only a colloquial label?',
    'Test ε-invariance: if each reading authors a stable ε under its own structural data (this reading: ε≈0.12; survival_competence: likely higher ε with operational maintenance costs; hybrid_atrophy: rising ε over time), they are distinct constraints linked by network.affects_constraints.',
    'Confirms or refutes the kernel/reading committer frame for this constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_membership, conceptual, 'Whether the three declared readings satisfy the ε-invariance principle for kernel decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.03).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.04).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 80, 0.05).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the mourning_practice_reading of the catastrophe_memory_preservation kernel. It decomposes the colloquial 'ritual preserves memory' claim into a structurally precise constraint: symbolic continuity without operational transfer, voluntary participation, rope-type coordination. The survival_competence_reading (operational threat-recognition preservation) and hybrid_atrophy_reading (atrophied survival-competence) are sibling constraints with different ε, different beneficiary/victim structures, and different types. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
