% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: Ritual as Mourning Practice and Identity Preservation
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures the mourning_practice_reading of the
 *   catastrophe_memory_preservation kernel. The reading holds that ritual's
 *   primary function after the survival era is symbolic: it preserves
 *   collective identity and catastrophic memory continuity through voluntary,
 *   coordinated mourning practice. No operational threat-recognition capacity
 *   is transmitted; the coordination is purely semiotic and
 *   identity-sustaining. Extraction is low-to-moderate because participation
 *   is opt-in and the primary flow is emotional investment returning as
 *   identity cohesion. The claimed type is rope — genuine coordination
 *   without suppression. The sibling readings (survival_competence_reading,
 *   hybrid_atrophy_reading) make different structural claims about the same
 *   ritual complex.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Ritual as Mourning Practice and Identity Preservation").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '9682c09e-6401-4f44-8986-6b2390f46445').
narrative_ontology:cs_kernel_codification('9682c09e-6401-4f44-8986-6b2390f46445', distributed).
narrative_ontology:cs_authority_grounding('9682c09e-6401-4f44-8986-6b2390f46445', practice).
narrative_ontology:cs_interpretation_layer_present('9682c09e-6401-4f44-8986-6b2390f46445').
narrative_ontology:cs_reading_relation('9682c09e-6401-4f44-8986-6b2390f46445', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9682c09e-6401-4f44-8986-6b2390f46445', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('9682c09e-6401-4f44-8986-6b2390f46445', foundational, ritual_function_is_symbolic_not_operational).
narrative_ontology:cs_axiom_status(ritual_function_is_symbolic_not_operational, holdable).
narrative_ontology:cs_axiom_grounding('9682c09e-6401-4f44-8986-6b2390f46445', ritual_function_is_symbolic_not_operational, conventional).
narrative_ontology:cs_axiom('9682c09e-6401-4f44-8986-6b2390f46445', foundational, voluntary_participation_suffices_for_memory_transmission).
narrative_ontology:cs_axiom_status(voluntary_participation_suffices_for_memory_transmission, holdable).
narrative_ontology:cs_axiom_grounding('9682c09e-6401-4f44-8986-6b2390f46445', voluntary_participation_suffices_for_memory_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('9682c09e-6401-4f44-8986-6b2390f46445', symbolic_continuity_frame).
narrative_ontology:cs_drift_state('9682c09e-6401-4f44-8986-6b2390f46445', contemporary_digital_memorial_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9682c09e-6401-4f44-8986-6b2390f46445', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, participating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_elders).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, ritual_preserves_symbolic_continuity).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, collective_identity_through_mourning_practice).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, voluntary_participation_suffices_for_cohesion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the descendant community who voluntarily engage in the mourning ritual. They experience the ritual as a source of collective identity, emotional solidarity, and symbolic connection to the catastrophe memory. Participation is opt-in; non-participation carries no formal sanction but may weaken felt belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, participating_community, beneficiary,
    organized, generational, mobile, regional).

% Custodians of the ritual form who transmit the practice, authorize variations, and maintain the symbolic vocabulary. They benefit from the authority and cohesion the ritual generates for their leadership role. Exit would mean relinquishing a structural position in the community's memory architecture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_elders, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, ritual_elders, beneficiary).

% Descendants of the affected population who do not identify with the collective catastrophe narrative or reject the ritual's framing. They are structurally excluded from the ritual's coordination benefits because the ritual's symbolic vocabulary presupposes a shared catastrophe identity they do not inhabit. Their objection would be to the identity claim itself, not the ritual mechanics.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, non_identifying_descendants, excluded,
    moderate, biographical, mobile, regional).

% Scholars of ritual, collective memory, and religious studies who analyze the practice from outside. They document the coordination function, track historical variation, and assess claims about identity maintenance. They neither collect nor pay the ritual's costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, anthropological_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared symbolic framework for catastrophe memory that sustains collective identity across generations without requiring operational survival skills. The ritual solves the coordination problem of how a dispersed community agrees on what the catastrophe means and who 'we' are in relation to it.
% TRANSFER_FUNCTION: Moves emotional labor, narrative authority, and identity affirmation from individual participants to the collective symbolic reservoir. Participants invest grief, reverence, and time; the collective receives a maintained identity boundary and a transmitted memory template.
% ABSENT_VOICES: Non-identifying descendants and those who experienced the catastrophe differently but are not part of the practicing community. They would object to the ritual's claim to speak for all descendants, but they are not in the room because the ritual's participation condition is self-identification with the collective narrative.
% DISAPPEARANCE_RATIONALE: If the mourning ritual vanished overnight, the participating community would lose its primary scheduled occasion for collective catastrophe remembrance. Identity cohesion would degrade; alternative memory practices (family storytelling, individual commemoration) would not immediately replicate the ritual's synchronization function. The collective identity would not disappear but would reorganize around weaker, less coordinated memory practices.
% FOUNDING_PROBLEM: After the catastrophe's immediate survival phase ended, the community faced a coordination problem: how to transmit the catastrophe's meaning and maintain group cohesion when the operational skills that once made the memory urgent (threat recognition, survival techniques) were no longer needed for daily survival.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological literature on post-catastrophe ritual (e.g., studies of Holocaust remembrance rituals, Armenian genocide commemoration, Indigenous boarding school memorial practices) documents the transition from survival-competence transmission to symbolic identity maintenance. Participating community elders corroborate the founding problem as live; non-identifying descendants contest whether the problem ever required this specific ritual form.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.28) reflects the emotional and time costs of ritual participation, which are real but voluntarily borne and reciprocated in identity goods. Suppression (0.12) is minimal — non-participation is socially visible but unsanctioned; the constraint persists because participants value it, not because alternatives are blocked. Theater ratio (0.35) captures the growing performative dimension: as the ritual ages, a larger share of its activity serves display and boundary-marking rather than the core coordination of memory transmission. The measurement series show gradual accumulation of both extractiveness and theater over 80 years, consistent with ritual elaboration without coercive turn.
 *
 * PERSPECTIVAL GAP:
 *   From the participating community seat, the constraint is experienced as a rope — voluntary coordination that solves a real identity-maintenance problem. From the non-identifying descendants seat, the same structure may appear as a subtle snare — the community's collective memory claim marginalizes alternative catastrophe narratives. The engine computes this divergence from the structural data; the authored claim (rope) reflects the dominant coordination reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Participating community and ritual elders sit at the beneficiary end (d ~ 0.15-0.25): they receive identity cohesion and authority respectively, with mobile/constrained exit. Non-identifying descendants are excluded rather than targeted — they are not forced into the ritual, but the ritual's symbolic vocabulary makes their non-participation a form of identity dissent. Anthropological observers are analytical (d = 0.5). No victim set exists because the constraint's coordination function does not require extraction from a designated payer group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-survival identity cohesion) remains live — the community still needs to coordinate what the catastrophe means and who belongs to its memory. The ritual has not atrophied into pure performance; its coordination function is actively used. However, the rising theater ratio signals a mandatrophy risk: if identity cohesion becomes fully decoupled from the ritual's specific form (e.g., replaced by digital memorials), the constraint could drift toward piton. Current status: rope with early mandatrophy indicators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the mourning_practice_reading instantiate a genuinely distinct constraint from its sibling readings, or do they describe the same ritual at different temporal phases?',
    'Compare the structural metrics (extractiveness, suppression, victim sets) across the three readings. If they diverge systematically, they are distinct constraints linked by kernel_id. If they converge, the kernel may be a single constraint with observer-dependent classification.',
    'If distinct, each reading gets its own ε and classification; the kernel_id becomes a family linkage via network.affects_constraints. If not distinct, the kernel should be a single story with perspectival seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints or one constraint with multiple observational frames.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression (0.12) structural (genuinely voluntary participation) or partially internalized (community members feel they cannot opt out without identity loss)?',
    'Post-exit suppression trajectory: track individuals who cease participation. If they report persistent identity anxiety or community pressure, suppression has an internalized component not captured by formal sanctions.',
    'If internalized, effective suppression is higher than the structural measure; the constraint may classify as tangled_rope from the participating community seat despite low formal suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in voluntary identity rituals.').

omega_variable(
    coordination_extraction_boundary,
    'Is the ritual''s coordination function (identity maintenance) genuinely separable from extractive dynamics (elder authority, boundary enforcement against non-identifying descendants)?',
    'Natural experiment: compare communities with similar catastrophe histories but different ritual forms. If identity cohesion persists without this specific ritual, the coordination function is separable and the ritual''s specific form may carry extraction.',
    'If inseparable, the ritual''s measured low extractiveness is the genuine price of coordination. If separable, the specific ritual form may be a tangled_rope where coordination is cover for elder authority extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the mourning ritual''s coordination and potential extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(cata_tr_t48, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 48, 0.31).
narrative_ontology:measurement(cata_tr_t64, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 64, 0.33).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 32, 0.22).
narrative_ontology:measurement(cata_be_t48, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 48, 0.25).
narrative_ontology:measurement(cata_be_t64, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 64, 0.27).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 80, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 16, 0.09).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 32, 0.1).
narrative_ontology:measurement(cata_su_t48, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 48, 0.11).
narrative_ontology:measurement(cata_su_t64, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 64, 0.11).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 80, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the mourning_practice_reading of the catastrophe_memory_preservation kernel. The sibling readings are survival_competence_reading (claims ritual transmits operational threat-recognition, higher extractiveness, powerless victims) and hybrid_atrophy_reading (claims ritual transitioned from survival-competence to mourning-practice, temporal drift from rope to piton). All three share the kernel_id but instantiate different ε values and stakeholder structures. This reading asserts rope with no victims; the others assert tangled_rope or piton with victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
