% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Catastrophe Memory Preservation via Mourning Practice
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint is the mourning_practice_reading of the
 *   catastrophe_memory_preservation kernel. It treats post-catastrophe ritual
 *   as a voluntary identity-coordination mechanism that preserves symbolic
 *   continuity and collective self-recognition without operational transfer
 *   of survival competence. Sibling readings include
 *   survival_competence_reading (operational threat-recognition) and
 *   hybrid_atrophy_reading (historical degradation from operational to
 *   symbolic). The authored metrics are deliberately low-extraction and
 *   low-suppression to reflect the opt-in, non-coercive character of symbolic
 *   mourning; the claimed type is rope.
 *
 * KEY AGENTS:
 *   - mourning_community: Primary beneficiary (organized/constrained) â receives identity cohesion
 *   - ritual_stewards: Agenda setter (moderate/identity_locked) â maintains form without extraction
 *   - survival_competence_advocates: Excluded voice (moderate/mobile) â argues for operational content
 *   - memory_scholars: Analytical observer (analytical/analytical) â studies memory transmission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation via Mourning Practice").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'd8d98bbd-9a53-44c4-8b9f-234cbbec5dc4').
narrative_ontology:cs_kernel_codification('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', distributed).
narrative_ontology:cs_authority_grounding('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', practice).
narrative_ontology:cs_interpretation_layer_present('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4').
narrative_ontology:cs_reading_relation('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', foundational, ritual_symbolic_sufficiency).
narrative_ontology:cs_axiom_status(ritual_symbolic_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', ritual_symbolic_sufficiency, conventional).
narrative_ontology:cs_axiom('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', foundational, operational_transfer_unnecessary).
narrative_ontology:cs_axiom_status(operational_transfer_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', operational_transfer_unnecessary, conventional).
narrative_ontology:cs_reference_frame('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', ritual_identity_preservation).
narrative_ontology:cs_drift_state('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', contemporary_secular_modernity, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d8d98bbd-9a53-44c4-8b9f-234cbbec5dc4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, mourning_community).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, symbolic_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, collective_identity_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in periodic mourning rituals, memorial days, and liturgical commemorations that mark catastrophe anniversaries. Participation is voluntary but socially expected within kinship networks; exit is possible but risks estrangement from collective identity. The community receives cohesion, intergenerational recognition, and symbolic continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, mourning_community, beneficiary,
    organized, generational, constrained, regional).

% Maintain the ritual calendar, textual traditions, and ceremonial protocols through which catastrophe memory is transmitted. They derive social status and personal identity from this role but exercise no coercive authority and extract no material tribute; their authority rests on recognized continuity with predecessors.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_stewards, agenda_setter,
    moderate, generational, identity_locked, regional).

% Would argue that catastrophe memory should encode actionable threat-recognition and practical survival training, not only symbolic mourning. They are excluded from ritual planning forums because their operational framing contradicts the community's symbolic self-understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, survival_competence_advocates, excluded,
    moderate, generational, mobile, national).

% Study catastrophe memory systems cross-culturally, observing how ritual sustains identity without operational survival content. They are structurally outside the ritual's normative pull and do not participate in its maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining collective identity and moral continuity after catastrophic rupture, binding dispersed kin and descendants into a recognizable 'we' across generations through shared calendrical observance.
% TRANSFER_FUNCTION: Moves symbolic obligation, emotional attention, and commemorative labor from individual participants into a shared ritual calendar, producing group cohesion and identity recognition as a non-rival, non-extractive public good.
% ABSENT_VOICES: Survival-competence advocates who would demand operational threat-training; secular modernizers who regard the practice as obsolete superstition; descendants who have exited the community and no longer speak in its forums.
% DISAPPEARANCE_RATIONALE: If the mourning practice vanished overnight, the community would lose its primary mechanism for intergenerational identity transmission after catastrophe; commemorative bonds would attenuate, kinship recognition would weaken, and the in-group would cease to experience itself as a continuity.
% FOUNDING_PROBLEM: How does a community maintain its collective identity, moral coherence, and intergenerational recognition after a catastrophic rupture that destroys institutions, territory, or population?
% FOUNDING_PROBLEM_CORROBORATION: Historians and memory studies scholars outside the ritual community attest to the recurring problem of post-catastrophic social disintegration and the documented role of commemorative practice in preventing identity collapse.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.28, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.28) because participation is voluntary and the transfer function produces a non-rival public good (cohesion) rather than material extraction. Suppression is very low (0.12) because there is no coercive enforcement; non-participants are estranged but not punished. Theater ratio is minimal (0.08) because the ritual's symbolic function is genuine and substantially enacted. Accessibility collapse is low (0.20) because secular identity and exit to other communities remain available. Resistance is negligible (0.05) because the arrangement is broadly accepted by participants and only lightly contested by excluded modernizers.
 *
 * PERSPECTIVAL GAP:
 *   The survival-competence reading would compute a higher extractiveness and a tangled_rope or snare classification by insisting that the ritual fails to deliver operational value. From the mourning-community seat, the same ritual is experienced as beneficial coordination with no victim set; the engine computes this divergence from the structural data (voluntary exit, non-material benefit, no enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   The mourning_community sits near the beneficiary end: it receives cohesion and continuity. Ritual_stewards sit near symmetric: they invest labor and identity but receive status and meaning, with no material capture. Survival_competence_advocates are excluded rather than targeted; their exclusion is structural (framing mismatch) rather than coercive. No agent occupies the full-target position because there is no victim set.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy because its founding problemâcollective identity preservation after catastropheâremains live, and the ritual continues to solve it without atrophying into pure performance. If the ritual were maintained after the community had fully dissolved or after its symbolic content had become unintelligible, it would degrade toward piton; here, the biographical and generational time horizons confirm ongoing uptake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_boundary,
    'Does the mourning_practice reading capture a structurally distinct constraint, or does it suppress the survival-competence and hybrid-atrophy interpretations by framing symbolic continuity as the only legitimate function?',
    'Comparative ethnographic analysis of whether the same ritual corpus contains encoded operational instructions (survival reading) or shows evidence of historical functional displacement (hybrid reading).',
    'If operational content is present, this reading''s epsilon is too low and its type may shift toward tangled_rope; if historical displacement is documented, the hybrid reading gains support and this reading becomes a piton of atrophied function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Whether the mourning practice reading is a complete or partial framing of the kernel').

omega_variable(
    identity_coordination_cost,
    'Is the cohesion produced by mourning ritual genuinely non-extractive, or does it impose hidden costs on participants (time, emotional labor, opportunity cost) that constitute low-grade extraction?',
    'Participant cost-benefit ethnography measuring time allocation and subjective well-being against reported identity benefits.',
    'If hidden costs exceed the identity_coordination Boltzmann floor, the constraint is a tangled_rope rather than a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coordination_cost, empirical, 'Whether identity coordination carries disguised extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 80, 0.07).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_preservation kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle: the operational survival-competence claim, the pure symbolic mourning claim, and the historical atrophy claim. Each has a distinct epsilon, beneficiary/victim structure, and coordination/extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
