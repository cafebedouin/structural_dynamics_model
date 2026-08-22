% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Catastrophe Memory Preservation Ritual — Hybrid Atrophy Reading
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This is the hybrid-atrophy reading of the catastrophe-memory-preservation
 *   kernel: ritual once encoded survival-competent threat-recognition and
 *   collective-response protocols but has atrophied under modernity to
 *   function primarily as mourning-practice and identity-maintenance theater.
 *   The constraint persists not because participants extract adaptive payoff
 *   but because the practice has fused with in-group identity; the present
 *   generation inherits both the ritual form and the obligation to maintain
 *   it, but without the functional payoff their ancestors received. This
 *   reading claims the constraint is a piton — mostly performance — because
 *   the founding adaptive function has been replaced by specialized
 *   institutions, but the practice persists due to inertia and identity
 *   anchoring rather than active rational choice by a concentrated
 *   beneficiary. Extractiveness is moderate and declining because the cost to
 *   present-generation practitioners is real (time, emotional labor, identity
 *   fusion) but diffuse and not clearly captured by any agent; theater_ratio
 *   rises as the constraint becomes increasingly about symbolic continuity
 *   rather than functional threat-recognition. Suppression remains low
 *   because the practice is maintained through identity internalization and
 *   social obligation rather than external coercion.
 *
 * KEY AGENTS:
 *   - Ritual custodians: organize and transmit the practice; incur administrative cost; cannot exit without severing identity as keeper of collective memory.
 *   - Present-generation practitioners: inherit and enact the ritual; pay time/attention/emotional labor; receive diminished adaptive payoff but face identity fusion and social cost of defection.
 *   - Historical survivors (non-agent reference): the adaptive threat-recognition competence the ritual originally preserved; now historical, no longer active.
 *   - Institutional threat detection (observer): modern systems (emergency management, epidemiology, security) now handle the operational functions the ritual originally encoded.
 *   - External skeptics (excluded): would argue for explicit reform or dissolution; treated as lacking standing on identity/memory matters; excluded from continuity decisions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation Ritual — Hybrid Atrophy Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e31926d5-62cc-4cf6-a8ec-38d076b05401').
narrative_ontology:cs_kernel_codification('e31926d5-62cc-4cf6-a8ec-38d076b05401', distributed).
narrative_ontology:cs_authority_grounding('e31926d5-62cc-4cf6-a8ec-38d076b05401', lineage).
narrative_ontology:cs_interpretation_layer_present('e31926d5-62cc-4cf6-a8ec-38d076b05401').
narrative_ontology:cs_reading_relation('e31926d5-62cc-4cf6-a8ec-38d076b05401', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('e31926d5-62cc-4cf6-a8ec-38d076b05401', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('e31926d5-62cc-4cf6-a8ec-38d076b05401', foundational, ritual_adaptive_function_historically_real_but_attenuated).
narrative_ontology:cs_axiom_status(ritual_adaptive_function_historically_real_but_attenuated, holdable).
narrative_ontology:cs_axiom_grounding('e31926d5-62cc-4cf6-a8ec-38d076b05401', ritual_adaptive_function_historically_real_but_attenuated, empirically_contingent).
narrative_ontology:cs_axiom('e31926d5-62cc-4cf6-a8ec-38d076b05401', foundational, identity_fusion_sustains_practice_absent_adaptive_payoff).
narrative_ontology:cs_axiom_status(identity_fusion_sustains_practice_absent_adaptive_payoff, holdable).
narrative_ontology:cs_axiom_grounding('e31926d5-62cc-4cf6-a8ec-38d076b05401', identity_fusion_sustains_practice_absent_adaptive_payoff, deontological).
narrative_ontology:cs_reference_frame('e31926d5-62cc-4cf6-a8ec-38d076b05401', post_catastrophe_adaptive_threat_recognition).
narrative_ontology:cs_drift_state('e31926d5-62cc-4cf6-a8ec-38d076b05401', contemporary_institutional_replacement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e31926d5-62cc-4cf6-a8ec-38d076b05401', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintenance).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the ritual practice, maintain its forms, transmit it to the next generation. Interpret its meaning as preserving both historical threat-recognition and collective identity continuity. Incur administrative cost (time, resources, coordination overhead) to sustain the practice. Cannot exit without severing identity as keeper of collective memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_custodians, agenda_setter,
    organized, generational, identity_locked, local).

% Inherit obligation to participate in ritual; invest time, attention, emotional labor. No longer receive adaptive threat-recognition payoff (modern institutions handle threat detection). Continue because inherited identity fusion with the practice and social cost of defection. Carry the constraint forward despite erosion of its original function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Non-actor entity: the collective identity anchored to the practice. The constraint vindicates the proposition that ritual authority grounds legitimacy in tradition and continuity. Not a person or organization; the actual beneficiary seat is ritual custodians who maintain professional/identity role through administering it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintenance, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintenance).

% Modern emergency management, epidemiology, security services now handle threat detection and collective response coordination. The operational functions the ritual originally encoded are now performed by specialized institutions with different authority structures and information bases.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, institutional_threat_detection, observer,
    institutional, generational, analytical, national).

% Would argue that maintaining costly ritual without adaptive function is wasteful or that ritual practice should be openly reformed rather than theatrically maintained. Excluded from decisions about ritual continuity; treated as lacking standing to speak on matters of collective identity and historical memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, external_skeptics, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits threat-recognition and collective-decision-making patterns across generations; encodes responses to catastrophic scenarios through embodied practice and symbolic narrative; maintains shared interpretive frameworks for understanding collective vulnerability.
% TRANSFER_FUNCTION: Transfers time, attention, and emotional labor from present-generation practitioners to the maintenance of symbolic continuity and in-group identity, with diminishing return of adaptive survival competence (that function is now outsourced to institutional threat detection).
% ABSENT_VOICES: External skeptics, descendants of survivors who chose not to pass the practice forward, secular institutional threat managers, and anthropologists who study the practice from outside its identity boundary would argue for explicit reform or dissolution. They are excluded because the practice is maintained as a matter of identity and collective memory rather than utility, and their objections are treated as outside the scope of 'what we do to stay who we are.'
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, practitioners would lose a key identity anchor and historical continuity marker, but institutional threat detection would remain intact and adaptive response capacity would not collapse. The disagreement is precisely where to draw the line: custodians read the ritual's disappearance as catastrophic for collective memory and group coherence; institutional skeptics read it as minor inconvenience masked by therapeutic theater.
% FOUNDING_PROBLEM: After catastrophe (genocide, plague, persecution, natural disaster), survivors encoded threat-recognition patterns and collective-response protocols into ritual form so descendants would carry the adaptive knowledge even if institutional memory was fragmented or destroyed.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists of the specific tradition attest the founding problem was real and acute. Contemporary institutional threat detection systems, epidemiology, and security services confirm that threat recognition and collective response coordination are now handled through specialized institutions with different authority structures. Custodians dispute whether the institutional replacement fully captures the adaptive function; independent observers note the institutional systems handle threat detection more comprehensively but note the ritual continues for reasons of identity and cultural continuity, not adaptive necessity.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.38 at interval end) is moderate and declining because the cost of maintaining the ritual is real but has become decoupled from any concentrated beneficiary — it is diffuse identity cost borne by practitioners, not extraction flowing to an agenda-setter. Theater_ratio (0.68 at interval end) is high and rising because the constraint increasingly consists of forms maintained for symbolic/commemorative reasons rather than functional threat-recognition; the ritual enactment has become performance of continuity rather than encoding of adaptive knowledge. Suppression is very low (0.12) and stable because the practice is maintained primarily through identity internalization and social obligation within the in-group rather than through external coercion or exit barriers imposed by an enforcer — practitioners continue because they cannot bear the identity cost of leaving, not because they are forcibly prevented. The measurement series on a shared time grid shows extractiveness declining as modernity progressively outsources threat-detection to institutions, while theater_ratio rises as the practice becomes increasingly about mourning/commemoration rather than operational knowledge transfer. This divergence — declining extraction coupled with rising theatricality — is diagnostic of piton dynamics: the constraint persists through inertia and identity anchoring, not through active extraction by a beneficiary or through coordination of genuine mutual benefit.
 *
 * PERSPECTIVAL GAP:
 *   Custodians and practitioners perceive the ritual as valuable identity-preservation and historical continuity even as they acknowledge its adaptive function has attenuated. Institutional threat-detection actors see it as ceremonial, correctly noting that threat recognition is now handled through specialized systems. External skeptics see the constraint as wasteful maintenance of outmoded forms. The engine should compute these seats' classifications differently: custodians may score as beneficiaries of identity continuity (low extraction target, d low); practitioners as payers bearing identity-fusion costs without reciprocal adaptive payoff (high extraction target, d high); institutional observers as unaffected (d near symmetric); external skeptics as excluded victims of a practice they read as irrational. The constraint should compute toward piton from the payer seats (high theater_ratio, low suppression, no concentrated extractor) and toward rope-degraded from custodial seats (they benefit from continuity, even if theatrically).
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual custodians benefit from the constraint because it anchors their identity and role as keepers of collective memory; they have low directionality toward extraction (d near 0.2–0.3) because they are the primary beneficiaries of continuity. Present-generation practitioners pay for the constraint through time, attention, emotional labor, and identity fusion without receiving adaptive survival-competence payoff; they have high directionality toward extraction (d near 0.7–0.85) because the cost is real and the benefit has eroded. The in-group identity-maintenance function (listed as beneficiary) is not a person or actor — it is an abstract good that the constraint vindicates; the actual beneficiary seat is the ritual custodians (who maintain professional/identity role). Identity_locked exit for both seats reflects the fusion of identity with the practice: for custodians, it is career and role identity; for practitioners, it is collective in-group identity. Institutional threat detection actors sit as observers (analytical power, unaffected d ≈ 0.5) because the constraint does not directly shape their situation. External skeptics are excluded because the practice is maintained as a matter of identity and collective memory rather than adaptive utility, and their objections are treated as outside the scope of 'what we do to stay who we are' — they have trapped exit if they are members of the in-group (forced to participate despite objection) but organized power to challenge the practice; if external to the group, they have constrained exit (cannot force dissolution, can only argue).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding problem (preserving threat-recognition competence across generations after catastrophe) is dead — modern institutions handle threat detection — but the practice persists because it has become fused with in-group identity and collective memory preservation. The mandate has outlived its function. Piton classification is appropriate because (1) no concentrated beneficiary profits enough to actively maintain the practice; custodians benefit from the identity anchor but do not run the constraint for rent; (2) no concentrated payer is hurt enough to fix it; practitioners bear diffuse costs (time, emotional labor) but identity fusion makes exit unthinkable; (3) the constraint persists through inertia and identity anchoring rather than active choice by either seat; (4) theater_ratio is high and rising, indicating increasing share of activity devoted to symbolic maintenance rather than functional adaptation. The constraint does not need enforcement because practitioners internalize the obligation through identity fusion. No seat extracts enough to incentivize maintaining the arrangement against defection; the arrangement persists because the cost of individual exit exceeds the cost of participation for each actor, even though collective exit would be pareto-improving. This is the piton structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptive_payoff_residual,
    'Does the ritual continue to encode or transmit survival-competent threat-recognition that would be lost if the practice were abandoned, even if modern institutions now handle frontline threat detection?',
    'Anthropological documentation of knowledge transfer through ritual; comparison of threat-recognition patterns in communities maintaining vs. abandoning the practice; analysis of institutional blind spots or failures relative to ritual-encoded patterns.',
    'If adaptive payoff is non-negligible, extractiveness should be higher and theater_ratio lower (constraint approaches rope-degraded rather than pure piton). If payoff is truly zero, the constraint is pure piton maintained by identity fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_payoff_residual, empirical, 'Whether the ritual transmits adaptive threat-recognition despite institutional replacement.').

omega_variable(
    identity_fusion_vs_social_obligation,
    'How much of practitioners'' continued participation is due to genuine identity fusion (they cannot conceive of themselves as members of the group without the practice) versus social obligation and preference falsification (they participate because the group expects it, but would choose differently if exit were costless)?',
    'Post-exit cohort studies: do practitioners who leave the group report identity reconfiguration or primarily relief from obligation? Ethnographic interview data on motivation and cost/benefit reasoning. Comparison with practitioners in contexts where ritual participation is optional.',
    'If fusion is genuine and deep, suppression remains low and identity_locked exit is correctly authored; the constraint persists through internalized obligation. If fusion is partial and preference-falsification is substantial, suppression should be higher (the group enforces participation) and exit should be constrained rather than identity_locked; the constraint is more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_social_obligation, empirical, 'Mechanism of practitioners'' continued participation: identity fusion versus coerced preference.').

omega_variable(
    sibling_reading_observables,
    'Could the same historical and institutional facts support the survival_competence_reading (threat-recognition still functionally real) or the mourning_practice_reading (identity-preservation was always the point) as plausibly as the hybrid_atrophy reading?',
    'Genealogical interview with founding-narrative custodians about original intent and current interpretation. Historical analysis of when/whether the adaptive function actually operated. Institutional comparison: do communities that have abandoned the ritual show measurable threat-recognition decay?',
    'If the facts support the survival_competence reading, ε should be higher and theater_ratio lower; constraint should classify toward tangled_rope (real coordination + extraction). If the facts support the mourning_practice reading, ε should be lower; constraint should classify toward rope (identity coordination). The hybrid reading assumes a historical shift; if that shift is not real, the classification changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_observables, conceptual, 'Whether the observed ritual structure is a historical atrophy or a reading contest over a constant commitment.').

omega_variable(
    kernel_reading_contest_grounding,
    'On what grounds is the hybrid_atrophy reading selected over the sibling readings as the correct interpretation of the kernel''s commitments and history?',
    'Explicit comparison of each reading''s fit to historical evidence: When did the ritual originate and for what stated purpose? How has its stated purpose and practice changed over time? What do practitioners now cite when explaining why they maintain it? What do external scholars and historians say about the original versus current function?',
    'If the survival_competence reading is more grounded in the founding narrative, this reading is foreclosed (both cannot be true in a single framework). If the mourning_practice reading is correct, this reading is foreclosed. If the readings coexist in different communities or historical periods, they remain coexistent; the reading choice depends on which community or historical moment is the referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_grounding, conceptual, 'Grounding of reading choice within the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 14, 0.31).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 42, 0.52).
narrative_ontology:measurement(cata_tr_t71, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 71, 0.64).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 28, 0.48).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 42, 0.42).
narrative_ontology:measurement(cata_be_t71, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 71, 0.38).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 14, 0.16).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 28, 0.14).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 42, 0.13).
narrative_ontology:measurement(cata_su_t71, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 71, 0.12).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_preservation kernel. The kernel is a contested commitment about how ritual practice transmits survival-competence and collective identity. Three distinct constraint stories exist for the three readings: survival_competence_reading (high adaptive payoff, tangled_rope or rope), mourning_practice_reading (low extractiveness, primary function is identity-preservation coordination), hybrid_atrophy_reading (this story — moderate declining extractiveness, piton classification). Each reading has a different ε, different beneficiary/victim structure, and different type. The readings are linked by network edges indicating that each reading's scholarly or practical acceptance creates pressure on the others' viability, but the readings coexist as live positions held by different communities and historical periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__hybrid_atrophy_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
