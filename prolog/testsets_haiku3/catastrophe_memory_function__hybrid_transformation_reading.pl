% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function (Hybrid Transformation Reading)
 *   domain: religious/ritual/collective-memory
 *
 * SUMMARY:
 *   The Passover seder encodes a dual structure: the bitter herbs, maror, and
 *   retelling of Egyptian slavery (D1/D4 mourning-practice and
 *   boundary-identity functions) are performed together with the seder's
 *   symbolic sequence (questions revealing ignorance, narrative rehearsal,
 *   role-reversals, spatial performances of decentralized decision-making)
 *   that transmit survival-capacity for institutional persistence without
 *   central authority (D5). This hybrid reading asserts that BOTH functions
 *   operate in a single ritual performance and that their simultaneity is
 *   structurally necessary — the emotional and identity weight of mourning
 *   makes the survival patterns stick; the survival patterns give mourning
 *   its adaptive meaning. This constraint instantiates one reading of a
 *   contested kernel (catastrophe_memory_function); sibling readings separate
 *   mourning from survivalism into distinct constraints. The hybrid reading
 *   claims the standing arrangement encodes both.
 *
 * KEY AGENTS:
 *   - ritual_community: participants in the annual seder; experience both mourning obligation and survival-competence rehearsal
 *   - ritual_elders: transmitters and administrators of the ritual structure; maintain the dual encoding across generations
 *   - external_observers: academic analysts, grief specialists, anthropologists who may read the functions separately
 *   - grief_specialists: therapeutic and scholarly authorities studying collective trauma and resilience; may read functions in isolation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function (Hybrid Transformation Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious/ritual/collective-memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'df4d9f5e-6fa0-4076-8ad0-959be7981ce1').
narrative_ontology:cs_kernel_codification('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', distributed).
narrative_ontology:cs_authority_grounding('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', distributed).
narrative_ontology:cs_reading_relation('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', foundational, dual_function_necessity).
narrative_ontology:cs_axiom_status(dual_function_necessity, holdable).
narrative_ontology:cs_axiom_grounding('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', dual_function_necessity, deontological).
narrative_ontology:cs_axiom('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', secondary, emotional_cognitive_fusion).
narrative_ontology:cs_axiom_status(emotional_cognitive_fusion, holdable).
narrative_ontology:cs_axiom_grounding('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', emotional_cognitive_fusion, empirically_contingent).
narrative_ontology:cs_reference_frame('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', catastrophe_dual_encoding).
narrative_ontology:cs_drift_state('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', post_shoah_contemporary, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('df4d9f5e-6fa0-4076-8ad0-959be7981ce1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, intergenerational_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, grief_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in the ritual annually (Passover seder). Experiences the bitter herbs as a direct encounter with the loss-memory of catastrophe (Egyptian slavery, Shoah survivors' testimonies) and the seder performance sequence as a rehearsal of survival capacity: what to do if forced to flee, how institutions persist without centralized authority, how to read signs of danger and opportunity. The ritual holds both functions simultaneously in their performance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_community, beneficiary,
    organized, generational, identity_locked, global).

% Transmit the ritual structure and its dual encoding across generations. They administer the texts, set the performance sequence, decide which survival-capacity elements are foregrounded or dampened in any given year. They experience the ritual as both a mourning obligation (the community would be incomplete without it) and a competence reservoir (younger members learning institutional resilience through the seder's spatial and temporal logic).
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_elders, agenda_setter,
    institutional, generational, identity_locked, global).

% Witness the ritual from outside the identity frame. Some read the bitter herbs as mourning-only (D1/D4 framing). Others read the seder structure as survival-rehearsal-only (D5 framing). The hybrid reading asserts both are visible in a single performance and neither alone captures the constraint's function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% Therapists, historians, liturgical scholars studying collective trauma and resilience. They may read the ritual's therapeutic function (mourning) or its epistemic function (transmitting survival patterns) in isolation. The hybrid reading adds: the constraint encodes both, and their simultaneity is structurally necessary — removing the mourning function would hollow out the survival rehearsal's emotional core; removing the survival function would transform the ritual into pure commemoration without adaptive capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, grief_specialists, observer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, grief_specialists, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A ritual that preserves loss-memory while encoding survival-competence. The bitter herbs encode the experience of catastrophe directly; the seder sequence (questions, narrative, symbolic foods, spatial role-reversals) encodes adaptive mechanisms for institutional persistence without centralized authority. Both functions operate in one annual performance.
% TRANSFER_FUNCTION: Transmits from elders to younger participants two intertwined things: (1) the emotional and identity reality of the catastrophe (not historical abstraction but lived memorial obligation) and (2) a rehearsal of decentralized institutional resilience — how to recognize danger, how to move groups without hierarchy, how to preserve knowledge without written records, how to make decisions in dispersed circumstances.
% ABSENT_VOICES: Secular scholars who reject both the mourning obligation and the survival-competence framing, viewing the ritual as purely cultural performance or social bonding. Religious fundamentalists who read the ritual as mourning-only and view survival framing as sacrilege or politicization. Catastrophe deniers or relativizers who would suppress the bitter herbs. Their absence means the ritual's hybrid encoding is not contested within the community but IS contested in public discourse and academia.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared: (1) the community would lose its structured annual encounter with the loss-memory; grief work would disperse into individual therapy instead of collective re-encoding. (2) The survival-capacity transmission would stop; younger members would not learn the seder's encoded patterns of distributed decision-making and institutional persistence. (3) Other ritual forms might partially substitute, but no single alternative holds both functions as tightly. The contest arises because different parties weight the functions differently: some see the loss as primary and survivalism as secondary exploitation of grief; others see survivalism as primary and mourning as cover for teaching institutional evasion; the hybrid reading holds that the constraint's power derives from encoding both simultaneously.
% FOUNDING_PROBLEM: A catastrophe created two simultaneous demands: (1) to remember and honor the dead and displaced, marking group identity through shared memorial obligation; (2) to transmit the adaptive and institutional patterns that enabled survival through the catastrophe, so the next generation would recognize danger and know how to persist without centralized authority. The seder ritual evolved to satisfy both in one structure.
% FOUNDING_PROBLEM_CORROBORATION: Survivor testimony (Shoah memorialists, displacement researchers) attests the founding problem remains live: the need to transmit both grief-memory and survival-pattern. Holocaust education scholarship (e.g., Wertheimer, Roth) supports this reading outside the faith community. Secular anthropologists studying institutional resilience and collective trauma (Shani and Westbrook on distributed authority; Herman on trauma memory) corroborate that ritual can encode both functions simultaneously. The ritual elders themselves attest the dual function explicitly in community teaching materials.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the ritual does extract participation time and emotional labor from the community, but the extraction is legitimized by genuine coordination functions (mourning, survival competence) that produce real benefits. Suppression is low (0.22) because the ritual is sustained primarily through commitment and identity-lock rather than coercive machinery; the modest suppression reflects active discouragement of alternative (secular, non-commemorative) framings. Theater is low-moderate (0.18): the ritual performance is genuine (actual mourning, actual skill rehearsal), but some portion of the sequence's meaning is performative — role-reversals that symbolize rather than directly train, narrative that evokes rather than instructs. The measurements track a slight rise over the 80-year interval (representing post-Shoah era through contemporary): as external observers increasingly contest the dual encoding and survivors' witness becomes time-distant, the ritual community slightly increases theatrical maintenance (more explicit framing, more pedagogy, more defense of the dual reading against mono-functional interpretations). Extractiveness rises slightly as the community invests more in transmission work to maintain the hybrid reading against competing framings.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_community seats (participants and elders) and external_observer seats should compute differently on the perceived type: from inside the identity frame, the constraint is pure coordination (both functions are genuine and necessary). From outside, observers may read it as mourning_practice_reading (D1/D4) or survival_competence_reading (D5) separately; the hybrid reading is only visible to those who hold the dual frame simultaneously. The elders and grief-specialists hold analytical seats that partially overlap — both see the dual structure, but grief specialists may read it as extraction (emotional labor for institutional transmission) while elders read it as benefit (community continuity). The engine computes directionality from beneficiary/victim data and exit options: ritual_community is beneficiary (identity, mourning work, competence) with identity_locked exit; external_observers are analytical with no extraction/benefit relationship; grief_specialists are observers with mobile exit and potential to frame the constraint differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The ritual_community (organized power, identity_locked exit) are beneficiaries of the dual encoding: they receive mourning structure (identity continuity, loss memory), survival competence (institutional resilience patterns), and the fusion that gives each meaning. Their identity-lock is partially structural (belonging requires participation) and partially identity-constituted (the ritual is how they understand catastrophe and continuity). Elders as agenda_setters (institutional power, identity_locked) benefit from the ability to transmit across generations and maintain community coherence. External_observers have no extraction relationship — they analyze but do not participate in the benefit or cost flow. Grief_specialists have analytical/powerful seats with mobile exit; they could reframe or leave, so their directionality approaches symmetric (they benefit from studying the ritual but are not trapped by it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmit both mourning and survival capacity after catastrophe) remains live: survivors and descendants actively need both functions. There is no evidence of mandate obsolescence. However, there is a contestation problem: the hybrid reading competes with mourning-only and survival-only framings that have developed separately (sibling readings). The hybrid reading's persistence requires active work (teaching the dual encoding, defending it against mono-functional interpretations). The modest rise in theater_ratio over 80 years suggests the ritual community is increasingly performing the hybrid reading (making it explicit, pedagogically clear) as it faces competition from external framings. This is not mandatrophy but rather constraint-maintenance-under-contestation. The hybrid reading remains a rope (genuine coordination with modest necessary extraction) rather than degrading to a piton (inertial performance) because the community actively justifies both functions and transmits them across generations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_survivalism_boundary,
    'Is the hybrid encoding structurally necessary, or could the mourning and survival functions separate into distinct rituals without loss of functional capacity?',
    'Empirical observation: communities that attempt to split the functions (mourning-only memorial services vs. separate institutional-skills workshops) report whether the splitting preserves both functions or whether one atrophies without the other''s emotional scaffolding.',
    'If separation preserves both functions, the hybrid reading becomes contingent rather than structurally necessary — the constraint could be two separate ropes instead of one tangled arrangement. If separation causes atrophy, the hybrid reading is vindicated: the constraint''s power derives from the encoding simultaneity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_survivalism_boundary, empirical, 'Whether the constraint''s dual encoding is structurally inseparable or contingently coupled.').

omega_variable(
    reading_frame_committer_ambiguity,
    'Is the hybrid encoding a property of the ritual''s standing structure, or a reading imposed by contemporary scholarship and survivors'' testimony seeking to derive survivalist meaning from a primarily mourning-oriented ritual?',
    'Textual-historical analysis: trace when survival-competence framing entered the ritual''s self-description (nineteenth-century Enlightenment rationalists? twentieth-century Shoah survivors? contemporary scholarship?). Compare with liturgical traditions'' own accounts of the seder''s foundational purpose.',
    'If the survival framing is imposed by contemporary readers, the constraint is a mourning_practice_reading (D1/D4) that external analysts decode as containing survival patterns. If the dual encoding was intentional from the founding, the hybrid reading captures the original design. This determines whether the constraint should be reclassified to the mourning-only reading or remains hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_frame_committer_ambiguity, conceptual, 'Whether the hybrid encoding is an intrinsic design or a contemporary interpretive projection.').

omega_variable(
    identity_lock_mechanism_hybrid,
    'For participants with identity_locked exit, is the lock attributable to the mourning obligation (belonging requires commemoration), the survival competence (belonging requires learning institutional resilience), or the fusion of both in the ritual''s single performance?',
    'Ethnographic interview: ask participants whether they would continue if the ritual became mourning-only, or if it became survival-rehearsal-only, or both. Trace whether departure threats center on breaking the memorial or breaking the competence transmission.',
    'If the identity lock is primarily mourning-driven, the constraint is a mourning_practice_reading where survival patterns are instrumental to the primary function. If primarily survival-driven, it is survival_competence_reading where mourning is emotional scaffolding. If genuinely bifurcated (neither dominates), the hybrid reading captures the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_hybrid, empirical, 'What aspect of the dual encoding drives identity-lock participation.').

omega_variable(
    suppression_target_in_hybrid_frame,
    'What is suppressed by maintaining the hybrid encoding? Is it suppression of the survival-competence framing by mourning-primary communities, or suppression of non-commemoration approaches by the ritual mandate?',
    'Institutional observation: which interpretations are actively discouraged or delegitimized within the community? Are survival-coded teaching moments downplayed as politicizing grief? Are mourning-skeptical framings marginalized? Where does institutional pressure lie?',
    'The measured suppression (0.22) is modest but non-zero, suggesting something is actively defended. If suppression is directed at the survival framing (keeping it latent while grief is explicit), the constraint may be closer to mourning_practice_reading than the hybrid framing suggests. If suppression is directed at alternatives (secular, non-commemorative readings), it confirms the hybrid frame''s structural requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_target_in_hybrid_frame, empirical, 'What specific framing or interpretation is suppressed in the hybrid encoding''s maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement_basis(cata_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family decomposing catastrophe_memory_function into three structurally distinct readings: the hybrid_transformation_reading (this file) claims both mourning and survival functions are necessary and simultaneous; the mourning_practice_reading isolates D1/D4 (loss-memory, boundary identity); the survival_competence_reading isolates D5 (adaptive institutional resilience). The three are not alternative measurements of the same constraint — they are three different constraints sharing a kernel. ε diverges across readings because the referent is the same standing arrangement (the seder) but each reading's own lights assess different functions and extraction mechanisms. The hybrid reading measures both extraction sources; the mono-functional readings each measure one. This is epsilon_invariance compliance (DP-001): different epsilon values for different constraints (different structural claims), not different epsilon for the same constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
