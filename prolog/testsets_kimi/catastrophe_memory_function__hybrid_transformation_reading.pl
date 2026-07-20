% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_transformation_reading of
 *   the catastrophe_memory_function kernel. The reading treats ritual
 *   (exemplified by Passover) as structurally encoding both mourning-practice
 *   (D1/D4) and survival-competence (D5) within a single performance. Bitter
 *   herbs commemorate loss; the seder rehearsal transmits adaptive mechanisms
 *   for institutional continuity. The constraint is the ritual form itself,
 *   which requires participants to perform both registers simultaneously,
 *   subordinating individual grief to a collective pedagogical project. As a
 *   contested kernel reading, it is distinguished from pure mourning-practice
 *   and pure survival-competence readings by its claim that the dual encoding
 *   is indivisible.
 *
 * KEY AGENTS:
 *   - ritual_specialists (agenda_setter/institutional): control script, authentication, and interpretation of dual encoding
 *   - memory_community (beneficiary/organized): receives identity continuity and boundary maintenance
 *   - individual_mourners (payer/moderate): bear emotional and cognitive cost of prescribed hybrid performance
 *   - memory_scholars (observer/analytical): external analysts assessing whether hybrid function is structural or interpretive imposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '45911a2c-862f-45a9-b1c0-f34be2bf0d79').
narrative_ontology:cs_kernel_codification('45911a2c-862f-45a9-b1c0-f34be2bf0d79', fixed_text).
narrative_ontology:cs_authority_grounding('45911a2c-862f-45a9-b1c0-f34be2bf0d79', lineage).
narrative_ontology:cs_interpretation_layer_present('45911a2c-862f-45a9-b1c0-f34be2bf0d79').
narrative_ontology:cs_reading_relation('45911a2c-862f-45a9-b1c0-f34be2bf0d79', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('45911a2c-862f-45a9-b1c0-f34be2bf0d79', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('45911a2c-862f-45a9-b1c0-f34be2bf0d79', foundational, hybrid_transformation_is_ritual_essence).
narrative_ontology:cs_axiom_status(hybrid_transformation_is_ritual_essence, holdable).
narrative_ontology:cs_axiom_grounding('45911a2c-862f-45a9-b1c0-f34be2bf0d79', hybrid_transformation_is_ritual_essence, instrumental).
narrative_ontology:cs_axiom('45911a2c-862f-45a9-b1c0-f34be2bf0d79', foundational, commemorative_rehearsal_unifies_grief_and_adaptation).
narrative_ontology:cs_axiom_status(commemorative_rehearsal_unifies_grief_and_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('45911a2c-862f-45a9-b1c0-f34be2bf0d79', commemorative_rehearsal_unifies_grief_and_adaptation, conventional).
narrative_ontology:cs_reference_frame('45911a2c-862f-45a9-b1c0-f34be2bf0d79', foundational_deliverance_memorial).
narrative_ontology:cs_drift_state('45911a2c-862f-45a9-b1c0-f34be2bf0d79', contemporary_secular_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('45911a2c-862f-45a9-b1c0-f34be2bf0d79', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, memory_community).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, individual_mourners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, individual_mourners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the ritual script, authenticate the proper performance of mourning and survival rehearsal, and interpret how specific elements encode both loss-memory and adaptive competence. Their authority derives from continuity with the founding catastrophe and the chain of transmission. They cannot abandon the hybrid form without undermining their own legitimacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_specialists, agenda_setter,
    institutional, generational, constrained, global).

% Receives group identity continuity, intergenerational coherence, and the reassurance that catastrophe memory is preserved in actionable form. Participation validates the ritual and maintains the boundary that distinguishes insiders from outsiders. Exit means severing a primary identity anchor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, memory_community, beneficiary,
    organized, generational, identity_locked, global).

% Must perform prescribed mourning within the ritual structure rather than processing grief idiosyncratically. They bear the emotional and cognitive cost of subordinating personal loss-memory to the collective hybrid form, including the labor of performing both grief and gratitude or survival rehearsal simultaneously.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, individual_mourners, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, individual_mourners, beneficiary).

% Study the ritual's dual encoding from outside the believing community, analyzing whether the hybrid function is a genuine structural feature or an interpretive imposition. They compare across catastrophe rituals and assess the empirical validity of survival-competence transmission claims.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational catastrophe memory while simultaneously transmitting survival-adaptive competence through embodied ritual performance, solving the collective problem of maintaining both identity continuity and institutional know-how after catastrophic loss.
% TRANSFER_FUNCTION: Moves emotional and cognitive labor from individual mourners to the collective ritual apparatus, while moving authority and interpretive control to ritual specialists and identity continuity to the memory community.
% ABSENT_VOICES: Individual mourners who would grieve outside prescribed forms; secular memorial practitioners and trauma clinicians who dispute the ritual's monopoly on legitimate memory-work; alternative communities that process catastrophe through non-ritual narrative.
% DISAPPEARANCE_RATIONALE: If the hybrid ritual structure vanished, the community would lose its primary mechanism for simultaneously processing collective loss and rehearsing institutional survival. Memory would fragment into private mourning or purely instrumental training, and the specific transformational bridging of mourning with adaptive competence would collapse, forcing a rearrangement of commemorative practice.
% FOUNDING_PROBLEM: Catastrophic events threaten to dissolve collective identity by overwhelming communal grief capacity while simultaneously destroying the institutional knowledge needed for group survival; the community required a mechanism that could process loss and rehearse continuity without splitting into separate grief and training institutions.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and historians attest to the dual threat of identity dissolution and competence loss in catastrophe contexts from outside the ritual-beneficiary circle. However, the claim that a single ritual form optimally solves both is primarily advanced by ritual specialists. Secular psychologists and trauma researchers contest whether ritualized mourning is individually adaptive, suggesting the hybrid solution may serve collective continuity at the expense of individual recovery.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.45) is moderate because the ritual demands significant emotional and performative labor from participants while concentrating interpretive authority in ritual specialists. Suppression (0.55) reflects the social and religious enforcement of correct performance and the stigma around non-participation or idiosyncratic mourning. Theater ratio (0.30) acknowledges that much ritual action is functional for coordination, but a portion serves to display compliance and maintain the authority of the form. Accessibility collapse (0.60) indicates that once embedded in the community, alternatives to prescribed ritual mourning are hard to envision. Resistance (0.25) is relatively low because the hybrid form offers genuine coordination benefits that mute overt opposition. The measurement series shows gradual intensification of both extraction and theater as the ritual becomes more codified over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual_specialist seat, the constraint is genuine coordination that prevents collective amnesia and ensures intergenerational survival; from the individual_mourner seat, the same structure enforces a specific grief grammar that may conflict with personal mourning needs. The memory_community experiences the constraint as protective boundary-maintenance. The engine computes this divergence from the structural data, and the kernel contest across sibling readings amplifies the perspectival instability.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual_specialists and memory_community sit at the beneficiary end: they receive authority, continuity, and boundary maintenance. Individual_mourners sit at the target end: they pay the compliance and emotional-labor costs. Memory_scholars occupy an analytical position with near-zero directionality. The structural asymmetry is reinforced by identity_locked exit options for the community and mourners, which amplifies effective extraction for the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this as pure coordination (Rope) because the victim declaration captures the asymmetric cost borne by individual mourners who must subordinate personal grief to the collective form. It prevents mislabeling as pure extraction (Snare) because the coordination function â genuine intergenerational memory transmission and survival rehearsal â is structurally real and acknowledged even by critical observers. The hybrid reading's legitimacy depends on both functions being simultaneously operative; if either atrophies, the classification shifts toward Piton or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_encoding_stability,
    'Does the ritual structure genuinely sustain both mourning and survival functions in equilibrium, or does one function typically subsume the other under historical pressure?',
    'Comparative historical analysis of catastrophe rituals across cultures: if hybrid rituals consistently collapse toward pure mourning or pure survival under stress, the dual encoding is unstable.',
    'If unstable, the hybrid reading is an interpretive imposition rather than a structural description, and the constraint should be reclassified as a contingent coupling of two distinct mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_encoding_stability, empirical, 'Whether dual encoding is structurally stable or collapses under pressure').

omega_variable(
    kernel_naturalness,
    'Is the catastrophe memory function an emergent universal of human ritual behavior, or a constructed authority claim by ritual specialists?',
    'Cross-cultural anthropological survey of non-textual catastrophe rituals; if dual encoding appears without lineage transmission, it suggests natural emergence.',
    'If naturally emergent, directionality dampens toward symmetric coordination; if constructed, extraction concentrates in the specialist seat and the constraint trends toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_naturalness, conceptual, 'Natural emergence vs specialist construction of ritual function').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative mourning forms structural (community exclusion) or internalized (belief that non-ritual grief is illegitimate)?',
    'Post-exit trajectory study of individuals who leave the community: if they resume alternative mourning, suppression was structural; if they persist in ritualized forms, it was internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint functions more extractively than scored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of alternative mourning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_transformation_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hybrid_transformation_tr_t8, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(hybrid_transformation_tr_t16, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(hybrid_transformation_tr_t24, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(hybrid_transformation_tr_t32, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(hybrid_transformation_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(hybrid_transformation_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hybrid_transformation_be_t8, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(hybrid_transformation_be_t16, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(hybrid_transformation_be_t24, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(hybrid_transformation_be_t32, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 32, 0.43).
narrative_ontology:measurement(hybrid_transformation_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__hybrid_transformation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, survival_competence_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel decomposes into three structurally distinct readings: mourning_practice (D1/D4 only), survival_competence (D5 only), and hybrid_transformation (D1/D4 + D5 integrated). Each reading assigns a different epsilon and stakeholder structure to the same ritual complex; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
