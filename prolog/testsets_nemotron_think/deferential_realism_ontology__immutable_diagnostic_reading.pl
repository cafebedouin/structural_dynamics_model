% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Immutable Diagnostic Reading of the DR Typology
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The immutable diagnostic reading is one of three live readings of the
 *   deferential_realism_ontology kernel. It asserts that the DR typology
 *   (mountain, rope, tangled_rope, snare, scaffold, piton) functions as an
 *   observational instrument with fixed, mind-independent referents —
 *   mountains correspond to physical invariants, snares to measurable
 *   extraction mechanisms, and any classification dispute is an observational
 *   error correctable by better measurement. This reading suppresses the
 *   rhetorical_scaffold_reading (which treats categories as normative
 *   declarations) and the hybrid_pragmatic_reading (which grants fixed
 *   referents only to the core). The constraint is the immutable reading
 *   itself: the commitment to treat the typology as a settled ontology rather
 *   than a contested vocabulary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.35).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.78).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Immutable Diagnostic Reading of the DR Typology").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'f8a3e570-ef7b-437d-a3a4-f2f06eef77e1').
narrative_ontology:cs_kernel_codification('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', formalized).
narrative_ontology:cs_authority_grounding('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', expertise).
narrative_ontology:cs_interpretation_layer_present('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1').
narrative_ontology:cs_reading_relation('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', foundational, typology_referents_are_mind_independent).
narrative_ontology:cs_axiom_status(typology_referents_are_mind_independent, holdable).
narrative_ontology:cs_axiom_grounding('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', typology_referents_are_mind_independent, deontological).
narrative_ontology:cs_axiom('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', secondary, classification_disputes_resolve_by_measurement).
narrative_ontology:cs_axiom_status(classification_disputes_resolve_by_measurement, holdable).
narrative_ontology:cs_axiom_grounding('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', classification_disputes_resolve_by_measurement, empirically_contingent).
narrative_ontology:cs_reference_frame('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', fixed_referent_ontology).
narrative_ontology:cs_drift_state('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', contemporary_framework_deployment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f8a3e570-ef7b-437d-a3a4-f2f06eef77e1', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, immutable_diagnostic_adherents).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_designers).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_adherents).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_adherents).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, classification_is_discovery_not_declaration).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, referents_are_mind_independent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that the DR typology's categories (mountain, rope, snare, etc.) refer to mind-independent structural realities. They invest professional identity in the framework's claim to observational objectivity. Exit means abandoning a self-concept as 'doing real classification' rather than 'advocating a normative vocabulary.'
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, immutable_diagnostic_adherents, beneficiary,
    organized, generational, identity_locked, global).

% Authored the formal schema, engine, and classification logic. They benefit when the framework is treated as a settled observational instrument because it secures the system's epistemic authority and adoption. They can pivot to other formalisms if this one loses credibility.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_designers, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, framework_designers, beneficiary).

% Hold that 'snare' and other categories are normative declarations, not discoveries. They experience the immutable reading's dominance as exclusion: their framing is treated as category error rather than legitimate disagreement. Exit requires either adopting the immutable frame or moving to a different critical vocabulary.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_adherents, payer,
    moderate, biographical, constrained, global).

% Hold that the core typology (mountain, rope) tracks real coordination/physics constraints but the periphery (tangled_rope, snare) involves irreducible normative judgment. They are pressured to either concede the full fixed-referent claim or be labeled as denying the framework's scientific status. Exit is constrained by professional investment in the framework's middle ground.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_adherents, payer,
    moderate, biographical, constrained, global).

% Study the framework's deployment and the contest between readings without committing to any. They see the immutable reading's suppression of alternatives as a structural feature of how the framework maintains its authority, not as evidence of the reading's correctness.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic vocabulary so that analysts across domains can identify constraint types without re-litigating foundations at every application. The fixed referents act as a coordination standard — like SI units for constraint structure.
% TRANSFER_FUNCTION: Moves epistemic authority and interpretive control from the hybrid/pragmatic and rhetorical/scaffold readings to the immutable diagnostic reading. The immutable reading captures the 'scientific legitimacy' dividend; the sibling readings bear the cost of having their framings treated as error rather than contestation.
% ABSENT_VOICES: Practitioners who use the framework instrumentally without committing to any reading — they would object to being forced into a metaphysical dispute about whether snares are 'discovered' or 'declared,' but they are not represented in the reading contest because the framework's formal structure does not surface an 'instrumental user' seat.
% DISAPPEARANCE_RATIONALE: If the immutable reading vanished, the framework would lose its claim to being an observational instrument with fixed referents. The hybrid and rhetorical readings would become the default frames, changing how every downstream classification is justified — from 'we measured epsilon and it falls in the snare region' to 'we judge this arrangement serves illegitimate beneficiaries.' The engine's certification logic would need rewriting.
% FOUNDING_PROBLEM: Early DR deployments showed analysts talking past each other: one called a constraint a snare, another called it a tangled rope, and there was no shared procedure to resolve the dispute. The immutable reading was built to solve this by making classification an observational matter — measure the metrics, compute the type, done.
% FOUNDING_PROBLEM_CORROBORATION: The framework designers attest the coordination problem is live (analysts still disagree). Hybrid and rhetorical adherents attest the problem was misdiagnosed — the disagreements were never purely observational but reflected divergent normative commitments about what counts as legitimate extraction. Independent reviewers of early classification disputes (e.g., the BGS decomposition case) corroborate that metric agreement did not produce type agreement; the interpretive layer remained decisive.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).
:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the reading extracts epistemic authority and interpretive control from sibling readings — it captures the 'scientific legitimacy' dividend. Suppression is high (0.78) and rising because the reading's persistence depends on actively treating alternative framings as category errors rather than legitimate contestation (the engine's certification logic, the schema's gate structure, and the corpus's exemplars all encode the immutable frame). Theater is low (0.22) — the coordination function (shared diagnostic vocabulary) is real and the reading genuinely believes its own objectivity claim. Accessibility collapse is high (0.72) because once an analyst adopts the immutable frame, alternative readings appear as simple mistakes. Resistance is moderate (0.55) because the hybrid and rhetorical readings persist and gain adherents despite suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the immutable seat, the constraint is a rope (genuine coordination via fixed referents). From the rhetorical and hybrid payer seats, the same structure operates as a snare (their framings suppressed, their normative commitments treated as errors). The engine computes this divergence from the structural data — the claimed_type 'tangled_rope' reflects my assessment that the constraint has BOTH a real coordination function (shared diagnostic vocabulary) AND asymmetric extraction (epistemic authority captured by one reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Immutable diagnostic adherents are beneficiaries (d ~ 0.2) — they gain a stable, authoritative framework that validates their classifications as 'correct.' Framework designers are agenda-setters with beneficiary capture (d ~ 0.15) — they set the schema and engine defaults that encode the immutable frame. Rhetorical and hybrid adherents are payers (d ~ 0.85) — their framings are structurally excluded from certification, their disputes are resolved by appeal to metrics they regard as question-begging, and they bear the cost of either conforming or operating outside the framework's legitimacy. Analytical observers sit at d ~ 0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (analysts talking past each other) remains live — classification disputes persist. But the immutable reading's solution (fixed referents + observational resolution) has not achieved its stated aim: better measurement has not produced consensus. The mandate has atrophied into a mechanism for suppressing the very contestation it was meant to resolve. The reading persists because the framework designers benefit from the 'observational instrument' brand, and adherents are identity-locked to the self-concept of doing objective classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the immutable_diagnostic_reading a distinct constraint from the kernel itself, or does it conflate the reading with the kernel (treating its own frame as the kernel''s fixed referents)?',
    'Compare the kernel''s formal specification (schema, engine, signature detection) against the immutable reading''s metaphysical commitments. If the kernel''s machinery operates without the immutable metaphysics (e.g., the engine computes types from metrics regardless of whether referents are ''mind-independent''), then the reading is a separable constraint layered on the kernel.',
    'If the reading is separable, its suppression of siblings is an extractive overlay on a pluralistic kernel. If inseparable, the kernel itself is monistic and the siblings are not ''readings of the same kernel'' but rejections of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the immutable reading is a separable constraint or the kernel''s own nature').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression structural (schema gates, engine defaults, corpus exemplars encoding the immutable frame) or internalized (adherents genuinely cannot see alternative framings as anything but error)?',
    'Track suppression trajectory after an analyst exposed to the immutable frame encounters a persistent hybrid/rhetorical adherent who refuses reclassification. If suppression persists (the analyst treats the holdout as irrational), internalized component is significant.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — adherents carry the suppression with them into new domains, pre-emptively dismissing alternative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the immutable reading''s dominance').

omega_variable(
    measurement_resolves_disputes,
    'Do classification disputes actually resolve when metrics are agreed, or do metric agreements mask persistent normative disagreements about threshold placement, metric weighting, and scope assignment?',
    'Audit resolved classification disputes in the corpus: when two analysts agreed on all metric values but disagreed on type, what was the residual disagreement about?',
    'If metric agreement does not produce type agreement, the immutable reading''s core axiom (''misclassification is observational error'') is falsified — the typology''s referents are not fixed by metrics alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_resolves_disputes, empirical, 'Whether observational convergence produces classificatory convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t2, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 2, 0.14).
narrative_ontology:measurement_basis(defe_tr_t2, observed).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(defe_tr_t4, observed).
narrative_ontology:measurement(defe_tr_t6, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(defe_tr_t6, observed).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(defe_tr_t8, observed).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(defe_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t2, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 2, 0.22).
narrative_ontology:measurement_basis(defe_be_t2, observed).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement_basis(defe_be_t4, observed).
narrative_ontology:measurement(defe_be_t6, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement_basis(defe_be_t6, observed).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement_basis(defe_be_t8, observed).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(defe_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t2, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement_basis(defe_su_t2, observed).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement_basis(defe_su_t4, observed).
narrative_ontology:measurement(defe_su_t6, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement_basis(defe_su_t6, observed).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement_basis(defe_su_t8, observed).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement_basis(defe_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.02).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This reading, rhetorical_scaffold_reading, and hybrid_pragmatic_reading form the deferential_realism_ontology constraint family. All three share the kernel (the DR formal schema + engine) but instantiate different constraints with different ε, beneficiaries, victims, and claimed types. The immutable reading claims the kernel has fixed referents; the rhetorical reading claims the kernel is a normative vocabulary; the hybrid claims the kernel has a fixed core and contested periphery. Their ε values differ because they govern different arrangements: the immutable reading governs the arrangement where the framework operates as a settled observational instrument; the rhetorical reading governs the arrangement where the framework operates as a critical vocabulary; the hybrid governs the arrangement where the framework operates as a partial standard with normative periphery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, organized, 0.2).
constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, institutional, 0.15).
constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, moderate, 0.85).
constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
