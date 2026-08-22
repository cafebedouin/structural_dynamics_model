% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hybrid Continuity Reading of Hebrew Vitality
 *   domain: sociolinguistic/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint is the hybrid_continuity_reading of the hebrew_vitality
 *   kernel, which contests what constitutes vitality in the Hebrew language.
 *   Sibling readings include liturgical_reading (ritual preservation as
 *   vitality) and native_daily_reading (native generation as vitality). The
 *   hybrid reading synthesizes the two by arguing that liturgical
 *   preservation was a necessary enabler but insufficient for vitality, while
 *   vernacular revival required reconstruction atop that substrate. It
 *   attempts to resolve the kernel contest via reframing rather than
 *   foreclosure, functioning as an analytical coordination device with low
 *   extractiveness and no clear beneficiary-victim asymmetry.
 *
 * KEY AGENTS:
 *   - Hebrew language scholars: Analytical observers who develop and maintain the synthetic framework
 *   - Liturgical communities: Analytical subject whose practice is reframed as necessary substrate
 *   - Vernacular revivalists: Analytical subject whose achievement is framed as dependent on substrate
 *   - Language planning institutions: Institutional observers who may adopt the synthesis for policy coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.2).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hybrid Continuity Reading of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistic/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '957ac20b-0de2-4124-8694-c8fac6c242a2').
narrative_ontology:cs_kernel_codification('957ac20b-0de2-4124-8694-c8fac6c242a2', distributed).
narrative_ontology:cs_authority_grounding('957ac20b-0de2-4124-8694-c8fac6c242a2', expertise).
narrative_ontology:cs_interpretation_layer_present('957ac20b-0de2-4124-8694-c8fac6c242a2').
narrative_ontology:cs_reading_relation('957ac20b-0de2-4124-8694-c8fac6c242a2', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('957ac20b-0de2-4124-8694-c8fac6c242a2', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('957ac20b-0de2-4124-8694-c8fac6c242a2', foundational, vitality_requires_substrate_and_reconstruction).
narrative_ontology:cs_axiom_status(vitality_requires_substrate_and_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('957ac20b-0de2-4124-8694-c8fac6c242a2', vitality_requires_substrate_and_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('957ac20b-0de2-4124-8694-c8fac6c242a2', foundational, liturgical_continuity_as_necessary_substrate).
narrative_ontology:cs_axiom_status(liturgical_continuity_as_necessary_substrate, holdable).
narrative_ontology:cs_axiom_grounding('957ac20b-0de2-4124-8694-c8fac6c242a2', liturgical_continuity_as_necessary_substrate, empirically_contingent).
narrative_ontology:cs_reference_frame('957ac20b-0de2-4124-8694-c8fac6c242a2', integrated_liturgical_vernacular_vitality).
narrative_ontology:cs_drift_state('957ac20b-0de2-4124-8694-c8fac6c242a2', post_revival_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('957ac20b-0de2-4124-8694-c8fac6c242a2', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates between liturgical preservationists and vernacular revivalists in Hebrew language discourse by proposing that vitality requires both historical substrate and active reconstruction, thereby creating a shared analytical vocabulary for otherwise polarized camps.
% TRANSFER_FUNCTION: Moves analytical weight and explanatory legitimacy from exclusive definitions of vitality to a synthetic two-factor framework; transfers no material resources.
% ABSENT_VOICES: Strict liturgical traditionalists who deny that vernacular innovation contributes to sacred language vitality, and strict secular nativists who view liturgical continuity as antiquarian preservation rather than living substrate.
% DISAPPEARANCE_RATIONALE: The synthesis currently mediates between competing camps in Hebrew language policy and scholarship; its disappearance would remove the primary reframing that permits integrated planning and historiography, likely returning the field to polarized deadlock between irreconcilable exclusivist positions.
% FOUNDING_PROBLEM: The polarization of Hebrew vitality discourse into mutually exclusive liturgical and native-daily definitions, which impeded unified language planning, educational policy, and historical understanding.
% FOUNDING_PROBLEM_CORROBORATION: Documented in historiography of the Hebrew revival (e.g., Benjamin Harshav, 'Language in Time of Revolution') and attested by archival records of language-planning institutions showing recurrent policy deadlock between religious and secular educational authorities.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the reading operates as an analytical synthesis rather than an actionable extraction mechanism; it moves explanatory weight, not material rents. Suppression is low (0.20) because the reframing does not actively suppress sibling readings, though it may marginalize them in policy discourse. Theater ratio is minimal (0.08) since the coordination is primarily discursive and scholarly, not performative. Accessibility collapse is moderate (0.35): the synthesis has become a default framing in parts of the field, yet liturgical and nativist alternatives remain fully articulable. Resistance is low-to-moderate (0.25) because both flanks contest the synthesis, but not through structural opposition to the reading itself. Measurements share a single time grid and show slight initial rise as the synthesis gained disciplinary traction, then stability.
 *
 * PERSPECTIVAL GAP:
 *   Minimal perspectival gap is expected: the reading functions as a symmetric analytical coordinate. Liturgical and nativist seats may each experience the reading as mildly diluting their exclusive claims, but neither is structurally targeted for extraction. The agenda-setting seat (scholarly discourse) and the analytical subjects experience the constraint as an interpretive framework rather than a coercive arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   No directional asymmetry is structurally declared: the constraint names no beneficiaries and no victims. The scholarly community and institutional adopters sit near symmetric or mildly beneficiary positions (d near 0.5) because the framework coordinates their understanding without subsidizing them. Liturgical and vernacular communities are analytical subjects, not payers. Effective extraction is therefore negligible across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mandatrophy by keeping its founding problem â the polarization of Hebrew vitality discourse â explicitly live. It does not claim the problem is solved; rather, it offers a permanent analytical framework for managing the tension. Because the founding problem remains live and the reading does not pretend to have transcended it, the mandatrophy-resolved flag is not triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hebrew_vitality_kernel_reading,
    'This constraint instantiates the hybrid_continuity_reading of the hebrew_vitality kernel. Would a sibling reading (liturgical_reading or native_daily_reading) change the beneficiary structure or the epsilon referent?',
    'Compare the stakeholder surfaces and base_extractiveness values of the sibling constraints in the same kernel family.',
    'If sibling readings produce materially different epsilon values or beneficiary/victim structures, the kernel is confirmed as decomposable per the epsilon-invariance principle; if readings converge, the kernel may be a single constraint with observer-relative framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hebrew_vitality_kernel_reading, conceptual, 'Kernel decomposition verification for hebrew_vitality readings').

omega_variable(
    reframing_vs_extraction,
    'Does the hybrid continuity reading''s reframing of vitality as requiring both liturgical substrate and vernacular reconstruction covertly extract analytical legitimacy or institutional resources from liturgical-only or nativist-only frameworks?',
    'Examine citation patterns, curriculum placement, and language-policy resource allocation for asymmetric legitimacy transfer toward institutions aligned with one synthesis component.',
    'If the reframing asymmetrically advantages one camp, epsilon rises and the constraint shifts from rope toward tangled_rope; if it symmetrically coordinates understanding without resource capture, it remains a low-epsilon rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reframing_vs_extraction, conceptual, 'Whether the analytical synthesis masks asymmetric extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t10, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t20, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t30, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t40, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t50, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t10, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t20, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t30, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t40, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t50, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 50, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel decomposes into three structurally distinct constraint stories per the epsilon-invariance principle: hybrid_continuity_reading (low-epsilon analytical synthesis), liturgical_reading (vitality as ritual preservation), and native_daily_reading (vitality as native generation). Each story carries a distinct epsilon, distinct stakeholder surfaces, and a distinct classification. The hybrid reading influences both siblings by reframing their core premises as complementary rather than exclusive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
