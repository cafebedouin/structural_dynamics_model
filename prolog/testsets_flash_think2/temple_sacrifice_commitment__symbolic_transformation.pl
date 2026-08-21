% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Authorized Transformation of Temple Sacrifice to Prayer and Study
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the reading of the temple sacrifice commitment
 *   where prayer and study are understood as an authorized, symbolic
 *   transformation and new instantiation of the divine command, rather than
 *   mere substitutes for a suspended practice. This interpretation, developed
 *   by halakhic authorities, ensures the continuity of religious observance
 *   after the destruction of the Temple. The constraint is claimed as a
 *   Tangled Rope because it coordinates the community around a new,
 *   accessible form of worship while simultaneously extracting from those who
 *   adhere to a more literal, material understanding of the original command,
 *   requiring active enforcement of the new interpretive norm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.65).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.75).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Authorized Transformation of Temple Sacrifice to Prayer and Study").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'dedaa33f-673a-4499-819c-6144d77dac54').
narrative_ontology:cs_kernel_codification('dedaa33f-673a-4499-819c-6144d77dac54', fixed_text).
narrative_ontology:cs_authority_grounding('dedaa33f-673a-4499-819c-6144d77dac54', lineage).
narrative_ontology:cs_interpretation_layer_present('dedaa33f-673a-4499-819c-6144d77dac54').
narrative_ontology:cs_reading_relation('dedaa33f-673a-4499-819c-6144d77dac54', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('dedaa33f-673a-4499-819c-6144d77dac54', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('dedaa33f-673a-4499-819c-6144d77dac54', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_axiom('dedaa33f-673a-4499-819c-6144d77dac54', foundational, divine_command_reinterpretable_by_authority).
narrative_ontology:cs_axiom_status(divine_command_reinterpretable_by_authority, holdable).
narrative_ontology:cs_axiom_grounding('dedaa33f-673a-4499-819c-6144d77dac54', divine_command_reinterpretable_by_authority, theological).
narrative_ontology:cs_axiom('dedaa33f-673a-4499-819c-6144d77dac54', secondary, prayer_study_equivalent_to_sacrifice).
narrative_ontology:cs_axiom_status(prayer_study_equivalent_to_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('dedaa33f-673a-4499-819c-6144d77dac54', prayer_study_equivalent_to_sacrifice, theological).
narrative_ontology:cs_reference_frame('dedaa33f-673a-4499-819c-6144d77dac54', rabbinic_halakhic_continuity).
narrative_ontology:cs_drift_state('dedaa33f-673a-4499-819c-6144d77dac54', contemporary_post_temple_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dedaa33f-673a-4499-819c-6144d77dac54', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, mainstream_adherents).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_adherents).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and enforce the authoritative understanding of divine commandments, including the transformation of sacrifice. They maintain the continuity and legitimacy of the tradition through this reinterpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, halakhic_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Find continuity and meaning in their religious practice through prayer and study, accepting the authoritative reinterpretation as the valid fulfillment of the divine command in the absence of the Temple. They benefit from a stable, accessible form of worship.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, mainstream_adherents, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of the reinterpretation, feeling alienated from what they perceive as the tradition's core material requirements. Their identity is deeply tied to the literal performance of sacrifice, making conceptual exit difficult despite the lack of material means.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, literalist_adherents, payer,
    powerless, biographical, identity_locked, local).

% Reject the reinterpretation as a permanent or fully authorized transformation, viewing current practices as temporary substitutes or preparations for a future messianic restoration of material sacrifice. They pay by having their preferred mode of worship deferred or delegitimized.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists, payer,
    moderate, generational, constrained, global).

% The historical record of how religious law and practice have adapted (or not) to changing circumstances, providing context for the legitimacy and impact of such transformations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, historical_precedent, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__symbolic_transformation, historical_precedent).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, accessible, and authoritative means for adherents to fulfill the divine command regarding sacrifice, even in the absence of the Temple, by re-centering practice on prayer and study.
% TRANSFER_FUNCTION: Transfers the locus of religious practice and spiritual merit from material offerings to intellectual and devotional acts (prayer and study), from individual adherents to the collective tradition, under the guidance of halakhic authorities.
% ABSENT_VOICES: Those who insist on the literal, material performance of sacrifice as the only valid form of the commandment are structurally excluded from the authoritative discourse and decision-making processes that define contemporary practice.
% DISAPPEARANCE_RATIONALE: If this authoritative reinterpretation vanished overnight, the entire framework for Jewish religious practice in the absence of the Temple would collapse, leading to widespread theological crisis, profound uncertainty regarding divine command, and a complete reorganization of halakhic observance, potentially fragmenting the community.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of divine worship (material sacrifice) impossible, threatening the continuity of the covenant and the means for atonement and communion with God.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from the Rabbinic period (Mishnah, Talmud) document the theological and practical crisis following the Temple's destruction and the subsequent development of prayer and study as substitutes. This is corroborated by centuries of continuous Jewish legal and theological discourse.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate-high (0.65) because while the transformation provides a functional path for most adherents, it imposes a significant cost on those who believe only material sacrifice fulfills the divine command. Suppression is high (0.75) as the authoritative interpretation actively delegitimizes and marginalizes alternative views, requiring continuous enforcement of the new halakhic norm. Theater ratio is low (0.20) because prayer and study are genuinely functional and central to the religious life of mainstream adherents, not merely performative maintenance of a defunct practice. Accessibility collapse is high (0.80) because material sacrifice is physically impossible, and conceptually, this reading asserts its supersession. Resistance is moderate (0.50) from literalist and messianic groups who maintain alternative interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic authorities and mainstream adherents, this constraint is a necessary and legitimate adaptation, a Rope ensuring continuity of divine service. From the perspective of literalist adherents and messianic restorationists, it is an enforced redefinition that extracts their preferred mode of worship and identity, operating more like a Snare or Tangled Rope. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are the agenda-setters and primary beneficiaries, as they maintain their institutional authority and the tradition's continuity. Mainstream adherents are beneficiaries, gaining an accessible path to observance. Literalists and messianic restorationists are payers, bearing the cost of having their preferred or anticipated forms of worship suppressed or redefined. The identity-lock for literalists amplifies their effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively *prevents* mandatrophy of the original sacrifice commitment by redefining its mandate and providing new, accessible means of fulfillment. However, this redefinition itself becomes a source of extraction for those who do not accept the authority of transformation. The classification as a Tangled Rope captures this dual function: it solves a genuine coordination problem (how to worship without a Temple) but does so through an asymmetric extraction from dissenting interpretations, requiring active enforcement to maintain the new norm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_authority_legitimacy,
    'Is the authority claimed by halakhic institutions to fundamentally redefine divine commandments truly legitimate, or does it represent an unauthorized drift from original intent?',
    'Theological and historical analysis of the scope of rabbinic authority in different eras, particularly regarding the interpretation of immutable divine law. Resolution depends on the adopted meta-halakhic framework.',
    'If unauthorized, the constraint''s extractiveness would be reclassified as higher, and its claimed type would shift closer to a Snare, as the ''coordination'' function would be revealed as a cover for institutional power over divine command.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_authority_legitimacy, conceptual, 'Legitimacy of institutional authority to redefine divine command.').

omega_variable(
    victim_identity_lock_mechanism,
    'Is the ''identity_locked'' exit option for literalist adherents primarily due to genuine internal commitment to the original material practice, or is it reinforced by external social and theological pressures from the mainstream interpretation?',
    'Sociological studies of dissenting religious communities, examining the persistence of literalist identity in contexts where external pressures are reduced or absent. If identity persists without external enforcement, it''s more internal.',
    'If primarily external, the effective suppression for literalist adherents is higher than the base measure suggests, as the constraint''s reach extends into their self-conception. If primarily internal, the identity-lock is a feature of their own commitment, not an imposed cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_lock_mechanism, empirical, 'Structural vs. internalized identity-lock for literalist adherents.').

omega_variable(
    kernel_reading_symbolic_transformation,
    'This constraint instantiates the ''symbolic_transformation'' reading of the ''temple_sacrifice_commitment'' kernel. What are the implications of this specific reading?',
    'Comparison with sibling readings and their structural implications.',
    'This reading asserts an authorized redefinition, leading to a Tangled Rope classification. Other readings might yield different classifications (e.g., a ''performance_only'' reading would likely be a Snare or Piton, as the core practice is impossible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_symbolic_transformation, conceptual, 'This constraint is the ''symbolic_transformation'' reading of the ''temple_sacrifice_commitment'' kernel.').

omega_variable(
    sibling_reading_impact_study_as_exercise,
    'How would the ''study_as_exercise'' reading of the ''temple_sacrifice_commitment'' kernel differ structurally from this ''symbolic_transformation'' reading?',
    'Analyze the ''study_as_exercise'' constraint story (if available) and compare its base properties, stakeholders, and cs_structure.',
    'The ''study_as_exercise'' reading might have lower extractiveness and suppression, potentially classifying as a Rope, as it emphasizes intellectual engagement as a direct fulfillment rather than a redefinition that displaces other forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_study_as_exercise, conceptual, 'Structural differences with the ''study_as_exercise'' sibling reading.').

omega_variable(
    sibling_reading_impact_performance_only,
    'How would the ''performance_only'' reading of the ''temple_sacrifice_commitment'' kernel differ structurally from this ''symbolic_transformation'' reading?',
    'Analyze the ''performance_only'' constraint story (if available) and compare its base properties, stakeholders, and cs_structure.',
    'The ''performance_only'' reading would likely have very high extractiveness and suppression (or be a Piton if the function is entirely atrophied), as it would classify the current impossibility of material sacrifice as a fundamental structural failure, with victims being all adherents who cannot perform it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_performance_only, conceptual, 'Structural differences with the ''performance_only'' sibling reading.').

omega_variable(
    sibling_reading_impact_hybrid_preparatory,
    'How would the ''hybrid_preparatory'' reading of the ''temple_sacrifice_commitment'' kernel differ structurally from this ''symbolic_transformation'' reading?',
    'Analyze the ''hybrid_preparatory'' constraint story (if available) and compare its base properties, stakeholders, and cs_structure.',
    'The ''hybrid_preparatory'' reading might have lower extractiveness for messianic restorationists, as it frames current practices as a temporary, preparatory state rather than a complete redefinition, potentially shifting its classification closer to a Rope for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_hybrid_preparatory, conceptual, 'Structural differences with the ''hybrid_preparatory'' sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 400, 0.22).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 800, 0.2).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1200, 0.19).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1600, 0.19).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 800, 0.6).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1200, 0.63).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1600, 0.64).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2000, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1200, 0.73).
narrative_ontology:measurement(temp_su_t1600, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1600, 0.74).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2000, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'temple_sacrifice_commitment' kernel. Each reading instantiates a distinct constraint with its own ε and classification, linked here to form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
