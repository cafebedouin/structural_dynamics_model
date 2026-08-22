% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Threshold Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint instantiates the harm-threshold reading of the speech
 *   protection kernel: speech is protected unless it causes demonstrable harm
 *   to identifiable victims. The reading narrows the protection boundary
 *   relative to absolutist and marketplace readings by making victim harm a
 *   trumping consideration. It broadens unprotected categories to include
 *   hate speech, targeted harassment, and speech that enables structural
 *   subordination. The constraint is a tangled rope: it coordinates a genuine
 *   social need (protecting vulnerable groups from harm) while extracting
 *   speech autonomy from speakers and concentrating regulatory power in the
 *   state. Active enforcement is required — the harm threshold must be
 *   continually adjudicated, and the boundary policed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.65).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '0807b9a2-47ee-4c8f-bd20-16f4e5d21853').
narrative_ontology:cs_kernel_codification('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', distributed).
narrative_ontology:cs_authority_grounding('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', practice).
narrative_ontology:cs_interpretation_layer_present('0807b9a2-47ee-4c8f-bd20-16f4e5d21853').
narrative_ontology:cs_reading_relation('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', foundational, victim_harm_trumps_speaker_autonomy).
narrative_ontology:cs_axiom_status(victim_harm_trumps_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', victim_harm_trumps_speaker_autonomy, deontological).
narrative_ontology:cs_axiom('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', secondary, demonstrable_harm_threshold_is_administrable).
narrative_ontology:cs_axiom_status(demonstrable_harm_threshold_is_administrable, holdable).
narrative_ontology:cs_axiom_grounding('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', demonstrable_harm_threshold_is_administrable, empirically_contingent).
narrative_ontology:cs_reference_frame('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', post_war_human_rights_settlement).
narrative_ontology:cs_drift_state('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0807b9a2-47ee-4c8f-bd20-16f4e5d21853', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, target_groups_protected_from_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_restricted_by_harm_threshold).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, dissident_voices_chilled_by_uncertainty).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_prevention_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, victim_protection_priority_over_speaker_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces speech restrictions justified by demonstrable harm thresholds. Gains regulatory legitimacy and expanded authority to define and police speech boundaries. Can adjust harm definitions and evidentiary standards over time.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, state_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, state_regulatory_authority, beneficiary).

% Marginalized communities, harassment victims, and historically subordinated groups who receive protection from speech that demonstrably harms them. Their exit is identity-locked — they cannot exit their vulnerability to hate speech, harassment, or structural subordination through speech. They gain legal remedies and institutional recognition of harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, target_groups_protected_from_harmful_speech, beneficiary,
    organized, generational, identity_locked, national).

% Speakers whose expression is restricted when it crosses the demonstrable harm threshold. Includes artists, journalists, activists, and ordinary citizens. They bear compliance costs, self-censorship, and legal risk. Exit is constrained — they can modify speech or move jurisdictions but cannot easily escape the regulatory regime.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_restricted_by_harm_threshold, payer,
    moderate, biographical, constrained, national).

% Speakers with minority or oppositional viewpoints who self-censor due to uncertainty about where the harm threshold lies. The vagueness of 'demonstrable harm' creates a chilling zone beyond the formal restriction. They lack resources to litigate boundary cases and face disproportionate enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, dissident_voices_chilled_by_uncertainty, payer,
    powerless, biographical, trapped, national).

% Interpret and apply the harm threshold in concrete cases. Their rulings define the operational boundary of protected vs. unprotected speech. They are not direct beneficiaries or payers but shape the constraint's real-world operation through precedent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, courts_and_adjudicators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, courts_and_adjudicators, agenda_setter).

% Civil liberties organizations that argue for broader speech protection. They would object to the harm-threshold framework as insufficiently protective of speaker autonomy. They are excluded from the core regulatory bargain but litigate at the margins and influence public discourse.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, free_speech_advocacy_organizations, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled boundary for speech regulation: protects vulnerable groups from demonstrable harm while preserving speech that does not meet the harm threshold. Solves the coordination problem of distinguishing legitimate expression from harmful speech in a pluralistic society.
% TRANSFER_FUNCTION: Transfers speech autonomy from speakers (especially those near the harm boundary) to protected groups and regulatory authority. Moves the power to define acceptable speech from individual speakers to institutional harm-assessment mechanisms.
% ABSENT_VOICES: Speakers whose expression is chilled but never reaches adjudication — the 'silent majority' of self-censored voices. Also excluded: future speakers who would test boundaries but are deterred by uncertainty. They are not in the room because their silence is the constraint's intended effect.
% DISAPPEARANCE_RATIONALE: If the harm-threshold framework vanished overnight, either absolutist protection would expand (if courts default to stricter scrutiny) or regulatory authority would fill the vacuum with broader restrictions. The specific balance of protected vs. unprotected speech categories would collapse and be renegotiated — the world rearranges around a new speech-regime equilibrium.
% FOUNDING_PROBLEM: How to reconcile robust speech protection with the reality that some speech causes demonstrable harm to identifiable victims — especially hate speech, targeted harassment, and speech that enables violence or discrimination.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Human Rights Committee, European Court of Human Rights) and comparative constitutional courts (Canada, Germany, South Africa) corroborate that the harm-prevention problem is live and requires balancing. The beneficiary groups (target communities) and state authorities attest it is live; absolutist and marketplace reading proponents contest it.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).
:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the constraint transfers meaningful speech autonomy from speakers to regulatory structures and protected groups, but less than a snare because the coordination function (harm prevention) is genuine and acknowledged across the political spectrum. Suppression (0.65) is substantial because the constraint depends on active enforcement — legal penalties, content removal, platform regulation — and the harm threshold's vagueness creates a chilling zone beyond formal restrictions. Theater ratio (0.28) captures that some enforcement performs 'harm prevention' while actually suppressing dissent or minority viewpoints, but the core harm-prevention function is real. Accessibility collapse (0.58) and resistance (0.52) reflect that alternatives (absolutist, marketplace frameworks) remain conceptually available and actively advocated, but the harm-threshold framework has become dominant in international and comparative law.
 *
 * PERSPECTIVAL GAP:
 *   The state regulatory authority and protected groups experience this as coordination (rope-like): a necessary framework for preventing harm. Restricted speakers and chilled dissidents experience it as extraction (snare-like): their autonomy is taken for others' protection with uncertain boundaries. Courts sit in the middle — they administer the constraint but their legitimacy depends on being seen as neutral arbiters rather than beneficiaries. The engine computes this divergence from the structural data: different power/exit/spatial_scope profiles yield different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority is a structural beneficiary (collects regulatory power, defines harm — d near 0.1). Protected groups are beneficiaries with identity-locked exit (d near 0.2 — they gain protection but cannot exit their vulnerability). Restricted speakers are payers with constrained exit (d near 0.75). Chilled dissidents are payers with trapped exit (d near 0.9). Courts are analytical observers (d = 0.5). Free speech advocates are excluded — their exclusion is structural, not accidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harm prevention) remains live — new speech harms emerge with technology (online harassment, algorithmic amplification, deepfakes). The constraint has not atrophied into a piton because the coordination function tracks evolving harm. However, mandatrophy risk exists if the harm threshold expands to cover speech that merely offends or challenges power, converting coordination into pure extraction. The current metrics suggest the constraint is still in its coordination phase but with growing extractive drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_vagueness,
    'How vague is the ''demonstrable harm'' standard in practice, and does its vagueness function as a feature (flexibility) or bug (chilling effect)?',
    'Empirical study of judicial outcomes: measure variance in harm findings across similar cases, and survey speaker self-censorship rates in jurisdictions with harm-threshold frameworks vs. absolutist frameworks.',
    'If vagueness is a bug producing systematic chilling beyond the formal boundary, extractiveness is understated and the constraint trends toward snare. If vagueness is a manageable feature, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_vagueness, empirical, 'Whether the harm threshold''s operational vagueness is a coordination cost or an extraction mechanism.').

omega_variable(
    harm_definition_capture,
    'Can the state or powerful groups capture the harm definition to suppress dissent rather than protect vulnerable groups?',
    'Longitudinal analysis of harm-adjudication cases: track what speech categories are added to ''harmful'' over time, and whether additions correlate with threat-to-power vs. threat-to-vulnerable-groups.',
    'If capture is documented, the constraint''s coordination function is a cover for extraction — reclassify toward snare. If harm definitions track genuine victim harm, the tangled rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_definition_capture, empirical, 'Whether the harm-threshold mechanism is captured by the agenda-setter for non-harm-prevention purposes.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Is the speech_protection_kernel best framed as a single commitment with competing readings, or as multiple distinct constraints (one per reading) that happen to share a label?',
    'Compare the structural parameters (beneficiaries, victims, enforcement, exit options) across readings. If they differ fundamentally, the ε-invariance principle demands separate constraint stories.',
    'If the kernel framing is incoherent, each reading should be authored as an independent constraint with its own ε, not as readings of a shared kernel. This affects cs_structure declarations and network linking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the committer-frame kernel abstraction accurately captures the structural reality or imposes a false unity.').

omega_variable(
    dignity_harm_boundary,
    'Where does the harm-threshold reading''s ''demonstrable harm'' end and the dignity_reading''s ''structural subordination'' begin — are they distinct thresholds or a continuum?',
    'Analyze case law where both frameworks are invoked: do courts treat them as alternative tests or as a unified harm/subordination inquiry?',
    'If they collapse into a single threshold, the two readings may be structurally the same constraint with different rhetoric. If distinct, they are separate constraints in the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_harm_boundary, conceptual, 'Structural boundary between harm-threshold and dignity-based speech restrictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_tr_t1950, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(speech_harm_tr_t1975, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(speech_harm_tr_t1990, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(speech_harm_tr_t2005, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(speech_harm_tr_t2015, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(speech_harm_tr_t2025, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(speech_harm_be_t1950, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(speech_harm_be_t1975, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement(speech_harm_be_t1990, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(speech_harm_be_t2005, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(speech_harm_be_t2015, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(speech_harm_be_t2025, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_su_t1950, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(speech_harm_su_t1975, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(speech_harm_su_t1990, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(speech_harm_su_t2005, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2005, 0.61).
narrative_ontology:measurement(speech_harm_su_t2015, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(speech_harm_su_t2025, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__harm_threshold_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five constraint stories (one per reading) linked by affects_constraints. Each reading has a distinct ε and beneficiary/victim structure. The harm_threshold_reading has the highest extractiveness among the readings because it actively restricts speech based on harm adjudication, whereas absolutist and marketplace readings extract near-zero, and dignity/democratic readings fall in between.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, organized, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
