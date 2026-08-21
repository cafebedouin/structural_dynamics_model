% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Protection (Listener Harm Not Grounds for Restriction)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents an 'absolutist' reading of speech protection,
 *   where speech is protected near-categorically and listener harm is
 *   generally not considered a valid ground for restriction. This reading
 *   prioritizes speaker autonomy and content-neutrality, often rooted in a
 *   historical fear of government censorship. While it provides broad
 *   protection for speakers, it imposes significant costs on those who
 *   experience harm from speech, particularly vulnerable groups. The
 *   constraint is actively enforced by the judiciary, which consistently
 *   upholds this broad protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.85).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.9).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Reading of Speech Protection (Listener Harm Not Grounds for Restriction)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'b2b99926-8c00-4106-bfb5-2bf27f75029c').
narrative_ontology:cs_kernel_codification('b2b99926-8c00-4106-bfb5-2bf27f75029c', fixed_text).
narrative_ontology:cs_authority_grounding('b2b99926-8c00-4106-bfb5-2bf27f75029c', lineage).
narrative_ontology:cs_interpretation_layer_present('b2b99926-8c00-4106-bfb5-2bf27f75029c').
narrative_ontology:cs_reading_relation('b2b99926-8c00-4106-bfb5-2bf27f75029c', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('b2b99926-8c00-4106-bfb5-2bf27f75029c', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2b99926-8c00-4106-bfb5-2bf27f75029c', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('b2b99926-8c00-4106-bfb5-2bf27f75029c', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('b2b99926-8c00-4106-bfb5-2bf27f75029c', foundational, speaker_autonomy_maximization).
narrative_ontology:cs_axiom_status(speaker_autonomy_maximization, holdable).
narrative_ontology:cs_axiom_grounding('b2b99926-8c00-4106-bfb5-2bf27f75029c', speaker_autonomy_maximization, deontological).
narrative_ontology:cs_axiom('b2b99926-8c00-4106-bfb5-2bf27f75029c', foundational, content_neutrality_principle).
narrative_ontology:cs_axiom_status(content_neutrality_principle, holdable).
narrative_ontology:cs_axiom_grounding('b2b99926-8c00-4106-bfb5-2bf27f75029c', content_neutrality_principle, conventional).
narrative_ontology:cs_reference_frame('b2b99926-8c00-4106-bfb5-2bf27f75029c', founding_era_anti_censorship).
narrative_ontology:cs_drift_state('b2b99926-8c00-4106-bfb5-2bf27f75029c', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b2b99926-8c00-4106-bfb5-2bf27f75029c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, listeners_experiencing_harm).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, advocates_for_speech_restriction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups who wish to express themselves without fear of legal reprisal, even if their speech causes distress or harm to others. They benefit from the broad protection and minimal restrictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers, beneficiary,
    powerful, biographical, arbitrage, global).

% Individuals who are directly subjected to speech that causes them emotional, psychological, or reputational harm, but have no legal recourse to restrict it under this absolutist framework. They bear the direct costs of the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, listeners_experiencing_harm, payer,
    powerless, immediate, trapped, local).

% Collectives historically targeted by hate speech, disinformation, or incitement, who experience systemic harm from speech that is protected under this reading. Their identity is often tied to the very categories targeted by harmful speech, making exit from the 'target' position impossible.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, vulnerable_groups, payer,
    powerless, generational, identity_locked, national).

% Organizations and legal professionals who champion the broadest possible interpretation of speech rights, viewing any restriction based on harm as a dangerous precedent that could lead to censorship. They benefit from the constraint's strong enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, free_speech_advocates, beneficiary,
    organized, generational, analytical, national).

% Organizations and legal scholars who argue for greater regulation of speech, particularly when it causes demonstrable harm or incites violence. They bear the cost of constantly challenging a deeply entrenched legal framework.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, advocates_for_speech_restriction, payer,
    organized, generational, constrained, national).

% The primary enforcers and interpreters of speech protection, who apply the absolutist reading by consistently rejecting harm-based restrictions and upholding speaker autonomy. Their institutional identity is tied to upholding constitutional principles.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, courts_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear and predictable legal boundary for protected speech, minimizing chilling effects on expression and ensuring speakers can articulate diverse viewpoints without fear of state censorship.
% TRANSFER_FUNCTION: Transfers the burden of speech-related harm from speakers to listeners and vulnerable groups, by denying legal grounds for restriction based on listener harm. It also transfers the cost of challenging this framework to advocates for speech restriction.
% ABSENT_VOICES: Those who are systematically marginalized or silenced by the very speech this constraint protects, whose experiences of harm are dismissed as insufficient grounds for restriction. Their voices are often drowned out by the volume of protected speech.
% DISAPPEARANCE_RATIONALE: If near-categorical speech protection vanished overnight, the legal landscape for expression would be fundamentally reconfigured. There would be a rapid proliferation of content-based restrictions, a chilling effect on controversial speech, and a complete reorganization of public discourse around new, more restrictive norms.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust public discourse by protecting individual expression from state interference, particularly after historical periods of political repression.
% FOUNDING_PROBLEM_CORROBORATION: Free speech advocates and many legal scholars attest that the problem of potential government overreach and chilling effects remains live. Advocates for restriction and some legal scholars argue that the nature of harm has shifted from state censorship to private harms (e.g., hate speech, disinformation), making the original problem less relevant to contemporary challenges. Legislative hearings and independent academic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint actively transfers the burden of speech-related harm from speakers to listeners and vulnerable groups, denying them legal recourse. Suppression is also high (0.90) as this reading actively suppresses alternative legal frameworks that would allow for harm-based restrictions. Accessibility collapse is high (0.90) because the legal avenues for restricting speech based on harm are almost entirely foreclosed. Resistance is substantial (0.75) from advocates for speech restriction and affected groups. Theater ratio is low (0.10) because the enforcement of this reading is genuine and functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and free speech advocates, this constraint is a vital Rope, ensuring a vibrant marketplace of ideas and protecting fundamental liberties. From the perspective of listeners experiencing harm and vulnerable groups, it operates as a Snare, actively extracting their safety and dignity by denying them protection from harmful speech. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and free speech advocates are clear beneficiaries, as the constraint maximizes their ability to express themselves. Listeners experiencing harm and vulnerable groups are primary targets/victims, as they bear the costs of unprotected harmful speech. Advocates for speech restriction are also targets, as their efforts to introduce restrictions are consistently suppressed. The judiciary acts as the agenda-setter, actively enforcing this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutism_vs_harm_threshold,
    'Is the rejection of listener harm as a ground for restriction a necessary component of robust speech protection, or an overextension that imposes undue costs on vulnerable populations?',
    'Empirical studies on the societal impact of unprotected harmful speech versus the chilling effect of potential restrictions, combined with a re-evaluation of foundational normative principles regarding individual autonomy versus collective well-being.',
    'If the latter, the constraint''s extractiveness from victims is higher than currently measured, and its classification shifts further towards Snare for those seats. If the former, the current classification is affirmed as a necessary cost of a Rope-like function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutism_vs_harm_threshold, conceptual, 'The fundamental tension between speaker autonomy and listener protection.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative speech regulations structural (legal precedent, institutional inertia) or internalized (a societal belief that any restriction is inherently wrong)?',
    'Post-exit suppression trajectory: if efforts to introduce harm-based restrictions persist and gain traction after legal precedents are challenged or overturned, it suggests the suppression was primarily structural. If resistance to such restrictions remains high even with legal changes, it indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the ''target'' (advocates for restriction) carries the suppression with them after formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for speech regulation.').

omega_variable(
    digital_era_relevance,
    'Does the ''absolutist_reading'' adequately address the unique challenges of speech in the digital era (e.g., virality of disinformation, algorithmic amplification of hate speech, global reach of harassment)?',
    'Comparative legal analysis of jurisdictions with different speech protection frameworks in the digital age, and empirical studies on the efficacy of ''more speech'' as a counter to harmful online content.',
    'If inadequate, the ''absolutist_reading'' may be functionally degrading, leading to higher unacknowledged extraction from victims and a potential reclassification towards Piton or a more extractive Tangled Rope, as its original coordination function struggles to adapt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_era_relevance, empirical, 'Relevance of absolutist speech protection in the digital age.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1791, speech_protection_kernel__absolutist_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(spee_tr_t1850, speech_protection_kernel__absolutist_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(spee_tr_t1900, speech_protection_kernel__absolutist_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__absolutist_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__absolutist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__absolutist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1791, speech_protection_kernel__absolutist_reading, base_extractiveness, 1791, 0.7).
narrative_ontology:measurement(spee_be_t1850, speech_protection_kernel__absolutist_reading, base_extractiveness, 1850, 0.75).
narrative_ontology:measurement(spee_be_t1900, speech_protection_kernel__absolutist_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__absolutist_reading, base_extractiveness, 1950, 0.82).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__absolutist_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__absolutist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1791, speech_protection_kernel__absolutist_reading, suppression_requirement, 1791, 0.75).
narrative_ontology:measurement(spee_su_t1850, speech_protection_kernel__absolutist_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(spee_su_t1900, speech_protection_kernel__absolutist_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__absolutist_reading, suppression_requirement, 1950, 0.87).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__absolutist_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__absolutist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech_protection_kernel', each representing a distinct structural claim about the nature and limits of free speech. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
