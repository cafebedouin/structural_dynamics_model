% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist First Amendment Speech Protection (High Harm Threshold)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   speech_harm_boundary kernel: a constitutional interpretation under which
 *   the First Amendment protects speech near-absolutely, with only a narrow
 *   set of historically fixed unprotected categories (incitement, true
 *   threats, defamation, obscenity). The harm override threshold is set
 *   extremely high, meaning targets of speech harmsâdefamation, harassment,
 *   dignitary injuryâbear those costs without legal recourse when the
 *   speech falls outside the narrow exceptions. The constraint is CLAIMED as
 *   a rope (essential coordination for democratic self-governance) while the
 *   authored metrics describe substantial asymmetric extraction: targets
 *   systematically subsidize speaker autonomy. The engine measures that
 *   divergence.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary (agenda_setter/institutional): enforces the absolutist boundary through judicial review
 *   - speakers_and_publishers (beneficiary/organized): receive immunity from liability for protected speech
 *   - targets_of_speech_harm (payer/powerless): bear uncompensated harms from protected expression
 *   - harm_prevention_advocates (excluded/organized): seek lower harm thresholds but are structurally excluded from constitutional viability
 *   - comparative_constitutional_scholars (observer/analytical): provide external corroboration on contestability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.78).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.7).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist First Amendment Speech Protection (High Harm Threshold)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '06910561-3b3b-4d15-9844-6131db3ed437').
narrative_ontology:cs_kernel_codification('06910561-3b3b-4d15-9844-6131db3ed437', fixed_text).
narrative_ontology:cs_authority_grounding('06910561-3b3b-4d15-9844-6131db3ed437', lineage).
narrative_ontology:cs_interpretation_layer_present('06910561-3b3b-4d15-9844-6131db3ed437').
narrative_ontology:cs_reading_relation('06910561-3b3b-4d15-9844-6131db3ed437', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('06910561-3b3b-4d15-9844-6131db3ed437', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('06910561-3b3b-4d15-9844-6131db3ed437', foundational, speech_protection_categorical).
narrative_ontology:cs_axiom_status(speech_protection_categorical, holdable).
narrative_ontology:cs_axiom_grounding('06910561-3b3b-4d15-9844-6131db3ed437', speech_protection_categorical, conventional).
narrative_ontology:cs_axiom('06910561-3b3b-4d15-9844-6131db3ed437', foundational, speaker_autonomy_trumps_harm).
narrative_ontology:cs_axiom_status(speaker_autonomy_trumps_harm, holdable).
narrative_ontology:cs_axiom_grounding('06910561-3b3b-4d15-9844-6131db3ed437', speaker_autonomy_trumps_harm, deontological).
narrative_ontology:cs_reference_frame('06910561-3b3b-4d15-9844-6131db3ed437', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('06910561-3b3b-4d15-9844-6131db3ed437', digital_speech_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('06910561-3b3b-4d15-9844-6131db3ed437', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_and_publishers).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_speech_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the First Amendment speech clause through judicial review, striking down laws that exceed the narrow unprotected categories. Bound by precedent, textual methodology, and the institutional legitimacy of maintaining doctrinal coherence. Cannot easily exit the absolutist framework without reversing foundational precedent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Receive near-absolute protection for expressive conduct across all media platforms. Bear no legal liability for speech harms that fall outside the narrow categories of incitement, true threats, defamation, and obscenity. The constraint subsidizes their expressive autonomy by externalizing harm costs.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_and_publishers, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of defamation, harassment, dignitary injury, and emotional harm from protected speech without adequate legal remedy. Cannot sue for damages or injunctions when the speech falls outside the narrow unprotected categories. Exit from the harm is blocked by the constitutional immunity the constraint grants to speakers.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_speech_harm, payer,
    powerless, immediate, trapped, national).

% Argue for lower harm thresholds and content-based regulations to protect vulnerable groups from speech harms. Their policy preferences are structurally excluded from constitutional viability by the absolutist reading, which treats such regulations as presumptively invalid.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harm_prevention_advocates, excluded,
    organized, biographical, constrained, national).

% Observe that other democratic constitutional orders protect dignity through proportionality balancing, achieving different harm distributions. They provide external corroboration for the contestability of the absolutist reading's harm threshold.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, speakers_and_publishers).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uninhibited public discourse by preventing government from restricting speech based on content or viewpoint, solving the collective-action problem of democratic deliberation and self-governance.
% TRANSFER_FUNCTION: Transfers the costs of speech-related harms (defamation, emotional injury, dignitary harm, harassment) from speakers and publishers to the individuals and groups who suffer those harms, by denying them legal recourse.
% ABSENT_VOICES: Targets of speech harms who seek legal remedy, and legislative majorities that would impose content-based restrictions, are effectively excluded from the constitutional conversation once the absolutist threshold is set.
% DISAPPEARANCE_RATIONALE: If the absolutist protection vanished overnight, speakers would face civil liability and criminal penalties for currently protected expression, legislatures would pass content-based regulations within months, and the marketplace of ideas would reorganize around risk-averse normsâdemocratic discourse would shift fundamentally.
% FOUNDING_PROBLEM: Government censorship of political dissent and viewpoint-based speech suppression, particularly the problem that democratic self-governance requires uninhibited criticism of the state.
% FOUNDING_PROBLEM_CORROBORATION: Historical corroboration exists from outside the beneficiary set: mid-20th-century civil liberties historians and comparative constitutional scholars document the censorship problem. However, contemporary empirical researchers outside the absolutist coalition contest whether the problem justifies the current breadth of protection.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically externalizes speech-harm costs to targets. Suppression is high (0.70) because the arrangement requires active judicial enforcement to strike down alternative regulatory frameworks (hate-speech laws, dignity torts, administrative content regulation). Theater is low-moderate (0.25): judicial rhetoric performatively invokes the marketplace of ideas, but the enforcement is substantively protecting speaker immunity. Accessibility collapse (0.55) reflects that European-style proportionality balancing is constitutionally closed off within the U.S. legal imagination. Resistance (0.60) captures sustained scholarly and activist critique from critical race theory, feminist legal theory, and comparative constitutional scholars.
 *
 * PERSPECTIVAL GAP:
 *   The speaker seat computes near-beneficiary (low d, low effective Ï) because the constraint subsidizes expressive autonomy and immunizes speakers from liability. The target seat computes near-target (high d, high effective Ï) because the constraint denies remedies and forces targets to internalize harm costs. The judiciary seat sits ambiguously: it enforces the constraint and is institutionally constrained by precedent, but does not personally collect the extractionâits directionality is moderate, though its institutional power position means the engine will dampen Ï. The resulting seat divergence is the central measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration maps speakers_and_publishers to low d (subsidy). Victim declaration maps targets_of_speech_harm to high d (extraction). The engine derives these from structural data without override. Harm prevention advocates are excluded (no d computation); comparative scholars are analytical (no d computation).
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy check, this constraint might be misread as a pure snare (the powerful speaking, the powerless suffering). The R5 founding problemâstate censorship of political dissentâwas genuine and is corroborated by historians outside the beneficiary set. However, founding_problem_status is contested because contemporary empirical researchers argue the original censorship problem no longer justifies the current breadth of protection. The mandatrophy mismatch flag (contested status + world_rearranges disappearance verdict) preserves the coordination function in the classification, preventing premature collapse into snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_harm_asymmetry,
    'Does the high harm override threshold systematically underprotect certain categories of speech targets (e.g., marginalized groups facing hate speech) while overprotecting powerful speakers?',
    'Comparative empirical analysis of speech-harm distributions across power differentials; historical correlation between absolutist doctrine and demographic disparities in remedy access.',
    'If asymmetry is systematic, the constraint functions as extraction structured by power; if random, it is a neutral coordination failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_harm_asymmetry, empirical, 'Whether harm threshold asymmetry tracks power differentials.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (state censorship of political dissent) been sufficiently solved such that the absolutist arrangement now persists as inertial extraction?',
    'Comparative historical analysis of government censorship propensity pre- and post-absolutist doctrine; measurement of remaining censorship risk versus extracted harm costs.',
    'If the founding problem is dead, the constraint is a scaffold that failed to sunset or a piton maintained by institutional inertia; if live, the extraction is the necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding censorship problem persists or is obsolete.').

omega_variable(
    kernel_reading_location,
    'This constraint is the absolutist reading of the speech_harm_boundary kernel. Is the disagreement with sibling readings located in the threshold of harm override, or in the fundamental status of speech relative to other constitutional values?',
    'Structural comparison of the three readings'' epsilon values and victim sets; jurisprudential tracing of whether a single judge can coherently hold both absolutist and dignity-based premises.',
    'Determines whether the three readings are threshold variants of one constraint or instantiate different kernels altogether.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Location of disagreement between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__absolutist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__absolutist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__absolutist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__absolutist_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__absolutist_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__absolutist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__absolutist_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__absolutist_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__absolutist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__absolutist_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__absolutist_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__absolutist_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
