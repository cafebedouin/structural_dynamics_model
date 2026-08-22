% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Harm-Limited Reading: Speech Protection Yields to Demonstrable Unconsented-to Harm
 *   domain: constitutional law / political philosophy / speech regulation
 *
 * SUMMARY:
 *   This constraint instantiates the harm_limited_reading of the
 *   first_amendment_speech_protection kernel. Under this reading, the First
 *   Amendment's protection of speech is not absolute; it contracts when
 *   expression causes demonstrable, unconsented-to harm, particularly to
 *   vulnerable minorities. The state enforces this boundary through civil and
 *   criminal law. The constraint is structurally a tangled rope: it
 *   coordinates genuine protection for historically injured groups while
 *   extracting expressive liberty from speakers whose utterances cross the
 *   harm threshold. The claim and metrics are independently authored; the
 *   engine computes per-seat divergence.
 *
 * KEY AGENTS:
 *   - Vulnerable minorities (powerless/constrained) â beneficiaries who depend on the harm boundary for protection against targeted injurious speech.
 *   - Speakers causing harm (moderate/constrained) â payers who bear the cost of expressive liability when their utterances cross the demonstrated-harm threshold.
 *   - State enforcers (institutional/constrained) â agenda_setters who administer the boundary through legislation, prosecution, and judicial interpretation.
 *   - Free speech advocates (organized/analytical) â observers who contest the breadth and vagueness of the harm boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.52).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.6).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Harm-Limited Reading: Speech Protection Yields to Demonstrable Unconsented-to Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional law / political philosophy / speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '6b63a542-3c39-453c-ac3a-629d7df85cd5').
narrative_ontology:cs_kernel_codification('6b63a542-3c39-453c-ac3a-629d7df85cd5', formalized).
narrative_ontology:cs_authority_grounding('6b63a542-3c39-453c-ac3a-629d7df85cd5', lineage).
narrative_ontology:cs_interpretation_layer_present('6b63a542-3c39-453c-ac3a-629d7df85cd5').
narrative_ontology:cs_reading_relation('6b63a542-3c39-453c-ac3a-629d7df85cd5', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b63a542-3c39-453c-ac3a-629d7df85cd5', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('6b63a542-3c39-453c-ac3a-629d7df85cd5', foundational, speech_yields_to_demonstrable_harm).
narrative_ontology:cs_axiom_status(speech_yields_to_demonstrable_harm, holdable).
narrative_ontology:cs_axiom_grounding('6b63a542-3c39-453c-ac3a-629d7df85cd5', speech_yields_to_demonstrable_harm, deontological).
narrative_ontology:cs_axiom('6b63a542-3c39-453c-ac3a-629d7df85cd5', foundational, harm_must_be_proven_not_presumed).
narrative_ontology:cs_axiom_status(harm_must_be_proven_not_presumed, holdable).
narrative_ontology:cs_axiom_grounding('6b63a542-3c39-453c-ac3a-629d7df85cd5', harm_must_be_proven_not_presumed, empirically_contingent).
narrative_ontology:cs_reference_frame('6b63a542-3c39-453c-ac3a-629d7df85cd5', harm_bounded_public_discourse).
narrative_ontology:cs_drift_state('6b63a542-3c39-453c-ac3a-629d7df85cd5', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6b63a542-3c39-453c-ac3a-629d7df85cd5', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically subject to targeted hate speech, harassment, and discriminatory expression that causes concrete social and economic harm. They depend on legal mechanisms to redress speech that inflicts demonstrable injury, as direct retaliation or market remedies are often unavailable to them.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, generational, constrained, national).

% Individuals whose expression falls outside protection because it causes demonstrable unconsented-to harm to vulnerable minorities. They face civil liability, injunctive relief, or criminal penalties for speech that crosses the harm boundary, and cannot easily relocate to a jurisdiction without similar constitutional norms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm, payer,
    moderate, biographical, constrained, national).

% Legislatures, prosecutors, and courts that define and enforce the boundary between protected speech and harmful expression. They set the standard for what counts as demonstrable harm and adjudicate claims under it, while remaining bound by the constitutional text and precedent.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, state_enforcers, agenda_setter,
    institutional, generational, constrained, national).

% Civil liberties organizations and legal scholars who monitor and challenge harm-based speech regulations, arguing that the harm boundary is vague, subject to viewpoint discrimination, and chills protected expression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, free_speech_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects vulnerable minorities from speech that causes concrete, demonstrable harm by establishing a legal boundary that channels expression away from injurious targets, reducing the need for individual self-help or retaliation.
% TRANSFER_FUNCTION: Moves the costs of expressive liberty from speakers (who must refrain from harmful expression or face sanctions) to vulnerable minorities (who receive legal protection against demonstrable harm they would otherwise bear alone).
% ABSENT_VOICES: Speakers from marginalized communities who might themselves be silenced by overbroad harm definitions; algorithmic amplification platforms that profit from polarizing speech and have no seat at the constitutional table; and minority voices within vulnerable groups who dissent from the harm framework's paternalism.
% DISAPPEARANCE_RATIONALE: If the harm-limited constraint vanished, vulnerable minorities would lose a specific legal remedy against targeted harmful speech, likely increasing reliance on private retaliation or self-censorship; speakers currently restrained by the boundary would expand the scope of expression, and the doctrinal architecture of First Amendment exceptions would contract significantly.
% FOUNDING_PROBLEM: How to preserve a robust sphere for public expression while protecting individuals and minority communities from speech that inflicts concrete, unconsented-to injury without a corresponding democratic or self-governance justification.
% FOUNDING_PROBLEM_CORROBORATION: Free speech advocates attest the problem is exaggerated and the cure is worse than the disease. Civil rights historians and some critical race theorists attest the problem is live and historically unresolved; empirical studies documenting economic and psychological harms from targeted speech corroborate the live-problem reading from outside the pure beneficiary set.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects a significant but bounded extraction of expressive liberty: only speech proven to cause harm is constrained, leaving a wide protected sphere. Suppression (0.60) captures the active legal machinery (liability, injunctions, criminal penalties) required to hold the boundary. Theater ratio (0.30) acknowledges that some harm claims are performatively deployed to silence disfavored speakers, though the core function remains operational. Accessibility collapse (0.50) is moderate: speakers can exit the harm category by altering their expression, but cannot exit the legal regime. Resistance (0.55) reflects persistent First Amendment challenges from civil liberties organizations.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable minorities experience the constraint as protective coordination (low d, subsidized safety), while speakers whose expression is regulated experience it as targeted extraction (high d, loss of liberty). State enforcers occupy a mixed position: they administer the constraint and are partly constrained by it (moderate d). The engine computes this divergence from the structural data rather than the narrative claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (vulnerable_minorities) feed low directionality for the protected class; victim declarations (speakers_causing_harm) feed high directionality for regulated speakers. State enforcers are agenda_setters without explicit beneficiary/victim status, placing them near symmetric. Free speech advocates are observers with analytical exit, producing negligible effective extraction. No overrides are needed because the structural derivation matches the relational map.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem â protecting minorities from injurious speech while preserving a robust public sphere â is contested but not dead. The arrangement has not atrophied into pure theater; courts still issue substantive rulings on harm, and vulnerable communities still seek redress. Classifying it as tangled_rope rather than snare prevents mislabeling the genuine protection function as pure extraction, while preserving the asymmetric cost on speakers. A snare reading would erase the coordination; a rope reading would erase the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the harm_limited_reading of kernel first_amendment_speech_protection. Would a shift to the absolutist_reading or categorical_balancing_reading alter the beneficiary/victim structure and the extraction calculus?',
    'Comparative analysis of the sibling constraint stories in this family.',
    'If the absolutist reading were adopted, victims would disappear and extraction would drop to near zero for speakers, shifting the constraint toward a mountain-like immunity. If categorical balancing were adopted, the victim set would broaden to include any speaker whose expression is weighed down, and extraction would become more diffuse and standard-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural ambiguity from kernel reading plurality').

omega_variable(
    harm_boundary_demonstrability,
    'Can ''demonstrable unconsented-to harm'' be operationalized without collapsing into viewpoint discrimination or paternalism?',
    'Empirical study of harm-based speech regulation outcomes across jurisdictions; measurement of chilling effect on minority speakers.',
    'If demonstrable harm cannot be isolated from viewpoint, the coordination function collapses and the constraint computes as a snare. If it can be isolated, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_boundary_demonstrability, empirical, 'Whether the harm boundary is administrable or a cover for viewpoint selection').

omega_variable(
    coordination_vs_paternalism,
    'Does the constraint genuinely coordinate protection for vulnerable minorities, or does it extract voice from marginalized communities under the guise of protection?',
    'Voice analysis: compare rates of minority-community self-expression in jurisdictions with strong harm-limited regimes versus absolutist regimes.',
    'If minority self-expression is suppressed by the same constraint, the beneficiary designation is undermined and the constraint shifts toward snare. If not, the coordination function is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_paternalism, empirical, 'Whether the coordination function reaches its intended beneficiaries or backfires').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fasp_harm_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fasp_harm_tr_t13, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 13, 0.18).
narrative_ontology:measurement(fasp_harm_tr_t26, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 26, 0.22).
narrative_ontology:measurement(fasp_harm_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(fasp_harm_tr_t53, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 53, 0.28).
narrative_ontology:measurement(fasp_harm_tr_t66, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 66, 0.32).
narrative_ontology:measurement(fasp_harm_tr_t80, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 80, 0.3).

% Extraction over time
narrative_ontology:measurement(fasp_harm_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fasp_harm_be_t13, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 13, 0.42).
narrative_ontology:measurement(fasp_harm_be_t26, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 26, 0.48).
narrative_ontology:measurement(fasp_harm_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(fasp_harm_be_t53, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 53, 0.52).
narrative_ontology:measurement(fasp_harm_be_t66, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 66, 0.5).
narrative_ontology:measurement(fasp_harm_be_t80, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 80, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(first_amendment_speech_protection__harm_limited_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'First Amendment speech protection' conflates three structurally distinct readings. Each reading carries its own epsilon, beneficiary/victim structure, and classification. This file covers the harm-limited reading only; the absolutist and categorical-balancing readings are separate constraints in the same family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
