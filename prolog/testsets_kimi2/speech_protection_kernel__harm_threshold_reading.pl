% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Threshold Conditional Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the harm_threshold_reading of the
 *   speech_protection_kernel. Under this reading, constitutional or legal
 *   protection for expression is not categorical; it is contingent on the
 *   absence of demonstrable harm to identifiable victims. The boundary
 *   between protected and unprotected speech is drawn by judicial balancing,
 *   with victim harm claims overriding speaker autonomy when the threshold is
 *   met. This produces a narrower protection boundary and broader unprotected
 *   categories than sibling readings such as the absolutist_reading or
 *   marketplace_reading. The constraint is actively enforced through judicial
 *   review, tort actions, and criminal prohibitions, and it coordinates
 *   society by providing a formal mechanism to resolve speech-harm conflicts
 *   while asymmetrically extracting expressive liberty from speakers who fall
 *   within the unprotected zone.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter (institutional/analytical) â administers the harm threshold and derives authority from precedent.
 *   - speakers_and_publishers: Primary payer (moderate/constrained) â bear liability risk and chilling costs.
 *   - speech_harm_claimants: Primary beneficiary (moderate/constrained) â gain legal remedies when harm is demonstrated.
 *   - civil_liberties_groups: Analytical observer (organized/analytical) â resist expansion of the threshold.
 *   - absolutist_advocates: Excluded voice (moderate/constrained) â reject harm-based restriction in principle but are outside the doctrinal mainstream.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.66).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.58).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Conditional Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'c7e6913e-aac1-42a7-897d-3055bc66faa5').
narrative_ontology:cs_kernel_codification('c7e6913e-aac1-42a7-897d-3055bc66faa5', formalized).
narrative_ontology:cs_authority_grounding('c7e6913e-aac1-42a7-897d-3055bc66faa5', lineage).
narrative_ontology:cs_interpretation_layer_present('c7e6913e-aac1-42a7-897d-3055bc66faa5').
narrative_ontology:cs_reading_relation('c7e6913e-aac1-42a7-897d-3055bc66faa5', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('c7e6913e-aac1-42a7-897d-3055bc66faa5', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('c7e6913e-aac1-42a7-897d-3055bc66faa5', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7e6913e-aac1-42a7-897d-3055bc66faa5', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('c7e6913e-aac1-42a7-897d-3055bc66faa5', foundational, victim_harm_override_speaker_autonomy).
narrative_ontology:cs_axiom_status(victim_harm_override_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c7e6913e-aac1-42a7-897d-3055bc66faa5', victim_harm_override_speaker_autonomy, deontological).
narrative_ontology:cs_axiom('c7e6913e-aac1-42a7-897d-3055bc66faa5', foundational, demonstrable_harm_as_protection_boundary).
narrative_ontology:cs_axiom_status(demonstrable_harm_as_protection_boundary, holdable).
narrative_ontology:cs_axiom_grounding('c7e6913e-aac1-42a7-897d-3055bc66faa5', demonstrable_harm_as_protection_boundary, empirically_contingent).
narrative_ontology:cs_reference_frame('c7e6913e-aac1-42a7-897d-3055bc66faa5', presumptive_speech_immunity_with_harm_exception).
narrative_ontology:cs_drift_state('c7e6913e-aac1-42a7-897d-3055bc66faa5', digital_public_sphere_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7e6913e-aac1-42a7-897d-3055bc66faa5', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, speech_harm_claimants).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_and_publishers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle_in_constitutional_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, judicial_balancing_test_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the harm threshold in constitutional and statutory review, setting precedents that determine when speech loses protection. Maintains authority through doctrinal continuity and appellate hierarchy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Bear the risk of liability, injunction, and criminal penalty when their expression crosses the harm threshold. Self-censor to avoid litigation costs and legal uncertainty; challenging restrictions requires costly legal defense.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_and_publishers, payer,
    moderate, biographical, constrained, national).

% Seek legal remedies for speech alleged to cause demonstrable harm. Rely on the harm-threshold doctrine to override speaker autonomy and secure injunctions or damages.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speech_harm_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Monitor harm-threshold applications, litigate on behalf of speakers, and argue that the standard chills protected expression. Provide amicus analysis in test cases.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, civil_liberties_groups, observer,
    organized, generational, analytical, national).

% Reject any harm-based speech restriction on principle. Structurally excluded from judicial benches and mainstream constitutional doctrine, though their arguments surface in dissents and academic critique.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of broad expressive freedom and legal protection for victims by supplying a judicially administrable boundary: speech is presumptively protected unless shown to cause demonstrable harm.
% TRANSFER_FUNCTION: Moves the burden of speech restriction from the state to the speaker, who bears the risk of liability once harm is demonstrated; moves the benefit of legal remedy to the harm claimant.
% ABSENT_VOICES: Absolutist speech advocates who deny that listener harm can ever justify restriction; also speakers from jurisdictions with near-categorical protections who would argue the threshold is inherently vague and politically manipulable.
% DISAPPEARANCE_RATIONALE: Without the harm-threshold doctrine, courts would lose the primary modern tool for restricting injurious speech; victims of targeted harassment, fraud, and incitement would lack a doctrinal remedy; speakers would face a dramatically different risk landscape, and constitutional jurisprudence would reorganize around absolutist, dignity, or marketplace alternatives.
% FOUNDING_PROBLEM: How to construct a speech-protection regime that shelters dissent, art, and political expression without leaving victims of defamation, incitement, or severe harassment without legal recourse.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional historians and free-speech scholars outside the direct beneficiary set attest that the problem of balancing expression and harm has persistent historical salience; civil liberties organizations corroborate that the tension remains live but dispute that the harm-threshold framework is the appropriate resolution.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 because the harm-threshold standard extracts a substantial margin of expressive liberty from speakers through liability risk, self-censorship, and legal defense costs. The historical series shows rising extraction (0.35 to 0.66) as courts expanded categories of unprotected speech and lowered practical thresholds for demonstrating harm. Suppression is 0.58: legal penalties and injunctive power suppress certain utterances, though alternatives (other topics, anonymous channels) persist imperfectly. Theater_ratio rises from 0.15 to 0.38, reflecting increasing ritualization of balancing tests and doctrinal formulae that sometimes mask outcome-driven reasoning. Accessibility_collapse is 0.72 because, once the precedent structure is understood, speakers find it difficult to locate safe harbors outside the harm framework, and judicial interpretation is the only authoritative exit. Resistance is 0.48, driven by persistent First Amendment advocacy and periodic legislative pushback.
 *
 * PERSPECTIVAL GAP:
 *   The speaker seat experiences the constraint as extractive: every utterance carries a risk of liability if a court later finds demonstrable harm, and the vagueness of the threshold chills marginal speech. The harm-claimant seat experiences it as protective coordination: the doctrine provides a recognized path to stop injurious expression. The judiciary sees a neutral balancing framework. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Speech_harm_claimants are declared beneficiaries; the constraint subsidizes their legal position and yields them a low directionality value (d near beneficiary). Speakers_and_publishers are declared victims (payer role); the constraint extracts from them and yields a high directionality value (d near target). The judiciary, as agenda_setter with analytical exit, sits near symmetric. Civil liberties groups, as observers with analytical exit, also sit near symmetric. The engine will compute high effective extraction for speakers and low or negative extraction for claimants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to shelter valuable expression while remedying genuine harm â is authored as contested, not dead. The constraint retains a live coordination function (victims obtain remedies) alongside its extraction function (speakers bear liability risk). Because the founding problem has not been resolved and the coordination function has not atrophied into pure theater, mandatrophy_resolved is not declared. The rising theater_ratio is a symptom of doctrinal ritualization but does not yet indicate that the primary function is performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_validity,
    'Is the harm-threshold reading the structurally accurate instantiation of the speech protection kernel, or does another reading (absolutist, dignity, marketplace, democratic participation) better capture the operative constraint?',
    'Cross-reading corpus comparison: evaluate which reading''s stakeholder structure, beneficiary/victim distribution, and directionality profile match observed judicial and legislative behavior across jurisdictions.',
    'If another reading is structurally dominant, this constraint''s classification as tangled_rope may shift to rope (absolutist/marketplace) or snare (dignity), altering the derived directionality for speakers and claimants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Structural ambiguity between sibling readings of the speech protection kernel').

omega_variable(
    harm_threshold_empirical_efficiency,
    'Does the demonstrable harm standard deliver measurable victim protection, or does it primarily function as a speech-chilling mechanism with uneven remedy distribution?',
    'Comparative empirical analysis of speech-restriction outcomes in harm-threshold jurisdictions versus absolutist or dignity-based regimes, measuring victim remedy rates, speaker self-censorship rates, and judicial consistency.',
    'If the standard chills more than it remedies, effective extractiveness rises and the coordination function weakens, pushing classification toward snare; if it remedies effectively, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_empirical_efficiency, empirical, 'Empirical ambiguity about whether the harm threshold protects victims or chills speakers').

omega_variable(
    interpretive_drift_vs_doctrinal_stability,
    'Has the harm threshold remained a stable legal standard, or has interpretive drift converted it into an ad hoc tool for judicial preference?',
    'Longitudinal doctrinal mapping of harm-threshold applications across ideologically divergent courts and eras, measuring predictability and consistency of outcomes.',
    'If interpretive drift is severe, the constraint''s theater_ratio understates the performative component, and the effective suppression is higher than structural measures suggest; this would strengthen piton or snare signals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_vs_doctrinal_stability, conceptual, 'Ambiguity about whether the harm threshold is a stable standard or a veneer for judicial discretion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__harm_threshold_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(spee_tr_t80, speech_protection_kernel__harm_threshold_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement(spee_tr_t100, speech_protection_kernel__harm_threshold_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(spee_be_t80, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(spee_be_t100, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 100, 0.66).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_kernel__harm_threshold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
