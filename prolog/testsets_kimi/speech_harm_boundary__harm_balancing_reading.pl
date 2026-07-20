% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Boundary â Harm Balancing Reading
 *   domain: constitutional_law/communication_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the harm_balancing_reading of the
 *   speech_harm_boundary kernel. Unlike the absolutist_reading, which treats
 *   speech protection as near-absolute, and the dignity_reading, which
 *   categorically subordinates speech to personhood claims, this reading
 *   presumes expressive liberty but permits restriction when harm is
 *   demonstrated and proportionate. It governs constitutional democracies
 *   that balance free expression against social harms such as hate speech,
 *   group libel, and harassment. The constraint coordinates pluralistic
 *   coexistence but extracts asymmetric costs from speakers who must navigate
 *   vague harm thresholds and risk state penalty.
 *
 * KEY AGENTS:
 *   - Constitutional courts (agenda_setter) â interpret proportionality and set evidentiary standards for demonstrated harm.
 *   - Legislative bodies (agenda_setter) â define statutory categories of punishable expression.
 *   - Targeted minorities (beneficiary) â receive legal protection against vilification and harassment.
 *   - Restricted speakers (payer) â bear restriction costs and chilling effects.
 *   - Civil liberties groups (observer) â challenge restrictions and attest to majoritarian-capture risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.55).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.62).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Boundary â Harm Balancing Reading").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'b54cabdd-b978-4d4f-b4c7-9d95086766e4').
narrative_ontology:cs_kernel_codification('b54cabdd-b978-4d4f-b4c7-9d95086766e4', formalized).
narrative_ontology:cs_authority_grounding('b54cabdd-b978-4d4f-b4c7-9d95086766e4', lineage).
narrative_ontology:cs_interpretation_layer_present('b54cabdd-b978-4d4f-b4c7-9d95086766e4').
narrative_ontology:cs_reading_relation('b54cabdd-b978-4d4f-b4c7-9d95086766e4', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b54cabdd-b978-4d4f-b4c7-9d95086766e4', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('b54cabdd-b978-4d4f-b4c7-9d95086766e4', foundational, presumptive_speech_protection).
narrative_ontology:cs_axiom_status(presumptive_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('b54cabdd-b978-4d4f-b4c7-9d95086766e4', presumptive_speech_protection, conventional).
narrative_ontology:cs_axiom('b54cabdd-b978-4d4f-b4c7-9d95086766e4', foundational, proportionality_yields_to_demonstrated_harm).
narrative_ontology:cs_axiom_status(proportionality_yields_to_demonstrated_harm, holdable).
narrative_ontology:cs_axiom_grounding('b54cabdd-b978-4d4f-b4c7-9d95086766e4', proportionality_yields_to_demonstrated_harm, instrumental).
narrative_ontology:cs_reference_frame('b54cabdd-b978-4d4f-b4c7-9d95086766e4', liberal_democratic_speech_order).
narrative_ontology:cs_drift_state('b54cabdd-b978-4d4f-b4c7-9d95086766e4', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b54cabdd-b978-4d4f-b4c7-9d95086766e4', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targeted_minorities).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, restricted_speakers).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, constitutional_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate whether specific expression causes demonstrated harm warranting legal restriction; develop and apply proportionality tests balancing speech protection against asserted harms such as hate speech, group libel, and harassment.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Draft and amend statutes defining categories of punishable expression including hate speech and harassment; set statutory frameworks that delegate proportionality assessment to the judiciary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Receive legal avenues to contest targeted vilification and group libel; rely on state recognition of demonstrated harm to limit speech that attacks group identity and social standing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targeted_minorities, beneficiary,
    organized, generational, constrained, national).

% Bear fines, injunctions, or criminal penalties when courts determine their expression crosses the demonstrated-harm threshold; face ambiguity in standards that chills political, artistic, and dissenting speech across contentious topics.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, restricted_speakers, payer,
    moderate, biographical, constrained, national).

% Litigate against speech restrictions in domestic and international forums; argue that harm thresholds are manipulable and that majoritarian enforcement suppresses minority and dissident viewpoints.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, civil_liberties_groups, observer,
    organized, generational, analytical, national).

narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal architecture for resolving conflict between expressive liberty and demonstrable social harm, enabling pluralistic societies to maintain open discourse while extending protection to vulnerable groups against targeted vilification.
% TRANSFER_FUNCTION: Moves restriction costs from the legal system and targeted communities to speakers when expression is adjudicated to cause demonstrated harm; moves protective relief and legal standing to targeted communities through prohibition and penalty.
% ABSENT_VOICES: Absolutist free-speech advocates who reject any harm-based override entirely; dignity-based theorists who would categorically subordinate speech to personhood without proportionality balancing; speakers from jurisdictions where no presumptive speech protection exists and who are absent from the proportionality calculus.
% DISAPPEARANCE_RATIONALE: If the harm-balancing framework vanished, democracies would revert either toward near-absolute speech protection with no remedy for targeted groups, or toward categorical dignity-based suppression; the specific proportionality architecture structures court dockets, statutory drafting, and everyday speech conduct.
% FOUNDING_PROBLEM: How to protect vulnerable groups from targeted vilification, group libel, and harassment while preserving a meaningful presumption in favor of free expression within a pluralistic democracy.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars and civil liberties organizations outside the targeted-minority beneficiary set attest to the problem's live status; they corroborate the need for harm mitigation but dispute whether the current framework achieves it without capturing majoritarian morality and suppressing legitimate dissent.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the framework genuinely extends protective coordination to targeted groups while imposing asymmetric speaker costs. Suppression is substantial (0.62) because the constraint's persistence depends on active state enforcement and ambiguous standards that suppress contested expression. Theater ratio is moderate (0.40): a growing share of enforcement performs symbolic condemnation rather than measurable harm reduction. Accessibility collapse (0.45) reflects that alternatives (speaking without causing demonstrated harm) remain formally available, but the vagueness of harm standards narrows practical expressive options. Resistance (0.58) is substantial because civil liberties organizations and affected speakers actively contest expanding restriction categories.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (targeted minorities) experiences the constraint as protective coordination that enables safer public participation; the payer seat (restricted speakers) experiences it as coercive extraction with unpredictable boundaries. The engine computes this divergence from the structural data independently of the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts and legislatures administer the constraint from institutional seats with analytical exit; they are neither beneficiaries nor victims in the extraction sense. Targeted minorities occupy the beneficiary position (gaining protection, low directionality). Restricted speakers occupy the target position (bearing restriction costs and chilling effects, high directionality). Civil liberties groups observe from an analytical seat but are structurally excluded from setting the harm threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents the mandatrophy error of mislabeling it as pure coordination (which would ignore the chilling costs imposed on speakers) or pure extraction (which would ignore the genuine protective function for targeted groups). The constraint requires active enforcement, names both beneficiaries and victims, and shows moderate extractiveness â meeting the structural criteria for hybrid coordination-extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_evidentiary_standard,
    'What evidentiary standard counts as ''demonstrated harm'' â rigorous empirical social science, subjective testimony, or majoritarian moral outrage?',
    'Meta-analysis of judicial decisions and comparative constitutional review to identify which evidentiary bases sustain restrictions across jurisdictions.',
    'If subjective testimony or majoritarian sentiment suffice, extraction rises because speaker predictability collapses; if rigorous proof is required, extraction moderates toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_evidentiary_standard, empirical, 'Uncertainty about the evidentiary threshold for demonstrated harm.').

omega_variable(
    majoritarian_capture_risk,
    'Does the proportionality framework genuinely protect vulnerable minorities, or does it encode majoritarian morality that disproportionately restricts minority and dissenting speech?',
    'Empirical analysis of enforcement patterns across regimes to determine whether minority speakers or dissident viewpoints are systematically targeted by harm-based restrictions.',
    'If capture is demonstrated, the constraint shifts toward snare-like extraction; if enforcement genuinely protects vulnerable groups without chilling dissent, it remains tangled rope with a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Risk that the harm-balancing framework is captured by majoritarian sensibilities.').

omega_variable(
    reading_stability,
    'Is the harm-balancing reading a stable legal synthesis, or an unstable compromise that drifts toward the absolutist or dignity poles over time?',
    'Longitudinal comparative constitutional analysis tracking whether harm-balancing regimes converge toward stricter categorical restrictions or toward more speech-protective thresholds.',
    'If unstable toward dignity, effective extraction rises and the constraint may reclassify; if unstable toward absolutism, extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability, conceptual, 'Structural stability of the harm-balancing reading within the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__harm_balancing_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__harm_balancing_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__harm_balancing_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__harm_balancing_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
