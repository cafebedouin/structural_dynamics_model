% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary_harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Conditional on Absence of Significant Harm to Dignity, Equality, and Freedom from Harassment
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   The harm-limited reading of speech protection treats dignity, equality,
 *   and freedom from harassment as constitutional values that categorically
 *   limit the protected sphere of expression. Unlike the absolutist reading
 *   (which confines the unprotected category to imminent lawless action) and
 *   the balancing reading (which weighs interests case by case), this reading
 *   creates fixed exclusion zones: hate speech, targeted harassment, and
 *   coded dog whistles are unprotected by definition. The state becomes the
 *   gatekeeper of these categories, with authority to define, detect, and
 *   suppress. The reading coordinates protection for marginalized groups but
 *   extracts expressive freedom from dissenters, minority-viewpoint holders,
 *   artists, and researchers — a genuine coordination function coupled with
 *   asymmetric extraction, requiring active enforcement to maintain the
 *   categorical boundaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.58).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Conditional on Absence of Significant Harm to Dignity, Equality, and Freedom from Harassment").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '20b64d26-befb-48ac-88bc-b841c4085fda').
narrative_ontology:cs_kernel_codification('20b64d26-befb-48ac-88bc-b841c4085fda', fixed_text).
narrative_ontology:cs_authority_grounding('20b64d26-befb-48ac-88bc-b841c4085fda', lineage).
narrative_ontology:cs_interpretation_layer_present('20b64d26-befb-48ac-88bc-b841c4085fda').
narrative_ontology:cs_reading_relation('20b64d26-befb-48ac-88bc-b841c4085fda', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20b64d26-befb-48ac-88bc-b841c4085fda', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('20b64d26-befb-48ac-88bc-b841c4085fda', foundational, dignity_equality_harm_categorically_limits_speech).
narrative_ontology:cs_axiom_status(dignity_equality_harm_categorically_limits_speech, holdable).
narrative_ontology:cs_axiom_grounding('20b64d26-befb-48ac-88bc-b841c4085fda', dignity_equality_harm_categorically_limits_speech, deontological).
narrative_ontology:cs_axiom('20b64d26-befb-48ac-88bc-b841c4085fda', foundational, state_as_legitimate_harm_gatekeeper).
narrative_ontology:cs_axiom_status(state_as_legitimate_harm_gatekeeper, holdable).
narrative_ontology:cs_axiom_grounding('20b64d26-befb-48ac-88bc-b841c4085fda', state_as_legitimate_harm_gatekeeper, conventional).
narrative_ontology:cs_reference_frame('20b64d26-befb-48ac-88bc-b841c4085fda', post_war_human_rights_constitutionalism).
narrative_ontology:cs_drift_state('20b64d26-befb-48ac-88bc-b841c4085fda', digital_platform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('20b64d26-befb-48ac-88bc-b841c4085fda', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_regulatory_agencies).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, civil_rights_organizations).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, political_dissenters).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, minority_viewpoint_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, artistic_expressors).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, academic_researchers).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignity_as_constitutional_value).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, equality_as_constitutional_value).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, freedom_from_harassment_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designates and enforces the boundaries of unprotected speech categories (hate speech, harassment, coded dog whistles). Administers complaint processes, issues takedown orders, and levies penalties. Gains institutional authority and resource allocation from expanded regulatory mandate. Can shift enforcement priorities across administrations.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Uses the harm-based framework to challenge hate speech, targeted harassment, and discriminatory expression. Gains legal tools and state enforcement backing for dignity/equality claims. Can pivot to alternative advocacy strategies (litigation, public pressure, legislative lobbying) if the reading is narrowed.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_rights_organizations, beneficiary,
    organized, generational, mobile, national).

% Experiences reduced exposure to hate speech, harassment, and dignity-denying expression through state suppression of such speech. Gains expressive safety and participatory parity in public discourse. Exit from the constraint's protection is not meaningfully available — the harm the constraint addresses is structural to their social position.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_groups, beneficiary,
    moderate, biographical, constrained, national).

% Faces suppression risk when dissent is framed as harmful to dignity/equality (e.g., criticism of state policy labeled as hate speech against protected groups, protest rhetoric characterized as harassment). Bears chilling effects and self-censorship costs. Exit requires abandoning the jurisdiction or accepting suppression — constrained by citizenship, platform dependence, and professional ties.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, political_dissenters, payer,
    moderate, biographical, constrained, national).

% Holds views outside mainstream consensus (religious, ideological, cultural) that regulators may classify as harmful to dignity or equality. Bears disproportionate enforcement targeting and platform de-amplification. Exit is constrained by the same structural factors as political dissenters.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, minority_viewpoint_speakers, payer,
    moderate, biographical, constrained, national).

% Creates work that engages provocative, offensive, or boundary-testing themes. Faces content removal, funding denial, and criminal liability under vague harm standards. Has relatively more exit mobility (alternative platforms, international distribution, genre shifts) than political speakers but still bears significant career and audience-access costs.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, artistic_expressors, payer,
    moderate, biographical, mobile, national).

% Studies contested topics (race, gender, sexuality, extremism, historical atrocities) using methodologies that may be characterized as harmful speech. Faces institutional review board scrutiny, funding withdrawal, and professional sanction. Exit is constrained by tenure systems, grant dependence, and disciplinary norms — cannot easily move research abroad without career disruption.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, academic_researchers, payer,
    organized, biographical, constrained, national).

% Advocates for near-absolute speech protection (Brandenburg imminent-lawless-action standard). Argues the harm-based reading is a censorship framework in disguise. Excluded from the regulatory design process because the reading's premises reject their core commitment. Would object to every enforcement action but has no seat at the table.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% Applies case-by-case weighing of speech interests against demonstrated harms. Observes the harm-limited reading's categorical exclusions as a competing framework that may constrain or inform judicial discretion. Neither collects nor pays — adjudicates disputes between the other seats.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, balancing_test_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable standard for suppressing speech that inflicts dignitary, equality, and harassment harms on vulnerable groups — replacing ad hoc balancing with categorical rules that state agencies can enforce consistently.
% TRANSFER_FUNCTION: Moves expressive freedom from dissenters, minority-viewpoint holders, artists, and researchers to state regulatory agencies and civil rights organizations, who gain enforcement authority and legal tools; marginalized groups receive the protective benefit of reduced exposure to targeted harm.
% ABSENT_VOICES: Absolutist free speech advocates (excluded by the reading's premises) and speakers whose expression falls in the gray zone between legitimate discourse and coded harm (e.g., satirists, philosophers of offensive ideas, historians of extremist movements) — they would object to categorical suppression but are not represented in the regulatory design.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished overnight, hate speech and harassment regulations based on dignity/equality grounds would lose their constitutional footing; state agencies would revert to narrower imminent-lawless-action standards or case-by-case balancing; marginalized groups would lose a primary legal shield; civil rights organizations would lose their strongest enforcement lever; the entire regulatory architecture of speech-as-harm would reorganize around the surviving readings.
% FOUNDING_PROBLEM: The absolutist reading (Brandenburg) failed to protect vulnerable groups from the cumulative, dignitary, and equality-eroding harms of hate speech, targeted harassment, and coded dog whistles — harms that do not meet imminent-lawless-action thresholds but structurally exclude targets from public life.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by empirical social science on hate speech effects (e.g., Matsuda, Delgado, Citron), international human rights bodies (UN Special Rapporteur on freedom of expression, ECtHR Article 10 jurisprudence), and testimony from targeted communities — sources outside the direct beneficiary set of state agencies and civil rights organizations.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of expressive liberty from payer seats to agenda-setter and beneficiary seats, with the state gaining regulatory authority and civil rights organizations gaining enforcement tools. Suppression (0.58) is significant because the constraint's persistence depends on active state enforcement — monitoring, takedown orders, penalties — not voluntary compliance. Theater ratio (0.32) captures the gap between the reading's stated protective purpose and the growing enforcement apparatus that also suppresses legitimate dissent, art, and inquiry. Accessibility collapse (0.45) is moderate: alternatives (absolutist, balancing frameworks) remain intellectually available and politically live, but the harm-limited reading has achieved institutional dominance in many jurisdictions. Resistance (0.55) is substantial from excluded absolutist advocates, constrained payer seats, and judicial actors applying competing frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the state/civil-rights seat, the constraint is genuine coordination: it solves the collective-action problem of protecting vulnerable groups from dignitary harm that no individual can address alone. From the payer seats, the same structure operates as enforced extraction: the categorical rules sweep up legitimate dissent, minority viewpoints, artistic provocation, and academic inquiry, and the state's gatekeeping power is exercised with minimal accountability. The engine computes this divergence from the structural data — the claimed type (tangled_rope) acknowledges both coordination and extraction as real.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory agencies are the primary agenda setters (d near 0.0 — they administer the constraint and collect institutional authority). Civil rights organizations are beneficiaries (d low — they gain legal tools without bearing enforcement costs). Marginalized groups are beneficiaries with constrained exit (d low but identity-locked — the harm they face is structural to their position). Political dissenters, minority-viewpoint speakers, and academic researchers are payers with constrained exit (d high — they bear chilling effects, self-censorship, and enforcement targeting; exit requires abandoning jurisdiction or career). Artistic expressors are payers with mobile exit (d moderate-high — they bear costs but have more platform/genre mobility). Absolutist advocates are excluded (d = analytical — they observe but cannot participate). Balancing-test judges are observers (d = analytical — they adjudicate but do not collect or pay).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cumulative dignitary/equality harms below the imminent-lawless-action threshold) remains live — hate speech and harassment persist and evolve (online amplification, coded language, stochastic terrorism). The reading has not atrophied into a piton; its enforcement machinery is expanding, not decaying. However, the theater ratio's rise (0.15 → 0.32) signals growing performative enforcement: actions taken more to demonstrate regulatory vigor than to address measurable harm. The mandatrophy risk is not obsolescence but mission creep — the coordination function (protecting the vulnerable) becoming a cover for the extraction function (suppressing inconvenient speech).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_vagueness,
    'Where is the line between ''significant harm to dignity/equality'' and protected offensive speech — and who decides?',
    'Longitudinal analysis of enforcement decisions across jurisdictions: if the boundary shifts predictably toward suppressing dissent/minority views, the vagueness is a structural feature, not a bug.',
    'If the threshold is inherently manipulable, the constraint''s coordination function is inseparable from its extraction function — the vagueness IS the extraction mechanism. Classification would shift toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_threshold_vagueness, conceptual, 'Whether the harm threshold is a stable coordination standard or an inherently manipulable gatekeeping tool.').

omega_variable(
    state_gatekeeper_abuse_risk,
    'Does making the state the gatekeeper of harm categories create a structural abuse risk that the reading''s own premises cannot contain?',
    'Historical comparison: track whether harm-limited regimes disproportionately target political opposition, minority religions, or disfavored art over time, controlling for stated protective intent.',
    'If abuse is structurally probable, the reading''s claimed coordination function is a false summit — the constraint is a snare with a protective cover story. The engine''s false_summit_mountain signature does not apply (not a mountain), but the same logic would reclassify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeper_abuse_risk, empirical, 'Whether state gatekeeping of harm categories inevitably becomes a tool for suppressing disfavored speech.').

omega_variable(
    coded_dog_whistle_detection_reliability,
    'Can ''coded dog whistles'' be reliably distinguished from legitimate discourse, metaphor, and irony without suppressing the latter?',
    'Inter-annotator agreement studies on dog-whistle classification; false-positive rates in automated detection systems deployed by platforms/state agencies.',
    'If detection is unreliable, the constraint''s suppression falls disproportionately on payer seats (artists, researchers, dissenters using irony/metaphor) — extraction is higher than measured. Theater ratio would be underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coded_dog_whistle_detection_reliability, empirical, 'Reliability of the coded-dog-whistle category as an enforcement boundary.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the speech_protection_boundary kernel admit only these three readings, or are there structurally distinct alternatives (e.g., a procedural reading focused on content-neutral time/place/manner rules, a listener-autonomy reading)?',
    'Survey constitutional theory literature and judicial opinions across jurisdictions for readings that do not map onto the absolutist/balancing/harm-limited trichotomy.',
    'If additional coherent readings exist, the current three-way decomposition is incomplete — the kernel''s framing is under-determined, and the harm-limited reading''s structural claims (e.g., that it is the only alternative to absolutism) are contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel framing captures all structurally distinct readings or imposes a false trichotomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1989, speech_protection_boundary__harm_limited_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(spee_tr_t1995, speech_protection_boundary__harm_limited_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(spee_tr_t2001, speech_protection_boundary__harm_limited_reading, theater_ratio, 2001, 0.21).
narrative_ontology:measurement(spee_tr_t2007, speech_protection_boundary__harm_limited_reading, theater_ratio, 2007, 0.24).
narrative_ontology:measurement(spee_tr_t2013, speech_protection_boundary__harm_limited_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement(spee_tr_t2019, speech_protection_boundary__harm_limited_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__harm_limited_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(spee_be_t1989, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1989, 0.35).
narrative_ontology:measurement(spee_be_t1995, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(spee_be_t2001, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement(spee_be_t2007, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2007, 0.52).
narrative_ontology:measurement(spee_be_t2013, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2013, 0.57).
narrative_ontology:measurement(spee_be_t2019, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1989, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1989, 0.35).
narrative_ontology:measurement(spee_su_t1995, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(spee_su_t2001, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2001, 0.47).
narrative_ontology:measurement(spee_su_t2007, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2007, 0.51).
narrative_ontology:measurement(spee_su_t2013, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2013, 0.54).
narrative_ontology:measurement(spee_su_t2019, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, platform_content_moderation_regimes).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, academic_freedom_protections).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, artistic_expression_protections).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_boundary kernel. The absolutist_reading and balancing_reading are sibling constraints with different ε, beneficiary/victim structures, and claimed types. The harm-limited reading's categorical exclusions structurally influence the balancing reading by pre-empting case-by-case weighing for covered categories. All three form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
