% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary (Balancing Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'balancing reading' of speech protection,
 *   where the scope of First Amendment rights is determined through a
 *   case-by-case weighing of speech interests against other constitutional
 *   values and demonstrated harms. This approach grants significant
 *   discretion to the judiciary and leads to a fluid, context-dependent
 *   boundary for protected speech. It is a Tangled Rope because it genuinely
 *   coordinates competing values but also extracts from certain speakers
 *   through the uncertainty and potential suppression inherent in a balancing
 *   test.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter (institutional/constrained)
 *   - controversial_speakers: Primary payer (moderate/constrained)
 *   - public_order_advocates: Primary beneficiary (organized/mobile)
 *   - marginalized_speech_groups: Secondary payer (powerless/identity_locked)
 *   - absolutist_advocates: Excluded voice (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.45).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.3).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary (Balancing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '764a93ed-7ae5-4590-b434-2913698355dd').
narrative_ontology:cs_kernel_codification('764a93ed-7ae5-4590-b434-2913698355dd', formalized).
narrative_ontology:cs_authority_grounding('764a93ed-7ae5-4590-b434-2913698355dd', lineage).
narrative_ontology:cs_interpretation_layer_present('764a93ed-7ae5-4590-b434-2913698355dd').
narrative_ontology:cs_reading_relation('764a93ed-7ae5-4590-b434-2913698355dd', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('764a93ed-7ae5-4590-b434-2913698355dd', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('764a93ed-7ae5-4590-b434-2913698355dd', foundational, speech_rights_are_context_dependent).
narrative_ontology:cs_axiom_status(speech_rights_are_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('764a93ed-7ae5-4590-b434-2913698355dd', speech_rights_are_context_dependent, conventional).
narrative_ontology:cs_axiom('764a93ed-7ae5-4590-b434-2913698355dd', foundational, competing_constitutional_values_must_be_weighed).
narrative_ontology:cs_axiom_status(competing_constitutional_values_must_be_weighed, holdable).
narrative_ontology:cs_axiom_grounding('764a93ed-7ae5-4590-b434-2913698355dd', competing_constitutional_values_must_be_weighed, deontological).
narrative_ontology:cs_reference_frame('764a93ed-7ae5-4590-b434-2913698355dd', post_brandenburg_balancing_framework).
narrative_ontology:cs_drift_state('764a93ed-7ae5-4590-b434-2913698355dd', contemporary_digital_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('764a93ed-7ae5-4590-b434-2913698355dd', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, public_order_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginalized_speech_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary arbiter of speech cases, weighing competing constitutional values and societal harms against First Amendment interests. This reading grants the judiciary significant discretion and a central role in defining the boundaries of protected speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals or groups whose speech is deemed harmful or offensive by some, and who face legal challenges or restrictions under this balancing approach. Their speech is not categorically protected but subject to case-by-case review, creating uncertainty and potential chilling effects.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, controversial_speakers, payer,
    moderate, immediate, constrained, local).

% Groups and individuals who prioritize public safety, civility, and the protection of vulnerable communities from harmful speech. This reading provides a mechanism for their concerns to be considered and for speech to be regulated when it demonstrably conflicts with other values.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, public_order_advocates, beneficiary,
    organized, biographical, mobile, national).

% Groups whose speech, often critical of existing power structures, may be perceived as 'harmful' or 'disruptive' and thus subject to restriction under a balancing test. They bear the cost of uncertainty and potential suppression, even when their intent is to advocate for justice.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginalized_speech_groups, payer,
    powerless, generational, identity_locked, local).

% Advocates for a near-absolute protection of speech, who view any balancing test as an erosion of fundamental rights and an invitation to censorship. Their categorical approach is largely sidelined by this reading's methodology.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free speech with other constitutional rights and societal interests, preventing speech from unduly infringing on public safety, privacy, or equality by providing a framework for judicial review.
% TRANSFER_FUNCTION: Transfers the burden of proof and the risk of restriction onto speakers whose expression is deemed to conflict with other values, while granting the judiciary the authority to define and enforce these boundaries.
% ABSENT_VOICES: Those who advocate for a purely categorical, absolutist approach to speech protection are structurally excluded from the balancing framework, as their core premise is rejected by the very methodology. Their arguments for minimal state intervention are not given equal weight.
% DISAPPEARANCE_RATIONALE: If this balancing framework disappeared, the legal landscape for speech would become highly unstable. Either an absolutist approach would prevail, leading to unchecked harmful speech, or a harm-centric approach would dominate, leading to widespread suppression. The current equilibrium, however imperfect, would collapse.
% FOUNDING_PROBLEM: To reconcile the fundamental right to free speech with the need to protect other constitutional values and prevent demonstrable societal harms, particularly in complex and evolving social contexts.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and public safety advocates from diverse perspectives corroborate the ongoing tension between free speech and other values, affirming the continued relevance of a framework to manage these conflicts. While the specific balance is contested, the need for a framework is widely acknowledged.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) stems from the uncertainty and chilling effect a balancing test imposes on speakers, particularly those whose speech is controversial or challenges norms. Suppression (0.30) is moderate but active, as judicial decisions actively restrict certain forms of speech. Theater ratio (0.10) is low, indicating that the balancing process is largely genuine, though it can be influenced by political pressures. Accessibility collapse (0.40) is moderate, as alternatives to regulated speech exist but are constrained by the legal framework. Resistance (0.25) is present from speakers and advocates who challenge specific applications of the balancing test.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and public order advocates perceive this as a necessary and legitimate coordination mechanism, ensuring a responsible exercise of speech rights. Controversial and marginalized speakers, however, experience it as an extractive and suppressive force, where their rights are contingent and subject to the discretion of powerful institutions. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from its expanded role and discretion in defining speech boundaries (low d). Public order advocates benefit from the mechanism to regulate harmful speech (low d). Controversial and marginalized speakers bear the costs of potential restriction and uncertainty (high d). Absolutist advocates are structurally excluded, their position incompatible with the balancing framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the balancing test as pure extraction by acknowledging its genuine coordination function in reconciling competing values. However, it also highlights the extractive and suppressive aspects for certain speakers, preventing it from being seen as a purely benign coordination mechanism. The 'contested' status of the founding problem reflects the ongoing debate about whether the current balance is still serving its original purpose or has drifted towards over-regulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_rule_of_law,
    'Does the case-by-case balancing approach lead to an unacceptably high level of judicial discretion, undermining the predictability and clarity required by the rule of law for free speech?',
    'Empirical analysis of judicial outcomes over time, assessing consistency across similar cases and the development of clear, predictable legal standards. Comparative analysis with jurisdictions employing more categorical rules.',
    'If discretion is found to be excessive, the constraint''s effective suppression and extractiveness for speakers would be higher than measured, as the uncertainty itself becomes a cost. This would push the classification closer to a Snare for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_rule_of_law, conceptual, 'Ambiguity regarding the balance between judicial discretion and legal predictability in speech regulation.').

omega_variable(
    chilling_effect_quantification,
    'What is the actual magnitude of the chilling effect on legitimate speech caused by the uncertainty and potential for restriction inherent in a balancing test?',
    'Surveys of speakers and organizations, analysis of self-censorship patterns, and comparison of speech output in jurisdictions with different regulatory frameworks. This is an empirical question requiring direct measurement.',
    'If the chilling effect is substantial, the measured suppression and extractiveness would be significantly underestimated, pushing the constraint towards a higher extraction classification for speakers, potentially a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Uncertainty about the true extent of self-censorship due to the balancing test.').

omega_variable(
    power_asymmetry_in_balancing,
    'Does the balancing test inherently favor powerful institutional interests (e.g., state, corporations) over marginalized speakers, due to unequal resources for litigation and advocacy?',
    'Analysis of legal aid access, success rates for different types of speakers, and the resources available to parties in speech cases. This would involve a detailed study of legal outcomes and resource allocation.',
    'If power asymmetry is confirmed, the effective extractiveness and suppression for marginalized speakers would be significantly higher, as the ''balancing'' is structurally skewed against them, pushing the constraint towards a Snare for these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_in_balancing, empirical, 'Whether the balancing test is applied equitably across different power levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__balancing_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(spee_tr_t1985, speech_protection_boundary__balancing_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__balancing_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__balancing_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__balancing_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__balancing_reading, base_extractiveness, 1969, 0.35).
narrative_ontology:measurement(spee_be_t1985, speech_protection_boundary__balancing_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__balancing_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__balancing_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__balancing_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__balancing_reading, suppression_requirement, 1969, 0.2).
narrative_ontology:measurement(spee_su_t1985, speech_protection_boundary__balancing_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__balancing_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__balancing_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__balancing_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, protest_permit_requirements).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_protection_boundary' kernel. The 'absolutist_reading' and 'harm_limited_reading' are sibling constraints, each representing a distinct structural interpretation of speech protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
