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
 *   This constraint represents the 'balancing reading' of the speech
 *   protection boundary, where First Amendment interests are weighed against
 *   other constitutional values and demonstrated harms on a case-by-case
 *   basis. This approach leads to a shifting boundary of protected speech,
 *   with the judiciary playing a central role in adjudication. It is distinct
 *   from absolutist or harm-limited readings, which propose more rigid or
 *   expansive boundaries.
 *
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
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '828433c0-e3a8-4cbe-9b0b-1a5c34c4e818').
narrative_ontology:cs_kernel_codification('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', formalized).
narrative_ontology:cs_authority_grounding('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', lineage).
narrative_ontology:cs_interpretation_layer_present('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818').
narrative_ontology:cs_reading_relation('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', speech_is_not_absolute, deontological).
narrative_ontology:cs_axiom('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', foundational, constitutional_values_must_be_balanced).
narrative_ontology:cs_axiom_status(constitutional_values_must_be_balanced, holdable).
narrative_ontology:cs_axiom_grounding('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', constitutional_values_must_be_balanced, conventional).
narrative_ontology:cs_reference_frame('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', judicial_balancing_framework).
narrative_ontology:cs_drift_state('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('828433c0-e3a8-4cbe-9b0b-1a5c34c4e818', '').
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

% The primary arbiter of speech cases, weighing competing constitutional values and societal harms against First Amendment interests. This role grants significant interpretive power and shapes the evolving boundary of protected speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals or groups whose speech is deemed to infringe on other constitutional values or cause demonstrable harm. They bear the cost of legal challenges, potential restrictions, or chilling effects, as their speech is subject to case-by-case scrutiny.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, controversial_speakers, payer,
    moderate, immediate, constrained, local).

% Groups and individuals who prioritize public safety, social cohesion, and the protection of vulnerable communities from harmful speech. They benefit from the balancing approach as it allows for restrictions on speech that might otherwise be protected under more absolutist interpretations.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, public_order_advocates, beneficiary,
    organized, biographical, mobile, national).

% Groups whose speech, often critical of dominant power structures, is frequently challenged or restricted under the balancing framework due to its perceived harm or offense. Their identity is often tied to their expressive acts, making exit from speech difficult.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginalized_speech_groups, payer,
    powerless, generational, identity_locked, national).

% Advocates for a near-absolute protection of speech, believing that any balancing test inherently undermines the First Amendment. They are structurally excluded from the core interpretive framework of the balancing reading, as their premise is rejected by its very operation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_speech_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free speech with other fundamental constitutional rights and societal interests, preventing speech from becoming an absolute right that overrides all other values. It provides a framework for judicial review of speech regulations.
% TRANSFER_FUNCTION: Transfers interpretive authority over the boundaries of protected speech from categorical rules to the judiciary, allowing for a flexible, context-dependent determination. It also transfers the burden of justifying speech to speakers when it conflicts with other values.
% ABSENT_VOICES: Advocates for categorical speech protection (absolutist reading) are structurally marginalized in this framework, as their core premise of minimal exceptions is rejected by the balancing act itself. They would argue that the balancing test is inherently prone to suppressing unpopular or challenging speech.
% DISAPPEARANCE_RATIONALE: If the balancing framework vanished, the legal landscape of speech would become highly unstable. Either an absolutist approach would prevail (leading to potential societal harms), or a harm-centric approach would dominate (leading to potential over-regulation of speech), fundamentally altering the relationship between speech and other rights.
% FOUNDING_PROBLEM: To reconcile the broad protection of free speech with the need to protect other fundamental rights and prevent demonstrable societal harms, avoiding both unchecked expression and arbitrary censorship.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the ideological spectrum, as well as various civil rights organizations and public safety advocates, corroborate the ongoing tension between speech and other rights, affirming the problem's continued relevance, even if they disagree on the optimal solution.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate because while it allows for some speech restriction, it also provides a mechanism for protecting speech. Suppression (0.30) is present due to the active enforcement by the judiciary and the chilling effect on speakers who fear their speech might be deemed harmful. Theater ratio (0.10) is low, as the balancing act is a genuine, if complex, judicial function, not primarily performative. The increasing extractiveness and suppression over time reflect the growing complexity of speech issues and the judiciary's expanding role in defining its limits.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary and legitimate coordination mechanism for a complex society. From the perspective of controversial speakers, it can feel like an arbitrary and extractive system that suppresses unpopular views. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and public order advocates are beneficiaries, as this reading grants the judiciary significant power and allows for the protection of other values. Controversial speakers and marginalized speech groups are payers, as their speech is often the target of restriction under this framework. Absolutist speech advocates are excluded, as their core premise is incompatible with the balancing approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_rule_of_law,
    'Does the case-by-case balancing approach lead to excessive judicial discretion, undermining the predictability and rule of law in speech regulation?',
    'Empirical analysis of judicial outcomes over time, assessing consistency across similar cases and the clarity of legal standards articulated by courts.',
    'If discretion is found to be excessive, it would suggest higher effective extraction from speakers due to uncertainty, potentially reclassifying the constraint closer to a Snare for those seats. If consistent, it would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_rule_of_law, empirical, 'Ambiguity regarding the extent of judicial discretion in balancing speech interests.').

omega_variable(
    balancing_vs_absolutism_framing,
    'Is the balancing approach a necessary evolution of First Amendment jurisprudence, or a fundamental departure from its core principles?',
    'Conceptual analysis of constitutional theory and historical intent, alongside a comparative study of free speech regimes in other democracies.',
    'If framed as a fundamental departure, it would strengthen the ''excluded'' status of absolutist advocates and highlight the conceptual extraction inherent in the balancing framework. If framed as evolution, it would reinforce its legitimacy as a coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_vs_absolutism_framing, conceptual, 'Conceptual debate over the legitimacy and philosophical grounding of the balancing test.').

omega_variable(
    systemic_harm_measurement,
    'How reliably and objectively can ''demonstrated harms'' be measured and attributed to specific speech acts, especially in cases of systemic or cumulative harm?',
    'Development of robust social science methodologies for measuring the impact of speech, and legal standards for evidentiary thresholds in speech-harm cases.',
    'If harms are difficult to measure, it could lead to arbitrary restrictions (higher suppression) or a chilling effect on speech, pushing the constraint towards a Snare for targeted speakers. If measurable, it strengthens the legitimacy of the balancing act.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_harm_measurement, empirical, 'Uncertainty in measuring and attributing systemic harms from speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1940, speech_protection_boundary__balancing_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_boundary__balancing_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__balancing_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__balancing_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__balancing_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1940, speech_protection_boundary__balancing_reading, base_extractiveness, 1940, 0.3).
narrative_ontology:measurement(spee_be_t1960, speech_protection_boundary__balancing_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__balancing_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__balancing_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__balancing_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1940, speech_protection_boundary__balancing_reading, suppression_requirement, 1940, 0.2).
narrative_ontology:measurement(spee_su_t1960, speech_protection_boundary__balancing_reading, suppression_requirement, 1960, 0.22).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__balancing_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__balancing_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__balancing_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_boundary' kernel. It is linked to the 'absolutist_reading' and 'harm_limited_reading' as sibling interpretations of the same constitutional principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
