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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary (Balancing Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint describes the 'balancing' reading of the First
 *   Amendment's speech protection boundary, where the scope of protected
 *   speech is determined through a case-by-case weighing of First Amendment
 *   interests against other constitutional values and demonstrated harms.
 *   This approach contrasts with absolutist views (minimal exceptions) and
 *   harm-limited views (broader exceptions for dignity/equality harms). The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinates
 *   conflicting values but does so through an asymmetric process that
 *   extracts from certain speakers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.75).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary (Balancing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '5b6be70f-941e-44bd-9d1c-d11b2d886eb4').
narrative_ontology:cs_kernel_codification('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', formalized).
narrative_ontology:cs_authority_grounding('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', lineage).
narrative_ontology:cs_interpretation_layer_present('5b6be70f-941e-44bd-9d1c-d11b2d886eb4').
narrative_ontology:cs_reading_relation('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', speech_is_not_absolute, conventional).
narrative_ontology:cs_axiom('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', foundational, constitutional_values_interact).
narrative_ontology:cs_axiom_status(constitutional_values_interact, holdable).
narrative_ontology:cs_axiom_grounding('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', constitutional_values_interact, deontological).
narrative_ontology:cs_reference_frame('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', ad_hoc_balancing_framework).
narrative_ontology:cs_drift_state('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b6be70f-941e-44bd-9d1c-d11b2d886eb4', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, public_order_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, unrestricted_speech_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_of_contested_speech).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, constitutional_balancing_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, judicial_review_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for interpreting the First Amendment and applying balancing tests to specific cases. They define the boundaries of protected speech, weighing it against other constitutional values and demonstrated harms. This role grants them significant authority in shaping public discourse.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for maximal speech protection, often viewing any limitation as an infringement on fundamental rights. They bear the cost of speech restrictions imposed by balancing tests, experiencing their speech as 'extracted' or suppressed when it falls outside the protected zone.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, unrestricted_speech_advocates, payer,
    organized, biographical, constrained, national).

% Champion the need for social cohesion, safety, and the protection of other rights. Their interests are explicitly considered and often upheld through the balancing framework, leading to the suppression of speech deemed harmful or disruptive.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, public_order_advocates, beneficiary,
    organized, biographical, mobile, national).

% Benefit from the balancing approach when it leads to the protection against harms like hate speech, harassment, or incitement to violence. The constraint provides a mechanism for their safety and dignity to be considered alongside speech rights.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, vulnerable_groups, beneficiary,
    powerless, generational, constrained, national).

% Individuals or groups whose speech is subject to the balancing test and may be restricted. They face legal challenges, potential penalties, and the chilling effect of uncertainty regarding the protected status of their expression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_of_contested_speech, payer,
    moderate, immediate, constrained, local).

% Proponents of a near-absolute interpretation of the First Amendment, who argue against any balancing of speech rights. Their theoretical framework is largely excluded from the operational application of the balancing test by the judiciary.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_scholars, excluded,
    analytical, generational, analytical, national).

% Academics and legal experts who analyze the nature and impact of speech-related harms. While their work informs the judiciary's understanding of harms, they do not directly apply the balancing test in a judicial capacity.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, harm_theorists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To reconcile the First Amendment's broad protection of speech with the need to maintain public order, protect individual rights, and prevent societal harms, particularly in complex and evolving social contexts. It coordinates conflicting constitutional values and social interests.
% TRANSFER_FUNCTION: Transfers the authority to define the limits of protected speech to the judiciary, along with the burden of proof for demonstrating harm. It also transfers the risk of suppression to speakers whose expression falls outside the judicially determined boundaries.
% ABSENT_VOICES: Absolutist proponents of speech rights are largely absent from the direct application of this balancing framework, as their core premise rejects the very act of weighing. Those whose speech is consistently suppressed by the balancing test also lack a direct voice in shaping the framework.
% DISAPPEARANCE_RATIONALE: If the balancing framework vanished overnight, the legal landscape of speech would become highly unstable. Either speech would become near-absolute, leading to significant social disruption and harm, or it would be subject to arbitrary restrictions without a clear constitutional standard. The judiciary's role in mediating these conflicts would be fundamentally altered, and society would have to rapidly reorganize its approach to free expression.
% FOUNDING_PROBLEM: Early mobile software distribution was fragmented and unsafe: no trusted install path, no reliable payment rail for small developers, malware risk on every download.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and public safety officials from diverse perspectives attest to the ongoing challenge of balancing these interests, often disagreeing on the specific outcomes but acknowledging the underlying tension. This corroboration comes from outside the direct beneficiaries of the judicial system's authority.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the balancing test inherently limits some speech, which is experienced as a cost by those whose expression is curtailed. Suppression is high (0.75) as the judiciary actively enforces these limits, requiring speakers to navigate complex legal standards. The theater ratio is low (0.2) because the judicial process of weighing interests is a genuine, complex, and often transparent function, not primarily performative. Accessibility collapse is moderate (0.6) as alternatives (unrestricted speech) are significantly constrained but not entirely eliminated. Resistance is high (0.7) due to ongoing legal challenges and public debate from advocates of broader speech rights.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and public order advocates perceive this constraint as a necessary and legitimate mechanism for maintaining social order and protecting rights. In contrast, unrestricted speech advocates and speakers of contested speech experience it as an extractive and suppressive force that chills legitimate expression. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from maintaining its authority to define speech boundaries (low directionality). Public order advocates and vulnerable groups are beneficiaries as their interests are protected (low directionality). Unrestricted speech advocates and speakers of contested speech are targets, bearing the costs of speech limitations (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to balance competing values remains live and highly contested, preventing it from becoming a Piton. The ongoing resistance and the 'live' status of the founding problem indicate that its function has not atrophied, even if its application is debated. The classification as a Tangled Rope acknowledges both its coordination function and its asymmetric extraction, preventing mislabeling as pure extraction (Snare) or pure coordination (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_objectivity,
    'Is the judicial balancing test applied objectively, or is it susceptible to political and social biases that disproportionately suppress certain viewpoints?',
    'Empirical analysis of judicial outcomes across different political climates and types of speech, comparing stated rationales with observed patterns of suppression.',
    'If biased, the effective extractiveness and suppression are higher than the stated metrics suggest, as the constraint functions as a tool for selective enforcement rather than neutral arbitration. This would push the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_objectivity, empirical, 'Assesses the neutrality and fairness of the balancing test''s application.').

omega_variable(
    definition_of_harm_ambiguity,
    'How consistently and clearly is ''demonstrated harm'' defined and applied across different cases and judicial circuits?',
    'Comparative legal analysis of case law, identifying variations in the interpretation and evidentiary requirements for harm, and their impact on speech outcomes.',
    'Inconsistent or vague definitions of harm increase uncertainty for speakers, leading to a greater chilling effect and higher effective suppression, even if not explicitly measured. This ambiguity could also be leveraged for greater extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_harm_ambiguity, conceptual, 'Examines the clarity and consistency of ''harm'' definition in balancing tests.').

omega_variable(
    judicial_discretion_scope,
    'To what extent does judicial discretion in applying the balancing test allow for arbitrary or unpredictable outcomes, rather than principled legal reasoning?',
    'Analysis of dissenting opinions, legal scholarship critiquing specific judicial decisions, and studies on the predictability of speech outcomes based on judicial panel composition.',
    'High levels of arbitrary discretion would undermine the coordination function, making the constraint less like a Rope and more like a Snare, as its persistence would depend more on the power of individual judges than on a stable legal framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_discretion_scope, empirical, 'Evaluates the role of judicial discretion in balancing test outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, protest_rights).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, online_content_moderation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_protection_boundary' kernel. It represents the 'balancing' approach, which directly influences and is influenced by other interpretations of speech rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
