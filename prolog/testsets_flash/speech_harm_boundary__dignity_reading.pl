% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Protection Subordinate to Human Dignity (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'dignity_reading' of the
 *   'speech_harm_boundary' kernel, where the protection of human dignity is
 *   paramount, and speech that denies personhood (e.g., Holocaust denial,
 *   hate speech, group defamation) is categorically unprotected. This reading
 *   prioritizes the protection of vulnerable groups from identity-harming
 *   speech, leading to significant restrictions on speakers of such content.
 *   The constraint is actively enforced through legal and regulatory
 *   mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.85).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.75).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Protection Subordinate to Human Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, 'e1912238-7dff-4495-9b74-22ee9406b50d').
narrative_ontology:cs_kernel_codification('e1912238-7dff-4495-9b74-22ee9406b50d', formalized).
narrative_ontology:cs_authority_grounding('e1912238-7dff-4495-9b74-22ee9406b50d', lineage).
narrative_ontology:cs_interpretation_layer_present('e1912238-7dff-4495-9b74-22ee9406b50d').
narrative_ontology:cs_reading_relation('e1912238-7dff-4495-9b74-22ee9406b50d', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e1912238-7dff-4495-9b74-22ee9406b50d', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('e1912238-7dff-4495-9b74-22ee9406b50d', foundational, human_dignity_is_foundational).
narrative_ontology:cs_axiom_status(human_dignity_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('e1912238-7dff-4495-9b74-22ee9406b50d', human_dignity_is_foundational, deontological).
narrative_ontology:cs_axiom('e1912238-7dff-4495-9b74-22ee9406b50d', foundational, personhood_denying_speech_has_no_value).
narrative_ontology:cs_axiom_status(personhood_denying_speech_has_no_value, holdable).
narrative_ontology:cs_axiom_grounding('e1912238-7dff-4495-9b74-22ee9406b50d', personhood_denying_speech_has_no_value, deontological).
narrative_ontology:cs_reference_frame('e1912238-7dff-4495-9b74-22ee9406b50d', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('e1912238-7dff-4495-9b74-22ee9406b50d', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e1912238-7dff-4495-9b74-22ee9406b50d', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, human_dignity_advocates).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, holocaust_deniers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, group_defamers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are directly protected from speech that denies their personhood or incites hatred against them. Their dignity and safety are prioritized, but they remain vulnerable to other forms of discrimination and may still experience harm from speech that falls outside the categorical exclusions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% These actors champion the principle of human dignity as a foundational value, advocating for legal and social frameworks that prioritize it over unfettered speech. They benefit from the constraint's existence as it aligns with their core mission and provides a legal basis for their advocacy.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, human_dignity_advocates, beneficiary,
    institutional, civilizational, analytical, global).

% Individuals or groups who engage in speech deemed to deny personhood or incite hatred. They bear the direct costs of this constraint through legal penalties, censorship, and social ostracization. Their options are to cease such speech, face legal consequences, or attempt to find platforms outside the regulated sphere.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_hate_speech, payer,
    moderate, immediate, trapped, local).

% Individuals who deny historical facts, particularly the Holocaust, which is often considered a form of personhood-denying speech. They face legal prohibitions and social condemnation, bearing significant costs for their expression. Their ability to disseminate their views is severely curtailed.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_deniers, payer,
    moderate, biographical, trapped, national).

% Those who engage in speech that systematically defames or slanders entire groups. They are subject to legal action and restrictions, incurring costs for their expression. While not always as strictly prohibited as Holocaust denial, it falls under the umbrella of dignity-violating speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, group_defamers, payer,
    moderate, biographical, constrained, national).

% These institutions define, interpret, and enforce the boundaries of dignity-protected speech. They are responsible for balancing speech rights with dignity protections, often navigating complex legal and philosophical debates. Their decisions shape the practical application and scope of the constraint.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% These advocates argue for minimal restrictions on speech, even offensive or hateful content, believing that the best response to bad speech is more speech. They are largely excluded from the framing of this constraint, which prioritizes dignity over their expansive view of speech freedom.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_speech_advocates, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a social and legal norm that human dignity is a foundational value, coordinating collective action to protect vulnerable groups from speech that denies their personhood, thereby fostering a more inclusive public sphere.
% TRANSFER_FUNCTION: Transfers the right to express certain categories of speech (personhood-denying, hate speech) from speakers to the collective, in exchange for enhanced protection of human dignity and social cohesion for vulnerable groups.
% ABSENT_VOICES: Absolutist speech advocates and those who prioritize individual expression above all else are largely excluded from the foundational premises of this constraint. They would argue that even hateful speech should be countered with more speech, not suppression, and that categorical bans are dangerous precedents.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, there would likely be an immediate increase in personhood-denying and hateful speech, leading to increased harm for vulnerable groups and a degradation of public discourse. Legal systems would need to rapidly re-evaluate their approach to speech regulation, and social norms around acceptable expression would shift dramatically.
% FOUNDING_PROBLEM: The historical and ongoing problem of speech being used to dehumanize, incite violence against, and deny the personhood of vulnerable groups, leading to profound social harm and undermining the equality and dignity of all individuals.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, historical records of genocide and discrimination, and contemporary reports from civil society organizations and academic studies consistently corroborate the ongoing problem of dignity-violating speech and its harms. This corroboration comes from sources outside the direct beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is very high because it imposes severe restrictions on certain categories of speech, effectively extracting the right to express those views from speakers. Suppression (0.75) is also high, as it requires active legal and social enforcement to identify and penalize dignity-violating speech. The theater ratio (0.1) is low, indicating that the enforcement is genuinely aimed at preventing harm, not merely performative, though the definition of 'dignity-violating' can be contested. Accessibility collapse (0.4) is moderate, as alternative forms of expression exist, but the specific content is blocked. Resistance (0.6) is substantial from those whose speech is restricted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups and human dignity advocates, this constraint is a necessary protection (beneficiary seat). From the perspective of speakers whose speech is restricted, it is a severe limitation on fundamental rights (victim seat). The agenda-setters (courts, legislatures) experience it as a complex balancing act, but one where dignity holds a categorical trump card.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups and human dignity advocates are primary beneficiaries (d near 0.0) as the constraint directly protects their personhood and well-being. Speakers of hate speech, Holocaust deniers, and group defamers are direct targets/victims (d near 1.0) as their speech is categorically suppressed. Courts and legislatures act as agenda-setters, enforcing the constraint and mediating its application.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (protecting human dignity) is considered foundational and ongoing. However, the *interpretation* of what constitutes 'dignity-violating' speech is subject to drift and contestation, which could lead to a perceived mismatch between the constraint's stated purpose and its actual application if the definition becomes overly broad or narrow. The high extractiveness and suppression are inherent to its function, not a sign of decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine instantiation of the ''dignity_reading'' of the ''speech_harm_boundary'' kernel, or is it a different constraint?',
    'Analysis of judicial precedent and legislative intent in jurisdictions adopting this framework; comparison with the ''absolutist_reading'' and ''harm_balancing_reading'' siblings.',
    'If misidentified, the classification of speech restrictions and their beneficiaries/victims would be inaccurate, leading to incorrect policy recommendations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''speech_harm_boundary'' kernel. This ''dignity_reading'' posits that speech protection is subordinate to human dignity, and personhood-denying speech is categorically unprotected. Sibling readings (''absolutist_reading'', ''harm_balancing_reading'') offer alternative frameworks for speech regulation.').

omega_variable(
    scope_of_dignity_violation,
    'What specific types of speech are considered ''personhood-denying'' or ''dignity-violating'' under this reading, and how consistently is this applied across different contexts and groups?',
    'Detailed content analysis of adjudicated cases and legislative definitions; comparative legal analysis across jurisdictions that adopt this reading.',
    'If the scope is inconsistently applied, the constraint''s extractiveness and suppression could vary significantly, potentially leading to arbitrary enforcement or chilling effects on legitimate speech. If the definition is too broad, it risks over-suppression; if too narrow, it fails to protect dignity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_dignity_violation, empirical, 'Ambiguity in defining ''personhood-denying'' speech can lead to inconsistent application of the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__dignity_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__dignity_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__dignity_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__dignity_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__dignity_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__dignity_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_harm_boundary' kernel, each representing a distinct structural approach to regulating harmful speech. This 'dignity_reading' focuses on categorical exclusions for personhood-denying speech.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
