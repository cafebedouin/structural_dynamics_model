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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Harm Boundary (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of the speech harm
 *   boundary kernel. It asserts that speech protection is subordinate to
 *   human dignity, and personhood-denying speech (e.g., Holocaust denial,
 *   hate speech, group defamation) is categorically unprotected. This reading
 *   prioritizes the protection of vulnerable groups from identity-based harm,
 *   leading to significant restrictions on certain forms of expression. The
 *   high extractiveness reflects the severe limitation on speakers of
 *   identity-harming speech, while the high suppression indicates the active
 *   enforcement required to maintain these boundaries.
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
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Harm Boundary (Dignity Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '11c78043-9237-4617-99e0-b784986d7246').
narrative_ontology:cs_kernel_codification('11c78043-9237-4617-99e0-b784986d7246', formalized).
narrative_ontology:cs_authority_grounding('11c78043-9237-4617-99e0-b784986d7246', lineage).
narrative_ontology:cs_interpretation_layer_present('11c78043-9237-4617-99e0-b784986d7246').
narrative_ontology:cs_reading_relation('11c78043-9237-4617-99e0-b784986d7246', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('11c78043-9237-4617-99e0-b784986d7246', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('11c78043-9237-4617-99e0-b784986d7246', foundational, human_dignity_is_foundational_limit_on_speech).
narrative_ontology:cs_axiom_status(human_dignity_is_foundational_limit_on_speech, holdable).
narrative_ontology:cs_axiom_grounding('11c78043-9237-4617-99e0-b784986d7246', human_dignity_is_foundational_limit_on_speech, deontological).
narrative_ontology:cs_axiom('11c78043-9237-4617-99e0-b784986d7246', foundational, personhood_denying_speech_lacks_value).
narrative_ontology:cs_axiom_status(personhood_denying_speech_lacks_value, holdable).
narrative_ontology:cs_axiom_grounding('11c78043-9237-4617-99e0-b784986d7246', personhood_denying_speech_lacks_value, conventional).
narrative_ontology:cs_reference_frame('11c78043-9237-4617-99e0-b784986d7246', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('11c78043-9237-4617-99e0-b784986d7246', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('11c78043-9237-4617-99e0-b784986d7246', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, state_institutions).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, groups_seeking_unrestricted_expression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are protected from speech that denies their personhood or incites hatred against them. They benefit from the legal and social mechanisms that suppress such speech, experiencing a reduction in harm and an affirmation of their dignity. Their exit options are constrained by the persistence of hate speech in other jurisdictions or online.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% These individuals or groups bear the direct costs of this constraint, facing legal penalties, social ostracization, or platform deplatforming for speech deemed to violate human dignity. Their ability to express certain views is severely restricted, and alternatives for reaching an audience are suppressed.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_hate_speech, payer,
    moderate, immediate, constrained, local).

% These institutions (courts, legislatures, regulatory bodies) define, interpret, and enforce the boundaries of protected speech, prioritizing human dignity. They actively suppress speech that falls outside these boundaries, balancing the right to free expression with the protection of vulnerable populations. They benefit from maintaining social cohesion and order.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, state_institutions, agenda_setter,
    institutional, generational, analytical, national).

% These groups advocate for broader interpretations of free speech, often clashing with the dignity-based limitations. They experience the constraint as an infringement on fundamental rights and bear the costs of legal challenges and public debate. Their exit options are limited to seeking legal reform or operating in less restrictive online spaces.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, groups_seeking_unrestricted_expression, payer,
    organized, biographical, constrained, national).

% These bodies monitor and advocate for human rights, including both freedom of expression and protection from discrimination. They provide a global framework that often supports the dignity-based reading, influencing national legal systems through conventions and recommendations.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding of the limits of free speech, ensuring that expression does not undermine the fundamental dignity and equality of all persons, thereby fostering a more inclusive and respectful public discourse.
% TRANSFER_FUNCTION: Transfers the burden of speech regulation from vulnerable individuals (who would otherwise bear the harm of personhood-denying speech) to speakers whose expression is deemed to violate human dignity, by restricting their ability to disseminate such speech.
% ABSENT_VOICES: Those who believe in an absolute right to free speech, even for hateful content, are often marginalized in this discourse. They would argue that any restriction, even for dignity, sets a dangerous precedent for censorship, but their arguments are often dismissed as prioritizing abstract liberty over concrete harm.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, there would be an immediate surge in personhood-denying speech, leading to increased harm for vulnerable groups, erosion of social cohesion, and a breakdown of trust in public institutions to protect fundamental rights. Legal and social systems would have to rapidly re-establish new boundaries or face significant societal fragmentation.
% FOUNDING_PROBLEM: The problem of speech that directly attacks the personhood and dignity of individuals or groups, leading to discrimination, violence, and the erosion of fundamental human rights, particularly in the aftermath of historical atrocities.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, the lived experience of vulnerable communities, and historical evidence of the impact of hate speech corroborate that this problem remains live. Legal scholars and civil society organizations outside of state institutions and vulnerable groups also attest to its ongoing relevance.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.85) because the constraint imposes categorical prohibitions on certain speech, representing a significant cost to those who wish to express such views. Suppression is also high (0.75) due to the active legal and social enforcement mechanisms required to identify, prosecute, and remove dignity-violating speech. The theater ratio is low (0.1) because the enforcement is generally seen as genuine and effective in its stated purpose of protecting dignity, not merely performative. Accessibility collapse is moderate (0.6) as alternative forms of expression exist, but not for the specific content deemed harmful. Resistance is high (0.7) from groups advocating for broader speech rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups, this constraint is a necessary protection, a 'rope' or even a 'mountain' of fundamental human rights. From the perspective of those whose speech is suppressed, it is a 'snare' or 'tangled rope' that unjustly curtails freedom. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups and state institutions are beneficiaries, as the constraint protects the former and empowers the latter to maintain social order. Speakers of hate speech and groups seeking unrestricted expression are victims, as their expressive freedom is curtailed. The directionality for victims is high, reflecting the direct and substantial costs they bear.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_dignity_violation,
    'What specific types of speech constitute a ''dignity violation'' and how are these categories defined and applied consistently across different contexts and evolving social norms?',
    'Development of clear, judicially consistent, and socially accepted definitions and precedents for ''dignity-violating speech'' through case law and legislative action, with input from affected communities.',
    'If the scope is too broad or inconsistently applied, the constraint risks over-suppression and could be reclassified as a ''snare'' due to arbitrary enforcement. If too narrow, it risks failing to protect vulnerable groups, weakening its ''rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_dignity_violation, conceptual, 'Ambiguity in defining the boundaries of ''dignity-violating speech''.').

omega_variable(
    slippery_slope_to_censorship,
    'Does the categorical exclusion of dignity-violating speech inevitably lead to a ''slippery slope'' where other forms of speech are gradually suppressed, beyond the initial intent?',
    'Longitudinal empirical studies tracking the evolution of speech restrictions in jurisdictions adopting this reading, comparing them to jurisdictions with more absolutist or harm-balancing approaches.',
    'If a slippery slope is empirically demonstrated, the constraint''s long-term extractiveness and suppression would be higher than currently measured, potentially shifting its classification towards a ''snare'' or ''tangled rope'' for a broader range of speakers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_to_censorship, empirical, 'Risk of overreach and unintended suppression of other speech categories.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (legal penalties, platform removal) or does it also include internalized self-censorship by speakers who fear misinterpretation or social backlash?',
    'Qualitative research (interviews, surveys) with speakers and content creators to assess their perceptions of risk and self-censorship behaviors, particularly in ambiguous cases.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as speakers carry the suppression with them even in spaces where formal enforcement is absent. This could amplify the perceived extractiveness for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t5, speech_harm_boundary__dignity_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(spee_tr_t15, speech_harm_boundary__dignity_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__dignity_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(spee_be_t5, speech_harm_boundary__dignity_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(spee_be_t15, speech_harm_boundary__dignity_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__dignity_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t5, speech_harm_boundary__dignity_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(spee_su_t15, speech_harm_boundary__dignity_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__dignity_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, hate_speech_legislation).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, online_content_moderation_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_harm_boundary' kernel. It focuses on the categorical subordination of speech to human dignity, distinct from absolutist or harm-balancing approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
