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
 *   boundary kernel, where the protection of human dignity is a foundational
 *   principle that subordinates and limits free speech. Speech that denies
 *   personhood or incites hatred is categorically unprotected. This reading
 *   emphasizes the protection of vulnerable groups from identity-harming
 *   speech, leading to high extraction from speakers of such speech and
 *   active enforcement by state institutions. The claimed type is
 *   'tangled_rope' because it coordinates social order and protects dignity
 *   (beneficiary function) but does so through asymmetric extraction from
 *   certain speakers (victim function) requiring active enforcement.
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
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, 'e62c5992-2366-4a10-9aa7-52fa0f2a3d75').
narrative_ontology:cs_kernel_codification('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', formalized).
narrative_ontology:cs_authority_grounding('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', lineage).
narrative_ontology:cs_interpretation_layer_present('e62c5992-2366-4a10-9aa7-52fa0f2a3d75').
narrative_ontology:cs_reading_relation('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', foundational, human_dignity_is_foundational).
narrative_ontology:cs_axiom_status(human_dignity_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', human_dignity_is_foundational, deontological).
narrative_ontology:cs_axiom('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', foundational, personhood_denial_is_categorical_harm).
narrative_ontology:cs_axiom_status(personhood_denial_is_categorical_harm, holdable).
narrative_ontology:cs_axiom_grounding('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', personhood_denial_is_categorical_harm, deontological).
narrative_ontology:cs_reference_frame('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e62c5992-2366-4a10-9aa7-52fa0f2a3d75', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, state_institutions).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, groups_seeking_to_deny_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are protected from speech that denies their personhood or incites hatred against them. The constraint provides a legal and social shield, affirming their dignity. Exit from this protection would mean re-exposure to identity-harming speech, which is not a viable option.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, vulnerable_groups, beneficiary,
    powerless, generational, identity_locked, national).

% These institutions (courts, legislatures, regulatory bodies) interpret and enforce the dignity-based limits on speech. They benefit from maintaining social order and protecting vulnerable populations, which aligns with their mandate. Their exit options are constrained by constitutional duties and international human rights obligations.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% These individuals or groups bear the direct costs of the constraint through legal penalties, censorship, or social ostracization for speech deemed to deny human dignity. Their speech is categorically unprotected, meaning they have no legal recourse to defend it under this reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_hate_speech, payer,
    moderate, immediate, constrained, local).

% Organized movements or ideologies that promote personhood-denying narratives (e.g., Holocaust denial, white supremacy) find their core expressive acts suppressed. They face legal and social barriers to disseminating their message, directly paying the cost of the dignity principle.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, groups_seeking_to_deny_dignity, payer,
    organized, biographical, constrained, national).

% Advocates for an absolutist view of speech protection, who believe almost all speech should be protected regardless of content, are excluded from the framing of this constraint. They would argue against any categorical exclusions based on dignity, but their arguments are not given standing within this reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_advocates, excluded,
    moderate, biographical, mobile, national).

% These bodies monitor and evaluate national speech laws against international human rights standards, many of which explicitly permit or require restrictions on hate speech. They provide an external analytical perspective, often reinforcing the dignity-based approach.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal norms to protect the fundamental human dignity of all individuals and groups, preventing speech from being used to deny personhood or incite hatred, thereby fostering a more inclusive and respectful public sphere.
% TRANSFER_FUNCTION: Transfers the burden of speech restriction from vulnerable groups (who would otherwise bear the harm of dignity-denying speech) to speakers whose expression is deemed to violate human dignity, through legal penalties and social sanctions.
% ABSENT_VOICES: Advocates for an absolutist interpretation of free speech are structurally excluded from the conversation, as their premise of near-absolute protection is incompatible with the foundational dignity principle of this reading. They would argue that any content-based restriction is a slippery slope.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, vulnerable groups would immediately lose a critical legal and social protection against personhood-denying speech. Hate speech and incitement would proliferate, leading to increased social fragmentation, conflict, and potential violence, forcing a rapid societal reorganization to address the resulting harms.
% FOUNDING_PROBLEM: The problem of speech being used to systematically deny the personhood and dignity of vulnerable groups, leading to historical atrocities and ongoing social harm, particularly in the aftermath of events like the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, the lived experience and testimony of vulnerable groups, and historical analysis of genocides and hate movements consistently corroborate that the problem of dignity-denying speech remains live and requires active measures.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the constraint imposes severe restrictions on a category of speech, effectively extracting the right to express certain views. Suppression is also high (0.75) due to the active legal and social enforcement mechanisms required to uphold these categorical exclusions. The theater ratio is low (0.1) as the enforcement is generally genuine and directly aimed at the stated goal of protecting dignity, not merely performative. Accessibility collapse is moderate (0.6) as alternative forms of expression exist, but the specific avenue of dignity-denying speech is closed. Resistance is high (0.7) from those who advocate for broader speech protections or wish to engage in the restricted speech.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups and state institutions, this constraint is a necessary and just mechanism for social coordination and protection. From the perspective of speakers of hate speech and absolutist advocates, it is a highly extractive and suppressive mechanism that infringes on fundamental freedoms. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups are clear beneficiaries (d=0.0) as the constraint directly protects their dignity. State institutions are agenda-setters and beneficiaries (d=0.15) as they uphold their mandate for social order. Speakers of hate speech and groups seeking to deny dignity are clear targets (d=1.0) as their speech is directly suppressed and penalized. Absolutist advocates are excluded (d=0.8) as their framing is not recognized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_dignity_violation,
    'What specific types of speech constitute a ''dignity violation'' or ''personhood denial'' in practice, and how consistently is this applied across different contexts and groups?',
    'Detailed jurisprudential analysis of case law and legislative definitions across multiple jurisdictions, coupled with empirical studies on the impact of different speech types on group dignity.',
    'If the scope is inconsistently applied or overbroad, the constraint''s effective extractiveness and suppression could be higher than intended, potentially impacting speech not directly related to dignity denial. If too narrow, it may fail to protect vulnerable groups adequately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_dignity_violation, empirical, 'Ambiguity in the practical definition and application of ''dignity-violating speech''.').

omega_variable(
    dignity_vs_free_expression_hierarchy,
    'Is the subordination of free expression to human dignity a universally accepted normative hierarchy, or is it a contested philosophical choice?',
    'Comparative analysis of constitutional frameworks and philosophical traditions globally, examining the foundational principles of different legal systems and their explicit or implicit hierarchies of rights.',
    'If it''s a contested choice, the ''dignity_reading'' is a preference-based constraint, not a natural law, and its persistence depends on ongoing normative consensus or enforcement. If universal, it moves closer to a ''mountain'' of political philosophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_vs_free_expression_hierarchy, conceptual, 'The philosophical grounding of the dignity-speech hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(spee_be_t1948, speech_harm_boundary__dignity_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(spee_be_t1970, speech_harm_boundary__dignity_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__dignity_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__dignity_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__dignity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1948, speech_harm_boundary__dignity_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(spee_su_t1970, speech_harm_boundary__dignity_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__dignity_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__dignity_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__dignity_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_harm_boundary' kernel. It focuses on the subordination of speech to human dignity, contrasting with absolutist and harm-balancing approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
