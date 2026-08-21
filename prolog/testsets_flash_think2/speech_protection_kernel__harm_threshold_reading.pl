% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Harm Threshold
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'harm_threshold_reading' of the broader
 *   'speech_protection_kernel'. It posits that speech protection is not
 *   absolute but conditional on the absence of demonstrable harm to victims.
 *   This reading emphasizes the balancing of speaker autonomy against the
 *   need for social order and individual safety. The constraint is actively
 *   enforced through legal and judicial mechanisms, leading to a narrower
 *   scope of protected speech compared to more absolutist interpretations.
 *   The metrics reflect a system that is substantially extractive from
 *   speakers whose speech is deemed harmful, and requires significant
 *   suppression to maintain its boundaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.7).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.8).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Harm Threshold").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '37a66bda-e2df-4222-b805-23b0a674a430').
narrative_ontology:cs_kernel_codification('37a66bda-e2df-4222-b805-23b0a674a430', fixed_text).
narrative_ontology:cs_authority_grounding('37a66bda-e2df-4222-b805-23b0a674a430', lineage).
narrative_ontology:cs_interpretation_layer_present('37a66bda-e2df-4222-b805-23b0a674a430').
narrative_ontology:cs_reading_relation('37a66bda-e2df-4222-b805-23b0a674a430', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('37a66bda-e2df-4222-b805-23b0a674a430', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('37a66bda-e2df-4222-b805-23b0a674a430', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('37a66bda-e2df-4222-b805-23b0a674a430', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('37a66bda-e2df-4222-b805-23b0a674a430', foundational, harm_principle_supremacy).
narrative_ontology:cs_axiom_status(harm_principle_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('37a66bda-e2df-4222-b805-23b0a674a430', harm_principle_supremacy, deontological).
narrative_ontology:cs_axiom('37a66bda-e2df-4222-b805-23b0a674a430', secondary, balancing_of_rights).
narrative_ontology:cs_axiom_status(balancing_of_rights, holdable).
narrative_ontology:cs_axiom_grounding('37a66bda-e2df-4222-b805-23b0a674a430', balancing_of_rights, conventional).
narrative_ontology:cs_reference_frame('37a66bda-e2df-4222-b805-23b0a674a430', balancing_of_rights_tradition).
narrative_ontology:cs_drift_state('37a66bda-e2df-4222-b805-23b0a674a430', contemporary_digital_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('37a66bda-e2df-4222-b805-23b0a674a430', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, society_at_large).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, potential_victims_of_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_of_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, free_speech_advocates).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, social_responsibility_of_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the harm threshold, defining what constitutes 'demonstrable harm' and balancing it against free speech claims. Their decisions set precedents that shape the constraint's boundaries.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Enacts statutes that codify or refine categories of speech deemed harmful (e.g., hate speech laws, defamation laws), thereby establishing the legal framework for the harm threshold.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from a more orderly public discourse and protection from various forms of speech-induced harm, contributing to social cohesion and safety. This is an abstract beneficiary representing the collective good.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, society_at_large, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_kernel__harm_threshold_reading, society_at_large).

% Receives protection from speech that causes demonstrable harm, such as incitement to violence, defamation, or harassment. Their claims of harm are central to triggering the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, potential_victims_of_speech, beneficiary,
    powerless, immediate, trapped, local).

% Bears the cost of restriction, censorship, or legal penalties when their speech is deemed to cross the harm threshold. Their autonomy to express certain views is curtailed.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, global).

% Actively challenge the scope and application of the harm threshold, arguing for broader speech protections. They bear the costs of litigation and public advocacy against restrictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, free_speech_advocates, payer,
    organized, biographical, mobile, national).

% Their position, which rejects harm as a legitimate basis for speech restriction, is largely outside the mainstream legal and philosophical discourse that shapes this constraint. They are excluded from the core debate on balancing.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_free_speech_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the evolution and application of the harm threshold, critiquing its definitions, impacts, and consistency across different contexts. They do not directly benefit or pay but inform the debate.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, society_at_large).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the fundamental right to free expression with the imperative to protect individuals and society from demonstrable harm caused by speech, thereby fostering a civil and safe public sphere.
% TRANSFER_FUNCTION: Transfers a degree of expressive autonomy from speakers to potential victims and society, in exchange for protection from speech-induced harm and the maintenance of public order.
% ABSENT_VOICES: Absolutist free speech advocates, who would argue that any restriction based on harm is a dangerous precedent, are largely excluded from the legal and policy-making processes that define and apply this constraint.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished overnight, there would be a rapid increase in speech causing direct and severe harm (e.g., incitement, defamation, harassment), leading to social fragmentation, increased conflict, and a breakdown of public trust. Society would be forced to reorganize to address these harms, likely through informal or extra-legal means.
% FOUNDING_PROBLEM: Unfettered speech, particularly in contexts of incitement, defamation, and harassment, historically led to direct and severe harm to individuals and public order, necessitating a mechanism to balance rights and responsibilities.
% FOUNDING_PROBLEM_CORROBORATION: Victims' rights organizations, public safety advocates, and international human rights bodies consistently attest that the problem of speech-induced harm remains live, particularly with the rise of digital platforms. This corroboration comes from outside the direct beneficiaries of speech restriction.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.7) is high because speakers face real costs (legal penalties, censorship) when their speech is found to cause harm. Suppression (0.8) is also high, reflecting the active and often coercive legal and social enforcement mechanisms required to identify, adjudicate, and restrict harmful speech. The theater ratio (0.4) indicates that while there is genuine intent to prevent harm, a significant portion of the activity involves performative balancing tests and definitional disputes that may not directly reduce harm but serve to legitimize the constraint. Accessibility collapse (0.7) is high as alternatives (unrestricted speech) are significantly curtailed for certain categories, and resistance (0.6) is moderate-high due to ongoing challenges from free speech advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of potential victims and society at large, this constraint is a necessary coordination mechanism that protects vulnerable groups and maintains public order. From the perspective of speakers of potentially harmful speech and free speech advocates, it is an extractive and suppressive mechanism that curtails fundamental rights and risks a 'slippery slope' of censorship. The judiciary, as the agenda-setter, attempts to navigate this gap through balancing tests, but the inherent tension remains.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legislature, as agenda-setters, define and enforce the constraint. Society at large and potential victims of speech are the primary beneficiaries, receiving protection from harm. Speakers of harmful speech and free speech advocates are the payers/targets, bearing the costs of restriction and legal challenges. Absolutist free speech advocates are excluded, as their core premise is incompatible with this reading's foundation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent demonstrable harm remains live and is arguably more critical in the digital age. The classification as a Tangled Rope acknowledges both its genuine coordination function (protecting from harm, maintaining public order) and its asymmetric extraction (from speakers). This prevents mislabeling it as pure extraction (Snare) by recognizing the societal benefit, while also preventing it from being seen as pure coordination (Rope) by acknowledging the coercive and extractive elements. The ongoing contestation over the definition of 'harm' and the scope of 'speech' indicates that the constraint is actively maintained and contested, not atrophied into a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_harm_threshold,
    'Is this constraint a genuine balancing mechanism or primarily a tool for suppressing disfavored speech?',
    'Analysis of judicial outcomes over time: if restrictions disproportionately target marginalized groups or political dissent without clear, consistent harm, it leans towards suppression. If applied consistently across diverse speech, it leans towards balancing.',
    'If primarily suppression, the effective extractiveness and suppression metrics are higher than currently estimated, and the constraint leans towards a Snare classification. If a genuine balancing act, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_harm_threshold, conceptual, 'This constraint is the ''harm_threshold_reading'' of the ''speech_protection_kernel''.').

omega_variable(
    absolutist_reading_delta,
    'How would the constraint''s structure and classification change under an ''absolutist_reading'' of speech protection?',
    'Counterfactual analysis: an absolutist reading would eliminate the harm condition, leading to significantly lower extractiveness and suppression for speakers, but potentially higher societal costs from unchecked harmful speech. This would shift the constraint towards a Rope or even Mountain for speakers, but create new, unaddressed harms for victims.',
    'The ''absolutist_reading'' would fundamentally alter the beneficiary/victim structure, likely eliminating ''speakers_of_harmful_speech'' as victims of the constraint, but creating ''victims_of_unrestricted_speech'' as a new class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_reading_delta, conceptual, 'Structural delta if an absolutist reading were adopted.').

omega_variable(
    definition_of_harm_ambiguity,
    'Is the definition of ''demonstrable harm'' sufficiently objective and consistent, or is it subject to arbitrary interpretation and political influence?',
    'Empirical study of judicial consistency across different types of speech and political contexts. Analysis of legislative intent and judicial reasoning for defining new categories of harm.',
    'If subjective and inconsistent, the constraint''s effective suppression and extractiveness are higher due to unpredictability, and its legitimacy as a fair balancing act is undermined. If objective, the constraint functions more reliably as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_harm_ambiguity, empirical, 'Contestation over the objectivity and consistency of ''demonstrable harm''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(spee_tr_t1970, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(spee_be_t1970, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(spee_be_t1990, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(spee_be_t2010, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(spee_su_t1970, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(spee_su_t1990, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(spee_su_t2010, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, hate_speech_laws).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, defamation_laws).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, incitement_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel'. Other readings (absolutist, marketplace, dignity, democratic_participation) represent alternative structural claims about speech protection and are modeled as separate, linked constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
