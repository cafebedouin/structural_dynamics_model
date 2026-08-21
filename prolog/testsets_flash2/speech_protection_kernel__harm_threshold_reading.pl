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
 *   human_readable: Speech Protection Conditional on Absence of Demonstrable Harm
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents a reading of speech protection where the right
 *   to speak is explicitly conditional on the absence of demonstrable harm to
 *   others. It is one interpretation within a broader kernel of 'speech
 *   protection' that includes absolutist, marketplace, dignity, and
 *   democratic participation readings. This reading prioritizes victim
 *   protection and social cohesion, leading to a narrower scope of protected
 *   speech and active enforcement against harmful expression. The metrics
 *   reflect the ongoing tension and the active role of enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.65).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.7).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Absence of Demonstrable Harm").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '3f689d66-6d3c-4c90-b3c3-097fa686523a').
narrative_ontology:cs_kernel_codification('3f689d66-6d3c-4c90-b3c3-097fa686523a', formalized).
narrative_ontology:cs_authority_grounding('3f689d66-6d3c-4c90-b3c3-097fa686523a', lineage).
narrative_ontology:cs_interpretation_layer_present('3f689d66-6d3c-4c90-b3c3-097fa686523a').
narrative_ontology:cs_reading_relation('3f689d66-6d3c-4c90-b3c3-097fa686523a', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f689d66-6d3c-4c90-b3c3-097fa686523a', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f689d66-6d3c-4c90-b3c3-097fa686523a', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f689d66-6d3c-4c90-b3c3-097fa686523a', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('3f689d66-6d3c-4c90-b3c3-097fa686523a', foundational, harm_principle_supremacy).
narrative_ontology:cs_axiom_status(harm_principle_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3f689d66-6d3c-4c90-b3c3-097fa686523a', harm_principle_supremacy, deontological).
narrative_ontology:cs_axiom('3f689d66-6d3c-4c90-b3c3-097fa686523a', secondary, social_cohesion_imperative).
narrative_ontology:cs_axiom_status(social_cohesion_imperative, holdable).
narrative_ontology:cs_axiom_grounding('3f689d66-6d3c-4c90-b3c3-097fa686523a', social_cohesion_imperative, instrumental).
narrative_ontology:cs_reference_frame('3f689d66-6d3c-4c90-b3c3-097fa686523a', millian_harm_principle_application).
narrative_ontology:cs_drift_state('3f689d66-6d3c-4c90-b3c3-097fa686523a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3f689d66-6d3c-4c90-b3c3-097fa686523a', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, potential_victims_of_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, social_cohesion_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_of_potentially_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, advocates_of_unfettered_expression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals or groups benefit from the legal framework that allows for the restriction of speech deemed harmful, providing a mechanism for redress or prevention of harm. Their ability to exit harmful situations is enhanced by this constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, potential_victims_of_speech, beneficiary,
    organized, biographical, constrained, local).

% These speakers bear the cost of this constraint through potential legal action, censorship, or self-censorship. Their speech is subject to a harm threshold, limiting their expressive freedom. Exiting means refraining from certain types of speech or facing consequences.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_of_potentially_harmful_speech, payer,
    moderate, immediate, constrained, local).

% These advocates and institutions (e.g., certain legal scholars, civil society groups) actively promote and enforce the harm threshold reading, viewing it as essential for maintaining public order and protecting vulnerable groups. They shape policy and legal interpretation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, social_cohesion_advocates, agenda_setter,
    institutional, generational, mobile, national).

% These groups (e.g., civil liberties organizations) bear the cost of this constraint by seeing the scope of protected speech narrowed. They actively resist the expansion of harm-based restrictions, often through litigation and public advocacy. Their exit options are limited to challenging the legal framework itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, advocates_of_unfettered_expression, payer,
    organized, generational, constrained, national).

% The courts are responsible for interpreting and applying the harm threshold, balancing speech rights against victim protection. Their decisions actively shape the boundaries of this constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the right to free expression with the need to protect individuals and groups from demonstrable harm, aiming to foster a society where speech does not unduly infringe on the safety and well-being of others.
% TRANSFER_FUNCTION: Transfers a degree of expressive freedom from speakers (who must ensure their speech does not cause demonstrable harm) to potential victims (who gain protection from such harm).
% ABSENT_VOICES: Those who believe that any restriction on speech, even for harm, is a slippery slope to tyranny, or those who argue that 'harm' is too subjective a standard and easily weaponized, are often marginalized in the discourse that establishes and applies these thresholds.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished, there would be a significant increase in speech causing direct and demonstrable harm, leading to social unrest, increased litigation, and a breakdown of trust in public discourse. Society would have to rapidly re-evaluate how to manage the consequences of unrestricted harmful speech.
% FOUNDING_PROBLEM: The problem of speech causing direct and severe harm to individuals or groups, leading to violence, discrimination, or other tangible negative consequences, which existing legal frameworks were insufficient to address.
% FOUNDING_PROBLEM_CORROBORATION: Victim advocacy groups, public health organizations, and international human rights bodies consistently attest to the ongoing problem of speech-related harm. While some free speech absolutists contest the necessity of such restrictions, the evidence of harm is widely corroborated by independent research and lived experience.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is moderate-high because it directly limits expressive freedom for speakers, imposing a cost in terms of self-censorship or legal risk. Suppression (0.70) is high due to the active legal and social enforcement mechanisms (e.g., hate speech laws, anti-harassment policies) required to identify and penalize harmful speech. The theater ratio (0.20) is low, indicating that the constraint's function of preventing harm is genuinely pursued, though the definition of 'harm' can be contested. Accessibility collapse (0.40) is moderate, as alternatives (e.g., private platforms, encrypted communication) exist but are often less effective for broad public discourse. Resistance (0.55) is moderate-high, reflecting ongoing legal challenges and public debate from those advocating for broader speech protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of potential victims, this constraint is a necessary protection, a 'rope' that coordinates safety. From the perspective of speakers whose speech is restricted, it can feel like a 'snare' that unjustly curtails fundamental rights. The engine's classification will reflect this tension by computing different effective extraction for different seats based on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential victims of speech and social cohesion advocates are beneficiaries (d near 0.0) as they gain protection and a more orderly public sphere. Speakers of potentially harmful speech and advocates of unfettered expression are payers (d near 1.0) as their expressive freedom is curtailed. The judiciary acts as an agenda-setter, interpreting and enforcing the boundaries of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_demonstrable_harm,
    'What constitutes ''demonstrable harm'' in practice, and how consistently is this standard applied across different contexts and jurisdictions?',
    'Empirical analysis of legal precedents and social science research on the impact of various forms of speech, leading to clearer, more objective criteria for harm.',
    'If ''harm'' is too broadly or inconsistently defined, the constraint''s extractiveness and suppression could be higher than intended, potentially reclassifying it closer to a Snare for speakers. If narrowly and consistently defined, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_demonstrable_harm, empirical, 'Ambiguity in the definition and application of ''demonstrable harm''.').

omega_variable(
    slippery_slope_to_censorship,
    'Does the implementation of a harm threshold for speech protection inevitably lead to an expansion of censorship and a chilling effect on legitimate expression?',
    'Longitudinal comparative studies of jurisdictions with and without harm thresholds, tracking trends in protected speech, censorship, and public discourse over time.',
    'If a ''slippery slope'' is empirically demonstrated, the constraint''s long-term suppression and extractiveness would be higher, potentially shifting its classification towards a Snare or a more extractive Tangled Rope. If not, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_to_censorship, empirical, 'Whether harm thresholds lead to an inevitable expansion of censorship.').

omega_variable(
    balancing_of_rights_framing,
    'Is the harm threshold reading a legitimate balancing of competing rights (speech vs. safety), or does it fundamentally re-prioritize safety over speech in a way that undermines core expressive freedoms?',
    'Conceptual analysis and philosophical debate on the hierarchy of rights, and the role of speech in a democratic society, leading to a clearer normative framework.',
    'If framed as an illegitimate re-prioritization, the constraint would be seen as more extractive and suppressive from the perspective of free speech advocates, potentially leading to a reclassification of their seat''s experience. If framed as legitimate balancing, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_of_rights_framing, conceptual, 'Conceptual framing of the balance between speech and safety.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__harm_threshold_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__harm_threshold_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel', focusing on the conditionality of speech protection based on demonstrable harm to victims. It is distinct from other readings that emphasize absolutism, truth-discovery, dignity, or democratic participation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
