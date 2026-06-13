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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   to speak is not absolute but is conditional on the absence of
 *   demonstrable harm to identifiable victims. It posits that when speech
 *   crosses a certain threshold of harm, the protection it receives
 *   diminishes or vanishes, allowing for restriction or punishment. This
 *   reading is often invoked in contexts of hate speech, incitement,
 *   defamation, and harassment, where the impact on the target is prioritized
 *   over the speaker's autonomy. It is a 'tangled rope' because it genuinely
 *   coordinates the rights of speakers and the safety of individuals/groups,
 *   but the enforcement mechanism inherently extracts from speakers whose
 *   speech is deemed harmful.
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
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2').
narrative_ontology:cs_kernel_codification('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', formalized).
narrative_ontology:cs_authority_grounding('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', lineage).
narrative_ontology:cs_interpretation_layer_present('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2').
narrative_ontology:cs_reading_relation('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', foundational, harm_to_victims_justifies_speech_restriction).
narrative_ontology:cs_axiom_status(harm_to_victims_justifies_speech_restriction, holdable).
narrative_ontology:cs_axiom_grounding('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', harm_to_victims_justifies_speech_restriction, deontological).
narrative_ontology:cs_axiom('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', foundational, state_has_duty_to_protect_from_speech_harm).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_from_speech_harm, holdable).
narrative_ontology:cs_axiom_grounding('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', state_has_duty_to_protect_from_speech_harm, deontological).
narrative_ontology:cs_reference_frame('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', balanced_rights_framework).
narrative_ontology:cs_drift_state('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', contemporary_social_media_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1e1c88a7-7df3-4075-b3a8-dfe9feb2b9b2', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, public_order_authorities).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_of_potentially_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, advocates_for_broad_speech_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups whose speech is subject to restriction or penalty if deemed to cause demonstrable harm. They bear the cost of self-censorship or legal consequences, and their ability to express certain ideas is curtailed.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_of_potentially_harmful_speech, payer,
    moderate, immediate, constrained, global).

% Individuals or groups who are protected from the demonstrable harms caused by certain categories of speech. They benefit from legal recourse and a reduced likelihood of experiencing direct threats, harassment, or incitement to violence.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, victims_of_harmful_speech, beneficiary,
    powerless, immediate, trapped, local).

% Government bodies, courts, and regulatory agencies responsible for defining 'demonstrable harm' and enforcing restrictions on speech. They administer the constraint, adjudicate cases, and apply penalties, thereby maintaining public order and protecting victims.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, public_order_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars, civil liberties organizations, and activists who argue for expansive interpretations of speech protection. They bear the cost of legal battles and public discourse to resist restrictions, viewing this constraint as an overreach that chills legitimate expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, advocates_for_broad_speech_rights, payer,
    organized, generational, constrained, global).

% The broader society that benefits from a discourse environment with reduced demonstrable harm, potentially leading to greater civility and safety. However, they also indirectly 'pay' by potentially having access to a narrower range of ideas or perspectives due to restrictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the right of free expression with the right of individuals and groups to be free from demonstrable harm, aiming to create a public discourse that is both robust and safe.
% TRANSFER_FUNCTION: Transfers the burden of potential harm from victims to speakers, by restricting certain categories of speech. It transfers the authority to define and enforce these limits to public order authorities.
% ABSENT_VOICES: Those who believe that all speech, regardless of harm, should be absolutely protected, or that the 'marketplace of ideas' is the only legitimate mechanism for addressing harmful speech. They are often marginalized in legal and political discourse that prioritizes harm prevention.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, there would be a significant increase in speech causing demonstrable harm (e.g., incitement, harassment), leading to a more volatile public sphere. Victims would lose legal recourse, and public order authorities would lose a key tool for maintaining social cohesion, forcing a rapid reorganization of legal and social norms around speech.
% FOUNDING_PROBLEM: The problem of speech causing direct, measurable harm to individuals and groups, leading to real-world consequences like violence, discrimination, and reputational damage, which existing legal frameworks struggled to address without infringing on legitimate expression.
% FOUNDING_PROBLEM_CORROBORATION: Victims' rights advocates, public safety officials, and international human rights bodies consistently attest that the problem of harmful speech remains live and requires active management. Legal scholars and civil liberties groups, while critical of the constraint's scope, generally acknowledge the existence of speech that causes harm, even if they dispute the appropriate remedy.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because it limits a fundamental right based on a harm assessment, which can be subjective and lead to chilling effects. Suppression (0.70) is high as it requires active enforcement (legal frameworks, judicial review, social pressure) to identify, adjudicate, and penalize harmful speech, thereby suppressing certain categories of expression. The theater ratio (0.20) is relatively low, indicating that the constraint's stated function (preventing harm) is largely aligned with its actual operation, though some performativity may exist in how 'harm' is defined or applied. Accessibility collapse (0.40) is moderate, as alternatives for expressing ideas still exist, but the 'harmful' category is significantly restricted. Resistance (0.55) is also moderate, reflecting ongoing debates and legal challenges from free speech advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'victims_of_harmful_speech' and 'public_order_authorities', this constraint functions as a necessary 'rope' or 'scaffold' that protects vulnerable individuals and maintains social cohesion. They experience it as a coordination mechanism that balances competing rights. However, from the perspective of 'speakers_of_potentially_harmful_speech' and 'advocates_for_broad_speech_rights', it operates as a 'snare' or 'tangled_rope' that chills legitimate expression and grants excessive power to authorities to define and restrict speech. The engine's classification will reflect this divergence based on their declared positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   'Victims_of_harmful_speech' are clear beneficiaries (d=0.0) as the constraint directly protects them. 'Public_order_authorities' are also beneficiaries (d=0.1) as it grants them a legitimate basis for intervention. 'Speakers_of_potentially_harmful_speech' are targets (d=0.9) as their expression is directly curtailed. 'Advocates_for_broad_speech_rights' are also targets (d=0.8) as their ideological position is undermined and their efforts to expand speech are resisted. The 'general_public' is a mixed beneficiary/payer (d=0.5), benefiting from a safer discourse environment but potentially losing access to a wider range of ideas.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not currently mandatrophic. Its mandate (preventing demonstrable harm from speech) remains a live and contested problem. The classification as a Tangled Rope acknowledges the genuine coordination function (protecting victims) while highlighting the inherent extraction from speakers. If the definition of 'harm' were to expand indefinitely, or if enforcement became purely pretextual, it would drift towards a Snare. Conversely, if harm became impossible to demonstrate, it would drift towards a Piton or even dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine protection against harm, or a mechanism for suppressing disfavored speech under the guise of harm prevention?',
    'Empirical analysis of enforcement patterns: if enforcement disproportionately targets marginalized speakers or speech critical of power, it suggests suppression; if it consistently targets speech causing objectively measurable harm (e.g., incitement to violence, defamation), it suggests genuine harm prevention.',
    'If primarily suppression, the constraint''s effective extractiveness and suppression are higher, reclassifying it closer to a Snare. If genuine harm prevention, it functions as a Tangled Rope, balancing rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Distinguishing genuine harm prevention from pretextual suppression.').

omega_variable(
    harm_definition_ambiguity,
    'What constitutes ''demonstrable harm'' and who defines it? Is it objective, subjective, or institutionally mediated?',
    'Judicial precedent and legislative clarity: a clear, narrow definition of harm (e.g., direct incitement, true threats) reduces ambiguity; broad, subjective definitions (e.g., ''offense,'' ''discomfort'') increase it.',
    'A broad, subjective definition of harm increases the constraint''s suppression and extractiveness, as more speech falls under its purview, potentially shifting it towards a Snare. A narrow, objective definition maintains its Tangled Rope function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_definition_ambiguity, conceptual, 'Ambiguity in the definition and adjudication of ''demonstrable harm''.').

omega_variable(
    sibling_reading_impact,
    'How would adopting the ''absolutist_reading'' or ''marketplace_reading'' of the speech protection kernel alter the structural properties of this ''harm_threshold_reading'' constraint?',
    'Counterfactual analysis: if the absolutist reading were adopted, this constraint would effectively cease to exist, as harm would not be a basis for restriction. If the marketplace reading were adopted, this constraint would be weakened, as the primary remedy for harmful speech would be more speech, not restriction.',
    'The absolutist reading would foreclose this constraint entirely. The marketplace reading would significantly reduce its extractiveness and suppression, as enforcement would shift from pre-emptive restriction to post-hoc counter-speech, potentially reclassifying it as a Rope or even a Piton if enforcement atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of alternative kernel readings on this constraint''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__harm_threshold_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__harm_threshold_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, defamation_law).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, incitement_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel', focusing on the harm threshold. Other readings (absolutist, marketplace, dignity, democratic_participation) are distinct constraints with different structural properties and classifications, linked through this kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
