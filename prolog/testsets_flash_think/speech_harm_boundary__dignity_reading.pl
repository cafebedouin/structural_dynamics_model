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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Protection Subordinate to Human Dignity (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the 'dignity reading' of the
 *   speech_harm_boundary kernel, where speech protection is explicitly
 *   subordinate to human dignity, and personhood-denying speech (e.g.,
 *   Holocaust denial, hate speech) is categorically unprotected. This reading
 *   prioritizes the protection of vulnerable groups from identity-based harm,
 *   leading to significant restrictions on certain forms of expression. The
 *   constraint is classified as a Tangled Rope because it coordinates social
 *   interaction by protecting dignity, but does so through asymmetric
 *   extraction from speakers of dignity-violating speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.8).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.85).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Protection Subordinate to Human Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '8795a986-1503-487e-b1a6-7db34d784bcc').
narrative_ontology:cs_kernel_codification('8795a986-1503-487e-b1a6-7db34d784bcc', formalized).
narrative_ontology:cs_authority_grounding('8795a986-1503-487e-b1a6-7db34d784bcc', lineage).
narrative_ontology:cs_interpretation_layer_present('8795a986-1503-487e-b1a6-7db34d784bcc').
narrative_ontology:cs_reading_relation('8795a986-1503-487e-b1a6-7db34d784bcc', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('8795a986-1503-487e-b1a6-7db34d784bcc', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('8795a986-1503-487e-b1a6-7db34d784bcc', foundational, human_dignity_is_precondition_for_speech).
narrative_ontology:cs_axiom_status(human_dignity_is_precondition_for_speech, holdable).
narrative_ontology:cs_axiom_grounding('8795a986-1503-487e-b1a6-7db34d784bcc', human_dignity_is_precondition_for_speech, deontological).
narrative_ontology:cs_axiom('8795a986-1503-487e-b1a6-7db34d784bcc', foundational, categorical_exclusion_for_personhood_denial).
narrative_ontology:cs_axiom_status(categorical_exclusion_for_personhood_denial, holdable).
narrative_ontology:cs_axiom_grounding('8795a986-1503-487e-b1a6-7db34d784bcc', categorical_exclusion_for_personhood_denial, conventional).
narrative_ontology:cs_reference_frame('8795a986-1503-487e-b1a6-7db34d784bcc', post_wwii_human_rights_consensus).
narrative_ontology:cs_drift_state('8795a986-1503-487e-b1a6-7db34d784bcc', contemporary_digital_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8795a986-1503-487e-b1a6-7db34d784bcc', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_advocates).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups (e.g., racial, religious, LGBTQ+ minorities) are the primary beneficiaries of dignity-based speech protections, as they are historically and currently targeted by personhood-denying speech. The constraint aims to create a public sphere where their equal participation is not undermined by such speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, vulnerable_groups, beneficiary,
    powerless, generational, trapped, national).

% Legal scholars, human rights organizations, and civil society groups who actively champion the principle of human dignity as a foundational limit on speech. They work to codify and enforce these protections, seeing them as essential for a just society.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_advocates, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, dignity_advocates, beneficiary).

% Individuals or groups whose speech is deemed to deny the personhood or dignity of others (e.g., Holocaust deniers, purveyors of racist or homophobic slurs). They bear the direct cost of this constraint through legal penalties, social censure, and the categorical exclusion of their speech from protection.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_hate_speech, payer,
    moderate, immediate, constrained, local).

% Organizations and individuals who argue for a near-absolute protection of speech, viewing any content-based restrictions, even for dignity, as a dangerous precedent. They bear the cost of this reading by seeing their preferred interpretation of free speech law rejected and their advocacy efforts challenged.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates, payer,
    powerful, generational, mobile, national).

% Judicial bodies and regulatory agencies tasked with interpreting and enforcing speech laws. They apply the dignity reading by identifying and sanctioning speech that falls into the categorically unprotected categories, balancing the right to speak with the duty to protect human dignity.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Organizations like the UN Human Rights Committee, which interpret international treaties that often prioritize human dignity and prohibit hate speech. They provide a global normative framework that influences national legal systems adopting this reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, international_human_rights_bodies, agenda_setter).

% Advocates who believe speech should be protected presumptively but yield to demonstrated harm through a proportionality balancing test, rather than categorical exclusions. While their approach might sometimes lead to similar outcomes, their methodology is distinct and often sidelined by the categorical nature of the dignity reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, harm_balancing_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction by establishing clear boundaries for acceptable public discourse, ensuring that speech does not undermine the fundamental dignity and equal participation of all individuals in society.
% TRANSFER_FUNCTION: Transfers the burden of speech restriction from vulnerable groups (who would otherwise bear the harm of personhood-denying speech) to the speakers of such speech, who face legal and social consequences.
% ABSENT_VOICES: Those who advocate for a purely absolutist interpretation of free speech, or those who insist on a case-by-case harm-balancing approach without categorical exclusions, are often marginalized in the discourse shaped by this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, public discourse would immediately become more hostile towards vulnerable groups, leading to increased social fragmentation, psychological harm, and potential incitement to violence, necessitating a rapid re-establishment of similar protections.
% FOUNDING_PROBLEM: The historical and ongoing problem of speech being used to deny the personhood and equal dignity of certain groups, leading to their marginalization, discrimination, and violence, particularly evident in the aftermath of atrocities like the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law (e.g., ICCPR Article 20), historical records of genocide and discrimination, and contemporary reports from civil society organizations, vulnerable communities, and legal scholars outside the immediate beneficiary groups.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.80) because speakers of dignity-violating speech face severe restrictions and penalties, effectively losing their 'right' to utter such speech. Suppression is very high (0.85) due to the categorical nature of the exclusion and active legal enforcement. Theater ratio is low (0.10) because the enforcement is direct and functional, with little performative maintenance; the goal is genuinely to suppress the identified speech. Accessibility collapse is high (0.70) as the legal framework aims to make certain types of speech entirely inaccessible for protection. Resistance is moderate-high (0.60) from absolutist free speech advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups and dignity advocates, this constraint is a necessary and just coordination mechanism that ensures equal participation and prevents harm. From the perspective of speakers of hate speech and absolutist free speech advocates, it is an unjust and highly extractive suppression of fundamental rights, leading to a chilling effect on legitimate expression.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups and dignity advocates are clear beneficiaries, experiencing enhanced protection and a more inclusive public sphere. Speakers of hate speech and absolutist free speech advocates are the primary targets, bearing the costs of restriction and the rejection of their interpretive framework. Courts and international bodies act as agenda-setters, enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_definition_ambiguity,
    'How is ''human dignity'' precisely defined in this context, and what are the boundaries of ''personhood-denying speech''?',
    'Further judicial clarification through case law, or legislative action providing more specific definitions and examples.',
    'A narrower definition might reduce extractiveness and suppression for some speakers, potentially shifting the classification towards a Rope or a less extractive Tangled Rope. A broader definition could increase extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_definition_ambiguity, conceptual, 'Ambiguity in the core concept of dignity and its application to speech.').

omega_variable(
    overreach_chilling_effect_tension,
    'Does the categorical exclusion of personhood-denying speech lead to an unacceptable chilling effect on legitimate, but controversial, expression?',
    'Empirical studies on self-censorship patterns among speakers, or judicial review of specific applications of the constraint to ensure it does not unduly restrict non-harmful speech.',
    'If a significant chilling effect on legitimate speech is demonstrated, it could increase the perceived extractiveness and suppression, potentially pushing the classification closer to a Snare from the speaker''s perspective, or at least highlighting a significant cost to the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overreach_chilling_effect_tension, empirical, 'Tension between categorical exclusion and potential for overreach.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal bans, penalties) or internalized (social pressure, self-censorship by speakers)?',
    'Post-legal-reform analysis: if the legal framework were relaxed, would social norms continue to suppress this speech? If so, internalized suppression is a significant factor.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests, as speakers carry the suppression with them even if formal barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dignity-violating speech.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the ''dignity_reading'' of the ''speech_harm_boundary'' kernel. What specific structural elements would change if an alternative reading (e.g., ''absolutist_reading'' or ''harm_balancing_reading'') were adopted?',
    'Comparative legal analysis of jurisdictions adopting different readings, or counterfactual modeling of legal outcomes under alternative interpretive frameworks.',
    'Adopting the ''absolutist_reading'' would drastically lower extractiveness and suppression for speakers, but increase harm for vulnerable groups. Adopting the ''harm_balancing_reading'' would replace categorical exclusions with a more nuanced, but potentially less predictable, balancing test, altering the nature of enforcement and the certainty of protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__dignity_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__dignity_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__dignity_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__dignity_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__dignity_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__dignity_reading, base_extractiveness, 50, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__dignity_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__dignity_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__dignity_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
