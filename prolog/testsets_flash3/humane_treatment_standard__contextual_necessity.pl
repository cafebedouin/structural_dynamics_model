% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Humane Treatment Standard (Contextual Necessity Reading)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'contextual necessity' reading of Common
 *   Article 3 of the Geneva Conventions, which posits that while a baseline
 *   of humane treatment exists, national security imperatives can override
 *   it, permitting 'enhanced interrogation' techniques. This reading is often
 *   advanced by state security agencies and political leadership, who argue
 *   for flexibility in intelligence gathering during perceived national
 *   emergencies. The constraint is classified as a Tangled Rope because it
 *   claims a coordination function (national security) but involves
 *   significant asymmetric extraction from detainees and requires active
 *   enforcement to maintain this interpretation against international norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.65).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.78).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Humane Treatment Standard (Contextual Necessity Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'a98dd911-f5dd-4e0b-9e30-f5b3cd752668').
narrative_ontology:cs_kernel_codification('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', fixed_text).
narrative_ontology:cs_authority_grounding('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', extraction).
narrative_ontology:cs_interpretation_layer_present('a98dd911-f5dd-4e0b-9e30-f5b3cd752668').
narrative_ontology:cs_reading_relation('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', foundational, national_security_overrides_absolute_prohibition).
narrative_ontology:cs_axiom_status(national_security_overrides_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', national_security_overrides_absolute_prohibition, instrumental).
narrative_ontology:cs_axiom('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', foundational, humane_treatment_is_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', humane_treatment_is_context_dependent, conventional).
narrative_ontology:cs_reference_frame('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', state_discretion_in_security_operations).
narrative_ontology:cs_drift_state('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', contemporary_human_rights_scrutiny, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a98dd911-f5dd-4e0b-9e30-f5b3cd752668', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, political_leadership).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_high_value_cases).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies interpret Common Article 3 to allow 'enhanced interrogation' techniques when national security is deemed at risk. They benefit from the discretion to use such methods, believing it essential for intelligence gathering and preventing attacks. They actively enforce this interpretation within their operations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_security_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from the perceived ability to protect national security through all available means, including 'enhanced interrogation.' They provide political cover and legal justification for the security agencies' actions, often facing public and international scrutiny.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, political_leadership, beneficiary,
    institutional, immediate, constrained, national).

% These individuals are subjected to 'enhanced interrogation' techniques under the justification of national security. They bear the direct physical and psychological costs, with minimal legal recourse or ability to resist due to their detention status.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_high_value_cases, payer,
    powerless, immediate, trapped, local).

% These groups actively campaign against any interpretation of Common Article 3 that permits 'enhanced interrogation.' They bear the costs of legal challenges, public advocacy, and reputational damage to the international human rights framework. Their resistance is primarily through legal and public pressure.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_advocates, payer,
    organized, generational, constrained, global).

% These bodies monitor compliance with international humanitarian law, including Common Article 3. They analyze state practices and issue rulings or recommendations, but their enforcement power is often limited to diplomatic pressure or sanctions.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows state security agencies to coordinate intelligence gathering and counter-terrorism efforts by providing a framework for detainee treatment that can be adapted to perceived national security exigencies.
% TRANSFER_FUNCTION: Transfers the burden of 'humane treatment' from state actors to detainees and the international human rights framework, allowing states to prioritize security over absolute detainee protections in specific contexts.
% ABSENT_VOICES: Victims of 'enhanced interrogation' and their families are often silenced or discredited; their voices would unequivocally condemn the practices and challenge the 'necessity' claims, but they lack a platform or legal standing to be heard effectively.
% DISAPPEARANCE_RATIONALE: If this contextual necessity reading vanished, state security agencies would lose a key justification for 'enhanced interrogation,' forcing a re-evaluation of intelligence methods and potentially increasing legal risks for officials. Detainee treatment protocols would revert to a stricter interpretation, and human rights advocates would gain significant ground.
% FOUNDING_PROBLEM: The perceived need for flexibility in intelligence gathering during asymmetric conflicts and the 'war on terror' to prevent future attacks and protect national security.
% FOUNDING_PROBLEM_CORROBORATION: State security agencies and political leadership consistently attest that the problem of national security threats requiring flexible intelligence methods remains live. However, human rights organizations and international legal experts contest the necessity of 'enhanced interrogation' for effective intelligence, citing evidence of its ineffectiveness and counterproductive nature.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading allows for practices that inflict severe physical and psychological harm, effectively extracting dignity and rights from detainees. Suppression (0.78) is also high, as detainees are physically and legally trapped, and state actors actively suppress legal challenges and public dissent against these practices. The theater ratio (0.4) reflects that while some genuine security concerns exist, a significant portion of the justification and enforcement is performative, aimed at legitimizing practices that would otherwise be considered illegal. The slight dip in extractiveness and suppression at the end of the interval reflects periods of increased scrutiny and legal challenges, forcing some re-evaluation, but the core interpretation remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state security agencies, this reading is a necessary coordination mechanism for national defense. From the perspective of detainees and human rights advocates, it is a clear instance of state-sanctioned extraction and abuse. The engine's classification will highlight this divergence, showing a claimed 'rope' (coordination) operating with high extractiveness and suppression (snare-like characteristics) from the victim's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and political leadership are beneficiaries, gaining discretion and perceived security benefits (low directionality). Detainees are clear victims, bearing the direct costs of 'enhanced interrogation' (high directionality). Human rights advocates are also payers, expending resources to resist this interpretation (high directionality). International legal bodies act as observers, analyzing and critiquing the constraint without direct participation in its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_justification_empirical_validity,
    'Is ''enhanced interrogation'' empirically effective in preventing national security threats, or are its claimed benefits largely uncorroborated or counterproductive?',
    'Declassified intelligence reports, independent expert reviews of intelligence outcomes, and comparative studies of interrogation methods in states with absolute prohibitions.',
    'If proven ineffective, the ''necessity'' claim collapses, reclassifying the constraint closer to a pure Snare by removing its coordination cover. If proven effective, it would strengthen the Tangled Rope classification, acknowledging a genuine (though costly) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_justification_empirical_validity, empirical, 'Empirical basis for the ''national security imperative'' justification.').

omega_variable(
    legal_interpretation_scope,
    'To what extent does ''contextual necessity'' represent a legitimate interpretation within the existing framework of international humanitarian law, versus a re-interpretation that fundamentally alters its core principles?',
    'Rulings by international courts (e.g., ICC, ICJ), consensus among leading international legal scholars, and state practice over time.',
    'If deemed a fundamental alteration, it would highlight the constraint''s suppressive nature against established legal norms, pushing it towards a Snare. If accepted as a valid (though controversial) interpretation, it would reinforce its Tangled Rope status as a contested but recognized legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_interpretation_scope, conceptual, 'Legitimacy of ''contextual necessity'' within international law.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, physical detention) or internalized (fear, psychological manipulation leading to self-censorship or compliance)?',
    'Post-release psychological assessments of detainees, analysis of legal aid access and effectiveness, and studies on the long-term impact of ''enhanced interrogation'' on resistance capacity.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as detainees carry the suppression with them after physical release, making exit (e.g., seeking justice) more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for detainees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__contextual_necessity, theater_ratio, 5, 0.35).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.4).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.42).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, international_criminal_jurisdiction).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, state_sovereignty_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'humane_treatment_standard' kernel (Common Article 3). It represents the 'contextual necessity' interpretation, which permits 'enhanced interrogation' under national security imperatives. It is linked to 'absolute_prohibition' and 'proportionality_balancing' as sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
