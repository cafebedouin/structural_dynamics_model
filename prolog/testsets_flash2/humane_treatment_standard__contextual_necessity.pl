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
 *   This constraint represents the 'contextual necessity' reading of the
 *   international humane treatment standard (Common Article 3 of the Geneva
 *   Conventions). Under this reading, while a baseline of humane treatment is
 *   acknowledged, national security imperatives can override certain
 *   protections, permitting 'enhanced interrogation' techniques. This
 *   interpretation grants discretion to state security agencies and national
 *   security decision-makers, making detainee protections conditional and
 *   narrowing the victim set to exclude those deemed high-value targets in
 *   necessity scenarios. The constraint is claimed as a 'rope' by its
 *   proponents, framing it as a necessary coordination mechanism for state
 *   security, but its operational metrics reflect a 'tangled_rope' due to
 *   significant extraction and suppression.
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
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Humane Treatment Standard (Contextual Necessity Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '2434c1ee-35fa-46bf-b7e8-d0e01622d267').
narrative_ontology:cs_kernel_codification('2434c1ee-35fa-46bf-b7e8-d0e01622d267', formalized).
narrative_ontology:cs_authority_grounding('2434c1ee-35fa-46bf-b7e8-d0e01622d267', extraction).
narrative_ontology:cs_interpretation_layer_present('2434c1ee-35fa-46bf-b7e8-d0e01622d267').
narrative_ontology:cs_reading_relation('2434c1ee-35fa-46bf-b7e8-d0e01622d267', humane_treatment_standard__absolute_prohibition_of_torture, coexists_with).
narrative_ontology:cs_reading_relation('2434c1ee-35fa-46bf-b7e8-d0e01622d267', humane_treatment_standard__proportionality_balancing_in_detention, coexists_with).
narrative_ontology:cs_axiom('2434c1ee-35fa-46bf-b7e8-d0e01622d267', foundational, national_security_overrides_absolute_prohibition).
narrative_ontology:cs_axiom_status(national_security_overrides_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('2434c1ee-35fa-46bf-b7e8-d0e01622d267', national_security_overrides_absolute_prohibition, instrumental).
narrative_ontology:cs_axiom('2434c1ee-35fa-46bf-b7e8-d0e01622d267', foundational, humane_treatment_is_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('2434c1ee-35fa-46bf-b7e8-d0e01622d267', humane_treatment_is_context_dependent, conventional).
narrative_ontology:cs_reference_frame('2434c1ee-35fa-46bf-b7e8-d0e01622d267', state_sovereignty_security_paradigm).
narrative_ontology:cs_drift_state('2434c1ee-35fa-46bf-b7e8-d0e01622d267', post_9_11_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2434c1ee-35fa-46bf-b7e8-d0e01622d267', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_decision_makers).
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

% Political and military leaders who authorize or defend the use of enhanced interrogation techniques under this reading. They benefit from the perceived flexibility in intelligence gathering, especially in crisis situations, and from avoiding accountability for actions taken under 'necessity'.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_security_decision_makers, beneficiary,
    institutional, immediate, constrained, national).

% Individuals designated as high-value targets or threats, who are subjected to 'enhanced interrogation' techniques. They bear the direct physical and psychological costs of treatment that falls below absolute humane standards, with no effective recourse or exit.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_high_value_cases, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals who campaign against any derogation from absolute humane treatment standards. They bear the cost of continuously challenging state interpretations and defending detainee rights, facing institutional resistance and often being labeled as undermining national security.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_advocates, payer,
    organized, generational, constrained, global).

% Courts and commissions that review state practices against international law. They observe the application of this reading, often finding it in violation of absolute prohibitions, but their enforcement power is limited by state sovereignty and political will.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows states to coordinate their security responses in perceived high-stakes national security scenarios by providing a framework that permits deviation from absolute humane treatment standards, aiming to prevent perceived greater harms.
% TRANSFER_FUNCTION: Transfers the burden of 'humane treatment' from state security agencies to individual detainees in specific contexts, allowing for the extraction of intelligence through methods that would otherwise be prohibited.
% ABSENT_VOICES: Detainees themselves, who are silenced by their captivity and the very nature of the 'enhanced interrogation' methods. Their perspective on 'humane treatment' is systematically excluded from the decision-making process that defines it.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state security agencies would lose a key justification for certain interrogation methods, forcing a re-evaluation of intelligence gathering practices and potentially leading to a stricter adherence to absolute prohibitions. This would significantly alter state security operations and legal frameworks.
% FOUNDING_PROBLEM: The perceived need for states to extract critical intelligence from high-value detainees to prevent imminent national security threats, where conventional interrogation methods are deemed insufficient.
% FOUNDING_PROBLEM_CORROBORATION: State security agencies and national security decision-makers consistently attest to the live status of this problem, citing ongoing threats. Human rights advocates and international legal bodies contest this, arguing that the 'necessity' is often exaggerated or that alternative, humane methods are effective, but their corroboration is often dismissed by the benefiting parties.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) stems from the direct harm inflicted on detainees and the erosion of universal human rights principles. Suppression (0.78) is high due to the active legal and operational efforts by states to defend and implement this interpretation, often through secrecy and by limiting judicial oversight. The theater ratio (0.4) reflects that while some genuine security concerns exist, a significant portion of the justification for 'enhanced interrogation' serves to legitimize practices that are primarily extractive and suppressive, rather than purely functional. The metrics show a slight increase in extractiveness and suppression over time, indicating a hardening of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state security agencies, this reading is a necessary, albeit difficult, coordination mechanism to protect national interests. From the perspective of detainees and human rights advocates, it is a highly extractive and suppressive mechanism that undermines fundamental human dignity. The engine's classification will highlight this divergence between the claimed 'rope' and the computed 'tangled_rope' or 'snare' from the victim's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and national security decision-makers are clear beneficiaries, gaining discretion and avoiding accountability (low directionality). Detainees are direct targets, bearing the full cost (high directionality). Human rights advocates are also targets, as their efforts to uphold absolute standards are undermined (high directionality). International legal bodies act as observers, analyzing the constraint's operation without direct benefit or cost from its application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_definition_ambiguity,
    'How is ''national security imperative'' defined, and who holds the authority to make that determination, particularly in real-time crisis situations?',
    'Establishment of independent, transparent oversight mechanisms with clear legal criteria for ''necessity'' and ex-post review of determinations.',
    'If the definition is narrow and oversight robust, the constraint''s extractiveness and suppression would decrease, pushing it closer to a ''rope'' or ''scaffold''. If the definition remains broad and self-serving, it reinforces the ''snare'' characteristics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_definition_ambiguity, conceptual, 'Ambiguity in the definition and authority for ''national security imperative''.').

omega_variable(
    effectiveness_of_enhanced_interrogation,
    'Is ''enhanced interrogation'' genuinely effective in producing actionable intelligence that cannot be obtained through humane, lawful methods?',
    'Independent, declassified empirical studies comparing intelligence yields from ''enhanced'' vs. humane methods, with rigorous controls for confounding factors.',
    'If proven ineffective, the primary justification for this reading collapses, exposing its purely extractive nature and reclassifying it closer to a ''snare''. If proven uniquely effective, it would strengthen the ''tangled_rope'' aspect by highlighting a genuine (though ethically fraught) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_enhanced_interrogation, empirical, 'Empirical validity of ''enhanced interrogation'' for intelligence gathering.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/institutional barriers) or internalized (fear, psychological conditioning of detainees)?',
    'Post-release psychological assessment of former detainees and analysis of legal frameworks: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making recovery harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in detainee treatment.').


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
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.38).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, absolute_prohibition_of_torture).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, proportionality_balancing_in_detention).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
