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
 *   human_readable: Contextual Necessity Reading of Humane Treatment Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'contextual necessity' reading of the
 *   international humane treatment standard, particularly Common Article 3 of
 *   the Geneva Conventions. It asserts that while a baseline of humane
 *   treatment exists, national security imperatives can override certain
 *   aspects, permitting 'enhanced interrogation' when deemed necessary. This
 *   interpretation grants state security agencies significant discretion,
 *   making detainee protections conditional and narrowing the effective
 *   victim set in high-stakes scenarios. The constraint is claimed as a
 *   Tangled Rope because it maintains a nominal coordination function
 *   (baseline humane treatment) but enables asymmetric extraction (enhanced
 *   interrogation) through active enforcement of its interpretation.
 *
 * KEY AGENTS:
 *   - state_security_agencies: Primary agenda_setter (institutional/immediate) — defines and implements 'humane' treatment.
 *   - national_security_decision_makers: Primary beneficiary (institutional/generational) — benefits from legal flexibility.
 *   - detainees_in_necessity_scenarios: Primary payer (powerless/immediate) — bears the direct costs of 'enhanced interrogation'.
 *   - human_rights_advocates: Secondary payer (organized/generational) — resists this interpretation.
 *   - international_legal_bodies: Observer (institutional/generational) — monitors and critiques.
 *   - public_opinion: Excluded (moderate/biographical) — often swayed by security narratives, but would object to clear abuses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.8).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.85).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.8).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Necessity Reading of Humane Treatment Standard").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '8d9e6899-8582-4ca8-9201-199a72a0868c').
narrative_ontology:cs_kernel_codification('8d9e6899-8582-4ca8-9201-199a72a0868c', fixed_text).
narrative_ontology:cs_authority_grounding('8d9e6899-8582-4ca8-9201-199a72a0868c', extraction).
narrative_ontology:cs_interpretation_layer_present('8d9e6899-8582-4ca8-9201-199a72a0868c').
narrative_ontology:cs_reading_relation('8d9e6899-8582-4ca8-9201-199a72a0868c', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('8d9e6899-8582-4ca8-9201-199a72a0868c', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('8d9e6899-8582-4ca8-9201-199a72a0868c', foundational, national_security_imperatives_can_override_general_prohibitions).
narrative_ontology:cs_axiom_status(national_security_imperatives_can_override_general_prohibitions, holdable).
narrative_ontology:cs_axiom_grounding('8d9e6899-8582-4ca8-9201-199a72a0868c', national_security_imperatives_can_override_general_prohibitions, instrumental).
narrative_ontology:cs_axiom('8d9e6899-8582-4ca8-9201-199a72a0868c', foundational, interpretation_of_humane_treatment_is_flexible).
narrative_ontology:cs_axiom_status(interpretation_of_humane_treatment_is_flexible, holdable).
narrative_ontology:cs_axiom_grounding('8d9e6899-8582-4ca8-9201-199a72a0868c', interpretation_of_humane_treatment_is_flexible, conventional).
narrative_ontology:cs_reference_frame('8d9e6899-8582-4ca8-9201-199a72a0868c', state_discretion_for_security).
narrative_ontology:cs_drift_state('8d9e6899-8582-4ca8-9201-199a72a0868c', post_9_11_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8d9e6899-8582-4ca8-9201-199a72a0868c', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_decision_makers).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_in_necessity_scenarios).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies interpret and apply the standard, gaining discretion to define 'humane' treatment in national security contexts. They implement and enforce 'enhanced interrogation' techniques when deemed necessary.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_security_agencies, agenda_setter,
    institutional, immediate, arbitrage, national).

% Political and military leaders who benefit from the flexibility this reading provides, allowing them to authorize methods they believe are critical for intelligence gathering without being absolutely constrained by international law.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_security_decision_makers, beneficiary,
    institutional, generational, mobile, national).

% Individuals designated as high-value targets or security threats, who are subjected to 'enhanced interrogation' methods under this interpretation. Their protections are conditional, and they bear the direct costs of the constraint.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_in_necessity_scenarios, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals who actively resist this interpretation, arguing for absolute prohibitions on torture and degrading treatment. They bear the costs of litigation, advocacy, and reputational damage when such practices are revealed.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_advocates, payer,
    organized, generational, constrained, global).

% International courts, UN committees, and other bodies that monitor state compliance with international humanitarian law. They critique this reading and its application, but their enforcement power is often limited by state sovereignty.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% The general populace, whose views on 'humane treatment' can be swayed by national security narratives. While many would object to clear abuses, the secrecy surrounding 'enhanced interrogation' often keeps them from fully engaging or holding decision-makers accountable.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, public_opinion, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and operational framework for states to conduct intelligence gathering, including 'enhanced interrogation,' in situations deemed critical for national security, while maintaining a nominal adherence to international humanitarian law's baseline standards.
% TRANSFER_FUNCTION: Transfers the burden of absolute humane treatment from state security agencies to detainees in specific, high-stakes national security scenarios, allowing agencies to extract information through methods that would otherwise be prohibited. It also transfers the cost of legal and ethical defense to human rights advocates.
% ABSENT_VOICES: The voices of detainees themselves are largely absent, as are those of international human rights monitors and independent legal experts when states assert national security imperatives and classify information. These voices would argue for non-derogable standards.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state security agencies would lose their legal justification for 'enhanced interrogation,' forcing a fundamental re-evaluation of intelligence-gathering methods. This would lead to increased legal challenges, international condemnation for past practices, and a significant shift in how states balance security needs with human rights obligations.
% FOUNDING_PROBLEM: The perceived need for states to extract critical intelligence from high-value detainees to prevent imminent threats, which conventional, non-derogable humane treatment standards were seen to impede.
% FOUNDING_PROBLEM_CORROBORATION: State security agencies and some political leaders attest to the ongoing and critical nature of the problem, citing persistent threats. Human rights organizations and international legal experts dispute this, arguing that effective intelligence can be gathered without derogating from absolute prohibitions, and that such practices are often counterproductive; their corroboration comes from independent investigations and expert reports.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.8) because the reading permits methods that inflict significant harm and violate fundamental rights, extracting information under duress. Suppression is very high (0.85) due to the active enforcement of secrecy, legal justifications, and the physical control over detainees, which prevents resistance and external scrutiny. Theater ratio is moderate (0.4) as there's an ongoing performance of legal compliance and 'humane' treatment, even as practices deviate. The increasing trend in extractiveness and suppression reflects the hardening of this interpretation in the post-9/11 era, with a slight decline towards the end of the interval as public and legal scrutiny increased.
 *
 * PERSPECTIVAL GAP:
 *   State security agencies and national security decision-makers perceive this reading as a necessary and legitimate adaptation of international law to modern threats, enabling vital intelligence gathering. Detainees and human rights advocates, however, experience it as a mechanism for state-sanctioned abuse and a fundamental erosion of non-derogable human rights. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope or Scaffold, and victims as a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and national security decision-makers are clear beneficiaries, gaining discretion and operational flexibility (low d). Detainees are direct targets, bearing the full cost of 'enhanced interrogation' (high d). Human rights advocates are also targets, as they expend significant resources to challenge this interpretation (high d). International legal bodies are analytical observers, while public opinion is largely excluded from the direct decision-making process.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the asymmetric extraction and victimhood) or a pure Snare (which would ignore the claimed coordination function of providing a framework for state security). The 'baseline humane treatment' provides the coordination cover, while the 'contextual necessity' clause enables the extraction. The 'live' status of the founding problem (from the perspective of proponents) suggests it is not yet a Piton, but the contestation over its status points to potential future mandatrophy if the security imperative is widely deemed to be a pretext for abuse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'How does this ''contextual necessity'' reading structurally differ from the ''absolute prohibition'' and ''proportionality balancing'' readings of the humane treatment standard?',
    'Comparative legal analysis of state practice and jurisprudence under each reading, focusing on the conditions under which derogations from humane treatment are permitted or justified.',
    'If the ''absolute prohibition'' reading were adopted, the victim set would expand to include all detainees, and extractiveness would drop to near zero. If ''proportionality balancing'' were adopted, extractiveness would be lower than ''contextual necessity'' but higher than ''absolute prohibition'', as some balancing of interests would still occur.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing this reading from its siblings within the humane treatment standard kernel.').

omega_variable(
    effectiveness_of_enhanced_interrogation,
    'Is ''enhanced interrogation'' demonstrably effective in preventing national security threats, or is it counterproductive and unreliable?',
    'Empirical studies, declassified intelligence reports, and expert testimony evaluating the intelligence yield and reliability of information obtained through ''enhanced interrogation'' versus conventional methods.',
    'If proven ineffective or counterproductive, the instrumental justification for this reading would collapse, significantly weakening its legitimacy and increasing pressure for its abandonment, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_enhanced_interrogation, empirical, 'Empirical validity of the instrumental justification for ''enhanced interrogation''.').

omega_variable(
    scope_of_necessity_definition,
    'How broadly or narrowly is ''national security imperative'' defined by state actors, and is this definition subject to independent oversight?',
    'Analysis of legal definitions, internal policy documents, and judicial review mechanisms (or lack thereof) that govern the declaration of ''necessity'' in practice.',
    'A broad, unchecked definition of ''necessity'' would amplify extractiveness and suppression, making the constraint more Snare-like. A narrow, independently reviewed definition would reduce its extractive potential, pushing it closer to a Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_necessity_definition, conceptual, 'The definitional scope and oversight of ''national security imperative''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 2001, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__contextual_necessity, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(huma_tr_t2005, humane_treatment_standard__contextual_necessity, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(huma_tr_t2009, humane_treatment_standard__contextual_necessity, theater_ratio, 2009, 0.4).
narrative_ontology:measurement(huma_tr_t2013, humane_treatment_standard__contextual_necessity, theater_ratio, 2013, 0.45).
narrative_ontology:measurement(huma_tr_t2017, humane_treatment_standard__contextual_necessity, theater_ratio, 2017, 0.42).
narrative_ontology:measurement(huma_tr_t2021, humane_treatment_standard__contextual_necessity, theater_ratio, 2021, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__contextual_necessity, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(huma_be_t2005, humane_treatment_standard__contextual_necessity, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(huma_be_t2009, humane_treatment_standard__contextual_necessity, base_extractiveness, 2009, 0.78).
narrative_ontology:measurement(huma_be_t2013, humane_treatment_standard__contextual_necessity, base_extractiveness, 2013, 0.81).
narrative_ontology:measurement(huma_be_t2017, humane_treatment_standard__contextual_necessity, base_extractiveness, 2017, 0.83).
narrative_ontology:measurement(huma_be_t2021, humane_treatment_standard__contextual_necessity, base_extractiveness, 2021, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__contextual_necessity, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(huma_su_t2005, humane_treatment_standard__contextual_necessity, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(huma_su_t2009, humane_treatment_standard__contextual_necessity, suppression_requirement, 2009, 0.85).
narrative_ontology:measurement(huma_su_t2013, humane_treatment_standard__contextual_necessity, suppression_requirement, 2013, 0.88).
narrative_ontology:measurement(huma_su_t2017, humane_treatment_standard__contextual_necessity, suppression_requirement, 2017, 0.87).
narrative_ontology:measurement(huma_su_t2021, humane_treatment_standard__contextual_necessity, suppression_requirement, 2021, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'humane_treatment_standard' kernel. Each reading represents a different structural interpretation of Common Article 3 and its applicability, leading to different extraction profiles and classifications. This reading (contextual_necessity) directly forecloses the absolute_prohibition reading and coexists with the proportionality_balancing reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
