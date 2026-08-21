% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable or Unwilling Doctrine
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint describes the 'unable or unwilling' doctrine as a reading
 *   of Article 51 of the UN Charter, which permits self-defense. This reading
 *   asserts that self-defense is triggered when a non-state actor attack
 *   originates from a host state that is unwilling or unable to suppress the
 *   threat. It creates a hybrid constraint: it requires an actual non-state
 *   actor attack (not purely preventive) but permits unilateral response,
 *   bypassing host state sovereignty. This reading is a Tangled Rope because
 *   it provides a coordination function (addressing transnational threats)
 *   but involves significant extraction (bypassing sovereignty) and requires
 *   active enforcement.
 *
 * KEY AGENTS:
 *   - intervening_states_with_counterterrorism_mandates: Primary beneficiary/agenda-setter (institutional/constrained)
 *   - host_states_with_non_state_actor_threats: Primary victim/payer (institutional/constrained)
 *   - non_state_actors: Direct target/payer (moderate/trapped)
 *   - international_legal_scholars: Analytical observer (analytical/analytical)
 *   - un_security_council: Agenda-setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.7).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable or Unwilling Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'c0f2daec-7989-4f45-822e-a5aa2f1929c8').
narrative_ontology:cs_kernel_codification('c0f2daec-7989-4f45-822e-a5aa2f1929c8', fixed_text).
narrative_ontology:cs_authority_grounding('c0f2daec-7989-4f45-822e-a5aa2f1929c8', lineage).
narrative_ontology:cs_interpretation_layer_present('c0f2daec-7989-4f45-822e-a5aa2f1929c8').
narrative_ontology:cs_reading_relation('c0f2daec-7989-4f45-822e-a5aa2f1929c8', article_51_self_defense__narrow_armed_attack_reading, influences).
narrative_ontology:cs_reading_relation('c0f2daec-7989-4f45-822e-a5aa2f1929c8', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('c0f2daec-7989-4f45-822e-a5aa2f1929c8', foundational, host_state_failure_justifies_intervention).
narrative_ontology:cs_axiom_status(host_state_failure_justifies_intervention, holdable).
narrative_ontology:cs_axiom_grounding('c0f2daec-7989-4f45-822e-a5aa2f1929c8', host_state_failure_justifies_intervention, conventional).
narrative_ontology:cs_axiom('c0f2daec-7989-4f45-822e-a5aa2f1929c8', foundational, non_state_actor_attack_triggers_self_defense).
narrative_ontology:cs_axiom_status(non_state_actor_attack_triggers_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('c0f2daec-7989-4f45-822e-a5aa2f1929c8', non_state_actor_attack_triggers_self_defense, conventional).
narrative_ontology:cs_reference_frame('c0f2daec-7989-4f45-822e-a5aa2f1929c8', post_9_11_counterterrorism_paradigm).
narrative_ontology:cs_drift_state('c0f2daec-7989-4f45-822e-a5aa2f1929c8', contemporary_international_law_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c0f2daec-7989-4f45-822e-a5aa2f1929c8', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actor_threats).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to use force in self-defense against non-state actors operating from other states, particularly when the host state is perceived as 'unable or unwilling' to address the threat. They benefit from expanded operational flexibility but face diplomatic and legal challenges.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, constrained, global).

% These states bear the cost of having their sovereignty bypassed by intervening states. They may lack the capacity to suppress non-state actors or may be unwilling to do so for political reasons, leading to interventions on their territory. Their options are to suppress the threat themselves or risk intervention.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actor_threats, payer,
    institutional, biographical, constrained, national).

% These groups are the direct targets of self-defense operations under this doctrine. They face direct military action from intervening states, regardless of the host state's consent. Their options are to cease operations or face military force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actors, payer,
    moderate, immediate, trapped, regional).

% These experts analyze the evolution and legality of the 'unable or unwilling' doctrine, debating its consistency with the UN Charter and customary international law. They influence the discourse but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% The UNSC is the primary body for authorizing the use of force. This doctrine challenges its exclusive authority, as intervening states often act unilaterally. The UNSC can condemn or legitimize such actions post-facto, but its power is often bypassed.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to respond to non-state actor threats originating from territories where the host state cannot or will not act, aiming to prevent the spread of terrorism and maintain international security.
% TRANSFER_FUNCTION: Transfers the right to use force from the UN Security Council (or the host state) to an intervening state, allowing the latter to project military power into another sovereign territory.
% ABSENT_VOICES: Host states that are genuinely unable to suppress threats but are unwilling to consent to intervention, or non-state actors who claim legitimate grievances, are often excluded from the decision-making process, leading to unilateral actions against them.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, intervening states would lose a key legal justification for unilateral counterterrorism operations abroad. This would likely lead to increased reliance on UNSC authorization, more diplomatic pressure on host states, or a return to more traditional interpretations of self-defense, significantly altering international security practices.
% FOUNDING_PROBLEM: The rise of transnational non-state actor terrorism (e.g., Al-Qaeda) operating from states that could not or would not control them, challenging traditional notions of state-on-state armed attack.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states and many security analysts attest that the problem of transnational non-state actor threats remains live. Some international legal scholars, however, contest the doctrine's necessity, arguing that existing international law provides sufficient mechanisms or that the doctrine is prone to abuse, but the core problem of non-state actor threats is widely acknowledged.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant cost to host state sovereignty and the potential for abuse. Suppression (0.70) is high because the doctrine relies on military force and diplomatic pressure to enforce its terms, often against the will of the host state. Theater ratio (0.20) is moderate; while there's a genuine security concern, the 'unable or unwilling' justification can sometimes serve as a pretext for interventions driven by other interests. Accessibility collapse (0.40) is moderate, as host states still have the option to suppress threats themselves, but this is often difficult. Resistance (0.75) is high, as the doctrine faces significant legal and political opposition from many states and scholars.
 *
 * PERSPECTIVAL GAP:
 *   Intervening states perceive this doctrine as a necessary and legitimate tool for collective security, experiencing it as a Rope or even a Mountain (a natural right). Host states, however, experience it as a Snare, as it infringes on their sovereignty and can lead to military intervention. The engine's classification as Tangled Rope reflects this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are beneficiaries (d near 0.0) as they gain expanded operational freedom. Host states and non-state actors are victims (d near 1.0) as they bear the costs of sovereignty bypass and direct military action. The UN Security Council is an agenda-setter whose authority is often bypassed, placing it in a constrained position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (ignoring sovereignty costs) or a pure Snare (ignoring the genuine security coordination function). The 'unable or unwilling' doctrine emerged to address a real problem, but its application has generated significant contestation regarding its scope and legitimacy, indicating a hybrid structure rather than a fully atrophied or purely extractive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_unwilling_threshold_ambiguity,
    'What objective criteria define a state as ''unable'' or ''unwilling'' to suppress a non-state actor threat, and who makes that determination?',
    'Development of internationally agreed-upon, verifiable metrics for state capacity and political will, and a multilateral body (e.g., UNSC) to make such determinations, rather than unilateral assessment by intervening states.',
    'Clearer criteria and multilateral determination would reduce the doctrine''s extractiveness by limiting unilateral interventions and protecting host state sovereignty, potentially shifting the classification closer to a Rope. Ambiguity allows for greater discretion and potential abuse, maintaining its Tangled Rope nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_unwilling_threshold_ambiguity, conceptual, 'Ambiguity in defining ''unable or unwilling'' status.').

omega_variable(
    sovereignty_vs_security_priority,
    'Does the imperative of international security against non-state actors legitimately override state sovereignty in cases of ''unable or unwilling'' host states, or does it set a dangerous precedent for intervention?',
    'A global consensus shift on the hierarchy of international norms, potentially through a new UN resolution or a widely ratified treaty clarifying the limits of sovereignty in the face of transnational threats.',
    'If security is universally prioritized, the doctrine''s legitimacy would increase, reducing perceived extraction. If sovereignty is reaffirmed as paramount, the doctrine would be seen as more extractive and less legitimate, pushing it towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_security_priority, preference, 'Normative conflict between state sovereignty and international security imperatives.').

omega_variable(
    doctrine_abuse_potential,
    'To what extent is the ''unable or unwilling'' doctrine used as a pretext for interventions driven by other geopolitical interests, rather than genuine self-defense?',
    'Independent, transparent investigations into the motivations and outcomes of interventions justified by this doctrine, comparing stated rationales with actual strategic objectives and long-term impacts.',
    'Evidence of widespread abuse would significantly increase the perceived extractiveness and theater ratio, potentially reclassifying the constraint as a Snare. If interventions are consistently found to align with genuine self-defense, the extractiveness might be seen as a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_abuse_potential, empirical, 'Risk of the doctrine being used as a pretext for other geopolitical interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(arti_tr_t2007, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(arti_be_t2007, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(arti_su_t2007, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, un_charter_prohibition_on_use_of_force).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 51 self-defense kernel. Its interpretation of 'unable or unwilling' directly influences the operational space of both the narrow armed attack and expansive preventive readings, as well as the broader UN Charter prohibition on the use of force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
