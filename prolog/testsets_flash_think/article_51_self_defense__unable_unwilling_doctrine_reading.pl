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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable or Unwilling Doctrine
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'unable or unwilling' doctrine as an
 *   interpretation of Article 51 of the UN Charter, which permits
 *   self-defense. This reading asserts that a state may use force in
 *   self-defense against non-state actors operating from another state's
 *   territory if the host state is unwilling or unable to suppress the
 *   threat. It emerged prominently in post-9/11 counterterrorism discourse,
 *   creating a hybrid constraint that aims to coordinate responses to
 *   transnational threats but does so by extracting from the sovereignty of
 *   host states. The claimed type (tangled_rope) reflects this dual function,
 *   while the metrics capture its increasingly extractive and suppressive
 *   operation over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.7).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable or Unwilling Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '3294d826-24dc-4a9e-9271-533062327ba4').
narrative_ontology:cs_kernel_codification('3294d826-24dc-4a9e-9271-533062327ba4', fixed_text).
narrative_ontology:cs_authority_grounding('3294d826-24dc-4a9e-9271-533062327ba4', lineage).
narrative_ontology:cs_interpretation_layer_present('3294d826-24dc-4a9e-9271-533062327ba4').
narrative_ontology:cs_reading_relation('3294d826-24dc-4a9e-9271-533062327ba4', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('3294d826-24dc-4a9e-9271-533062327ba4', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('3294d826-24dc-4a9e-9271-533062327ba4', foundational, self_defense_against_non_state_actors_is_permissible).
narrative_ontology:cs_axiom_status(self_defense_against_non_state_actors_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('3294d826-24dc-4a9e-9271-533062327ba4', self_defense_against_non_state_actors_is_permissible, conventional).
narrative_ontology:cs_axiom('3294d826-24dc-4a9e-9271-533062327ba4', foundational, host_state_failure_waives_sovereignty_immunity).
narrative_ontology:cs_axiom_status(host_state_failure_waives_sovereignty_immunity, holdable).
narrative_ontology:cs_axiom_grounding('3294d826-24dc-4a9e-9271-533062327ba4', host_state_failure_waives_sovereignty_immunity, conventional).
narrative_ontology:cs_reference_frame('3294d826-24dc-4a9e-9271-533062327ba4', post_9_11_counterterrorism_framework).
narrative_ontology:cs_drift_state('3294d826-24dc-4a9e-9271-533062327ba4', contemporary_international_law, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3294d826-24dc-4a9e-9271-533062327ba4', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that possess counterterrorism capabilities and mandates, and invoke this doctrine to justify unilateral military action against non-state actors in other states. They benefit from the expanded scope of self-defense, allowing them to address perceived threats directly.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates, agenda_setter,
    institutional, generational, mobile, global).

% States from whose territory non-state armed groups operate, and which are deemed 'unwilling or unable' to suppress these threats. They bear the cost of interventions on their sovereign territory, including potential civilian casualties and destabilization, and experience an erosion of their sovereignty.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_non_state_actors, payer,
    institutional, generational, constrained, national).

% The primary body responsible for maintaining international peace and security. Its authorization is traditionally required for the use of force, but this doctrine allows unilateral action, often bypassing the UNSC due to political deadlock or perceived urgency. It observes and debates the legality of such actions.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% The direct targets of military action justified by this doctrine. They are not recognized as legal actors in the international law debate and have no voice in the formulation or interpretation of self-defense norms, despite being the object of the force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, excluded,
    powerless, immediate, trapped, local).

% Academics and legal experts who analyze, debate, and critique the doctrine's legality, implications for state sovereignty, and consistency with the UN Charter. Their work influences, but does not directly determine, state practice or legal outcomes.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal basis for states to coordinate responses to cross-border non-state actor threats when the territorial state is perceived as failing to address them, thereby preventing safe havens for such groups.
% TRANSFER_FUNCTION: Transfers the primary right to use force against non-state actors from the host state (whose sovereignty is bypassed) to the intervening state (claiming self-defense), effectively reallocating the burden and authority for security provision.
% ABSENT_VOICES: Non-state armed groups, as the direct targets, are entirely excluded from the legal discourse. Additionally, the populations within host states, who often bear the brunt of interventions, have limited direct voice in the international legal debate.
% DISAPPEARANCE_RATIONALE: If the 'unable or unwilling' doctrine vanished, intervening states would face significant legal and political hurdles in responding to non-state actor threats originating from other states. This would force a major re-evaluation of counterterrorism strategies, potentially leading to increased reliance on UNSC authorization (which is often subject to veto) or alternative, less overt forms of intervention, fundamentally altering international security dynamics.
% FOUNDING_PROBLEM: The rise of transnational non-state armed groups (e.g., Al-Qaeda, ISIS) capable of launching attacks from states that either lacked the capacity or political will to suppress them, creating a perceived gap in the traditional state-centric framework of self-defense.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states consistently assert the problem is live, citing ongoing transnational terrorist threats. While some international law scholars acknowledge the challenge, many host states and other scholars contest the doctrine's necessity, arguing it's a pretext for intervention and that existing international law (e.g., UNSC authorization, consent) is sufficient. The debate is ongoing in academic and diplomatic forums.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.65) is substantial because the doctrine permits unilateral action that bypasses the host state's sovereignty, imposing costs without consent. Suppression (0.70) is high as it actively overrides the host state's control over its territory and the traditional requirement for UNSC authorization. The theater ratio (0.15) is low, indicating that the doctrine is genuinely invoked and acted upon, not merely performative. Accessibility collapse (0.60) is moderate, as it reduces the intervening state's alternatives (e.g., waiting for host state consent or UNSC resolution) but doesn't eliminate all other diplomatic or coercive options. Resistance (0.55) is moderate, as many states and scholars actively contest its legality and implications.
 *
 * PERSPECTIVAL GAP:
 *   Intervening states perceive this doctrine as a necessary and legitimate adaptation of self-defense to modern threats, enabling effective counterterrorism. Host states, however, often view it as an illegitimate erosion of their sovereignty and a pretext for intervention. The UN Security Council and international law scholars are divided, reflecting the tension between security imperatives and foundational principles of international law.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are clear beneficiaries, gaining the legal justification for unilateral action. Host states are victims, as their sovereignty is directly challenged and they bear the costs of intervention. The UN Security Council is an observer, often sidelined but still a forum for debate. Non-state armed groups are excluded, being the object of the force rather than participants in the legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transnational non-state actor threats from 'unwilling or unable' states) is still considered 'live' by proponents of the doctrine. Therefore, the constraint is not currently experiencing mandatrophy, though its continued necessity and legitimacy are 'contested' by other actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_unwilling_threshold_ambiguity,
    'What objective criteria define a state as ''unwilling'' or ''unable'' to suppress non-state actor threats, and who makes this determination?',
    'Development of clear, internationally agreed-upon legal standards or a consistent pattern of UN Security Council pronouncements on specific cases. Without this, the determination remains unilateral and potentially self-serving.',
    'If objective criteria are established, the doctrine''s application could become more legitimate and less extractive. If it remains subjective, it risks being a pretext for intervention, increasing its effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_unwilling_threshold_ambiguity, conceptual, 'Ambiguity in the criteria for invoking the ''unable or unwilling'' doctrine.').

omega_variable(
    sovereignty_erosion_vs_security_imperative,
    'Is the ''unable or unwilling'' doctrine a necessary and legitimate evolution of international law to address modern security threats, or does it represent an unacceptable erosion of state sovereignty and the UN Charter''s prohibition on the use of force?',
    'A shift in international legal consensus, either through a new UN General Assembly resolution, a landmark International Court of Justice ruling, or a sustained, widespread change in state practice and opinio juris.',
    'If deemed a legitimate evolution, its classification as a tangled_rope might shift towards a more coordination-focused rope. If deemed an illegitimate erosion, it would solidify its classification as a snare, with higher effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_erosion_vs_security_imperative, preference, 'Fundamental normative disagreement on the doctrine''s legality and desirability.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''article_51_self_defense'' kernel. What specific structural elements would change if a sibling reading were adopted?',
    'Analysis of the legal and practical implications of adopting the ''narrow_armed_attack_reading'' (requiring state attribution) or the ''expansive_preventive_reading'' (allowing preemptive force).',
    'Adopting the ''narrow_armed_attack_reading'' would significantly reduce the scope for unilateral intervention, increasing host state sovereignty and reducing extraction. Adopting the ''expansive_preventive_reading'' would further expand intervening states'' ability to use force, potentially increasing extraction and suppression on host states by lowering the threshold for intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documenting this constraint as a specific reading of the Article 51 self-defense kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(arti_tr_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(arti_tr_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2009, 0.13).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2013, 0.14).
narrative_ontology:measurement(arti_tr_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(arti_tr_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2021, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(arti_be_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(arti_be_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2009, 0.61).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(arti_be_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2017, 0.64).
narrative_ontology:measurement(arti_be_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2021, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(arti_su_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(arti_su_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2009, 0.67).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2013, 0.69).
narrative_ontology:measurement(arti_su_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2017, 0.7).
narrative_ontology:measurement(arti_su_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2021, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, international_humanitarian_law_compliance).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, un_charter_prohibition_on_use_of_force).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
