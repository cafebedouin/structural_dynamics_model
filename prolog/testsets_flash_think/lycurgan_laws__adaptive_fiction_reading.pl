% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Adaptive Fiction
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'adaptive fiction' reading of the Lycurgan
 *   laws, which posits that the Spartan claim of immutability and divine
 *   origin was a 'noble lie' designed to secure social stability. Beneath
 *   this rhetoric, the ruling elite (ephors and kings) engaged in covert
 *   adaptation and interpretive flexibility, allowing the system to evolve
 *   without overtly challenging its foundational myths. This reading
 *   contrasts with those emphasizing sacral fidelity or the laws' inherent
 *   rigidity leading to demographic collapse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.4).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.5).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Adaptive Fiction").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '31e4dc49-00fb-4886-8ef0-013b11ef1ee4').
narrative_ontology:cs_kernel_codification('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', fixed_text).
narrative_ontology:cs_authority_grounding('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', lineage).
narrative_ontology:cs_interpretation_layer_present('31e4dc49-00fb-4886-8ef0-013b11ef1ee4').
narrative_ontology:cs_reading_relation('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', foundational, immutability_as_political_tool).
narrative_ontology:cs_axiom_status(immutability_as_political_tool, holdable).
narrative_ontology:cs_axiom_grounding('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', immutability_as_political_tool, conventional).
narrative_ontology:cs_axiom('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', foundational, covert_adaptation_is_possible).
narrative_ontology:cs_axiom_status(covert_adaptation_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', covert_adaptation_is_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', rhetoric_of_divine_immutability).
narrative_ontology:cs_drift_state('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', spartan_decline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('31e4dc49-00fb-4886-8ef0-013b11ef1ee4', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephors_and_kings).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ruling class of Sparta, benefiting from the stability and legitimacy provided by the Lycurgan laws, even as they covertly adapted their interpretation to maintain power and respond to changing circumstances. Their power was tied to the perceived immutability of the system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_elite, agenda_setter,
    institutional, generational, constrained, national).

% The executive and religious authorities responsible for administering the Lycurgan laws. They were the primary agents of covert adaptation, interpreting the 'immutable' laws in ways that allowed the system to persist, thereby maintaining their own authority and the stability of the state.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephors_and_kings, agenda_setter,
    powerful, biographical, constrained, national).

% Lived under the strictures of the Lycurgan laws, believing in their divine origin and immutability. They bore the costs of social conformity and limited individual agency, but also benefited from the perceived stability and unique identity of Spartan society. Their identity was deeply fused with adherence to these laws.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizens, payer,
    moderate, biographical, identity_locked, national).

% The enslaved population of Laconia, whose labor supported the Spartan system. While not directly victims of the 'immutability fiction' itself, they were structurally excluded from any political discourse or benefit, and their subjugation was a foundational element of the society the laws upheld.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helots, excluded,
    powerless, generational, trapped, local).

% Scholars like Plutarch and Xenophon who documented the Lycurgan laws and their impact, often perpetuating the narrative of their divine origin and rigidity, but also providing clues to their practical application and evolution.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ancient_historians, observer,
    analytical, civilizational, analytical, universal).

% Contemporary analysts who critically examine the historical record of the Lycurgan laws, seeking to understand their true function, the mechanisms of their persistence, and the role of rhetoric versus reality in their operation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, modern_political_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, spartan_elite).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, seemingly unchanging framework for Spartan society, allowing for covert adaptation by the ruling elite to maintain social order, military discipline, and their own power in the face of evolving challenges.
% TRANSFER_FUNCTION: Transferred legitimacy and stability to the Spartan elite by presenting their rule as divinely ordained and immutable, while transferring the burden of strict adherence and limited individual freedom to the Spartan citizens.
% ABSENT_VOICES: Any individuals or groups who might have questioned the divine origin, immutability, or practical application of the laws were systematically suppressed or excluded from the political sphere. The helots, in particular, had no voice in the system that exploited them.
% DISAPPEARANCE_RATIONALE: The entire Spartan social and political order, including its unique military and communal lifestyle, was predicated on the Lycurgan laws and the belief in their immutability. If this foundational fiction and the laws themselves vanished overnight, the basis of Spartan identity and governance would collapse, leading to a complete reorganization of power and social structure.
% FOUNDING_PROBLEM: To establish a stable, disciplined, and militarily superior society in Sparta, preventing internal strife, ensuring social cohesion, and securing the state against external threats, particularly from its subjugated helot population.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians like Plutarch and Xenophon describe the laws' intent and effect, corroborating the founding problem. Modern political theorists analyze the historical context and the system's eventual decline, supporting the view that the specific founding problems are no longer live, though the constraint's persistence mechanisms are still relevant for study.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).
:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the cost of maintaining the fiction and the limited agency of citizens, but not as high as a truly rigid, unadaptable system. Suppression (0.5) is also moderate, as the fiction was enforced, but the covert adaptation provided a release valve. The theater ratio (0.7) is high, as the 'noble lie' is inherently performative. Accessibility collapse (0.8) is high due to ideological and structural barriers to alternatives, while resistance (0.2) is low, reflecting the system's long-term stability. The claimed type is 'rope' due to the underlying institutional flexibility, even if covert.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spartan elite, the adaptive fiction was a necessary and effective tool for governance, a 'rope' that coordinated society. From the perspective of the citizens, it was a more extractive 'tangled rope' or 'snare' that demanded adherence to a rigid ideal while their leaders quietly adapted. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartan elite and the ephors/kings are beneficiaries and agenda-setters, as they maintained power and stability through the adaptive fiction. Spartan citizens are payers, bearing the costs of conformity and limited freedom, but also benefiting from the system's stability. Helots are excluded, bearing the ultimate costs of the system but not directly by the 'immutability' claim itself. Ancient and modern observers provide analytical perspectives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    noble_lie_vs_genuine_belief,
    'To what extent was the claim of Lycurgan immutability a deliberate ''noble lie'' by the elite, versus a genuinely held belief by all strata of Spartan society?',
    'Archaeological evidence of elite behavior, re-interpretation of ancient texts for subtle cues of elite cynicism, or comparative analysis with other historical ''noble lies'' and their social reception.',
    'If primarily a deliberate lie, the constraint''s effective extractiveness and suppression are higher, as it represents a conscious manipulation. If a widely held genuine belief, the constraint''s persistence is more deeply internalized, making it more akin to a ''mountain'' of social consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(noble_lie_vs_genuine_belief, conceptual, 'Ambiguity regarding the elite''s intent behind the immutability claim.').

omega_variable(
    extent_of_covert_adaptation,
    'What was the true extent and frequency of covert adaptation and interpretive flexibility by Spartan authorities, versus periods of genuine, unyielding rigidity?',
    'Detailed historical and archaeological analysis of specific legal and social changes over time, comparing them against the stated ''immutable'' principles. Examination of judicial decisions by ephors and kings for evidence of interpretive shifts.',
    'Greater evidence of covert adaptation would strengthen the ''rope'' classification, highlighting the system''s functional flexibility. Less adaptation would push the classification towards ''tangled_rope'' or even ''snare'', emphasizing the costs of rigidity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extent_of_covert_adaptation, empirical, 'Uncertainty about the degree of actual flexibility within the ''immutable'' system.').

omega_variable(
    fiction_contribution_to_stability_or_decline,
    'How much did the ''noble lie'' of immutability contribute to Sparta''s long-term stability, versus its eventual demographic and political decline?',
    'Counterfactual historical analysis, comparing Sparta''s trajectory with other city-states that adopted more flexible or rigid constitutional frameworks. Modeling the impact of different levels of adaptation on social cohesion and military effectiveness.',
    'If the fiction primarily enabled stability, it reinforces the ''rope'' aspect. If it ultimately masked unaddressed structural problems leading to decline, it highlights the ''snare'' aspect of the system, where the fiction prevented necessary reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiction_contribution_to_stability_or_decline, empirical, 'The dual role of the immutability fiction in Spartan history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 100, 0.68).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 200, 0.7).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 300, 0.72).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.71).
narrative_ontology:measurement(lycu_tr_t500, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 500, 0.7).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 300, 0.42).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.41).
narrative_ontology:measurement(lycu_be_t500, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 500, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 300, 0.52).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.51).
narrative_ontology:measurement(lycu_su_t500, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 500, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
