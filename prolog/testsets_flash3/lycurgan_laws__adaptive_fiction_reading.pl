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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Laws: Adaptive Fiction Reading
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the Lycurgan laws of Sparta as an 'adaptive
 *   fiction' – a system publicly presented as immutable and divinely ordained
 *   (a Mountain claim), but which in practice allowed for covert adaptation
 *   and interpretation by its governing bodies (ephors and kings) to maintain
 *   stability. The immutability was a 'noble lie' that provided legitimacy
 *   and cohesion, while the actual operation was more akin to a Rope,
 *   coordinating social order through flexible, if unacknowledged,
 *   adjustments. The claimed type is 'rope' to reflect this underlying
 *   adaptive function, despite the public 'mountain' rhetoric.
 *
 * KEY AGENTS:
 *   - spartan_elite: Beneficiary (institutional/identity_locked)
 *   - ephors_and_kings: Agenda-setter (powerful/constrained)
 *   - spartan_citizens_subject_to_adaptation: Payer (moderate/identity_locked)
 *   - analytical_historians: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.35).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.6).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Laws: Adaptive Fiction Reading").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '861bbd4e-b2ac-420b-bf58-3cfa185cd3ce').
narrative_ontology:cs_kernel_codification('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', formalized).
narrative_ontology:cs_authority_grounding('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', lineage).
narrative_ontology:cs_interpretation_layer_present('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce').
narrative_ontology:cs_reading_relation('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', lycurgan_laws__sacral_fidelity_reading, influences).
narrative_ontology:cs_reading_relation('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', foundational, immutability_as_legitimizing_fiction).
narrative_ontology:cs_axiom_status(immutability_as_legitimizing_fiction, holdable).
narrative_ontology:cs_axiom_grounding('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', immutability_as_legitimizing_fiction, conventional).
narrative_ontology:cs_axiom('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', foundational, covert_adaptation_for_systemic_survival).
narrative_ontology:cs_axiom_status(covert_adaptation_for_systemic_survival, holdable).
narrative_ontology:cs_axiom_grounding('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', covert_adaptation_for_systemic_survival, instrumental).
narrative_ontology:cs_reference_frame('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', spartan_constitutional_stability).
narrative_ontology:cs_drift_state('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', late_classical_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('861bbd4e-b2ac-420b-bf58-3cfa185cd3ce', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephors_and_kings).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizens_subject_to_adaptation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability and legitimacy provided by the 'immutable' laws, which allows them to maintain their social and political order while quietly adapting the system to changing realities. Their identity is fused with the Lycurgan ideal.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_elite, beneficiary,
    institutional, generational, identity_locked, local).

% The primary interpreters and enforcers of the laws. They publicly uphold the fiction of immutability while subtly adapting the laws through interpretation and selective enforcement to manage internal and external pressures, ensuring the system's long-term survival.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephors_and_kings, agenda_setter,
    powerful, biographical, constrained, local).

% Bear the costs of the laws' adaptations, which may manifest as shifts in social expectations, economic burdens, or military obligations. They are identity-locked by their Spartan citizenship and the pervasive cultural narrative of Lycurgan perfection, making overt resistance difficult.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizens_subject_to_adaptation, payer,
    moderate, biographical, identity_locked, local).

% Analyze the historical record to discern patterns of adaptation beneath the rhetoric of immutability. They are outside the system and can observe its structural dynamics without being subject to its internal narratives.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, unifying legal and social framework for Spartan society, fostering civic virtue, military discipline, and collective identity through a shared, revered constitutional narrative.
% TRANSFER_FUNCTION: Transfers social and political stability, and the legitimacy of a 'divinely ordained' order, from the rhetorical claim of immutability to the Spartan elite and governing bodies, in exchange for quiet adaptation by the ephors and kings, with the costs of adaptation borne by the general citizenry.
% ABSENT_VOICES: Any Spartan citizen who might have questioned the 'immutability' of the laws or the legitimacy of their covert adaptation would have been silenced by the pervasive cultural narrative and the enforcement power of the ephors. Their dissent would have been framed as disloyalty to Sparta itself.
% DISAPPEARANCE_RATIONALE: If the Lycurgan laws, even as an adaptive fiction, vanished overnight, the entire Spartan social, political, and military structure would collapse. The foundational myths, the distribution of power, and the collective identity would dissolve, leading to a complete societal reorganization.
% FOUNDING_PROBLEM: To establish a stable, militarily powerful, and socially cohesive state in ancient Sparta, overcoming internal strife and external threats through a unique constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians like Plutarch and Xenophon, while often romanticizing Sparta, corroborate the foundational intent to create a unique and stable order. Modern political theorists and historians, from outside the benefiting parties, attest that the problem of maintaining state stability and identity in a changing world remains live, even if the Lycurgan solution was historically specific.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate because the adaptations, while benefiting the elite, also served to maintain a functional society, preventing outright collapse. Suppression (0.60) is high due to the pervasive cultural indoctrination and the severe penalties for dissent, which enforced the 'immutability' narrative. Theater ratio (0.70) is high because the public performance of unchangeable laws significantly outweighed the actual rigidity; much of the system's maintenance was about preserving the illusion. Accessibility collapse (0.40) is moderate, as alternatives were not entirely absent but severely constrained by the Spartan system. Resistance (0.20) is low, reflecting the effectiveness of the suppression and identity-locking mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The Spartan elite and governing bodies (ephors and kings) would perceive the laws as a legitimate, stable, and effective coordination mechanism (a Rope or even a Mountain, in their public rhetoric). Spartan citizens, while benefiting from the stability, would experience the adaptations as costs imposed by an unchallengeable authority, making it feel more extractive. Analytical historians, from an external perspective, can discern the adaptive fiction and the underlying Rope-like function.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartan elite and ephors/kings are beneficiaries, as they derive legitimacy and stability from the system, and actively manage its adaptation. Spartan citizens are payers, bearing the costs of these adaptations without direct agency. All internal actors are identity-locked by their Spartan identity, making exit unthinkable. Analytical historians are observers, outside the system's direct influence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Lycurgan laws as a pure Mountain (sacral_fidelity_reading) by highlighting the active, adaptive function beneath the rhetoric. It also avoids the Snare classification by showing a genuine, if imperfect, coordination function for the society as a whole, not just pure extraction. The high theater ratio and moderate extractiveness, coupled with active enforcement, point to a Rope that relies on a 'noble lie' for its persistence, rather than a fully atrophied Piton or a purely extractive Snare. The mandate (societal stability) is still live, but its execution involves a significant performative element.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_covert_adaptation,
    'What was the true extent and frequency of covert adaptation of the Lycurgan laws by the ephors and kings, versus genuine adherence to fixed principles?',
    'Further archaeological and textual discoveries, or re-interpretation of existing historical sources through a lens specifically looking for evidence of ''noble lies'' and institutional flexibility.',
    'If adaptation was more extensive, it strengthens the ''rope'' classification and the ''adaptive fiction'' reading. If it was minimal, it would push the classification closer to a ''tangled_rope'' or even a ''mountain'' (sacral_fidelity reading), implying greater rigidity and less internal flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_covert_adaptation, empirical, 'Ambiguity regarding the degree of actual flexibility versus rhetorical immutability.').

omega_variable(
    identity_lock_mechanism,
    'To what extent was the ''identity_locked'' status of Spartan citizens a result of genuine ideological commitment versus structural coercion and lack of alternatives?',
    'Comparative historical analysis with other ancient city-states regarding citizen mobility, dissent suppression, and educational indoctrination. Counterfactual analysis of how the system would have fared with greater exit options.',
    'If identity lock was primarily ideological, it reinforces the coordination aspect of the ''rope'' classification. If it was primarily coercive, it would increase the effective suppression and push the classification towards a ''tangled_rope'' or ''snare'' from the citizen''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Structural vs. internalized suppression mechanism for Spartan citizens.').

omega_variable(
    mandatrophy_of_immutability_fiction,
    'At what point did the ''noble lie'' of immutability cease to be an effective coordination mechanism and become purely theatrical, contributing to the system''s decline?',
    'Historical analysis of periods of significant Spartan decline, correlating with evidence of increased internal dissent, external challenges, and the failure of covert adaptations to maintain stability.',
    'If the fiction became purely theatrical and counterproductive, it would indicate a shift towards a ''piton'' classification, where the performance of immutability actively hindered necessary adaptation, leading to systemic failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_immutability_fiction, empirical, 'The point at which the adaptive fiction became a liability rather than an asset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 50, 0.63).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 100, 0.65).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 150, 0.67).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 200, 0.68).
narrative_ontology:measurement(lycu_tr_t250, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 250, 0.69).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 300, 0.7).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 150, 0.32).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 200, 0.33).
narrative_ontology:measurement(lycu_be_t250, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 250, 0.34).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 300, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 150, 0.57).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(lycu_su_t250, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 250, 0.59).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 300, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Lycurgan laws' kernel. This 'adaptive fiction' reading emphasizes covert flexibility. It influences the 'sacral fidelity' reading by challenging its premise of absolute adherence, and the 'demographic trap' reading by offering an alternative explanation for decline (failure of adaptation, not rigidity itself).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
