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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint story presents the 'adaptive fiction' reading of the
 *   Lycurgan laws, arguing that their proclaimed immutability was a 'noble
 *   lie' that masked a significant degree of practical adaptation and
 *   interpretation by Spartan magistrates (ephors and kings). The laws, while
 *   presented as a Mountain, functioned more like a Rope, providing a stable
 *   framework that allowed for flexibility. The demographic decline of Sparta
 *   is attributed to enforcement failures and external pressures, rather than
 *   the inherent rigidity of the laws themselves. This reading emphasizes the
 *   institutional capacity for covert adaptation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.25).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.4).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Laws: Adaptive Fiction Reading").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '71d897b2-64b4-44ad-bd98-63fdbaf224d5').
narrative_ontology:cs_kernel_codification('71d897b2-64b4-44ad-bd98-63fdbaf224d5', formalized).
narrative_ontology:cs_authority_grounding('71d897b2-64b4-44ad-bd98-63fdbaf224d5', lineage).
narrative_ontology:cs_interpretation_layer_present('71d897b2-64b4-44ad-bd98-63fdbaf224d5').
narrative_ontology:cs_reading_relation('71d897b2-64b4-44ad-bd98-63fdbaf224d5', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('71d897b2-64b4-44ad-bd98-63fdbaf224d5', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('71d897b2-64b4-44ad-bd98-63fdbaf224d5', foundational, immutability_as_legitimizing_fiction).
narrative_ontology:cs_axiom_status(immutability_as_legitimizing_fiction, holdable).
narrative_ontology:cs_axiom_grounding('71d897b2-64b4-44ad-bd98-63fdbaf224d5', immutability_as_legitimizing_fiction, conventional).
narrative_ontology:cs_axiom('71d897b2-64b4-44ad-bd98-63fdbaf224d5', foundational, covert_adaptation_as_governance_mechanism).
narrative_ontology:cs_axiom_status(covert_adaptation_as_governance_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('71d897b2-64b4-44ad-bd98-63fdbaf224d5', covert_adaptation_as_governance_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('71d897b2-64b4-44ad-bd98-63fdbaf224d5', functional_adaptability_under_rhetoric).
narrative_ontology:cs_drift_state('71d897b2-64b4-44ad-bd98-63fdbaf224d5', late_spartan_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('71d897b2-64b4-44ad-bd98-63fdbaf224d5', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_ephors).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_citizenry).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, constitutional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, noble_lie_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ephors, as annually elected magistrates, held significant interpretive power over the Lycurgan laws, allowing for practical adaptation and policy adjustments under the guise of upholding tradition. They benefited from the stability and legitimacy conferred by the 'immutable' laws while exercising de facto flexibility.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_ephors, agenda_setter,
    institutional, biographical, constrained, local).

% The dual kings, while nominally bound by the laws, also participated in their interpretation and application, particularly in military and religious spheres. Their authority was bolstered by the perceived divine origin of the laws, even as they navigated practical governance through interpretive flexibility.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_kings, agenda_setter,
    institutional, generational, constrained, local).

% The Spartan citizens (Homoioi) benefited from the social cohesion and stability provided by the Lycurgan system, believing in its divine and immutable nature. This belief fostered a strong collective identity and commitment to the Spartan way of life, even as the laws were subtly adapted over time.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizenry, beneficiary,
    organized, generational, identity_locked, local).

% The Helots, an enslaved population, were fundamentally excluded from the Lycurgan system's benefits and its interpretive processes. Their labor supported the Spartan way of life, but they had no voice in the laws that governed them, experiencing the system as pure extraction and suppression.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helots, excluded,
    powerless, generational, trapped, local).

% Modern historians and political theorists analyze the Lycurgan system, discerning the gap between its proclaimed immutability and its actual adaptive practice. They interpret the 'noble lie' as a mechanism for maintaining social order and legitimacy.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Lycurgan laws coordinated Spartan society around a shared ideal of military discipline, communal living, and civic virtue, providing a stable framework for governance and social reproduction.
% TRANSFER_FUNCTION: The system transferred authority and legitimacy to the Spartan ruling class (ephors, kings, Gerousia) by grounding their power in a divinely inspired, immutable legal code. It also transferred social stability and identity to the citizenry in exchange for strict adherence to communal norms.
% ABSENT_VOICES: The Helots, who bore the brunt of the system's extractive and suppressive elements, were entirely excluded from any discourse about the laws' nature or adaptation. Their perspective would highlight the coercive foundation beneath the 'noble lie'.
% DISAPPEARANCE_RATIONALE: If the Lycurgan laws and the belief in their immutability vanished overnight, the entire Spartan social, political, and military structure would collapse. The authority of the ephors and kings would be undermined, the communal lifestyle would dissolve, and the Helot population would likely revolt, leading to a complete societal reorganization.
% FOUNDING_PROBLEM: The laws were purportedly established by Lycurgus to address internal strife, social inequality, and military weakness in early Sparta, aiming to create a stable, disciplined, and militarily superior society.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians like Plutarch and Xenophon, though writing centuries later, corroborate the narrative of Lycurgus addressing foundational problems. Modern historical scholarship, while questioning the historicity of Lycurgus as a single figure, generally agrees that the laws emerged to solve real societal challenges, even if their 'immutability' was a later construct.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.25) is relatively low because the laws genuinely coordinated Spartan society and provided benefits to the citizenry, not just the rulers. Suppression (0.4) reflects the need for active enforcement to maintain the social order and the Helot system, but it's not overwhelmingly high for the citizen body. The high theater ratio (0.6) is central to this reading: the performance of immutability was crucial for legitimacy, even as the actual practice involved significant interpretation and adaptation. The slight increase in extractiveness and suppression over time, followed by a slight decrease, reflects periods of internal and external stress where the 'fiction' was harder to maintain, requiring more overt enforcement, before a return to a more stable, albeit still adaptive, equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spartan ruling class, the laws were a flexible instrument for governance, cloaked in the rhetoric of immutability. From the perspective of the citizenry, they were genuinely immutable and divinely inspired, providing a stable foundation for their lives. The analytical observer (historical scholars) sees the structural gap between these perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartan ephors and kings, as agenda-setters, benefited from the stability and legitimacy of the laws while exercising interpretive flexibility (low directionality). The Spartan citizenry were beneficiaries, gaining social cohesion and identity (low directionality). The Helots, though not directly 'victims' of the laws' adaptation, were victims of the overall system that the laws legitimized, and were entirely excluded from the interpretive process (high directionality, though not explicitly listed as victims of *this* constraint's operation, but of the broader system).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Lycurgan laws as a pure Snare or a rigid Mountain. By highlighting the covert adaptation, it shows that the constraint retained a coordination function (Rope-like) even as its public presentation was theatrical (Piton-like in its performativity). The mandate (social cohesion, military strength) was maintained through flexibility, not rigid adherence, suggesting the constraint did not fully atrophy but rather evolved its operational mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_covert_adaptation,
    'What was the precise extent and mechanism of covert adaptation of the Lycurgan laws by Spartan magistrates?',
    'Discovery of new archaeological or textual evidence detailing specific instances of legal interpretation, judicial rulings, or policy shifts that deviated from literal readings of the laws.',
    'Greater evidence of adaptation would strengthen the ''Rope'' classification and the ''adaptive fiction'' reading, while less evidence would push towards a more ''Mountain'' or ''Tangled Rope'' classification, depending on the degree of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_covert_adaptation, empirical, 'Empirical uncertainty regarding the degree of flexibility in the Lycurgan system.').

omega_variable(
    noble_lie_intentionality,
    'Was the ''noble lie'' of immutability a conscious, deliberate strategy by the Spartan elite, or an emergent cultural phenomenon?',
    'Discovery of explicit philosophical or political treatises from ancient Sparta discussing the strategic use of such fictions, or comparative analysis with other ancient constitutional systems.',
    'If deliberate, it reinforces the ''agenda-setter'' role of the ephors/kings and the ''theater'' aspect. If emergent, it suggests a more diffuse, cultural mechanism for maintaining the constraint, potentially shifting the classification towards a more ''Mountain''-like (cultural inertia) or ''Piton''-like (unconscious performance) type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(noble_lie_intentionality, conceptual, 'Conceptual uncertainty regarding the intentionality behind the ''noble lie''.').

omega_variable(
    demographic_decline_causality,
    'To what extent was Spartan demographic decline caused by the rigidity of the Lycurgan laws versus other factors like war, disease, or economic changes?',
    'Detailed demographic modeling and historical analysis, controlling for various external and internal factors, to isolate the impact of legal structures.',
    'If rigidity was a primary cause, it would strengthen the ''demographic_trap_reading'' and push this constraint towards a ''Snare'' or ''Tangled Rope'' due to its negative consequences. If other factors dominate, it supports this reading''s emphasis on adaptive capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_causality, empirical, 'Empirical uncertainty regarding the causes of Spartan demographic decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(lycu_tr_t75, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 150, 0.6).
narrative_ontology:measurement(lycu_tr_t225, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 225, 0.65).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 300, 0.6).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lycu_be_t75, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement(lycu_be_t225, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 225, 0.27).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 300, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lycu_su_t75, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 75, 0.38).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 150, 0.4).
narrative_ontology:measurement(lycu_su_t225, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 225, 0.42).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 300, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Lycurgan laws' kernel. This 'adaptive fiction' reading emphasizes the covert flexibility and the 'noble lie' of immutability, contrasting with readings that focus on rigidity or sacred adherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
