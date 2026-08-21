% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Reading of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the Amun-Ra polytheistic reading of divine
 *   legitimacy in ancient Egypt, where the authority of the pharaoh and the
 *   stability of the cosmic order are mediated through the established
 *   priesthood's interpretation of a multi-deity cosmology. This reading is
 *   one of several competing interpretations of the 'divine legitimacy
 *   substrate' kernel. It is characterized by distributed interpretive
 *   authority among various temple economies, with the Amun priesthood
 *   holding primary influence, and the pharaoh's power being validated by,
 *   and thus constrained by, priestly interpretations. This system
 *   accommodates regional religious variations while maintaining a central
 *   patron deity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.45).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.6).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Reading of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, 'b2f46a16-6856-45f3-955e-e4658eba4207').
narrative_ontology:cs_kernel_codification('b2f46a16-6856-45f3-955e-e4658eba4207', formalized).
narrative_ontology:cs_authority_grounding('b2f46a16-6856-45f3-955e-e4658eba4207', lineage).
narrative_ontology:cs_interpretation_layer_present('b2f46a16-6856-45f3-955e-e4658eba4207').
narrative_ontology:cs_reading_relation('b2f46a16-6856-45f3-955e-e4658eba4207', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('b2f46a16-6856-45f3-955e-e4658eba4207', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('b2f46a16-6856-45f3-955e-e4658eba4207', foundational, multi_deity_pantheon_essential_for_cosmic_order).
narrative_ontology:cs_axiom_status(multi_deity_pantheon_essential_for_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('b2f46a16-6856-45f3-955e-e4658eba4207', multi_deity_pantheon_essential_for_cosmic_order, theological).
narrative_ontology:cs_axiom('b2f46a16-6856-45f3-955e-e4658eba4207', foundational, priestly_interpretation_as_divine_will_channel).
narrative_ontology:cs_axiom_status(priestly_interpretation_as_divine_will_channel, holdable).
narrative_ontology:cs_axiom_grounding('b2f46a16-6856-45f3-955e-e4658eba4207', priestly_interpretation_as_divine_will_channel, conventional).
narrative_ontology:cs_reference_frame('b2f46a16-6856-45f3-955e-e4658eba4207', established_amun_cult_hegemony).
narrative_ontology:cs_drift_state('b2f46a16-6856-45f3-955e-e4658eba4207', late_new_kingdom_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b2f46a16-6856-45f3-955e-e4658eba4207', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, divine_kingship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the will of Amun-Ra and the multi-deity cosmology, performing rituals essential for cosmic order. Benefits from temple endowments, offerings, and political influence derived from their interpretive authority. Their identity is fused with the religious and political structure.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).

% Is divinely appointed but requires priestly validation for legitimacy. Bears the cost of maintaining temple complexes and providing offerings, and is constrained by priestly interpretations in policy and ritual. Benefits from the stability and order the system provides, but at the cost of interpretive autonomy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Flourish under the established polytheistic system, receiving offerings and endowments that support local communities and provide employment. Their economic and social structure is deeply intertwined with the temple system.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_economies, beneficiary,
    organized, generational, constrained, regional).

% Participates in rituals and provides offerings, believing in the necessity of maintaining cosmic balance through the established pantheon. Bears the economic burden of supporting the temple system and is subject to its social and moral authority. Their identity is deeply tied to the local cults and the broader cosmology.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace, payer,
    powerless, immediate, trapped, local).

% Advocate for a monotheistic system centered on Aten, directly challenging the Amun-Ra priesthood's authority. They are suppressed by the established system and their views are marginalized or actively persecuted, making their exit from the dominant belief system a high-cost, identity-threatening act.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_reformers, excluded,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, widely accepted framework for divine legitimacy and cosmic order, coordinating religious practice, political authority, and social cohesion across diverse regions and communities through a shared multi-deity cosmology.
% TRANSFER_FUNCTION: Transfers political and spiritual authority from the divine realm to the Amun-Ra priesthood (and, through them, to the pharaoh), and economic resources (offerings, endowments) from the populace and state to the temple economies.
% ABSENT_VOICES: Atenist reformers and other monotheistic or alternative religious movements are actively suppressed or marginalized; they would challenge the interpretive monopoly of the Amun-Ra priesthood and the polytheistic framework itself.
% DISAPPEARANCE_RATIONALE: If this system of divine legitimacy vanished, the pharaoh's authority would collapse, the temple economies would destabilize, and the social order would fragment as the cosmic framework for daily life dissolved. A new system of legitimation would rapidly emerge, likely through conflict.
% FOUNDING_PROBLEM: To establish a stable, unifying source of authority and cosmic order for a large, diverse kingdom, integrating regional cults under a chief patron deity (Amun-Ra) and legitimizing pharaonic rule.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and archaeological evidence from the period corroborate the need for a unifying religious and political structure following periods of fragmentation. The persistence of the Amun cult for centuries attests to its success in addressing this problem, as do the writings of scribes and foreign observers who describe the system's function.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a complex belief system across a vast kingdom, providing social cohesion and political stability (beneficiaries: priesthood, temple economies). However, it also involves significant asymmetric extraction (victims: pharaoh, common populace) in terms of resources and interpretive autonomy, and requires active enforcement to suppress alternative readings (e.g., Atenism). The metrics reflect this: moderate extractiveness and suppression are necessary to maintain the system, but the coordination function is real. Theater ratio is low, indicating the rituals and interpretations are largely functional for maintaining belief and order, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The Amun priesthood perceives this as a necessary and beneficial coordination mechanism for cosmic and social order, with their role as essential interpreters. The pharaoh, while benefiting from legitimacy, experiences the constraint as a limitation on absolute power and a drain on resources. The common populace experiences it as a fundamental, unchangeable aspect of their world, with both benefits (cosmic stability) and costs (obligations).
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood and regional temple economies are clear beneficiaries, collecting resources and wielding significant influence (low directionality). The pharaoh is a payer, bearing the costs of temple maintenance and interpretive constraints, but also a beneficiary of the legitimacy the system provides (moderate directionality). The common populace are payers, contributing offerings and labor, with limited exit options (high directionality). Atenist reformers are excluded, actively targeted by the system's suppression, making them full targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as pure extraction (Snare) by acknowledging its genuine coordination function in maintaining a complex, multi-faceted society. It also avoids mislabeling it as a pure coordination (Rope) by recognizing the significant, actively enforced extraction from the pharaoh and populace. The system's mandate (cosmic and social order) remains live, but the contestation over its 'founding problem status' highlights the ongoing tension between its coordination and extractive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_autonomy_vs_legitimacy,
    'To what extent does the pharaoh''s acceptance of priestly interpretive authority represent a genuine coordination choice for stability, versus a coerced submission to a powerful institutional actor?',
    'Comparative historical analysis of pharaonic reigns that attempted to bypass or suppress the priesthood (e.g., Akhenaten''s reign), assessing the costs and benefits of such attempts.',
    'If primarily coerced, the pharaoh''s directionality would shift further towards ''target'', increasing the effective extraction from the pharaonic seat. If a genuine coordination choice, the current directionality holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_autonomy_vs_legitimacy, conceptual, 'Ambiguity in the pharaoh''s relationship to priestly authority.').

omega_variable(
    regional_variation_impact,
    'How much does the ''Amun Polytheistic Reading'' genuinely accommodate regional variations in cult practice, versus imposing a standardized, Amun-centric interpretation?',
    'Archaeological and textual analysis of regional temple records and local cult practices, comparing them to official state-sanctioned cosmology.',
    'If accommodation is high, the constraint''s coordination function is stronger and its suppression lower for regional actors. If standardization is high, suppression is higher and the coordination function is more extractive for local cults.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_impact, empirical, 'Degree of genuine accommodation of regional religious practices.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the ''divine_legitimacy_substrate'' kernel, or a distinct constraint that merely interacts with it?',
    'Analysis of the core commitment: if the Amun-Ra system''s legitimacy claims are fundamentally about grounding political and social order in divine will, it is a reading. If it''s primarily about economic control using religious symbols, it''s a distinct constraint.',
    'If a distinct constraint, it would be reclassified as a Snare, and its relationship to the kernel would be an ''affects_constraints'' edge rather than a ''reading_relation''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirming this constraint as a reading of the divine legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(divi_tr_t60, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(divi_tr_t80, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(divi_be_t60, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(divi_be_t80, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(divi_su_t40, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(divi_su_t60, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(divi_su_t80, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_succession_protocol).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, state_resource_allocation_to_temples).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'divine_legitimacy_substrate' kernel, each with its own structural properties and classification. This reading focuses on the Amun-Ra polytheistic system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
