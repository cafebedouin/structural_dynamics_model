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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Divine Legitimacy via Amun-Ra Polytheistic Interpretation
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the system of divine legitimacy in ancient
 *   Egypt, specifically through the lens of the Amun-Ra polytheistic
 *   interpretation. Legitimacy flows from the gods, mediated by an
 *   established priesthood, particularly that of Amun-Ra, who interpret a
 *   multi-deity cosmology. This system accommodates regional variations while
 *   constraining the pharaoh's direct divine authority and benefiting
 *   powerful temple economies. It is a 'tangled rope' because it provides
 *   genuine coordination (social and political stability) but also involves
 *   significant, asymmetric extraction by the priesthood.
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
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Divine Legitimacy via Amun-Ra Polytheistic Interpretation").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '5f127f00-f85c-4f7f-b2dd-861940501843').
narrative_ontology:cs_kernel_codification('5f127f00-f85c-4f7f-b2dd-861940501843', formalized).
narrative_ontology:cs_authority_grounding('5f127f00-f85c-4f7f-b2dd-861940501843', lineage).
narrative_ontology:cs_interpretation_layer_present('5f127f00-f85c-4f7f-b2dd-861940501843').
narrative_ontology:cs_reading_relation('5f127f00-f85c-4f7f-b2dd-861940501843', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('5f127f00-f85c-4f7f-b2dd-861940501843', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('5f127f00-f85c-4f7f-b2dd-861940501843', foundational, divine_power_distributed_among_pantheon).
narrative_ontology:cs_axiom_status(divine_power_distributed_among_pantheon, holdable).
narrative_ontology:cs_axiom_grounding('5f127f00-f85c-4f7f-b2dd-861940501843', divine_power_distributed_among_pantheon, theological).
narrative_ontology:cs_axiom('5f127f00-f85c-4f7f-b2dd-861940501843', foundational, priestly_mediation_essential_for_legitimacy).
narrative_ontology:cs_axiom_status(priestly_mediation_essential_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5f127f00-f85c-4f7f-b2dd-861940501843', priestly_mediation_essential_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('5f127f00-f85c-4f7f-b2dd-861940501843', established_amun_ra_hegemony).
narrative_ontology:cs_drift_state('5f127f00-f85c-4f7f-b2dd-861940501843', late_new_kingdom, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5f127f00-f85c-4f7f-b2dd-861940501843', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, common_worshippers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters of the multi-deity cosmology, especially concerning Amun-Ra. They derive immense wealth and political influence from their role, controlling vast temple estates and validating the pharaoh's divine mandate. Their identity is fused with this interpretive tradition.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).

% Requires priestly validation for their divine legitimacy and access to the sacred power of Amun-Ra. While benefiting from the system's stability, they are constrained by the priesthood's interpretive authority, which can challenge or undermine their rule if not appeased. Attempts to bypass this (e.g., Akhenaten) are met with severe resistance.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Benefit from the decentralized nature of polytheistic worship, maintaining local cults and collecting offerings. They are aligned with the Amun priesthood's interpretive authority, which ensures their continued economic and social relevance within their regions.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_economies, beneficiary,
    organized, generational, constrained, regional).

% Participate in rituals and make offerings to various deities, including Amun-Ra, as interpreted by the priesthood. They bear the economic costs of temple maintenance and offerings, and their spiritual access is mediated by the priestly class. Their options are limited by social norms and the perceived necessity of divine favor.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_worshippers, payer,
    powerless, immediate, trapped, local).

% Advocates for a monotheistic worship of Aten, directly challenging the Amun priesthood's authority. They are actively suppressed by the established system, their beliefs deemed heretical and their practices dismantled. Their identity is fused with their reformist ideology, making exit from their beliefs unthinkable.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_reformers, excluded,
    moderate, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, widely accepted framework for divine legitimacy, integrating diverse regional cults under a coherent multi-deity cosmology with Amun-Ra as chief patron, thereby legitimizing pharaonic rule and social order.
% TRANSFER_FUNCTION: Transfers spiritual authority, political influence, and material wealth (offerings, land) from the pharaoh and common worshippers to the Amun priesthood and regional temple economies, in exchange for divine validation and cosmic order.
% ABSENT_VOICES: Atenist reformers and other monotheistic or alternative religious movements are actively suppressed; they would argue for a direct, unmediated relationship with a single deity, bypassing the established priestly class and its interpretive monopoly.
% DISAPPEARANCE_RATIONALE: If this interpretive framework vanished, the pharaoh's divine legitimacy would collapse, leading to political instability and potential civil war. The vast temple economies would lose their grounding, and the social order, deeply intertwined with religious practice, would fragment. A new system of legitimacy would rapidly emerge, likely through conflict.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned political and social order across a diverse kingdom with multiple regional cults, ensuring the pharaoh's authority while accommodating local religious practices.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, inscriptions, and archaeological evidence from outside the Amun priesthood (e.g., royal decrees, administrative texts) corroborate the ongoing need for a unifying religious framework to maintain stability and legitimize rule, even as the specific interpretations evolve.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate because the priesthood collects substantial wealth and influence, but the system also provides genuine social and political stability. Suppression (0.6) is high due to the active enforcement against rival interpretations (e.g., Atenism) and the lack of alternatives for common worshippers. Theater ratio (0.2) is low, as the rituals and interpretations are genuinely believed to maintain cosmic order, though they also serve the priesthood's interests. Accessibility collapse is high (0.7) because the interpretive monopoly makes alternative spiritual paths difficult to access or even conceive of for most, and resistance is moderate (0.3) as challenges are rare but historically significant.
 *
 * PERSPECTIVAL GAP:
 *   The pharaoh experiences this constraint as a necessary, if sometimes burdensome, source of legitimacy, while the priesthood experiences it as the natural order from which they derive their power. Common worshippers perceive it as the only path to divine favor, bearing its costs without direct agency. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood and regional temple economies are clear beneficiaries (low d) as they gain wealth and power from their interpretive role. The pharaoh is a payer (high d) in terms of constrained authority and resources, but also a beneficiary (low d) of the system's legitimizing function. Common worshippers are primarily payers (high d) through offerings and mediated access. Atenist reformers are excluded, actively suppressed, and thus experience the constraint as highly extractive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priestly_vs_pharaonic_authority,
    'To what extent is the pharaoh''s authority truly constrained by the priesthood, versus merely using the priesthood as a tool for control?',
    'Analysis of historical periods where pharaohs attempted to assert direct divine authority or suppress priestly power (e.g., Akhenaten''s reign), examining the long-term success and societal impact of such attempts.',
    'If the pharaoh is genuinely constrained, the extraction from the pharaoh is higher. If the priesthood is merely a tool, the pharaoh is more of a beneficiary, and the constraint is more of a snare for common worshippers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_vs_pharaonic_authority, empirical, 'Ambiguity in the power dynamic between pharaoh and priesthood.').

omega_variable(
    natural_vs_constructed_cosmology,
    'Is the multi-deity cosmology a ''natural'' emergent belief system, or a constructed narrative maintained by the priesthood for its own benefit?',
    'Comparative anthropological studies of other ancient polytheistic systems and their evolution, alongside textual analysis of priestly writings versus popular religious practices.',
    'If ''natural'', the constraint leans more towards a Mountain or Rope. If ''constructed'', it leans more towards a Snare or Tangled Rope, with higher effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_cosmology, conceptual, 'Whether the underlying cosmology is natural or constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal/social barriers to alternative worship) or internalized (e.g., common worshippers'' belief in the necessity of priestly mediation)?',
    'Archaeological evidence of underground or heterodox cults, and analysis of texts expressing dissent or alternative spiritual paths. If such practices persisted despite official suppression, it suggests a stronger structural component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. If purely structural, removing the priestly power would immediately free worshippers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for common worshippers.').


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

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
