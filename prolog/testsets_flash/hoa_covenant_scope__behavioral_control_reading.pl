% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant: Behavioral Control Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint describes the operation of a Homeowners Association (HOA)
 *   covenant as a mechanism for enforcing aesthetic uniformity and behavioral
 *   conformity, primarily justified by the goal of maximizing property
 *   values. This reading focuses on the expansive scope of control, including
 *   subjective aesthetic judgments, lifestyle restrictions, and even speech
 *   suppression (e.g., yard signs, flags), which often extends beyond genuine
 *   externalities. The constraint is actively enforced through fines and
 *   legal action.
 *
 * KEY AGENTS:
 *   - hoa_board_members: Agenda setter (institutional/arbitrage) — administers and enforces covenants, benefits from perceived property value stability.
 *   - conformist_majority_homeowners: Beneficiary (organized/constrained) — benefits from perceived property value stability and social conformity.
 *   - nonconformist_homeowners: Payer (moderate/constrained) — bears costs of fines, legal action, and social pressure for non-compliance.
 *   - homeowners_with_marginal_aesthetics: Victim (powerless/trapped) — disproportionately targeted by enforcement, faces financial penalties and social exclusion.
 *   - property_management_company: Agenda setter (organized/mobile) — contracted to enforce covenants, benefits from HOA fees and fine collection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.45).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.7).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant: Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '895f6471-b033-4982-ac24-a808e557e91c').
narrative_ontology:cs_kernel_codification('895f6471-b033-4982-ac24-a808e557e91c', formalized).
narrative_ontology:cs_authority_grounding('895f6471-b033-4982-ac24-a808e557e91c', lineage).
narrative_ontology:cs_interpretation_layer_present('895f6471-b033-4982-ac24-a808e557e91c').
narrative_ontology:cs_reading_relation('895f6471-b033-4982-ac24-a808e557e91c', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('895f6471-b033-4982-ac24-a808e557e91c', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('895f6471-b033-4982-ac24-a808e557e91c', foundational, aesthetic_uniformity_maximizes_property_value).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_maximizes_property_value, holdable).
narrative_ontology:cs_axiom_grounding('895f6471-b033-4982-ac24-a808e557e91c', aesthetic_uniformity_maximizes_property_value, empirically_contingent).
narrative_ontology:cs_axiom('895f6471-b033-4982-ac24-a808e557e91c', secondary, behavioral_conformity_ensures_community_harmony).
narrative_ontology:cs_axiom_status(behavioral_conformity_ensures_community_harmony, holdable).
narrative_ontology:cs_axiom_grounding('895f6471-b033-4982-ac24-a808e557e91c', behavioral_conformity_ensures_community_harmony, conventional).
narrative_ontology:cs_reference_frame('895f6471-b033-4982-ac24-a808e557e91c', property_value_maximization_framework).
narrative_ontology:cs_drift_state('895f6471-b033-4982-ac24-a808e557e91c', contemporary_individual_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('895f6471-b033-4982-ac24-a808e557e91c', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, hoa_board_members).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, homeowners_with_marginal_aesthetics).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the financial burden of fines and the non-pecuniary cost of suppressed expression and lifestyle choices. Suppression (0.7) is high due to the legal enforceability of covenants, the power of the HOA board, and the difficulty of exit (selling a home). Theater ratio (0.2) is low, as enforcement is generally direct and punitive, not merely performative. Accessibility collapse (0.6) is moderate, as homeowners can technically sell and move, but this is a high-friction exit. Resistance (0.4) is present but often fragmented, as individual homeowners face an organized institutional power.
 *
 * PERSPECTIVAL GAP:
 *   HOA board members and conformist homeowners perceive the covenant as a legitimate tool for maintaining community standards and property values (closer to a Rope or even a Mountain of 'market forces'). Nonconformist homeowners and those with 'marginal aesthetics' experience it as an arbitrary, extractive Snare that curtails personal freedoms and imposes financial penalties for subjective violations. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board and conformist majority are beneficiaries (low d) as they gain from perceived property value stability and a desired aesthetic/social environment. Nonconformist homeowners and those with marginal aesthetics are victims (high d) as they bear the direct costs of fines and the indirect costs of suppressed self-expression. The property management company, as an agent of the board, also benefits from the enforcement regime.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'Rope' (pure coordination) by highlighting the asymmetric extraction and suppression. While a coordination function (maintaining property values) is claimed, the expansive scope of enforcement into subjective aesthetics and behavior, coupled with high suppression, indicates a Snare. The 'property value maximization' mandate is used to justify broad control, even when the link to actual value is tenuous or contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily a behavioral control mechanism, a coordination mechanism for shared infrastructure, or an extraction mechanism?',
    'Analysis of enforcement patterns: if enforcement disproportionately targets subjective aesthetic judgments and lifestyle choices over genuine externalities or infrastructure maintenance, this reading is corroborated. If fines primarily fund board operations rather than maintenance, the extraction reading is stronger.',
    'If the behavioral control reading is primary, the constraint is a Snare. If coordination is primary, it''s a Rope. If extraction is primary, it''s a Snare with a different beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''hoa_covenant_scope'' kernel. This ''behavioral_control_reading'' emphasizes aesthetic uniformity and conformity as property value maximization. Sibling readings include ''coordination_reading'' (shared infrastructure) and ''extraction_reading'' (fine proliferation).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal enforcement, financial penalties) or internalized (e.g., social pressure, fear of ostracism)?',
    'Post-exit suppression trajectory: if homeowners continue to self-regulate their aesthetics and behavior even after leaving the HOA, it suggests internalized suppression. If conformity immediately drops, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as homeowners carry the suppression with them. If purely structural, removing the HOA would immediately alleviate the pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in HOA covenants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hoa_covenant_scope' kernel. This 'behavioral_control_reading' focuses on aesthetic and behavioral conformity, while the 'coordination_reading' emphasizes shared infrastructure, and the 'extraction_reading' highlights fine proliferation for revenue. All three are distinct constraints linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
