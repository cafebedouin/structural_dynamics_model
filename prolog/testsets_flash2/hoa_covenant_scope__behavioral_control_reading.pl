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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant: Behavioral Control Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story represents the 'behavioral control' reading of an
 *   HOA covenant, where the primary function is perceived to be the
 *   enforcement of aesthetic uniformity and behavioral conformity as a
 *   strategy for property value maximization. This reading emphasizes the
 *   suppressive and extractive aspects of the covenant, particularly for
 *   homeowners whose choices deviate from the established norms. It is one of
 *   several possible readings of the same underlying kernel (the HOA covenant
 *   itself).
 *
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
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'beba3504-650e-4d09-bd5e-4f5446e46c6f').
narrative_ontology:cs_kernel_codification('beba3504-650e-4d09-bd5e-4f5446e46c6f', fixed_text).
narrative_ontology:cs_authority_grounding('beba3504-650e-4d09-bd5e-4f5446e46c6f', practice).
narrative_ontology:cs_interpretation_layer_present('beba3504-650e-4d09-bd5e-4f5446e46c6f').
narrative_ontology:cs_reading_relation('beba3504-650e-4d09-bd5e-4f5446e46c6f', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('beba3504-650e-4d09-bd5e-4f5446e46c6f', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('beba3504-650e-4d09-bd5e-4f5446e46c6f', foundational, aesthetic_homogeneity_maximizes_property_value).
narrative_ontology:cs_axiom_status(aesthetic_homogeneity_maximizes_property_value, holdable).
narrative_ontology:cs_axiom_grounding('beba3504-650e-4d09-bd5e-4f5446e46c6f', aesthetic_homogeneity_maximizes_property_value, empirically_contingent).
narrative_ontology:cs_axiom('beba3504-650e-4d09-bd5e-4f5446e46c6f', foundational, collective_aesthetic_trumps_individual_expression).
narrative_ontology:cs_axiom_status(collective_aesthetic_trumps_individual_expression, holdable).
narrative_ontology:cs_axiom_grounding('beba3504-650e-4d09-bd5e-4f5446e46c6f', collective_aesthetic_trumps_individual_expression, conventional).
narrative_ontology:cs_reference_frame('beba3504-650e-4d09-bd5e-4f5446e46c6f', uniform_aesthetic_community).
narrative_ontology:cs_drift_state('beba3504-650e-4d09-bd5e-4f5446e46c6f', contemporary_individual_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('beba3504-650e-4d09-bd5e-4f5446e46c6f', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the covenant rules, interprets ambiguities, and levies fines. Benefits from perceived order and property value stability, which reinforces its authority. Its members are often homeowners themselves, but their role grants them disproportionate power.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, biographical, constrained, local).

% Actively support strict enforcement of aesthetic and behavioral rules, believing it protects their property values and lifestyle preferences. They benefit from the conformity and the suppression of 'undesirable' elements, often having a direct line to the HOA board.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    organized, biographical, mobile, local).

% Passively benefits from the perceived stability and uniformity, which they believe contributes to property values. They generally comply with rules to avoid conflict, even if they don't actively participate in enforcement, and enjoy the 'curb appeal' the covenant creates.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of enforcement through fines, legal fees, and forced changes to their property or lifestyle. They often feel their individual expression is suppressed and that the rules are arbitrary or selectively enforced. Their exit options are limited by their property ownership.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, biographical, identity_locked, local).

% Specifically targeted by rules against yard signs, flags, specific paint colors, or landscaping choices that deviate from the norm. They experience direct extraction of their right to self-expression on their own property, with high costs for non-compliance.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates, payer,
    powerless, biographical, identity_locked, local).

% Hired by the HOA board to administer and enforce the covenant. Benefits financially from the ongoing enforcement activities, including processing fines and managing disputes. Has an incentive to maintain a complex rule structure that requires its services.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, property_management_company, agenda_setter,
    institutional, generational, arbitrage, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a consistent aesthetic and behavioral standard across the community, aiming to prevent perceived 'blight' and ensure shared property values are protected.
% TRANSFER_FUNCTION: Transfers the right to individual aesthetic and behavioral expression from nonconformist homeowners to the collective (represented by the HOA board and conformist majority), in exchange for perceived property value stability.
% ABSENT_VOICES: Future homeowners who might desire more diverse aesthetic choices or less restrictive living conditions are absent from the current covenant-setting process. Also, those who value individual liberty over collective aesthetic uniformity are often marginalized or forced out.
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight, individual property owners would immediately begin to exercise greater autonomy over their homes and yards. Aesthetic uniformity would quickly erode, leading to a more diverse but potentially less 'curated' neighborhood appearance. Property values might fluctuate based on individual preferences rather than collective standards.
% FOUNDING_PROBLEM: To prevent individual property owners from making choices that could negatively impact the collective aesthetic and, by extension, the property values of all homes in the development.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and many long-term residents attest that the problem of maintaining property values and community standards is still live, citing examples of potential aesthetic degradation if rules were relaxed. However, nonconformist homeowners and legal scholars argue that the problem is over-solved, and the covenant now serves primarily as a tool for control rather than genuine value protection.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate, reflecting the cost borne by nonconformists in terms of fines, legal fees, and suppressed self-expression. Suppression (0.7) is high because the covenant actively restricts choices and punishes deviations, requiring continuous enforcement. The theater ratio (0.2) is low, as the enforcement is genuinely aimed at achieving the stated goal of conformity, even if the justification is contested. Accessibility collapse is moderate (0.6) as homeowners are 'locked in' by property ownership, but some forms of subtle resistance or legal challenge remain. Resistance (0.4) is present but often fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the HOA board and conformist majority, the covenant is a necessary 'rope' for maintaining community standards and property values. From the perspective of nonconformist homeowners, it operates as a 'snare' that extracts their autonomy and imposes arbitrary rules. The engine's classification will highlight this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board and aligned homeowners are beneficiaries, as they gain from the perceived stability and control. Nonconformist homeowners and those with marginal aesthetics are victims, bearing the direct costs of enforcement. The property management company, while an enforcer, also benefits financially from the system's complexity. The directionality for victims is high due to identity-locked exit options (selling one's home is a high-friction exit).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_value_causality,
    'To what extent does aesthetic uniformity and behavioral conformity actually cause higher property values, versus merely reflecting a preference for homogeneity among a subset of buyers?',
    'Longitudinal econometric studies comparing property value appreciation in similar communities with and without strict aesthetic covenants, controlling for other factors.',
    'If the causal link is weak or absent, the ''property value maximization'' justification for the covenant''s suppressive aspects collapses, reclassifying it closer to pure extraction. If strong, it supports the coordination aspect, albeit with high costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_causality, empirical, 'Empirical link between covenant enforcement and property values.').

omega_variable(
    subjectivity_of_aesthetics,
    'Is the ''aesthetic uniformity'' enforced by the covenant an objective standard, or a subjective preference imposed by the dominant group?',
    'Analysis of covenant rule changes over time, and comparison with evolving architectural and design trends. Legal challenges to specific aesthetic rules based on vagueness or arbitrary enforcement.',
    'If subjective, the enforcement mechanism is more arbitrary and extractive, as it targets individual expression rather than objective harm. This would push the classification further towards a Snare, as the coordination story becomes a cover for preference enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjectivity_of_aesthetics, conceptual, 'Objectivity vs. subjectivity of aesthetic standards.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (fines, legal action) or internalized (homeowners self-censor to avoid conflict)?',
    'Post-exit suppression trajectory: if homeowners who move to non-HOA communities continue to self-censor their aesthetic choices, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hoa_covenant_scope' kernel. This 'behavioral control' reading emphasizes the suppressive aspects of aesthetic and behavioral enforcement, distinct from readings focused on pure coordination or pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
