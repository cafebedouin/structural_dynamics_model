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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant: Behavioral Control Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story describes the Homeowners Association (HOA) covenant
 *   from the perspective of its function as a mechanism for behavioral
 *   control and aesthetic uniformity, often justified as a property value
 *   maximization strategy. It is one reading of the broader
 *   'hoa_covenant_scope' kernel, focusing on the expansive and often
 *   subjective enforcement of rules that go beyond basic maintenance to
 *   dictate personal expression and lifestyle choices. The claimed type is
 *   'snare' because its persistence relies heavily on coercion and the
 *   suppression of alternatives for homeowners, with identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.45).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant: Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '5775c074-570c-43c7-b92b-ebc299f2b52a').
narrative_ontology:cs_kernel_codification('5775c074-570c-43c7-b92b-ebc299f2b52a', formalized).
narrative_ontology:cs_authority_grounding('5775c074-570c-43c7-b92b-ebc299f2b52a', practice).
narrative_ontology:cs_interpretation_layer_present('5775c074-570c-43c7-b92b-ebc299f2b52a').
narrative_ontology:cs_reading_relation('5775c074-570c-43c7-b92b-ebc299f2b52a', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('5775c074-570c-43c7-b92b-ebc299f2b52a', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5775c074-570c-43c7-b92b-ebc299f2b52a', foundational, aesthetic_uniformity_maximizes_property_value).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_maximizes_property_value, holdable).
narrative_ontology:cs_axiom_grounding('5775c074-570c-43c7-b92b-ebc299f2b52a', aesthetic_uniformity_maximizes_property_value, empirically_contingent).
narrative_ontology:cs_axiom('5775c074-570c-43c7-b92b-ebc299f2b52a', foundational, subjective_conformity_ensures_community_harmony).
narrative_ontology:cs_axiom_status(subjective_conformity_ensures_community_harmony, holdable).
narrative_ontology:cs_axiom_grounding('5775c074-570c-43c7-b92b-ebc299f2b52a', subjective_conformity_ensures_community_harmony, empirically_contingent).
narrative_ontology:cs_reference_frame('5775c074-570c-43c7-b92b-ebc299f2b52a', uniformity_as_value_protection).
narrative_ontology:cs_drift_state('5775c074-570c-43c7-b92b-ebc299f2b52a', contemporary_social_norms_and_legal_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5775c074-570c-43c7-b92b-ebc299f2b52a', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, hoa_board).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, property_management_company).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets, creates, and enforces the covenant rules, often expanding their scope to include subjective aesthetic judgments and lifestyle restrictions. Benefits from increased control over the community and perceived stability of property values. Collects fines and directs enforcement actions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, generational, arbitrage, local).

% Benefits from the enforced uniformity and perceived protection of property values. Actively supports the board's enforcement actions and often reports non-compliance, reinforcing the constraint.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    powerful, biographical, constrained, local).

% Bears the costs of forced compliance, fines, or legal action for minor deviations from aesthetic or behavioral norms. Their property is subject to the covenant, making exit difficult without selling and relocating.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, immediate, trapped, local).

% Homeowners whose aesthetic preferences (e.g., native landscaping, specific yard art, political signs) are deemed non-conforming and suppressed. Their identity is often tied to their expressive choices, making compliance a personal cost.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates, excluded).

% Hired by the HOA board to administer and enforce the covenant, often profiting from fines, fees, and the ongoing need for enforcement. Has an incentive to maintain the complexity and scope of the rules.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, property_management_company, beneficiary,
    organized, biographical, mobile, local).

% Urban planners, legal scholars, or civil liberties advocates who analyze the impact of HOA covenants on individual rights, community diversity, and the actual effect on property values. They observe and critique the system from outside.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, external_critics, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, hoa_board).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish and maintain a uniform aesthetic and behavioral standard within the community, aiming to prevent perceived degradation of property values and foster a harmonious living environment.
% TRANSFER_FUNCTION: Transfers autonomy and expressive freedom from individual homeowners to the HOA board and the conformist majority, in exchange for perceived property value stability and community order. Financial transfers occur through fines and fees for non-compliance.
% ABSENT_VOICES: Homeowners who prioritize individual expression, diverse cultural aesthetics, or specific lifestyle choices that conflict with the enforced uniformity are effectively silenced or excluded from the decision-making process. Their perspectives are not genuinely considered in covenant interpretation or amendment.
% DISAPPEARANCE_RATIONALE: If the covenant and its enforcement mechanisms vanished overnight, individual homeowners would immediately exercise greater autonomy over their property's appearance and use. This would lead to a more diverse aesthetic landscape, potentially varied property values based on individual preferences rather than enforced uniformity, and a shift in community dynamics away from top-down control.
% FOUNDING_PROBLEM: The covenant was established to prevent property value decline due to perceived aesthetic degradation, incompatible land uses, or behaviors deemed disruptive, and to ensure a harmonious community environment.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and conformist majority assert that the founding problem of maintaining property values and community harmony is still live, citing ongoing needs for regulation. Nonconformist homeowners and external critics argue that the original problem is largely solved or exaggerated, and the covenant is now primarily used for control beyond its initial intent, supported by legal challenges and sociological studies.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.45) is moderate because it primarily extracts non-monetary values like autonomy and expressive freedom, though financial penalties are also common. Suppression (0.75) is high due to the legal enforceability of covenants, the difficulty of exit for homeowners, and the social pressure from the conformist majority. The theater ratio (0.25) is low to moderate, indicating that enforcement is active and real, not merely performative, though some justifications for rules may be theatrical. The increasing trend in extractiveness and suppression reflects the common 'covenant creep' where rules expand over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the HOA board and conformist majority, the covenant is a legitimate tool for maintaining community standards and property values. From the perspective of nonconformist homeowners, it is an oppressive mechanism that curtails individual rights and extracts personal freedom under the guise of collective good. The engine's classification as a Snare from the victims' seats captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board and the conformist majority are the primary beneficiaries, gaining control and perceived stability. The property management company also benefits financially from enforcement. Nonconformist homeowners and advocates for marginal aesthetics are the clear targets, bearing the costs of compliance and suppressed expression. Their exit options are severely constrained by property ownership and the legal structure of the HOA.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_legitimate_authority,
    'What is the legitimate scope of HOA authority in regulating individual property aesthetics and homeowner behavior, beyond direct externalities?',
    'Judicial rulings on specific covenant challenges, legislative action to limit HOA powers, or a shift in community consensus regarding individual rights versus collective uniformity.',
    'If the legitimate scope is narrow, this reading''s high extractiveness and suppression would be fully validated as illegitimate. If a broader scope is deemed legitimate, some of the measured extraction might be reclassified as a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_legitimate_authority, conceptual, 'Ambiguity regarding the boundary between legitimate collective governance and overreaching behavioral control.').

omega_variable(
    property_value_maximization_efficacy,
    'Does strict aesthetic uniformity and behavioral conformity genuinely maximize property values, or does it merely create a homogenous market that excludes diverse preferences?',
    'Empirical economic studies comparing property value trends in HOAs with varying levels of aesthetic and behavioral control, controlling for other market factors.',
    'If uniformity does not demonstrably maximize values, the primary justification for the covenant''s behavioral control function collapses, strengthening its classification as a Snare. If it does, the ''beneficiary'' claim of the conformist majority gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_maximization_efficacy, empirical, 'Whether the stated goal of property value maximization is empirically supported by the covenant''s enforcement of uniformity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa__tr_t6, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(hoa__tr_t18, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hoa__be_t6, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(hoa__be_t18, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hoa__su_t6, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(hoa__su_t18, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
