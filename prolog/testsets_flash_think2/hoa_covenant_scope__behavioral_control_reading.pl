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
 *   This constraint story analyzes an HOA covenant through the lens of
 *   'behavioral control,' where the covenant's primary function is
 *   interpreted as enforcing aesthetic uniformity and behavioral conformity,
 *   justified as a strategy for property value maximization. This reading
 *   highlights the extractive nature of such control, particularly for
 *   nonconformist residents, and positions the constraint as a Snare. The
 *   metrics reflect a moderate level of extraction, high suppression, and a
 *   significant degree of theatricality in its justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.45).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant: Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'e5814b2a-4775-43ca-bfaf-b604c8696ff0').
narrative_ontology:cs_kernel_codification('e5814b2a-4775-43ca-bfaf-b604c8696ff0', formalized).
narrative_ontology:cs_authority_grounding('e5814b2a-4775-43ca-bfaf-b604c8696ff0', practice).
narrative_ontology:cs_interpretation_layer_present('e5814b2a-4775-43ca-bfaf-b604c8696ff0').
narrative_ontology:cs_reading_relation('e5814b2a-4775-43ca-bfaf-b604c8696ff0', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5814b2a-4775-43ca-bfaf-b604c8696ff0', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('e5814b2a-4775-43ca-bfaf-b604c8696ff0', foundational, aesthetic_uniformity_is_value).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_is_value, holdable).
narrative_ontology:cs_axiom_grounding('e5814b2a-4775-43ca-bfaf-b604c8696ff0', aesthetic_uniformity_is_value, empirically_contingent).
narrative_ontology:cs_axiom('e5814b2a-4775-43ca-bfaf-b604c8696ff0', foundational, board_discretion_ensures_conformity).
narrative_ontology:cs_axiom_status(board_discretion_ensures_conformity, holdable).
narrative_ontology:cs_axiom_grounding('e5814b2a-4775-43ca-bfaf-b604c8696ff0', board_discretion_ensures_conformity, conventional).
narrative_ontology:cs_reference_frame('e5814b2a-4775-43ca-bfaf-b604c8696ff0', collective_property_value_maximization).
narrative_ontology:cs_drift_state('e5814b2a-4775-43ca-bfaf-b604c8696ff0', contemporary_enforcement_practices, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5814b2a-4775-43ca-bfaf-b604c8696ff0', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, hoa_board).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, property_management_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The governing body that interprets and enforces the covenant, setting and adjudicating rules for aesthetic uniformity and behavioral conformity. Benefits from the power to control the community's appearance and residents' actions, ostensibly to protect property values.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, generational, constrained, local).

% Homeowners who align with the HOA's vision of uniformity and conformity. They benefit from the perceived stability of property values and a predictable, homogeneous neighborhood aesthetic, often supporting strict enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, constrained, local).

% Homeowners whose aesthetic preferences or behavioral choices (e.g., yard signs, exterior paint colors, flag displays) deviate from the covenant's interpretation. They bear the costs of fines, forced modifications, or legal action, and face social pressure to conform. Their primary exit is selling their property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    moderate, biographical, constrained, local).

% Individuals or groups within the community who advocate for diverse or non-traditional aesthetic expressions that are explicitly or implicitly suppressed by the covenant. They are structurally excluded from influencing the rules and are often the targets of enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates, excluded,
    powerless, biographical, trapped, local).

% A third-party company hired by the HOA board to administer and enforce the covenant. Benefits from service fees, which often increase with the complexity and frequency of enforcement actions, creating an incentive for active control.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, property_management_company, beneficiary,
    institutional, biographical, arbitrage, local).

% A municipal department that oversees urban planning and community development. They observe the impact of HOA covenants on neighborhood character, housing diversity, and resident well-being, but typically have limited direct authority over private covenant enforcement unless it violates public law.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, local_government_planning_dept, observer,
    institutional, generational, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish and maintain a consistent aesthetic and behavioral standard within the community, aiming to prevent perceived blight and ensure a desirable living environment for all residents.
% TRANSFER_FUNCTION: Transfers individual autonomy and expressive freedom from homeowners to the HOA board, enforcing conformity through fines, legal action, and social pressure, ostensibly for the collective benefit of property value maximization.
% ABSENT_VOICES: Homeowners who prioritize individual expression, diverse aesthetics, or specific political/social displays (e.g., yard signs, flags) are often silenced by the covenant's enforcement. They would argue for greater individual liberty and challenge the premise that strict uniformity is always beneficial.
% DISAPPEARANCE_RATIONALE: If the covenant and its enforcement vanished overnight, individual homeowners would immediately exercise greater autonomy over their property's aesthetics and their own behavior. This would lead to a more diverse, less uniform community appearance, and a significant shift in how property values are perceived and maintained, as the HOA board's power to dictate conformity would dissolve.
% FOUNDING_PROBLEM: To prevent a decline in property values and maintain community desirability by ensuring a minimum standard of maintenance and aesthetic consistency, thereby avoiding negative externalities from uncoordinated individual choices.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and conformist homeowners assert the founding problem is still live, citing ongoing needs for property value protection. However, independent urban planning studies and real estate economists often find that while basic maintenance prevents blight, strict aesthetic uniformity has diminishing returns for property value and can stifle community diversity; these external analyses often contest the board's claims that the original problem still warrants such expansive control.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while some homeowners genuinely benefit from perceived value stability, the cost of suppressed expression and forced conformity is substantial for others. Suppression (0.75) is high due to active enforcement mechanisms (fines, legal action) and the difficulty of exit (selling property). The theater ratio (0.5) indicates that roughly half of the enforcement activity is genuinely aimed at maintaining property values, while the other half serves to enforce subjective preferences or control for control's sake, under the guise of value protection. The increasing trend in extractiveness, suppression, and theater ratio over time reflects a common lifecycle where such covenants expand their scope of control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the HOA board and conformist majority, the covenant is a necessary coordination mechanism that protects their investment and lifestyle. From the perspective of nonconformist homeowners, it is an arbitrary and extractive mechanism of control that suppresses individual rights and expression. The engine's classification as a Snare reflects the latter, highlighting the coercive aspects that maintain the uniformity.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board and conformist majority are beneficiaries, as they gain control and perceived value stability with relatively low direct cost. Nonconformist homeowners and advocates for marginal aesthetics are targets, bearing the direct costs of fines, forced changes, and suppressed expression. The property management company is also a beneficiary, profiting from enforcement activities. The local government planning department acts as an analytical observer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_value_vs_control_ambiguity,
    'To what extent does strict aesthetic uniformity and behavioral conformity genuinely maximize property values, versus serving as a mechanism for social control and preference enforcement?',
    'Longitudinal studies comparing property value appreciation in HOAs with varying levels of aesthetic control, controlling for other market factors, alongside sociological studies on resident satisfaction and community diversity.',
    'If uniformity has negligible impact on property values beyond basic maintenance, the constraint''s extractiveness and theater ratio would be re-evaluated upward, strengthening its Snare classification. If a strong correlation is found, the coordination aspect would be more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_vs_control_ambiguity, empirical, 'Distinguishing genuine property value protection from subjective social control.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by nonconformist homeowners primarily structural (fines, legal action, forced changes) or internalized (social pressure, fear of conflict, desire to avoid confrontation)?',
    'Surveys and qualitative interviews with residents who have challenged or considered challenging HOA rules, assessing the perceived costs and psychological barriers to nonconformity.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the structural measures suggest, as residents carry the suppression with them even without direct enforcement actions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in HOA covenants.').

omega_variable(
    kernel_reading_distinction,
    'Given the ''hoa_covenant_scope'' kernel, what specific structural elements differentiate this ''behavioral_control_reading'' from the ''coordination_reading'' and ''extraction_reading''?',
    'Detailed comparative analysis of covenant texts, enforcement records, and resident testimonies across multiple HOAs, mapping specific clauses and outcomes to each reading''s core premises.',
    'Clarifies the precise points of divergence between readings, allowing for more precise classification and targeted interventions. If the readings are found to be less distinct, it may suggest a single, more complex constraint rather than a kernel with multiple distinct readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing structural elements across HOA covenant readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
