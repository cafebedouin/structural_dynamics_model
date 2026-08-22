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
 *   This constraint describes a Homeowners Association (HOA) covenant,
 *   specifically interpreted as a mechanism for enforcing aesthetic
 *   uniformity and behavioral conformity, justified by the goal of maximizing
 *   property values. While presented as a coordination tool, its expansive
 *   scope into subjective judgments and lifestyle restrictions, coupled with
 *   active enforcement against nonconformists, reveals a substantially
 *   extractive dynamic. This is one reading of the 'hoa_covenant_scope'
 *   kernel.
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
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '1b85c156-1fb0-4431-ac96-9f8c8b5106e3').
narrative_ontology:cs_kernel_codification('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', formalized).
narrative_ontology:cs_authority_grounding('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', practice).
narrative_ontology:cs_interpretation_layer_present('1b85c156-1fb0-4431-ac96-9f8c8b5106e3').
narrative_ontology:cs_reading_relation('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', foundational, aesthetic_uniformity_maximizes_value).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_maximizes_value, holdable).
narrative_ontology:cs_axiom_grounding('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', aesthetic_uniformity_maximizes_value, empirically_contingent).
narrative_ontology:cs_axiom('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', foundational, behavioral_conformity_ensures_social_order).
narrative_ontology:cs_axiom_status(behavioral_conformity_ensures_social_order, holdable).
narrative_ontology:cs_axiom_grounding('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', behavioral_conformity_ensures_social_order, empirically_contingent).
narrative_ontology:cs_reference_frame('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', uniform_and_orderly_community).
narrative_ontology:cs_drift_state('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', contemporary_individual_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1b85c156-1fb0-4431-ac96-9f8c8b5106e3', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, hoa_board_members).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, homeowners_with_marginal_aesthetics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, property_management_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected by homeowners, they interpret and enforce the covenant, often with broad discretion over aesthetic and behavioral rules. They benefit from maintaining perceived property values and social order, which reinforces their authority and influence within the community.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board_members, agenda_setter,
    organized, biographical, constrained, local).

% These homeowners generally agree with or benefit from the aesthetic and behavioral rules, believing they protect property values and maintain a desirable neighborhood character. They experience the covenant as a coordination mechanism that preserves their investment and lifestyle.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners, beneficiary,
    moderate, biographical, mobile, local).

% These homeowners face fines, legal action, or social pressure for violations of aesthetic or behavioral rules, such as specific landscaping choices, exterior paint colors, or displaying certain yard signs. Their identity is often tied to their personal expression, making compliance feel like a significant personal cost.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, immediate, identity_locked, local).

% Homeowners whose personal aesthetic preferences (e.g., drought-tolerant landscaping, non-traditional exterior decor) fall outside the HOA's subjective 'uniformity' standards. They bear the cost of forced compliance or ongoing disputes, often without a clear path to appeal or change the rules.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, homeowners_with_marginal_aesthetics, payer,
    powerless, biographical, constrained, local).

% Contracted by the HOA board, this company handles day-to-day enforcement, issuing notices and collecting fines. They benefit financially from the ongoing need for enforcement, which can incentivize a broad interpretation of covenant violations.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, property_management_company, beneficiary,
    organized, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains a consistent aesthetic and behavioral standard across the community, aiming to create a cohesive neighborhood appearance and social environment.
% TRANSFER_FUNCTION: Transfers autonomy and individual expression from homeowners to the HOA board, in exchange for a perceived increase in collective property value and social order. Financial transfers occur through fines for non-compliance.
% ABSENT_VOICES: Potential buyers who value individual expression over strict conformity are implicitly excluded from the community, as are those who might advocate for more diverse or sustainable aesthetic standards. Their absence reinforces the existing conformity.
% DISAPPEARANCE_RATIONALE: If the covenant's behavioral and aesthetic controls vanished, homeowners would immediately begin exercising greater individual discretion over their property, leading to a rapid diversification of aesthetics and behaviors. Property values might fluctuate based on individual preferences rather than enforced uniformity, and the social dynamics of the community would shift significantly.
% FOUNDING_PROBLEM: The original problem was to prevent individual property choices from negatively impacting the collective property values and desirability of the community, particularly regarding maintenance and appearance.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and many conformist homeowners argue the problem is still live, citing the need to protect property values. Nonconformist homeowners and some external legal observers argue the problem has been over-solved, and the covenant now serves primarily to enforce subjective preferences rather than address genuine externalities; court challenges and media reports from outside the benefiting parties corroborate this shifted function.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, reflecting the tangible costs borne by nonconformist homeowners in terms of fines, legal fees, and suppressed self-expression. Suppression (0.70) is high due to the HOA's legal authority to enforce rules, levy fines, and place liens, making exit or non-compliance difficult. The theater ratio (0.20) is low, as the enforcement activity is genuinely aimed at achieving conformity, even if the underlying justification (property value maximization) is debatable. The claimed type is 'snare' because the coordination story (property value protection) serves as cover for asymmetric extraction of conformity from specific homeowners.
 *
 * PERSPECTIVAL GAP:
 *   The HOA board and conformist homeowners experience this covenant as a 'rope' or 'scaffold' that protects their investment and lifestyle. Nonconformist homeowners, however, experience it as a 'snare' that extracts their autonomy and expression. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   HOA board members and conformist homeowners are beneficiaries, as they perceive direct benefits from the enforced uniformity and often drive the enforcement agenda. Nonconformist homeowners and those with 'marginal' aesthetics are clear payers, bearing the costs of compliance or resistance. The property management company also benefits from the enforcement activity itself. The 'identity_locked' exit option for nonconformist homeowners reflects the deep personal cost of suppressing their expression within their own home.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (property value maximization) is still 'live' for many, but its 'status' is 'contested' because the means (behavioral control) have arguably outlived their necessity or become disproportionate to the problem. The classification as a snare prevents mislabeling this as pure coordination by highlighting the victims and the active suppression required for its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_value_correlation,
    'To what extent does strict aesthetic and behavioral conformity, enforced by the HOA, actually correlate with higher or more stable property values in this specific market?',
    'Independent econometric analysis comparing property value trends in HOAs with strict aesthetic/behavioral covenants versus those with more permissive or no covenants in similar market segments.',
    'If the correlation is weak or non-existent, the primary justification for the constraint (property value maximization) is undermined, strengthening the ''snare'' classification and potentially shifting it towards a ''piton'' if the enforcement becomes purely inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_correlation, empirical, 'Empirical link between covenant enforcement and property values.').

omega_variable(
    subjectivity_of_aesthetics,
    'Is the enforcement of ''aesthetic uniformity'' based on objective, measurable criteria, or is it inherently subjective and prone to arbitrary interpretation by the HOA board?',
    'Legal review of covenant language and enforcement records for specific, quantifiable standards versus vague, discretionary clauses. Analysis of appeals processes and outcomes.',
    'If subjective, the suppression metric is effectively higher for nonconformists due to unpredictable enforcement, and the ''snare'' classification is reinforced by the arbitrary nature of extraction. If objective, the constraint might lean more towards a ''tangled_rope'' if the rules are clear but still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjectivity_of_aesthetics, conceptual, 'Objectivity vs. subjectivity in aesthetic enforcement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (fines, legal action) or internalized (social pressure, fear of ostracism) for nonconformist homeowners?',
    'Post-exit suppression trajectory: if nonconformist behaviors persist after moving to a non-HOA community, the suppression was primarily structural. If the individual continues to self-censor, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the ''identity_locked'' exit option more salient.',
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
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.43).
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

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral_control_reading' of the 'hoa_covenant_scope' kernel. It focuses on the covenant's function in enforcing subjective aesthetic and behavioral norms, distinct from readings focused on infrastructure or pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
