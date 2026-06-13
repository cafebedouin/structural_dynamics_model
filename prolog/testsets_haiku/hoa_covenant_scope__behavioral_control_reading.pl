% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: HOA Covenant: Behavioral Control via Aesthetic Uniformity
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   Homeowners associations (HOAs) enforce covenants ostensibly to protect
 *   community character and property values. This reading interprets the
 *   covenant's operative function as behavioral control: the enforcement
 *   machinery exists to normalize aesthetic conformity and lifestyle
 *   compliance through legal coercion. The foundational disagreement is about
 *   what the covenant's primary purpose IS. The coordination reading sees
 *   genuine shared-infrastructure problems (roof maintenance timing, common
 *   area use). The extraction reading sees pure revenue generation and board
 *   authority consolidation. This reading—the behavioral-control reading—sees
 *   the covenant as a mechanism for formalizing and enforcing conformity that
 *   would otherwise rest on informal social pressure. The behavioral norm
 *   itself is the good being extracted: compliance with aesthetic uniformity
 *   becomes obligatory rather than voluntary, and nonconformists are subject
 *   to legal sanction for deviation. The claim and metrics are INDEPENDENT:
 *   we claim snare (the control structure) and author metrics that reflect
 *   moderate extraction (0.42), substantial suppression (0.68), and rising
 *   theater as enforcement machinery ossifies around conformity aesthetics
 *   rather than genuine coordination problems.
 *
 * KEY AGENTS:
 *   - conformist_majority: beneficiary, receives property value protection and social affirmation of their aesthetic preferences
 *   - hoa_board: agenda-setter, controls enforcement machinery and interprets covenant scope
 *   - nonconformist_owners: victim, subject to enforcement for aesthetic/behavioral deviation
 *   - marginal_aesthetic_households: victim, face selective and unpredictable enforcement
 *   - board_aligned_homeowners: beneficiary with concentrated power, receive preferential enforcement
 *   - property_appraiser_ecosystem: beneficiary, profits from uniformity-as-asset narrative
 *   - prospective_buyers: excluded, inherit obligations without participation in enforcement
 *   - courts_and_regulators: observer, establish enforceability standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.68).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant: Behavioral Control via Aesthetic Uniformity").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '45c3f4e4-21eb-41e5-8db6-1d94d42a3b58').
narrative_ontology:cs_kernel_codification('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', fixed_text).
narrative_ontology:cs_authority_grounding('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', extraction).
narrative_ontology:cs_interpretation_layer_present('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58').
narrative_ontology:cs_reading_relation('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', foundational, conformity_as_extractable_good).
narrative_ontology:cs_axiom_status(conformity_as_extractable_good, holdable).
narrative_ontology:cs_axiom_grounding('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', conformity_as_extractable_good, deontological).
narrative_ontology:cs_axiom('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', foundational, board_authority_over_aesthetic_judgment).
narrative_ontology:cs_axiom_status(board_authority_over_aesthetic_judgment, holdable).
narrative_ontology:cs_axiom_grounding('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', board_authority_over_aesthetic_judgment, conventional).
narrative_ontology:cs_reference_frame('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', conformity_through_covenant_enforcement).
narrative_ontology:cs_drift_state('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', contemporary_selective_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45c3f4e4-21eb-41e5-8db6-1d94d42a3b58', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_owners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_households).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness is moderate (0.42 at interval end) because the extraction is not raw financial rent but behavioral compliance. Nonconformists pay the cost in restricted self-expression and forced modification, while conformists gain cost-free social affirmation. The trajectory rises from 0.28 to 0.42 over the interval: as enforcement machinery hardens, the extractiveness increases—the covenant shifts from stated intent (protect shared interests) to operational function (enforce conformity). Suppression is substantial (0.68) because nonconformists have limited exit (trapped in a purchased property, constrained by resale losses) and the enforcement is backed by legal mechanisms (architectural review boards, fine authority). Theater is moderate-to-rising (0.32→0.48): the covenant is justified by value-protection claims, but the enforcement increasingly targets subjective aesthetic preferences (yard signs, paint color, garden style) rather than objective maintenance standards. The rising theater trajectory indicates the enforcement function is becoming more about conformity theater (maintaining appearance of consensus) than solving genuine coordination problems. Measurements share one time grid: every metric is authored at every time point (0, 5, 10, 15, 20, 25).
 *
 * PERSPECTIVAL GAP:
 *   The conformist and board-aligned seats should perceive a genuine coordination mechanism protecting their interests; the nonconformist and marginal-aesthetic seats perceive behavioral control and selective enforcement. The engine computes per-seat directionality: conformists get low d (beneficiaries, receive security), nonconformists get high d (victims, trapped, subject to coercion). The board sits at institutional power with mobile exit—the board members themselves could leave the community, but they have incentive to maintain enforcement. The perspectival gap is structural: one seat's protection is another seat's control. The board's ability to selectively enforce (permit some deviations, punish others) creates an asymmetric directionality: the conformist-aligned owner at 'powerful' power level experiences d near beneficiary; the marginally-nonconforming owner at 'moderate' power level experiences d near full target because their exit is constrained and enforcement is unpredictable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (conformist_majority, board_aligned_homeowners) derive d from their role and exit options: organized power + constrained exit (can't move easily, invested in community) + beneficiary role (gain value protection) → d approximately 0.2-0.3 (beneficiary-tilted). Victims (nonconformist_owners, marginal_aesthetic_households) derive d from victim role + trapped/constrained exit + powerless/moderate power → d approximately 0.7-0.85 (full-target-tilted). The board itself (agenda_setter, institutional power, mobile exit) has d ambiguous: as enforcer it benefits from enforcement authority, but as individual owners it also has exit options. Board members are typically drawn from the conformist majority, so their directionality should track beneficiary (d ~0.25-0.35). Prospective buyers (excluded, moderate power, mobile exit) have low effective d in the current moment (they haven't purchased yet), but upon purchase would shift to constrained exit and potentially high d if their preferences are nonconforming.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is protection of property values through uniformity. The behavioral-control reading asserts that the operative function has decoupled from value protection and become conformity enforcement as an end in itself. Evidence for mandatrophy: (1) enforcement targets increasingly subjective aesthetic preferences (yard signs, paint color, visible repairs) rather than objective maintenance (roof condition, structural integrity); (2) theater rises over the interval as the covenant's language emphasizes 'community character' and 'harmonious appearance' rather than infrastructure durability; (3) the founding problem (value protection via uniformity) may have been live 20+ years ago when homogeneous construction was economically linked to resale value, but contemporary evidence is mixed—many communities with covenant enforcement show no value premium, and some show negative effects from selective enforcement. The constraint persists via institutional inertia (the board has authority and uses it) and beneficiary lock-in (conformist owners vote for boards that maintain enforcement). The behavioral-control reading resolves mandatrophy by asserting that the founding problem IS the behavioral control—that what appeared to be a proxy for value protection was always the mechanism for formalizing conformity. This reading prevents misclassification as a genuine coordination mechanism (rope) by naming what is actually being extracted: behavioral compliance and aesthetic conformity, with value protection as the cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_protection_vs_conformity_control_boundary,
    'Is the covenant''s extraction primarily about protecting property values through uniformity, or about enforcing behavioral conformity as a social control mechanism that incidentally supports valuations?',
    'Longitudinal study of enforcement patterns vs. property value outcomes: if enforcement predicts value preservation, the value-protection reading is supported; if enforcement predicts conformity independent of value outcomes, or if selective non-enforcement of conforming owners'' deviations shows no value effect, the conformity-control reading is supported. Case law analysis of covenant scope limitations: statutes restricting covenant scope to ''objective, maintenance-based standards'' would operationally distinguish these readings.',
    'If value protection is primary, the constraint should classify as rope (genuine coordination). If conformity control is primary, snare classification is correct. The boundary is where enforcement shifts from ''this maintenance standard protects value'' to ''this aesthetic choice is not acceptable regardless of value impact.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_protection_vs_conformity_control_boundary, empirical, 'Whether the covenant''s extraction is instrumental (value protection) or direct (conformity normalization).').

omega_variable(
    selective_enforcement_asymmetry_origin,
    'Is selective enforcement (permitting deviations from aligned owners, punishing nonconformists) structural to the covenant''s design, or incidental to board capture and institutional drift?',
    'Audit of enforcement records (architectural approvals, fines issued) against a baseline of rule application: if the same deviation triggers enforcement selectively based on owner alignment, the structure is asymmetric. Surveys of nonconforming vs. conforming owners on enforcement experience. Analysis of board composition changes over time: does turnover increase or decrease selective enforcement?',
    'If asymmetry is structural, the covenant operationally functions as a snare (the rules are selectively applied to extract conformity from targeted owners). If asymmetry is incidental, the constraint might still be snare, but for different reasons (the rules themselves are extractive, and selective enforcement is a symptom). If enforcement becomes more uniform over time or with new boards, mandatrophy may resolve and the constraint may reclassify toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_asymmetry_origin, empirical, 'Whether selective enforcement is designed into the covenant or emergent from institutional drift.').

omega_variable(
    internalized_suppression_vs_structural_suppression,
    'How much of the measured suppression (0.68) is structural (trapped by property investment, legal barriers to modification) vs. internalized (owners have adopted the conformity norm and believe deviation is wrong)?',
    'Post-exit interviews with former community members: if suppression persists after leaving (owners continue to self-censor or believe their former neighbors'' nonconformity was wrong), suppression is partly internalized. Surveys measuring explicit agreement with covenant values vs. agreement under coercion. Comparison of suppression measures between owners who feel trapped vs. owners who have exit options.',
    'If suppression is primarily structural (trapped by investment cost), the constraint''s effective suppression ends with exit. If partly internalized, the constraint''s social control persists beyond exit—the conformity norm is embedded in identity. Higher internalization increases the constraint''s effective control and suggests deeper extraction of behavioral conformity as a self-perpetuating norm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_vs_structural_suppression, empirical, 'Mechanism of suppression: structural barriers vs. internalized norm adoption.').

omega_variable(
    sibling_reading_empirical_priority,
    'Among the three readings of the hoa_covenant_scope kernel (coordination, extraction, behavioral_control), which reading''s ε best predicts enforcement patterns and victim experience?',
    'Prospective coding of enforcement actions against the three ε models: does coordination-ε (near-zero) predict the enforcement patterns? Does extraction-ε (near-maximum) predict them? Does behavioral-control-ε (moderate) predict them? Ground truth from nonconformist owner interviews and enforcement records.',
    'This omega is a kernel-level question, not specific to this reading. The answer determines which reading''s ε is most defensible. All three readings should author this omega independently and report the same resolution mechanism, allowing the corpus to measure which reading''s structural model best fits the empirical record.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_priority, empirical, 'Cross-reading empirical adjudication: which reading''s ε model predicts reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'hoa_covenant_scope'. The coordination reading interprets covenants as solving shared infrastructure problems (ε ~0.05, rope); the extraction reading interprets covenants as board revenue and power consolidation (ε ~0.75, snare); the behavioral_control reading (this file) interprets covenants as formalizing behavioral conformity and aesthetic control (ε ~0.42, snare). The three readings are siblings: they read the same legal and institutional structure differently, producing different ε values and different victim sets. See commentary.kernel_context for the kernel decomposition logic. All three readings are required to understand the constraint's structural contestation. The sibling omega (sibling_reading_empirical_priority) is an empirical arbitration: which reading's ε best predicts enforcement patterns and victim experience. The network edges link all three; each story should reference the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
