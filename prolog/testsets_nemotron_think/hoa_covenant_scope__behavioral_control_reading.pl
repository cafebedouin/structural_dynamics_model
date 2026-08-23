% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Behavioral Control Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story captures the behavioral_control_reading of the
 *   hoa_covenant_scope kernel: the covenant operates as a mechanism for
 *   enforcing aesthetic uniformity and behavioral conformity under the banner
 *   of property value maximization. The reading holds that the covenant's
 *   genuine function is not coordinating shared infrastructure
 *   (coordination_reading) nor generating revenue for its own sake
 *   (extraction_reading), but rather policing the boundaries of acceptable
 *   neighborhood identity through subjective aesthetic judgments, lifestyle
 *   restrictions, and speech suppression (yard signs, flags, cultural
 *   displays). The constraint extracts compliance costs and autonomy from
 *   nonconformists and marginal_aesthetics while subsidizing the conformist
 *   majority's aesthetic preferences and the board's disciplinary authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'bb046a2a-fccf-4bc2-b0db-d348c6a0f852').
narrative_ontology:cs_kernel_codification('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', formalized).
narrative_ontology:cs_authority_grounding('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', lineage).
narrative_ontology:cs_interpretation_layer_present('bb046a2a-fccf-4bc2-b0db-d348c6a0f852').
narrative_ontology:cs_reading_relation('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', foundational, property_values_depend_on_aesthetic_and_behavioral_uniformity).
narrative_ontology:cs_axiom_status(property_values_depend_on_aesthetic_and_behavioral_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', property_values_depend_on_aesthetic_and_behavioral_uniformity, empirically_contingent).
narrative_ontology:cs_axiom('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', foundational, subjective_aesthetic_judgment_is_valid_governance_criterion).
narrative_ontology:cs_axiom_status(subjective_aesthetic_judgment_is_valid_governance_criterion, holdable).
narrative_ontology:cs_axiom_grounding('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', subjective_aesthetic_judgment_is_valid_governance_criterion, conventional).
narrative_ontology:cs_reference_frame('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', developer_control_era).
narrative_ontology:cs_drift_state('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', contemporary_board_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb046a2a-fccf-4bc2-b0db-d348c6a0f852', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformists).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__behavioral_control_reading, property_value_maximization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the covenant, interprets aesthetic standards, levies fines, and initiates enforcement actions. Board members are elected from the homeowner population but campaign on platforms of 'protecting property values' through strict conformity. They control the architectural review committee and the fine schedule.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, biographical, arbitrage, local).

% Homeowners whose aesthetic preferences align with the board's enforcement priorities. They benefit from the neighborhood's uniform appearance which they believe protects their investment. They attend meetings, vote for strict enforcement candidates, and report violations. Their exit option is selling — they can leave if the neighborhood changes, but they actively shape the rules to avoid that.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter).

% The larger group of homeowners who broadly prefer a neat, predictable neighborhood and comply with covenant rules without active enforcement engagement. They benefit from the visual uniformity and perceived property value stability. They rarely attend meetings but their passive compliance and voting weight legitimize the board's agenda. Exit is constrained by mortgage, job, and family ties.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    moderate, biographical, constrained, local).

% Homeowners whose aesthetic choices, lifestyle, or identity expression fall outside the covenant's narrowing standards — native plant gardens, cultural/religious displays, non-standard paint colors, home-based businesses, political yard signs. They bear fines, forced remediation costs, litigation risk, and social ostracism. Exit is constrained by the same ties as the majority plus the difficulty of selling a 'noncompliant' property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformists, payer,
    powerless, biographical, constrained, local).

% Residents whose nonconformity stems from economic necessity, disability, cultural practice, or identity — e.g., disability ramps deemed 'unsightly,' multigenerational housing arrangements violating occupancy limits, religious symbols flagged as 'political,' low-income repairs using non-approved materials. They bear disproportionate enforcement because their nonconformity is least voluntary and least resourced to fight. Exit is effectively trapped.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics, payer,
    powerless, immediate, trapped, local).

% Renters have no vote in HOA governance but are bound by covenant rules through lease terms. Future buyers inherit the covenant without consent. Both groups would likely oppose expansive behavioral restrictions if enfranchised, but the covenant structure excludes them from amendment processes. Their exit is trapped — renters by lease and market; future buyers by the deed restriction itself.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, renters_and_future_buyers, excluded,
    powerless, immediate, trapped, local).

% Study HOA covenants as a governance form. They document the drift from developer sales tools to perpetual behavioral control regimes, the racialized and classed enforcement patterns, and the property value empirical debates. They have no stake in any specific neighborhood's covenant but their analysis shapes legislative reform efforts and court precedent.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, legal_scholars_urban_planners, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates aesthetic and behavioral standards across properties to maintain uniform neighborhood character, presented as solving the externality problem of one property's appearance affecting neighbors' values.
% TRANSFER_FUNCTION: Moves compliance costs and lifestyle freedom from nonconforming homeowners to the conformist majority and board, mediated through fines, forced remediation, litigation threats, and social sanction. The board gains disciplinary power and budget from fine revenue; the majority gains aesthetic assurance.
% ABSENT_VOICES: Renters, future buyers, and dissenting homeowners who fear retaliation are structurally excluded from covenant amendment processes. The covenant's amendment threshold (typically 67-75% of owners) and the board's control of the agenda make it nearly impossible for excluded voices to change the rules. Cultural minorities, disability advocates, and low-income residents are disproportionately silenced.
% DISAPPEARANCE_RATIONALE: If the covenant's enforcement vanished overnight, nonconforming aesthetics and behaviors would proliferate within months — native plant yards, cultural displays, home businesses, political signs. The board's disciplinary machinery would dissolve. Property values might initially dip from uncertainty but would likely reorganize around market fundamentals rather than enforced uniformity. The neighborhood's social contract would shift from conformity to pluralism.
% FOUNDING_PROBLEM: Original developers created covenants to protect initial sales prices by guaranteeing buyers a predictable, uniform neighborhood aesthetic during the build-out and initial sales period.
% FOUNDING_PROBLEM_CORROBORATION: Historical deed records, developer marketing brochures, and contemporaneous real estate coverage from the subdivision's founding era (1970s-1990s) corroborate the sales-protection purpose. Long-term residents who purchased during build-out, legal historians specializing in common-interest communities, and urban planners outside the current board-aligned majority attest the founding problem is dead — the developer is gone, build-out is complete, and the covenant persists as a permanent behavioral control regime.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the primary extraction is non-monetary — autonomy, expression, and cultural practice — with fines as the enforcement mechanism rather than the primary revenue stream. Suppression is high (0.75) because the covenant's persistence depends on actively suppressing alternatives: architectural review committees pre-approve all exterior changes, fine schedules escalate for repeat violations, and litigation threats silence challenges. Theater ratio is moderate (0.45) because genuine coordination functions (shared infrastructure maintenance, insurance negotiation) exist but are increasingly subordinate to conformity enforcement. Accessibility collapse (0.65) reflects that alternatives exist legally (amendment, litigation, sale) but are practically foreclosed by supermajority thresholds, cost asymmetry, and social pressure. Resistance (0.55) captures ongoing but fragmented pushback from nonconformists, legal challenges, and state-level reform efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the board_aligned_homeowners seat, the constraint appears as genuine coordination protecting their largest asset. From the nonconformists seat, the same structure operates as a snare — subjective standards weaponized against difference. From the marginal_aesthetics seat, it is a snare with trapped exit — their nonconformity is involuntary (disability, culture, poverty) yet punished identically. The engine computes this divergence from the structural data; the authored claim (snare) reflects the analytical observer's assessment that extraction and suppression dominate the coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The hoa_board and board_aligned_homeowners are structural beneficiaries (d near 0.0-0.2): they set the agenda, control enforcement discretion, and collect the aesthetic subsidy. The conformist_majority sits near symmetric (d ~0.4-0.5): they benefit from uniformity but bear compliance costs and lose exit flexibility. Nonconformists and marginal_aesthetics are full targets (d ~0.8-1.0): they bear the extraction, face suppressed exit, and have no agenda-setting power. Renters_and_future_buyers are excluded targets (d ~0.9) — bound without voice. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (developer sales protection) is dead per corroborated history, yet the covenant persists and has expanded its scope from objective standards (setbacks, roof materials) to subjective behavioral control (yard signs, gardening choices, holiday decorations). This is classic mandatrophy: the arrangement's mandate has outlived its function, but the constraint remains due to institutional inertia and the board's extraction of disciplinary power. The coordination_reading's claim that the covenant solves genuine externalities is the cover story; the behavioral_control_reading exposes the drift from coordination to conformity enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the behavioral_control_reading a distinct constraint from the coordination_reading and extraction_reading, or do they describe the same observable constraint from different analytical angles?',
    'Apply the ε-invariance test: if measuring the covenant''s operation via coordination metrics (infrastructure maintenance outcomes) yields low ε but measuring via behavioral enforcement metrics (fine rates, variance in aesthetic approvals, speech restrictions) yields moderate ε, they are distinct constraints. Decompose if ε differs by observable.',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. The behavioral_control_reading classifies as snare; the coordination_reading may classify as rope or tangled_rope; the extraction_reading may classify as snare or piton. Linkage via network.affects_constraints enables contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings instantiate structurally distinct constraints per the ε-invariance principle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal enforcement, fine machinery, architectural review gatekeeping) or internalized (homeowners self-censor, conform preemptively, police neighbors)?',
    'Post-enforcement relaxation study: if a jurisdiction mandates narrowed covenant scope (e.g., state law banning political sign restrictions), measure whether suppression behaviors persist among homeowners. Persistent self-censorship after legal barrier removal indicates internalized component.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression with them. This would increase the snare classification confidence and suggest remedy must address cultural/internalized dimension, not just legal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in HOA behavioral control.').

omega_variable(
    coordination_extraction_boundary,
    'Is the conformity enforcement function structurally separable from the genuine coordination function (infrastructure, insurance, common areas), or are they inextricably bundled?',
    'Natural experiment: in HOAs that have spun off infrastructure management to professional management companies while retaining architectural control, measure whether behavioral enforcement intensity changes. If enforcement persists or intensifies without infrastructure coordination, the functions are separable.',
    'If separable, the coordination function is cover for the conformity enforcement (strengthening snare classification). If inseparable, part of the measured extraction is the price of coordination (tangled_rope possible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the covenant''s coordination and behavioral control components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.08).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the behavioral_control_reading of the hoa_covenant_scope kernel. It decomposes the colloquial 'HOA covenant' into structurally distinct claims: (1) behavioral_control_reading (this story) — enforces conformity for property values, ε≈0.42, snare; (2) coordination_reading — coordinates infrastructure/externalities, ε≈0.15, rope/tangled_rope; (3) extraction_reading — generates revenue/consolidates power, ε≈0.55, snare/piton. The readings differ in ε by wide margins, have different failure modes, and different beneficiary/victim structures. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, organized, 0.15).
constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
