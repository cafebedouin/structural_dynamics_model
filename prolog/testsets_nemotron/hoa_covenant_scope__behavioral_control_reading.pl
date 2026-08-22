% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: HOA Covenant as Behavioral Control Instrument
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story captures the behavioral-control reading of the HOA
 *   covenant scope kernel — the interpretation that the covenant's primary
 *   function has shifted from infrastructure coordination to enforcing
 *   aesthetic and behavioral conformity as a property value strategy. The
 *   covenant's vague standards ('harmonious,' 'community character,'
 *   'nuisance') grant the board expansive enforcement discretion over
 *   subjective judgments: plant choices, holiday decorations, yard signs,
 *   flags, home-based work, cultural expressions. This reading sees the
 *   coordination function as a legacy layer; the active constraint is the
 *   behavioral control apparatus. The sibling readings — coordination_reading
 *   (genuine infrastructure coordination) and extraction_reading (revenue and
 *   power consolidation via fines) — are separate constraint stories linked
 *   via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.68).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant as Behavioral Control Instrument").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'e284c314-d933-4633-896a-519cc1914d98').
narrative_ontology:cs_kernel_codification('e284c314-d933-4633-896a-519cc1914d98', formalized).
narrative_ontology:cs_authority_grounding('e284c314-d933-4633-896a-519cc1914d98', extraction).
narrative_ontology:cs_interpretation_layer_present('e284c314-d933-4633-896a-519cc1914d98').
narrative_ontology:cs_reading_relation('e284c314-d933-4633-896a-519cc1914d98', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e284c314-d933-4633-896a-519cc1914d98', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('e284c314-d933-4633-896a-519cc1914d98', foundational, aesthetic_uniformity_maximizes_property_values).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_maximizes_property_values, holdable).
narrative_ontology:cs_axiom_grounding('e284c314-d933-4633-896a-519cc1914d98', aesthetic_uniformity_maximizes_property_values, empirically_contingent).
narrative_ontology:cs_axiom('e284c314-d933-4633-896a-519cc1914d98', foundational, community_character_requires_behavioral_conformity).
narrative_ontology:cs_axiom_status(community_character_requires_behavioral_conformity, holdable).
narrative_ontology:cs_axiom_grounding('e284c314-d933-4633-896a-519cc1914d98', community_character_requires_behavioral_conformity, conventional).
narrative_ontology:cs_reference_frame('e284c314-d933-4633-896a-519cc1914d98', original_infrastructure_covenant).
narrative_ontology:cs_drift_state('e284c314-d933-4633-896a-519cc1914d98', contemporary_behavioral_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e284c314-d933-4633-896a-519cc1914d98', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces aesthetic and behavioral standards through architectural review committees and fine schedules. Controls enforcement discretion — selective application of vague standards like 'harmonious appearance' and 'community character.' Collects fine revenue and legal fee reimbursements. Board members are elected from the homeowner population but campaigns are low-turnout and often uncontested.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, generational, arbitrage, local).

% Homeowners whose aesthetic preferences align with the covenant's baseline — neutral exteriors, manicured lawns, no visible 'clutter.' They experience the covenant as protecting their property values and neighborhood character. They can sell and move relatively easily if dissatisfied, but rarely are. Their benefit is diffuse: stable resale value, predictable visual environment.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, mobile, local).

% A subset of homeowners who actively participate in enforcement — attend meetings, report violations, serve on committees. They shape the covenant's interpretation in real time. Their alignment with the board gives them disproportionate influence over which violations are pursued. Some hold leadership positions; others are informal enforcers. Exit is constrained by social capital invested in the community.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter).

% Homeowners whose aesthetic or lifestyle choices fall outside the covenant's narrowing baseline — native plant gardens, non-traditional holiday displays, home-based micro-enterprises, cultural or religious expressions in yards. They face fines, forced remediation, liens, and litigation threats. Exit requires selling the home, which is financially and emotionally costly. The covenant's vague standards mean they cannot reliably predict what will trigger enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    moderate, biographical, constrained, local).

% Homeowners whose nonconformity is tied to identity, culture, disability, or economic necessity — religious symbols, accessibility modifications that alter exterior appearance, cultural landscaping traditions, low-cost adaptations. They bear the highest compliance costs and face the most aggressive enforcement because their deviations are visible and 'other.' Legal defenses (fair housing, religious freedom) exist but require resources they lack. Exit is effectively impossible — they cannot afford to move and have no alternative housing that accepts their practices.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_practitioners, payer,
    powerless, immediate, trapped, local).

% Subject to covenant rules through lease terms but have no vote in HOA elections, no standing at meetings, no ability to propose amendments. Bear compliance costs (fines passed through by landlords, restrictions on lifestyle) with zero governance voice. Often the most diverse demographic in the community — their exclusion from the constraint's political process is structural.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, renters_and_nonowner_occupants, excluded,
    powerless, immediate, trapped, local).

% Enforces baseline health, safety, and zoning codes. Occasionally conflicts with HOA standards (e.g., HOA bans clotheslines; state law protects them). Has authority to override covenants that violate public policy but rarely intervenes in aesthetic disputes. Sees the covenant as private governance layer — outside its mandate unless civil rights or statutory preemption is triggered.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, municipal_code_enforcement, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared infrastructure maintenance (roads, drainage, common areas) and resolves genuine externalities (fire hazards, structural hazards, drainage impacts on neighbors).
% TRANSFER_FUNCTION: Moves compliance costs and enforcement risk from the conformist majority onto nonconformists and marginal aesthetic practitioners. Moves fine revenue and legal fee reimbursements to the HOA (controlled by the board). Moves social control authority to board-aligned homeowners who shape enforcement discretion.
% ABSENT_VOICES: Renters and non-owner occupants (structurally excluded from governance). Marginal aesthetic practitioners whose practices are culturally or economically determined (trapped by cost and identity). Would-be homebuyers deterred by the covenant's reputation — they never enter the conversation. Disability advocates who would challenge aesthetic standards that conflict with accessibility needs.
% DISAPPEARANCE_RATIONALE: If the behavioral control layer vanished overnight, enforcement discretion would collapse to objective safety/hazard standards only. Nonconformist homeowners would immediately express previously suppressed practices (gardens, displays, home businesses). Property value effects would be contested — some neighbors would sell, others would invest in personalization. The HOA would lose its primary leverage (subjective aesthetic enforcement) and would either shrink to a maintenance corporation or dissolve. The neighborhood's visual character would diversify rapidly.
% FOUNDING_PROBLEM: New subdivisions needed a mechanism to maintain common infrastructure and prevent a few negligent owners from degrading shared assets (roads, drainage, amenities) that affect all property values.
% FOUNDING_PROBLEM_CORROBORATION: Original developer records and early HOA minutes (held by county recorder and historical society) confirm the infrastructure-maintenance purpose. Long-term residents who predate the behavioral-control expansion attest the covenant's scope crept over decades — the 'architectural review' committee was added 15 years after founding. No independent corroboration exists for the claim that behavioral uniformity is necessary for property values; economic studies on HOA effects are mixed and methodologically contested (cited: UVA Law Review 2022, Lincoln Institute 2019).
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.42) is moderate but rising — the constraint extracts compliance costs, legal risk, and expressive freedom from nonconformists while delivering diffuse property-value insurance to conformists. Suppression (0.68) is high because enforcement depends on vague standards that cannot be objectively met — compliance is performative and perpetual. Theater ratio (0.22) reflects that the coordination infrastructure (roads, pools) is real but increasingly incidental to the covenant's active enforcement energy. Accessibility collapse (0.45) is moderate — alternatives exist (move, litigate, comply) but are costly. Resistance (0.55) is significant — nonconformists push back, but the power asymmetry favors the board. The measurement series shows extractiveness and suppression rising together over 40 years as the covenant's scope expanded from objective standards to subjective aesthetic judgments.
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat, the covenant is a coordination mechanism protecting shared assets. From the conformist majority's seat, it is a reasonable community standard. From nonconformists' seats, it is an enforced conformity regime with vague, shifting goalposts. From marginal practitioners' seats, it is a trap — their identity-expression is the violation. The engine computes this divergence from the structural data: power, exit_options, and beneficiary/victim declarations map to distinct directionality values per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board (agenda_setter, institutional power) sits at the beneficiary end — it controls the rule interpretation and collects enforcement revenue. Conformist majority and board-aligned homeowners (beneficiaries, organized power) receive diffuse property-value protection and social control authority — their directionality is low (d ~ 0.15-0.25). Nonconformist homeowners (payers, moderate power, constrained exit) bear targeted enforcement — directionality high (d ~ 0.75). Marginal aesthetic practitioners (payers, powerless, trapped) bear the highest extraction with zero exit — directionality near 1.0. Renters (excluded, powerless, trapped) are pure targets with no governance voice. Municipal observers (analytical) sit outside the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (infrastructure maintenance) is contested as live — the infrastructure still exists and needs maintenance. But the behavioral control layer (subjective aesthetic enforcement) has no founding mandate; it accreted through mission creep. The constraint exhibits mandatrophy: the original coordination function persists as a cover story while the active enforcement apparatus serves a different purpose (conformity enforcement). The theater_ratio rise tracks this — more enforcement energy goes to aesthetic control, less to infrastructure. The covenant is a snare wearing a rope's clothes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_control_vs_extraction_primacy,
    'Is the covenant''s behavioral enforcement primarily driven by conformist majority demand (this reading) or by board revenue/power incentives (extraction_reading)?',
    'Compare fine revenue as share of HOA budget vs. conformist-majority turnout at enforcement hearings. If fines are <5% of budget but enforcement hearings draw >30% of homeowners, behavioral_control_reading gains support. If fines are >20% of budget and hearings are sparsely attended, extraction_reading gains support.',
    'If extraction_reading is primary, the constraint reclassifies toward snare with board as primary beneficiary. If behavioral_control_reading is primary, the conformist majority is the structural beneficiary and the board is their agent — the constraint remains snare but with different seat mapping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_control_vs_extraction_primacy, empirical, 'Whether conformity enforcement serves majority preference or board self-interest.').

omega_variable(
    coordination_residual_authenticity,
    'Does the covenant''s infrastructure coordination function genuinely require the behavioral control layer, or is the coordination function fully separable?',
    'Natural experiment: jurisdictions where state law strips HOAs of aesthetic enforcement authority but preserves maintenance authority (e.g., Florida''s 2024 HOA reform). If infrastructure maintenance continues effectively without aesthetic control, the functions are separable.',
    'If separable, the behavioral control layer is pure extraction/coercion with no coordination justification — strengthens snare classification. If inseparable, some extraction is the price of coordination — supports tangled_rope classification for the combined constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_residual_authenticity, empirical, 'Whether aesthetic enforcement is structurally necessary for infrastructure coordination.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (fines, liens, legal threats) or internalized (homeowners self-censor to avoid conflict, internalize aesthetic norms)?',
    'Post-exit suppression trajectory: track nonconformist homeowners who sell and move — do they resume suppressed practices immediately in unregulated neighborhoods? If yes, suppression was primarily structural. If suppression persists (they continue self-censoring), internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would increase the effective extraction for identity-locked agents beyond what the raw metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in HOA behavioral control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel decomposes into three constraint stories: behavioral_control_reading (this file, moderate ε, snare, beneficiary=conformist_majority/board_aligned), coordination_reading (low ε, rope/tangled_rope, beneficiary=all_owners for infrastructure), extraction_reading (moderate-high ε, snare, beneficiary=board/management_company). The behavioral control reading sits between the other two — it acknowledges the coordination layer exists but sees it as captured by the conformity function, whereas extraction_reading sees the conformity function as captured by revenue incentives. All three share the same covenant text but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, organized, 0.2).
constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, moderate, 0.75).
constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
