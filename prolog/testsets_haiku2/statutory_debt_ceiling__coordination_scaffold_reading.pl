% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Procedural Coordination Scaffold
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the coordination_scaffold_reading of the
 *   contested statutory debt ceiling kernel. The reading frames the ceiling
 *   as a procedural mechanism that coordinates Treasury operational autonomy
 *   with congressional fiscal authority, enabling efficient debt management
 *   within an authorized aggregate limit. The constraint's function is to
 *   eliminate repeated legislative micromanagement while preserving
 *   congressional budget control. This is contrasted with two sibling
 *   readings: the constitutional_nullity_reading (the ceiling is superseded
 *   by the 14th Amendment's debt clause and should be void) and the
 *   extraction_snare_reading (the ceiling is weaponized by legislative
 *   minorities as a hostage mechanism to extract policy concessions under
 *   default threat). Each reading assigns a different ε value and
 *   beneficiary/victim structure to the same kernel. This story authors only
 *   the coordination_scaffold_reading.
 *
 * KEY AGENTS:
 *   - Treasury Department: manages debt issuance within authorized ceiling; benefits from operational autonomy without repeated legislative approval
 *   - Congressional Leadership: sets and adjusts the ceiling in response to Treasury notification and fiscal conditions; coordinates budget authority oversight
 *   - Financial Markets: maintain confidence in Treasury debt servicing predictability; benefit from routine adjustments that prevent default risk
 *   - Legislative Minority: constrained by ceiling adjustments but not hostage-takers in this reading; routine procedures apply
 *   - Fiscal Purists / Balanced-Budget Advocates: structurally excluded; their reading (extraction_snare) is a sibling story, not this one
 *   - Constitutional Scholars: some favor the nullity reading; not parties to this coordination reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Procedural Coordination Scaffold").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, 'd8aadb30-895f-4996-943b-2af2d52ed31c').
narrative_ontology:cs_kernel_codification('d8aadb30-895f-4996-943b-2af2d52ed31c', formalized).
narrative_ontology:cs_authority_grounding('d8aadb30-895f-4996-943b-2af2d52ed31c', lineage).
narrative_ontology:cs_interpretation_layer_present('d8aadb30-895f-4996-943b-2af2d52ed31c').
narrative_ontology:cs_reading_relation('d8aadb30-895f-4996-943b-2af2d52ed31c', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8aadb30-895f-4996-943b-2af2d52ed31c', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('d8aadb30-895f-4996-943b-2af2d52ed31c', foundational, periodic_adjustment_preserves_coordination).
narrative_ontology:cs_axiom_status(periodic_adjustment_preserves_coordination, holdable).
narrative_ontology:cs_axiom_grounding('d8aadb30-895f-4996-943b-2af2d52ed31c', periodic_adjustment_preserves_coordination, instrumental).
narrative_ontology:cs_axiom('d8aadb30-895f-4996-943b-2af2d52ed31c', foundational, congress_retains_aggregate_fiscal_authority).
narrative_ontology:cs_axiom_status(congress_retains_aggregate_fiscal_authority, holdable).
narrative_ontology:cs_axiom_grounding('d8aadb30-895f-4996-943b-2af2d52ed31c', congress_retains_aggregate_fiscal_authority, conventional).
narrative_ontology:cs_reference_frame('d8aadb30-895f-4996-943b-2af2d52ed31c', procedural_budget_coordination).
narrative_ontology:cs_drift_state('d8aadb30-895f-4996-943b-2af2d52ed31c', post_congressional_budget_act_1974, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8aadb30-895f-4996-943b-2af2d52ed31c', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congressional_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, federal_appropriations_committees).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, financial_markets).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, administrative_state).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, legislative_minority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under a standing aggregate debt authorization. Manages cash flow and debt structure within the authorized ceiling without seeking line-item congressional approval for each issuance. Benefits from operational autonomy to manage the government's financing. Treasury informs Congress when the ceiling will be reached; Congress adjusts it in response to Treasury notification, maintaining a predictable operating boundary.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, beneficiary,
    institutional, generational, constrained, national).

% Sets and adjusts the ceiling in response to Treasury notification and fiscal conditions. Delegates routine debt issuance to Treasury while retaining aggregate-limit oversight. Majority leadership and coalition builders coordinate with Treasury on timing and magnitude of ceiling adjustments to avoid operational disruption. Congress retains the power to change, suspend, or eliminate the ceiling; it chooses to adjust routinely.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_leadership, agenda_setter,
    institutional, generational, mobile, national).

% Appropriate and spend according to the budget process without continuous debt-issuance micromanagement. The ceiling provides a macro-level fiscal constraint without daily legislative involvement in Treasury operations. They benefit from standing authorization that allows budgeting and spending policy to proceed without repeated debt-approval negotiations.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, federal_appropriations_committees, beneficiary,
    organized, generational, mobile, national).

% Operate with predictable Treasury debt issuance and payment certainty. Under the coordination reading, the ceiling is adjusted routinely when needed, maintaining confidence in U.S. debt servicing. Markets price debt based on fiscal fundamentals and debt-servicing capacity; the ceiling itself does not create default risk under this reading because adjustments occur on schedule.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, financial_markets, beneficiary,
    powerful, biographical, mobile, global).

% Cannot unilaterally block or accelerate ceiling adjustments without coalition support. Under the coordination reading, ceiling adjustment is a routine fiscal management task subject to normal legislative process, not a leverage point for policy extraction. Minority parties retain veto power through the legislative process but operate within procedural norms that do not treat the ceiling as a hostage mechanism.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, legislative_minority, payer,
    moderate, biographical, constrained, national).

% Argue the ceiling should bite harder and force spending constraints rather than being routinely adjusted. They are structurally excluded from the coordination reading's procedural logic: they want the ceiling to function as a spending control, not as a procedural coordination tool. Their position—hardening the ceiling as a fiscal brake—is the substance of sibling readings (extraction_snare_reading frames the ceiling as hostage tool; constitutional_nullity_reading frames it as void) or distinct constraint stories.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, fiscal_discipline_advocates, excluded,
    moderate, biographical, constrained, national).

% Hold U.S. Treasury securities and care about debt servicing and payment certainty. They observe the ceiling adjustment process but do not set the agenda. Under the coordination reading, the ceiling creates minimal additional default risk beyond underlying fiscal policy; routine adjustments maintain payment certainty.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, bond_holders, observer,
    powerful, biographical, mobile, global).

% Federal agencies operate with standing authorization to spend according to appropriations. The ceiling provides a macro-level fiscal boundary without micromanaging agency operations. Agencies benefit from predictable budget authority that does not depend on repeated debt-approval negotiations.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, administrative_state, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates Treasury debt issuance authority into a single aggregate authorization, eliminating the need for line-item congressional approval of each Treasury offering. Congress sets a total; Treasury operates within it; when conditions require adjustment, Congress raises the ceiling through normal legislative process. This avoids administrative bottlenecks and daily micromanagement while preserving congressional aggregate-level fiscal control.
% TRANSFER_FUNCTION: Transfers procedural authority from Congress to Treasury for day-to-day debt management within an authorized aggregate limit. No direct transfer of wealth or resources; the transfer is administrative—Congress delegates execution of debt management while retaining policy oversight through the periodic adjustment mechanism.
% ABSENT_VOICES: Fiscal hawks argue the ceiling should be a hard constraint that forces spending discipline; progressives and constitutional scholars argue it should be void or radically reformed. Both groups are structurally excluded from this reading's procedural coordination logic. Their concerns are the substance of sibling readings (extraction_snare_reading and constitutional_nullity_reading) or separate stories. This reading does not adjudicate whether the ceiling should be higher, lower, or eliminated; it describes how it functions as a coordination tool.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling disappeared and Treasury had standing authorization to issue debt to fund appropriations, Congress would retain budget control through the appropriations process but would lose the procedural checkpoint of periodic ceiling adjustments. The fiscal governance process would shift: Treasury could issue debt on demand up to the limit of ongoing appropriations; Congress would face more continuous pressure to manage spending through appropriations rather than through an aggregate limit. The constraint coordinates the timing and frequency of legislative fiscal oversight; without it, oversight becomes diffuse across every appropriation.
% FOUNDING_PROBLEM: Early 20th-century Treasury practice required line-item congressional approval for each debt issuance. Multiple Treasury bond offerings per year generated repeated legislative negotiations over routine financing decisions, creating administrative bottlenecks and reducing legislative efficiency. The 1939 debt ceiling was enacted to consolidate these approvals into a single aggregate authorization, allowing Treasury to manage financing operationally while Congress retained control through periodic review of the aggregate limit.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Budget Office historical analysis and Treasury Department records confirm that the founding problem (fragmented approval bottlenecks) was real and the ceiling solved it for several decades. Economic historians (Kettl, 'Deficit Politics'; Kydland & Prescott on fiscal policy) corroborate the coordination function. Modern analysis by budget scholars outside Treasury (e.g., Greenspan testimony, GAO fiscal reports) confirms the procedural coordination was operational through the 1980s. The corroboration covers the founding coordination function; it does not extend to post-1995 dynamics or speak to whether weaponization has overcome the coordination function.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the coordination_scaffold_reading, extractiveness is LOW (0.18 at interval end, starting at 0.08 in 1939) because the ceiling solves a genuine coordination problem (avoiding bottlenecks) without creating systematic asymmetric extraction. Treasury benefits from operational autonomy, Congress benefits from retained authority without micromanagement, and markets benefit from predictability. Suppression is minimal (0.12) because the constraint operates through routine procedures, not coercion; ceiling adjustments occur on schedule without hostage crises. Theater ratio is very low (0.08) because the coordination function is genuine—the ceiling does the work it was built to do. Accessibility of alternatives is moderate (0.35) because the constraint could in principle be replaced by case-by-case congressional authorization or a permanent authorization, but the coordination reading treats the periodic adjustment as a feature, not a bug. Resistance is moderate (0.42) because fiscal hawks resist the ceiling (wanting it tighter) and some progressives resist (on constitutional grounds), but under the coordination reading, neither group is resisting the constraint as extractive—they are resisting its design or legitimacy. The measurement trajectory shows slow drift upward in extractiveness and theater ratio over the interval 1939–1995, reflecting modest institutional drift but NOT the sharp turn to weaponization that occurs post-1995 (which belongs to the extraction_snare_reading and lies outside this interval).
 *
 * PERSPECTIVAL GAP:
 *   Treasury and congressional leadership occupy different but aligned seats: Treasury experiences the ceiling as enabling authority (d near beneficiary end), while congressional leadership experiences it as a retained oversight tool (d near symmetric). Financial markets experience it as risk-reducing (d beneficiary end). Legislative minorities experience it as a procedural constraint they cannot unilaterally override (d near payer end, but not extractive because the procedures are routine). The engine computes this divergence: Treasury and Congress should compute as rope-beneficiaries from the coordination function, while minorities compute as payers of a procedural constraint that is not extractive. The gap widens if the extraction_snare_reading is correct—then minorities are trapped (d = 1.0) by a weaponized ceiling.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury: beneficiary (gains operational autonomy within authorized limit); power=institutional, exit=constrained (cannot unilaterally exit Treasury operations). Directionality d ≈ 0.2 (low target, high beneficiary). Congressional leadership: agenda-setter (sets the ceiling) and beneficiary (retains authority, reduces micromanagement); power=institutional, exit=mobile (can alter or eliminate the constraint). Directionality d ≈ 0.15 (beneficiary end). Financial markets: beneficiary (predictability, reduced default risk); power=powerful, exit=mobile (can divest or hedge); d ≈ 0.1. Legislative minorities: payers (constrained by routine procedures, cannot extract concessions in exchange for routine adjustments under the coordination reading); power=moderate, exit=constrained (cannot unilaterally block ceiling adjustments); d ≈ 0.6 (payer end, but NOT targeted extraction because the arrangement is non-extractive). Fiscal purists: excluded and not seated because their position (the ceiling should bite harder) belongs to the extraction_snare_reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This story avoids mandatrophy (founding problem solved but constraint persists for other reasons) by design: the founding problem (bottleneck elimination) remains LIVE across the 1939–1995 interval. Congressional practice shows repeated adjustment rather than fixed authorization, confirming that the procedural coordination function is still being performed. Mandatrophy would fire if the ceiling persisted despite the founding problem being solved and despite Congress adopting alternatives (e.g., standing authorization). That did not happen in the interval covered. Post-1995 weaponization (the extraction_snare_reading's interval) would raise mandatrophy questions: is the coordination function still being performed, or is the ceiling now theater for extractive hostage-taking? This story bounds its interval to 1939–1995 to capture the coordination function before weaponization. The committer frame places mandatrophy analysis in the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinated_vs_weaponized_boundary,
    'At what point does the ceiling shift from a procedural coordination mechanism (routine adjustments on schedule) to a weaponized hostage mechanism (adjustments delayed or conditioned on policy concessions)?',
    'Compare the historical record: pre-1995 adjustments occur routinely in response to Treasury notification vs. post-1995 delays and conditionality. The 1995–1996 shutdowns and post-2011 brinksmanship mark the transition.',
    'If the boundary is crossed, the same statutory kernel instantiates a different constraint (extraction_snare_reading) with high extractiveness and active suppression. The coordination reading remains valid for the pre-1995 interval but does not extend to post-1995 dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinated_vs_weaponized_boundary, empirical, 'Whether the ceiling''s function remains coordination or shifts to extraction over time.').

omega_variable(
    founding_problem_salience_drift,
    'Does the procedural coordination value of the ceiling persist across the interval, or does Congress''s institutional capacity for managing budget processes reduce the coordination function''s necessity?',
    'Compare Congressional Budget Act (1974) reforms and their effect on the ceiling''s role. If CBA adoption reduced the coordination problem, the ceiling''s function becomes more theatrical.',
    'If the founding problem atrophies while the ceiling persists, mandatrophy emerges: the constraint becomes a Piton rather than a live Scaffold. This would shift the reading from live-founding-problem to dead-founding-problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_salience_drift, empirical, 'Whether the coordination function the ceiling solves remains vital or becomes redundant.').

omega_variable(
    alternative_authorization_structures,
    'Could Congress achieve the same coordination outcome through standing authorization, permanent debt authority, or continuous re-authorization without periodic ceiling adjustments?',
    'Comparative analysis of other sovereigns'' debt authorization mechanisms (e.g., U.K., Australia, Canada). If comparable coordination is achieved without periodic adjustments, the ceiling''s coordination value is not necessary.',
    'If alternatives exist that provide coordination without the periodic-adjustment mechanism, the ceiling''s claimed coordination function becomes contingent rather than necessary, weakening the scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_authorization_structures, conceptual, 'Whether the periodic ceiling adjustment is intrinsic to the coordination function or one possible design among alternatives.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the coordination_scaffold_reading logically foreclose the extraction_snare_reading, or do they coexist as live framings of the same kernel that different parties hold simultaneously?',
    'Examine the core premises: coordination reading asserts the ceiling solves a procedural problem; extraction reading asserts it enables hostage-taking. These do not logically contradict—an arrangement can serve both functions depending on whether it is used cooperatively or adversarially. Both can be true of the same mechanism.',
    'If they coexist without foreclosure, both readings are live constraints on the same kernel, held by different parties (Treasury/Congress coordination advocates vs. legislative minorities and constitutional scholars). If the coordination reading forecloses the extraction reading, only the coordination reading is structurally valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the readings are mutually exclusive or co-possible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1939, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1939, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1939, 0.02).
narrative_ontology:measurement_basis(stat_tr_t1939, observed).
narrative_ontology:measurement(stat_tr_t1955, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1955, 0.03).
narrative_ontology:measurement_basis(stat_tr_t1955, observed).
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1970, 0.04).
narrative_ontology:measurement_basis(stat_tr_t1970, observed).
narrative_ontology:measurement(stat_tr_t1980, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement_basis(stat_tr_t1980, observed).
narrative_ontology:measurement(stat_tr_t1990, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement_basis(stat_tr_t1990, observed).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement_basis(stat_tr_t1995, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1939, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1939, 0.08).
narrative_ontology:measurement_basis(stat_be_t1939, observed).
narrative_ontology:measurement(stat_be_t1955, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1955, 0.1).
narrative_ontology:measurement_basis(stat_be_t1955, observed).
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement_basis(stat_be_t1970, observed).
narrative_ontology:measurement(stat_be_t1980, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement_basis(stat_be_t1980, observed).
narrative_ontology:measurement(stat_be_t1990, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1990, 0.16).
narrative_ontology:measurement_basis(stat_be_t1990, observed).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement_basis(stat_be_t1995, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1939, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1939, 0.04).
narrative_ontology:measurement_basis(stat_su_t1939, observed).
narrative_ontology:measurement(stat_su_t1955, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1955, 0.06).
narrative_ontology:measurement_basis(stat_su_t1955, observed).
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement_basis(stat_su_t1970, observed).
narrative_ontology:measurement(stat_su_t1980, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement_basis(stat_su_t1980, observed).
narrative_ontology:measurement(stat_su_t1990, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1990, 0.11).
narrative_ontology:measurement_basis(stat_su_t1990, observed).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement_basis(stat_su_t1995, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling kernel decomposes into three structurally distinct constraints, each with different ε values and beneficiary/victim structures. The coordination_scaffold_reading (this story, low ε, procedural coordination) is the upstream claim in the family: the extraction_snare_reading and constitutional_nullity_reading both presuppose the ceiling's existence and contest its legitimacy or function. The family is linked via network.affects_constraints: each reading influences the others through legitimacy challenge, not through direct causal dependence. The three readings should be consulted together to understand the full contested structure; none is complete alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
