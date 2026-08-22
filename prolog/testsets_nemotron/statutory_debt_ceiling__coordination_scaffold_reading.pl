% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   This reading of the statutory debt ceiling treats it as a procedural
 *   coordination scaffold enacted in 1917 (Second Liberty Bond Act) to
 *   replace the prior regime of Congress authorizing each bond issuance
 *   individually. The constraint aggregates borrowing authority into a single
 *   statutory limit, allowing Treasury to manage cash flow, debt maturity
 *   structure, and market operations without returning to Congress for every
 *   auction. The coordination function is genuine: financial markets price
 *   Treasury securities against a known aggregate ceiling; Treasury plans
 *   issuance calendars within that ceiling; Congress retains the power to
 *   adjust the ceiling when aggregate borrowing approaches the limit.
 *   Historically (1917–1995), adjustments were routine, near-unanimous, and
 *   decoupled from policy hostage-taking. The extractiveness of this reading
 *   is low because the ceiling itself does not transfer resources — it
 *   coordinates the timing and form of legislative consent. Theatricality
 *   rose after 1995 as the constraint was repurposed for leverage, but this
 *   reading isolates the scaffold function from the later extraction layer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Procedural Coordination Scaffold").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, '0f04adef-59b9-413b-9bdd-73931d2fbead').
narrative_ontology:cs_kernel_codification('0f04adef-59b9-413b-9bdd-73931d2fbead', formalized).
narrative_ontology:cs_authority_grounding('0f04adef-59b9-413b-9bdd-73931d2fbead', lineage).
narrative_ontology:cs_interpretation_layer_present('0f04adef-59b9-413b-9bdd-73931d2fbead').
narrative_ontology:cs_reading_relation('0f04adef-59b9-413b-9bdd-73931d2fbead', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f04adef-59b9-413b-9bdd-73931d2fbead', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('0f04adef-59b9-413b-9bdd-73931d2fbead', foundational, aggregate_authorization_serves_coordination_not_extraction).
narrative_ontology:cs_axiom_status(aggregate_authorization_serves_coordination_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('0f04adef-59b9-413b-9bdd-73931d2fbead', aggregate_authorization_serves_coordination_not_extraction, conventional).
narrative_ontology:cs_axiom('0f04adef-59b9-413b-9bdd-73931d2fbead', foundational, treasury_operational_autonomy_within_ceiling_is_legitimate).
narrative_ontology:cs_axiom_status(treasury_operational_autonomy_within_ceiling_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0f04adef-59b9-413b-9bdd-73931d2fbead', treasury_operational_autonomy_within_ceiling_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('0f04adef-59b9-413b-9bdd-73931d2fbead', second_liberty_bond_act_1917_coordination_regime).
narrative_ontology:cs_drift_state('0f04adef-59b9-413b-9bdd-73931d2fbead', post_1995_brinkmanship_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f04adef-59b9-413b-9bdd-73931d2fbead', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congress_fiscal_oversight).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_operations).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, financial_market_participants).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, legislative_power_of_the_purse).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, fiscal_responsibility_norm).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, treasury_operational_autonomy_within_aggregate_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains the constitutional power of the purse in aggregated form. Sets the statutory ceiling and adjusts it when aggregate borrowing approaches the limit. Avoids the transaction cost of authorizing each bond issuance individually (the pre-1917 regime). Can suspend, raise, or abolish the ceiling at will. Benefits from a predictable legislative workflow for fiscal authority.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congress_fiscal_oversight, agenda_setter,
    institutional, generational, arbitrage, national).

% Manages federal debt issuance, cash flow, and maturity structure within the aggregate ceiling. Plans auction calendars, responds to market conditions, and uses extraordinary measures when the ceiling binds. Gains operational autonomy from not needing per-issuance congressional approval. Constrained by the ceiling's hard limit and by political dynamics around adjustment.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_operations, beneficiary,
    institutional, biographical, constrained, national).

% Price Treasury securities against a known aggregate borrowing limit. The ceiling provides a fiscal anchor for the risk-free rate and benchmark pricing. Benefit from predictable issuance calendars and deep, liquid markets. Can shift to alternative sovereign debt or private assets if ceiling dysfunction raises perceived risk, but the dollar system's centrality makes exit costly.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, financial_market_participants, beneficiary,
    organized, biographical, mobile, global).

% Under this reading, these actors are not part of the coordination function. They would use the ceiling as leverage for policy concessions (the extraction_snare_reading's payer seat), but the scaffold reading treats that use as contamination, not the constraint's intrinsic structure. Their exclusion from the coordination scaffold is what the snare reading exploits.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, legislative_minority_factions, excluded,
    organized, biographical, trapped, national).

% Analyze the ceiling's constitutionality under the 14th Amendment Section 4 (constitutional_nullity_reading) and its interaction with the separation of powers. Provide the interpretive framework that either validates the scaffold, condemns it as void, or characterizes it as a weaponizable boundary.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_scholars_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates congressional borrowing authorization into a single statutory limit, replacing the pre-1917 regime of per-issuance legislative approval. Allows Treasury to manage debt operations (issuance timing, maturity structure, cash management) with operational autonomy while preserving Congress's constitutional power of the purse. Provides financial markets with a predictable fiscal anchor for pricing Treasury securities.
% TRANSFER_FUNCTION: Moves legislative transaction costs from Congress (avoiding per-issuance votes) to a periodic aggregate adjustment. No systematic resource transfer between constituencies under this reading — the ceiling coordinates the timing of consent, it does not extract. The 'cost' is the occasional legislative action to raise the ceiling, distributed across the political system.
% ABSENT_VOICES: Future taxpayers and program beneficiaries who would bear the costs of a default triggered by ceiling brinkmanship. They are not present in the coordination scaffold (which assumes routine adjustment) but are the victims in the extraction_snare_reading. Also absent: state and local governments whose federal funding streams are threatened by ceiling standoffs.
% DISAPPEARANCE_RATIONALE: If the statutory ceiling vanished overnight, Congress would need to revert to per-issuance authorization or enact a new aggregate mechanism. Treasury would lose its current operational autonomy. Financial markets would lose the fiscal anchor the ceiling provides. The world rearranges because the coordination function is real and actively used — the scaffold bears load.
% FOUNDING_PROBLEM: Pre-1917, Congress had to authorize each individual bond issuance by separate legislation. This became unworkable as federal borrowing scaled during WWI — the legislative transaction cost of individual authorizations exceeded Congress's capacity to manage them in a timely manner.
% FOUNDING_PROBLEM_CORROBORATION: The Second Liberty Bond Act of 1917's legislative history explicitly states the purpose: 'to avoid the necessity of Congress passing upon each separate issue of bonds.' The Congressional Research Service and Treasury's own historical accounts confirm the coordination rationale. No serious scholar disputes that the aggregate ceiling replaced per-issuance authorization for transaction-cost reasons. The snare reading does not dispute the founding problem; it argues the solution was later repurposed.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.12) reflects the minimal resource transfer inherent in the ceiling as a coordination device — the cost is the administrative burden of periodic legislative action, not a systematic extraction from any constituency. Suppression (0.18) is low because the constraint does not foreclose alternatives (Congress can raise, suspend, or abolish the ceiling; Treasury has extraordinary measures). Theater ratio (0.22) captures the growing performative dimension: debt-ceiling 'crises' after 1995 involve staged brinkmanship, but the scaffold reading treats this as contamination from the extraction_snare_reading, not the scaffold's intrinsic nature. Accessibility collapse (0.35) is moderate: the ceiling is well-understood by market participants and Congress, but its bindingness is contested (14th Amendment arguments, platinum coin seigniorage). Resistance (0.25) is low from the scaffold's perspective — the constraint operates with broad institutional acquiescence when functioning as coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the scaffold seat (Congress + Treasury + markets as a functional unit), the constraint is a low-friction coordination mechanism. From the extraction_snare seat (legislative minority using default threat), the same statutory text is a high-extraction weapon. The engine computes this divergence from the structural data: the scaffold reading declares no victims and low suppression; the snare reading declares victims and high suppression. Both are valid readings of the same kernel — the kernel_id is the statutory text; the readings are different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress (agenda_setter) benefits from retaining the power of the purse in aggregated form — avoids micromanagement while preserving constitutional authority. Treasury (beneficiary) gains operational autonomy within the aggregate limit — can optimize issuance for market conditions. Financial market participants (beneficiary) gain a predictable fiscal anchor for pricing risk-free securities. No constituency is systematically extracted from under this reading; the 'payer' role is empty because the coordination function distributes transaction-cost savings across all three. The extraction_snare_reading introduces a payer seat (the public / future taxpayers / program beneficiaries threatened by default leverage), but that seat belongs to a different constraint instantiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (replacing per-issuance congressional authorization with an aggregate limit) remains live: Congress still does not want to authorize each Treasury auction individually. The scaffold has not atrophied — its coordination function is actively used every time Treasury manages debt within the ceiling. However, the constraint now carries a parasitic extraction layer (the snare reading) that uses the scaffold's legitimacy as cover. Mandatrophy is not resolved because the original function persists; the contamination is the problem, not the scaffold's obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_snare_boundary,
    'At what point does the procedural coordination scaffold become inseparable from the extraction snare — i.e., when does the ceiling''s routine adjustment function become structurally dependent on the threat of default?',
    'Counterfactual analysis: if the ceiling were adjusted by a standing automatic mechanism (e.g., Gephardt Rule made permanent), would the coordination function survive without the extraction layer? Historical comparison: 1917–1995 routine adjustments vs. post-1995 brinkmanship frequency.',
    'If inseparable, the scaffold reading is a false description — the constraint is only ever the snare. If separable, the scaffold is a real but contaminated constraint, and the snare is a distinct parasitic layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_snare_boundary, conceptual, 'Whether the coordination and extraction functions are structurally separable or fused.').

omega_variable(
    committer_frame_disagreement_locus,
    'Which structural element do the three readings of the statutory_debt_ceiling kernel disagree on?',
    'Map each reading''s ε, beneficiary/victim structure, and claimed_type to the kernel''s statutory text. The disagreement locus is the element that changes across readings while the kernel text stays fixed.',
    'Identifies whether the contest is about extractiveness (scaffold vs snare), legitimacy (scaffold/nullity vs nullity), or both. This is the committer-axis disagreement location for the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_locus, conceptual, 'Structural locus of disagreement among kernel readings: extractiveness, beneficiary structure, and type assignment.').

omega_variable(
    fourteenth_amendment_interaction,
    'Does the constitutional_nullity_reading''s claim (14th Amendment Section 4 voids the ceiling) foreclose the scaffold reading''s coordination function, or can the scaffold operate as a sub-constitutional procedural rule?',
    'Legal analysis of whether a procedural coordination device that does not itself repudiate debt can coexist with a constitutional prohibition on questioning debt validity.',
    'If the nullity reading forecloses the scaffold, the two readings cannot coexist in one legal framework (forecloses relation). If the scaffold can operate as a sub-constitutional rule, they coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourteenth_amendment_interaction, conceptual, 'Whether the constitutional nullity claim logically eliminates the procedural coordination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(stat_tr_t1940, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1940, 0.12).
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement(stat_be_t1940, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1940, 0.08).
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2011, 0.18).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1917, 0.05).
narrative_ontology:measurement(stat_su_t1940, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1940, 0.08).
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1995, 0.18).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2011, 0.35).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, treasury_extraordinary_measures).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, gephardt_rule).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, congressional_budget_process).

% DUAL FORMULATION NOTE:
% This constraint is the coordination_scaffold_reading of the statutory_debt_ceiling kernel. The extraction_snare_reading and constitutional_nullity_reading are sibling constraints with the same kernel_id but different reading_ids. The scaffold reading has low extractiveness (0.12) and no victims; the snare reading has high extractiveness and victims; the nullity reading claims the constraint is void. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
