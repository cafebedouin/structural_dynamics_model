% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Means-Tested Income Support Targeting Principle
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   The targeting_efficiency_reading instantiates the constraint that income
 *   support should be concentrated on demonstrated need via means-testing,
 *   opposing universal basic income. This reading claims the targeted system
 *   is efficient coordination (rope): it solves the genuine coordination
 *   problem of allocating scarce fiscal resources to those most in need. The
 *   authored metrics describe a system with moderate extraction (0.42) from
 *   administrative burden, eligibility cliffs excluding the near-poor, and
 *   stigma costs — and rising theater (0.38) as compliance rituals
 *   increasingly serve bureaucratic self-justification rather than accurate
 *   targeting. The claim/metric gap is deliberate: the reading claims rope
 *   while metrics show extractive friction; the engine measures that
 *   divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.42).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.55).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, rope).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Means-Tested Income Support Targeting Principle").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '32d2290d-6ee6-4005-821a-8ff49ad9871e').
narrative_ontology:cs_kernel_codification('32d2290d-6ee6-4005-821a-8ff49ad9871e', formalized).
narrative_ontology:cs_authority_grounding('32d2290d-6ee6-4005-821a-8ff49ad9871e', extraction).
narrative_ontology:cs_interpretation_layer_present('32d2290d-6ee6-4005-821a-8ff49ad9871e').
narrative_ontology:cs_reading_relation('32d2290d-6ee6-4005-821a-8ff49ad9871e', income_support_commitment__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('32d2290d-6ee6-4005-821a-8ff49ad9871e', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('32d2290d-6ee6-4005-821a-8ff49ad9871e', foundational, resources_are_finite_and_must_be_targeted).
narrative_ontology:cs_axiom_status(resources_are_finite_and_must_be_targeted, holdable).
narrative_ontology:cs_axiom_grounding('32d2290d-6ee6-4005-821a-8ff49ad9871e', resources_are_finite_and_must_be_targeted, empirically_contingent).
narrative_ontology:cs_axiom('32d2290d-6ee6-4005-821a-8ff49ad9871e', foundational, need_based_allocation_maximizes_poverty_reduction_per_dollar).
narrative_ontology:cs_axiom_status(need_based_allocation_maximizes_poverty_reduction_per_dollar, holdable).
narrative_ontology:cs_axiom_grounding('32d2290d-6ee6-4005-821a-8ff49ad9871e', need_based_allocation_maximizes_poverty_reduction_per_dollar, empirically_contingent).
narrative_ontology:cs_reference_frame('32d2290d-6ee6-4005-821a-8ff49ad9871e', means_tested_welfare_state).
narrative_ontology:cs_drift_state('32d2290d-6ee6-4005-821a-8ff49ad9871e', ubi_proposal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('32d2290d-6ee6-4005-821a-8ff49ad9871e', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, low_income_families_with_children).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, near_poor_excluded_by_cliffs).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, taxpayers_funding_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, resource_scarcity_requires_targeting).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, means_testing_maximizes_poverty_reduction_per_dollar).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive means-tested benefits (e.g., Queens parent: $31,100/year across housing, nutrition, child support). Face compliance burden: recertification, asset tests, work requirements. Lose benefits sharply at income cliffs (effective marginal tax rates >100%). Stigma reduces take-up. Cannot exit the system without losing survival income; changing programs requires navigating bureaucratic maze.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, payer).

% Earn slightly above eligibility thresholds (e.g., $45,000-$60,000 for family of three). Pay full taxes funding the system but receive zero transfers. Face effective marginal tax rates exceeding 100% if they increase earnings and lose residual benefits. No voice in program design; political representation captures middle-class tax concerns not near-poor exclusion. Exit options: reduce earnings to qualify (perverse incentive) or relocate to lower-cost jurisdiction.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, near_poor_excluded_by_cliffs, payer,
    moderate, biographical, constrained, national).

% Fund both transfers and administrative overhead (~15-20% of program costs). Organized through taxpayer associations, business groups, fiscal conservative coalitions. Can exit via tax avoidance, relocation, or political pressure for reform. Benefit from social stability the system provides but contest its cost and efficiency.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, taxpayers_funding_administration, payer,
    organized, generational, mobile, national).

% Design and enforce eligibility rules, manage compliance apparatus, adjudicate appeals. Collect institutional rents: budget authority, staffing, rule-making power. Justify the system as fraud prevention and targeting accuracy. Can transition to private-sector consulting or policy roles; their exit is arbitrage-grade. The compliance machinery they maintain is the constraint's active enforcement.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue for universal unconditional cash transfers as replacement. Structurally excluded from current program design — their proposal would dismantle the administrators' authority and the targeted recipients' concentrated benefits. Would benefit from universal floor but lose targeted intensity. Their voice appears in pilot programs and legislative proposals but not in operational rule-making.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_advocates, excluded,
    organized, generational, mobile, global).

% Evaluate targeting efficiency, cliff effects, administrative costs, and labor supply responses across regimes. No material stake in the constraint's persistence. Provide evidence for all three readings. Their exit is analytical: they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce fiscal resources to those with demonstrated material need, avoiding universal dilution that would reduce per-recipient adequacy or require politically infeasible tax increases.
% TRANSFER_FUNCTION: Moves tax revenue from broad taxpayer base (concentrated on middle/upper income) to low-income households with children, disabilities, or unemployment — mediated by administrative apparatus that verifies need and enforces eligibility boundaries.
% ABSENT_VOICES: The near-poor excluded by cliffs (stakeholder near_poor_excluded_by_cliffs) would object to funding a system that excludes them at the margin. Future generations who will bear debt from current welfare financing are not represented. Non-citizen residents with need but no eligibility are structurally absent.
% DISAPPEARANCE_RATIONALE: If means-testing vanished overnight, either (a) universal replacement at current spending levels would slash per-recipient benefits by ~60% (world rearranges for current recipients), or (b) spending would need to triple to maintain adequacy (world rearranges for taxpayers). The targeted system's beneficiaries and payers both depend on its current calibration.
% FOUNDING_PROBLEM: Post-WWII/War on Poverty: how to alleviate material deprivation without creating permanent dependency or requiring confiscatory taxation — solved by concentrating aid on demonstrable need via means-testing.
% FOUNDING_PROBLEM_CORROBORATION: Targeting_efficiency_reading proponents (CBPP, progressive policy institutes) attest the problem is live: poverty persists, targeting remains fiscally necessary. Freedom_floor_reading proponents (UBI advocates, some labor economists) attest it's dead: universality is now affordable and targeting's administrative costs exceed its savings. Dependency_trap_reading proponents (conservative welfare reformers) attest it's live but misdiagnosed: the problem is work disincentives, not targeting per se. No consensus outside beneficiary sets.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).
:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects administrative overhead (15-20% of benefits), cliff effects that penalize earnings, and stigma costs — real but not dominant. Suppression (0.55) comes from eligibility rules that actively gatekeep, not merely passive exclusion. Theater ratio (0.38) rising over time: fraud-prevention rituals and work-requirement compliance consume growing administrative capacity without improving targeting accuracy. The measurement series runs on a shared time grid (T=0,10,20,30) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter (administrator) seat, the system is coordination: it rations scarce funds. From the near_poor_excluded seat, it is extraction: they fund a system that excludes them by design. From the targeted_recipient seat, it is ambiguous: benefit receipt coexists with compliance extraction. The engine computes this divergence; the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted recipients are structural beneficiaries (d≈0.2): they receive concentrated transfers but face compliance burden and cliffs. Near-poor excluded by cliffs are payers (d≈0.8): they bear tax costs without receiving benefits, and face marginal tax rates >100% at phase-out boundaries. Taxpayers are moderate payers (d≈0.6). Administrators are agenda_setters with arbitrage exit (d≈0.15). The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war poverty alleviation via targeted relief) is contested: targeting_efficiency_reading says it's live (need persists), freedom_floor_reading says it's dead (universal floor is now feasible), dependency_trap_reading says it's live but misdiagnosed (work disincentives are the real problem). The targeted system persists despite UBI proposals — not because the founding problem is solved, but because no coalition can agree on replacement. This is mandatrophy: the arrangement outlives its consensus but not its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (targeting_efficiency_reading) of the contested kernel income_support_commitment. What does the sibling reading freedom_floor_reading change structurally?',
    'Compare the beneficiary/victim structures and ε values across the three readings. The freedom_floor_reading instantiates a UBI constraint with universal beneficiaries and concentrated taxpayer victims; the dependency_trap_reading instantiates work-conditioned support with different exclusion dynamics.',
    'If the kernel framing is accepted, this reading''s ε=0.42 for the targeted system competes with freedom_floor_reading''s ε for UBI (which this reading claims is higher). The classification divergence between readings measures the kernel''s contestedness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee frame: this reading''s constraint vs. sibling readings'' constraints from same kernel').

omega_variable(
    ubi_as_snare_claim,
    'Does the targeting_efficiency_reading''s claim that UBI funded by cannibalizing targeted programs constitutes a snare (ε≈0.7, victims=current recipients) hold under empirical scrutiny?',
    'Micro-simulation of UBI replacement financing: if UBI is funded by eliminating targeted programs + broad-based taxes, trace net transfers for current targeted recipients. Compare extraction/suppression metrics of the resulting universal system.',
    'If confirmed, the freedom_floor_reading''s instantiated constraint classifies as snare from this reading''s seat, creating a cross-reading extraction asymmetry. If false, the readings'' conflict is about values not structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ubi_as_snare_claim, empirical, 'Whether UBI replacement extracts from current targeted recipients as this reading claims').

omega_variable(
    administrative_extraction_vs_coordination,
    'Is the targeted system''s administrative burden (means-testing, cliffs, stigma) structural extraction from recipients, or necessary coordination cost?',
    'Compare administrative cost per transfer dollar in targeted vs. universal systems; measure labor supply effects at eligibility cliffs; survey recipient experience of compliance burden.',
    'If administrative burden is extractive (not coordination), the targeted system''s ε rises toward tangled_rope/snare. If coordination, the reading''s rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_extraction_vs_coordination, empirical, 'Whether means-testing overhead is coordination cost or extractive friction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.55) structural (eligibility rules, cliffs, bureaucratic gatekeeping) or internalized (recipients believing they don''t deserve more, stigma preventing claims)?',
    'Post-reform suppression trajectory: if suppression persists after eligibility expansion or automation reduces gatekeeping, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — recipients carry the suppression with them after policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in means-tested welfare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isc_te_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(isc_te_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(isc_te_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(isc_te_tr_t30, income_support_commitment__targeting_efficiency_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(isc_te_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(isc_te_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(isc_te_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(isc_te_be_t30, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(isc_te_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(isc_te_su_t10, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(isc_te_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(isc_te_su_t30, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__targeting_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% Kernel income_support_commitment decomposes into three constraint stories: targeting_efficiency_reading (this file, ε=0.42 claimed rope), freedom_floor_reading (UBI, ε≈0.3 claimed rope by proponents but ε≈0.7 snare from this reading's seat), dependency_trap_reading (work-conditioned, ε≈0.5 tangled_rope). All three share the kernel's commitment to income support but instantiate different constraints with different beneficiary/victim structures. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, moderate, 0.65).
constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
