% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy: Qualitative Development Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint story captures the 'qualitative development' reading of
 *   the performance legitimacy kernel — the claim that the regime's
 *   legitimacy rests on successfully steering the economy toward innovation,
 *   sustainability, and efficiency gains rather than raw GDP growth. The
 *   reading became authoritative after 2012, redefining performance metrics
 *   around 'high-quality development' (创新、协调、绿色、开放、共享). Structurally, it
 *   operates as a tangled rope: it solves a genuine coordination problem
 *   (escaping the middle-income trap via directed upgrading) while
 *   simultaneously extracting from traditional sectors and property-dependent
 *   local governments to fund the innovation coalition. The extraction has
 *   intensified over the interval as the innovation push deepened
 *   (semiconductors, AI, new energy) and the property model exhausted itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.48).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '79aaf731-0385-4b0d-940a-cd8fa3294cb4').
narrative_ontology:cs_kernel_codification('79aaf731-0385-4b0d-940a-cd8fa3294cb4', formalized).
narrative_ontology:cs_authority_grounding('79aaf731-0385-4b0d-940a-cd8fa3294cb4', extraction).
narrative_ontology:cs_interpretation_layer_present('79aaf731-0385-4b0d-940a-cd8fa3294cb4').
narrative_ontology:cs_reading_relation('79aaf731-0385-4b0d-940a-cd8fa3294cb4', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('79aaf731-0385-4b0d-940a-cd8fa3294cb4', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('79aaf731-0385-4b0d-940a-cd8fa3294cb4', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('79aaf731-0385-4b0d-940a-cd8fa3294cb4', foundational, structural_transformation_as_legitimacy_standard).
narrative_ontology:cs_axiom_status(structural_transformation_as_legitimacy_standard, holdable).
narrative_ontology:cs_axiom_grounding('79aaf731-0385-4b0d-940a-cd8fa3294cb4', structural_transformation_as_legitimacy_standard, instrumental).
narrative_ontology:cs_axiom('79aaf731-0385-4b0d-940a-cd8fa3294cb4', foundational, growth_rate_tolerance_for_quality_upgrading).
narrative_ontology:cs_axiom_status(growth_rate_tolerance_for_quality_upgrading, holdable).
narrative_ontology:cs_axiom_grounding('79aaf731-0385-4b0d-940a-cd8fa3294cb4', growth_rate_tolerance_for_quality_upgrading, instrumental).
narrative_ontology:cs_axiom('79aaf731-0385-4b0d-940a-cd8fa3294cb4', secondary, innovation_ecosystem_primacy_over_employment_volume).
narrative_ontology:cs_axiom_status(innovation_ecosystem_primacy_over_employment_volume, holdable).
narrative_ontology:cs_axiom_grounding('79aaf731-0385-4b0d-940a-cd8fa3294cb4', innovation_ecosystem_primacy_over_employment_volume, instrumental).
narrative_ontology:cs_reference_frame('79aaf731-0385-4b0d-940a-cd8fa3294cb4', post_2012_high_quality_development_mandate).
narrative_ontology:cs_drift_state('79aaf731-0385-4b0d-940a-cd8fa3294cb4', post_property_crisis_2021, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('79aaf731-0385-4b0d-940a-cd8fa3294cb4', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, venture_capital_mna_infrastructure).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, labor_in_sunset_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, innovation_driven_growth_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, structural_transformation_imperative).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, efficiency_over_volume_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the strategic pivot from quantitative to qualitative growth targets; authorizes industrial policy, capital allocation, and regulatory frameworks that privilege high-tech sectors. Legitimacy narrative is authored at this level; the constraint is maintained through policy directives, credit guidance, and personnel appointments.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_leadership, agenda_setter,
    institutional, generational, analytical, national).

% Receive directed credit, tax incentives, talent recruitment support, and procurement preferences. Also bear compliance costs, data localization requirements, and strategic alignment mandates. Exit is constrained by dependence on state-backed ecosystems and regulatory moats.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, high_tech_sectors, payer).

% National labs, state-affiliated research institutes, and strategic SOEs in semiconductors, AI, biotech, and new energy capture the bulk of upgraded R&D budgets and talent flows. Their institutional survival is fused to the qualitative development narrative.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, analytical, national).

% Policy-guided funds, government-backed VC, and M&A channels for tech transfer are prioritized in capital allocation. They extract management fees and carry from directed flows; exit options include global LP diversification but are practically constrained by policy alignment requirements.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, venture_capital_mna_infrastructure, beneficiary,
    organized, biographical, mobile, national).

% Face credit tightening, rising compliance costs (environmental, safety, upgrading mandates), and land-use pressure from higher-value zoning. Workforce ages and shrinks; capital stock depreciates without replacement investment. Exit is constrained by sunk assets, local employment obligations, and limited alternative sites.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    organized, biographical, constrained, regional).

% Fiscal model relies on land conveyance fees and property-linked revenues. Industrial land redevelopment for high-tech parks yields one-time windfalls but erodes the long-term tax base. Caught between central upgrading mandates and local revenue dependence; trapped by intergovernmental fiscal structure.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, beneficiary).

% Older, lower-skilled workers in declining sectors face wage stagnation, benefit erosion, and displacement without viable retraining pathways. Social insurance portability is limited; geographic mobility is constrained by hukou-linked benefits and family obligations. Bear the adjustment costs of structural transformation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, labor_in_sunset_industries, payer,
    powerless, immediate, trapped, local).

% Analyze the efficiency of directed credit, the authenticity of innovation metrics, and the distributional consequences of the qualitative pivot. Their assessments inform international discourse but have limited domestic policy uptake when findings contradict the official narrative.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, independent_economists, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide structural transformation by aligning capital, talent, and regulatory capacity toward innovation-intensive sectors — solving the collective action problem of escaping the middle-income trap through directed upgrading rather than spontaneous market forces.
% TRANSFER_FUNCTION: Moves fiscal resources, credit, land, and talent from traditional manufacturing, property-dependent local budgets, and labor in sunset industries toward high-tech sectors, state-backed innovation institutions, and policy-guided venture capital infrastructure.
% ABSENT_VOICES: Migrant workers in export manufacturing, small-town economies dependent on single factories, and rural populations whose land was converted for industrial parks without adequate compensation — they would object to the pace and distributional asymmetry but are excluded from policy deliberation by hukou barriers and representation gaps.
% DISAPPEARANCE_RATIONALE: If the qualitative development mandate vanished overnight, credit would revert to property and infrastructure, local governments would resume land-finance cycles, high-tech sectors would lose directed capital and face a funding cliff, and the innovation ecosystem would fragment — the entire industrial upgrading trajectory would collapse or radically reorient.
% FOUNDING_PROBLEM: After 2008, the export-led, investment-heavy model hit diminishing returns: overcapacity in steel, cement, and basic manufacturing; environmental degradation; rising debt; and the middle-income trap loomed. The founding problem was how to sustain development without the old growth engines.
% FOUNDING_PROBLEM_CORROBORATION: Central leadership and state-affiliated think tanks attest the problem remains live — upgrading is incomplete and new bottlenecks (chips, basic science) persist. Independent economists, international organizations, and displaced workers' advocates attest the original overcapacity problem has been displaced into new imbalances (property crisis, youth unemployment, local fiscal distress) and the arrangement now serves as a rent-distribution mechanism for the innovation coalition.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the scale of resource reallocation: directed credit to strategic sectors, fiscal transfers to innovation ecosystems, and implicit subsidies via land and talent policies. Suppression (0.48) is moderate — the constraint relies more on capital allocation and regulatory privilege than overt coercion, but exit is blocked for trapped agents (local governments, sunset-industry labor). Theater ratio (0.38) has risen as 'high-quality development' rhetoric increasingly decorates industrial policies that function as rent distribution to the innovation coalition. Accessibility collapse (0.41) is partial — alternative development models exist but are marginalized in policy space. Resistance (0.55) is significant from local governments facing fiscal squeeze and labor bearing adjustment costs.
 *
 * PERSPECTIVAL GAP:
 *   From the central leadership seat, the constraint is a necessary coordination mechanism — the only viable path past the middle-income trap. From traditional manufacturing and local government seats, it is an enforced extraction that dismantles their fiscal and productive base. From sunset labor, it is a trap with no exit. The engine computes these divergent per-seat types from the structural data; the claimed tangled_rope reflects the authoring seat's assessment that both coordination and extraction are genuine and structurally fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Central leadership (agenda_setter) sits at d ≈ 0.15 — the constraint subsidizes its legitimacy. High-tech sectors and innovation ecosystem (beneficiaries) sit at d ≈ 0.25-0.35 — they collect directed resources but bear strategic alignment costs. Traditional manufacturing, property-dependent local governments, and sunset labor (payers) sit at d ≈ 0.7-0.9 — they bear the extraction with constrained or trapped exit. VC/M&A infrastructure (beneficiary) sits at d ≈ 0.3 — mobile exit but policy-dependent flows. Independent economists (observer) sit at d = 0.5 (analytical). The directionality derivation from beneficiary/victim declarations plus exit options captures this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (escaping the middle-income trap via upgrading) was live in 2012 and remains partially live (bottlenecks persist in core technologies). However, the arrangement has accumulated extraction layers — directed credit, land privileges, talent mandates — that now serve the innovation coalition's interests beyond the coordination function. The mandatrophy is unresolved: the constraint persists because the coalition it created is now powerful enough to defend it, not solely because the founding problem demands it. The founding_problem_status = contested and disappearance_verdict = world_rearranges capture this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'How much of the resource reallocation toward high-tech sectors is structurally necessary for the upgrading coordination function, versus how much is extractive rent capture by the innovation coalition?',
    'Counterfactual analysis: would a market-based allocation with carbon pricing, IP protection, and basic research funding achieve comparable upgrading with less extraction? Compare TFP growth in directed vs. non-directed sectors.',
    'If most reallocation is coordination-necessary, the constraint leans rope; if predominantly extractive, it leans snare. Current metrics place it in tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the qualitative development pivot''s resource transfers are coordination costs or coalition rents.').

omega_variable(
    local_government_fiscal_trap,
    'Is the property-dependent local government fiscal crisis a temporary transition cost or a structural feature of the qualitative development model?',
    'Track local fiscal health in regions that successfully attracted high-tech investment vs. those that did not. If successful regions still face structural revenue gaps (high-tech generates less land revenue per hectare), the trap is structural.',
    'If structural, the constraint extracts from local governments as a permanent fiscal design, not a transition cost — deepening the snare character for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(local_government_fiscal_trap, empirical, 'Whether local government victimization is transitional or baked into the model.').

omega_variable(
    kernel_reading_ambiguity,
    'Does the qualitative_development_reading genuinely cohere as a distinct legitimacy claim, or is it a rhetorical overlay that masks the techno_nationalist_reading''s strategic autonomy imperative?',
    'Analyze policy documents and resource flows: when qualitative targets (efficiency, green) conflict with techno-nationalist targets (self-sufficiency, supply chain control), which prevails in capital allocation and regulatory decisions?',
    'If techno-nationalist imperatives consistently override qualitative ones, the readings are not distinct — this constraint story would be mis-specified as a separate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether qualitative development and techno-nationalist readings are structurally separable or fused in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_leg_qual_dev_tr_t2012, performance_legitimacy__qualitative_development_reading, theater_ratio, 2012, 0.12).
narrative_ontology:measurement(perf_leg_qual_dev_tr_t2015, performance_legitimacy__qualitative_development_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(perf_leg_qual_dev_tr_t2018, performance_legitimacy__qualitative_development_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(perf_leg_qual_dev_tr_t2021, performance_legitimacy__qualitative_development_reading, theater_ratio, 2021, 0.32).
narrative_ontology:measurement(perf_leg_qual_dev_tr_t2024, performance_legitimacy__qualitative_development_reading, theater_ratio, 2024, 0.36).
narrative_ontology:measurement(perf_leg_qual_dev_tr_t2026, performance_legitimacy__qualitative_development_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(perf_leg_qual_dev_be_t2012, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2012, 0.28).
narrative_ontology:measurement(perf_leg_qual_dev_be_t2015, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(perf_leg_qual_dev_be_t2018, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement(perf_leg_qual_dev_be_t2021, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(perf_leg_qual_dev_be_t2024, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2024, 0.6).
narrative_ontology:measurement(perf_leg_qual_dev_be_t2026, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perf_leg_qual_dev_su_t2012, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2012, 0.22).
narrative_ontology:measurement(perf_leg_qual_dev_su_t2015, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(perf_leg_qual_dev_su_t2018, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement(perf_leg_qual_dev_su_t2021, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2021, 0.43).
narrative_ontology:measurement(perf_leg_qual_dev_su_t2024, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2024, 0.46).
narrative_ontology:measurement(perf_leg_qual_dev_su_t2026, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2026, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, local_government_land_finance).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, state_sector_credit_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, hukou_migration_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel. The kernel's other readings instantiate distinct constraints with different ε values, beneficiary/victim structures, and coordination/extraction balances. The qualitative_development_reading prioritizes structural transformation metrics and tolerates lower growth; the quantitative_growth_reading prioritizes GDP growth rates; the techno_nationalist_reading prioritizes strategic autonomy; the livelihood_security_reading prioritizes distributional outcomes. All four compete for authority within the same legitimacy framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, institutional, 0.15).
constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, organized, 0.3).
constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
