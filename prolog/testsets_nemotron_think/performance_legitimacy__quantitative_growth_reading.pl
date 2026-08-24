% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Legitimacy Grounded in GDP Growth Rate Maintenance
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint captures the quantitative growth reading of the
 *   performance legitimacy kernel: the regime's claim to rule rests on
 *   delivering GDP growth rates that demonstrate continued economic expansion
 *   and job creation. The growth target operates as a tangible,
 *   non-ideological metric that coordinates decentralized actors (local
 *   governments, SOEs, banks) toward a common objective while concentrating
 *   extraction on households (suppressed consumption share), the environment,
 *   and financial stability. The constraint is claimed as a coordination
 *   mechanism (rope/tangled_rope framing) but operates with substantial
 *   asymmetric extraction — the industrial-export complex and local officials
 *   capture the gains while households and small firms bear the costs. Active
 *   enforcement is required: statistical management, credit direction, land
 *   allocation, and suppression of rebalancing alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.72).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Legitimacy Grounded in GDP Growth Rate Maintenance").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '8fc66faa-bdfe-4966-980c-beb879b21873').
narrative_ontology:cs_kernel_codification('8fc66faa-bdfe-4966-980c-beb879b21873', formalized).
narrative_ontology:cs_authority_grounding('8fc66faa-bdfe-4966-980c-beb879b21873', extraction).
narrative_ontology:cs_interpretation_layer_present('8fc66faa-bdfe-4966-980c-beb879b21873').
narrative_ontology:cs_reading_relation('8fc66faa-bdfe-4966-980c-beb879b21873', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('8fc66faa-bdfe-4966-980c-beb879b21873', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fc66faa-bdfe-4966-980c-beb879b21873', performance_legitimacy__livelihood_security_reading, forecloses).
narrative_ontology:cs_axiom('8fc66faa-bdfe-4966-980c-beb879b21873', foundational, gdp_growth_equals_system_legitimacy).
narrative_ontology:cs_axiom_status(gdp_growth_equals_system_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8fc66faa-bdfe-4966-980c-beb879b21873', gdp_growth_equals_system_legitimacy, conventional).
narrative_ontology:cs_axiom('8fc66faa-bdfe-4966-980c-beb879b21873', foundational, investment_driven_model_necessary_for_stability).
narrative_ontology:cs_axiom_status(investment_driven_model_necessary_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('8fc66faa-bdfe-4966-980c-beb879b21873', investment_driven_model_necessary_for_stability, instrumental).
narrative_ontology:cs_reference_frame('8fc66faa-bdfe-4966-980c-beb879b21873', planned_economy_breakthrough).
narrative_ontology:cs_drift_state('8fc66faa-bdfe-4966-980c-beb879b21873', post_2008_stimulus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fc66faa-bdfe-4966-980c-beb879b21873', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_banks_financial_sector).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, households_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_capacity).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, financial_stability).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_private_firms).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, growth_rate_as_legitimacy_metric).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, investment_driven_model_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State-owned and export-oriented manufacturers receive directed credit, land, and policy support to hit output targets. Their profits and expansion depend on the growth mandate; exiting would mean losing privileged access to cheap capital and protected markets. They capture the bulk of investment flows but bear little of the consumption suppression cost.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, constrained, national).

% Career advancement is explicitly tied to hitting GDP growth targets. Officials both administer the constraint (setting local targets, approving projects, suppressing dissent) and benefit from it (promotion, fiscal resources, patronage networks). Their professional identity is fused with the growth model; exit means abandoning the only career logic they know.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary).

% Design the Five-Year Plans and aggregate targets that translate growth mandates into sectoral allocations. They control the statistical apparatus (NBS) that validates success. They can pivot the model (as seen in 'new normal' rhetoric) but face structural pressure from the beneficiaries they created. Their exit options include international positions and academic roles.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planners, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the cost of financial repression (low deposit rates, suppressed wage share), environmental degradation, and consumption deferral. They receive employment and rising absolute wages but at a declining share of GDP. Exit is constrained by hukou system, lack of social safety net, and limited voice in allocation decisions. Migrant workers face the sharpest extraction with the weakest protections.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, households_workers, payer,
    organized, biographical, constrained, national).

% Intermediate the directed credit that fuels investment-driven growth. They earn spread on policy lending, gain implicit guarantees, and expand balance sheets. They bear rising NPL risk from overcapacity sectors but are recapitalized by the state. Their business model is structurally dependent on the investment mandate; market-oriented reform would shrink their protected franchise.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_banks_financial_sector, beneficiary,
    organized, biographical, constrained, national).

% Crowded out of credit and land by SOE investment priority. Face higher borrowing costs, regulatory discrimination, and market access barriers. They generate disproportionate employment and innovation but are treated as residual claimants. Exit means moving to services, exiting to grey economy, or seeking foreign listing — all constrained by capital controls and regulatory approval.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_private_firms, payer,
    moderate, biographical, constrained, national).

% Absorb the export surplus generated by the growth model. Face trade friction, overcapacity spillovers, and forced technology transfer. Their structural exclusion from the domestic allocation logic means they cannot influence the constraint but must react to its output. Trapped by market size — they cannot afford to lose access but gain diminishing terms.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, foreign_trading_partners, excluded,
    powerful, biographical, trapped, global).

% International economists, development agencies, and domestic reform economists who track the model's trajectory. They see the full structure — the coordination achievements (infrastructure, poverty reduction) and the extraction dynamics (inequality, debt, overcapacity). Their exit is analytical: they can change frameworks but not the constraint itself.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates massive cross-sectoral investment allocation, regional competition for capital, employment targets, and industrial policy direction toward a single measurable aggregate (GDP growth rate) that aligns local officials, SOEs, and banks without requiring constant central negotiation.
% TRANSFER_FUNCTION: Transfers resources from household consumption (via financial repression suppressing deposit rates and wage share), environmental capacity (pollution sinks, resource depletion), and financial stability (rising leverage, misallocated credit) to industrial investment, export capacity, and local government fiscal revenue.
% ABSENT_VOICES: Rural populations bearing land expropriation and pollution without compensation; migrant workers excluded from urban social services despite generating export output; future generations inheriting debt overhang and ecological damage; small private firms crowded out of capital allocation; consumer advocacy voices absent from planning process.
% DISAPPEARANCE_RATIONALE: If the GDP growth mandate vanished overnight, the entire investment-driven allocation system would lose its coordinating logic: local officials would lose promotion criteria, SOEs would lose directed credit rationale, banks would lose policy lending mandate, and the statistical apparatus would lose its validation target. The economy would need to reorganize around consumption, services, and market allocation — a fundamental regime transition.
% FOUNDING_PROBLEM: Post-1978 reform era need to demonstrate system superiority through measurable economic expansion and job creation after Maoist ideological legitimacy collapsed. The growth target provided a transparent, non-ideological metric that could coordinate decentralized experimentation while maintaining central political control.
% FOUNDING_PROBLEM_CORROBORATION: World Bank (1997, 2013) and IMF Article IV reports document the model's success in poverty reduction but warn of diminishing returns and structural imbalances. Domestic reform economists (e.g., Lin Yifu, Justin Yifu Lin; Zhang Weiying) attest the founding problem (basic development) is largely solved but the arrangement persists. Beneficiaries (NDRC, local officials, SOE leaders) claim the model remains necessary for stability and 'high-quality development' transition.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the growth mandate structurally transfers resources from consumption to investment via financial repression, wage suppression, and directed credit. Suppression (0.68) is substantial because alternatives (consumption-led rebalancing, service sector liberalization, financial market opening) are actively blocked by the beneficiaries who control allocation. Theater ratio (0.45) is moderate and rising: the growth target performs real coordination (infrastructure, urbanization, poverty reduction) but an increasing share of enforcement activity defends the extraction structure (overcapacity protection, statistic smoothing, local debt rollover) rather than the coordination function. Accessibility collapse (0.58) reflects that alternatives exist technically but are politically inaccessible. Resistance (0.42) is moderate: labor protests, environmental activism, and reform advocacy exist but are fragmented and managed.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute strong seat divergence: from the local official/SOE seat this reads as genuine coordination (they built the infrastructure, created the jobs, hit the targets); from the household/small firm seat it reads as enforced extraction (they pay the repressed rates, breathe the pollution, lose the market access). The central planner seat experiences the constraint as a manageable but increasingly brittle coordination tool. The foreign partner seat experiences it as an externalized cost structure they cannot influence. The analytical seat sees the tension between the coordination achievement and the extraction accumulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Industrial-export complex and state banks are structural beneficiaries (d near 0.0-0.2): they collect directed credit, protected markets, and implicit guarantees. Local officials are agenda_setters who also benefit (d ~0.15): they administer the constraint and capture career/fiscal rewards. Central planners are agenda_setters with arbitrage exit (d ~0.2): they set targets but can pivot rhetoric. Households/workers and small firms are payers (d ~0.7-0.8): they bear financial repression, environmental costs, and credit discrimination with constrained exit. Foreign partners are excluded (trapped): they absorb export surplus but cannot influence the allocation logic. Analytical observers sit at d=0.5 (symmetric): they see the full structure but bear no direct cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (demonstrating system viability through growth after ideological collapse) was substantially solved by ~2010 (per capita GDP crossing middle-income threshold, poverty elimination). The arrangement persists because the beneficiaries it created (local officials, SOEs, banks) now control the allocation machinery and suppress rebalancing. This is a classic mandatrophy case: the coordination function (growth as legitimacy metric) has atrophied into an extraction mechanism that the beneficiaries defend. The 'new normal' and 'high-quality development' rhetoric represents the theatrical maintenance of a constraint whose original justification has expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the GDP growth mandate a genuine coordination necessity for a developing economy, or an extractive arrangement that benefits identifiable agents at the expense of households and the environment?',
    'Counterfactual analysis: if growth targets were replaced with employment, wage share, and sustainability metrics, would coordination collapse or would a more efficient allocation emerge? Compare with East Asian peers (Japan, Korea, Taiwan) that transitioned at similar income levels.',
    'If coordination necessity, the constraint is a genuine tangled_rope with irreducible extraction as coordination cost. If extractive arrangement, it is a snare disguised as coordination, and the beneficiaries are rent-seekers defending captured rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the growth mandate''s coordination function is structurally necessary or a cover for extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of rebalancing alternatives structural (policy barriers, credit allocation, statistical control) or internalized (officials genuinely believe GDP growth equals legitimacy, households accept consumption deferral as patriotic duty)?',
    'Post-policy-shift observation: if growth targets were relaxed, would local officials spontaneously rebalance toward services/consumption, or would they resist because their identity/promotion logic is fused with GDP? Survey data on official and public belief systems.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists even without active enforcement because agents carry the suppression with them. This would push classification toward piton/snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the growth mandate.').

omega_variable(
    growth_quality_vs_quantity,
    'Does the measured GDP growth actually deliver the claimed legitimacy benefits (job creation, stability, system credibility), or has the growth-quality decoupling made the metric a poor proxy for the founding problem?',
    'Decompose GDP growth into employment elasticity, wage growth, productivity gains, and distributional outcomes. Track legitimacy indicators (trust surveys, protest frequency, emigration rates) against growth composition.',
    'If growth quality has decoupled from legitimacy production, the constraint''s coordination function is degrading — it coordinates the wrong thing. This would support reclassification toward piton (degraded coordination) or snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_quality_vs_quantity, empirical, 'Whether GDP growth remains a valid proxy for the legitimacy it claims to produce.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_leg_qg_tr_t1978, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(perf_leg_qg_tr_t1992, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(perf_leg_qg_tr_t2001, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(perf_leg_qg_tr_t2008, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(perf_leg_qg_tr_t2015, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(perf_leg_qg_tr_t2024, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_leg_qg_be_t1978, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1978, 0.25).
narrative_ontology:measurement(perf_leg_qg_be_t1992, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement(perf_leg_qg_be_t2001, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(perf_leg_qg_be_t2008, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement(perf_leg_qg_be_t2015, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(perf_leg_qg_be_t2024, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perf_leg_qg_su_t1978, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(perf_leg_qg_su_t1992, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement(perf_leg_qg_su_t2001, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(perf_leg_qg_su_t2008, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement(perf_leg_qg_su_t2015, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(perf_leg_qg_su_t2024, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, local_government_debt_overhang).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, soe_overcapacity_persistence).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, financial_repression_household_transfer).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the performance_legitimacy kernel. The quantitative_growth_reading prioritizes aggregate growth rate as the legitimacy metric; qualitative_development_reading prioritizes structural transformation; techno_nationalist_reading prioritizes strategic industry dominance; livelihood_security_reading prioritizes direct welfare delivery. They share the kernel (performance legitimacy) but instantiate different constraints with different ε, beneficiaries, and extraction structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, institutional, 0.15).
constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, powerful, 0.18).
constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, organized, 0.72).
constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
