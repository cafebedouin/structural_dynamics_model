% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy via GDP Growth Rate Maintenance
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This story instantiates the quantitative-growth reading of a broader
 *   performance-legitimacy kernel: the claim that a governing authority's
 *   legitimacy rests on demonstrating continued GDP growth and job creation.
 *   Under this reading, the growth rate itself becomes the primary constraint
 *   local officials and industrial policy are optimized against, sustaining
 *   an investment-driven model in which export dependency and industrial
 *   overcapacity are tolerated as acceptable costs of hitting the number. The
 *   beneficiaries are the industrial-export complex, which receives
 *   subsidized capital and land, and local officials whose careers are
 *   measured on GDP performance. This is a distinct constraint from the
 *   sibling readings of the same kernel (qualitative development,
 *   techno-nationalist self-sufficiency, livelihood security) — each of those
 *   has a different beneficiary structure, a different transfer function, and
 *   a different epsilon, and each is authored as its own separate constraint
 *   story per the epsilon-invariance principle. Only this reading's own
 *   arrangement (the growth-target regime as currently practiced) is assessed
 *   here; the ε value is not averaged across readings and does not describe
 *   the reading's own endorsed alternative.
 *
 * KEY AGENTS:
 *   - industrial_export_complex: organized beneficiary with arbitrage-grade exit via capacity relocation and subsidized financing access
 *   - gdp_measured_local_officials: institutional agenda-setter constrained by career-evaluation criteria they cannot individually alter
 *   - overcapacity_sector_workers: powerless payer, trapped by sector-specific skills and regional labor markets
 *   - household_consumption_base: moderate-power payer whose consumption share is structurally suppressed by investment-favoring channels
 *   - central_planning_authority: institutional observer/agenda-setter with the formal power to redefine the metric but political incentive not to fully do so
 *   - foreign_trading_partners: excluded powerful actors who absorb externalized overcapacity through trade exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.6).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy via GDP Growth Rate Maintenance").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '8025178d-be0f-491a-a018-14858206e51e').
narrative_ontology:cs_kernel_codification('8025178d-be0f-491a-a018-14858206e51e', distributed).
narrative_ontology:cs_authority_grounding('8025178d-be0f-491a-a018-14858206e51e', extraction).
narrative_ontology:cs_interpretation_layer_present('8025178d-be0f-491a-a018-14858206e51e').
narrative_ontology:cs_reading_relation('8025178d-be0f-491a-a018-14858206e51e', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('8025178d-be0f-491a-a018-14858206e51e', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8025178d-be0f-491a-a018-14858206e51e', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('8025178d-be0f-491a-a018-14858206e51e', foundational, growth_rate_is_sufficient_legitimacy_proxy).
narrative_ontology:cs_axiom_status(growth_rate_is_sufficient_legitimacy_proxy, holdable).
narrative_ontology:cs_axiom_grounding('8025178d-be0f-491a-a018-14858206e51e', growth_rate_is_sufficient_legitimacy_proxy, instrumental).
narrative_ontology:cs_axiom('8025178d-be0f-491a-a018-14858206e51e', secondary, overcapacity_and_export_dependency_are_acceptable_transition_costs).
narrative_ontology:cs_axiom_status(overcapacity_and_export_dependency_are_acceptable_transition_costs, holdable).
narrative_ontology:cs_axiom_grounding('8025178d-be0f-491a-a018-14858206e51e', overcapacity_and_export_dependency_are_acceptable_transition_costs, empirically_contingent).
narrative_ontology:cs_reference_frame('8025178d-be0f-491a-a018-14858206e51e', reform_era_capital_scarcity_growth_mandate).
narrative_ontology:cs_drift_state('8025178d-be0f-491a-a018-14858206e51e', post_overcapacity_debt_recognition_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8025178d-be0f-491a-a018-14858206e51e', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_owned_construction_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, export_oriented_manufacturers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, overcapacity_sector_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_consumption_base).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, local_government_debt_holders).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmentally_exposed_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large exporters and manufacturers benefit from a policy regime that channels investment, subsidized credit, and land toward production capacity because growth targets reward output volume over profitability. They can relocate capacity, access preferential financing, and lobby for continued investment-driven stimulus whenever growth softens.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    organized, generational, arbitrage, national).

% Local and provincial officials are promoted or penalized substantially on regional GDP growth figures. They approve infrastructure projects, subsidize favored industries, and tolerate overcapacity and debt accumulation because their career horizon rewards near-term growth numbers over long-term balance-sheet health. Their exit from the growth-target regime would require the central authority to change the evaluation criteria they are individually powerless to alter.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, beneficiary).

% Workers in steel, cement, solar panel, and construction-adjacent industries built up by investment-driven growth targets face employment volatility when overcapacity forces consolidation or export dumping triggers foreign tariffs. They bear layoffs and wage stagnation as the visible cost of maintaining aggregate growth figures, with limited ability to relocate to sectors the growth model does not prioritize.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, overcapacity_sector_workers, payer,
    powerless, biographical, trapped, regional).

% Ordinary households see a persistently low consumption share of GDP because investment and export channels are favored by the growth-target regime over wage growth and social transfers. They pay through suppressed real income growth, thin social safety nets, and elevated household savings rates driven by insecurity the growth model does not address; switching to a consumption-led household strategy is constrained by the same institutions that set investment priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_consumption_base, payer,
    moderate, generational, constrained, national).

% Local government financing vehicles and their creditors accumulate debt from growth-target-driven infrastructure and industrial-park spending that outpaces genuine demand. When growth slows, this debt becomes a fiscal drag that local governments cannot easily restructure, and it is often quietly rolled over rather than resolved, since acknowledging the debt would undercut the growth narrative that justified it.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_debt_holders, payer,
    moderate, generational, trapped, regional).

% Communities near heavy-industry export zones absorb pollution, land conversion, and resource depletion that accompany investment-driven growth. Environmental review processes are frequently expedited or waived to hit growth and job-creation targets, leaving these communities with degraded conditions and no meaningful voice in the investment approval process.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmentally_exposed_communities, payer,
    powerless, generational, trapped, regional).

% Sets the growth-rate targets and the evaluation criteria that local officials respond to, while also periodically warning against overcapacity and debt risk. Holds the power to redefine the legitimacy metric away from raw GDP growth but has not fully done so, since the growth reading remains a politically legible proof of continued success to both domestic and international audiences.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planning_authority, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, central_planning_authority, agenda_setter).

% Trading partner economies absorb the export overcapacity generated by the growth-target regime through import surges and price competition in steel, EVs, solar panels, and other sectors. They are not party to the domestic legitimacy calculation but bear its externalized costs through industrial dislocation and are increasingly responding with tariffs and trade barriers — a pressure the domestic reading does not internalize.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, foreign_trading_partners, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible, internationally comparable metric (GDP growth) that coordinates investment planning, local government incentive structures, and claims of national economic success around one number, avoiding the ambiguity of multi-dimensional welfare metrics.
% TRANSFER_FUNCTION: Moves capital, land, and subsidized credit toward export-oriented and investment-heavy sectors and the officials who administer them, and moves the resulting costs — overcapacity, household consumption suppression, local debt, and environmental externality — onto workers, households, local creditors, and exposed communities.
% ABSENT_VOICES: Foreign trading partners absorbing overcapacity exports have no formal voice in the domestic growth-target-setting process; environmentally exposed communities and workers in overcapacity sectors are structurally outside the evaluation criteria that reward officials for growth numbers rather than for the distributional or environmental consequences of how that growth is produced.
% DISAPPEARANCE_RATIONALE: If GDP growth rate were abandoned overnight as the legitimacy metric, local officials would lose their primary promotion criterion, investment-driven stimulus toward favored industrial sectors would lose its justification, overcapacity-dependent firms would face immediate restructuring pressure, and local government financing vehicles built on growth-justified debt would face a reckoning that the current arrangement currently defers.
% FOUNDING_PROBLEM: In the reform era, raw GDP growth was adopted as a legible, verifiable proxy for successful economic transformation and poverty reduction at a time when the country was genuinely capital-starved and underemployed, making investment-led growth a reasonably targeted response to real scarcity.
% FOUNDING_PROBLEM_CORROBORATION: Central planning authority documents (five-year plans, state media commentary on 'high-quality development') themselves acknowledge that the founding problem of capital scarcity has been substantially solved and that growth-rate fixation now produces overcapacity and debt risk; independent international financial institutions and foreign trading partners' trade-remedy filings corroborate that export overcapacity driven by growth targets is a live externality, while the industrial-export complex and GDP-measured local officials continue to assert the growth problem remains live and requires continued investment support.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.42 to 0.68) reflecting the accumulation of overcapacity, local government debt, and consumption suppression as the growth-target regime persists past the point where capital scarcity (the founding problem) was resolved. Theater ratio rises in parallel (0.20 to 0.45) as growth figures increasingly reflect investment volume and infrastructure activity rather than underlying demand or productivity gains — a Goodhart-style substitution of the proxy (GDP growth) for the target (broad-based welfare improvement). Suppression requirement rises moderately (0.35 to 0.60) as maintaining the growth narrative requires more active management: debt rollovers, expedited environmental approvals, and continued investment stimulus despite diminishing returns. All three metrics share one time grid across the same seven points.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of gdp_measured_local_officials, the arrangement looks like functional coordination: a clear, comparable metric that lets the central authority allocate resources and evaluate performance efficiently. From the seat of overcapacity_sector_workers or environmentally_exposed_communities, the same arrangement operates as an enforced extraction mechanism whose primary function is preserving the appearance of continued success for those who administer it, at the cost of bearing the volatility, debt, and environmental externalities the growth-target regime displaces onto them. The engine computes these divergent seat-classifications from the structural power/exit data; this story does not adjudicate which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   The industrial-export complex and GDP-measured local officials sit near the beneficiary end of directionality: they collect subsidized credit, land, promotion, and market access respectively, with meaningful exit or discretion over how the arrangement operates. Overcapacity workers, household consumers, local debt holders, and environmentally exposed communities sit near the target end: they bear the costs (unemployment risk, suppressed consumption, fiscal exposure, pollution) with constrained or trapped exit, unable to individually opt out of a nationally administered growth-target regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine capital scarcity requiring investment-led growth — was real at the reform era's outset but is substantially resolved today, as evidenced by the central planning authority's own periodic overcapacity and debt warnings. The quantitative-growth reading's persistence past this point is a mandatrophy candidate: the mandate (grow to overcome scarcity) has outlived its founding condition, but the metric persists because it remains legible and politically convenient, and because local officials' career incentives are locked to it. Classifying this as tangled_rope (rather than snare) preserves the genuine coordination function the metric once served and may still partially serve (investment coordination, international legibility) while registering the asymmetric extraction now layered onto it — a pure snare classification would understate the real coordination history; a pure rope classification would ignore the accumulating victim class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_reading_reversibility,
    'Is the quantitative-growth reading a genuinely load-bearing coordination mechanism still solving a real scarcity problem, or has it become a legacy metric maintained mainly because switching to a different legitimacy reading (qualitative development, livelihood security) would require politically costly acknowledgment that past growth-driven investment was partly misallocated?',
    'Track whether central planning authority documents and resource allocation formulas shift weighting away from raw growth rate toward the qualitative-development or livelihood-security metrics over successive planning cycles, and whether local official evaluation criteria are formally revised.',
    'If the metric is being actively displaced by sibling readings, this constraint is transitioning toward scaffold (a temporary arrangement being wound down); if it persists unchanged despite acknowledged overcapacity costs, the tangled_rope-to-piton trajectory (extraction persisting on inertia rather than active coordination benefit) becomes more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_reading_reversibility, empirical, 'Whether the growth-rate reading is being actively superseded or is persisting on institutional inertia.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the contest between this reading and its siblings (qualitative_development_reading, techno_nationalist_reading, livelihood_security_reading) get resolved — is it a genuine multi-metric weighting decided at the central planning level, or does the quantitative-growth reading retain de facto primacy because it alone offers a single, comparably measurable number for evaluating local officials?',
    'Examine whether local government promotion and evaluation systems have been reweighted to incorporate qualitative-development or livelihood-security indicators with real consequences, versus growth rate remaining the tie-breaking or default criterion when metrics conflict.',
    'If growth rate remains the practical tie-breaker beneath nominal multi-metric evaluation, this reading''s beneficiary structure (industrial-export complex, GDP-measured officials) retains its extractive leverage regardless of official rhetoric shift toward the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the quantitative-growth reading retains practical primacy beneath official multi-metric framing.').

omega_variable(
    overcapacity_externalization_measurement,
    'How much of the reading''s tolerated overcapacity is actually absorbed domestically (via local demand, state stockpiling, write-offs) versus externalized onto foreign trading partners via export dumping, and how does that split affect the true extractiveness borne domestically versus internationally?',
    'Trade data on export volume growth in overcapacity sectors relative to domestic consumption growth, cross-referenced with anti-dumping and countervailing duty filings from trading partners over the interval.',
    'A larger externalized share would mean domestic extractiveness is understated relative to the true cost of the growth-target regime, with foreign trading partners functioning as an additional uncompensated payer class not fully captured in the current victim list.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overcapacity_externalization_measurement, empirical, 'The degree to which overcapacity costs are exported rather than absorbed domestically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__quantitative_growth_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__quantitative_growth_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__quantitative_growth_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy_qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy_techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy_livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the performance_legitimacy kernel, each authored as a separate constraint story with its own ε and beneficiary/victim structure per the ε-invariance principle. The quantitative_growth_reading (this file) treats GDP growth rate as the primary legitimacy metric, sustaining an investment-driven, export-tolerant model. The qualitative_development_reading treats structural transformation (innovation, efficiency, sustainability) as primary and would show a different beneficiary structure (innovation-sector firms, environmental regulators) and likely lower extractiveness toward overcapacity-exposed workers. The techno_nationalist_reading treats strategic-industry self-sufficiency as primary, benefiting different firms (national champions in semiconductors, defense-adjacent sectors) under different suppression dynamics (export controls, technology denial responses). The livelihood_security_reading treats direct welfare delivery as primary, with beneficiaries being citizens receiving healthcare/education/pension improvements and a correspondingly different, likely lower extractiveness profile. These four readings compete for practical primacy within the same institutional apparatus; the influences edges below capture how growth-rate persistence structurally constrains resource availability for the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
