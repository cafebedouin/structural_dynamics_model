% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   This story reads performance legitimacy through its quantitative-growth
 *   instantiation: the state's claim to legitimacy rests on demonstrating
 *   continued GDP growth and job-creation figures, largely through an
 *   investment-driven, export-oriented model. Under this reading, the
 *   headline growth rate itself becomes the binding constraint on policy,
 *   tolerating export dependency, industrial overcapacity, and rising local
 *   government debt as the acceptable costs of maintaining the number. This
 *   is a distinct constraint from the qualitative-development reading (which
 *   treats efficiency and innovation gains as the legitimacy anchor and would
 *   tolerate a lower headline rate), the techno-nationalist reading (which
 *   anchors legitimacy in strategic self-sufficiency regardless of GDP
 *   contribution), and the livelihood-security reading (which anchors
 *   legitimacy in directly experienced welfare improvements, tolerating
 *   slower growth for better distribution). Each reading has a different
 *   beneficiary structure and a different ε; they are linked as siblings
 *   under the shared kernel, not merged into one constraint.
 *
 * KEY AGENTS:
 *   - industrial_export_complex: primary beneficiary (organized/arbitrage) — captures subsidized inputs and continued output demand
 *   - gdp_measured_local_officials: agenda-setter and secondary beneficiary (institutional/constrained) — career advancement tied to the metric
 *   - overcapacity_sector_workers: primary target (powerless/trapped) — absorbs cyclical layoffs and wage suppression
 *   - provincial_debt_burdened_residents: target (powerless/trapped) — bears local government debt service costs
 *   - central_planning_authority: agenda-setter (institutional/analytical) — sets and could revise the evaluation criteria
 *   - independent_economists: excluded analytical voice — documents the costs from outside the reporting chain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.61).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy via GDP Growth Rate Maintenance").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '30193b46-7b08-4ccf-8f64-827f27d81857').
narrative_ontology:cs_kernel_codification('30193b46-7b08-4ccf-8f64-827f27d81857', distributed).
narrative_ontology:cs_authority_grounding('30193b46-7b08-4ccf-8f64-827f27d81857', extraction).
narrative_ontology:cs_interpretation_layer_present('30193b46-7b08-4ccf-8f64-827f27d81857').
narrative_ontology:cs_reading_relation('30193b46-7b08-4ccf-8f64-827f27d81857', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('30193b46-7b08-4ccf-8f64-827f27d81857', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('30193b46-7b08-4ccf-8f64-827f27d81857', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('30193b46-7b08-4ccf-8f64-827f27d81857', foundational, headline_growth_rate_is_primary_legitimacy_signal).
narrative_ontology:cs_axiom_status(headline_growth_rate_is_primary_legitimacy_signal, holdable).
narrative_ontology:cs_axiom_grounding('30193b46-7b08-4ccf-8f64-827f27d81857', headline_growth_rate_is_primary_legitimacy_signal, instrumental).
narrative_ontology:cs_axiom('30193b46-7b08-4ccf-8f64-827f27d81857', secondary, investment_volume_tolerable_despite_diminishing_returns).
narrative_ontology:cs_axiom_status(investment_volume_tolerable_despite_diminishing_returns, holdable).
narrative_ontology:cs_axiom_grounding('30193b46-7b08-4ccf-8f64-827f27d81857', investment_volume_tolerable_despite_diminishing_returns, empirically_contingent).
narrative_ontology:cs_reference_frame('30193b46-7b08-4ccf-8f64-827f27d81857', reform_era_high_growth_consensus).
narrative_ontology:cs_drift_state('30193b46-7b08-4ccf-8f64-827f27d81857', post_overcapacity_debt_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('30193b46-7b08-4ccf-8f64-827f27d81857', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_owned_construction_conglomerates).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, export_oriented_manufacturers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, overcapacity_sector_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, provincial_debt_burdened_residents).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, trading_partner_domestic_industries).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_commons_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large manufacturing and export-oriented firms receive subsidized credit, land, and energy priced to sustain output volumes that feed the growth number. They can relocate capacity, shift into new subsidized sectors, or lobby for continued support; their financial health tracks whatever the growth target currently favors, not underlying demand.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    organized, biographical, arbitrage, national).

% Provincial and municipal officials are evaluated for promotion primarily on local GDP and investment figures. They approve infrastructure and industrial projects regardless of demand signals because the metric, not the underlying return, determines career advancement. Their exit from this incentive structure would require the central government to change the evaluation criteria, which they do not control.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, beneficiary).

% State-linked builders and infrastructure firms receive a continuous pipeline of investment-driven projects justified by growth targets, independent of whether the resulting capacity is used. They benefit from volume regardless of utilization and face little downside if projects underperform.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_owned_construction_conglomerates, beneficiary,
    institutional, generational, arbitrage, national).

% Workers in steel, solar panel, EV, and construction-materials sectors experience cyclical layoffs and wage suppression when overcapacity forces price wars or plant closures. They lack the capital or mobility to shift into other sectors quickly, and social insurance is thin, so the boom-bust cycle of investment-driven growth lands directly on their household stability.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, overcapacity_sector_workers, payer,
    powerless, biographical, trapped, regional).

% Residents of localities that borrowed heavily through local government financing vehicles to hit growth and investment targets now live with underused infrastructure and constrained local public services as debt service consumes municipal budgets. They did not choose the borrowing and cannot easily relocate or vote out the officials who approved it.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, provincial_debt_burdened_residents, payer,
    powerless, generational, trapped, regional).

% Manufacturers in importing countries face price competition from subsidized export volumes that exist to satisfy the growth target rather than global demand. They can lobby for tariffs or anti-dumping measures, but the underlying overcapacity persists as long as the domestic legitimacy structure rewards output over profitability.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, trading_partner_domestic_industries, payer,
    moderate, biographical, constrained, global).

% Communities near heavy-industry export zones absorb pollution, water stress, and land conversion driven by continuous investment-intensive production. Environmental review is frequently subordinated to hitting growth and employment figures, and residents have limited recourse to slow or redirect projects.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_commons_communities, payer,
    powerless, generational, trapped, regional).

% Sets the headline growth target and the incentive architecture that ties local promotion to GDP performance. Can in principle change the evaluation criteria toward qualitative or livelihood metrics but has historically renewed the growth-rate anchor as the primary legitimacy signal, particularly during periods of external pressure or slowdown risk.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Domestic and international economists documenting overcapacity, hidden local government debt, and diminishing marginal returns on investment are marginalized in official data reporting and policy discourse. Their analyses circulate outside the official statistical apparatus and rarely alter the target-setting process.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, independent_economists, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible, comparable performance metric that lets a large multi-level bureaucracy allocate promotions, credit, and investment priority without requiring qualitative judgment calls that would be far harder to adjudicate and audit across thousands of jurisdictions.
% TRANSFER_FUNCTION: Moves capital, land, and credit toward output-maximizing industrial and construction activity and toward officials who can demonstrate growth, while moving the costs of resulting overcapacity, debt service, environmental degradation, and cyclical unemployment onto workers, residents, and downstream trading partners.
% ABSENT_VOICES: Independent economists and statisticians who document overcapacity and hidden debt are structurally outside the reporting chain that sets and rewards the growth number; overcapacity-sector workers and debt-burdened residents have no formal channel to contest the target itself, only its local implementation.
% DISAPPEARANCE_RATIONALE: If GDP growth-rate maintenance stopped functioning as the legitimacy anchor overnight, the incentive structure driving local officials to approve investment regardless of demand would collapse, credit allocation to overcapacity sectors would tighten sharply, and the industrial-export complex's privileged access to subsidized inputs would be renegotiated — a substantial share of current investment activity exists because of the metric, not independent of it.
% FOUNDING_PROBLEM: In earlier decades, sustained high growth was needed to absorb a large labor force moving out of agriculture, reduce poverty at scale, and establish the material basis for the government's post-revolutionary and post-reform legitimacy claims.
% FOUNDING_PROBLEM_CORROBORATION: The industrial-export complex and provincial officials attest the growth-rate anchor remains necessary to sustain employment and investment momentum. Independent economists, international financial institutions, and increasingly some central-government policy documents attest the labor-absorption problem the target was built to solve has substantially eased, while the target itself now drives overcapacity and debt accumulation that outstrip its original justification — corroboration from outside the beneficiary set points toward a shifted, partly obsolete function.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.68 and rising because the growth-rate anchor increasingly drives investment past the point of productive return — capital is directed to maintain the headline figure rather than to meet demonstrated demand, and the resulting overcapacity, debt, and environmental costs are borne by parties who did not choose the target. Suppression is moderate-to-high (0.61) because dissenting data (independent overcapacity and debt analyses) is structurally marginalized within the official reporting apparatus, and local officials face little room to deviate from the metric without career risk. Theater ratio (0.47) reflects a growing share of investment activity whose primary function is generating a reportable growth number rather than productive capacity — visible in ghost infrastructure and underutilized industrial parks. All three series share one time grid from t=0 to t=24 with values authored at every point.
 *
 * DIRECTIONALITY LOGIC:
 *   The industrial-export complex, state-owned conglomerates, and GDP-measured officials sit near the beneficiary end of directionality: they collect subsidized inputs, career advancement, or investment volume directly from the arrangement, and several (export firms, conglomerates) have arbitrage-grade exit into new subsidized sectors. Overcapacity-sector workers, debt-burdened residents, and environmental-commons communities sit near the full-target end: trapped exit options, no capacity to renegotiate the metric that produces their costs, and generational time horizons for debt and environmental harm. Trading-partner domestic industries are a moderate-power external target — able to lobby for tariffs but unable to alter the underlying domestic incentive structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — absorbing a massive rural-to-urban labor transition and establishing baseline material legitimacy — was genuinely live in earlier decades and the growth-rate target was a reasonably calibrated response to it. The founding_problem_status is authored as contested rather than dead because officials and export-sector beneficiaries continue to assert its necessity, while independent economists and increasingly parts of the central planning apparatus itself acknowledge the labor-absorption problem has substantially eased and the target now functions primarily to sustain investment volume and career incentives disconnected from demonstrated demand. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a legible, comparable metric across a vast bureaucracy) while registering the asymmetric extraction that has grown alongside it — collapsing this into pure extraction would erase the real administrative coordination problem the metric solves; treating it as pure coordination would erase the debt, overcapacity, and environmental costs that fall on parties with no voice in setting the target.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantitative_reading_kernel_position,
    'This constraint instantiates the quantitative_growth_reading of the performance_legitimacy kernel — legitimacy grounded in maintaining headline GDP growth and job-creation figures via an investment-driven, export-oriented model. Sibling readings (qualitative_development_reading, techno_nationalist_reading, livelihood_security_reading) are separate constraints with different beneficiary structures and different ε values, linked via network.affects_constraints, not merged here.',
    'Compare ε and stakeholder sets across the four sibling constraint files; track which reading dominates actual policy allocation in a given period via budget and credit-allocation data.',
    'If the quantitative_growth_reading''s dominance in credit allocation and promotion criteria weakens relative to siblings (e.g., qualitative_development_reading gaining institutional weight), this constraint''s extractiveness and enforcement intensity should be expected to decline as investment shifts toward efficiency-weighted criteria.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantitative_reading_kernel_position, conceptual, 'This story is one reading of the performance_legitimacy kernel; sibling readings are separate constraints.').

omega_variable(
    growth_target_natural_vs_constructed,
    'Is the growth-rate anchor a structurally necessary macroeconomic stabilizer (given employment and social-stability requirements at this scale), or a constructed metric whose primary current function is sustaining the political position of the industrial-export complex and GDP-measured officials?',
    'Examine whether growth-rate deviations below target are met with proportionate stimulus regardless of demonstrated demand (constructed-metric signature) versus genuinely calibrated to employment stress indicators (stabilizer signature).',
    'If the anchor is revealed as substantially decoupled from employment stress and driven by metric maintenance alone, the tangled_rope classification would shift toward snare as the coordination justification thins relative to the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_target_natural_vs_constructed, empirical, 'Whether the growth-rate target still tracks a genuine stabilization function or has become self-referential.').

omega_variable(
    local_debt_data_opacity,
    'What is the true scale of local government financing vehicle debt incurred to hit investment and growth targets, and how much of it is serviceable without further distortionary investment?',
    'Independent audit or disclosure-forcing mechanism (e.g., international financial institution review, off-balance-sheet debt reporting requirement) applied across provinces.',
    'A larger-than-reported debt overhang would strengthen the case that current extractiveness is understated and that the constraint is approaching a forced transition; a smaller overhang would support the official framing of manageable, growth-supporting investment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(local_debt_data_opacity, empirical, 'Scale of hidden local government debt incurred to sustain reported growth figures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__quantitative_growth_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__quantitative_growth_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__quantitative_growth_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.47).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the performance_legitimacy kernel. Each reading names a different primary legitimacy metric (headline growth rate here; efficiency/innovation gains for qualitative_development_reading; strategic self-sufficiency for techno_nationalist_reading; directly experienced welfare for livelihood_security_reading), producing different ε values, different beneficiary/victim structures, and different classifications. They are linked here rather than merged per the ε-invariance principle: measuring 'performance legitimacy' by growth rate versus by innovation intensity versus by welfare delivery yields structurally distinct constraints, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
