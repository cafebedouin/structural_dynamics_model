% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   Local and provincial officials are evaluated substantially on the growth
 *   rate their jurisdiction produces; national legitimacy claims are
 *   substantially anchored to the aggregate growth number holding above a
 *   politically meaningful threshold. This produces a self-reinforcing
 *   structure: officials with career incentives tied to the number favor
 *   debt-financed infrastructure and industrial capacity expansion because
 *   these move the growth number quickly and legibly, even when the resulting
 *   capacity exceeds demand and the debt exceeds fiscal capacity to service
 *   it. The industrial-export complex captures the policy support this
 *   generates; the costs land on overleveraged local governments,
 *   overcapacity-sector workers, the household consumption base, and
 *   (externally) trading partners absorbing subsidized export volume.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.61).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy via GDP Growth Rate Maintenance").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '7f27fed0-1dd8-450b-b6b6-bd73beea42db').
narrative_ontology:cs_kernel_codification('7f27fed0-1dd8-450b-b6b6-bd73beea42db', distributed).
narrative_ontology:cs_authority_grounding('7f27fed0-1dd8-450b-b6b6-bd73beea42db', practice).
narrative_ontology:cs_interpretation_layer_present('7f27fed0-1dd8-450b-b6b6-bd73beea42db').
narrative_ontology:cs_reading_relation('7f27fed0-1dd8-450b-b6b6-bd73beea42db', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f27fed0-1dd8-450b-b6b6-bd73beea42db', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('7f27fed0-1dd8-450b-b6b6-bd73beea42db', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('7f27fed0-1dd8-450b-b6b6-bd73beea42db', foundational, growth_rate_is_primary_legitimacy_indicator).
narrative_ontology:cs_axiom_status(growth_rate_is_primary_legitimacy_indicator, holdable).
narrative_ontology:cs_axiom_grounding('7f27fed0-1dd8-450b-b6b6-bd73beea42db', growth_rate_is_primary_legitimacy_indicator, conventional).
narrative_ontology:cs_axiom('7f27fed0-1dd8-450b-b6b6-bd73beea42db', secondary, investment_led_overcapacity_is_tolerable_transition_cost).
narrative_ontology:cs_axiom_status(investment_led_overcapacity_is_tolerable_transition_cost, holdable).
narrative_ontology:cs_axiom_grounding('7f27fed0-1dd8-450b-b6b6-bd73beea42db', investment_led_overcapacity_is_tolerable_transition_cost, instrumental).
narrative_ontology:cs_reference_frame('7f27fed0-1dd8-450b-b6b6-bd73beea42db', reform_era_growth_legitimation).
narrative_ontology:cs_drift_state('7f27fed0-1dd8-450b-b6b6-bd73beea42db', post_overcapacity_debt_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f27fed0-1dd8-450b-b6b6-bd73beea42db', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_owned_construction_conglomerates).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, export_dependent_manufacturing_regions).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, overleveraged_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, overcapacity_sector_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_consumption_base).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, trade_partner_domestic_industries).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_commons).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, investment_driven_growth_model_validity).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, export_led_development_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large manufacturing and export conglomerates receive subsidized credit, land, and energy tied to output and export volume targets. They benefit directly from growth-rate-linked policy support and can relocate or restructure across provinces or borders when local conditions sour, giving them far more mobility than the workers or governments whose fortunes are pinned to the same growth numbers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    organized, generational, arbitrage, global).

% Local and provincial officials are evaluated and promoted substantially on regional GDP growth and investment attraction. They set local incentive structures, approve land and credit allocations, and administer growth targets handed down from above. Their career trajectory depends on hitting the number, which pushes them toward debt-financed infrastructure and industrial capacity regardless of downstream demand.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, beneficiary).

% Municipal and provincial governments carry the debt incurred building the infrastructure and industrial capacity that produced the growth numbers. They cannot easily exit the fiscal arrangement — bond obligations, off-balance-sheet financing vehicles, and continued pressure to show growth in the next reporting cycle keep them recommitting to the same model even as debt service consumes rising shares of revenue.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, overleveraged_local_governments, payer,
    institutional, generational, trapped, regional).

% Workers in steel, cement, solar panel, and EV manufacturing sectors built up to satisfy investment-driven growth targets bear the cost when overcapacity triggers price wars, wage suppression, or plant closures. Their exit options are limited to internal migration toward other growth zones that are themselves subject to the same boom-bust cycle.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, overcapacity_sector_workers, payer,
    powerless, biographical, constrained, regional).

% Households receive a comparatively low share of national income under an investment- and export-weighted growth model; wages, social insurance, and consumption subsidies are systematically underweighted relative to industrial investment because household consumption does not move the headline growth number as directly or as fast as capital formation does.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_consumption_base, payer,
    powerless, biographical, trapped, national).

% Manufacturers in importing countries absorb the effects of subsidized export overcapacity — price undercutting, market share loss, plant closures — but have no seat in the domestic policy process that sets growth targets. Their only leverage is external: tariffs, anti-dumping suits, or trade litigation, none of which touches the underlying legitimacy arrangement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, trade_partner_domestic_industries, excluded,
    organized, biographical, constrained, global).

% Represents the non-agent ecological cost — emissions, resource depletion, land conversion — absorbed to sustain investment-heavy growth rates. Listed for completeness; carries no directionality weight of its own since it is not an actor.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, environmental_commons).

% Central economic planners set the growth-rate target that cascades into provincial incentive structures. They observe the accumulating debt and overcapacity, periodically announce rebalancing initiatives, but repeatedly recommit to growth-rate defense when the number threatens to fall, because abandoning the target risks the legitimacy claim itself.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planning_authorities, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A visible, comparable, internationally legible number — the GDP growth rate — lets a large and internally diverse state coordinate expectations among officials, investors, workers, and citizens about whether the economic project is succeeding, without requiring consensus on contested distributional or ideological questions.
% TRANSFER_FUNCTION: Moves capital, land, and cheap credit toward industrial and export capacity and toward officials whose careers are tied to the growth number, and moves the resulting debt burden, overcapacity risk, wage suppression, and environmental cost onto local governments, sector workers, households, and trading partners.
% ABSENT_VOICES: Trade-partner domestic industries absorbing the overcapacity have no seat in the domestic target-setting process. Household consumption interests are represented only weakly relative to investment interests in the incentive structure that rewards local officials for capital formation over consumption growth.
% DISAPPEARANCE_RATIONALE: If growth-rate-based legitimacy vanished overnight, local officials would lose the incentive structure driving debt-financed infrastructure and industrial overcapacity; capital allocation would likely shift toward consumption and services; overleveraged local governments would face immediate scrutiny over previously growth-justified debt; export-oriented conglomerates would lose a major source of policy support. The industrial-export complex and GDP-measured official corps would have to reconstitute their legitimacy claims on different grounds entirely.
% FOUNDING_PROBLEM: In the reform era, sustained high growth was the most legible way to demonstrate that market-oriented reforms were delivering rising living standards and national strength after decades of stagnation, providing a performance-based legitimacy substitute for procedural or electoral legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Central planning authorities and the industrial-export complex attest the growth-rate target remains necessary to sustain employment and national strength. Independent economists, IMF and World Bank structural assessments, and domestic household-income researchers outside the beneficiary set attest that the marginal returns to investment-driven growth have fallen sharply, that overcapacity and local government debt are now larger risks than slower growth itself, and that the founding problem of demonstrating reform success has been substantially supplanted by inertial pursuit of the metric.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.42 to 0.68 over the interval as the growth-rate target increasingly requires debt-financed capacity expansion rather than organic demand growth to sustain — a rent-seeking overlay accumulating on top of what began as a genuine coordination/signaling function. Theater ratio rises in parallel (0.18 to 0.42) as reported growth increasingly reflects investment volume and infrastructure completion rather than underlying demand or household welfare — a Goodhart-drift signature where the proxy (growth rate) is increasingly optimized independently of the substantive goal (rising living standards) it was meant to indicate. Suppression rises moderately (0.40 to 0.61) as maintaining the target requires increasingly active management of credit allocation, local government debt rollover, and export channel support to keep the number from falling below the politically load-bearing threshold.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (GDP-measured local officials and central planning authorities), the arrangement reads as functional and necessary coordination — a legible signal that lets a large, complex economy demonstrate continued success and sustain investor and citizen confidence. From the payer seats (overleveraged local governments, overcapacity workers, households), the same structure reads as an extraction mechanism sustained by debt rollover and suppressed consumption share. This is not disagreement about facts; it is the structural asymmetry the engine is built to detect — the same constraint computes differently per seat because the seats bear genuinely different costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The industrial-export complex and GDP-measured local officials sit near the beneficiary end: the growth-rate target directly channels credit, land, and career advancement toward them. Overleveraged local governments, overcapacity-sector workers, and the household consumption base sit near the target end: they carry the debt, the boom-bust employment volatility, and the systematically underweighted consumption share respectively, with limited exit — local governments are bound by bond obligations and administrative hierarchy, workers by regional labor-market immobility, households by the structural weighting of the growth model itself. Trade-partner domestic industries experience extraction without even a formal seat in the arrangement that produces it — they are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — demonstrating that reform-era policy was delivering rising living standards — was substantially real in the early period and is now, per independent assessment outside the beneficiary set, largely solved; continued pursuit of the same growth-rate target increasingly serves to defend the legitimacy claim itself rather than to solve the problem the claim was built to address. This is the tangled_rope signature rather than a pure snare: a genuine coordination function (a legible national performance signal) persists alongside an accumulating asymmetric extraction (debt, overcapacity, suppressed consumption) that requires active enforcement — credit allocation, promotion incentives, export support — to sustain. Reclassifying this arrangement as a pure snare would erase the coordination function it once performed and may still partially perform; reclassifying it as a pure rope would erase the now-dominant extraction. Tangled rope holds both facts simultaneously, which is the point of the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_target_kernel_reading_choice,
    'Is the quantitative growth-rate reading the operative legitimacy criterion, or has actual state practice already shifted toward one of the sibling readings (qualitative development, techno-nationalist, or livelihood security) while retaining growth-rate rhetoric as residual framing?',
    'Track which metric officials are actually promoted or sanctioned on over time — growth rate vs. innovation/patent metrics vs. strategic-sector self-sufficiency vs. household income/social-insurance coverage — and whether policy documents'' stated priorities diverge from revealed promotion criteria.',
    'If revealed practice has shifted toward a sibling reading, this constraint''s beneficiary set and extraction profile would need revision to match; the quantitative_growth_reading would itself be drifting toward obsolescence as the operative kernel reading even while remaining the nominally stated one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_target_kernel_reading_choice, conceptual, 'Whether stated and revealed legitimacy criteria have diverged within the kernel contest.').

omega_variable(
    debt_absorption_capacity,
    'Is the accumulated local government debt from growth-rate-driven infrastructure and industrial investment ultimately absorbable through central fiscal transfer, or does it represent an irreversible transfer of extraction cost onto future fiscal capacity?',
    'Central government balance sheet analysis and comparison of local government debt service ratios against realistic revenue growth projections, independent of official growth-rate reporting.',
    'If absorbable, current extraction is better read as temporary front-loading of a genuine coordination investment; if not absorbable, the tangled_rope''s extraction component is structurally locked in rather than provisional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(debt_absorption_capacity, empirical, 'Whether debt accumulated to sustain the growth-rate target is a temporary or permanent extraction.').

omega_variable(
    growth_rate_measurement_integrity,
    'To what extent does the reported growth rate itself reflect genuine output versus reporting practices optimized to meet the politically load-bearing threshold?',
    'Cross-check reported GDP growth against independent proxies — electricity consumption, rail freight volume, satellite nighttime luminosity, tax receipt growth — over the same interval.',
    'A wide divergence between reported growth and independent proxies would indicate the theater_ratio understates the degree to which the legitimacy signal itself has been substituted for the underlying reality it claims to represent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_rate_measurement_integrity, empirical, 'Whether the growth-rate signal has decoupled from the reality it purports to measure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__quantitative_growth_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__quantitative_growth_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__quantitative_growth_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language concept 'performance legitimacy' per the ε-invariance principle: each reading of the performance_legitimacy kernel names a structurally distinct legitimacy criterion with its own beneficiary/victim structure and its own ε. This reading (quantitative_growth_reading) is linked to its three siblings via affects_constraints because the four readings compete for the same institutional resource — the state's finite legitimacy-and-policy-attention budget — such that resource commitment to this reading's investment-driven model structurally reduces resource availability for the qualitative_development_reading's efficiency-and-innovation agenda and the livelihood_security_reading's consumption-and-social-insurance agenda, while partially overlapping with (and sometimes instrumentally serving) the techno_nationalist_reading's strategic-sector build-out.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
