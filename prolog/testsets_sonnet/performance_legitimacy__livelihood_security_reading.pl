% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy — Livelihood Security Reading
 *   domain: political economy / development planning / state capitalism
 *
 * SUMMARY:
 *   This story instantiates the livelihood-security reading of the
 *   performance legitimacy kernel: a governance regime grounds its claim to
 *   rule not in procedural consent but in tangible, personally-verifiable
 *   improvements in employment, healthcare, education, and elder care.
 *   Structurally, this reading redirects fiscal and administrative priority
 *   away from capital-intensive industrial expansion and local-government
 *   infrastructure spending and toward household consumption support and
 *   service-sector employment. This is a distinct constraint from the
 *   quantitative_growth_reading (GDP-rate legitimacy), the
 *   qualitative_development_reading (innovation/efficiency legitimacy), and
 *   the techno_nationalist_reading (strategic self-sufficiency legitimacy) —
 *   each of those readings has a different beneficiary/victim structure and a
 *   different epsilon, because each redirects a different resource flow under
 *   the same broad legitimacy label. Conflating them would violate
 *   epsilon-invariance; they are authored as separate constraint files linked
 *   through the network field.
 *
 * KEY AGENTS:
 *   - central_fiscal_transfer_administrators: sets the redistribution formula (institutional/analytical) — administers the shift toward livelihood security
 *   - household_consumers and elderly_care_recipients: primary beneficiaries (powerless/trapped) — receive visible daily-life improvements
 *   - capital_intensive_industrial_firms and local_government_infrastructure_bureaus: primary payers (powerful-organized/constrained-trapped) — lose fiscal and policy priority
 *   - policy_analysts: analytical observer assessing sustainability of the substitution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.51).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.58).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.51).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy — Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political economy / development planning / state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'b13a33da-c392-4df4-b65a-da038622b98d').
narrative_ontology:cs_kernel_codification('b13a33da-c392-4df4-b65a-da038622b98d', distributed).
narrative_ontology:cs_authority_grounding('b13a33da-c392-4df4-b65a-da038622b98d', extraction).
narrative_ontology:cs_interpretation_layer_present('b13a33da-c392-4df4-b65a-da038622b98d').
narrative_ontology:cs_reading_relation('b13a33da-c392-4df4-b65a-da038622b98d', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('b13a33da-c392-4df4-b65a-da038622b98d', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('b13a33da-c392-4df4-b65a-da038622b98d', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('b13a33da-c392-4df4-b65a-da038622b98d', foundational, directly_experienced_welfare_is_the_legitimacy_test).
narrative_ontology:cs_axiom_status(directly_experienced_welfare_is_the_legitimacy_test, holdable).
narrative_ontology:cs_axiom_grounding('b13a33da-c392-4df4-b65a-da038622b98d', directly_experienced_welfare_is_the_legitimacy_test, instrumental).
narrative_ontology:cs_axiom('b13a33da-c392-4df4-b65a-da038622b98d', secondary, consumption_and_service_delivery_outrank_capital_formation).
narrative_ontology:cs_axiom_status(consumption_and_service_delivery_outrank_capital_formation, holdable).
narrative_ontology:cs_axiom_grounding('b13a33da-c392-4df4-b65a-da038622b98d', consumption_and_service_delivery_outrank_capital_formation, empirically_contingent).
narrative_ontology:cs_reference_frame('b13a33da-c392-4df4-b65a-da038622b98d', investment_led_growth_legitimacy_baseline).
narrative_ontology:cs_drift_state('b13a33da-c392-4df4-b65a-da038622b98d', post_growth_deceleration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b13a33da-c392-4df4-b65a-da038622b98d', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_care_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, central_fiscal_transfer_administrators).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_firms).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, construction_and_heavy_industry_workers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, provincial_debt_bondholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the redistribution formula that channels revenue toward pensions, healthcare subsidies, unemployment insurance, and education transfers, and administer the audits and target-setting cycles that reward local officials for livelihood-security metrics. They redefine what counts as legitimate governance performance and can redirect capital away from industrial megaprojects toward consumption support at will.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_fiscal_transfer_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Gain from expanded employment in retail, care work, healthcare, and education as consumption-support policy channels demand and subsidy their way. Their livelihoods improve directly and visibly, which is the point of the reading, but their sector's growth is contingent on continued political prioritization rather than independent market demand.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    moderate, biographical, constrained, national).

% Receive subsidized healthcare, education vouchers, elder-care stipends, and social insurance payouts that visibly raise daily living standards. They have no exit from the state's welfare architecture — it is the only channel through which these services reach them — and their support for the regime is transactionally tied to continued delivery.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumers, beneficiary,
    powerless, biographical, trapped, local).

% Depend entirely on pension adequacy and elder-care infrastructure funded under this reading; they have no private alternative at scale and cannot exit the system that determines whether their care needs are met.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_care_recipients, beneficiary,
    powerless, biographical, trapped, local).

% Lose priority access to investment credit, subsidized land, and infrastructure spending as fiscal and policy attention shifts toward consumption and services. They can lobby or relocate production, but exiting the domestic policy environment entirely means abandoning scale advantages built over decades.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_firms, payer,
    powerful, biographical, constrained, national).

% Historically funded themselves and their political standing through land sales and investment-driven GDP contribution; under this reading their infrastructure megaprojects are deprioritized in favor of transfers to households, cutting into the fiscal base and political relevance built around construction-led growth. They cannot exit the hierarchy that now downgrades their preferred growth model.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus, payer,
    organized, biographical, trapped, regional).

% Face reduced project pipelines and layoffs as investment-led growth decelerates in favor of consumption support. Some can retrain into service work, but many are geographically and skill-locked into industrial regions that lose priority under the livelihood-security framing.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, construction_and_heavy_industry_workers, payer,
    powerless, biographical, constrained, regional).

% Hold debt issued against expected infrastructure and land-sale revenue streams; deprioritizing investment-led local government spending threatens repayment capacity built on the assumption that construction-driven growth would continue funding debt service.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, provincial_debt_bondholders, payer,
    organized, biographical, constrained, national).

% Assess whether livelihood-security spending represents durable social-contract renewal or a fiscally unsustainable substitution for the investment-led model, comparing outcomes across governance cycles and provinces.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, diffuse).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the state's fiscal and administrative apparatus around delivering employment, healthcare, education, and elder care that citizens experience directly, converting aggregate economic performance into legible, personally-felt welfare gains that sustain political consent without requiring procedural or electoral legitimation.
% TRANSFER_FUNCTION: Moves fiscal capacity, credit allocation, and local administrative priority away from capital-intensive industrial expansion and infrastructure megaprojects and toward household consumption subsidies, service-sector employment support, and social insurance — from industrial capital and construction-oriented local governments to households, service workers, and welfare-dependent populations.
% ABSENT_VOICES: Industrial regional governments and heavy-industry labor constituencies whose political capital was built on investment-led growth are structurally sidelined in the redistribution formula-setting process; they can lobby through bureaucratic channels but are not present when livelihood-security metrics are defined at the center.
% DISAPPEARANCE_RATIONALE: If livelihood-security legitimacy were abandoned overnight, fiscal transfers to healthcare, education, and elder care would contract, service-sector employment support would lapse, and political consent among the urban and rural poor — currently transacted against visible welfare delivery — would have to be re-secured through alternative means (coercion, growth-based legitimacy, or nationalist mobilization); local governments and industrial firms would regain fiscal priority almost immediately.
% FOUNDING_PROBLEM: As export- and investment-led growth decelerated and inequality from decades of infrastructure-and-industry-first development became politically salient, the regime needed a legitimacy claim that ordinary households could verify directly, rather than one resting on aggregate statistics increasingly disconnected from lived experience.
% FOUNDING_PROBLEM_CORROBORATION: Independent household-survey data and international development economists outside the central administration corroborate that healthcare/education/elder-care service gaps remain substantial and politically salient; provincial fiscal auditors (an institution with incentives to overstate transfer needs to preserve budget share) also attest to persistent underfunding, providing partial corroboration from a seat with mixed incentives rather than a fully independent one.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.51, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.51 at interval end) because this reading does not extract wealth outright — it reallocates fiscal capacity from one constituency (industrial capital, construction-heavy local governments) to another (households, service workers) under the frame of legitimacy maintenance. The extraction is real from the payer seats' perspective: capital and infrastructure programs they depend on for their political and economic standing are deprioritized without their consent, and this deprioritization is enforced through central budget allocation and cadre evaluation criteria rather than negotiated. Suppression (0.58) reflects the active administrative enforcement needed to redirect provincial fiscal behavior against the entrenched investment-led incentive structure — local officials whose careers were built on land-sale-financed infrastructure face demotion or discipline for continuing the old model. Theater ratio (0.42) captures a meaningful risk that headline livelihood-security metrics (jobs created, subsidies disbursed) substitute for durable welfare-state capacity, especially where transfers are one-off or symbolic rather than structural.
 *
 * PERSPECTIVAL GAP:
 *   From the central administrators' seat, this looks like rope: a necessary and overdue rebalancing toward welfare delivery that restores the state's social contract. From the local infrastructure bureau and industrial capital seats, the same redirection looks like tangled rope at best — a coordination function (welfare delivery) riding on an enforced extraction of fiscal priority they had structured their operations around. The engine should register this divergence rather than resolve it toward either seat's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Central fiscal administrators sit at the agenda-setting end: they redefine the metrics of legitimate governance and control the redistribution formula, giving them low d relative to this constraint. Household consumers and elderly care recipients are structural beneficiaries with trapped exit — they cannot access these services except through the state welfare architecture that this reading strengthens, giving them low-to-moderate d despite their powerlessness (their trapped exit is a subsidy-lock, not an extraction-lock). Capital-intensive industrial firms and local government infrastructure bureaus are targets: fiscal and policy priority is actively redirected away from them, and their exit options (relocate production, lobby) are constrained rather than free, pushing their d toward the target end. Construction workers and provincial bondholders bear second-order costs from the same redirection with even less capacity to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — growth-model legitimacy no longer felt by ordinary households — remains live per independent survey corroboration, which argues against mandatrophy. But the theater_ratio trend (0.22 to 0.42) signals a live risk: if livelihood-security delivery increasingly substitutes symbolic transfers for structural welfare-state capacity while the underlying legitimacy problem persists, the constraint would drift toward Piton even as its founding problem remains genuinely unresolved. Tracking founding_problem_status against theater_ratio trajectory over subsequent cycles is the diagnostic to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_reading_vs_growth_reading_boundary,
    'At what point does livelihood-security spending stop being a distinct legitimacy strategy and become merely a redistributive subset of quantitative growth policy — i.e., is this reading structurally separable from the quantitative_growth_reading, or does sustained service-sector employment ultimately require the GDP growth the other reading targets?',
    'Track whether livelihood-security fiscal transfers are funded from growth-independent sources (wealth taxes, state asset dividends) versus growth-contingent revenue (VAT, land sales); persistent reliance on the latter would indicate the readings are not structurally independent constraints but phases of one growth-legitimacy cycle.',
    'If not separable, this story''s epsilon and beneficiary/victim structure would need revision, and the two readings might need to be merged or reframed as sequential phases rather than parallel readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_reading_vs_growth_reading_boundary, conceptual, 'Whether livelihood-security legitimacy is structurally independent of growth-based legitimacy or parasitic on it.').

omega_variable(
    sustainability_of_consumption_prioritization,
    'Can consumption-support and service-sector prioritization be fiscally sustained over a generational time horizon without the industrial and infrastructure investment base it deprioritizes, or does this reading create a slow-building fiscal crisis that will force reversion to investment-led legitimacy?',
    'Longitudinal fiscal-sustainability modeling comparing provincial debt service capacity, tax base growth, and social-insurance liability accumulation under sustained livelihood-security prioritization versus counterfactual investment-led paths.',
    'If unsustainable, the current tangled_rope classification understates the eventual extraction from future households and workers who will bear adjustment costs when the model reverses; the constraint''s victim set would expand intergenerationally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sustainability_of_consumption_prioritization, empirical, 'Long-run fiscal sustainability of prioritizing consumption and services over industrial investment.').

omega_variable(
    cs_framing_kernel_or_institution,
    'Is the correct commitment-system kernel the abstract ''performance legitimacy'' claim itself, or is the more fundamental kernel the Party''s institutional monopoly on defining what counts as legitimate governance performance at all — of which livelihood security is merely one selectable content?',
    'Compare classification outcomes: framing the kernel as the abstract legitimacy claim (this file''s approach) treats readings as competing content selections; framing the kernel as the institutional authority to select ANY content would fold all four readings into a single higher-order constraint about discretionary legitimacy-content selection, with this file''s four readings becoming instances rather than siblings.',
    'Under the alternative framing, the institutional-authority constraint would likely classify as tangled_rope or snare regardless of which content reading is selected, since the extraction (unaccountable discretion over what counts as legitimate) would be constant across readings — this file''s per-reading epsilon variation would then be diagnostic noise around a stable higher-order extraction, not four genuinely distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_or_institution, conceptual, 'Alternative CS framing: content-selection kernel (as authored) versus institutional-discretion kernel (unauthored alternative) that would subsume all four readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__livelihood_security_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__livelihood_security_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__livelihood_security_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__livelihood_security_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__livelihood_security_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__livelihood_security_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__livelihood_security_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__livelihood_security_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__livelihood_security_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__livelihood_security_reading, base_extractiveness, 24, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__livelihood_security_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__livelihood_security_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__livelihood_security_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__livelihood_security_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__livelihood_security_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the performance_legitimacy kernel, each with its own constraint_id, epsilon, and beneficiary/victim structure per the ε-invariance principle. livelihood_security_reading redirects fiscal priority toward households/services and away from industrial capital and local infrastructure spending. quantitative_growth_reading maintains GDP-rate legitimacy and would show a different beneficiary set (export/industrial capital, employment-generating sectors broadly). qualitative_development_reading prioritizes innovation/efficiency (beneficiary: high-tech and green sectors; victim: legacy heavy industry). techno_nationalist_reading prioritizes strategic self-sufficiency (beneficiary: state-directed strategic-sector firms; victim: consumer welfare spending and open trade-dependent sectors). All four compete for the same finite fiscal and administrative capacity, so strengthening one reading structurally weakens the resource base available to the others — hence the affects_constraints linkage in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
