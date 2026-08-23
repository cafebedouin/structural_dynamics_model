% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the creditor coordination reading of
 *   structural adjustment conditionalities — the view that conditionality
 *   frameworks (IMF Stand-By Arrangements, World Bank Structural Adjustment
 *   Loans, PRGT facilities) function as necessary coordination mechanisms
 *   solving a genuine collective action problem among sovereign creditors.
 *   The reading acknowledges adjustment costs but frames them as the price of
 *   coordination, not extraction. Beneficiaries are future taxpayers
 *   (intergenerational fiscal sustainability), international capital (risk
 *   reduction), and creditor institutions themselves (mandate fulfillment).
 *   The primary bearers of concentrated costs are inefficient state sectors
 *   disciplined by reform benchmarks. The engine will compute per-seat
 *   classifications from this structural data; the claimed type is rope
 *   (coordination with minimal coercive overhead, net beneficiaries,
 *   alternatives not suppressed).
 *
 * KEY AGENTS:
 *   - creditor_institutions: agenda_setter (institutional/arbitrage) — designs and enforces frameworks
 *   - debtor_governments: payer/beneficiary (organized/constrained) — trades autonomy for market access
 *   - future_taxpayers: beneficiary (powerless/trapped) — inherits fiscal space
 *   - international_capital: beneficiary (powerful/arbitrage) — gains credible commitment
 *   - inefficient_state_sectors: payer (moderate/constrained) — bears concentrated reform costs
 *   - civil_society_excluded: excluded (powerless/trapped) — experiences distribution without voice
 *   - independent_evaluation_offices: observer (analytical/analytical) — provides epistemic accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.18).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '79fb1d1f-5f14-4b84-82c6-e28763235714').
narrative_ontology:cs_kernel_codification('79fb1d1f-5f14-4b84-82c6-e28763235714', formalized).
narrative_ontology:cs_authority_grounding('79fb1d1f-5f14-4b84-82c6-e28763235714', lineage).
narrative_ontology:cs_interpretation_layer_present('79fb1d1f-5f14-4b84-82c6-e28763235714').
narrative_ontology:cs_reading_relation('79fb1d1f-5f14-4b84-82c6-e28763235714', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('79fb1d1f-5f14-4b84-82c6-e28763235714', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('79fb1d1f-5f14-4b84-82c6-e28763235714', foundational, conditional_lending_solves_creditor_coordination).
narrative_ontology:cs_axiom_status(conditional_lending_solves_creditor_coordination, holdable).
narrative_ontology:cs_axiom_grounding('79fb1d1f-5f14-4b84-82c6-e28763235714', conditional_lending_solves_creditor_coordination, instrumental).
narrative_ontology:cs_axiom('79fb1d1f-5f14-4b84-82c6-e28763235714', foundational, fiscal_discipline_credibility_enables_market_access).
narrative_ontology:cs_axiom_status(fiscal_discipline_credibility_enables_market_access, holdable).
narrative_ontology:cs_axiom_grounding('79fb1d1f-5f14-4b84-82c6-e28763235714', fiscal_discipline_credibility_enables_market_access, empirically_contingent).
narrative_ontology:cs_reference_frame('79fb1d1f-5f14-4b84-82c6-e28763235714', baker_brady_coordination_framework).
narrative_ontology:cs_drift_state('79fb1d1f-5f14-4b84-82c6-e28763235714', post_washington_consensus_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79fb1d1f-5f14-4b84-82c6-e28763235714', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_discipline_enhances_credibility).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, market_access_requires_policy_anchors).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, conditional_lending_solves_time_inconsistency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce conditionality frameworks (IMF, World Bank, Paris Club, regional development banks). They set the terms of lending programs, monitor compliance, and control access to concessional finance. Their authority derives from capital provision and the collective action problem they solve among sovereign lenders. They face minimal exit costs — they can reallocate lending or adjust terms — and their institutional mandate is sustained by member state contributions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Negotiate and implement adjustment programs to access balance-of-payments support and market signaling. They bear the political costs of reform (subsidy cuts, privatization, tax increases) but gain restored market access, lower borrowing costs, and credibility with investors. Exit is constrained: sovereign default is costly, and alternative financing (regional arrangements, bilateral deals) typically carries similar or stricter conditions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments, beneficiary).

% Inherit the fiscal space created by today's adjustment. They benefit from avoided debt crises, lower debt service burdens, and functional public services. They have no voice in current negotiations and cannot exit the intergenerational fiscal contract. Their situation is defined by structural dependency on today's policy choices.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% Private investors, bondholders, and financial institutions who price sovereign risk. Conditionality provides a credible commitment device that reduces information asymmetry and moral hazard, enabling portfolio allocation to emerging markets. They benefit from reduced default risk and more predictable policy environments. Exit is near-costless — capital is mobile and can reallocate across jurisdictions instantly.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% State-owned enterprises, bloated bureaucracies, and protected industries targeted by structural benchmarks (privatization, subsidy removal, civil service reform). They bear concentrated adjustment costs: job losses, revenue declines, loss of rents. Their political organization (unions, lobbying networks) gives them moderate blocking power, but exit options are limited — skills are often sector-specific, and geographic mobility is low.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors, payer,
    moderate, immediate, constrained, local).

% Labor unions, community organizations, indigenous groups, and informal sector workers who experience adjustment's distributive effects but are not seated at program design tables. They would object to regressive VAT increases, user fees for health/education, and labor market flexibilization. Their exclusion is structural: conditionality negotiations occur between finance ministries and creditor staff, with parliamentary ratification often rushed or bypassed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, civil_society_excluded, excluded,
    powerless, biographical, trapped, local).

% IEO (IMF), IEG (World Bank), and academic researchers who assess program outcomes ex post. They produce the evidence base on whether conditionalities achieve stated objectives (growth, stability, poverty reduction). Their analytical independence varies — some are institutionally housed, others fully external. They neither collect rents nor bear costs; their function is epistemic accountability.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, independent_evaluation_offices, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem among heterogeneous creditors (official bilateral, multilateral, private) by providing a single, rule-based framework for crisis lending. Without conditionality, each creditor would demand idiosyncratic reforms, creating policy chaos; or no creditor would lend, fearing free-rider problems where others benefit from their discipline. Conditionality coordinates creditor expectations, sequences reforms, and anchors debtor credibility — a genuine multi-party coordination problem with no unilateral solution.
% TRANSFER_FUNCTION: Moves policy autonomy from debtor governments to creditor institutions in exchange for financial resources and market credibility. The transfer is not primarily monetary (loans are repaid) but institutional: debtors accept externally monitored reform agendas; creditors provide the coordination infrastructure (monitoring, technical assistance, sequential tranching). Inefficient state sectors transfer rents and employment to the broader fiscal commons (future taxpayers) via privatization and subsidy reform.
% ABSENT_VOICES: Civil society organizations, informal sector workers, and subnational governments are structurally excluded from conditionality design. They would object to the regressive incidence of consumption taxes, user fees for essential services, and labor flexibilization that disproportionately affect women and youth. Their absence is not accidental — the negotiation architecture (finance ministry + creditor staff) has no formal channel for participatory input, and parliamentary oversight is often pro forma.
% DISAPPEARANCE_RATIONALE: If conditionality frameworks vanished overnight, sovereign lending would not cease but would fragment: bilateral creditors would impose ad hoc political conditions, private creditors would demand higher risk premia, and debtors would face coordination failure among lenders. Market access would become more volatile and costly. The coordination function is structural — its absence rearranges the sovereign lending architecture, it does not leave it unchanged.
% FOUNDING_PROBLEM: The 1980s Latin American debt crisis revealed a systemic coordination failure: commercial banks had overlent, no single creditor could enforce reform, and debtors faced unsustainable debt service without a credible adjustment path. The Baker and Brady Plans institutionalized conditionality as the solution — multilateral institutions would coordinate creditors, provide new money, and monitor reform programs, converting a chaotic standoff into a structured workout.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions and academic economists (e.g., Sachs, Williamson) attest the founding problem was the 1980s coordination vacuum and that conditionality resolved it. Debtor-country economists (e.g., Stiglitz, Rodrik) and civil society networks (Jubilee, Eurodad) attest the problem has mutated: today's conditionalities address not coordination failures but ideological priors (liberalization, privatization) applied regardless of country context. Independent evaluation offices (IEO, IEG) document mixed outcomes — conditionality improves fiscal balances but often fails on growth and poverty reduction, suggesting the founding problem is only partially solved and the instrument has drifted.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because the constraint's primary function is coordination — creditors get a shared framework, debtors get market access, future taxpayers get fiscal sustainability. The transfer is policy autonomy for financial credibility, not resource extraction. Suppression is low (0.18) because participation is voluntary (countries request programs), alternatives exist (regional funds, bilateral deals, default), and enforcement is monitoring-based not coercive. Theater ratio is low (0.12) — conditionality mechanisms (tranching, benchmarks, technical assistance) are functional, not performative. Accessibility collapse is moderate (0.35) because while alternatives exist, they are costly (higher spreads, political conditionality, default stigma). Resistance is low (0.22) — most programs are country-owned at the executive level, though parliamentary and street-level resistance occurs. The metric profile is consistent with rope: genuine coordination, net beneficiaries, minimal coercion.
 *
 * PERSPECTIVAL GAP:
 *   The creditor_coordination_reading computes as rope from the agenda_setter and beneficiary seats: a working coordination mechanism. The debtor_extraction_reading (sibling) would compute as snare from the payer/excluded seats: extraction under coercion. The hybrid_selectivity_reading would compute as tangled_rope: coordination for some, extraction for others, depending on geopolitical weight. This seat divergence is the point — the same constraint structure produces different classifications depending on structural position. The engine captures this; the authoring task is to declare the structural data honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditor institutions and international capital are structural beneficiaries (d ~ 0.1-0.2): they gain coordination infrastructure and risk reduction. Future taxpayers are beneficiaries (d ~ 0.1) but trapped — they cannot exit the intergenerational contract. Debtor governments are near-symmetric (d ~ 0.45): they pay political costs but gain market access and lower spreads. Inefficient state sectors are targets (d ~ 0.7-0.8): they bear concentrated costs with constrained exit. Civil society excluded are effectively trapped (d ~ 0.9) but not targeted — they are collateral to the coordination logic. The engine derives these from beneficiary/victim declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s creditor coordination failure) was real and conditionality solved it. Whether the mandate has atrophied is contested: creditor institutions argue the coordination problem persists (new creditors, more complex debt structures, climate finance); critics argue the instrument has drifted into ideological imposition (one-size-fits-all liberalization) and the original coordination logic no longer fits a world of diverse creditors (China, bond markets, sovereign wealth funds). The mandatrophy question is live — captured by founding_problem_status: contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the creditor_coordination_reading structurally relate to the sibling readings (debtor_extraction_reading, hybrid_selectivity_reading) of the same kernel?',
    'Trace the institutional genealogy: does the creditor coordination framework logically foreclose the extraction framing, or do they coexist as competing interpretations of the same institutional apparatus? The IEO/IEG evaluation record provides empirical adjudication.',
    'If forecloses: this reading''s axioms (fiscal discipline as coordination) are structurally incompatible with extraction axioms — one framework cannot hold both. If coexists_with: both readings are live positions in ongoing discourse, and the kernel remains contested. If influences: this reading''s dominance in creditor institutions creates structural pressure on debtor-country policy space, shaping the terrain on which the extraction reading operates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relationship between this reading and its kernel siblings').

omega_variable(
    conditionality_design_vs_implementation_gap,
    'Does the measured low extractiveness reflect conditionality''s design (coordination logic) or its implementation (where structural benchmarks often exceed coordination needs)?',
    'Compare program documents (design) with implementation completion reports (actual benchmarks). Structural benchmarks on labor markets, land tenure, and SOE privatization often go beyond fiscal coordination into ideological territory.',
    'If implementation systematically exceeds coordination needs, the rope claim describes the design myth, not the operational reality — the constraint may compute as tangled_rope from payer seats despite low aggregate ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_design_vs_implementation_gap, empirical, 'Design-implementation gap in conditionality structural benchmarks').

omega_variable(
    geopolitical_selectivity_empirical,
    'Is the hybrid_selectivity_reading''s claim (waivers for strategic debtors) empirically systematic or anecdotal?',
    'Cross-country regression of conditionality count/stringency on geopolitical alignment indices (UN voting, military alliances, resource access), controlling for macroeconomic fundamentals.',
    'If systematic, the coordination reading''s claim of universal rule-based frameworks is falsified — the constraint operates as tangled_rope (coordination for some, extraction for others) rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_selectivity_empirical, empirical, 'Whether geopolitical selectivity is a structural feature of conditionality application').

omega_variable(
    intergenerational_distribution_measurement,
    'Can the benefit to future_taxpayers be measured independently of the political discount rate applied by current governments?',
    'Sovereign debt sustainability analyses with varying discount rates; fiscal reaction function estimation; counterfactual simulations of no-adjustment debt trajectories.',
    'If benefits to future taxpayers are highly discount-rate sensitive, the beneficiary claim may be a modeling artifact rather than a structural fact — undermining the rope classification''s net-beneficiary premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_distribution_measurement, empirical, 'Measurability of intergenerational fiscal benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_ccr_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(sac_ccr_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(sac_ccr_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(sac_ccr_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(sac_ccr_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(sac_ccr_tr_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(sac_ccr_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(sac_ccr_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(sac_ccr_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(sac_ccr_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(sac_ccr_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement(sac_ccr_be_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sac_ccr_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(sac_ccr_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(sac_ccr_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(sac_ccr_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(sac_ccr_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.16).
narrative_ontology:measurement(sac_ccr_su_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This is the creditor_coordination_reading of the structural_adjustment_conditionalities kernel. The debtor_extraction_reading frames the same institutional apparatus as snare (extraction from debtor populations for creditor profit). The hybrid_selectivity_reading frames it as tangled_rope (coordination for weak states, waived extraction for strategic debtors). The three readings share the kernel (conditional lending frameworks) but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. This decomposition follows the ε-invariance principle: one label, multiple structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__creditor_coordination_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
