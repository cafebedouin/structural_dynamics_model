% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading
 *   domain: political_economy/international_law/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story models the SOVEREIGNTY PRIMACY READING of NAFTA's
 *   jurisdictional boundary: trade agreement text operates as a coordination
 *   mechanism subordinate to sovereign domestic law, with states retaining
 *   full regulatory authority over labor, environmental, and health standards
 *   within their territories. This reading interprets the agreement as
 *   creating non-discriminatory market access and dispute resolution without
 *   imposing regulatory harmonization or treaty supremacy. Under this
 *   reading, domestic regulatory agencies have legal authority to set
 *   standards; multinational capital faces compliance costs but cannot invoke
 *   the treaty to override domestic choices. The constraint is framed as ROPE
 *   (genuine coordination on market access and dispute resolution) because
 *   the founding problem it solves—preventing tariff wars while preserving
 *   regulatory autonomy—is real and both coordination functions (preventing
 *   beggar-thy-neighbor tariffs; maintaining jurisdictional authority)
 *   benefit all parties. Extraction is low (0.28) because the coordination
 *   gains are substantial relative to the compliance-cost burden; suppression
 *   is very low (0.15) because the constraint relies on democratic
 *   ratification and ongoing legislative authority, not coercion. This
 *   reading COEXISTS WITH the capital_supremacy_reading (held by different
 *   institutional actors and capital markets) and INFLUENCES the
 *   embedded_liberalism_reading (which accepts some regulatory authority but
 *   adds compatibility requirements).
 *
 * KEY AGENTS:
 *   - participating_states: Institutional agenda-setter; negotiate, ratify, and administer the agreement while retaining regulatory authority
 *   - domestic_regulatory_agencies: Institutional beneficiary; maintain statutory authority over standards, bear compliance-cost burden
 *   - multinational_capital: Institutional payer; gain market access but cannot use treaty supremacy to override standards
 *   - labor_unions and environmental_advocates: Organized beneficiaries; retain legislative avenue for standard-setting, depend on domestic political power
 *   - trade_dispute_panels: Institutional observer; adjudicate non-discrimination and transparency, not regulatory substance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.28).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "political_economy/international_law/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '7205734a-e8e5-4fc0-9e68-55af9a86e16c').
narrative_ontology:cs_kernel_codification('7205734a-e8e5-4fc0-9e68-55af9a86e16c', fixed_text).
narrative_ontology:cs_authority_grounding('7205734a-e8e5-4fc0-9e68-55af9a86e16c', lineage).
narrative_ontology:cs_interpretation_layer_present('7205734a-e8e5-4fc0-9e68-55af9a86e16c').
narrative_ontology:cs_reading_relation('7205734a-e8e5-4fc0-9e68-55af9a86e16c', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('7205734a-e8e5-4fc0-9e68-55af9a86e16c', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_axiom('7205734a-e8e5-4fc0-9e68-55af9a86e16c', foundational, treaty_subordinate_to_domestic_law).
narrative_ontology:cs_axiom_status(treaty_subordinate_to_domestic_law, holdable).
narrative_ontology:cs_axiom_grounding('7205734a-e8e5-4fc0-9e68-55af9a86e16c', treaty_subordinate_to_domestic_law, deontological).
narrative_ontology:cs_axiom('7205734a-e8e5-4fc0-9e68-55af9a86e16c', foundational, regulatory_authority_retained_by_states).
narrative_ontology:cs_axiom_status(regulatory_authority_retained_by_states, holdable).
narrative_ontology:cs_axiom_grounding('7205734a-e8e5-4fc0-9e68-55af9a86e16c', regulatory_authority_retained_by_states, deontological).
narrative_ontology:cs_reference_frame('7205734a-e8e5-4fc0-9e68-55af9a86e16c', treaty_as_coordination_mechanism_not_supremacy).
narrative_ontology:cs_drift_state('7205734a-e8e5-4fc0-9e68-55af9a86e16c', contemporary_capital_mobility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7205734a-e8e5-4fc0-9e68-55af9a86e16c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, participating_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_labor_unions).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_capital).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, capital_markets).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereignty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and ratify the trade agreement text. Retain formal authority to set labor, environmental, and health standards within their territory. The agreement provides market access and dispute resolution mechanisms, but does not override domestic regulatory jurisdiction. States can enforce their standards provided they do not discriminate against foreign goods or services.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, participating_states, agenda_setter,
    institutional, generational, constrained, continental).

% Maintain full statutory and regulatory authority over labor standards, environmental protection, and public health within national territory. Trade agreement obligations create a compliance-cost set (documenting justifications for standards, participating in dispute resolution if challenged) but do not constrain the substance of regulatory decisions. They write and enforce standards according to domestic political process.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, analytical, national).

% Under this reading, can advocate for labor standards through domestic law without treaty obligations overriding legislative outcomes. Their ability to set standards depends on domestic political power, not on trade agreement preemption. They bear the cost of enforcement across borders (labor arbitrage pressures remain) but retain the statutory avenue.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_labor_unions, beneficiary,
    organized, biographical, constrained, national).

% Can lobby for environmental standards through domestic legislative process without treaty-mandated harmonization limits. The agreement requires non-discriminatory application but does not impose ceiling-level harmonization. They bear pressure from regulatory arbitrage and capital mobility, but the formal avenue remains open.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_advocates, beneficiary,
    organized, generational, constrained, national).

% Under this reading, cannot invoke treaty supremacy to overturn domestic labor or environmental standards. They face a compliance-cost set: if a state sets standards, multinationals must meet them or exit the market. Market access is granted on condition of domestic regulatory compliance. Their ability to arbitrage standards depends on actual regulatory gaps, not on treaty preemption of differing standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_capital, payer,
    institutional, biographical, arbitrage, global).

% Price in the cost of regulatory compliance across jurisdictions. Under this reading, cannot assume harmonization will occur; must model divergent standards and compliance costs as persistent features. Investment decisions must account for domestic regulatory authority as binding, not as noise.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, capital_markets, payer,
    institutional, biographical, arbitrage, global).

% Adjudicate disputes between states on trade agreement compliance. Under this reading, their role is narrow: they enforce non-discrimination and transparency requirements, not regulatory harmonization. They cannot strike down a labor or environmental standard as 'excessive' if applied equally to domestic and foreign producers.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, observer,
    institutional, generational, analytical, continental).

% Capital seeking low-wage jurisdictions to minimize labor costs. Under this reading, they are not formally excluded from participating in trade, but they cannot invoke the treaty to prevent a state from raising labor standards. They would prefer capital_supremacy_reading but are kept out by the sovereignt-primacy framing.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_arbitrage_beneficiaries, excluded,
    organized, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates market access and dispute resolution across three states: guarantees tariff reduction and nondiscriminatory treatment of traded goods and services, establishes a mechanism for states to resolve trade disputes without unilateral action or tariffs, and creates predictability for capital and trade flows across borders.
% TRANSFER_FUNCTION: Moves decision-making authority from unilateral state action (tariffs, quotas, regulatory protectionism) into a coordinated framework where states commit to nondiscrimination and transparency. States accept compliance costs (documenting regulatory justifications, participating in dispute resolution) in exchange for market access and dispute predictability. Multinational capital and trade-exposed firms gain market access and regulatory certainty on non-discrimination, but do NOT gain the right to override domestic standards.
% ABSENT_VOICES: Capital interests advocating for regulatory harmonization and supremacy of capital mobility are structurally present (they can and do challenge standards in dispute panels) but are NOT in the legislative bodies that set domestic standards—domestic labor unions, environmental advocates, and health advocates are the seats represented in standard-setting. The absent voice is formal: capital loses voting power over the substance of standards (it has market power, not democratic power).
% DISAPPEARANCE_RATIONALE: If this jurisdictional reading vanished and capital_supremacy_reading took its place, states would lose the legal authority to set labor/environmental standards independent of cost-benefit analysis against trade value; capital would gain the right to challenge and overturn standards through dispute panels; investment decisions would restructure toward lowest-standard jurisdictions immediately. The regulatory landscape would harmonize downward and capital mobility would accelerate. The disappearance of sovereignty_primacy_reading means the disappearance of the legal avenue for domestic standard-setting.
% FOUNDING_PROBLEM: Post-WWII trading system required rules to prevent tariff wars and beggar-thy-neighbor policies, while preserving each state's ability to regulate labor conditions, environmental protection, and public health according to domestic values and conditions. The founding problem was: how to coordinate trade without surrendering the regulatory autonomy that societies depend on to protect workers, environments, and health?
% FOUNDING_PROBLEM_CORROBORATION: Labor movements, environmental organizations, and public-health advocates testify that the founding problem remains live: regulatory competition and capital arbitrage continue to erode standards, and the question of whether trade rules should constrain domestic authority is actively contested in legislatures and civil society. Capital-aligned commentators and some trade economists attest the founding problem is outdated and harmonization is economically necessary. Legislative testimony from affected labor and environmental constituencies (outside the benefiting capital seats) corroborates the 'live and contested' status.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28) because the coordination function is genuine and non-zero extraction is the price of dispute resolution machinery and compliance documentation. Suppression is very low (0.15) because the constraint rests on ongoing democratic ratification and legislative authority—states can amend standards or withdraw without facing coercive barriers (exit is constrained by economic costs, not legal prohibition). Theater_ratio rises modestly over time (0.12 to 0.22) because dispute panels increasingly issue rulings that domestic regulators must publicly justify (performative documentation of regulatory process increases while the substance of authority remains with domestic legislatures). The measurement grid is shared across all metrics at six time points spanning the 1994–2024 interval. The reading's extractiveness is constrained by the fact that no party is forced into the arrangement—states choose to ratify, capital chooses to participate, and both can exit (states through withdrawal, capital through market-relocation). The low suppression reflects the absence of enforcement machinery targeting domestic regulatory authority; the treaty does not authorize panels to strike down standards, so suppression is limited to economic consequences of regulatory divergence and the cost of dispute participation.
 *
 * PERSPECTIVAL GAP:
 *   From the participating_states and domestic_regulatory_agencies perspective, this is rope (pure coordination on market access with retained authority). From the multinational_capital perspective, the same constraint is extractive (they pay compliance costs and cannot arbitrage regulatory differences through treaty-supremacy claims), but still rope rather than snare because the coordination gains are real and the exit option (relocate production) is available at market-price cost. From the labor_unions and environmental_advocates perspective, the constraint is beneficial (it preserves their legislative avenue) but carries risk (regulatory arbitrage can erode standards even without treaty supremacy, and capital mobility pressure remains real). The engine computes directionality from the structural data: participating_states and regulatory_agencies get low d (beneficiaries); multinational_capital gets moderate-to-high d (payers with arbitrage exit); labor and environmental actors get moderate d (beneficiaries of the reading, but economically pressured by capital mobility). The perspectival gap is NOT a type divergence—all seats should compute as rope—because the reading's fundamental commitment is to preserve jurisdictional authority, which benefits all parties relative to capital_supremacy_reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Participating_states: d ≈ 0.25 (beneficiaries—they gain market access and dispute resolution; exit is constrained by economic integration and neighboring-state reciprocity, but formal authority remains theirs; power is institutional). Domestic_regulatory_agencies: d ≈ 0.20 (primary beneficiaries—they retain full authority; exit is analytical, power is institutional). Multinational_capital: d ≈ 0.65 (payers—they bear compliance costs, face divergent standards, and cannot use treaty to override; but arbitrage exit is available; power is institutional). Labor_unions: d ≈ 0.40 (mixed—they retain legislative authority but face capital-mobility pressure; power is organized). Trade_dispute_panels: d ≈ 0.50 (symmetric—they provide a service [dispute resolution] and bear the cost [adjudication burden]; analytical power, so baseline symmetric). The overrides section is omitted because the structural derivation (beneficiary/victim + power + exit) produces accurate directionality: no single seat is misclassified by the automatic chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory autonomy without tariff wars) remains live and contested as of 2024. The constraint has NOT experienced mandate erosion—states continue to exercise regulatory authority, and labor/environmental standards remain within domestic jurisdiction. However, the PRACTICAL EFFECT has degraded: regulatory competition and capital arbitrage have eroded standards in some jurisdictions even without treaty supremacy (the capital_supremacy_reading's pressure exerts real effects despite this reading's formal authority). The theater_ratio rise (0.12 to 0.22) indicates that dispute panels increasingly conduct substantive review of regulatory justifications, which creeps toward the capital_supremacy_reading's position (panels reviewing not just discrimination, but proportionality). This is not mandatrophy (the mandate—preserve regulatory authority—is still alive and enforced by domestic legislatures) but it IS erosion of practical scope. The constraint avoids the piton classification because the regulatory authority is genuinely exercised (it is not merely theatrical) and states continue to invest in standard-setting. The slight theater increase reflects creeping interpretive drift toward the embedded_liberalism_reading (which adds 'compatibility' requirements), not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_arbitrage_vs_authority_retention,
    'Can states formally retain regulatory authority under this reading while capital mobility and regulatory competition erode standards in practice, such that the formal authority becomes decorative?',
    'Long-term monitoring of standard trajectories across NAFTA jurisdictions: if standards converge downward despite formal authority and active domestic advocacy, the reading has retained formal authority but lost practical jurisdiction. If standards diverge and are actively defended through domestic process, the reading holds.',
    'If formal authority proves decorative under capital pressure, the reading drifts toward a piton (inertial performance of authority without real function). The classification would remain rope because no extraction accrues to a beneficiary, but the justification would degrade from ''genuine coordination'' to ''maintained fiction.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_vs_authority_retention, empirical, 'Whether formal regulatory authority survives capital arbitrage pressure without treaty supremacy.').

omega_variable(
    dispute_panel_mission_creep,
    'As trade dispute panels interpret ''non-discrimination'' requirements, do they incrementally shift toward reviewing the proportionality and rationality of standards, thereby creeping toward capital_supremacy_reading''s position?',
    'Audit of dispute panel decisions over time: track whether panels expand from narrow discrimination review to broader regulatory-proportionality assessment. Coded analysis of panel reasoning patterns.',
    'Mission creep would indicate interpretive drift from sovereignty_primacy_reading toward embedded_liberalism_reading or capital_supremacy_reading. The constraint''s claimed type would remain rope, but the effective axis would shift—extraction would rise as panels accumulate authority over substantive standards.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dispute_panel_mission_creep, empirical, 'Whether dispute-panel interpretation drifts toward regulatory-proportionality review rather than non-discrimination enforcement.').

omega_variable(
    domestic_political_capacity_for_standard_defense,
    'Under this reading, regulatory authority is real only if domestic legislatures actually invest in setting and defending standards against capital pressure. Does domestic political capacity for standard-setting remain sufficient, or has it atrophied into a formal right exercised only marginally?',
    'Comparative analysis of standard-setting activity (legislative bills, regulatory initiatives, enforcement actions) across NAFTA jurisdictions pre- and post-agreement, controlling for economic pressure and capital mobility.',
    'If domestic capacity has atrophied, the reading''s beneficiary claim (regulatory agencies and labor/environmental advocates benefit from retained authority) becomes nominal. The constraint would be reclassified as piton (authority retained but not exercised, maintained for legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_political_capacity_for_standard_defense, empirical, 'Whether domestic capacity for standard-setting has been maintained or eroded by capital mobility pressure.').

omega_variable(
    contested_reading_instantiation,
    'Is this the reading that states actually hold, or is capital_supremacy_reading the de facto interpretation even if formal law says otherwise?',
    'Institutional genealogy of dispute-panel decisions, legislative testimony on regulatory authority, and capital-allocation patterns: if capital behaves as though treaty supremacy exists (invests in challenging standards, lobbies for harmonization), while legislatures formally retain authority but defer to trade concerns, the reading gap is wide.',
    'If the de facto reading diverges from the formal reading, the constraint is experiencing an authority-erosion drift (cs_structure.drift_state = authority_erosion, magnitude = substantial). The classification would shift from rope to embedded_liberalism_reading territory (compatibility rather than primacy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_reading_instantiation, conceptual, 'Whether this sovereignty_primacy_reading reflects the actual institutional commitments or has been superseded by capital_supremacy_reading in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement_basis(naft_tr_t1994, observed).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement_basis(naft_tr_t2000, observed).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement_basis(naft_tr_t2006, observed).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement_basis(naft_tr_t2012, observed).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.23).
narrative_ontology:measurement_basis(naft_tr_t2018, observed).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(naft_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement_basis(naft_be_t1994, observed).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement_basis(naft_be_t2000, observed).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.27).
narrative_ontology:measurement_basis(naft_be_t2006, observed).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.29).
narrative_ontology:measurement_basis(naft_be_t2012, observed).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.28).
narrative_ontology:measurement_basis(naft_be_t2018, observed).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(naft_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.08).
narrative_ontology:measurement_basis(naft_su_t1994, observed).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement_basis(naft_su_t2000, observed).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2006, 0.13).
narrative_ontology:measurement_basis(naft_su_t2006, observed).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2012, 0.15).
narrative_ontology:measurement_basis(naft_su_t2012, observed).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2018, 0.16).
narrative_ontology:measurement_basis(naft_su_t2018, observed).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2024, 0.15).
narrative_ontology:measurement_basis(naft_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of the treaty's relationship to domestic regulatory authority. This story (sovereignty_primacy_reading) treats treaty obligations as subordinate to domestic law. The capital_supremacy_reading treats treaty obligations as supreme and mandatory for regulatory harmonization. The embedded_liberalism_reading accepts both constraints as compatible when standards are non-discriminatory. Each reading has distinct ε values, beneficiary structures, and type classifications. They coexist as live positions held by different institutional seats (labor/environmental advocates, capital markets, and trade judges, respectively). Network edges route the causal structure: sovereignty_primacy_reading INFLUENCES the embedded_liberalism_reading (which accepts some of its premises about regulatory authority but adds compatibility requirements); both readings are affected by mission-creep drift in capital_supremacy_reading's direction (modeled in omega variables as dispute-panel interpretation drift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
