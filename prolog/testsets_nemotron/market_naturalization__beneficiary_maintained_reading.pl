% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended by Incumbent Capital Holders
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the beneficiary_maintained_reading of
 *   the market_naturalization kernel. The reading holds that market dominance
 *   is not a natural law or a lapsed historical accident, but an actively
 *   maintained arrangement: identifiable capital holders (incumbent firms,
 *   financial intermediaries, dominant executives) deploy enforcement
 *   machinery — regulatory capture, predatory acquisitions, IP thickets,
 *   lobbying, narrative control — to suppress alternatives and extract rents.
 *   The constraint presents itself as the 'natural' outcome of competition
 *   (the efficient market hypothesis, natural monopoly narratives), but its
 *   persistence depends on active suppression of exits and alternatives. The
 *   claimed_type is tangled_rope because there IS a genuine coordination
 *   function (market order, scale economies, network effects) AND asymmetric
 *   extraction — the same structure coordinates and extracts. The
 *   hybrid_reading sees both lapsed and active elements; the
 *   lapsed_alternative_reading sees only historical inertia. This reading
 *   sees active beneficiary maintenance as the dominant dynamic.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: Primary beneficiary (institutional/arbitrage) — collect rents, control enforcement
 *   - dominant_firm_executives: Primary beneficiary (institutional/arbitrage) — manage extraction, set strategy
 *   - financial_intermediaries: Secondary beneficiary (organized/mobile) — extract fees from dominance maintenance
 *   - competing_firms: Primary victim (organized/constrained) — blocked from markets, acquired or crushed
 *   - new_entrants: Primary victim (moderate/trapped) — face insurmountable barriers
 *   - workers_displaced_by_monopsony: Primary victim (powerless/trapped) — wage suppression, reduced mobility
 *   - consumers_facing_monopoly_pricing: Primary victim (organized/constrained) — pay monopoly prices, lose choice
 *   - small_businesses_excluded_from_dominant_channels: Primary victim (moderate/trapped) — platform dependency, fee extraction
 *   - competition_authorities: Observer (institutional/analytical) — investigate but often captured
 *   - economic_orthodoxy_institutions: Agenda_setter (institutional/identity_locked) — legitimize naturalization narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.85).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended by Incumbent Capital Holders").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '064fea5b-8c62-4da7-8b56-a01ec7e219e3').
narrative_ontology:cs_kernel_codification('064fea5b-8c62-4da7-8b56-a01ec7e219e3', distributed).
narrative_ontology:cs_authority_grounding('064fea5b-8c62-4da7-8b56-a01ec7e219e3', extraction).
narrative_ontology:cs_interpretation_layer_present('064fea5b-8c62-4da7-8b56-a01ec7e219e3').
narrative_ontology:cs_reading_relation('064fea5b-8c62-4da7-8b56-a01ec7e219e3', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_reading_relation('064fea5b-8c62-4da7-8b56-a01ec7e219e3', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_axiom('064fea5b-8c62-4da7-8b56-a01ec7e219e3', foundational, dominance_requires_active_maintenance).
narrative_ontology:cs_axiom_status(dominance_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('064fea5b-8c62-4da7-8b56-a01ec7e219e3', dominance_requires_active_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('064fea5b-8c62-4da7-8b56-a01ec7e219e3', foundational, beneficiaries_are_identifiable_capital_holders).
narrative_ontology:cs_axiom_status(beneficiaries_are_identifiable_capital_holders, holdable).
narrative_ontology:cs_axiom_grounding('064fea5b-8c62-4da7-8b56-a01ec7e219e3', beneficiaries_are_identifiable_capital_holders, empirically_contingent).
narrative_ontology:cs_reference_frame('064fea5b-8c62-4da7-8b56-a01ec7e219e3', competitive_market_equilibrium).
narrative_ontology:cs_drift_state('064fea5b-8c62-4da7-8b56-a01ec7e219e3', contemporary_platform_capitalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('064fea5b-8c62-4da7-8b56-a01ec7e219e3', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, dominant_firm_executives).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, financial_intermediaries).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, competing_firms).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, new_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, workers_displaced_by_monopsony).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumers_facing_monopoly_pricing).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, small_businesses_excluded_from_dominant_channels).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, efficient_market_hypothesis_as_ideology).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, natural_monopoly_narrative).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, shareholder_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own controlling stakes in dominant firms across sectors. Collect monopoly rents, financial engineering returns, and capital gains from enforced scarcity. Deploy capital to shape regulation, acquire rivals, fund narrative production. Exit is trivial — capital is globally mobile and diversified. They set the agenda through board control, lobbying, and think-tank funding.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Manage the daily enforcement of dominance: pricing strategy, acquisition targets, regulatory engagement, narrative management. Compensated via equity tied to dominance metrics. They are the operational layer of beneficiary maintenance. Exit is mobile — executive labor market rewards dominance experience. They administer the constraint and benefit from it.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, dominant_firm_executives, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, dominant_firm_executives, agenda_setter).

% Investment banks, private equity, asset managers, and corporate law firms that structure and finance dominance maintenance: M&A advisory, leverage for buybacks, index fund voting, regulatory arbitrage. They collect fees proportional to the scale of dominance. Exit is mobile — they serve capital wherever it flows. They are secondary beneficiaries who amplify the primary beneficiaries' power.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, financial_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Established firms in adjacent or overlapping markets. Face predatory pricing, exclusive dealing, acquisition pressure, and IP litigation from dominants. Some are acquired (exit with payout); most are marginalized or forced into niche segments. Exit is constrained — they have resources to fight but not to overcome structural barriers. They bear enforcement costs (legal, compliance, lost markets).
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competing_firms, payer,
    organized, biographical, constrained, global).

% Startups and potential competitors. Face capital requirements inflated by dominant incumbents' pricing power, network effects that lock users, data advantages, and regulatory barriers shaped by incumbents. Most fail or are acquired before scaling. Exit is trapped — the only viable paths are acquisition by a dominant or niche survival. They bear the full suppression of the constraint.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, new_entrants, payer,
    moderate, immediate, trapped, global).

% Workers in labor markets dominated by few employers (tech, healthcare, logistics, retail). Face wage suppression, non-compete enforcement, algorithmic management, and reduced mobility. Exit is trapped — geographic mobility is costly, skills are specific, and the monopsony is sector-wide. They bear extraction through lower wages and worse conditions, and pay taxes that subsidize enforcement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, workers_displaced_by_monopsony, payer,
    powerless, biographical, trapped, national).

% End users of dominant platforms and products. Pay monopoly prices, surrender data, accept degraded service/quality. Organized as 'users' or 'consumers' but structurally fragmented. Exit is constrained — alternatives exist but lack network effects, interoperability, or feature parity. They bear extraction directly (prices) and indirectly (privacy, attention, reduced innovation).
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumers_facing_monopoly_pricing, payer,
    organized, biographical, constrained, global).

% Merchants, developers, creators dependent on dominant platforms (app stores, marketplaces, ad networks, search). Pay escalating fees (15–30%), accept opaque rules, face algorithmic demotion. Exit is constrained — building independent distribution is prohibitively expensive. They bear extraction as platform fees and suppression as rule changes that favor the platform's own services.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, small_businesses_excluded_from_dominant_channels, payer,
    moderate, biographical, constrained, national).

% Regulatory bodies (FTC, DOJ, EU Commission, CMA, etc.) tasked with enforcing competition law. Investigate dominance abuse, review mergers, impose remedies. Often captured intellectually (Chicago School framework) and resource-constrained. Exit is analytical — they observe and intervene but do not bear the constraint's costs or benefits directly. Their effectiveness varies by political cycle.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% Economics departments, central banks, international financial institutions (IMF, World Bank, BIS), major journals, and Nobel committees. Produce and legitimize the 'efficient markets' and 'natural monopoly' narratives that frame dominance as natural law. Tenure, funding, and prestige depend on the paradigm. Exit is identity_locked — professional identity is constituted through the framework. They set the intellectual agenda that makes active maintenance invisible.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, economic_orthodoxy_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates large-scale production, distribution, and innovation through hierarchical firms and financial markets — solves the problem of mobilizing capital and labor at scale across time and uncertainty.
% TRANSFER_FUNCTION: Moves economic surplus (rents, suppressed wages, monopoly prices, platform fees, socialized enforcement costs) from competing firms, new entrants, workers, consumers, and small businesses to incumbent capital holders, dominant executives, and financial intermediaries.
% ABSENT_VOICES: Workers in the global supply chain (especially Global South), future generations bearing ecological costs of growth-at-all-costs dominance, mutualist/cooperative enterprises that never form due to capital barriers, and the unemployed/underemployed rendered redundant by monopoly efficiency. They are structurally excluded from the conversation — no seat at the table, no organized representation in the forums where dominance is negotiated.
% DISAPPEARANCE_RATIONALE: If active maintenance of dominance vanished overnight (no predatory acquisitions, no regulatory capture, no IP thickets, no narrative enforcement), markets would fragment: new entrants would flood in, platform cooperatives would emerge, workers would gain bargaining power, prices would fall toward marginal cost, and capital would flee dominant firms for competitive opportunities. The world would rearrange dramatically — the constraint is not a natural law but an active construction.
% FOUNDING_PROBLEM: Late 19th/early 20th century: coordinating industrial production at unprecedented scale required concentrated capital, hierarchical management, and stable markets — the 'visible hand' of the corporation replaced the 'invisible hand' of the market for complex, capital-intensive goods.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiaries (incumbent capital, executives, orthodoxy) attest the problem is LIVE: scale economies, network effects, and innovation risk still require dominance. Critics (Chandlerian historians, institutional economists, labor economists, platform cooperative movement, antimonopoly revival) attest the problem is DEAD: digital coordination, distributed ledger, public infrastructure, and stakeholder governance solve the founding problem without extraction. No consensus outside the beneficiary set.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) reflects rent extraction above competitive returns plus enforcement costs socialized onto victims. Suppression (0.85) is high because alternatives are actively crushed: predatory pricing, exclusive dealing, regulatory barriers, acquisition of rivals, narrative suppression of alternatives. Theater ratio (0.32) is moderate: the 'competition' and 'innovation' narratives are real coordination functions but increasingly performative as enforcement of dominance becomes the primary activity. Accessibility collapse (0.68): alternatives exist in theory (new firms, cooperatives, public options) but collapse in practice due to capital requirements, network effects, and legal barriers. Resistance (0.72): significant — antitrust actions, worker organizing, regulatory reform attempts, alternative platform building — but fragmented and often co-opted. The measurement grid shows rising extraction and suppression over 32 periods (roughly 1990–2022), with theater slowly increasing as the coordination cover story thins.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the constraint is a Rope: market order, innovation incentives, efficient allocation — they genuinely believe the coordination story. From the victim seats, it is a Snare: extraction enforced by suppressed alternatives. The engine computes this divergence from the structural data. The claimed_type tangled_rope captures the structural truth: BOTH coordination AND extraction are real and inseparable in the same mechanism. The hybrid_reading would compute differently from the lapsed seats; the lapsed_alternative_reading would compute as Mountain or Piton from every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders and dominant executives are structural beneficiaries (d ≈ 0.1–0.2): they collect rents, control the rules, have arbitrage-grade exit (capital mobility). Financial intermediaries are secondary beneficiaries (d ≈ 0.25): they profit from the arrangement but have mobile exit. Competing firms and new entrants are targets (d ≈ 0.85–0.95): constrained exit, bear suppression costs. Workers and consumers are targets (d ≈ 0.8–0.9): trapped by monopsony/monopoly, identity-locked through consumption norms and labor market dependence. Small businesses are targets (d ≈ 0.85): constrained exit (platform dependency). Competition authorities are observers (d = 0.5): analytical seat. Economic orthodoxy institutions are agenda_setters (d ≈ 0.3): they benefit from the narrative but are partly captured by it — identity_locked through professional formation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating complex production at scale) is contested: beneficiaries claim it is live (scale economies, network effects require dominance); victims and reformers argue it is dead (digital coordination can be decentralized, platform cooperatives exist, public alternatives work). The constraint persists because the mandate (market efficiency) has been captured by the beneficiaries — the coordination function is real but the extraction has hypertrophied. Mandatrophy is NOT resolved: the arrangement has outlived its proportional justification but the beneficiaries actively prevent sunset. This is not a Piton (inertial decay) — it is actively, expensively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the market_naturalization kernel (beneficiary_maintained_reading), or does it collapse into the hybrid or lapsed readings under scrutiny?',
    'Compare beneficiary/victim structure and enforcement dynamics across the three readings. If the hybrid_reading shows both lapsed and active elements without a clear beneficiary class, and the lapsed_alternative_reading shows no active suppression, this reading is structurally distinct by its identifiable beneficiary class and active enforcement profile.',
    'If readings are not structurally distinct, the kernel decomposition is invalid and the three stories should be merged. If distinct, each reading carries its own ε and classification — the beneficiary_maintained_reading as tangled_rope, the others as their metrics dictate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of the beneficiary_maintained_reading from sibling readings of the market_naturalization kernel.').

omega_variable(
    natural_vs_constructed_dominance,
    'Is market dominance a natural outcome of competitive efficiency (Mountain) or a constructed constraint maintained by identifiable beneficiaries (Tangled Rope/Snare)?',
    'Historical analysis of entry barriers: if dominance persists after efficiency advantages erode, if incumbents lobby for regulatory barriers, if enforcement machinery (IP, lobbying, predatory pricing, acquisition) is actively deployed — the constraint is constructed. If dominance correlates with persistent efficiency differentials and no active suppression, the natural-law framing holds.',
    'If natural, claimed_type Mountain with emerges_naturally: true, low extractiveness, low suppression. If constructed, high extractiveness, high suppression, active enforcement, identifiable beneficiaries — the current metrics. This is the core ε-invariance test for this kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_dominance, empirical, 'Whether market dominance reflects natural competitive superiority or constructed extraction.').

omega_variable(
    enforcement_cost_burden_distribution,
    'Who bears the enforcement costs of maintaining market dominance — the beneficiaries (capital holders) or the victims (taxpayers, workers, competitors)?',
    'Track public expenditure on competition enforcement, regulatory capture, IP litigation subsidies, and bailout exposures. If enforcement costs are socialized while rents are privatized, extraction is higher than the commission rate alone suggests.',
    'If enforcement costs are socialized, effective extraction for victims increases (they pay for their own suppression). If beneficiaries bear full enforcement costs, the constraint may be a Rope with genuine coordination function (market order) at a price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_burden_distribution, empirical, 'Distribution of enforcement costs between beneficiaries and victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_naturalization_beneficiary_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(market_naturalization_beneficiary_tr_t8, market_naturalization__beneficiary_maintained_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(market_naturalization_beneficiary_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(market_naturalization_beneficiary_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(market_naturalization_beneficiary_tr_t32, market_naturalization__beneficiary_maintained_reading, theater_ratio, 32, 0.32).

% Extraction over time
narrative_ontology:measurement(market_naturalization_beneficiary_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(market_naturalization_beneficiary_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(market_naturalization_beneficiary_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(market_naturalization_beneficiary_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(market_naturalization_beneficiary_be_t32, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 32, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(market_naturalization_beneficiary_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(market_naturalization_beneficiary_su_t8, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(market_naturalization_beneficiary_su_t16, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(market_naturalization_beneficiary_su_t24, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(market_naturalization_beneficiary_su_t32, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 32, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.15).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, intellectual_property_regime).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, financial_regulation_capture).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, labor_market_monopsony).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, platform_governance_extraction).

% DUAL FORMULATION NOTE:
% The market_naturalization kernel decomposes into three readings with distinct ε and structural profiles: (1) beneficiary_maintained_reading — active enforcement by identifiable capital holders, high ε, tangled_rope; (2) hybrid_reading — mixed lapsed/active, contested beneficiary structure, likely tangled_rope or snare depending on measurement; (3) lapsed_alternative_reading — no active enforcement, low ε, likely piton or mountain. The decomposition follows the ε-invariance principle: each reading has a stable ε when assessed by its own lights on the same referent (the standing arrangement of market dominance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, institutional, 0.3).
constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, organized, 0.75).
constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, moderate, 0.85).
constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
