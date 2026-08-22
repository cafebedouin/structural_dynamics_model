% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Climate Adaptation-Priority Response Framework
 *   domain: climate_policy/political_economy/intergenerational
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation-priority reading of the
 *   climate response kernel. It describes a policy framework that treats
 *   temperature rise as inevitable, prioritizes capital-intensive adaptive
 *   infrastructure, and channels climate response through multilateral
 *   finance institutions. The reading is not authored as a claim about
 *   climate physics (warming is factually locked in) but as a policy choice:
 *   acceptance of that warming as the operational baseline, combined with
 *   prioritization of adaptation spending over emissions reductions. This
 *   reading produces a tangled-rope structure because it solves a genuine
 *   coordination problem (allocating global adaptation resources) while
 *   extracting asymmetrically from vulnerable nations, which bear adaptation
 *   costs without having caused the emissions and face debt obligations from
 *   concessional finance. The enforcement is active because the policy
 *   framework must exclude competing framings (mitigation-priority, degrowth)
 *   from high-income policy boards and multilateral governance to remain
 *   operational.
 *
 * KEY AGENTS:
 *   - High-income nations: set policy toward adaptation, control multilateral boards, benefit from infrastructure contracting
 *   - Low-income nations: face adaptation burden with limited fiscal capacity, trapped into concessional debt, excluded from policy-setting
 *   - Climate-vulnerable populations: identity-locked to place, bear protection disparities, carry costs of adaptation failure
 *   - Infrastructure contractors & finance intermediaries: benefit from $540B+ adaptation market, operate at arm's length from outcomes
 *   - Future generations: inherit higher-warming baseline and depleted adaptive capacity (aquifer depletion, forest clearance)
 *   - Mitigation and degrowth advocates: structurally excluded from policy boards, their objections not integrated into framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Climate Adaptation-Priority Response Framework").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '9c54a30b-4579-4620-92b5-074f84932863').
narrative_ontology:cs_kernel_codification('9c54a30b-4579-4620-92b5-074f84932863', formalized).
narrative_ontology:cs_authority_grounding('9c54a30b-4579-4620-92b5-074f84932863', extraction).
narrative_ontology:cs_interpretation_layer_present('9c54a30b-4579-4620-92b5-074f84932863').
narrative_ontology:cs_reading_relation('9c54a30b-4579-4620-92b5-074f84932863', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('9c54a30b-4579-4620-92b5-074f84932863', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('9c54a30b-4579-4620-92b5-074f84932863', foundational, temperature_rise_inevitable).
narrative_ontology:cs_axiom_status(temperature_rise_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('9c54a30b-4579-4620-92b5-074f84932863', temperature_rise_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('9c54a30b-4579-4620-92b5-074f84932863', foundational, capital_intensive_adaptation_sufficient).
narrative_ontology:cs_axiom_status(capital_intensive_adaptation_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('9c54a30b-4579-4620-92b5-074f84932863', capital_intensive_adaptation_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('9c54a30b-4579-4620-92b5-074f84932863', secondary, multilateral_finance_allocation_legitimate).
narrative_ontology:cs_axiom_status(multilateral_finance_allocation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9c54a30b-4579-4620-92b5-074f84932863', multilateral_finance_allocation_legitimate, conventional).
narrative_ontology:cs_reference_frame('9c54a30b-4579-4620-92b5-074f84932863', inevitable_warming_requiring_adaptive_capital).
narrative_ontology:cs_drift_state('9c54a30b-4579-4620-92b5-074f84932863', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9c54a30b-4579-4620-92b5-074f84932863', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, high_income_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, adaptive_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, climate_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, low_income_nations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations_under_warming).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set climate policy toward adaptation as primary response, framing inevitable warming as requiring infrastructure and financial instruments. Control multilateral development bank boards and climate finance channels. Benefit from positioning adaptation finance as requiring technological solutions (dams, coastal barriers, insurance products) in which they have supply and service advantages. Avoid the domestic emissions reductions and economic restructuring that mitigation or degrowth would mandate.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, high_income_nations, agenda_setter,
    institutional, generational, arbitrage, global).

% Must adapt to climatic impacts on agricultural systems, freshwater, and coastal settlements with limited fiscal capacity. Expected to raise $350B annually (the financing gap) from domestic budgets or concessional loans when high-income nations contributed to atmospheric CO2 and promised climate finance never materializes at the scale committed. Adaptation investments crowd out health, education, and poverty reduction. Warming accepted as inevitable under this reading means they bear adaptation costs while mitigation (which would reduce future warming) is deferred.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, low_income_nations, payer,
    powerless, generational, trapped, global).

% Indigenous communities, subsistence farmers, small-island dwellers, coastal poor. Adaptation infrastructure is designed at national or regional scales; protection disparities emerge between wealthy urban centers and rural/informal settlements. Identity is geographically rooted (small-island nationality, pastoral livelihoods). Cannot exit because exit means abandoning livelihoods, ancestral lands, and cultural identity. Carry the costs of adaptation failures (climate migration, displacement, livelihood collapse) while high-income nations' adaptation preserves comfortable living standards.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, identity_locked, global).

% International engineering firms, construction companies, and technology vendors (coastal barrier manufacturers, flood management systems, precision agriculture suppliers). Adaptation spending creates a $540B+ annual market. Benefit from contracts for resilience infrastructure, climate-proof buildings, and water management systems. Operate globally; project-by-project mobility means they absorb no long-term risk of adaptation failure in any given jurisdiction.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, adaptive_infrastructure_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Multilateral Development Banks, bilateral aid agencies, green investment funds, carbon credit traders. Channel adaptation finance and collect administrative fees, intermediation margins, and interest on concessional loans. Adaptation-priority framing sustains their institutional role and budget justification. Growth of adaptation finance expands their mandate and fiscal base. Operate at arm's length from outcomes (if adaptation fails, the borrowing nation is blamed; if adaptation succeeds, the intermediary claims credit).
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_finance_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Born into a higher-warming world because this reading accepts temperature rise as inevitable and defers emissions reductions. Adaptation can slow some impacts but cannot prevent: sea-level rise exceeding protection timescales, ecosystem collapse, agricultural productivity decline, and compound extremes. They inherit depleted natural systems (aquifer depletion through adaptation irrigation, forest cleared for adaptive agriculture) and a climate baseline requiring continuous, expensive adaptation to maintain 1990s-baseline human welfare. Cannot exit because they have no pre-warming world to return to.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations_under_warming, payer,
    powerless, civilizational, trapped, universal).

% Scientific bodies (IPCC, climate research institutions), some environmental movements, and nations committed to net-zero-by-2050 targets. Would argue that accepting temperature rise as inevitable is self-fulfilling and forecloses the emissions-reduction pathway that would minimize adaptation need. Excluded from high-income policy boards where adaptation-priority framing is established as operational doctrine; their input is solicited on technical questions ('how high can we go?') but not on strategic direction ('should we accept warming as inevitable?').
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, mitigation_priority_advocates, excluded,
    organized, generational, constrained, global).

% Academics, policy networks, and movements arguing that climate response requires structural economic contraction in high-income nations and resource-use reduction globally. Would argue that adaptation-priority and mitigation-priority are both high-consumption framings: both assume capacity to invest billions in infrastructure. Excluded from high-income policy boards; their analysis appears in journal articles and NGO reports but does not shape state-level climate policy.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, degrowth_transformation_advocates, excluded,
    moderate, generational, constrained, global).

% Sees the full structure: how adaptation-priority framing solves the coordination problem of resource allocation (who pays, who decides, who benefits) by naturalizing temperature rise and converting climate response into a finance and infrastructure question rather than a production/consumption question. Observes that the arrangement is enforced through control of multilateral development bank voting, climate finance channel design, and the exclusion of alternative framings from policy boards.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, high_income_nations).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates global climate response resources to adaptation infrastructure and capacity-building: coordinating investment in flood management, drought-resistant agriculture, cooling systems, and insurance mechanisms. Establishes a framework for channeling climate finance through multilateral institutions with standardized project criteria, reducing transaction costs and enabling large-scale investment.
% TRANSFER_FUNCTION: Moves capital and labor from high-income-nation treasuries and private capital markets toward infrastructure contracts in vulnerable regions, channeled through multilateral development banks and bilateral aid agencies. Moves debt from vulnerable nations to international creditors (in the form of concessional loans for adaptation). Moves management authority over adaptation priorities from vulnerable nations to international technical committees dominated by high-income nations and their contractors.
% ABSENT_VOICES: Mitigation-priority advocates and degrowth voices are structurally excluded. Mitigation advocates argue the framework forecloses emissions-reduction investment; degrowth advocates argue it presumes indefinite capital availability and avoids the consumption-reduction question. Both would change the policy architecture fundamentally; their exclusion is maintained through control of UNFCCC negotiating delegations, IPCC Working Group II framing (adaptation) vs. Working Group III (mitigation) compartmentalization, and multilateral development bank governance where voting power is proportional to capital contributions (high-income-nation-weighted).
% DISAPPEARANCE_RATIONALE: If adaptation-priority framing and its enforcement disappeared, climate response would shift toward mitigation and emissions reduction (competing reading) or degrowth restructuring (alternative reading). The $540B annual adaptation finance would reorient toward renewable energy infrastructure, demand-side reduction, or industrial transformation in high-income nations. Vulnerable nations would face climate impacts without the adaptive infrastructure this reading funds, but also without debt obligations from concessional loans. The institutional landscape of multilateral development banks and bilateral climate finance would reorganize around different policy priorities.
% FOUNDING_PROBLEM: Atmospheric CO2 concentration is rising; warming of 1.1°C above pre-industrial has already locked in further warming to ~1.5°C; unilateral mitigation efforts are insufficient to prevent 2°C+ warming on current trajectories. Vulnerable nations and populations face imminent climate impacts (flood, drought, hurricane intensification) regardless of mitigation success. Adaptation infrastructure can reduce mortality and livelihood disruption from those inevitable impacts.
% FOUNDING_PROBLEM_CORROBORATION: Climate research institutions (IPCC, NOAA, UK Met Office) and vulnerable-nation governments attest the founding problem is live: warming is locked in and impacts are arriving. High-income-nation governments attest adaptation is therefore necessary. Mitigation advocates and degrowth researchers attest the problem is mis-stated: the claim 'warming is inevitable' treats the current emissions trajectory as fixed when it is policy-determined; accepting inevitability is a choice, not a scientific finding. Independent researchers (Romm, Holz, climate modelers at GFDAE/Princeton) corroborate: on current policy, warming approaches 3°C and adaptation alone cannot protect vulnerable populations at that scale.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval because the financing gap ($350B annually) hardens as vulnerable nations accumulate adaptation debt and the pattern of unmet commitments becomes structural rather than accidental. Theater ratio remains low (0.12→0.28) because the adaptation projects are functionally real — dams and irrigation systems serve genuine needs — but the share of effort devoted to maintaining the policy framework (keeping mitigation off the agenda, excluding alternative framings, managing debt service) grows as resistance mounts. Suppression is moderate-to-high (0.48→0.62) because the framework relies on controlling multilateral governance structures and UNFCCC negotiation delegations; vulnerable nations cannot exit or change terms, but mitigation advocates retain scientific credibility and some institutional platforms (IPCC, universities), so suppression never reaches the near-total that a snare would require. The time grid is one shared set of time points (0, 5, 10, 15, 20, 25, 30, 40) where every metric is authored for consistency.
 *
 * PERSPECTIVAL GAP:
 *   Payers and beneficiaries experience this constraint in opposite modal registers. From high-income nations' and contractors' positions, adaptation-priority is a solution: it allocates responsibility (vulnerable nations adapt), distributes capital (through finance channels they control), and creates markets (infrastructure contracts). From low-income and vulnerable-population seats, the same structure is coercive: it forecloses alternative framings (mitigation, degrowth), locks them into debt, and offers protection that is unequal (wealthy cities protected, rural settlements underprotected). The engine computes this divergence from power/exit_options/directionality; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income nations are beneficiaries with institutional power and arbitrage exit (they can shift policy, change allocation, move capital). Low-income nations are victims with powerless position and trapped exit (they cannot change terms, cannot exit climate vulnerability, cannot shift to alternative responses without high-income cooperation). Climate-vulnerable populations are trapped identity-locked victims: geographic rootedness and livelihood dependence make exit tantamount to losing identity. Contractors and finance intermediaries are beneficiaries with powerful position and mobile exit (project completion ends their obligation; they operate globally). Future generations are powerless, civilizationally time-horizoned, trapped victims with universal scope — the most asymmetric position. Directionality for high-income institutional actors derives toward 0.1–0.3 (beneficiary damping); for low-income institutional actors toward 0.7–0.9 (target amplification); for powerless individuals toward 0.85+ (trapped identity-locked target maximum).
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptation-priority framework solves a genuine coordination problem: how to allocate finite global resources for climate response and establish legitimate decision-making authority. The mandatrophy risk is that the founding problem (climate impacts are arriving; adaptation is necessary) is conflated with a policy choice (accept warming as inevitable; prioritize adaptation over mitigation). If mitigation becomes technically feasible at lower cost than adaptation, or if political will for emissions reduction emerges, the policy framework persists anyway because the institutional infrastructure (multilateral development banks, adaptation finance channels, contractor relationships) is now lock-in. The framework prevents mandatrophy resolution by excluding the framings that would detect the shift (mitigation advocates cannot reach policy tables; if they could, they would argue that emissions reduction now costs less than adaptation and should be prioritized). The theater ratio rising from 0.12 to 0.28 marks the increasing share of effort devoted to maintaining the framework rather than functional adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_policy_choice,
    'Is temperature rise above 2°C an inevitable physical fact given current atmosphere composition, or a policy-choice consequence of accepting current emissions trajectories?',
    'Distinguish between: (a) locked-in warming from past emissions (irreversible on decadal timescales), and (b) additional warming from future emissions (reversible by policy change). The adaptation-priority reading conflates these, treating (a)+(b) as (a)—inevitable. If policy shifts sufficiently to reduce (b) to near-zero, adaptation is still necessary for (a), but the frame of ''accepting inevitability'' becomes visible as a choice rather than a constraint.',
    'If resolved as policy-choice: the constraint shifts from mountain-like (fixed reality) toward tangled_rope or snare (enforced arrangement). Mitigation-priority framing would enter policy boards. If resolved as inevitable: the constraint''s tangled_rope classification is confirmed (genuine coordination need for adaptation, but asymmetric extraction from vulnerable regions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inevitability_vs_policy_choice, empirical, 'Whether ''inevitable warming'' is physics or policy.').

omega_variable(
    adaptation_sufficiency_boundary,
    'At what warming threshold does adaptation become technically or economically infeasible at the scale required to protect vulnerable populations?',
    'Climate impact modeling: coral ecosystem collapse (1.5°C threshold), agricultural productivity decline (2-3°C), compound extremes frequency (3-4°C+). Adaptation cost projections at different warming levels. Field trials in adaptation-prioritizing regions: can infrastructure sustain livelihoods at 3°C+, or does adaptation fail and populations face displacement regardless?',
    'If adaptation remains feasible to 3°C+: the adaptation-priority frame holds and vulnerable populations bear adaptation costs indefinitely. If adaptation fails at 2.5°C: the frame becomes a zombie — the founding problem (protect vulnerable populations) cannot be solved by the prescribed solution (adaptation infrastructure), and the constraint shifts toward piton (maintained for institutional inertia, not function). If adaptation fails early, mitigation or degrowth framings enter via failure-driven policy revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_boundary, empirical, 'Physical and economic limits of adaptive capacity.').

omega_variable(
    financing_gap_closure,
    'Will the $350B annual financing gap (actual vs. committed climate finance) close, or harden as a structural feature of the arrangement?',
    'Track multilateral development bank disbursements, bilateral climate finance flows, and vulnerable-nation domestic adaptation spending against the UNFCCC/Paris Agreement commitments. If gap remains ~$350B for 5+ years, the gap is structural (enforcement: high-income nations are not paying). If gap narrows to <$100B, the coordination function is improving.',
    'Structural financing gap (gap remains) → the arrangement is extractive by design: vulnerable nations are expected to fund most adaptation themselves while high-income nations retain adaptation finance control and intermediation. Closure (gap narrows) → the coordination function improves and the tangled_rope classification persists but with lower asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financing_gap_closure, empirical, 'Whether the financing gap is incidental or structural.').

omega_variable(
    competing_reading_exclusion_mechanism,
    'Is the exclusion of mitigation-priority and degrowth-reading advocates from high-income policy boards maintained by structural institutions (voting power, funding control) or by discursive suppression (they are heard but not heeded)?',
    'Audit high-income climate policy boards (national delegations, multilateral development bank governance, UNFCCC negotiation committees) for representation of mitigation and degrowth advocates. If they are absent or token: structural exclusion. If present and active but outvoted: discursive suppression. Monitor whether research funding for mitigation or degrowth policy reaches policy-shaping platforms.',
    'Structural exclusion → the adaptation-priority framework''s enforcement is active and institutional; changing policy would require governance reform. Discursive suppression → the framework is vulnerable to intellectual-shift events (consensus change, crisis-triggered re-evaluation). High-income nation capacity to suppress competing framings is the mechanism that maintains adaptation-priority as policy. If suppression breaks, the frame is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_reading_exclusion_mechanism, empirical, 'Exclusion is institutional or rhetorical.').

omega_variable(
    north_south_power_asymmetry_stability,
    'Is the North-South power asymmetry in climate finance governance (high-income nations control multilateral boards, set adaptation criteria) maintained indefinitely, or do emerging economies and coalition-building by vulnerable nations gradually shift power?',
    'Monitor voting reforms in multilateral development banks, emerging economies'' creation of alternative financing vehicles (New Development Bank, AIIB), and vulnerable-nation coalition strength in UNFCCC negotiations. If alternative finance grows faster than multilateral development bank funding, power is shifting.',
    'If asymmetry persists: extraction remains structural and adaptation-priority framing persists under high-income control. If power shifts: vulnerable nations gain agenda-setting capacity and can reframe climate response (prioritize mitigation differently, demand emissions reductions from high-income nations, shift adaptation toward community-led solutions). A shift would destabilize the reading and pull toward mitigation or degrowth framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_south_power_asymmetry_stability, empirical, 'Whether North-South power asymmetry in climate governance is stable or shifting.').

omega_variable(
    kernel_foreclosure_scope,
    'Does the adaptation-priority reading foreclose mitigation-priority and degrowth-transformation readings within any single policy framework, or do the readings coexist as competing live positions?',
    'Examine whether a high-income nation could simultaneously pursue adaptation-priority AND mitigation-priority (yes: they can build renewables and adaptation simultaneously). Whether adaptation-priority is compatible with degrowth (no: adaptation assumes capital availability; degrowth requires contraction). If adaptation-priority forecloses degrowth but coexists with mitigation-priority, the reading_relations should be: forecloses=[degrowth], coexists_with=[mitigation].',
    'If adaptation-priority forecloses degrowth: degrowth is outside the policy space under this framework. Degrowth advocates are truly excluded. If adaptation-priority and mitigation coexist: policy disputes are about funding allocation, not frame choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_scope, conceptual, 'Logical structure of the kernel dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_response_action__adaptation_priority, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__adaptation_priority, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__adaptation_priority, theater_ratio, 15, 0.22).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__adaptation_priority, theater_ratio, 20, 0.25).
narrative_ontology:measurement(clim_tr_t25, climate_response_action__adaptation_priority, theater_ratio, 25, 0.27).
narrative_ontology:measurement(clim_tr_t30, climate_response_action__adaptation_priority, theater_ratio, 30, 0.28).
narrative_ontology:measurement(clim_tr_t40, climate_response_action__adaptation_priority, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_response_action__adaptation_priority, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_response_action__adaptation_priority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t15, climate_response_action__adaptation_priority, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(clim_be_t20, climate_response_action__adaptation_priority, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(clim_be_t25, climate_response_action__adaptation_priority, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_response_action__adaptation_priority, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(clim_be_t40, climate_response_action__adaptation_priority, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(clim_su_t5, climate_response_action__adaptation_priority, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(clim_su_t10, climate_response_action__adaptation_priority, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(clim_su_t15, climate_response_action__adaptation_priority, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(clim_su_t20, climate_response_action__adaptation_priority, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(clim_su_t25, climate_response_action__adaptation_priority, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(clim_su_t30, climate_response_action__adaptation_priority, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(clim_su_t40, climate_response_action__adaptation_priority, suppression_requirement, 40, 0.62).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_grid_01, climate_response_action__adaptation_priority, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(clim_grid_02, climate_response_action__adaptation_priority, accessibility_collapse(class), 40, 0.74).
narrative_ontology:measurement(clim_grid_03, climate_response_action__adaptation_priority, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(clim_grid_04, climate_response_action__adaptation_priority, accessibility_collapse(individual), 40, 0.81).
narrative_ontology:measurement(clim_grid_05, climate_response_action__adaptation_priority, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(clim_grid_06, climate_response_action__adaptation_priority, accessibility_collapse(organizational), 40, 0.61).
narrative_ontology:measurement(clim_grid_07, climate_response_action__adaptation_priority, accessibility_collapse(structural), 0, 0.42).
narrative_ontology:measurement(clim_grid_08, climate_response_action__adaptation_priority, accessibility_collapse(structural), 40, 0.58).
narrative_ontology:measurement(clim_grid_09, climate_response_action__adaptation_priority, resistance(class), 0, 0.75).
narrative_ontology:measurement(clim_grid_10, climate_response_action__adaptation_priority, resistance(class), 40, 0.81).
narrative_ontology:measurement(clim_grid_11, climate_response_action__adaptation_priority, resistance(individual), 0, 0.68).
narrative_ontology:measurement(clim_grid_12, climate_response_action__adaptation_priority, resistance(individual), 40, 0.74).
narrative_ontology:measurement(clim_grid_13, climate_response_action__adaptation_priority, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(clim_grid_14, climate_response_action__adaptation_priority, resistance(organizational), 40, 0.79).
narrative_ontology:measurement(clim_grid_15, climate_response_action__adaptation_priority, resistance(structural), 0, 0.62).
narrative_ontology:measurement(clim_grid_16, climate_response_action__adaptation_priority, resistance(structural), 40, 0.68).
narrative_ontology:measurement(clim_grid_17, climate_response_action__adaptation_priority, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(clim_grid_18, climate_response_action__adaptation_priority, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(clim_grid_19, climate_response_action__adaptation_priority, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(clim_grid_20, climate_response_action__adaptation_priority, stakes_inflation(individual), 40, 0.72).
narrative_ontology:measurement(clim_grid_21, climate_response_action__adaptation_priority, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(clim_grid_22, climate_response_action__adaptation_priority, stakes_inflation(organizational), 40, 0.48).
narrative_ontology:measurement(clim_grid_23, climate_response_action__adaptation_priority, stakes_inflation(structural), 0, 0.31).
narrative_ontology:measurement(clim_grid_24, climate_response_action__adaptation_priority, stakes_inflation(structural), 40, 0.44).
narrative_ontology:measurement(clim_grid_25, climate_response_action__adaptation_priority, suppression(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_26, climate_response_action__adaptation_priority, suppression(class), 40, 0.67).
narrative_ontology:measurement(clim_grid_27, climate_response_action__adaptation_priority, suppression(individual), 0, 0.54).
narrative_ontology:measurement(clim_grid_28, climate_response_action__adaptation_priority, suppression(individual), 40, 0.68).
narrative_ontology:measurement(clim_grid_29, climate_response_action__adaptation_priority, suppression(organizational), 0, 0.41).
narrative_ontology:measurement(clim_grid_30, climate_response_action__adaptation_priority, suppression(organizational), 40, 0.55).
narrative_ontology:measurement(clim_grid_31, climate_response_action__adaptation_priority, suppression(structural), 0, 0.38).
narrative_ontology:measurement(clim_grid_32, climate_response_action__adaptation_priority, suppression(structural), 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.22).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is part of the climate_response_action kernel family decomposed into three distinct readings. Each reading holds different axioms about climate response requirements and produces different beneficiary/victim structures. ε values differ substantially across readings because each reading measures the standing arrangement under contest from its own epistemic vantage: adaptation-priority reading measures the current multilateral-finance-led adaptation framework (high ε from vulnerable-nation perspective, low from high-income-nation perspective); mitigation-priority reading measures emissions-reduction investment frameworks; degrowth-transformation reading measures growth-dependent economic structures. The three readings are not variations of a single constraint; they are competing framings of what climate response means. They are linked via network.affects_constraints because upstream framings (what counts as 'climate response') constrain downstream policy options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__adaptation_priority, institutional, 0.25).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
