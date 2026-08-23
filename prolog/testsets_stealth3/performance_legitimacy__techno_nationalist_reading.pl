% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Reading of Performance Legitimacy: Strategic Self-Sufficiency Mandate
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A developmental party-state stakes its ruling legitimacy on a specific
 *   performance claim: that it will deliver technological self-sufficiency in
 *   chokepoint industries and durable leadership in the frontier technologies
 *   that constitute great-power rank. This reading of the
 *   performance-legitimacy kernel organizes a vast directed-investment
 *   apparatus — policy-bank credit at administered rates, procurement
 *   preferences, export-control and import-substitution programs, talent
 *   mobilization — that overrides market signals wherever strategic
 *   priorities conflict with commercial ones. The arrangement solves a
 *   genuine collective-action problem: no private firm can internalize the
 *   national-security externality of supply dependence, and rivals have
 *   demonstrated willingness to weaponize chokepoints. But the same structure
 *   moves resources asymmetrically: household savings are channeled into
 *   strategic lending at below-market returns, consumer services and
 *   non-strategic manufacturers are crowded out of capital and labor, and
 *   allocation decisions are made in planning councils from which the paying
 *   seats are absent. Per the epsilon-invariance principle this file
 *   instantiates ONLY the techno-nationalist reading; the
 *   quantitative-growth, qualitative-development, and livelihood-security
 *   readings are separate constraints with their own epsilon values, linked
 *   through network.affects_constraints. The claim/metrics split is
 *   deliberate: claimed_type is my structural judgment (genuine security
 *   coordination entangled with asymmetric transfer); the metrics describe
 *   observed operation and are not tuned to the claim.
 *
 * KEY AGENTS:
 *   - techno_industrial_planning_apparatus: agenda-setter (institutional/identity_locked) — drafts the strategic plans, steers credit, administers export controls; its budget and standing expand with the mission
 *   - national_champions: primary beneficiary (powerful/arbitrage) — receive subsidized capital, protected procurement, and forbearance; net collectors of the transfer, though burdened by mandates
 *   - defense_industrial_base: secondary beneficiary (organized/constrained) — guaranteed demand and preferential inputs; locked into the national program as its customer
 *   - household_savers_consumers: primary target (powerless/trapped) — savings captured by the domestic financial system fund the buildout; consumption share compressed; no seat in allocation decisions
 *   - consumer_services_sectors: target (moderate/constrained) — bid for labor and capital against policy-favored rivals with cheaper credit
 *   - nonstrategic_private_manufacturers: target (moderate/trapped) — credit-starved outside the strategic list; partially absorbed as tier-2 vendors to the champions
 *   - market_oriented_reformers: excluded voice (moderate/identity_locked) — argue for market allocation from inside the apparatus; marginalized after each security crisis
 *   - international_policy_analysts: analytical observer (analytical/analytical) — track subsidy flows, milestone credibility, and consumption-share divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.65).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Reading of Performance Legitimacy: Strategic Self-Sufficiency Mandate").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '713be4c5-209a-443a-827d-4cf921b85cb5').
narrative_ontology:cs_kernel_codification('713be4c5-209a-443a-827d-4cf921b85cb5', implicit).
narrative_ontology:cs_authority_grounding('713be4c5-209a-443a-827d-4cf921b85cb5', practice).
narrative_ontology:cs_interpretation_layer_present('713be4c5-209a-443a-827d-4cf921b85cb5').
narrative_ontology:cs_reading_relation('713be4c5-209a-443a-827d-4cf921b85cb5', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('713be4c5-209a-443a-827d-4cf921b85cb5', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('713be4c5-209a-443a-827d-4cf921b85cb5', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('713be4c5-209a-443a-827d-4cf921b85cb5', foundational, national_security_requires_technological_self_sufficiency).
narrative_ontology:cs_axiom_status(national_security_requires_technological_self_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('713be4c5-209a-443a-827d-4cf921b85cb5', national_security_requires_technological_self_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('713be4c5-209a-443a-827d-4cf921b85cb5', foundational, great_power_rank_is_frontier_industry_position).
narrative_ontology:cs_axiom_status(great_power_rank_is_frontier_industry_position, holdable).
narrative_ontology:cs_axiom_grounding('713be4c5-209a-443a-827d-4cf921b85cb5', great_power_rank_is_frontier_industry_position, conventional).
narrative_ontology:cs_reference_frame('713be4c5-209a-443a-827d-4cf921b85cb5', strategic_self_sufficiency_standard).
narrative_ontology:cs_drift_state('713be4c5-209a-443a-827d-4cf921b85cb5', contemporary_sanctions_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('713be4c5-209a-443a-827d-4cf921b85cb5', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, techno_industrial_planning_apparatus).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, household_savers_consumers).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_services_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, nonstrategic_private_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, chokepoint_weaponization_thesis).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, frontier_leadership_constitutes_great_power_rank).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the periodic strategic-industry plans, directs policy-bank lending at administered rates, maintains the export-control and procurement-preference lists, and runs talent-mobilization programs. Its budget, staffing, and bureaucratic rank have grown with each security crisis that vindicates the mission. Leaving would mean dismantling the mandate the institution has become; careers inside it are built on the strategic frame.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, techno_industrial_planning_apparatus, agenda_setter,
    institutional, generational, identity_locked, continental).

% Receive below-market credit from policy banks, protected procurement, land and power allocations, and regulatory forbearance in exchange for capacity commitments and political accountability. They also carry binding output targets, price disciplines in designated categories, and leadership exposure when flagships miss milestones. Many list offshore and sell globally, giving them more room to maneuver than any other domestic seat, but abandoning the national program would forfeit the subsidy stream and invite political reprisal.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, national_champions, payer).

% Operates with guaranteed state demand, priority access to scarce materials and engineering talent, and cost-plus contracting. Its order book depends on the strategic program continuing; serving commercial markets alone would leave it undersized and exposed.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_industrial_base, beneficiary,
    organized, generational, constrained, national).

% Supply the savings that fund the strategic buildout through the domestic banking system, earning administered deposit rates below market-clearing levels while asset alternatives abroad are closed by capital controls. Consumption takes a smaller share of national income than in peer economies at similar income levels. Individually they have no seat in allocation decisions; collectively their main lever is precautionary saving, which slows the economy but also shrinks their own incomes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, household_savers_consumers, payer,
    powerless, biographical, trapped, national).

% Restaurants, retail, logistics, and domestic services compete for workers and premises against policy-favored sectors that borrow cheaper and expand on mandates rather than margins. They pay market rates for capital in a system where rivals do not, and their growth tracks the consumption share the strategic program compresses.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_services_sectors, payer,
    moderate, biographical, constrained, national).

% Fall outside the strategic list and face the tightest credit conditions in the system, compliance burdens sized for larger firms, and market segments increasingly served by champions expanding beyond their mandates. Some survive by becoming tier-2 suppliers to the very champions that crowd them, absorbing themselves into the structure that displaces them; relocating abroad is blocked by the same capital controls that trap household savings.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, nonstrategic_private_manufacturers, payer,
    moderate, biographical, trapped, national).

% Economists and senior officials inside the system who argue for market-determined pricing, consumption-led rebalancing, and winding down directed credit. Each security crisis that validates the strategic frame pushes them further from planning councils, yet their careers, pensions, and networks are embedded in the apparatus they criticize; exit would mean leaving the country or silence.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_oriented_reformers, excluded,
    moderate, biographical, identity_locked, national).

% Academic and multilateral researchers who track subsidy magnitudes, verify self-sufficiency milestones against physical output, and compare consumption shares across peer economies. They publish outside the regime's control and are read by foreign governments more often than by the planners they assess.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the national-security collective-action problem of chokepoint dependence: individual firms cannot internalize the cost of supply-chain weaponization by rivals, private discount rates cannot fund decades-long frontier-industry maturation, and redundant domestic capacity in critical inputs is a public good no firm will oversupply. The state coordinates patient capital formation, talent pipelines, and demand guarantees at a scale and horizon markets do not generate.
% TRANSFER_FUNCTION: Moves loanable capital (largely household deposits at administered rates), engineering talent, energy and land quotas, and procurement demand from households, consumer services, and non-strategic manufacturing toward strategic-technology sectors and national champions; moves legitimacy-relevant performance claims upward to the ruling apparatus.
% ABSENT_VOICES: Market-oriented reformers hold the market-allocation case but are marginalized in planning councils; households have no formal representation in allocation decisions and learn of priority shifts through prices and credit conditions; foreign technology suppliers facing import substitution are outside the domestic conversation entirely, though their governments answer through counter-controls. Unanimity around the strategic frame arises partly because the seats that would dispute it were never in the room.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement machinery vanished overnight, credit would reprice to commercial risk, champions would shed mandated capacity and reprice products, consumer and household income shares would rebound as lending normalized, export controls would relax toward commercial reciprocity, and the regime would need a replacement legitimacy claim, most plausibly migrating to a sibling reading of the same kernel. Sectoral fortunes, career structures, regional economies built around flagship projects, and the security posture would all rearrange.
% FOUNDING_PROBLEM: Sanctions and export-control episodes demonstrated that commercial supply dependencies on rival states convert into coercive leverage precisely when relations deteriorate; simultaneously, status competition among great powers had come to be decided by frontier-industry position. The arrangement was built to eliminate the dependency exposure and secure the frontier position before a crisis forced the issue.
% FOUNDING_PROBLEM_CORROBORATION: Externally corroborated in part: rival governments' own export-control actions against this regime's firms document that chokepoint coercion is real — the vulnerability is attested by adversary behavior, not self-assertion — and independent security studies confirm the dependency concentrations. But the claim that massive directed investment is the necessary and sufficient remedy is attested mainly by the planning apparatus and the champion sectors that receive the flows; market-oriented economists outside the beneficiary set dispute the remedy's necessity and scale. No corroborator outside the benefiting parties attests the specific remedy; the vulnerability is corroborated, the remedy is not.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the transfer is large and asymmetric — administered-rate household deposits on-lent to favored sectors, procurement preferences, factor-cost distortions — but partially offset by genuine public-good outputs (redundant critical capacity, employment concentration in strategic regions, spillovers). Suppression 0.62: persistence depends on active machinery — credit window guidance, capital controls, export-control lists, procurement mandates — that overrides market alternatives; the mechanism is predominantly structural (allocation control), with a thinner internalized layer (socialized willingness to defer consumption for security) that the scalar does not separate. Theater 0.34: showcase milestones and self-sufficiency announcements outrun verified substance in visible programs, but physical capacity accumulation is real, keeping the ratio well below piton territory. Accessibility_collapse 0.55: once the strategic frame is adopted, alternative allocations become politically unthinkable inside the system, yet market-led and livelihood-first models remain live elsewhere and conceptually available — alternatives narrow but do not vanish. Resistance 0.48: consumer-sector lobbying, reformist critique inside the apparatus, household precautionary retrenchment, and foreign counter-controls meet the program continuously without threatening it. Coalition check: households are individually powerless, but their latent coalition power operates passively — precautionary saving already functions as uncoordinated resistance, and coordinated consumption retrenchment is the tail risk the absorption-limit omega tracks. Campaign-wave oscillation (crisis, intensification, consolidation) rides on top of the monotonic trend; the shared grid resolves the trend, not the waves. All three tracked series are authored at every point of one shared grid (2010, 2013, 2016, 2019, 2022, 2025), all observed.
 *
 * PERSPECTIVAL GAP:
 *   The paying seats and the agenda-setting seat should compute different types from identical metrics. From the planning apparatus's position the structure is a coordination achievement it designed and staffs; from the trapped household seat the same structure is a transfer it never consented to and cannot exit. Same-level divergence separates the two moderate payer seats: consumer services retain constrained exit (downsize, informalize) while non-strategic manufacturers are trapped behind capital controls and champion preemption — identical nominal power, different effective extraction. Inter-institutional divergence separates planners from champions: both sit near the beneficiary end, but champions additionally bear mandated burdens the role declaration cannot encode, handled through the directionality override on the powerful seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (champions, defense base, planning apparatus) derive low d for those seats; victim declarations (households, consumer services, non-strategic manufacturers) derive high d. Exit modulation places trapped households nearest the full-target end — their savings are captive to the domestic system that on-lends to the strategic sectors — while the champions' arbitrage-grade mobility (offshore listings, global sales) would otherwise pull them toward the beneficiary extreme; the explicit override sets the powerful seat to d=0.22 to register mandated-capacity burdens, price disciplines, and political exposure that the beneficiary role alone conceals. The planning apparatus derives low d from its beneficiary declaration (it collects budget, mandate, and standing), which is accurate: it is the arrangement's administrator and a net collector, though in mandate rather than capital. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated, so the arrangement is not yet a zombie mandate — mandatrophy resolution does not fire. The classification discipline cuts both ways: recognizing the genuine coordination function prevents mislabeling the arrangement as a pure snare (the security premium is real, and paying seats receive some offsetting employment and spillover benefits), while recognizing the asymmetric transfer prevents mislabeling it as a pure rope (the paying seats did not consent to the allocation and cannot exit it). The forward risk is mandate outliving function: if rivalry de-escalates or substitution succeeds, founding_problem_status flips to dead while disappearance_verdict stays world_rearranges — the mismatch consumer's zombie flag — and the theater trajectory in the measurement series is the early indicator. Identity-lock dynamics reinforce inertia: the planning apparatus has institutionally fused with the mission, and the reformers' professional identities are embedded in the apparatus they criticize; if a failed flagship program broke the planners' identity frame, internal challenge would surface, resistance would rise, and the founding-problem assessment would shift quickly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the performance_legitimacy kernel actually governs marginal resource allocation — and does this techno-nationalist reading''s dominance survive a sustained growth or livelihood shortfall?',
    'Observe crisis-time budget behavior: whether the state defends headline growth targets (quantitative reading), expands livelihood transfers (livelihood reading), or doubles down on strategic-industry funding (this reading); code the marginal unit of allocation across three consecutive crises.',
    'If a sibling reading governs the marginals, this constraint''s beneficiary/victim structure is misdescribed — under livelihood_security_reading households become beneficiaries and champions lose subsidy priority, collapsing measured extraction toward the rope range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading of the performance-legitimacy kernel binds actual allocation.').

omega_variable(
    security_function_vs_rent_cover,
    'Is the chokepoint vulnerability driving this arrangement genuine at the scale of investment undertaken, or does the security framing primarily cover sectoral rent-seeking by champions and the planning bureaucracy?',
    'Compare delivered self-sufficiency per unit of directed credit against counterfactual diversified-sourcing cost curves; audit duplication and overcapacity across champion firms; test whether subsidy intensity tracks measured chokepoint exposure or sectoral lobbying strength.',
    'If the security function is largely cover, the constraint migrates toward snare (coordination story as pretext, identifiable victims, suppressed exits); if genuine, tangled_rope stands with extraction priced as a security premium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_function_vs_rent_cover, empirical, 'Genuine security coordination versus rent-seeking cover.').

omega_variable(
    target_gaming_theater_trajectory,
    'Does tying ruling legitimacy to strategic-industry milestones make the program progressively unfalsifiable — do missed targets get reframed and statistics inflated as the legitimacy stakes compound?',
    'Independent physical verification of strategic-sector output (wafer capacity, airframe deliveries, installed compute) against announced milestones; track restatements and definitional changes in self-sufficiency metrics.',
    'Rising verified theater would push specific flagship programs toward piton dynamics (theatrical maintenance of atrophied claims) even while the aggregate constraint remains tangled_rope, and would erode the reading''s reference frame faster than the recorded drift state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(target_gaming_theater_trajectory, empirical, 'Legitimacy-stakes feedback inflating the theater ratio.').

omega_variable(
    household_absorption_limit,
    'How much consumption compression can trapped households absorb before the transfer function destabilizes — is there a demand-shortfall threshold that forces rebalancing?',
    'Track household consumption share of GDP against peer economies at comparable income levels; monitor precautionary-saving rates and informal capital-flight proxies as leading indicators.',
    'Hitting the absorption limit would force a rebalancing that converts the constraint toward a scaffold-like transition, or forces a reading shift toward livelihood_security_reading, reversing the extractiveness trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(household_absorption_limit, empirical, 'Absorption ceiling of the household-funded transfer.').

omega_variable(
    self_sealing_control_spiral,
    'Does the constraint''s own operation — import substitution and export-control retaliation — manufacture the evidence that validates its premise, creating a self-sealing escalation loop independent of realized vulnerability?',
    'Counterfactual tracing of rivalry escalation: does counter-control timing track this regime''s substitution milestones (loop confirmed) or exogenous geopolitical shocks (loop refuted)?',
    'If self-sealing, suppression and extraction ratchet autonomously of threat level and the constraint drifts toward snare across successive cycles; the founding problem becomes partially endogenous to the arrangement built to solve it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_sealing_control_spiral, conceptual, 'Self-sealing loop between the constraint''s operation and its own justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(perf_tr_t2010, observed).
narrative_ontology:measurement(perf_tr_t2013, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement_basis(perf_tr_t2013, observed).
narrative_ontology:measurement(perf_tr_t2016, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2016, 0.27).
narrative_ontology:measurement_basis(perf_tr_t2016, observed).
narrative_ontology:measurement(perf_tr_t2019, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement_basis(perf_tr_t2019, observed).
narrative_ontology:measurement(perf_tr_t2022, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2022, 0.32).
narrative_ontology:measurement_basis(perf_tr_t2022, observed).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2025, 0.34).
narrative_ontology:measurement_basis(perf_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(perf_be_t2010, observed).
narrative_ontology:measurement(perf_be_t2013, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2013, 0.47).
narrative_ontology:measurement_basis(perf_be_t2013, observed).
narrative_ontology:measurement(perf_be_t2016, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement_basis(perf_be_t2016, observed).
narrative_ontology:measurement(perf_be_t2019, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2019, 0.57).
narrative_ontology:measurement_basis(perf_be_t2019, observed).
narrative_ontology:measurement(perf_be_t2022, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement_basis(perf_be_t2022, observed).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(perf_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement_basis(perf_su_t2010, observed).
narrative_ontology:measurement(perf_su_t2013, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2013, 0.49).
narrative_ontology:measurement_basis(perf_su_t2013, observed).
narrative_ontology:measurement(perf_su_t2016, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2016, 0.53).
narrative_ontology:measurement_basis(perf_su_t2016, observed).
narrative_ontology:measurement(perf_su_t2019, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement_basis(perf_su_t2019, observed).
narrative_ontology:measurement(perf_su_t2022, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2022, 0.59).
narrative_ontology:measurement_basis(perf_su_t2022, observed).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(perf_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'performance legitimacy' is one colloquial label covering at least four structurally distinct arrangements depending on what counts as legitimating performance. This file instantiates the techno-nationalist reading only — strategic-industry dominance as the primary constraint, with champions and defense-adjacent sectors as beneficiaries and consumer sectors plus market-driven allocation as payers. The quantitative_growth_reading (headline expansion), qualitative_development_reading (innovation-and-efficiency transformation), and livelihood_security_reading (daily-life delivery) are separate files with their own epsilon values, beneficiary/victim sets, and classifications. Historical flow runs upstream from quantitative_growth_reading — the growth surplus funded the strategic turn — while this reading now exerts structural pressure on the siblings' operating environments (see cs_structure.reading_relations). Linkage via network.affects_constraints keeps the family connected for contamination propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
