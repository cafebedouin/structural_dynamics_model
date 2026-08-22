% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Compact
 *   domain: political economy / environmental governance / intergenerational ethics
 *
 * SUMMARY:
 *   The standing arrangement under contest is the post-Kyoto climate
 *   governance compact: a below-2°C temperature limit pursued through
 *   nationally determined emissions reductions, carbon markets, and
 *   technology-led substitution, explicitly designed to preserve GDP growth.
 *   This story instantiates ONE reading of the climate_response_action kernel
 *   — the mitigation_priority reading — and authors epsilon for that standing
 *   arrangement as this reading assesses it; the sibling readings
 *   (adaptation_priority, degrowth_transformation) are separate stories, not
 *   folded into this one. The claim and metrics are independent authored
 *   facts: the compact is CLAIMED here as tangled_rope because it possesses a
 *   genuine coordination function (the free-rider problem is real and the
 *   pledge-review machinery addresses it) while the same structure carries
 *   asymmetric extraction (market rents, deferred adaptation costs, an
 *   uncompensated intergenerational transfer). The authored metrics describe
 *   how the arrangement actually operates; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take.
 *   Interval mapping: t0 corresponds to the UNFCCC's entry into force
 *   (~1992), t30 to the early-2020s post-ratchet period.
 *
 * KEY AGENTS:
 *   - - unfccc_cop_process: Agenda-setter (institutional/constrained) — administers the pledge-review machinery under consensus rules
 *   - - innovation_capacity_nations: Primary beneficiary (institutional/arbitrage) — shaped the rules, capture technology and finance rents
 *   - - carbon_market_intermediaries and green_technology_firms: Secondary beneficiaries (organized·powerful/arbitrage) — collect fees, spreads, and IP rents on every unit of activity
 *   - - incumbent_permit_holders: Dual-positioned (powerful/constrained) — hold grandfathered allowances yet bear compliance and stranded-asset exposure
 *   - - high_emitting_sector_workers and fossil_fuel_exporting_states: Payers (organized/constrained) — concentrated transition costs, tempered by agenda-blocking leverage in the exporters' case
 *   - - global_south_vulnerable_populations: Payer (powerless/trapped) — carries the deferred adaptation burden
 *   - - future_generations: Payer (powerless/trapped) — bears residual impacts with no seat and no exit
 *   - - youth_climate_movements: Excluded voice (moderate/trapped) — objecting from outside the credentialled rooms
 *   - - ipcc_assessment_body: Analytical observer — supplies the budgets and trajectories the regime cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.45).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Mitigation-Priority Climate Response Compact").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "political economy / environmental governance / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, 'aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e').
narrative_ontology:cs_kernel_codification('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', formalized).
narrative_ontology:cs_authority_grounding('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', lineage).
narrative_ontology:cs_interpretation_layer_present('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e').
narrative_ontology:cs_reading_relation('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', foundational, gdp_growth_compatible_with_climate_stability).
narrative_ontology:cs_axiom_status(gdp_growth_compatible_with_climate_stability, holdable).
narrative_ontology:cs_axiom_grounding('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', gdp_growth_compatible_with_climate_stability, empirically_contingent).
narrative_ontology:cs_axiom('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', foundational, warming_limit_as_primary_response_metric).
narrative_ontology:cs_axiom_status(warming_limit_as_primary_response_metric, holdable).
narrative_ontology:cs_axiom_grounding('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', warming_limit_as_primary_response_metric, conventional).
narrative_ontology:cs_reference_frame('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', green_growth_mitigation_compact).
narrative_ontology:cs_drift_state('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', post_first_global_stocktake, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aa9d7ccc-dceb-43cd-bbe7-5eac092e7f1e', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_capacity_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, incumbent_permit_holders).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_sector_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, green_technology_firms).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, incumbent_permit_holders).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, fossil_fuel_exporting_states).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, green_growth_decoupling_thesis).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, technological_substitution_optimism).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, carbon_pricing_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty framework: convenes the annual conferences, runs the transparency and review machinery, maintains the registry of national pledges, and brokers the finance negotiations. Operates by consensus, so any party can block decisions; its staff implement what parties agree rather than setting substantive terms themselves.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, unfcc_cop_process, agenda_setter,
    institutional, generational, constrained, global).

% Host the renewable-energy manufacturing bases, clean-energy research systems, and green finance centers. They shaped the design of the pledge-and-review architecture and the market mechanisms, and their firms capture the largest share of clean-technology patents, subsidies, and export markets. They can redirect industrial policy or hedge across regimes if the architecture disappoints them.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_capacity_nations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, innovation_capacity_nations, agenda_setter).

% Operate exchanges, registries, verification services, and trading desks for allowance and credit instruments. They earn fees and spreads on every transaction regardless of whether the underlying reductions materialize, and can relocate to whichever jurisdiction's market grows.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Own the power plants, cement kilns, steel works, and refinery fleets covered by allowance systems. Many received initial allocations free or at discounts; they bear retrofit and compliance spending but also hold appreciating permit portfolios and pass costs to customers. Leaving means writing down long-lived capital, so they lobby for allocations and transition timelines instead.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, incumbent_permit_holders, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, incumbent_permit_holders, payer).

% Build solar, wind, battery, hydrogen, and grid-software products whose demand the pledge cycle and subsidy programs create. They depend on sustained policy commitments for order books and collect licensing income from intellectual property; they can shift production and sales across borders to follow incentives.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, green_technology_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Work in coal mining, oil and gas, combustion-engine manufacturing, and energy-intensive industry. Transition policies concentrate job losses and wage cuts on their regions while retraining programs arrive late and underfunded; moving means relocating households or abandoning skills built over decades.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emitting_sector_workers, payer,
    organized, biographical, constrained, national).

% Depend on hydrocarbon export revenue for budgets and sovereign funds. They participate fully in negotiations and have blocked phase-out language, secured flexibility provisions in market rules, and slowed finance commitments; diversification away from export dependence has repeatedly stalled. Stranded-asset risk hangs over their reserves.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_fuel_exporting_states, payer,
    organized, generational, constrained, global).

% Live in low-lying coastal zones, arid interiors, and flood-prone deltas with minimal protective infrastructure. They contributed least to cumulative emissions yet absorb flooding, extreme heat, and crop failure first; promised adaptation finance arrives late and as loans more often than grants. Migration is partial, dangerous, and often unavailable to the poorest.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_vulnerable_populations, payer,
    powerless, generational, trapped, regional).

% Will inherit whatever concentration of greenhouse gases the present leaves in the atmosphere, along with the bill for removing or adapting to it. They are present in no negotiating room, cannot decline the arrangement, and their interests enter only as projections in models and as rhetorical appeals.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Organize strikes, litigation, and electoral pressure outside the formal negotiating rooms, which require delegation credentials. Many members fuse personal identity with the cause; they hold moral visibility but no votes, and their access consists of demonstrations and side-events at the conferences.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, youth_climate_movements, excluded,
    moderate, generational, trapped, global).

% Produces the periodic scientific assessments that define the carbon budgets, warming trajectories, and feasibility judgments the regime cites. It takes no position on policy design, reports what the literature supports, and its scenario ranges supply the reference points against which pledges are judged.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, ipcc_assessment_body, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts the atmospheric free-rider problem into reciprocal, reviewable commitments: a shared temperature budget, nationally determined contributions on five-year ratchet cycles, and market mechanisms that price emissions so reductions occur where they are cheapest.
% TRANSFER_FUNCTION: Moves abatement expenditure and compliance costs onto current high-emitting sectors and their workforces; moves subsidy flows, patent rents, and market fees toward innovation-capacity economies and market intermediaries; defers residual warming damages and adaptation bills to future generations and to exposed populations in the Global South.
% ABSENT_VOICES: Future generations hold no seat anywhere in the process. Sufficiency and degrowth advocates operate outside the frame the agenda assumes and are heard only in side-events. Exposed-community representatives attend as observers without decision rights. Adaptation needs enter as aid requests rather than as claims.
% DISAPPEARANCE_RATIONALE: If the compact vanished overnight, allowance prices would collapse, national pledge accounting and review would stop, green industrial strategy would lose its coordinating target, and climate finance flows would reroute or dry up. Climate response would reorganize around whatever successor arrangement major emitters accepted — or fragment into unilateral measures — rearranging trillions in planned investment.
% FOUNDING_PROBLEM: After the top-down targets of the Kyoto era failed to bind major emitters, build a regime that coordinates global emissions reductions while remaining compatible with continued economic growth, so that large and growing economies would join and stay inside it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: IPCC assessment reports and the first Global Stocktake document both the reality of the coordination problem and the shortfall of the growth-compatible response against it; AOSIS and Least Developed Country submissions attest that the arrangement under-delivers protection for the exposed; the independent climate-economics literature attests the founding framing. No source outside the benefiting parties attests that the current design is sufficient — that assertion originates only within the beneficiary set.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the compact's cost-bearing is systematically asymmetric: allowance grandfathering, credit non-additionality, chronically missed adaptation-finance promises, and a residual-warming bill assigned to parties who never consented. Suppression is 0.45 — enforcement against non-compliance is legally weak (no binding penalties), but the frame suppresses alternatives discursively: the growth clause and market orthodoxy crowd sufficiency approaches to the margins, and finance conditionality bounds poorer parties' choices. Theater is 0.47 and rising: net-zero declarations without implementation plans, offset accounting, and ratchet ceremonies now rival the functioning machinery (EU ETS, deployment subsidies, actual renewables buildout). Accessibility_collapse is 0.52 — once the frame is accepted, the solution space narrows sharply to markets-plus-innovation, but the frame itself remains contestable and its rivals are live. Resistance is 0.58: petrostate obstruction, Global South finance demands, worker-region pushback, and street movements all contest the arrangement continuously. All three temporal series run on one shared six-point grid so every metric is authored at every examined time point; suppression_requirement is tracked because the story's enforcement picture genuinely changed — MRV, the transparency framework, Article 6 rulebooks, and border adjustments hardened over the interval. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter and innovation-nation seats the compact is a coordination achievement they designed and staff — the free-rider problem solved at civilizational scale, with rents as the price of participation. From the worker, exporter, and Global South seats the same structure operates as a cost-allocation machine that charges them for a transition whose benefits accrue elsewhere and whose residuals land on the unrepresented. Future generations occupy the extreme case: total exposure, zero presence. The engine derives these divergent per-seat classifications from the power, exit, and role data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Innovation_capacity_nations sit nearest the beneficiary pole (rule-shapers with arbitrage-grade exit); carbon_market_intermediaries and green_technology_firms likewise (fee and IP collectors, mobile across jurisdictions). Incumbent_permit_holders derive a mid-low directionality — net recipients of grandfathered value despite real compliance costs. High_emitting_sector_workers derive high directionality (pay concentrated costs, constrained exit). Global_south_vulnerable_populations and future_generations derive near-full-target directionality (powerless, trapped, no consent channel). One nuance is left to the derivation rather than an override: fossil_fuel_exporting_states carry payer-role costs, but their agenda-blocking success (phase-out language removed, market-rule flexibilities secured) tempers their effective targeting below what a pure payer profile would yield; a power-atom-keyed override would also collide with high_emitting_sector_workers at the same atom, so the correction is documented here instead of forced through the override surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating cuts compatible with growth so major emitters stay inside — remains live, so no mandatrophy resolution is declared; the arrangement has not outlived its mandate, it under-performs it. The tangled_rope classification guards against two opposite misreadings. Reading the compact as pure snare erases the genuine collective-action achievement: some coordination mechanism for the atmospheric commons is irreducibly necessary, and the pledge-review architecture does real allocative work. Reading it as pure rope erases the extraction: the market layer leaks rents to intermediaries and incumbents, the growth clause externalizes residuals onto the unrepresented, and the theater series shows pledges substituting for delivery. The rising theater_ratio alongside rising extractiveness marks the drift vector to watch: if pledge performance continues substituting for physical outcomes, the compact slides toward piton — administered theatrically, borne diffusely, fixed by no one because the agenda-setters' consensus rules make repair prohibitively expensive relative to any single party's stake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the mitigation_priority reading of the climate_response_action kernel; how would the beneficiary/victim structure and measured extraction shift if the same standing arrangement were read under the sibling readings?',
    'Comparative classification across the linked sibling stories (climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation): the identical referent arrangement assessed under each reading''s own lights.',
    'Under adaptation_priority the victim set shifts toward presently-exposed populations and beneficiaries toward resilience-infrastructure builders, with the 2°C limit demoted from response metric to background condition. Under degrowth_transformation the GDP-growth clause drops out entirely, expanding victims to throughput-dependent industries and raising measured extraction of the growth-preserving components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints with different victim sets and epsilon.').

omega_variable(
    carbon_removal_feasibility,
    'Will carbon dioxide removal scale to the volumes the below-2°C-with-overshoot pathways assume, at costs the growth-preserving frame tolerates?',
    'Deployment and cost curves for direct air capture and bioenergy-with-capture against integrated-assessment pathway requirements; sensitivity runs on overshoot reliance.',
    'If removal underdelivers, residual warming grows, the intergenerational transfer deepens, and effective extraction rises across all payer seats; if it scales, part of the deferred-cost critique dissolves and the technological-substitution proposition gains vindication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_removal_feasibility, empirical, 'Feasibility of the carbon-removal assumption underwriting the reading''s target arithmetic.').

omega_variable(
    absolute_decoupling_adequacy,
    'Can GDP growth be maintained while emissions fall at the rate the 2°C limit requires — is the green-growth axiom empirically satisfiable at the needed pace?',
    'National decoupling rates versus required rates on production- and consumption-based accounts; detection of offshored emissions masking territorial progress.',
    'If not satisfiable, the growth clause forces either target abandonment or concealed extraction (offshored footprints, uncounted consumption), and the compact drifts toward snare as the coordination story becomes cover; if satisfiable, the foundational axiom holds and the extraction reading narrows to the market-rent channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_adequacy, empirical, 'Whether the reading''s distinguishing empirical premise — growth-compatible stabilization — survives the evidence.').

omega_variable(
    carbon_market_additionality,
    'What share of traded carbon credits and allocated allowances represents additional abatement rather than pre-existing trends, hot air, or phantom reductions?',
    'Credit-level integrity audits, registry data, and controlled comparisons of offset project outcomes against counterfactual baselines.',
    'Low additionality converts the market pillar from coordination instrument into a rent channel, raising both extractiveness and theater_ratio and strengthening the case that the market layer is extraction riding on the pledge architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_market_additionality, empirical, 'Integrity of the market mechanism the compact relies on for cost-effective reductions.').

omega_variable(
    intergenerational_transfer_legitimacy,
    'Does deferring residual climate costs to future generations constitute extraction, or a legitimate policy tradeoff under positive discount rates?',
    'Not resolvable by data alone — turns on the normative weight assigned to future welfare; resolvable only by explicit ethical commitment, which the corpus records as competing preference positions.',
    'A near-zero-discount ethic reclassifies the deferred-cost component as pure extraction aimed at the unrepresented; a positive-discount ethic treats part of it as ordinary intertemporal policy choice, lowering the victim-weighted extraction attributable to the compact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transfer_legitimacy, preference, 'Normative status of the intergenerational cost-shift at the heart of this reading''s structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cramp_tr_t0, climate_response_action__mitigation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cramp_tr_t6, climate_response_action__mitigation_priority, theater_ratio, 6, 0.26).
narrative_ontology:measurement(cramp_tr_t12, climate_response_action__mitigation_priority, theater_ratio, 12, 0.34).
narrative_ontology:measurement(cramp_tr_t18, climate_response_action__mitigation_priority, theater_ratio, 18, 0.4).
narrative_ontology:measurement(cramp_tr_t24, climate_response_action__mitigation_priority, theater_ratio, 24, 0.44).
narrative_ontology:measurement(cramp_tr_t30, climate_response_action__mitigation_priority, theater_ratio, 30, 0.47).

% Extraction over time
narrative_ontology:measurement(cramp_be_t0, climate_response_action__mitigation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cramp_be_t6, climate_response_action__mitigation_priority, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(cramp_be_t12, climate_response_action__mitigation_priority, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(cramp_be_t18, climate_response_action__mitigation_priority, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(cramp_be_t24, climate_response_action__mitigation_priority, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(cramp_be_t30, climate_response_action__mitigation_priority, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cramp_su_t0, climate_response_action__mitigation_priority, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cramp_su_t6, climate_response_action__mitigation_priority, suppression_requirement, 6, 0.3).
narrative_ontology:measurement(cramp_su_t12, climate_response_action__mitigation_priority, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(cramp_su_t18, climate_response_action__mitigation_priority, suppression_requirement, 18, 0.4).
narrative_ontology:measurement(cramp_su_t24, climate_response_action__mitigation_priority, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(cramp_su_t30, climate_response_action__mitigation_priority, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'climate response' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. Each reading yields a different constraint with its own epsilon, beneficiary/victim structure, and coordination function: mitigation_priority (this file — growth-compatible emissions reductions via markets and innovation), adaptation_priority (resilience investment accepting temperature rise), and degrowth_transformation (sufficiency-centered transformation rejecting the growth organizing principle). The readings are not observables of one constraint; forcing them into one story would average incompatible epsilons. This upstream reading influences the adaptation sibling by dominating resource allocation and conditioning adaptation's legitimacy as second-order, and stands in premise-level contradiction with the degrowth sibling. All family members link through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
