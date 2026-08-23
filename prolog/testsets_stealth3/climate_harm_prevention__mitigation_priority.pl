% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of Legitimate Climate Response
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation_priority reading of the
 *   climate_harm_prevention kernel: the standing arrangement under which
 *   legitimate climate response is defined as prioritized emissions
 *   reduction, pursued through technological transition, within a continuing
 *   growth framework. The referent for epsilon is that standing arrangement —
 *   the actual operating architecture of treaty cycles, national pledges,
 *   subsidy regimes, carbon markets, disclosure mandates, and border
 *   adjustments — assessed by this reading's own lights, never the
 *   adaptation-first or degrowth arrangements its siblings would install. Per
 *   the epsilon-invariance principle, the colloquial label 'climate policy'
 *   decomposes into three structurally distinct arrangements sharing one
 *   kernel commitment; this file authors only the mitigation-priority
 *   instance and links its siblings through the network block. The claim and
 *   the metrics are independent authored facts: the reading CLAIMS a
 *   coordination-centered arrangement serving future generations at
 *   transition cost to the present, while the authored metrics describe the
 *   arrangement's actual operation — rising extraction layered onto real
 *   coordination, a maturing enforcement apparatus, and a substantial and
 *   growing performative component. The divergence between claim and computed
 *   classification is the datum this corpus exists to take.
 *
 * KEY AGENTS:
 *   - - future_generations: Declared primary beneficiary (powerless/trapped) — receives avoided harm if delivery occurs; holds no vote, contract, or court access; appears only through proxies
 *   - - clean_technology_industries: Concentrated present-day beneficiary (powerful/mobile) — receives subsidies, mandates, and de-risked capital; shapes scheme design through lobbying
 *   - - green_finance_intermediaries: Beneficiary (institutional/arbitrage) — earns fees on every layer of the architecture regardless of aggregate outcomes
 *   - - fossil_fuel_incumbents: Dual-positioned payer-beneficiary (institutional/arbitrage) — bears compliance and stranding exposure while exploiting distant targets, offsets, and weak compliance to expand present output
 *   - - carbon_intensive_workforce: Primary present-day payer (organized/trapped) — occupation retired faster than local replacement; geographically and skill locked
 *   - - household_energy_consumers: Payer (moderate/constrained) — bears regressive pass-through costs; electoral leverage only
 *   - - general_taxpayers: Payer (moderate/constrained) — funds subsidy and finance regimes through public budgets
 *   - - climate_negotiation_apparatus: Agenda setter (institutional/identity_locked) — administers the cycles, certifies pledges; its existence and professional identity depend on the framework
 *   - - climate_vulnerable_nations: Beneficiary with subordinated claims (organized/trapped) — collects avoided warming; loses the parallel contest for adaptation finance
 *   - - adaptation_first_advocates: Excluded seat (organized/constrained) — present in the room, structurally subordinated in allocation
 *   - - independent_emissions_assessors: Analytical observer (institutional/analytical) — reconciles pledges against the atmosphere; no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.63).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.56).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.63).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Reading of Legitimate Climate Response").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'e0e7a997-a80b-445d-9e1b-e3b3826f30a7').
narrative_ontology:cs_kernel_codification('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', fixed_text).
narrative_ontology:cs_authority_grounding('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', lineage).
narrative_ontology:cs_interpretation_layer_present('e0e7a997-a80b-445d-9e1b-e3b3826f30a7').
narrative_ontology:cs_reading_relation('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', foundational, growth_compatible_decarbonization_sufficient).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', growth_compatible_decarbonization_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', foundational, intergenerational_prevention_primacy).
narrative_ontology:cs_axiom_status(intergenerational_prevention_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', intergenerational_prevention_primacy, deontological).
narrative_ontology:cs_axiom('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', secondary, technological_substitution_over_contraction).
narrative_ontology:cs_axiom_status(technological_substitution_over_contraction, holdable).
narrative_ontology:cs_axiom_grounding('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', technological_substitution_over_contraction, instrumental).
narrative_ontology:cs_reference_frame('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', managed_transition_prevention_compact).
narrative_ontology:cs_drift_state('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', contemporary_post_paris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e0e7a997-a80b-445d-9e1b-e3b3826f30a7', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, clean_technology_industries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_finance_intermediaries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_vulnerable_nations).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_workforce).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, household_energy_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People not yet born who will inherit whatever atmospheric concentration and warming trajectory present policy locks in. They receive avoided harm if emissions reduction delivers and inherited damage if it does not. They hold no vote, contract, or court access in any present jurisdiction; they appear only through proxy advocates, ombudsperson offices, and constitutional clauses adopted in a handful of states.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Manufacturers and developers of renewables, batteries, electric vehicles, electrolyzers, and heat pumps. Subsidy programs, purchase mandates, and carbon prices create guaranteed demand for their products, and public funds de-risk their capital expenditure. They lobby actively over scheme design — tariff levels, mandate timing, subsidy duration — and can redirect investment across jurisdictions if policy support moves.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, clean_technology_industries, beneficiary,
    powerful, biographical, mobile, global).

% Asset managers, carbon-market registries, offset verifiers, and ESG rating agencies. They earn fees on every layer of the framework — credit issuance, disclosure compliance, transition-fund management — whether or not aggregate emissions fall. Their revenue scales with the complexity and continuity of the architecture itself.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_finance_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Oil, gas, and coal producers and heavy-process industries. They face compliance costs, border adjustments, and eventual stranding of reserves under announced targets. At the same time, distant target dates, purchasable offsets, and weak compliance mechanisms allow them to expand production now against promised later reductions; several major producers have grown output since adopting net-zero commitments. They can relocate production, acquire offset portfolios, and fund political opposition to binding near-term steps.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_incumbents, payer,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, fossil_fuel_incumbents, beneficiary).

% Coal miners, oil and gas workers, combustion-engine plant employees, and the regional service economies around them. Transition policy retires their occupations faster than replacement jobs arrive locally; their skills and housing are location-specific. Unions negotiate transition packages but cannot stop closures; relocating means abandoning homes and community networks.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_workforce, payer,
    organized, immediate, trapped, regional).

% Households paying carbon costs passed through fuel and electricity prices, plus higher upfront costs for heat pumps and electric vehicles. Incidence is regressive — energy takes a larger share of low-income budgets. They can change governments at elections but not the framework itself; fuel-price increases have repeatedly triggered protest movements and policy reversals.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, household_energy_consumers, payer,
    moderate, immediate, constrained, national).

% Fund subsidy schemes, grid upgrades, international climate finance contributions, and write-downs for failed green industrial projects through public budgets. Benefits arrive as diffuse system improvements; costs appear as itemized expenditures. Control is exercised indirectly through fiscal politics.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% The UNFCCC secretariat, rotating COP presidencies, national climate ministries, and treaty bodies. They convene negotiation cycles, maintain the transparency framework, and certify national pledges. Their budgets, staffing, and diplomatic standing exist because the framework exists, and their professional identities are built around its procedures; they cannot credibly champion replacing the structure they administer.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_negotiation_apparatus, agenda_setter,
    institutional, generational, identity_locked, global).

% Low-lying island states, drought-exposed Sahel and Horn states, and delta nations negotiating in blocs. They collect avoided warming from any real mitigation and contributed least to cumulative emissions. Their near-term adaptation and loss-and-damage needs compete with mitigation for the same finance envelopes and consistently lose; they cannot exit the climate system, and their leverage depends on moral standing within the process.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_vulnerable_nations, beneficiary,
    organized, generational, trapped, global).

% Vulnerable-country delegates, resilience practitioners, and development agencies arguing that populations facing floods, heatwaves, and crop failure now need funded adaptation ahead of further mitigation spending. They attend negotiations but their agenda is structurally subordinated: adaptation receives a small fraction of tracked climate finance, and adaptation-first framing is treated in the process as a fallback for actors who cannot mitigate, not a coequal strategy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_first_advocates, excluded,
    organized, immediate, constrained, global).

% IPCC author teams, UNEP Emissions Gap analysts, Climate Action Tracker, and academic carbon-accounting groups. They reconcile pledged targets against measured atmospheric concentrations and enacted policies. They hold no enforcement power; their findings feed the global stocktake cycle and the public record.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, independent_emissions_assessors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, clean_technology_industries).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a response to a shared-atmosphere collective-action problem: harmonizing national emissions commitments, standardizing measurement and accounting so pledges are comparable, pooling technology diffusion and finance so no single actor bears first-mover costs alone, and providing a recurring diplomatic cycle in which ambition can be revisited.
% TRANSFER_FUNCTION: Moves present-day costs — public subsidy funds, carbon-price payments, consumer energy premiums, and employment in carbon-intensive regions — from present payers to clean-technology producers and financial intermediaries now, and, if delivery occurs, moves deferred climate damages away from future generations.
% ABSENT_VOICES: Future generations have no seat at all and appear only through proxies. Adaptation-first constituencies are physically present in the process but structurally subordinated in finance allocation. Degrowth and ecological-economics voices sit outside the legitimate-policy boundary entirely — their premise (that growth-compatible decarbonization is impossible at the required pace) is not entertained within the framework's own proceedings.
% DISAPPEARANCE_RATIONALE: Energy investment pipelines, industrial strategy, trade instruments such as carbon border adjustments, corporate disclosure regimes, carbon markets, and the entire diplomatic architecture presuppose the framework. Overnight disappearance would strand subsidy-dependent industries, void offset and allowance portfolios, remove the comparability standard that makes national pledges meaningful, and leave emissions governance to uncoordinated national measures.
% FOUNDING_PROBLEM: Organizing a response to a global externality whose worst harms fall on people not yet alive, under conditions where no market or existing institution prices those harms and sovereign states face persistent free-riding incentives — building a legitimate framework under which emissions reduction can be coordinated at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: IPCC assessment cycles and UNEP Emissions Gap reports document a widening gap between pledged and delivered reduction against a finite remaining carbon budget, and the atmospheric CO2 concentration record is the physical attestation that the founding problem persists. Vulnerable-nation diplomatic statements attest the same urgency from an exposed seat. Stated plainly: the scientific bodies attesting operate within the framework's broader epistemic world, though they are organizationally independent of the parties that collect from the arrangement; no attesting source is fully external to it.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.63: costs are concentrated and present — displaced carbon-intensive labor, regressive energy-price pass-through, taxpayer-funded subsidy and de-risking regimes — while the headline benefit accrues to a seat that cannot collect, audit, or complain; meanwhile mandated markets and subsidy channels route substantial public value to identifiable private recipients. Suppression 0.56 is authored as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by the engine): it is institutional closure rather than police coercion — adaptation and degrowth framings marginalized in finance allocation and policy legitimacy, with an enforcement machinery (disclosure mandates, border adjustments, compliance litigation) that visibly hardened across the interval, which is why suppression_requirement is tracked temporally. Theater_ratio 0.62: the pledge-delivery gap, offset-integrity failures, and corporate net-zero commitments without transition plans sit alongside real deployment and real technology-cost declines — the functional and performative components coexist, with the performative share growing. Accessibility_collapse 0.40: alternatives do not vanish on inspection — adaptation-first and degrowth remain visible and argued — the arrangement outcompetes them for resources and legitimacy rather than erasing them. Resistance 0.65: fuel-price protest movements, carbon-tax repeal, federal policy whiplash, industry lobbying, and equity standoffs in negotiation. All three metric series run on one shared seven-point grid (1992–2025) so no metric is sampled against another's end-state; a COP-cycle modulation (pledge spikes around summits, troughs after) is superimposed on the monotonic rise and is not itself the extraction mechanism — the underlying trend is accumulation, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as the achievement of multilateralism — completed COP cycles, ratified targets, a functioning transparency regime — and computes a coordination-dominated picture. The payer seats experience costs arriving now against benefits scheduled for later, and compute a materially harsher type from identical structural facts. The future_generations seat computes as a full beneficiary yet exerts zero causal force anywhere in the system — the arrangement's defining asymmetry, and the reason its beneficiary declaration cannot be read as evidence of benignity. The fossil-incumbent seat computes divergently across its two roles: as payer it faces stranding; as secondary beneficiary it harvests delay. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality (subsidized): future_generations sits at the extreme beneficiary pole — maximal subsidy, zero enforcement capacity — which inverts the usual beneficiary check on extraction, since the seat that would resist mission drift cannot act. clean_technology_industries and green_finance_intermediaries sit nearby, with green finance's arbitrage-grade exit placing it closest to the beneficiary bound. Payers derive high directionality: carbon_intensive_workforce sits nearest the full-target end (trapped exit amplifies), with household_energy_consumers and general_taxpayers somewhat below on constrained exits. fossil_fuel_incumbents are deliberately NOT listed in the victims array: their dual role (payer with secondary_role beneficiary) plus arbitrage exit places them mid-range without an override, encoding the empirical fact that the arrangement's costs and its delay dividends land on the same actors. No directionality_overrides are authored: the structural declarations already separate the seats, and the override mechanism is keyed by power atom, so any override would collide across same-atom agents (for example, 'institutional' covers both green_finance_intermediaries and climate_negotiation_apparatus, which sit at different points).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — the gap between pledged and delivered reduction against a finite budget is widening, not closed — so the mandate has not outlived its function and mandatrophy is not resolved. The classification work runs in both directions: reading this arrangement as pure extraction erases the genuine coordination achievement (the only operating framework for carbon-budget comparability, real deployment acceleration, and real cost declines in transition technologies); reading it as pure coordination erases the asymmetric extraction (present-concentrated costs, intermediary capture, and a performative layer that converts announced ambition into delay). The early-warning signature for degradation is already visible: a live founding problem combined with a world_rearranges dependence verdict and a steadily rising theater_ratio is the mismatch pattern that precedes administered-performance operation. The scaffold ambiguity is documented as an omega rather than resolved here: the 2050-style horizon reads like a sunset clause in the arrangement's self-description but has so far functioned as a receding horizon; if post-target planning winds the apparatus down, the transitional reading strengthens, and if it rebrands and extends, the persistence is institutional self-perpetuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (mitigation_priority) of the climate_harm_prevention kernel. What structurally changes if a sibling reading is instantiated instead?',
    'Compare the three files'' victim sets, temporal horizons, and growth premises: adaptation_priority shifts primary beneficiaries to presently exposed populations and accepts a higher warming trajectory; degrowth_reading rejects the growth-compatibility premise and assigns contraction burdens to the Global North. The disagreement is located in three specific elements: the temporal distribution of costs and benefits, whether decarbonization at the required pace is compatible with continued growth, and whose harm counts as ''prevented.''',
    'Each sibling instantiation produces a different beneficiary/victim structure and therefore a different directionality profile and classification; conclusions drawn from this file do not transfer to the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of a contested kernel; sibling readings alter the structural data, not merely the emphasis.').

omega_variable(
    growth_compatibility_empirics,
    'Is decarbonization at the pace the remaining carbon budget requires physically and politically compatible with continued economic growth, as this reading''s foundational axiom assumes?',
    'Sustained comparison of observed absolute-decoupling rates in major economies against the decline rates implied by carbon-budget trajectories; technology cost-curve and deployment-speed audits against required buildout.',
    'If observed decoupling is chronically too slow, the reading''s core premise fails empirically, the degrowth sibling gains force, and this arrangement''s classification drifts toward a deferral mechanism — coordination rhetoric sustaining continued growth while the budget depletes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_empirics, empirical, 'Whether the growth-compatibility premise survives contact with decoupling-rate evidence.').

omega_variable(
    pledge_delivery_reconciliation,
    'Does the pledge architecture produce real reductions, or does it primarily manufacture accounting artifacts — with the theater_ratio measuring transitional lag versus structural performance substitution?',
    'Reconcile NDC-reported inventory reductions against independent atmospheric and satellite measurements; track the fraction of announced reductions attributable to offsets, land-use accounting, and projected future removals rather than present-year emission cuts.',
    'If the accounting share dominates, the arrangement''s coordination function is largely ceremonial and the extraction profile concentrates in intermediaries; if delivery lags but is real, the theater reflects a slow transition rather than substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pledge_delivery_reconciliation, empirical, 'Whether measured progress is physical or bookkeeping.').

omega_variable(
    intergenerational_enforceability,
    'Can an arrangement whose declared primary beneficiary cannot enforce, observe, or even consent to its operation sustain non-extractive operation over time?',
    'Track whether representation innovations — future-generations commissioners, youth councils, rights-based litigation, constitutional clauses — measurably shift resource allocation toward long-horizon outcomes, or remain symbolic offices.',
    'If representation mechanisms fail, the beneficiary structure is nominal and present-day intermediaries are the operative collectors; the arrangement''s effective beneficiary set collapses to its present-day seats, raising effective extraction on present payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_enforceability, conceptual, 'Whether a non-present beneficiary seat can anchor the arrangement''s legitimacy claim.').

omega_variable(
    offset_additionality_integrity,
    'Do carbon offsets represent additional reductions, or do they function as permission slips that license continued emissions while generating intermediary fee income?',
    'Integrity audits of major offset registries against counterfactual baselines and leakage; compare retirement volumes against verified atmospheric outcomes.',
    'A large non-additional share converts a substantial portion of claimed mitigation into pure transaction volume — inflating theater_ratio and shifting the arrangement toward administered performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(offset_additionality_integrity, empirical, 'Integrity of the offset layer that the net-zero doctrine depends on.').

omega_variable(
    sunset_horizon_credibility,
    'Is the net-zero horizon a credible terminal condition that makes this arrangement transitional, or a perpetually receding horizon that sustains a permanent administrative and financial apparatus?',
    'Observe whether post-target planning winds the apparatus down (mandates lapse, markets close, institutions sunset) or rebrands and extends (negative-emissions maintenance, perpetual carbon management, rolling new horizons).',
    'If the horizon recedes indefinitely, the arrangement lacks a genuine sunset despite its transitional self-description, and its persistence rests on institutional self-perpetuation rather than on the transition it names.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_horizon_credibility, conceptual, 'Scaffold-ambiguity check on the 2050-style horizon: sunset clause or receding horizon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__mitigation_priority, theater_ratio, 1992, 0.24).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__mitigation_priority, theater_ratio, 1997, 0.33).
narrative_ontology:measurement(clim_tr_t2002, climate_harm_prevention__mitigation_priority, theater_ratio, 2002, 0.41).
narrative_ontology:measurement(clim_tr_t2009, climate_harm_prevention__mitigation_priority, theater_ratio, 2009, 0.47).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.51).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.57).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__mitigation_priority, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__mitigation_priority, base_extractiveness, 1992, 0.3).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__mitigation_priority, base_extractiveness, 1997, 0.37).
narrative_ontology:measurement(clim_be_t2002, climate_harm_prevention__mitigation_priority, base_extractiveness, 2002, 0.41).
narrative_ontology:measurement(clim_be_t2009, climate_harm_prevention__mitigation_priority, base_extractiveness, 2009, 0.46).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__mitigation_priority, base_extractiveness, 2025, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__mitigation_priority, suppression_requirement, 1992, 0.18).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__mitigation_priority, suppression_requirement, 1997, 0.27).
narrative_ontology:measurement(clim_su_t2002, climate_harm_prevention__mitigation_priority, suppression_requirement, 2002, 0.31).
narrative_ontology:measurement(clim_su_t2009, climate_harm_prevention__mitigation_priority, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__mitigation_priority, suppression_requirement, 2025, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel climate_harm_prevention. The colloquial label 'climate policy' conflates three structurally distinct arrangements: mitigation-priority (this file), adaptation-priority, and degrowth. They differ in victim sets (future generations vs presently exposed populations vs present overconsumers), temporal horizons, and the growth-compatibility premise, so each carries its own epsilon, beneficiaries, and classification. Upstream/downstream structure: mitigation-priority currently dominates climate finance allocation and legitimacy, which structurally conditions the adaptation sibling's operating environment (adaptation financed as residual) — hence the influences edge; the degrowth sibling is logically excluded by this reading's foundational growth-compatibility axiom, hence the foreclosure edge. Each file links the others; no member of the family is orphaned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
