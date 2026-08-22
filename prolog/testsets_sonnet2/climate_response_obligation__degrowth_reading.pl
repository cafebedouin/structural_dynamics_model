% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Reading of the Climate Response Obligation — Sufficiency Over Efficiency
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the contested
 *   climate_response_obligation kernel: the claim that avoiding planetary
 *   boundary breach requires an absolute reduction in material and energy
 *   throughput, not merely decarbonization of existing growth trajectories.
 *   Sufficiency — living with less material turnover — is prioritized over
 *   efficiency gains that historically have been outpaced by growth (Jevons
 *   paradox). Under this reading, planetary systems and future generations
 *   become the primary beneficiaries of restraint, current high-consumption
 *   populations in the Global North enter the victim set as the ones asked to
 *   reduce lived material standard, and capital accumulation itself — because
 *   it structurally requires continuous throughput growth — becomes an
 *   extractive mechanism rather than a neutral instrument. This is a
 *   genuinely different constraint from the mitigation_priority reading
 *   (which holds throughput/growth constant and targets carbon intensity) and
 *   from the adaptation_priority reading (which holds throughput
 *   unconstrained and targets resilience investment); each reading has its
 *   own ε and its own beneficiary/victim structure and is authored as a
 *   separate story.
 *
 * KEY AGENTS:
 *   - planetary_systems: non-agent beneficiary, absorbs relief from reduced throughput
 *   - future_generations: powerless beneficiary, no bargaining power, civilizational time horizon
 *   - global_south_frontline_communities: dual beneficiary/payer depending on whether Northern sequencing is honored
 *   - global_north_consumer_households: primary payer, immediate lifestyle constraint
 *   - growth_dependent_corporations: primary payer, business-model-level constraint
 *   - global_south_industrializing_states: payer conditional on unenforceable Northern sequencing promise
 *   - degrowth_movement_coalition: agenda-setter, persuasive not coercive power
 *   - mitigation_priority_advocates: excluded rival reading, efficiency-first critique not incorporated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.55).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Reading of the Climate Response Obligation — Sufficiency Over Efficiency").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'ffe4735d-895e-4a1e-adcf-594f3939b45c').
narrative_ontology:cs_kernel_codification('ffe4735d-895e-4a1e-adcf-594f3939b45c', distributed).
narrative_ontology:cs_authority_grounding('ffe4735d-895e-4a1e-adcf-594f3939b45c', distributed).
narrative_ontology:cs_reading_relation('ffe4735d-895e-4a1e-adcf-594f3939b45c', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('ffe4735d-895e-4a1e-adcf-594f3939b45c', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('ffe4735d-895e-4a1e-adcf-594f3939b45c', foundational, throughput_reduction_is_necessary_not_sufficient_efficiency).
narrative_ontology:cs_axiom_status(throughput_reduction_is_necessary_not_sufficient_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('ffe4735d-895e-4a1e-adcf-594f3939b45c', throughput_reduction_is_necessary_not_sufficient_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('ffe4735d-895e-4a1e-adcf-594f3939b45c', foundational, capital_accumulation_requires_continuous_throughput_growth).
narrative_ontology:cs_axiom_status(capital_accumulation_requires_continuous_throughput_growth, holdable).
narrative_ontology:cs_axiom_grounding('ffe4735d-895e-4a1e-adcf-594f3939b45c', capital_accumulation_requires_continuous_throughput_growth, empirically_contingent).
narrative_ontology:cs_axiom('ffe4735d-895e-4a1e-adcf-594f3939b45c', secondary, global_north_consumption_reduction_is_a_justice_precondition).
narrative_ontology:cs_axiom_status(global_north_consumption_reduction_is_a_justice_precondition, holdable).
narrative_ontology:cs_axiom_grounding('ffe4735d-895e-4a1e-adcf-594f3939b45c', global_north_consumption_reduction_is_a_justice_precondition, deontological).
narrative_ontology:cs_reference_frame('ffe4735d-895e-4a1e-adcf-594f3939b45c', post_1972_limits_to_growth_framework).
narrative_ontology:cs_drift_state('ffe4735d-895e-4a1e-adcf-594f3939b45c', post_paris_agreement_green_growth_consensus, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ffe4735d-895e-4a1e-adcf-594f3939b45c', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_frontline_communities).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumer_households).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_dependent_corporations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_industrializing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_industrializing_states).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_frontline_communities).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, biophysical_limits_to_growth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Climate, biodiversity, freshwater, and nitrogen/phosphorus cycles absorb the physical consequences of material throughput. A degrowth reading treats reduced extraction and reduced waste flows as the primary relief this arrangement would provide, but the systems themselves have no voice or agency — they are represented only through proxies (scientists, activists, future-oriented institutions).
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% Inherit whatever biophysical envelope current throughput decisions leave behind. Under the degrowth reading they are the primary beneficiaries of restraint exercised now; they have no seat at any negotiating table and no capacity to bargain, litigate, or exit the consequences of present consumption levels.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Already absorb the physical impacts of Global North-driven throughput — extraction, dumping, climate volatility — while receiving a small share of the material benefit. Under this reading they benefit from Northern restraint but are also constrained: their own industrialization pathways would be curtailed if the reading's sequencing (North reduces first) is not honored in practice.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_frontline_communities, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_frontline_communities, payer).

% Have built daily life, housing, mobility, and consumption expectations around a high-throughput economy. A sufficiency-oriented degrowth program asks them to accept reduced material standard of living, smaller homes, less travel, less turnover of goods — losses that are immediate and personally felt, in exchange for a diffuse, delayed, collective planetary benefit they will not individually experience as a return.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumer_households, payer,
    moderate, biographical, constrained, national).

% Business models, debt structures, and shareholder return expectations are built on continuous throughput growth. A degrowth mandate directly targets their revenue model, not just their emissions; they can lobby, relocate operations, or attempt greenwashed compliance, but cannot exit the demand for shrinking material footprint without structural transformation of their core business.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_dependent_corporations, payer,
    powerful, biographical, constrained, global).

% Are mid-trajectory toward industrial development patterns the Global North already completed at high material cost. The degrowth reading asks them to forgo or radically reshape that trajectory before achieving comparable material security, conditional on Northern states reducing first — a sequencing promise they have limited ability to enforce and that Northern domestic politics has repeatedly failed to honor.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_industrializing_states, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_industrializing_states, beneficiary).

% Academics, activists, and allied policymakers who articulate the sufficiency framework, propose material throughput caps, and press for policy adoption (four-day work weeks, wealth-based consumption limits, moratoria on aviation expansion). They administer the reading's normative content and political strategy but hold no direct enforcement power over states or firms — their leverage is persuasive and electoral, not coercive.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_movement_coalition, agenda_setter,
    organized, generational, mobile, global).

% Favor rapid decarbonization within the existing growth paradigm — green tech deployment, carbon pricing, renewable buildout — and would object that the sufficiency framework's political unpopularity risks delaying decarbonization that could otherwise proceed faster under a growth-compatible banner. They are not part of this reading's coalition and their efficiency-first critique is not incorporated here.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mitigation_priority_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective reduction in material and energy throughput across high-consuming populations and firms so that aggregate extraction and waste flows fall within biophysical limits — a genuine collective-action problem, since no single actor's restraint matters unless enough others restrain simultaneously.
% TRANSFER_FUNCTION: Moves material consumption capacity, and the comfort/convenience/growth-derived returns bound up with it, away from Global North households and growth-dependent firms and toward reduced planetary pressure and preserved developmental headroom for the Global South and future generations — a transfer of forgone throughput, not a cash transfer.
% ABSENT_VOICES: Non-human ecosystems and future generations have no direct representation and are spoken for only by proxies. Global South states are nominally centered as beneficiaries of Northern restraint but are not decision-makers over whether or how fast the North actually reduces; if the North's compliance stalls, the promised sequencing benefit does not materialize and the South bears the constraint without the compensating relief.
% DISAPPEARANCE_RATIONALE: If the degrowth framework vanished as a policy proposal, mitigation-through-efficiency and adaptation approaches would continue unimpeded — much of the emissions-reduction and resilience apparatus does not depend on sufficiency framing. Degrowth advocates would say planetary boundary breaches (biodiversity loss, freshwater stress, material footprint) would proceed unchecked because efficiency gains are outpaced by growth (Jevons paradox); mitigation-priority advocates would say little changes because degrowth was never the operative mechanism driving actual decarbonization.
% FOUNDING_PROBLEM: Rising global material and energy throughput is breaching multiple planetary boundaries (climate, biodiversity, biogeochemical flows, freshwater) faster than efficiency improvements can offset, and decoupling of GDP growth from absolute resource use has not been empirically demonstrated at the scale or speed required.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and planetary boundaries researchers (Rockström, Steffen, Hickel) outside the direct degrowth political coalition corroborate the empirical claim that absolute decoupling has not occurred at sufficient scale. Mainstream growth economists and most national governments dispute the founding problem's framing, arguing green growth and efficiency-led decarbonization remain viable and that sufficiency framing is a political overreach not required by the underlying physics.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (rising over the interval) because the degrowth reading, as it moves from academic proposal toward binding policy, increasingly requires actual material sacrifice from concentrated populations (Northern consumers, growth-dependent firms) to deliver a diffuse and delayed planetary benefit — the classic shape of a redistribution that looks like extraction from the payer's vantage even when the coordination function is real. Suppression is moderate-rising (0.55) because enforcement so far is soft (norm entrepreneurship, some regulatory experiments — four-day weeks, degrowth-adjacent policy in a handful of jurisdictions) rather than hard coercion; a fully realized degrowth mandate would require much stronger enforcement machinery than currently exists, which the rising trajectory anticipates. Theater ratio (0.4) reflects that a meaningful share of current 'degrowth' activity is symbolic — corporate ESG gestures, voluntary simplicity movements — rather than binding throughput caps. Accessibility collapse is low-moderate (0.35): alternative paradigms (green growth, efficiency-led mitigation, adaptation) remain fully live and contested, unlike a settled natural law. Resistance is high (0.78) because the reading directly threatens entrenched material and political interests on every side — corporate growth models, Northern consumer expectations, and Southern developmental aspirations all resist some dimension of the sufficiency mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations sit at the pure-beneficiary end: they cannot bargain, cannot be coerced, and structurally gain from any real throughput reduction. Global North households and growth-dependent corporations sit toward the target end: they bear concentrated, immediate costs (reduced consumption, disrupted business models) in exchange for benefits that are diffuse, collective, and delayed. Global South states occupy a genuinely split position — beneficiaries in principle (preserved planetary boundary headroom, reduced Northern extraction pressure) but payers in practice if Northern reduction does not materialize on schedule, since their own development is asked to defer regardless. This directional asymmetry — the reading's normative logic constrains the South's future conditional on the North's compliance, which the South cannot enforce — is the structural core of why this reading, despite genuine coordination content, computes with meaningful extraction rather than as a clean rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (throughput exceeding planetary boundaries faster than efficiency gains offset it) remains contested but is corroborated by ecological economists outside the degrowth political coalition, so this is not simply a self-serving genealogy. The classification as tangled_rope rather than snare reflects that the coordination function is real and independently attested, not merely a cover story — but the asymmetric burden placement (concentrated cost on Northern populations and firms, diffuse and delayed benefit to planetary systems and future generations, conditional and unenforced benefit to the Global South) means the arrangement cannot be waved through as a pure rope. Declaring it tangled_rope rather than snare prevents mislabeling a genuine, contestable coordination proposal as pure extraction, while the authored victim set and rising extraction trajectory prevent the opposite error of laundering real distributive costs as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_vs_green_growth_empirical_status,
    'Is absolute decoupling of GDP growth from material throughput and emissions achievable at the scale and speed required to meet planetary boundaries, or does throughput reduction require abandoning growth as an organizing goal?',
    'Longitudinal empirical tracking of material footprint and emissions intensity across economies attempting green growth strategies versus degrowth-adjacent policy experiments; meta-analysis of decoupling literature over the next decade.',
    'If absolute decoupling proves achievable at required scale, the degrowth reading''s core premise weakens and the constraint''s justification shifts toward the mitigation_priority reading; if decoupling remains empirically absent, the degrowth reading''s founding problem is strongly corroborated and its extraction claim on Northern consumption gains legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_vs_green_growth_empirical_status, empirical, 'Whether green growth or degrowth is the empirically viable path to planetary-boundary compliance.').

omega_variable(
    north_south_sequencing_enforceability,
    'Can the Global South''s conditional benefit under this reading — constrained development contingent on prior Northern reduction — be structurally enforced, or is it a promise with no compliance mechanism?',
    'Track record analysis of prior North-South climate finance and technology transfer commitments (e.g., the unmet $100bn climate finance pledge) as a base rate for whether sequencing commitments of this type are honored.',
    'If the sequencing promise is structurally unenforceable, the Global South industrializing states classification shifts further toward pure payer/victim, strengthening the tangled_rope-toward-snare reading; if credible enforcement mechanisms exist or emerge, the coordination function is better corroborated and the classification stabilizes toward tangled_rope with a smaller extraction margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_south_sequencing_enforceability, empirical, 'Whether the North-South sequencing promise embedded in the degrowth reading is enforceable or merely rhetorical.').

omega_variable(
    sufficiency_political_feasibility_vs_mitigation_reading,
    'Does pursuing sufficiency framing over efficiency framing accelerate or delay actual material and emissions reduction, given the sufficiency reading''s greater political unpopularity in high-consumption democracies?',
    'Comparative political-economy analysis of jurisdictions that have adopted sufficiency-adjacent policy versus efficiency/mitigation-only policy, tracking actual throughput and emissions trajectories against electoral and policy durability outcomes.',
    'If sufficiency framing systematically triggers political backlash that delays any climate action, the degrowth reading may be actively counterproductive relative to the mitigation_priority reading even if its underlying physics is correct — this would not change ε for this story but would matter for network-level analysis of which reading dominates in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_political_feasibility_vs_mitigation_reading, preference, 'Whether sufficiency framing helps or hinders actual throughput/emissions reduction relative to the efficiency-first sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__degrowth_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(clim_tr_t12, climate_response_obligation__degrowth_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(clim_tr_t18, climate_response_obligation__degrowth_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__degrowth_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__degrowth_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__degrowth_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(clim_be_t12, climate_response_obligation__degrowth_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(clim_be_t18, climate_response_obligation__degrowth_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__degrowth_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__degrowth_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__degrowth_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(clim_su_t12, climate_response_obligation__degrowth_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(clim_su_t18, climate_response_obligation__degrowth_reading, suppression_requirement, 18, 0.45).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__degrowth_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__degrowth_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_obligation kernel, decomposed per the ε-invariance principle because the natural-language concept 'climate response obligation' conflates structurally distinct claims with different beneficiary/victim sets and different ε values. mitigation_priority holds throughput/growth roughly constant and targets carbon intensity via rapid decarbonization; adaptation_priority accepts continued warming and targets resilience investment rather than prevention; degrowth_reading (this story) targets absolute material throughput reduction and treats capital accumulation itself as an extractive mechanism. Each story carries its own ε, its own claimed_type, and its own stakeholder set; they are linked here rather than merged because averaging or hedging across them would violate DP-001 ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
