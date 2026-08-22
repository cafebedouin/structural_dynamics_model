% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-First Climate Response Framework (Techno-Market Reading)
 *   domain: climate policy / political economy / intergenerational justice
 *
 * SUMMARY:
 *   This constraint is the mitigation_priority_reading of the
 *   climate_response_imperative kernel. It holds that climate stabilization
 *   should be pursued primarily through growth-compatible emissions
 *   reductionâtechnological innovation, carbon markets, and innovation
 *   subsidiesâwhile treating adaptation as a residual, secondary concern.
 *   The framework is instantiated in UNFCCC architecture, Paris NDC logic,
 *   and IPCC WGIII assessment emphases. Future generations and
 *   climate-vulnerable regions enter the victim set through deferred
 *   adaptation costs and reliance on unproven CDR; Global North innovation
 *   sectors are the concentrated beneficiaries. The sibling readings
 *   (adaptation_priority and degrowth) are structurally marginalized by
 *   agenda control and finance-flow design. This is a kernel reading: the
 *   epsilon and beneficiary/victim structure are specific to this reading,
 *   not averaged across the kernel.
 *
 * KEY AGENTS:
 *   - global_north_innovation_sectors: Primary beneficiary (powerful/arbitrage) â captures RCD and carbon-market rents
 *   - future_generations: Primary target (powerless/trapped) â bears deferred costs with no seat at the table
 *   - climate_vulnerable_regions: Primary target (powerless/constrained) â faces loss and damage under residual adaptation finance
 *   - global_north_governments: Agenda setter (institutional/arbitrage) â structures rules that minimize adaptation liability
 *   - adaptation_advocacy_coalition: Excluded voice (moderate/constrained) â present but out-resourced in agenda-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.62).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.6).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-First Climate Response Framework (Techno-Market Reading)").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate policy / political economy / intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '7a082d15-65dc-468c-9d3b-e931534845b0').
narrative_ontology:cs_kernel_codification('7a082d15-65dc-468c-9d3b-e931534845b0', formalized).
narrative_ontology:cs_authority_grounding('7a082d15-65dc-468c-9d3b-e931534845b0', lineage).
narrative_ontology:cs_interpretation_layer_present('7a082d15-65dc-468c-9d3b-e931534845b0').
narrative_ontology:cs_reading_relation('7a082d15-65dc-468c-9d3b-e931534845b0', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_reading_relation('7a082d15-65dc-468c-9d3b-e931534845b0', climate_response_imperative__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('7a082d15-65dc-468c-9d3b-e931534845b0', foundational, market_mechanism_sufficiency).
narrative_ontology:cs_axiom_status(market_mechanism_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('7a082d15-65dc-468c-9d3b-e931534845b0', market_mechanism_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('7a082d15-65dc-468c-9d3b-e931534845b0', foundational, adaptation_as_residual).
narrative_ontology:cs_axiom_status(adaptation_as_residual, holdable).
narrative_ontology:cs_axiom_grounding('7a082d15-65dc-468c-9d3b-e931534845b0', adaptation_as_residual, conventional).
narrative_ontology:cs_reference_frame('7a082d15-65dc-468c-9d3b-e931534845b0', market_environmentalism_reference).
narrative_ontology:cs_drift_state('7a082d15-65dc-468c-9d3b-e931534845b0', post_paris_accountability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a082d15-65dc-468c-9d3b-e931534845b0', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive direct public R&D funding, carbon-market rents, offset-development revenue, and regulatory preference for techno-solutions (CCS, hydrogen, direct-air capture). Their profitability and growth trajectory are structurally coupled to climate policy continuing to channel capital through innovation rather than through adaptation finance or demand-side reduction.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    powerful, generational, arbitrage, global).

% Bear the deferred costs of postponed adaptation, unproven CDR deployment, and locked-in warming trajectories. They have no delegation in current UNFCCC processes and cannot opt out of the atmospheric legacy created by a mitigation-priority, adaptation-residual policy architecture.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Experience escalating loss and damage while climate-finance flows remain skewed toward mitigation projects in donor-country supply chains. Adaptation finance is residual, grant-based, and chronically under-pledged relative to the cost of climate impacts, forcing these regions to absorb the gap.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions, payer,
    powerless, generational, constrained, global).

% Structure the UNFCCC agenda, design carbon-market mechanisms, and set NDC accounting rules. They benefit from a framework that minimizes immediate adaptation liabilities, preserves fiscal space, and channels domestic industrial policy through green-innovation subsidies rather than large-scale international redistributive transfers.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Comprises climate-vulnerable country negotiators, loss-and-damage advocates, and adaptation scientists who argue for parity between mitigation and adaptation finance. They are present in COP halls but systematically outvoted and under-resourced in agenda-setting; their priorities are relegated to residual budget lines.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, adaptation_advocacy_coalition, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global greenhouse-gas abatement through a common accounting architecture (NDCs, GHG inventories), carbon-pricing linkages, and innovation-subsidy races, avoiding a fragmented patchwork of incompatible national regulations.
% TRANSFER_FUNCTION: Moves financial and political capital toward Global North innovation sectors and mitigation projects, while deferring adaptation costs, loss-and-damage liabilities, and unabated climate risk to vulnerable regions and future generations.
% ABSENT_VOICES: Future generations have no delegation; the adaptation advocacy coalition is present but structurally outspent and outvoted. Degrowth advocates are largely outside formal UNFCCC processes and would argue for demand-side contraction rather than innovation-led decarbonization.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished overnight, climate finance would reallocate toward adaptation and loss-and-damage, innovation subsidies would shrink or pivot, carbon markets would lose their institutional anchor, and policy discourse would recenter on immediate resilience and redistribution rather than long-term techno-optimism.
% FOUNDING_PROBLEM: The atmospheric commons is a global public good with no inherent limit on emissions; no single state has incentive to unilaterally abate, producing a collective-action trap of escalating greenhouse-gas concentrations.
% FOUNDING_PROBLEM_CORROBORATION: Atmospheric science and independent earth-system monitoring confirm the physical commons problem persists. However, corroboration that a MARKET-BASED, MITIGATION-FIRST response is the correct solution comes overwhelmingly from within the benefiting parties (Global North governments, innovation sectors, and affiliated economic-modeling communities). Critics from vulnerable regions and degrowth scholarship attest the problem has shifted to include adaptation deficits and structural overconsumption not captured by the original framing.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the framework systematically channels finance toward mitigation in wealthy-country supply chains while adaptation and loss-and-damage remain underfunded; the residualization of adaptation is not a neutral ordering but a transfer of risk. Suppression (0.60) reflects the active agenda control, voting-weight asymmetries, and carbon-market enforcement required to keep adaptation residual and degrowth alternatives outside serious consideration. Theater ratio (0.48) captures the growing share of performative activityânet-zero pledges, speculative CDR portfolios, and offset markets that do not deliver commensurate atmospheric outcomes. Accessibility collapse (0.48) is moderate because alternatives are intellectually live but structurally excluded from dominant forums. Resistance (0.55) reflects persistent contestation from vulnerable-country negotiators and climate-justice movements. The metrics are authored independently of the claimed tangled_rope type; they describe the constraint's actual operation.
 *
 * PERSPECTIVAL GAP:
 *   From the Global North innovation-sector seat, the constraint operates as coordination (a global carbon-pricing and R&D architecture solving the commons problem). From the vulnerable-region and future-generation seats, the same structure operates as extraction (deferring their survival needs to subsidize present innovation rents). The engine computes this divergence from structural dataâbeneficiary declarations, victim declarations, and exit optionsâwithout requiring the author to reconcile the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North innovation sectors are declared beneficiaries (low d, subsidized by the constraint). Future generations and vulnerable regions are declared victims (high d, extraction amplified by universal scope and trapped/identity-locked exit). Global North governments are agenda-setters with arbitrage-grade exit; their directionality falls to the institutional-atom canonical default, sitting between the beneficiary and symmetric poles because they administer the extraction but also bear diffuse reputational costs. The adaptation advocacy coalition is excluded rather than coordinated; their exclusion is the mechanism by which the constraint maintains its priority ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe atmospheric commonsâremains live, so this is not a piton or zombie constraint. The coordination function (common NDC accounting, carbon-price linkage, innovation diffusion) is genuine and not merely theatrical cover. However, the transfer function is asymmetric: the same structure that coordinates abatement also concentrates its benefits in wealthy-region incumbents and concentrates its costs in those least able to exit. Mandatrophy resolution therefore classifies this as tangled rope rather than rope (because of the victim set) or snare (because the coordination is not coverâit is a real collective-action mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the mitigation_priority_reading of kernel climate_response_imperative; do sibling readings (adaptation_priority, degrowth) produce structurally different epsilon values and victim sets?',
    'Comparison across the constraint family: adaptation_priority_reading would show lower extraction from vulnerable regions but higher extraction from high-emitting industrial sectors; degrowth_reading would show extraction from global North consumption sectors. The divergence confirms epsilon is reading-indexed.',
    'If sibling readings yield similar epsilon and beneficiary structures, the kernel may be a single constraint mislabeled as contested; if divergence is large, the decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Epsilon invariance across kernel readings').

omega_variable(
    cdr_technological_bet,
    'Will carbon-dioxide removal and other speculative technologies scale to levels that make the mitigation-priority framework viable, or does reliance on them constitute a disguised deferral of extraction to the future?',
    'Empirical tracking of CDR deployment rates, cost curves, and geological-storage capacity against IPCC pathway assumptions; a sustained gap validates the extraction reading.',
    'If CDR fails to scale, the framework''s high extractiveness is exposed as a bet against future generations rather than a viable coordination plan; if CDR succeeds, part of the current extraction metric may be reclassified as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_technological_bet, empirical, 'Unproven CDR reliance and future risk transfer').

omega_variable(
    mitigation_adaptation_priority_ontology,
    'Is the prioritization of mitigation over adaptation a natural-law consequence of atmospheric physics (prevention is cheaper than cure), or a constructed constraint that protects Global North fiscal and industrial interests?',
    'Comparative cost-benefit analysis of adaptation vs mitigation at different warming levels, cross-referenced with the historical distribution of climate-finance flows; if mitigation finance systematically accrues to donor-country vendors while adaptation finance is withheld, the ordering is constructed.',
    'If the hierarchy is physically determined, the constraint should compute as rope or mountain; if it is politically constructed with identifiable beneficiaries, it confirms the tangled_rope classification and may trigger false-summit analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_adaptation_priority_ontology, conceptual, 'Natural vs constructed priority hierarchy').

omega_variable(
    intergenerational_extraction_mechanism,
    'Is the deferral of adaptation costs to future generations an unavoidable feature of long-horizon atmospheric coordination, or is it an extractive time-shift that benefits present actors?',
    'Analysis of discount-rate choices in integrated assessment models and the legal standing of future persons in climate litigation; high discount rates and zero standing support the extraction reading.',
    'If the deferral is structurally unavoidable, the victim status of future generations is mitigated; if it is a policy choice, the extraction is intensified and the directionality toward future generations rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_mechanism, preference, 'Intergenerational deferral as coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_mit_tr_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_mit_tr_t6, climate_response_imperative__mitigation_priority_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(clim_mit_tr_t12, climate_response_imperative__mitigation_priority_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(clim_mit_tr_t18, climate_response_imperative__mitigation_priority_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement(clim_mit_tr_t24, climate_response_imperative__mitigation_priority_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(clim_mit_tr_t30, climate_response_imperative__mitigation_priority_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(clim_mit_tr_t34, climate_response_imperative__mitigation_priority_reading, theater_ratio, 34, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_mit_be_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_mit_be_t6, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement(clim_mit_be_t12, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(clim_mit_be_t18, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(clim_mit_be_t24, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(clim_mit_be_t30, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(clim_mit_be_t34, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 34, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_mit_su_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_mit_su_t6, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(clim_mit_su_t12, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(clim_mit_su_t18, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(clim_mit_su_t24, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(clim_mit_su_t30, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(clim_mit_su_t34, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 34, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_response_imperative kernel decomposes into three structurally distinct constraints: mitigation_priority (this file), adaptation_priority, and degrowth. They share the referent (climate change as collective problem) but have different epsilon values, beneficiary/victim structures, and coordination types. This decomposition follows the epsilon-invariance principle: the label 'climate response' conflates multiple structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
