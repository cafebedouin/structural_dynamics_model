% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of Climate Response Legitimacy (Green Growth / Decoupling Paradigm)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the mitigation-priority reading of the
 *   contested climate-response-legitimacy kernel: legitimate climate response
 *   is defined as emissions reduction via technological innovation and carbon
 *   pricing, structured to preserve economic growth by decoupling it from
 *   emissions. This is the dominant reading in international negotiating
 *   architecture (UNFCCC market mechanisms, national carbon pricing regimes,
 *   IPCC WG3 mitigation chapters) and in most G7/G20 domestic climate
 *   legislation. It is NOT the adaptation-priority reading (which accepts the
 *   warming trajectory and prioritizes protecting vulnerable populations) or
 *   the degrowth-transformation reading (which holds that growth itself must
 *   be dismantled in wealthy nations). Those are separate constraints with
 *   their own ε and stakeholder structures, linked here via
 *   network.affects_constraints — this story does not average over them or
 *   describe their contest internally.
 *
 * KEY AGENTS:
 *   - incumbent_energy_and_industrial_capital: primary beneficiary via allowance capture and subsidy access (institutional/arbitrage)
 *   - carbon_market_intermediaries: fee-collecting beneficiary independent of decoupling outcome (organized/mobile)
 *   - high_income_nation_governments: agenda-setter defending the framework for domestic political feasibility (institutional/constrained)
 *   - future_generations: primary victim of decoupling-failure tail risk, structurally voiceless (powerless/trapped)
 *   - fossil_dependent_workers and low_income_carbon_price_payers: near-term cost bearers of the transition (powerless/trapped-constrained)
 *   - global_south_states_excluded_from_tech_access: bear pricing discipline without innovation-subsidy parity (moderate/constrained)
 *   - climate_scientists_and_iam_modelers: analytical observers flagging CDR feasibility assumptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.42).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Reading of Climate Response Legitimacy (Green Growth / Decoupling Paradigm)").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '6d2c30c8-a38f-4479-9ed7-36cda0675f8b').
narrative_ontology:cs_kernel_codification('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', distributed).
narrative_ontology:cs_authority_grounding('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', distributed).
narrative_ontology:cs_reading_relation('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', foundational, growth_and_emissions_are_severable).
narrative_ontology:cs_axiom_status(growth_and_emissions_are_severable, holdable).
narrative_ontology:cs_axiom_grounding('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', growth_and_emissions_are_severable, empirically_contingent).
narrative_ontology:cs_axiom('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', secondary, market_price_signals_are_the_legitimate_coordination_instrument).
narrative_ontology:cs_axiom_status(market_price_signals_are_the_legitimate_coordination_instrument, holdable).
narrative_ontology:cs_axiom_grounding('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', market_price_signals_are_the_legitimate_coordination_instrument, instrumental).
narrative_ontology:cs_reference_frame('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', post_kyoto_market_mechanism_consensus).
narrative_ontology:cs_drift_state('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', post_paris_ndc_gap_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6d2c30c8-a38f-4479-9ed7-36cda0675f8b', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_energy_and_industrial_capital).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, high_income_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, green_technology_investors).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_dependent_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, low_income_carbon_price_payers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_states_excluded_from_tech_access).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, growth_emissions_decoupling_feasibility).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, market_based_carbon_pricing_efficiency).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, innovation_led_transition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shapes the design of carbon pricing and offset schemes through lobbying and technical advisory participation, securing transition timelines and allowance allocations that protect asset value. Can arbitrage between jurisdictions with different carbon price stringency and capture subsidies for capital-intensive decarbonization technology it already owns or is positioned to build.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_energy_and_industrial_capital, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_energy_and_industrial_capital, agenda_setter).

% Originates, verifies, and trades offsets and allowances, collecting fees on every transaction the mitigation-priority architecture requires. Has no stake in whether decoupling actually occurs, only in the volume of instruments traded; can relocate operations to whichever jurisdiction hosts the most active carbon market.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Design and enforce carbon pricing, subsidize green technology R&D, and defend the mitigation-priority framing in international negotiations because it lets them claim climate leadership while preserving domestic GDP trajectories and avoiding redistributive or degrowth political costs at home.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, high_income_nation_governments, agenda_setter,
    institutional, generational, constrained, national).

% Deploy capital into renewables, carbon capture, and storage ventures whose valuations depend on continued policy commitment to the decoupling narrative; can exit any single national market if subsidy regimes shift, moving capital toward wherever the mitigation framework is best subsidized.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, green_technology_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Inherit whatever climate trajectory results if decoupling and carbon dioxide removal fail to scale fast enough to compensate for growth-preserving delay; cannot participate in present decisions, cannot exit the physical system, and bear the tail risk of a strategy premised on unproven technological timelines.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Employed in coal, oil, gas, and adjacent industries slated for phase-down under the mitigation-priority timeline; bear job loss and regional economic disruption while carbon pricing revenue is more often recycled into technology subsidy or fiscal consolidation than into worker transition guarantees.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% Pay a proportionally larger share of income on carbon-priced energy and goods than wealthier households, since energy and transport costs are a larger fraction of their budgets and they lack capital to substitute toward electric vehicles or efficient housing at the pace pricing assumes.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, low_income_carbon_price_payers, payer,
    powerless, biographical, constrained, national).

% Are told to decarbonize along the same technology-and-pricing pathway but lack access to patented green technology, cheap capital, or the carbon budget headroom that early industrializers already consumed; carbon pricing imposed on their growth path arrives without the innovation subsidies wealthy states extend to their own industries.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, global_south_states_excluded_from_tech_access, payer,
    moderate, generational, constrained, global).

% Produce integrated assessment models on which decoupling and negative-emissions-technology feasibility claims rest; some publicly flag that many pathways compatible with mitigation-priority policy assume large-scale carbon dioxide removal that does not yet exist at required scale.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_scientists_and_iam_modelers, observer,
    analytical, civilizational, analytical, global).

% Argue in academic and activist venues that decoupling at required speed is empirically unsupported and that adaptation investment or structural economic transformation should take priority, but are largely absent from the international negotiating architecture, which is built around carbon pricing and innovation instruments.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_and_adaptation_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, incumbent_energy_and_industrial_capital).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common framework — carbon pricing plus technology innovation subsidy — through which many independent emitters, firms, and states can coordinate emissions reduction without any single actor having to internalize the full cost alone, using price signals rather than command allocation.
% TRANSFER_FUNCTION: Moves compliance costs from carbon pricing toward low-income and fossil-dependent populations in the near term, moves subsidy and innovation-rent capture toward incumbent capital and green-technology investors, and moves the tail risk of decoupling failure toward future generations who have no seat in current policy design.
% ABSENT_VOICES: Future generations have no representation in the negotiating rooms where mitigation-priority frameworks are designed. Degrowth and adaptation-priority advocates are marginalized in the dominant international architecture (UNFCCC, carbon markets, IPCC WG3 policy chapters) despite holding structurally coherent alternative readings of the same kernel.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework disappeared overnight, incumbent capital and carbon-market intermediaries would lose a substantial revenue architecture and would argue the world rearranges catastrophically (no coordinated decarbonization mechanism). Adaptation-priority and degrowth advocates would argue the underlying climate problem persists regardless and a different coordination mechanism would simply replace this one — contested because the parties disagree about whether the specific instrument (carbon pricing + innovation subsidy) is necessary or merely one contingent implementation among viable alternatives.
% FOUNDING_PROBLEM: International climate negotiations needed a framework that major emitting economies, particularly the United States and other high-growth industrial states, would actually accept — one that did not require abandoning growth-based development models, given that growth-preserving approaches faced far less political resistance than redistributive or degrowth alternatives.
% FOUNDING_PROBLEM_CORROBORATION: High-income governments and incumbent capital attest the founding problem (political infeasibility of non-growth-preserving pathways) remains live and justifies continued mitigation-priority primacy. Independent climate scientists and IPCC contributing authors outside the negotiating apparatus have publicly noted that many mitigation-priority-compatible pathways depend on carbon dioxide removal deployment at a scale with no current technological or financial precedent, corroborating a claim that the founding problem's solution (decoupling feasibility) is empirically unresolved rather than settled.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, contested).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 and rising over the interval because the framework's revenue and subsidy architecture increasingly channels resources toward incumbent capital and technology investors relative to the pace of actual emissions decoupling achieved — a gap documented in the growing divergence between pledged NDCs and realized reductions. Suppression is moderate (0.42): the mitigation-priority reading does not coercively bar the sibling readings from being argued, but it does structurally dominate treaty text, funding mechanisms, and diplomatic agenda-setting, which constitutes a real if non-absolute form of alternative-suppression. Theater ratio is authored at a substantial and rising 0.47, reflecting the growing share of carbon market activity (offset issuance, corporate net-zero pledges) that functions as compliance performance rather than verified atmospheric reduction — a widely documented pattern in offset market audits. Accessibility collapse is moderate (0.40): rival readings remain articulable and are actively argued in scholarly and activist venues, so alternatives have not collapsed as completely as under a true mountain or entrenched snare. Resistance is moderate-high (0.55), reflecting active pushback from degrowth and adaptation advocates, climate justice movements, and Global South negotiators who contest the framework's fairness.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent energy capital, carbon market intermediaries, high-income governments, and green technology investors all sit near the beneficiary end of directionality: they administer, profit from, or politically benefit from the framework's persistence regardless of whether decoupling is actually achieved at pace. Future generations sit at the extreme target end — fully trapped, zero present voice, civilizational time horizon, bearing the entire tail risk of a strategy whose central empirical premise (rapid decoupling at required scale) remains unproven. Fossil-dependent workers and low-income carbon-price payers are targets in the near term: trapped or constrained exit, immediate time horizon, bearing disproportionate compliance costs relative to their capacity to substitute. Global South states occupy an intermediate position: moderate power (some negotiating leverage as a bloc) but constrained exit, since they face pricing discipline without matching access to the innovation subsidies wealthy states extend domestically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing political buy-in from powerful growth-oriented economies for any coordinated climate action — was real and arguably remains partially live (political feasibility constraints have not disappeared). This prevents a naive dismissal of the framework as pure extraction: it does solve a genuine coordination problem among heterogeneous emitters. But the tangled_rope classification is warranted because the same structure that solves the coordination problem simultaneously and systematically transfers costs to non-consenting future generations and to lower-power current populations, and requires active enforcement (carbon market regulation, subsidy administration, trade-linked carbon border mechanisms) to persist. The mandatrophy question is not whether coordination occurred but whether the mandate — legitimate climate response — has been quietly redefined to mean 'growth-preserving response' by the parties for whom growth preservation is most valuable, a redefinition corroborated by scientists outside the beneficiary set who note the empirical gap between pledged decoupling and verified outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_at_required_scale,
    'Is absolute decoupling of economic growth from emissions achievable at the speed and scale the mitigation-priority pathway requires to avoid dangerous warming, or does the pathway structurally depend on carbon dioxide removal technology that does not yet exist at the necessary scale?',
    'Longitudinal tracking of realized decoupling rates against IPCC-compatible pathway requirements, combined with independent auditing of carbon dioxide removal deployment against the volumes assumed in integrated assessment models underlying national pledges.',
    'If decoupling is infeasible at required scale, future generations'' victim status is structurally guaranteed rather than a risk, and the framework''s coordination claim collapses into a growth-preservation extraction mechanism dressed as climate policy. If feasible, the tangled_rope''s extractive component is a transitional cost rather than a terminal one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_at_required_scale, empirical, 'Whether the core empirical premise of this reading — that growth and emissions can be decoupled fast enough — is true.').

omega_variable(
    kernel_framing_choice_mitigation_vs_siblings,
    'Is the mitigation-priority reading''s dominance in international negotiating architecture a reflection of superior structural merit relative to adaptation-priority and degrowth-transformation readings, or a reflection of which parties hold agenda-setting power in that architecture?',
    'Comparative institutional analysis of which actors funded, staffed, and set agendas for the bodies (UNFCCC technical panels, IPCC WG3, national climate finance ministries) that elevated carbon pricing and innovation-subsidy instruments over adaptation-financing or degrowth-oriented structural reform as the primary policy vehicle.',
    'If dominance tracks agenda-setting power rather than structural merit, the mitigation-priority reading''s legitimacy claim is itself an artifact of the beneficiary structure it authorizes — strengthening the case that this is a captured kernel reading rather than a neutrally superior policy framework. This is a conceptual framing question, not resolvable by climate science alone, and is exactly the kind of committer-structure content this omega exists to hold rather than folding into the constraint''s classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_mitigation_vs_siblings, conceptual, 'Whether this reading''s institutional dominance reflects merit or power — the kernel-contest structure itself.').

omega_variable(
    future_generations_victim_status_contingency,
    'Does the decoupling failure risk that places future generations in the victim set apply uniformly, or is it contingent on which specific national and sectoral implementations of the mitigation-priority framework are adopted?',
    'Disaggregated pathway analysis distinguishing high-ambition carbon pricing regimes with strong CDR investment from low-ambition regimes that use carbon pricing primarily as a compliance-theater instrument.',
    'If victim status is contingent on implementation quality rather than inherent to the reading itself, the tangled_rope classification may be too harsh for high-ambition implementations and too lenient for low-ambition ones — suggesting this single story may itself warrant future decomposition by implementation stringency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_victim_status_contingency, empirical, 'Whether decoupling-failure risk to future generations is uniform across implementations of this reading or varies by policy stringency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.33).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.37).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.4).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.43).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.45).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.47).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.37).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This story is one of three members of the climate_response_legitimacy kernel family. mitigation_priority (this story) authors ε=0.58 as a tangled_rope: genuine multi-emitter coordination function via carbon pricing, coupled with asymmetric extraction from future generations and lower-power current populations, requiring active enforcement (carbon market regulation, subsidy administration). adaptation_priority and degrowth_transformation are separate constraints with independently authored ε values, beneficiary/victim structures, and classifications — they are not observable-parameter variants of this constraint but structurally distinct readings of what 'legitimate climate response' requires, per the ε-invariance principle. Each story documents its relationship to the siblings in commentary.kernel_context and cs_structure.reading_relations rather than folding sibling content into shared metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
