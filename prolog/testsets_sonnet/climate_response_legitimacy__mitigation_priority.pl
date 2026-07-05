% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Mitigation-Priority Reading of Legitimate Climate Response (Tech-Innovation + Carbon Pricing, Growth-Preserving)
 *   domain: Climate Policy / Political Economy / Intergenerational Ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the contested
 *   'legitimate climate response' kernel: the position that emissions
 *   reduction via technological innovation and carbon pricing, undertaken so
 *   as to preserve economic growth while decoupling it from emissions, is the
 *   legitimate core of climate response. This is a distinct constraint from
 *   the adaptation-priority reading (which accepts the warming trajectory and
 *   prioritizes protective resilience infrastructure) and the
 *   degrowth-transformation reading (which holds that legitimacy requires
 *   dismantling the growth imperative itself). The three readings are not the
 *   same constraint measured three ways — they have different
 *   beneficiary/victim structures, different theories of what counts as an
 *   adequate response, and different institutional homes (UNFCCC/IPCC WG3
 *   technology-and-mitigation chapters vs. WG2 adaptation chapters vs.
 *   ecological economics and post-growth literatures). This story generates
 *   ONLY the mitigation-priority reading; the sibling readings are separate
 *   constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - incumbent_energy_technology_firms: Primary beneficiary (institutional/arbitrage) — captures innovation subsidies and sets the technical menu of acceptable pathways
 *   - carbon_market_intermediaries: Secondary beneficiary (organized/mobile) — earns fees on trading volume independent of abatement outcomes
 *   - future_generations: Primary target (powerless/trapped, civilizational horizon) — bears residual climate risk if the decoupling bet fails to close in time
 *   - carbon_intensive_workers: Direct target (powerless/constrained) — absorbs employment disruption from the phase-down side of pricing
 *   - global_south_frontline_states: Direct target (powerless/trapped) — faces earlier and more severe physical impacts under a growth-preserving pace of cuts
 *   - climate_policy_negotiators: Agenda setter (institutional/analytical) — drafts the frameworks that operationalize this reading as default legitimate policy
 *   - adaptation_and_degrowth_advocates: Excluded voice (organized/constrained) — present in discourse but structurally marginal in treaty-level agenda-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.42).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Reading of Legitimate Climate Response (Tech-Innovation + Carbon Pricing, Growth-Preserving)").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "Climate Policy / Political Economy / Intergenerational Ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '28a482dc-3082-4cc2-af0d-32c1708ac107').
narrative_ontology:cs_kernel_codification('28a482dc-3082-4cc2-af0d-32c1708ac107', distributed).
narrative_ontology:cs_authority_grounding('28a482dc-3082-4cc2-af0d-32c1708ac107', distributed).
narrative_ontology:cs_reading_relation('28a482dc-3082-4cc2-af0d-32c1708ac107', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('28a482dc-3082-4cc2-af0d-32c1708ac107', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('28a482dc-3082-4cc2-af0d-32c1708ac107', foundational, growth_emissions_decoupling_is_achievable).
narrative_ontology:cs_axiom_status(growth_emissions_decoupling_is_achievable, holdable).
narrative_ontology:cs_axiom_grounding('28a482dc-3082-4cc2-af0d-32c1708ac107', growth_emissions_decoupling_is_achievable, empirically_contingent).
narrative_ontology:cs_axiom('28a482dc-3082-4cc2-af0d-32c1708ac107', foundational, economic_growth_is_non_negotiable_policy_constraint).
narrative_ontology:cs_axiom_status(economic_growth_is_non_negotiable_policy_constraint, holdable).
narrative_ontology:cs_axiom_grounding('28a482dc-3082-4cc2-af0d-32c1708ac107', economic_growth_is_non_negotiable_policy_constraint, instrumental).
narrative_ontology:cs_reference_frame('28a482dc-3082-4cc2-af0d-32c1708ac107', post_kyoto_market_mechanism_consensus).
narrative_ontology:cs_drift_state('28a482dc-3082-4cc2-af0d-32c1708ac107', post_paris_agreement_implementation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('28a482dc-3082-4cc2-af0d-32c1708ac107', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_energy_technology_firms).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, growth_dependent_finance_sector).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, wealthy_nation_consumers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, cdr_dependent_land_communities).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, decoupling_feasibility_thesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, market_based_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell renewable, carbon capture, and efficiency technologies; lobby for carbon pricing designs and innovation subsidies that route public transition finance toward their product lines. Set much of the technical agenda for what counts as a credible pathway, since their patents and deployment capacity define the available toolkit. Can relocate capital across jurisdictions as policy regimes shift.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_energy_technology_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_energy_technology_firms, agenda_setter).

% Broker, verify, and trade carbon credits and offsets; earn fees on transaction volume and verification services regardless of whether underlying abatement is real. Their revenue is tied to market activity, not emissions outcomes, giving them a structural interest in maintaining pricing-and-trading as the dominant mechanism.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Prices sovereign debt, equities, and infrastructure bonds on the assumption of continued GDP growth; underwrites green bonds and transition finance that require growth-compatible framing to remain investable. Benefits from a reading that preserves growth trajectories rather than one requiring economic contraction or restructuring.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, growth_dependent_finance_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Retain current consumption patterns and living standards while emissions reduction is pursued through supply-side technology and pricing rather than demand-side lifestyle change. Pay somewhat higher carbon-priced goods but are shielded from the deeper disruption that adaptation-only or degrowth readings would impose on daily life.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, wealthy_nation_consumers, beneficiary,
    moderate, biographical, constrained, national).

% Inherit whatever climate trajectory results if decoupling and technology scale-up prove insufficient or too slow relative to the carbon budget. Have no seat in current policy negotiation, no capacity to renegotiate the bet being made on their behalf, and bear compounding physical risk if the mitigation-priority wager fails to deliver on schedule.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Employed in coal, oil, gas, and heavy industry sectors slated for phase-down under carbon pricing; bear job loss and regional economic disruption as the direct transition cost, often without commensurate retraining or relocation support built into the pricing mechanism itself.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_intensive_workers, payer,
    powerless, biographical, constrained, regional).

% Contributed least historically to cumulative emissions but face the earliest and most severe physical impacts; the mitigation-priority reading's growth-preserving framing in wealthy nations slows the pace of emissions cuts relative to what their exposure would require, and offers technology transfer and finance that is frequently underdelivered.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, global_south_frontline_states, payer,
    powerless, generational, trapped, global).

% Live on or near land increasingly allocated to large-scale carbon dioxide removal, afforestation, or bioenergy projects that mitigation-priority pathways require at scale to hit net-zero targets without cutting growth. Face land-use displacement and water-resource competition driven by a technology bet made in distant capitals.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, cdr_dependent_land_communities, payer,
    powerless, generational, trapped, regional).

% Draft and ratify the international and domestic frameworks (carbon pricing schedules, innovation subsidy programs, net-zero targets) that operationalize the mitigation-priority reading as the legitimate default. Mediate between technology-sector lobbying, finance-sector risk models, and civil-society pressure from excluded groups.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_policy_negotiators, agenda_setter,
    institutional, generational, analytical, global).

% Argue that resilience infrastructure for already-locked-in warming, or structural economic transformation away from growth dependency, should be co-equal or prior to the technology-and-pricing pathway. Present at climate conferences and in academic literature but structurally marginal in the treaties and national policy instruments that actually allocate transition finance.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, adaptation_and_degrowth_advocates, excluded,
    organized, generational, constrained, global).

% Produce the integrated assessment models and carbon budget analyses that the mitigation-priority reading cites as its evidentiary basis, while also publishing the uncertainty ranges and negative-emissions feasibility caveats that qualify how confidently the decoupling bet can be made.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_scientists_and_iam_modelers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns a very large number of independent economic actors — firms, states, consumers — around a single price signal and a shared technology-deployment trajectory, avoiding the coordination failure of each actor waiting for others to decarbonize first, while allowing existing growth-based institutions (finance, trade, employment) to continue operating largely unchanged in structure.
% TRANSFER_FUNCTION: Moves transition costs from current high-consumption populations and incumbent capital (who retain growth and receive subsidized innovation pathways) onto carbon-intensive workers (job displacement), Global South frontline states (accelerated physical impacts from a slower-than-required emissions trajectory), CDR-dependent land communities (land-use displacement for negative-emissions infrastructure), and future generations (residual climate risk if the decoupling bet does not close in time).
% ABSENT_VOICES: Future generations have no representative seat in any negotiating body that sets the pace of the mitigation-priority pathway. Adaptation-priority and degrowth-transformation advocates participate in the discourse but hold no comparable share of institutional agenda-setting power in the treaties, national climate laws, and carbon-market architectures that actually govern resource allocation.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority reading lost its status as the default legitimate climate response overnight, carbon pricing schedules, green-technology subsidy programs, and net-zero-by-technology national commitments would lose their normative anchor; finance sector green-bond frameworks tied to growth-compatible transition pathways would need re-underwriting; and policy attention and public transition finance could shift substantially toward adaptation infrastructure or degrowth-oriented structural reforms, redirecting trillions in projected investment flows.
% FOUNDING_PROBLEM: Early 1990s–2000s climate policy needed a framework that could secure buy-in from high-emitting industrialized economies and their electorates, who would not accept mandated economic contraction; carbon pricing and technology-led decoupling offered a path that promised to address emissions without requiring abandonment of growth as the organizing economic objective.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent energy-technology firms and finance-sector institutions attest the founding problem remains live and the pathway is working as intended, citing falling renewable costs and expanding carbon markets. Independent sources outside the beneficiary set — IPCC working-group synthesis reports, Global South negotiating blocs (e.g. AOSIS, LDC Group), and academic decoupling-feasibility literature — corroborate that absolute decoupling at the pace required by remaining carbon budgets has not yet been empirically demonstrated at the scale the reading assumes, making the founding problem's resolution status genuinely disputed rather than settled.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects a substantial but not extreme transfer: real abatement and real technology deployment occur (this is not pure extraction), but the growth-preservation commitment systematically shifts the pace and cost of adjustment onto workers, frontline states, and future generations who have no comparable seat in setting that pace. Suppression (0.42) is moderate — alternative readings are not banned, but treaty architecture, carbon-market infrastructure, and finance-sector risk models are all built around this reading, raising the switching cost of adopting adaptation-priority or degrowth-transformation approaches at any comparable scale. Theater ratio (0.46) is elevated and rising because a growing share of reported 'progress' (voluntary offset markets, net-zero pledges without near-term binding trajectories) functions more as legitimacy performance than as verified abatement — this is descriptively true independent of the tangled_rope claim and is exactly the kind of divergence the engine is built to surface. Accessibility collapse (0.4) is moderate: the pricing-and-innovation toolkit has become the dominant policy vocabulary, but adaptation and degrowth alternatives remain conceptually and institutionally available, just resourced far below parity. Resistance (0.55) is real and organized — from Global South negotiating blocs, degrowth and climate-justice movements, and increasingly from IPCC WG3 authors flagging decoupling-feasibility uncertainty.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent technology firms, carbon-market intermediaries, growth-dependent finance, and wealthy-nation consumers sit near the beneficiary end of directionality: the reading is structured to preserve their institutional position and consumption patterns while routing transition finance through mechanisms they control or benefit from. Future generations, carbon-intensive workers, Global South frontline states, and CDR-dependent land communities sit near the target end: they bear either the direct transition costs (workers) or the deferred/externalized costs of a pathway whose adequacy is empirically uncertain (frontline states, future generations, land communities). Future generations in particular are trapped by construction — a civilizational time horizon with zero present-tense voice in the negotiating architecture is the paradigm case of a directionality-target with no exit option at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing high-emitter buy-in without mandating growth contraction) is genuinely contested rather than resolved or dead: incumbent beneficiaries attest it remains live and functioning, while independent corroborating sources (IPCC synthesis reports, Global South negotiating blocs, decoupling-feasibility literature) indicate the empirical premise the reading depends on — that absolute decoupling can occur fast enough to meet remaining carbon budgets — has not been demonstrated at the required scale. This is not yet a case of mandatrophy (a dead problem propping up a persisting arrangement); it is a live bet whose resolution is still pending, which is why founding_problem_status is authored as contested rather than dead. Should decoupling continue to underperform IPCC pathways through the 2030s while growth-preservation policy commitments hold fixed, the T17 abductive trigger on rising base_extractiveness (0.38→0.58 over the interval) would be exactly the signal indicating the reading has shifted from good-faith coordination bet toward extraction dressed as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_at_required_scale,
    'Can absolute decoupling of GDP growth from emissions occur fast enough, at global scale, to remain within remaining carbon budgets consistent with agreed temperature targets?',
    'Longitudinal tracking of realized global decoupling rates against IPCC-consistent required trajectories; if realized rates persistently fall short of required rates over a decade or more, the feasibility premise is empirically falsified for practical policy purposes.',
    'If decoupling proves infeasible at the required pace, the mitigation-priority reading''s growth-preservation commitment directly produces the outcome its future-generations victim class bears — converting what is currently an uncertain bet into a realized transfer, and strengthening the case for the degrowth-transformation or adaptation-priority readings as the more defensible legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_at_required_scale, empirical, 'Whether the central technological premise of this reading is achievable in time.').

omega_variable(
    kernel_reading_relationship_mitigation_vs_siblings,
    'Are the three readings of climate_response_legitimacy genuinely mutually exclusive commitments, or can elements of adaptation and degrowth be incorporated into a mitigation-priority framework without abandoning its growth-preservation core?',
    'Track whether actual policy portfolios (e.g. EU Green Deal, US IRA) combine elements from multiple readings in ways that remain coherent, versus whether growth-preservation commitments structurally crowd out adaptation finance and degrowth-consistent reforms in practice.',
    'If the readings are more compatible in practice than in ideal-typical form, treating them as sharply distinct constraints (as this decomposition does) may overstate the tension; if growth-preservation systematically crowds out the alternatives, the decomposition''s coexists_with/influences relations are the correct structural characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_mitigation_vs_siblings, conceptual, 'Whether the kernel''s three readings are structurally exclusive or practically blendable.').

omega_variable(
    cdr_scale_up_land_and_technology_risk,
    'Is large-scale carbon dioxide removal (BECCS, afforestation, direct air capture) a genuinely available technology pathway at the scale mitigation-priority net-zero targets assume, or is it a placeholder that defers the growth-emissions tension onto future land-use and technology-deployment risk?',
    'Track actual CDR deployment rates and land-use conflict incidence against IPCC scenario assumptions requiring gigaton-scale removal by mid-century.',
    'If CDR scale-up underdelivers, the reading''s implicit bet shifts more of its cost onto CDR-dependent land communities and future generations than currently priced into the extractiveness metric, and the theater_ratio trajectory would likely need to be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scale_up_land_and_technology_risk, empirical, 'Whether CDR technology dependency is a real pathway or a deferred-cost placeholder.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__mitigation_priority, theater_ratio, 6, 0.33).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__mitigation_priority, theater_ratio, 12, 0.37).
narrative_ontology:measurement(clim_tr_t18, climate_response_legitimacy__mitigation_priority, theater_ratio, 18, 0.41).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__mitigation_priority, theater_ratio, 24, 0.44).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.46).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__mitigation_priority, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__mitigation_priority, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(clim_be_t18, climate_response_legitimacy__mitigation_priority, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__mitigation_priority, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__mitigation_priority, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__mitigation_priority, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(clim_su_t18, climate_response_legitimacy__mitigation_priority, suppression_requirement, 18, 0.38).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__mitigation_priority, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Three-member constraint family decomposing the natural-language 'legitimate climate response' claim, per the ε-invariance principle: this file (mitigation_priority), climate_response_legitimacy__adaptation_priority, and climate_response_legitimacy__degrowth_transformation. Each reading has a distinct beneficiary/victim structure, distinct claimed type, and distinct ε — they are not the same constraint measured three ways. Mitigation-priority is authored here with beneficiaries concentrated in incumbent technology/finance capital and victims spanning workers, frontline states, land communities, and future generations; the sibling files should be authored with their own independent metrics rather than derived from this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
