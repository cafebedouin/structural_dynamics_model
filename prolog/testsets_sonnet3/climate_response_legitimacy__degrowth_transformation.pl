% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation Reading of Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth_transformation reading of the
 *   contested climate_response_legitimacy kernel: the claim that legitimate
 *   climate response in wealthy nations requires deliberately dismantling the
 *   growth imperative through structural economic transformation — universal
 *   basic services, working time reduction, and democratic firm ownership —
 *   rather than pursuing growth-preserving decoupling (the
 *   mitigation_priority reading) or accepting the warming trajectory and
 *   building resilience (the adaptation_priority reading). ε is authored for
 *   the standing arrangement this reading contests — the current
 *   growth-oriented wealthy-economy structure it holds illegitimate —
 *   assessed by this reading's own lights, not for the transformed
 *   post-degrowth arrangement it endorses.
 *
 * KEY AGENTS:
 *   - degrowth_policy_coalition: agenda_setter (organized/analytical) — advocates and drafts the structural transformation program
 *   - current_wealthy_nation_workers: primary payer (powerless/trapped) — bears income reduction and structural dislocation
 *   - shareholding_middle_class: secondary payer (moderate/constrained) — asset returns compressed
 *   - future_generations: primary beneficiary (powerless/trapped, civilizational horizon) — inherits stabilized climate without technological dependency
 *   - climate_vulnerable_nations: beneficiary (moderate/trapped) — reduced impacts from wealthy-nation demand contraction
 *   - growth_sector_industry: excluded (powerful/mobile) — defined as adversary rather than consulted party
 *   - climate_science_and_economics_researchers: observer (analytical) — contested empirical arbiter of decoupling feasibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.42).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation Reading of Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '5a990145-9d6d-40a5-ad60-241692a69c07').
narrative_ontology:cs_kernel_codification('5a990145-9d6d-40a5-ad60-241692a69c07', distributed).
narrative_ontology:cs_authority_grounding('5a990145-9d6d-40a5-ad60-241692a69c07', distributed).
narrative_ontology:cs_reading_relation('5a990145-9d6d-40a5-ad60-241692a69c07', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('5a990145-9d6d-40a5-ad60-241692a69c07', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_axiom('5a990145-9d6d-40a5-ad60-241692a69c07', foundational, growth_imperative_incompatible_with_carbon_budget).
narrative_ontology:cs_axiom_status(growth_imperative_incompatible_with_carbon_budget, holdable).
narrative_ontology:cs_axiom_grounding('5a990145-9d6d-40a5-ad60-241692a69c07', growth_imperative_incompatible_with_carbon_budget, empirically_contingent).
narrative_ontology:cs_axiom('5a990145-9d6d-40a5-ad60-241692a69c07', foundational, present_generation_cost_bearing_obligation_to_future).
narrative_ontology:cs_axiom_status(present_generation_cost_bearing_obligation_to_future, holdable).
narrative_ontology:cs_axiom_grounding('5a990145-9d6d-40a5-ad60-241692a69c07', present_generation_cost_bearing_obligation_to_future, deontological).
narrative_ontology:cs_reference_frame('5a990145-9d6d-40a5-ad60-241692a69c07', post_1970s_growth_consensus_political_economy).
narrative_ontology:cs_drift_state('5a990145-9d6d-40a5-ad60-241692a69c07', contemporary_climate_emergency_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5a990145-9d6d-40a5-ad60-241692a69c07', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, ecosystem_stability_claimants).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_wealthy_nation_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, shareholding_middle_class).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, growth_dependent_pension_holders).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, biophysical_limits_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates and drafts the structural transformation program — UBS, working-time reduction, democratized firm ownership — arguing that the growth imperative itself is structurally incompatible with remaining carbon budgets. Sets the agenda for what counts as a legitimate response, framing incremental mitigation as inadequate. Bears reputational and political risk from advocating an unpopular structural break; has no direct exit from the argument they've staked their political identity on.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_policy_coalition, agenda_setter,
    organized, generational, analytical, national).

% Would absorb income reduction, job dislocation in growth-dependent sectors, and life-plan disruption if working-time reduction and firm restructuring were implemented at the scale the reading demands. They did not choose the emissions trajectory that created the crisis and have limited capacity to relocate, retrain, or otherwise exit the transformation's direct costs within a single career horizon.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_wealthy_nation_workers, payer,
    powerless, biographical, trapped, national).

% Holds retirement savings, home equity, and consumption expectations built on continued growth. Democratic firm ownership and de-growth of profit-driven sectors would compress the asset returns this group depends on. Some capacity to diversify into non-equity assets, but pension structures and housing markets create real lock-in.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, shareholding_middle_class, payer,
    moderate, biographical, constrained, national).

% Depend on continued economic growth and investment returns to fund retirement income already committed under existing pension formulas. A deliberate contraction of growth-oriented finance threatens the funding basis of pensions already promised, with essentially no individual ability to renegotiate or exit the arrangement.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_dependent_pension_holders, payer,
    powerless, biographical, trapped, national).

% Cannot participate in the current political process but would inherit either a stabilized climate and altered economic structure, or continued warming under a preserved-growth model. This reading holds that they benefit from a genuine structural break now — including via avoiding dependency on unproven future technology — rather than from mitigation-through-innovation promises that defer the hardest choices onto them.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Bear disproportionate climate impacts despite minimal historical emissions responsibility. This reading holds their interests are better served by wealthy-nation demand contraction (freeing carbon budget headroom and reducing resource extraction pressure) than by growth-preserving technological decoupling, which this reading treats as too slow and too uncertain to protect them in time.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_nations, beneficiary,
    moderate, generational, trapped, global).

% Multinational firms in carbon-intensive and growth-dependent sectors are structurally positioned against this reading's premises but are treated within the reading as an interest to be overridden rather than a voice to be accommodated; they retain capital mobility and lobbying capacity the other payer groups lack, and are largely absent from the degrowth coalition's internal deliberation except as an adversary to be defeated.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_sector_industry, excluded,
    powerful, biographical, mobile, global).

% Hold that decoupling growth from emissions via technology and carbon pricing is both more feasible and less socially costly. They are structurally excluded from this reading's own legitimacy claim (the reading defines their preferred path as inadequate by construction) even though they operate in the same policy space and compete for the same institutional attention and finance.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, mitigation_priority_advocates, excluded,
    organized, generational, mobile, global).

% Assess whether the carbon-budget math actually requires demand contraction in wealthy economies at the pace and scale the reading asserts, or whether decoupling and adaptation pathways can achieve comparable outcomes. Their empirical findings are contested inputs to, not neutral arbiters of, the underlying value conflict.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_science_and_economics_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates wealthy-nation populations around a shared, deliberate reduction in aggregate material throughput and paid working time, replacing growth-dependent income and status structures with universal basic services and democratized ownership, on the premise that this is the only route compatible with remaining carbon budgets.
% TRANSFER_FUNCTION: Moves consumption capacity, asset returns, and labor-market security away from current workers, shareholders, and pension holders in wealthy nations, toward a stabilized climate trajectory and reduced resource-extraction pressure that primarily benefit future generations and climate-vulnerable populations elsewhere.
% ABSENT_VOICES: Growth-sector industry and mitigation-priority advocates would object that the transformation is unnecessary, infeasible, or achievable through less disruptive means; they participate in surrounding policy debate but are structurally defined as the problem within this reading rather than consulted as legitimate co-authors of the solution.
% DISAPPEARANCE_RATIONALE: If the degrowth transformation program disappeared as a live political demand, wealthy-nation growth trajectories would continue largely unaltered in the near term (advocates say this confirms the world depends on the demand to force change; opponents say the world would be materially unchanged because the program was never implemented at scale, only argued for). The dispute over which is true is itself part of the kernel contest.
% FOUNDING_PROBLEM: Empirical findings that continued aggregate economic growth in wealthy nations is difficult or impossible to fully decouple from emissions and resource throughput at the pace required to meet climate stabilization targets, combined with a judgment that technological and pricing solutions alone will arrive too slowly.
% FOUNDING_PROBLEM_CORROBORATION: Some ecological economists and biophysical-limits researchers outside the degrowth political coalition corroborate that absolute decoupling at required scale is empirically unproven; mainstream climate-economics institutions (IEA, most IPCC WG3 scenario modeling) corroborate the opposing view that decoupling pathways exist and are more tractable. No consensus corroboration exists from a genuinely disinterested outside body — the empirical question remains open and contested across expert communities.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, contested).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that this reading imposes real, concentrated near-term costs (income, asset value, retirement security) on current wealthy-nation populations in the name of a benefit that accrues mostly to non-present or non-domestic parties. Suppression is moderate (0.42) rather than high: the reading operates as political advocacy and, where partially implemented, as policy requiring democratic buy-in — it does not yet possess the coercive enforcement apparatus of an entrenched constraint, though the requires_active_enforcement flag reflects that full implementation (mandated working-time caps, ownership restructuring) would require real state enforcement machinery. Resistance is high (0.78) because the payer groups are organized, electorally powerful, and actively contest the program. Accessibility collapse is moderate-low (0.35): mitigation_priority and adaptation_priority remain live, non-suppressed alternatives — the degrowth reading has not foreclosed them, it competes with them.
 *
 * PERSPECTIVAL GAP:
 *   From the degrowth_policy_coalition's seat, the arrangement is a genuine coordination mechanism solving an intergenerational and biophysical crisis; from the current_wealthy_nation_workers' seat, the same structural transformation computes as an imposed cost transfer they did not choose and cannot readily exit. The tangled_rope classification captures both faces existing in the same structure simultaneously — real coordination function (climate stabilization) and asymmetric extraction (concentrated near-term cost on a population with limited exit) — rather than forcing a choice between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and climate-vulnerable nations are beneficiaries under this reading's own accounting — they bear no present cost and gain from reduced warming, hence low d. Current wealthy-nation workers, shareholders, and pension holders are victims — they absorb the income and asset costs of the transformation with limited exit (trapped/constrained), hence high d. The degrowth_policy_coalition is agenda_setter rather than beneficiary because it does not personally collect from the transformation — it bears reputational and political risk in advocating it, which differentiates its position from a captured beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — biophysical limits to growth-emissions decoupling at the pace required — is not dead by the degrowth coalition's own account, but mainstream climate-economics institutions corroborate the opposing empirical claim. This divergence in corroboration is exactly why founding_problem_status is 'contested' rather than 'live': the classification must not simply accept the advocating coalition's self-assessment of its own necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_ambiguity,
    'Is absolute decoupling of wealthy-nation GDP growth from emissions and resource throughput achievable at the pace required to meet climate stabilization targets, or is the growth imperative structurally incompatible with remaining carbon budgets?',
    'Longitudinal empirical tracking of decoupling rates in economies pursuing aggressive carbon pricing and green-technology deployment, compared against required decarbonization trajectories under IPCC pathways; resolution would require multi-decade data the current interval does not yet contain.',
    'If decoupling proves achievable at required pace, this reading''s founding premise weakens substantially and the constraint''s legitimacy claim shifts toward the mitigation_priority reading. If decoupling proves structurally impossible at required scale, this reading''s founding problem is vindicated as live rather than contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility_ambiguity, empirical, 'Whether the empirical premise underlying the degrowth reading''s necessity claim is correct.').

omega_variable(
    cost_bearer_consent_and_political_feasibility,
    'Can the structural transformation this reading demands be implemented through legitimate democratic processes with the informed consent of the cost-bearing population, or does its implementation require overriding majoritarian preference for continued growth?',
    'Track electoral outcomes, referenda, and policy adoption in jurisdictions where degrowth-adjacent policies (working-time reduction, UBS pilots) are proposed; assess whether adoption occurs through ordinary democratic consent or requires technocratic/emergency-powers implementation.',
    'If implementation consistently requires bypassing majoritarian consent, the requires_active_enforcement flag and the tangled_rope classification are strongly reinforced — the coordination function persists but rides on suppressed political consent. If consent is achieved through ordinary democratic majorities, extraction is better read as a legitimated distributive choice rather than imposed cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_bearer_consent_and_political_feasibility, empirical, 'Whether the transformation can proceed with genuine democratic consent from those bearing its costs.').

omega_variable(
    kernel_framing_choice,
    'Is the underlying disagreement across the three kernel readings genuinely about what climate legitimacy requires, or is it a disagreement about empirical decoupling feasibility dressed in normative language?',
    'Structured elicitation separating readings'' factual claims (about decoupling rates, technology timelines, adaptation costs) from their normative claims (about whose costs count, what discount rate applies to future generations); test whether readings converge once factual disagreements are resolved.',
    'If the disagreement is mostly factual, the three readings might converge under updated evidence, undermining the claim that this is a genuine value-kernel contest rather than an empirical dispute misclassified as one. If a genuine normative residue remains after factual convergence, the kernel framing is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the kernel contest is genuinely normative or is a factual disagreement in normative clothing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t4, climate_response_legitimacy__degrowth_transformation, theater_ratio, 4, 0.22).
narrative_ontology:measurement(clim_tr_t8, climate_response_legitimacy__degrowth_transformation, theater_ratio, 8, 0.25).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__degrowth_transformation, theater_ratio, 12, 0.27).
narrative_ontology:measurement(clim_tr_t16, climate_response_legitimacy__degrowth_transformation, theater_ratio, 16, 0.29).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t4, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(clim_be_t8, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(clim_be_t16, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(climate_response_legitimacy__degrowth_transformation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'legitimate climate response' per the ε-invariance principle. Each reading (degrowth_transformation, mitigation_priority, adaptation_priority) has its own beneficiary/victim structure, its own ε, and its own claimed type, and is authored as a separate file. They are linked here as a constraint family rather than merged into one story with an observable parameter, because their extraction profiles, cost-bearer sets, and feasibility risks differ structurally rather than merely by measurement angle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
