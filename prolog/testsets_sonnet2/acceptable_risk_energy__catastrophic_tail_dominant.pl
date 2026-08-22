% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Reading of Acceptable Energy Risk
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This story instantiates the catastrophic-tail-dominant reading of the
 *   contested 'acceptable energy risk' kernel: the position that avoiding
 *   low-probability, high-salience, irreversible catastrophic outcomes
 *   (nuclear accidents) is normatively prior to minimizing aggregate expected
 *   harm, even when the aggregate-harm-minimizing pathway (continued or
 *   expanded fossil generation, or nuclear expansion) produces a
 *   statistically larger but distributed body count. Under this reading,
 *   nuclear accident risk enters the victim-weighting with effectively
 *   unbounded salience while fossil-combustion mortality is treated as a
 *   background, reversible, already-priced-in cost. This is ONE reading among
 *   three siblings sharing the kernel (expected_value_dominant,
 *   option_value_preserving); those are separate constraint stories with
 *   their own ε and stakeholder sets, not alternative measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - incumbent_fossil_fuel_operators: structural beneficiary of nuclear-pathway suppression
 *   - anti_nuclear_advocacy_organizations: agenda-setter whose institutional identity is fused to the tail-dominant frame
 *   - risk_assessment_regulators: agenda-setter administering the asymmetric standard
 *   - populations_exposed_to_fossil_fuel_air_pollution: primary payer, structurally invisible to the metric
 *   - future_generations_facing_accelerated_climate_harm: payer with no present seat
 *   - decision_theorists_and_risk_analysts: analytical observer of the tradeoff
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.61).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.61).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Reading of Acceptable Energy Risk").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'f365ac9a-1864-40a8-aed8-993062e11b49').
narrative_ontology:cs_kernel_codification('f365ac9a-1864-40a8-aed8-993062e11b49', distributed).
narrative_ontology:cs_authority_grounding('f365ac9a-1864-40a8-aed8-993062e11b49', distributed).
narrative_ontology:cs_reading_relation('f365ac9a-1864-40a8-aed8-993062e11b49', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('f365ac9a-1864-40a8-aed8-993062e11b49', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('f365ac9a-1864-40a8-aed8-993062e11b49', foundational, irreversible_concentrated_harm_weighted_infinitely).
narrative_ontology:cs_axiom_status(irreversible_concentrated_harm_weighted_infinitely, holdable).
narrative_ontology:cs_axiom_grounding('f365ac9a-1864-40a8-aed8-993062e11b49', irreversible_concentrated_harm_weighted_infinitely, deontological).
narrative_ontology:cs_axiom('f365ac9a-1864-40a8-aed8-993062e11b49', secondary, distributed_reversible_harm_discounted_as_background).
narrative_ontology:cs_axiom_status(distributed_reversible_harm_discounted_as_background, holdable).
narrative_ontology:cs_axiom_grounding('f365ac9a-1864-40a8-aed8-993062e11b49', distributed_reversible_harm_discounted_as_background, instrumental).
narrative_ontology:cs_reference_frame('f365ac9a-1864-40a8-aed8-993062e11b49', post_three_mile_island_precautionary_standard).
narrative_ontology:cs_drift_state('f365ac9a-1864-40a8-aed8-993062e11b49', post_fukushima_operational_data_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f365ac9a-1864-40a8-aed8-993062e11b49', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, coal_and_gas_dependent_regional_utilities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, populations_exposed_to_fossil_fuel_air_pollution).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, displaced_communities_near_retired_nuclear_sites).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_facing_accelerated_climate_harm).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_workers_and_engineers).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_for_irreversible_harms).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, catastrophic_risk_asymmetry_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continue operating coal and gas generation capacity that would otherwise face nuclear competition; the catastrophic-tail framing of nuclear risk keeps regulatory and permitting barriers high for the competing pathway, extending the operating life and market share of existing fossil assets without those operators needing to argue the merits of fossil fuel risk directly.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Set and maintain the risk-assessment frame in public discourse and regulatory comment processes, emphasizing Chernobyl- and Fukushima-scale tail events as definitive evidence against nuclear expansion. Their institutional identity and funding base are constituted around this framing; abandoning it would dissolve their organizational purpose.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Operate generation portfolios tied to fossil infrastructure with long depreciation schedules; lobby regulators using catastrophic-tail nuclear risk arguments to slow licensing of nuclear alternatives that would otherwise undercut their capacity factor and pricing position.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, coal_and_gas_dependent_regional_utilities, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, coal_and_gas_dependent_regional_utilities, agenda_setter).

% Bear the distributed, statistically diffuse mortality and morbidity burden of particulate and combustion pollution from continued fossil generation. Because these deaths are dispersed across time, geography, and cause-of-death coding, they never register as a single countable catastrophe the risk framework is built to avoid — they are structurally invisible to the metric that governs the constraint.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, populations_exposed_to_fossil_fuel_air_pollution, payer,
    powerless, biographical, trapped, regional).

% Live with the economic and social aftermath of premature nuclear plant closures driven by tail-risk framing rather than operational safety records at their specific facility; lost jobs, depressed local tax bases, and stranded infrastructure follow closures that were not triggered by any incident at their own plant.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, displaced_communities_near_retired_nuclear_sites, payer,
    powerless, biographical, trapped, local).

% Inherit a decarbonization trajectory slowed by the foreclosure of nuclear scale-up, since the catastrophic-tail framing raises the effective cost and delay of nuclear licensing relative to continued fossil burn. They have no seat in the risk-assessment process that sets today's energy mix.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_facing_accelerated_climate_harm, payer,
    powerless, civilizational, trapped, global).

% Face a shrinking, reputationally stigmatized labor market as the tail-dominant frame suppresses new construction and accelerates decommissioning; career paths in the sector are treated as inherently suspect regardless of individual plant safety performance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_workers_and_engineers, payer,
    moderate, biographical, constrained, national).

% Administer licensing and safety review processes that formally encode the catastrophic-tail weighting — e.g., as-low-as-reasonably-achievable standards applied asymmetrically to nuclear relative to fossil combustion permitting. Could in principle reweight the framework toward expected-aggregate-harm metrics but face institutional and political costs for doing so that exceed any individual regulator's incentive to act.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_assessment_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Study the comparative mortality-per-TWh literature and the psychological/political economy of dread-risk aversion; can characterize the structural tradeoff between tail-avoidance and aggregate-harm-minimization but hold no enforcement power over which framework regulators adopt.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theorists_and_risk_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible standard for what counts as an acceptable energy risk that regulators, courts, and publics can invoke without re-litigating first principles for every project; the catastrophic-tail weighting genuinely reduces public anxiety about irreversible, low-probability, high-salience events and creates political durability for siting and licensing decisions.
% TRANSFER_FUNCTION: Moves realized harm from a concentrated, visible, attributable channel (nuclear accidents) to a distributed, statistically invisible, hard-to-attribute channel (fossil combustion mortality and accelerated climate harm), while moving political and reputational risk away from regulators and advocacy organizations who can point to the tail-avoidance standard as due diligence.
% ABSENT_VOICES: Populations bearing fossil pollution mortality have no comparably salient, single-incident event to mobilize around and are structurally absent from the risk-comparison conversation; future generations bearing climate harm have no representation in current licensing or public-comment processes at all.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant standard were replaced overnight by an aggregate-expected-harm standard, nuclear licensing timelines and cost structures would shift substantially, fossil utilities would lose a key regulatory-delay lever against nuclear competitors, anti-nuclear advocacy organizations would need to reconstitute their argument around a different metric, and capital allocation across the energy sector would visibly reorganize within a single investment cycle.
% FOUNDING_PROBLEM: Built to give democratic societies a principled way to say no to technologies whose failure modes are catastrophic, irreversible, and geographically concentrated — a genuine response to the felt asymmetry between a single Chernobyl-scale event and a diffuse mortality rate, when public trust in expert risk quantification was low.
% FOUNDING_PROBLEM_CORROBORATION: Comparative mortality-per-TWh analyses from energy-systems researchers and public-health epidemiologists outside both the nuclear and fossil-fuel industries attest that the founding problem (justified public fear of a specific class of failure) has been substantially answered by post-Fukushima safety-system redesign and multi-decade operational data, while the framework's suppressive effect on the nuclear pathway persists independent of that evidence; fossil-fuel-industry-funded and anti-nuclear-advocacy-funded sources are excluded from this corroboration as interested parties.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 (moderate-high, rising over the interval) because the reading systematically transfers realized harm from a countable, attributable channel to an uncounted, unattributable one — this is a real transfer, not a wash, even though no single agent 'collects' the transferred harm as a rent in the ordinary sense; the beneficiaries collect market share, regulatory durability, and organizational continuity, which is the extraction. Suppression is authored higher (0.78) and rising because the reading's persistence depends on actively maintained asymmetric permitting and licensing barriers against the nuclear pathway specifically, not on the pathway's comparative safety record, which is a structural (not merely rhetorical) suppression mechanism. Theater ratio (0.42, rising) reflects the growing share of risk-assessment activity that reproduces dread-risk salience arguments in comment processes and litigation rather than performing fresh comparative-mortality analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the anti-nuclear advocacy and fossil-incumbent seats, this is coherent, principled risk conservatism — a rope-like coordination standard protecting the public from irreversible catastrophe. From the seats of fossil-pollution-exposed populations and future generations, the same standard operates as an enforced transfer mechanism that launders a larger aggregate harm through statistical invisibility. The engine computes this divergence from the structural beneficiary/victim/enforcement data; the claimed_type (tangled_rope) is authored to reflect that BOTH a genuine coordination function (real public anxiety about irreversible catastrophe, a real coordination benefit in licensing legibility) AND asymmetric extraction (the transfer to distributed victims) are present simultaneously — this is precisely the tangled_rope signature rather than a pure snare, because the coordination function is not merely cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil incumbents and dependent utilities sit near the beneficiary end: the tail-dominant standard suppresses their principal long-run competitor without requiring them to defend fossil risk on its own terms. Anti-nuclear advocacy organizations are also structural beneficiaries in the narrow sense that the frame IS their organizational output, even though they do not extract material rent — this is why they carry agenda_setter as primary role rather than beneficiary in the ordinary rent-collection sense; their benefit is organizational continuity, not revenue. Exposed pollution populations, displaced nuclear-site communities, future generations, and nuclear workers all sit near the target end: they bear costs generated by the frame's asymmetric weighting while having no comparable voice in setting it. Regulators are agenda-setters with constrained exit — they administer the standard but bear political costs for revising it that exceed their individual incentive to do so, which is itself part of why the constraint persists by inertia in the regulatory layer even where it is actively defended in the advocacy layer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justified fear of catastrophic, irreversible, geographically concentrated failure in an era of low public trust in expert risk quantification) is contested rather than cleanly dead: post-Fukushima safety redesign and multi-decade operational data have substantially answered the empirical component of the original fear for modern reactor designs, while the frame's suppressive effect on nuclear licensing persists at the same intensity regardless of that evidence. This is the mandatrophy signature: the standard's enforcement infrastructure (permitting asymmetry, advocacy mobilization, regulatory caution) has not decayed in proportion to the decay of the empirical justification, so classifying this as tangled_rope rather than either pure rope (which would ignore the transfer to invisible victims) or pure snare (which would ignore the real coordination function around catastrophic-risk aversion) prevents both a false-coordination and a false-extraction misreading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_versus_aggregate_harm_weighting,
    'Is asymmetric weighting of catastrophic, low-probability, concentrated harm over higher-probability, distributed aggregate harm a defensible normative axiom (irreversibility and identifiability matter intrinsically) or a cognitive-bias artifact (dread risk, availability heuristic) that a corrected decision procedure should discount?',
    'This is not resolvable by additional mortality data alone — it is a question about which axiology governs risk aggregation under fixed empirical facts. Convergent philosophical work on catastrophic risk ethics (e.g., ambiguity aversion, non-identity problem treatments) combined with revealed-preference studies of how affected populations themselves would trade off tail-avoidance against aggregate harm reduction if fully informed would narrow but not close the gap.',
    'If the tail-asymmetric weighting is normatively defensible on its own terms, the coordination function this story attributes to the constraint is stronger than authored and the tangled_rope classification tilts toward rope; if the weighting is primarily a bias artifact with no independent normative standing, the coordination story is closer to pure cover and the classification tilts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tail_risk_versus_aggregate_harm_weighting, preference, 'Whether catastrophic-tail asymmetry is a defensible axiology or a bias-driven artifact — the central normative fork of this reading.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading diverge from expected_value_dominant and option_value_preserving — is the disagreement about the FACTS (comparative mortality-per-TWh data), about the METRIC (whether mortality-per-TWh is the right aggregation unit at all), or about the ETHICS (whether identifiable, concentrated, involuntary harm should be weighted differently from statistical, distributed, background harm)?',
    'Explicit decomposition of each reading''s argument into its factual, metric, and ethical components, cross-examined against the same underlying operational and epidemiological dataset, would locate the disagreement precisely. Current public discourse conflates all three layers.',
    'If the disagreement is purely factual, it should in principle resolve with better data and the three readings should converge; if it is metric or ethical, no amount of additional data resolves it and the three constraints remain permanently distinct siblings sharing the kernel rather than converging measurements of one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the structural disagreement among the three kernel readings — factual, metric, or ethical layer.').

omega_variable(
    regulator_agency_versus_inertia,
    'Do risk_assessment_regulators actively choose the tail-dominant weighting as a considered policy position, or has the asymmetric standard become self-perpetuating institutional inertia that no individual regulator has sufficient incentive to revisit?',
    'Internal regulatory-agency deliberation records, comparative analysis of how licensing standards changed (or failed to change) following major post-Fukushima safety data releases, and interviews with regulators about revision costs would distinguish active agenda-setting from administrative inertia.',
    'If regulatory maintenance of the standard is genuine agenda-setting, the tangled_rope classification with regulators as a co-beneficiary-adjacent enforcer is well-supported; if it is pure inertia with no one meaningfully profiting from the specific regulatory posture, the regulatory layer specifically (though not the advocacy/fossil-incumbent layer) would read closer to a piton riding inside the larger tangled_rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_agency_versus_inertia, empirical, 'Whether regulatory maintenance of the tail-dominant standard is active agenda-setting or administrative inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(acce_tr_t8, observed).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(acce_tr_t16, observed).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(acce_tr_t24, observed).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(acce_tr_t32, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(acce_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(acce_be_t8, observed).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(acce_be_t16, observed).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(acce_be_t24, observed).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.6).
narrative_ontology:measurement_basis(acce_be_t32, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(acce_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(acce_su_t8, observed).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(acce_su_t16, observed).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(acce_su_t24, observed).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.76).
narrative_ontology:measurement_basis(acce_su_t32, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(acce_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy_expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy_option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'acceptable energy risk' per the epsilon-invariance principle: catastrophic_tail_dominant (this file), expected_value_dominant, and option_value_preserving. Each sibling has its own epsilon, its own beneficiary/victim set, and its own claimed_type — they are not the same constraint measured three ways. All three should link to each other via affects_constraints since a shift in the dominant public reading of one directly changes the political and resource environment the other readings operate in (e.g., a nuclear accident event would strengthen this reading's grip and weaken expected_value_dominant's traction, independent of any change in underlying mortality statistics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
