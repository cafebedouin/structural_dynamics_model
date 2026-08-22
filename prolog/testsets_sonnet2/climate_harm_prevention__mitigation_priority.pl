% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of Legitimate Climate Response (Growth-Compatible Decarbonization)
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the
 *   climate_harm_prevention kernel: legitimate climate response is defined as
 *   emissions reduction via technological substitution (renewables, EVs,
 *   carbon capture) achieved without requiring contraction of aggregate
 *   economic output. Since the Rio Earth Summit (1992) through the Paris
 *   Agreement era, this reading has become the dominant institutional
 *   definition of climate legitimacy in UNFCCC processes, national climate
 *   laws (net-zero targets), and multilateral finance architecture. The
 *   coordination function is real — aligning global capital and policy around
 *   decarbonization pathways solves a genuine collective-action problem that
 *   unilateral national action cannot. But the reading also structurally
 *   privileges future generations (rhetorically) and present
 *   green-technology/carbon-market beneficiaries (materially) while imposing
 *   near-term, concentrated costs on fossil-dependent workers and
 *   carbon-intensive economies, and while marginalizing both the
 *   adaptation-priority reading (which argues near-term resilience for
 *   vulnerable populations should come first) and the degrowth reading (which
 *   argues growth-compatible decarbonization is physically insufficient). Per
 *   the kernel-reading discipline, ε here is authored strictly for the
 *   mitigation-priority arrangement as it actually operates, not for either
 *   sibling reading's preferred alternative.
 *
 * KEY AGENTS:
 *   - future_generations: rhetorical primary beneficiary, no present agency (powerless/trapped)
 *   - green_technology_industries: material beneficiary capturing subsidy and mandate flows (organized/arbitrage)
 *   - incumbent_growth_economies: agenda-setter defining 'legitimate' response around growth-compatible substitution (institutional/arbitrage)
 *   - fossil_fuel_dependent_workers: primary payer of near-term transition cost (powerless/trapped)
 *   - degrowth_advocates and adaptation_focused_vulnerable_states: excluded sibling-reading holders (moderate-to-powerless/constrained-to-trapped)
 *   - climate_science_assessment_bodies: analytical observer whose physical findings are cited selectively by all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.47).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Reading of Legitimate Climate Response (Growth-Compatible Decarbonization)").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '87d347a7-beab-451b-aa0f-23864e76fc17').
narrative_ontology:cs_kernel_codification('87d347a7-beab-451b-aa0f-23864e76fc17', distributed).
narrative_ontology:cs_authority_grounding('87d347a7-beab-451b-aa0f-23864e76fc17', distributed).
narrative_ontology:cs_reading_relation('87d347a7-beab-451b-aa0f-23864e76fc17', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('87d347a7-beab-451b-aa0f-23864e76fc17', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('87d347a7-beab-451b-aa0f-23864e76fc17', foundational, technological_substitution_can_decouple_growth_from_emissions).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_growth_from_emissions, holdable).
narrative_ontology:cs_axiom_grounding('87d347a7-beab-451b-aa0f-23864e76fc17', technological_substitution_can_decouple_growth_from_emissions, empirically_contingent).
narrative_ontology:cs_axiom('87d347a7-beab-451b-aa0f-23864e76fc17', foundational, future_harm_prevention_takes_priority_over_present_distributional_cost).
narrative_ontology:cs_axiom_status(future_harm_prevention_takes_priority_over_present_distributional_cost, holdable).
narrative_ontology:cs_axiom_grounding('87d347a7-beab-451b-aa0f-23864e76fc17', future_harm_prevention_takes_priority_over_present_distributional_cost, instrumental).
narrative_ontology:cs_reference_frame('87d347a7-beab-451b-aa0f-23864e76fc17', rio_unfccc_common_but_differentiated_responsibility).
narrative_ontology:cs_drift_state('87d347a7-beab-451b-aa0f-23864e76fc17', post_paris_net_zero_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('87d347a7-beab-451b-aa0f-23864e76fc17', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_technology_industries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, incumbent_growth_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_export_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_poor_in_transition_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in present policy decisions but are named as the primary intended beneficiary of emissions reduction; their claimed interest in a stable climate is the moral warrant invoked to justify present transition costs, though they have no seat, no vote, and no mechanism to contest how mitigation is designed or paced.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Solar, wind, battery, EV, and carbon-capture firms capture subsidies, mandates, and preferential procurement created by mitigation-priority policy. They can relocate production and lobby across jurisdictions to capture the most favorable incentive regime, and their market growth is directly generated by the policy framework.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_technology_industries, beneficiary,
    organized, generational, arbitrage, global).

% Exchanges, verification bodies, offset developers, and consultancies profit from the compliance architecture (carbon markets, offset registries, ESG reporting) that mitigation-priority policy requires. Their revenue depends on the transition being technocratic and market-mediated rather than structurally simplified.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_market_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Wealthy industrialized states and multilateral bodies (IPCC-adjacent policy coalitions, G7 climate finance frameworks) set the terms of 'legitimate' climate response around technological substitution within continued GDP growth, because this framing avoids contraction of their own consumption base and preserves the growth-legitimacy of their political systems. They administer the mitigation-priority framework as the default meaning of climate responsibility.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, incumbent_growth_economies, agenda_setter,
    institutional, generational, arbitrage, global).

% Coal, oil, and gas extraction and refining workers, plus adjacent supply-chain labor, absorb job loss and regional economic collapse as mitigation policy phases out their industries. Retraining and just-transition funds are typically underfunded relative to the scale of displacement, and relocation is constrained by housing, family, and skill specificity.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% Nations whose export base is coal, oil, gas, or carbon-intensive manufacturing face carbon border adjustments, divestment pressure, and declining terms of trade under mitigation-priority frameworks designed largely by wealthier importing economies. They can negotiate transition finance but have limited leverage to reshape the framework's core assumptions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_export_economies, payer,
    moderate, generational, constrained, national).

% Low-income households bear the near-term cost of carbon pricing, energy price shocks, and mineral-extraction externalities (lithium, cobalt mining for the technology transition) without commensurate present benefit, since the harm being prevented is decades away and the harm being imposed is immediate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_poor_in_transition_regions, payer,
    powerless, immediate, trapped, local).

% Argue that growth-compatible decarbonization is physically impossible within remaining carbon budgets and that planned contraction in wealthy economies is required. Their position is treated as politically unserious within mainstream policy venues (IPCC summaries for policymakers, UNFCCC negotiating texts) and rarely receives a formal hearing in the frameworks that define 'legitimate' response.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, civilizational, constrained, global).

% Small island states and low-lying nations facing near-term existential risk argue resilience and loss-and-damage financing should be prioritized over distant mitigation targets that will not save them in time. Their claims are acknowledged rhetorically in mitigation-priority venues but receive a fraction of the financing commitment mitigation technology receives.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_focused_vulnerable_states, excluded,
    powerless, immediate, trapped, national).

% Bodies like the IPCC synthesize physical evidence on warming trajectories and technology pathways. They document scenarios compatible and incompatible with growth-based mitigation but do not adjudicate which reading of legitimate response should govern policy; their assessments are cited selectively by all three readings.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_science_assessment_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global transition away from fossil energy toward low-carbon technology within existing economic and trade institutions, allowing states, firms, and capital markets to align investment, regulation, and innovation around a shared decarbonization trajectory without requiring wholesale abandonment of growth-based economic organization.
% TRANSFER_FUNCTION: Moves capital and political legitimacy toward green technology sectors and carbon-market infrastructure, moves near-term economic costs (job loss, price increases, extraction externalities) onto fossil-dependent workers and carbon-intensive economies, and defers realized climate benefit to future generations who bear none of the transition cost and hold no seat in designing it.
% ABSENT_VOICES: Degrowth advocates who contest the physical feasibility of growth-compatible decarbonization, and adaptation-focused vulnerable states whose near-term survival needs are subordinated to a mitigation timeline calibrated to century-scale outcomes, are both structurally present in the debate but excluded from defining what counts as 'legitimate' response within mainstream policy architecture (IPCC SPMs, UNFCCC texts, national climate law).
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framing lost its status as the definition of legitimate climate response overnight, green-technology subsidy regimes, carbon markets, and international climate finance architecture built around emissions-reduction targets would lose their legitimating rationale; policy debate would open toward adaptation-first and degrowth alternatives currently marginalized, and the political coalitions currently benefiting from transition-technology investment would lose their privileged claim to represent 'the' climate response.
% FOUNDING_PROBLEM: Rising atmospheric greenhouse gas concentrations from industrial and energy activity threaten severe and potentially irreversible harm to human and ecological systems; some collectively binding response was needed to reduce future emissions before harm became locked in.
% FOUNDING_PROBLEM_CORROBORATION: The underlying physical problem (continued warming from continued emissions) is independently corroborated by climate science bodies outside the coalition of green-technology and carbon-market beneficiaries — the IPCC's physical science basis reports are produced by scientists whose institutional interest is not directly tied to mitigation-priority's specific growth-compatible framing. However, whether growth-compatible technological substitution is an adequate or merely convenient solution to that problem is contested even among climate scientists and economists outside the beneficiary coalition, which is why founding_problem_status is authored 'live' for the underlying harm but the specific mitigation-priority SOLUTION to it remains contested territory addressed by the omega variables below.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects a real but partial extraction: mitigation-priority policy transfers costs from diffuse future benefit onto concentrated present populations (fossil workers, carbon-exporting economies, extraction-adjacent communities) while channeling gains to green-tech and carbon-market intermediaries who did not bear proportional risk. It is not as extractive as a pure snare because the underlying coordination problem (preventing catastrophic warming) is genuine and shared. Suppression (0.47) is moderate: the mitigation-priority framing is enforced through institutional gatekeeping (which proposals count as 'serious' climate policy) rather than direct coercion, but this gatekeeping is real and has intensified as net-zero commitments have become treaty and legal obligations. Theater ratio (0.42) is substantial and rising — voluntary corporate net-zero pledges, offset markets with weak additionality, and carbon accounting exercises have grown faster than verified emissions reductions, a genuine Goodhart-drift signature. Accessibility collapse (0.4) and resistance (0.62) reflect that alternative framings (degrowth, adaptation-priority) remain live and actively contested rather than fully suppressed — this is not a mountain-like closure of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are coded as beneficiaries by declaration but functionally powerless and trapped — they cannot bargain over the terms of the mitigation pathway designed on their behalf, which is why the derived directionality for that seat, despite nominal beneficiary status, should not be read as full subsidy in practice. Green-technology industries and carbon-market intermediaries are genuine structural beneficiaries with mobile/arbitrage exit — the policy framework materially expands their markets and they can relocate to capture the most favorable regulatory treatment. Fossil-fuel-dependent workers and present poor in transition regions are targets: high extraction, trapped exit, immediate time horizon, and no meaningful voice in how the transition is paced or financed. Carbon-intensive export economies sit in between: moderate power, constrained (not fully trapped) exit via diplomatic negotiation over transition finance, but ultimately price-takers in a framework set by wealthier importing economies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing severe climate harm from continued emissions) remains live and is independently corroborated by physical science outside the beneficiary coalition — this blocks a simple mandatrophy verdict of 'the problem is gone but the arrangement persists.' However, the SPECIFIC mitigation-priority solution to that problem — growth-compatible technological substitution — is a contested empirical and political claim, not a corroborated fact; treating it as the definition of legitimate response (rather than one contested reading among three) is where extraction is concentrated. The tangled-rope classification (rather than snare) reflects that this reading does coordinate real capital and policy around a real threat; it is not pure cover. But active enforcement (net-zero legal mandates, carbon border adjustments, exclusion of degrowth and adaptation-priority framings from 'serious' policy venues) plus a clear victim class (fossil workers, carbon-exporting economies) plus a clear beneficiary class (green-tech, carbon-market intermediaries, growth economies preserving their consumption base) satisfies the tangled-rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_empirical_status,
    'Is growth-compatible technological decarbonization actually physically and politically achievable within remaining carbon budgets at the pace mitigation-priority policy assumes, or does the degrowth reading''s incompatibility claim hold?',
    'Track realized decoupling rates (GDP growth vs. absolute emissions) against required decarbonization curves for 1.5C/2C pathways over the next decade; persistent failure to decouple at required rates would corroborate the degrowth reading''s core empirical claim and undermine this reading''s foundational premise.',
    'If growth-compatible decarbonization is empirically failing to meet required pace, the mitigation-priority reading''s claimed coordination function partially dissolves into cover for continued extraction (technology investment without adequate emissions outcomes), pushing the classification toward snare; if decoupling proceeds at required pace, the tangled-rope reading''s coordination component is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_empirical_status, empirical, 'Whether growth-compatible decarbonization is achievable at the required pace.').

omega_variable(
    future_generations_representation_gap,
    'Can a beneficiary group (future generations) that has no present agency and cannot negotiate the terms of its own benefit be treated as a genuine directionality beneficiary in the same sense as an agent with exit options?',
    'Compare the design of transition policy (pace, financing, burden distribution) against what an empowered future-generations proxy (e.g., binding intergenerational equity litigation, youth climate plaintiffs) would negotiate for if given standing; divergence indicates the beneficiary declaration is more rhetorical than structural.',
    'If future generations function only as a legitimating rhetorical beneficiary rather than a structurally empowered one, the true beneficiary set narrows to present green-technology and carbon-market actors, which would push the classification further toward tangled_rope with a thinner coordination justification, or toward snare if the future-benefit claim is found to be substantially decorative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_representation_gap, conceptual, 'Whether nominal future-generations benefit is structurally real or primarily legitimating rhetoric.').

omega_variable(
    reading_selection_mechanism,
    'Why does mitigation-priority (rather than adaptation-priority or degrowth) function as the default definition of ''legitimate'' climate response in mainstream institutions, and is that selection itself a product of which actors hold agenda-setting power?',
    'Trace the institutional history of UNFCCC/IPCC framing choices and funding flows to green-technology sectors versus adaptation and degrowth research programs; a strong correlation between agenda-setting power (incumbent growth economies, private capital) and reading selection would indicate the kernel contest is resolved by power rather than by the comparative merits of the three readings.',
    'If reading selection tracks agenda-setting power rather than comparative empirical or ethical merit, the mitigation-priority reading''s status as ''the'' legitimate response (rather than one contested reading) is itself an extraction of legitimacy from the excluded sibling readings and their constituencies (vulnerable states, degrowth advocates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_mechanism, conceptual, 'Whether the kernel contest is resolved by institutional power rather than comparative merit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__mitigation_priority, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__mitigation_priority, theater_ratio, 1997, 0.24).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__mitigation_priority, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__mitigation_priority, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__mitigation_priority, base_extractiveness, 1992, 0.32).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__mitigation_priority, base_extractiveness, 1997, 0.36).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__mitigation_priority, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__mitigation_priority, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__mitigation_priority, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__mitigation_priority, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__mitigation_priority, suppression_requirement, 2024, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_harm_prevention kernel, each authored as a separate constraint story with its own ε per the ε-invariance principle: mitigation_priority (this file, tangled_rope, ε=0.58 — growth-compatible technological transition prioritizing future harm prevention), adaptation_priority (near-term resilience prioritization, accepting higher warming trajectory), and degrowth_reading (planned Global North contraction as the only physically adequate response). The three readings share the founding commitment that legitimate climate response must prevent future climate harm but diverge sharply on mechanism, beneficiary set, and victim set. They are linked via affects_constraints because policy resource allocation to one reading structurally reduces resource availability and legitimacy for the others (a mitigation-dominated finance architecture starves adaptation finance and forecloses degrowth policy proposals from serious consideration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
