% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable-Risk Regime in Energy Governance
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   The operative risk-governance regime in energy policy evaluates hazards
 *   asymmetrically by failure shape: technologies capable of rare,
 *   concentrated, catastrophic accidents (nuclear fission) are governed under
 *   near-zero-tolerance licensing that tightens retroactively after every
 *   foreign incident, while harms delivered continuously and dispersively
 *   (combustion mortality, carbon accumulation) are absorbed into background
 *   statistics and ordinary environmental permitting. Safety authorities
 *   administer the regime; its operation suppresses the nuclear pathway
 *   through licensing delay, financing exclusion, and ordered shutdowns,
 *   while incumbent fossil generation faces no comparable bar — transferring
 *   operational space, capital, and the mortality burden to populations
 *   downwind of combustion. KEY AGENTS (by structural relationship):
 *   energy_regulators: agenda-setter (institutional/identity_locked) —
 *   administers the weighting; fossil_fuel_incumbents: primary beneficiary
 *   (institutional/arbitrage) — collects the displaced operational space;
 *   anti_nuclear_advocacy_networks: secondary beneficiary
 *   (organized/identity_locked) — collects authority and funding;
 *   nuclear_power_sector: primary target (institutional/trapped) — bears
 *   licensing and closure costs; air_pollution_exposed_populations: diffuse
 *   target (powerless/constrained) — bears the aggregate harm;
 *   climate_vulnerable_populations: diffuse target (powerless/trapped) —
 *   bears delayed decarbonization; energy_risk_analysts: analytical observer
 *   — sees the full structure. This file is one reading of the
 *   acceptable_risk_energy kernel; the sibling readings are separate
 *   constraint files linked through the network section, and the committer
 *   structure is recorded in the omegas.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.7).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.74).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable-Risk Regime in Energy Governance").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '34ec6ad6-45fb-47c0-9e16-6db54494cf6e').
narrative_ontology:cs_kernel_codification('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', formalized).
narrative_ontology:cs_authority_grounding('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', expertise).
narrative_ontology:cs_interpretation_layer_present('34ec6ad6-45fb-47c0-9e16-6db54494cf6e').
narrative_ontology:cs_reading_relation('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', foundational, catastrophic_harm_categorical_priority).
narrative_ontology:cs_axiom_status(catastrophic_harm_categorical_priority, holdable).
narrative_ontology:cs_axiom_grounding('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', catastrophic_harm_categorical_priority, deontological).
narrative_ontology:cs_axiom('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', secondary, distributed_mortality_background_discount).
narrative_ontology:cs_axiom_status(distributed_mortality_background_discount, holdable).
narrative_ontology:cs_axiom_grounding('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', distributed_mortality_background_discount, empirically_contingent).
narrative_ontology:cs_reference_frame('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', catastrophic_release_intolerance_baseline).
narrative_ontology:cs_drift_state('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', contemporary_mortality_comparison_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('34ec6ad6-45fb-47c0-9e16-6db54494cf6e', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_networks).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_power_sector).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, air_pollution_exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, catastrophic_tail_aversion_norm).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, linear_no_threshold_radiation_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the numerical safety goals, dose limits, and licensing conditions that nuclear projects must satisfy. Their institutional mandate fused with the catastrophic-release mission after Chernobyl and deepened after Fukushima; approving a reactor later involved in a severe accident would end careers and the agency's public standing. They can delay or halt nuclear projects indefinitely but hold no comparable lever over dispersed combustion emissions, which proceed under separate environmental statutes with different thresholds.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_regulators, agenda_setter,
    institutional, generational, identity_locked, national).

% Operate coal and gas fleets whose combustion causes hundreds of thousands of premature deaths annually. Because those deaths arrive as dispersed medical statistics rather than as named events, no licensing bar, retroactive shutdown order, or financing exclusion attaches to their operations. They fund research and advertising that keeps nuclear accident imagery publicly salient, and every reactor cancelled or retired early leaves its load to be served by their plants.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Membership organizations, legal shops, and campaign groups whose purpose, funding, volunteer base, and moral authority rest on opposing nuclear power; the catastrophic-accident frame is their core asset. Their interventions in licensing hearings, courts, and legislatures raise project costs and timelines regardless of the technical merits of any particular design, and their organizational identity makes advocacy for any nuclear pathway unthinkable from inside.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).

% Operators, vendors, and suppliers whose plants face decade-long licensing, retroactive safety mandates triggered by accidents at foreign facilities, escalating capital costs, and progressive exclusion from lender and insurer portfolios. Several jurisdictions ordered operating plants closed outright on political timetables. Capital already sunk in certified designs cannot redeploy; exiting the sector means writing off the investment, and staying means absorbing costs no competing generator faces.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_power_sector, payer,
    institutional, biographical, trapped, global).

% People living near and downwind of fossil combustion who bear premature death, chronic disease, and lost life-years at rates orders of magnitude above the historical toll of nuclear power. Their harms arrive one at a time, medically coded, never aggregated into an event that triggers emergency response, licensing review, or compensation schemes. Individual households can sometimes move away from pollution corridors; the class as a whole cannot.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, air_pollution_exposed_populations, payer,
    powerless, immediate, constrained, global).

% Communities exposed to sea-level rise, heat extremes, and storm intensification whose cumulative exposure grows with every year of delayed decarbonization. They hold no seat in energy licensing proceedings, receive no notice of the decisions that shape their exposure, and have no exit from the atmosphere's trajectory.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Government planners in rapidly industrializing countries weighing firm low-carbon generation for growing grids. Export-control regimes, development-bank lending exclusions, and insurer withdrawal keep new nuclear largely outside their procurement set, so their objections surface only in diplomatic channels rather than in the forums where the risk rules are actually written.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, emerging_economy_energy_planners, excluded,
    moderate, generational, constrained, continental).

% Academic and institutional researchers who compile mortality-per-TWh comparisons, probabilistic risk assessments, and integrated energy-system models documenting how the prevailing weighting treats harms of equal magnitude unequally according to their failure shape. They publish, testify, and advise, but hold no vote in licensing and no enforcement power.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_risk_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, administrable decision rule for evaluating technologies whose failure modes differ in kind rather than degree: it commits society collectively to preventing rare, concentrated, irreversible catastrophes (mass evacuation, land contamination, multi-generational stigma) that no individual actor could insure against or exit from, and it sustains the public consent on which siting any hazardous facility depends.
% TRANSFER_FUNCTION: Moves operational space, capital, and regulatory tolerance away from the nuclear pathway and toward incumbent fossil generation; moves the mortality burden onto populations near combustion infrastructure; and moves reputational and moral authority to the advocacy institutions that police the catastrophic frame.
% ABSENT_VOICES: Air-pollution victims and climate-vulnerable populations are absent from every proceeding where the weighting is set — their harms arrive as statistics without names, so no one speaks for them in licensing hearings. Emerging-economy energy planners are excluded by financing and export-control structures. Prospective nuclear entrants lack standing in forums dominated by safety objection.
% DISAPPEARANCE_RATIONALE: If the tail-dominant weighting vanished overnight, nuclear licensing would accelerate and completed reviews would convert to permits, capital locked out of the sector would rotate back in, fossil plants would face mortality-per-TWh scrutiny they currently escape, and the generation mix in several large economies would shift materially within a decade — the arrangement's beneficiaries lose their protected position and the suppressed pathway reopens.
% FOUNDING_PROBLEM: The regime was built to prevent recurrence of catastrophic technological disaster and to restore public consent for hazardous infrastructure after Chernobyl (1986) shattered trust in nuclear governance across the industrialized democracies; Fukushima (2011) re-founded it a second time.
% FOUNDING_PROBLEM_CORROBORATION: Multilateral and academic sources outside the benefiting parties attest the genealogy: IAEA and OECD-NEA post-accident reviews document the regulatory expansion following Chernobyl, and comparative-policy studies show countries with no domestic nuclear accident adopted similar regimes by diffusion rather than local experience, indicating the founding problem was trust restoration, not locally discovered hazard. Energy-system modelers corroborate that the founding rationale no longer tracks the current mortality distribution. Fossil incumbents attest nothing — they benefit silently and have never sponsored the founding narrative; the advocacy networks attest the problem is live, but they are inside the beneficiary set, so their attestation carries no corroborating weight.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.70: substantial but not maximal, because the regime delivers a real service — catastrophe avoidance and consent maintenance — alongside the transfer; the extraction concentrates in the asymmetry of application, not in the safety function itself. Suppression is 0.74 and structural, not internalized: persistence depends on licensing discretion, retroactive mandates, lender and insurer exclusion, and ordered shutdowns — external machinery, not belief. Theater is 0.42: post-Fukushima stress tests, evacuation planning for implausible scenarios, and serial generic safety reviews carry a large performative share, while core dose-limiting and probabilistic risk assessment remain functional. Accessibility collapse is 0.45: alternatives persist (renewables, storage, gas, efficiency) — the regime skews the portfolio rather than foreclosing it. Resistance is 0.60: sustained pushback from energy economists, climate scientists, the nuclear industry, and some governments, though the diffuse victim classes cannot organize. The measurement series run on one shared time grid (all three metrics authored at all eight points). The series show a crisis-ratchet cycle: each visible accident (Chernobyl 1986, Fukushima 2011) produces a suppression and theater spike followed by partial relaxation — but each cycle resets the baseline higher, an intermittent-reinforcement ratchet in which the oscillation itself consolidates the arrangement. Base properties are measured at interval end (2025), on the post-spike plateau. The claimed_type (tangled_rope) is stated independently of these metrics; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the datum, not an error.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit four different worlds under the same rules. From the regulator's position the arrangement is faithful execution of a safety mandate — every tightening is defensible case-by-case. From the nuclear sector's position it is an existential barrier no volume of evidence moves. From the pollution-exposed populations' position it is literally invisible: their harm-source operates unremarked, which is precisely the mechanism. From the fossil incumbents' position it is a normal business environment. The engine computes this divergence from power, exit, and directional data; the divergence is largest where exit is most blocked (trapped nuclear capital, trapped climate-exposed populations) and smallest where exit is cheapest (arbitrage-capable incumbents). Coalition potential among the victim classes exists in principle — a climate-and-nuclear alliance — but is blocked by the advocacy networks' identity lock, which makes cross-pathway cooperation organizationally unthinkable for one of the two natural partners.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel incumbents sit nearest the beneficiary pole: the weighting subsidizes their continued operation by exempting their harm profile from the licensing category, and their arbitrage-grade exit means costs never bind. Anti-nuclear advocacy networks also derive low directionality — they collect authority and funding — but their identity-locked exit means their position is constitutive, not incidental. The nuclear power sector sits near the full-target pole: it pays the transfer directly, and trapped exit amplifies effective extraction beyond the nominal rate. Air-pollution-exposed populations bear the displaced aggregate harm with constrained exit; climate-vulnerable populations bear the delayed-decarbonization channel with no exit at all, placing them furthest toward the target pole despite receiving nothing tangible to point to. Regulators are not beneficiaries — they collect no rents — but their identity fusion with the mission makes them reliable administrators of the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restoring public consent after a trust-shattering catastrophe — is contested and decaying in the mature democracies that administer the regime: no new catastrophe has occurred on regulating territory, the affected fleets have largely retired, and the specific rupture the regime answered has healed or aged out. Yet the regime expanded after Fukushima rather than sunsetting. The mismatch between a contested-to-dead founding problem and a world that visibly rearranges without the arrangement is the capture/zombie signature: the arrangement now persists substantially because fossil incumbents and advocacy institutions depend on it, not because the original problem commands it. Mandatrophy discipline prevents the two symmetric errors: reading the whole regime as pure extraction erases the genuine catastrophic-aversion service that no alternative governance currently provides; reading it as pure safety erases the transfer that funds the beneficiaries. The tangled-rope classification holds both truths in one structure — coordination function intact, extraction layered on top — and directs reform attention at the asymmetry rather than at the safety mission itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the acceptable_risk_energy kernel (reading: catastrophic_tail_dominant). Which conception of acceptable risk a jurisdiction adopts determines the entire victim structure — what would the sibling readings change structurally?',
    'Comparative analysis of jurisdictions operating under each reading: observe which pathway bears the regulatory burden where mortality-per-TWh reasoning governs versus where tail-dominance governs, holding the physical generation facts constant.',
    'Under expected_value_dominant, fossil combustion enters the victim set as the dominant harm and nuclear exits it — the classification of the same physical facts flips. Under option_value_preserving, no pathway is suppressed and this constraint loses its enforcement object entirely. The committer structure, not the physics, decides who counts as harmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: kernel membership, reading identity, and sibling-delta routing for the acceptable_risk_energy family.').

omega_variable(
    disagreement_location_valuation_function,
    'Where exactly do the readings disagree — in the empirical record or in the valuation function applied to it?',
    'Check whether the readings dispute the mortality and risk data (they do not — all accept broadly comparable per-TWh and consequence figures) or the normative weighting applied to identical data (they do).',
    'Confirms the disagreement is located in the tail-weighting axiom itself, making the dispute preference- and framework-indexed rather than empirically resolvable; forensic implication: no quantity of additional safety data can dissolve this constraint — only a change in the governing reading can.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_valuation_function, conceptual, 'Locates the inter-reading disagreement in the valuation function, not the evidence base.').

omega_variable(
    fossil_discounting_motivation,
    'Is the discounting of distributed fossil mortality a principled distinction (reversibility, controllability, individual causal attribution) or motivated discounting serving incumbent interests?',
    'Symmetry test: apply the same reversibility, controllability, and attribution criteria to nuclear waste legacies and decommissioning liabilities; if the criteria flip directionally by convenience of the conclusion, the discounting is motivated rather than principled.',
    'If motivated, the genuine coordination component shrinks and the arrangement trends toward pure extraction riding a safety cover story; if principled, a larger share of the measured extraction is the price of a defensible catastrophic-aversion norm and reform should target application, not existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_discounting_motivation, empirical, 'Whether the fossil-side discount reflects a coherent harm taxonomy or interest-serving selectivity.').

omega_variable(
    nuclear_suppression_carbon_counterfactual,
    'Did tail-dominant suppression of nuclear actually increase fossil generation and carbon emissions in shutting jurisdictions, or did renewables and efficiency backfill the gap?',
    'Grid-level dispatch and emissions data from post-shutdown periods (German Atomausstieg, Japanese restart gaps, California closure windows), isolating replacement generation by source.',
    'If fossil backfilled, the climate_vulnerable_populations victim channel is confirmed and effective extraction rises above the authored base; if clean sources backfilled, that channel weakens and extraction concentrates on the nuclear sector and pollution-exposed populations alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_suppression_carbon_counterfactual, empirical, 'Empirical grounding for the climate-victim channel of the extraction structure.').

omega_variable(
    lnt_low_dose_validity,
    'Does the linear-no-threshold dose model that underwrites nuclear''s catastrophic weighting reflect radiobiology at low doses, or does it overstate risk in the regulatory range?',
    'Pooled low-dose epidemiological cohorts and radiobiological repair-mechanism studies at doses below current limit thresholds.',
    'If LNT materially overstates low-dose risk, part of the catastrophic weighting rests on a contested empirical foundation and the regime''s scientific warrant weakens independent of its normative warrant; if supported, the weighting gains empirical footing and contestation shifts wholly to the valuation layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lnt_low_dose_validity, empirical, 'Validity of the dose-response foundation beneath the catastrophic-release weighting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 1986, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(are_ctd_tr_t1986, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.22).
narrative_ontology:measurement_basis(are_ctd_tr_t1986, observed).
narrative_ontology:measurement(are_ctd_tr_t1993, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1993, 0.28).
narrative_ontology:measurement_basis(are_ctd_tr_t1993, observed).
narrative_ontology:measurement(are_ctd_tr_t2000, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2000, 0.33).
narrative_ontology:measurement_basis(are_ctd_tr_t2000, observed).
narrative_ontology:measurement(are_ctd_tr_t2007, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2007, 0.37).
narrative_ontology:measurement_basis(are_ctd_tr_t2007, observed).
narrative_ontology:measurement(are_ctd_tr_t2011, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.49).
narrative_ontology:measurement_basis(are_ctd_tr_t2011, observed).
narrative_ontology:measurement(are_ctd_tr_t2015, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2015, 0.47).
narrative_ontology:measurement_basis(are_ctd_tr_t2015, observed).
narrative_ontology:measurement(are_ctd_tr_t2020, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(are_ctd_tr_t2020, observed).
narrative_ontology:measurement(are_ctd_tr_t2025, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(are_ctd_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(are_ctd_be_t1986, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement_basis(are_ctd_be_t1986, observed).
narrative_ontology:measurement(are_ctd_be_t1993, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement_basis(are_ctd_be_t1993, observed).
narrative_ontology:measurement(are_ctd_be_t2000, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement_basis(are_ctd_be_t2000, observed).
narrative_ontology:measurement(are_ctd_be_t2007, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement_basis(are_ctd_be_t2007, observed).
narrative_ontology:measurement(are_ctd_be_t2011, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement_basis(are_ctd_be_t2011, observed).
narrative_ontology:measurement(are_ctd_be_t2015, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(are_ctd_be_t2015, observed).
narrative_ontology:measurement(are_ctd_be_t2020, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2020, 0.69).
narrative_ontology:measurement_basis(are_ctd_be_t2020, observed).
narrative_ontology:measurement(are_ctd_be_t2025, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement_basis(are_ctd_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(are_ctd_su_t1986, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.52).
narrative_ontology:measurement_basis(are_ctd_su_t1986, observed).
narrative_ontology:measurement(are_ctd_su_t1993, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement_basis(are_ctd_su_t1993, observed).
narrative_ontology:measurement(are_ctd_su_t2000, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(are_ctd_su_t2000, observed).
narrative_ontology:measurement(are_ctd_su_t2007, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement_basis(are_ctd_su_t2007, observed).
narrative_ontology:measurement(are_ctd_su_t2011, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.8).
narrative_ontology:measurement_basis(are_ctd_su_t2011, observed).
narrative_ontology:measurement(are_ctd_su_t2015, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2015, 0.79).
narrative_ontology:measurement_basis(are_ctd_su_t2015, observed).
narrative_ontology:measurement(are_ctd_su_t2020, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement_basis(are_ctd_su_t2020, observed).
narrative_ontology:measurement(are_ctd_su_t2025, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2025, 0.74).
narrative_ontology:measurement_basis(are_ctd_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'acceptable risk in energy policy' covers three structurally distinct decision rules that assign different victim sets to identical physical facts. This file instantiates the catastrophic_tail_dominant reading (victims: nuclear sector, pollution-exposed and climate-vulnerable populations; beneficiaries: fossil incumbents, advocacy networks). The expected_value_dominant sibling assigns the victim set almost inversely (fossil combustion as dominant harm, nuclear exonerated); the option_value_preserving sibling suppresses no pathway and thus lacks this constraint's enforcement object. The tail-dominant reading is the historically operative one and structurally influences the option-value sibling by shrinking the pathway set its reasoning operates on; it logically forecloses the expected-value sibling as a governing rule. Each member carries its own epsilon; no averaging across readings occurs anywhere in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
