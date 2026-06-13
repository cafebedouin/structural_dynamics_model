% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value Risk Acceptability Framework for Nuclear Energy
 *   domain: energy_policy/risk_assessment/environmental_governance
 *
 * SUMMARY:
 *   Expected-value risk optimization is embedded in global nuclear energy
 *   governance: annual costs and climate benefits determine whether specific
 *   reactor projects and waste-repository sites are approved. The framework
 *   weights rare, high-consequence events (meltdown, repository breach) by
 *   their probability × consequence product, making low-probability accidents
 *   negligible in cost-benefit analysis even if consequences are severe. This
 *   constraint is ONE READING of a contested kernel about acceptable energy
 *   risk. The other readings — catastrophic_tail_dominant (irreversibility
 *   and intergenerational burden dominate) and comparative_risk_dominant
 *   (risk acceptable only relative to coal/climate alternatives) — rest on
 *   different mathematical and ethical foundations. This JSON instantiates
 *   the expected-value reading as a clean, ε-invariant constraint with stable
 *   extraction and suppression metrics. The sibling readings are OTHER
 *   constraints (separate JSON files), not alternatives folded into this one.
 *
 * KEY AGENTS:
 *   - Nuclear operators: institutional agenda-setters who administer the framework and benefit from expected-value math that treats low-probability accidents as negligible.
 *   - Carbon-constrained planners: beneficiaries (usually governmental) who unlock nuclear investment by deploying expected-value frameworks that overcome climate-vs-radiation risk trade-offs.
 *   - Radiation-exposure communities: powerless payers bearing concentrated localized risk (thyroid disease, childhood leukemia near reactors) — thousands of people at immediate risk of immediate harms, weighted as statistical negligibilities in expected-value sums.
 *   - Intergenerational waste bearers: powerless payers bearing civilizational-timescale burden (24,000-year plutonium custody) — mathematically erased by discounting but temporally real. The framework's temporal scope is fundamentally mismatched to their stake.
 *   - Regulatory agencies: institutional agenda-setters who administer licensing but operate under statutory mandate tied to expected-value optimization; they see the framework's limits operationally but enforce it.
 *   - Tail-risk skeptics (excluded): organized actors (indigenous groups, precautionary doctrine advocates, deep-green environmentalists) arguing alternative frameworks should govern. They are excluded from formal regulatory authority but represent live conceptual alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.62).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.58).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Risk Acceptability Framework for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "energy_policy/risk_assessment/environmental_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'a58149cb-6ca8-4268-8c24-c147f04b892f').
narrative_ontology:cs_kernel_codification('a58149cb-6ca8-4268-8c24-c147f04b892f', formalized).
narrative_ontology:cs_authority_grounding('a58149cb-6ca8-4268-8c24-c147f04b892f', extraction).
narrative_ontology:cs_interpretation_layer_present('a58149cb-6ca8-4268-8c24-c147f04b892f').
narrative_ontology:cs_reading_relation('a58149cb-6ca8-4268-8c24-c147f04b892f', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('a58149cb-6ca8-4268-8c24-c147f04b892f', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('a58149cb-6ca8-4268-8c24-c147f04b892f', foundational, probabilistic_weighting_dominance).
narrative_ontology:cs_axiom_status(probabilistic_weighting_dominance, holdable).
narrative_ontology:cs_axiom_grounding('a58149cb-6ca8-4268-8c24-c147f04b892f', probabilistic_weighting_dominance, empirically_contingent).
narrative_ontology:cs_axiom('a58149cb-6ca8-4268-8c24-c147f04b892f', foundational, intertemporal_discounting_legitimacy).
narrative_ontology:cs_axiom_status(intertemporal_discounting_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a58149cb-6ca8-4268-8c24-c147f04b892f', intertemporal_discounting_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('a58149cb-6ca8-4268-8c24-c147f04b892f', expected_value_risk_optimization).
narrative_ontology:cs_created_at('a58149cb-6ca8-4268-8c24-c147f04b892f', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_power_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, carbon_constrained_energy_planners).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, electricity_consumers_in_carbon_taxed_markets).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, radiation_exposure_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, long_term_waste_repository_neighbors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, intergenerational_waste_bearers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is coded 0.62 because the framework concentrates climate benefits (decarbonization, avoided coal deaths, carbon-tax savings — flows to millions of consumers and institutional planners) while dispersing costs (accidents, waste) across small geographically isolated communities and mathematically erased future people. Expected-value weighting makes diffuse high-consequence risks become marginal, turning what would be snare-like extraction into something that can plausibly claim coordination function (solving the incommensurable-risks problem). Suppression is 0.58 because alternative frameworks exist intellectually (tail-risk dominance, precautionary principle, intergenerational justice) but are institutionally excluded from regulatory authority. Resistance is 0.72 because even with institutional suppression, local communities around reactor sites and waste repositories mount real resistance, and academic/environmental critiques circulate persistently. Theater is 0.28 because the framework does solve a real coordination problem (comparing heterogeneous risks on a common scale), but its mathematical operations increasingly become performance of false precision as intergenerational and tail-risk issues accumulate. The measurement series (1970–2050) traces rising extractiveness from early adoption (1970: framework nascent, low extraction) through Chernobyl/TMI-induced tightening and climate-crisis deployment (2000–2015: rising extraction as carbon urgency increased reliance on the framework) to present plateau (2030–2050: framework stabilized, extraction holds). Suppression rises as institutional gatekeeping against tail-risk frameworks strengthens post-Fukushima. Theater ratios rise modestly because safety theater (Fukushima aftermath, Yucca Mountain messaging) increases without addressing the core mathematical framework.
 *
 * PERSPECTIVAL GAP:
 *   The expected-value reading and the tail-risk reading (catastrophic_tail_dominant, a sibling constraint) should compute VERY differently from different seats. From the nuclear operator seat: expected-value dominance is rational risk governance, tail-risk frames are alarmism. From the waste-repository community seat: expected-value optimization is mathematized injustice, tail-risk precaution is the only defensible stance. From the intergenerational seat: expected-value discounting is structural invisibility (we are mathematically erased), tail-risk doctrine would recognize our standing. The engine computes per-seat types from structural data (power, exit, beneficiary/victim status); the wide divergence in seat-level conclusions reflects the fundamental asymmetry in the framework's benefits and burdens. The expected-value framework itself encodes the divergence: by construction, it benefits those who discount futures and concentrates costs on those who cannot escape long timescales.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators: d ~0.1 (primary beneficiaries, arbitrage exit — can switch to renewables if expected-value math shifts; they benefit directly from the framework's dominance; beneficiary status places them at low d). Carbon planners: d ~0.2 (beneficiaries, mobile exit — the framework solves their core mandate, but they have alternatives in renewables; institutional power moderates extraction experience). Radiation communities: d ~0.88 (victims, trapped exit — bear concentration risk without meaningful exit, no beneficiary status, powerless structural position; high d). Waste communities: d ~0.92 (victims, identity-locked exit — civilizational timescale, region-dependent livelihood, internalized sacrifice narrative; very high d). Intergenerational: d ~0.95 (victims, temporally trapped, no exit by definition, powerless, discounted to invisibility in the framework's mathematics; maximum d). Regulatory agencies: d ~0.55 (symmetric — they administer the framework but also constrained by statutory mandate; secondary beneficiary role from institutional prestige, secondary victim role from having to suppress alternative frameworks). The directionality_overrides field is not used here because the derivation from beneficiary/victim + exit produces accurate d values; the framework's structure IS the directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint faces incipient mandatrophy: the founding problem was 'how to compare incommensurable risks (radiation, coal emissions, climate) on a common metric.' That coordination problem remains partially live in global energy policy. However, the frame of what counts as 'acceptable risk' has shifted: from 1970s focus on single-plant safety to contemporary focus on climate necessity and intergenerational justice. The expected-value framework was designed to solve the incommensurability problem of 1970s; it poorly addresses the intergenerational and irreversibility questions of 2020s. Mandatrophy emerges as: (a) the founding problem is contested (some planners still think expected-value solves it; others think it obscures the real problem); (b) alternative frameworks (tail-risk dominance, comparative-risk-with-equity overlays) are increasingly articulated, making expected-value less obviously dominant; (c) the framework persists not because it solves the contemporary problem but because institutional path dependence and licensing infrastructure are sunk (piton-adjacent behavior). The classification does NOT flip to mandatrophy-resolved yet, because carbon-constrained planners still genuinely need the framework to justify nuclear investment in climate scenarios. But the tension between founding problem (1970s incommensurability) and contemporary problem (intergenerational justice) is irreconcilable within expected-value math, marking the constraint as moving toward mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discounting_moral_standing,
    'Should the mathematical discounting of future consequences (standard in expected-value cost-benefit analysis) apply to intergenerational harms? Does applying a 3–7% annual discount rate to civilizational-timescale waste burden reduce a legitimate moral claim to statistical negligibility?',
    'Philosophical and ethical frameworks (intergenerational justice, non-identity problem, long-termism) must arbitrate; empirical resolution is unavailable. Regulatory adoption of alternative frameworks (zero discounting for timescales >100 years, precautionary principle for irreversible harms) would resolve operationally but not substantively.',
    'If discounting is deemed illegitimate for intergenerational harms, the expected-value framework fails as an adequate decision rule, and the constraint reclassifies from tangled_rope (asymmetric but coordinated) to snare (purely extractive). Nuclear becomes unjustifiable under any expected-value analysis because future costs are no longer negligible. Alternatively, if discounting is retained but the issue is acknowledged, suppression increases: the framework persists despite known mathematical disadvantages to future people.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discounting_moral_standing, conceptual, 'Whether intergenerational justice can be mathematically represented in expected-value frameworks.').

omega_variable(
    geological_prediction_limits,
    'How confident can we be in 24,000-year isolation guarantees for deep geological repositories? What is the actual epistemic foundation for ''breach probability'' estimates in official risk assessments, and how does that compare with intergenerational timescales?',
    'Empirical: paleoclimatic data on tectonic and hydrological changes over comparable timescales; comparative analysis of long-term predictive accuracy in geology vs. short-term regulatory assumptions. A track record showing that 100-year predictions from 1970s geology have failed would undermine the framework; conversely, 50+ years of Yucca Mountain study with stable predictions would support it.',
    'If epistemic limits are severe, breach probabilities in expected-value calculations are placeholder guesses, not scientifically grounded, and the framework''s quantitative rigor is theatrical. This increases suppression (the framework is maintained despite acknowledged uncertainty) and may shift classification toward piton (inertially maintained, functionally degraded). If confidence is high, the framework retains technical defensibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geological_prediction_limits, empirical, 'Calibration of long-term geological prediction confidence against intergenerational timescale assumptions.').

omega_variable(
    kernel_reading_alternative_axioms,
    'Is this expected-value reading one legitimate axiomatization of risk acceptability, or is it the only defensible framework? The sibling readings (catastrophic_tail_dominant, comparative_risk_dominant) rest on fundamentally different ethical and mathematical axioms. Which axioms should govern energy policy?',
    'Committer frame: the axioms in cs_structure.axioms declare what THIS reading holds as foundational. Sibling readings will author their own axioms and reading_relations. The kernel contest is not empirically resolvable — it is a choice among incommensurable frameworks. The measure is whether the three readings constitute a genuine contest (each coherent, each supported by real constituencies) or whether the expected-value reading has foreclosed the others through epistemic/institutional dominance.',
    'This omega documents that the constraint IS a reading of a contested kernel, not a natural decision procedure. If foreclosure is the case, the low suppression coded in the base_properties is false — suppression should be higher because alternative frameworks are suppressed by institutional dominance, not legitimacy. If genuine contest persists, suppression reflects real coexistence of competing frameworks in the policy space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_axioms, conceptual, 'Whether expected-value dominance reflects genuine superiority or institutional foreclosure of alternative risk frameworks.').

omega_variable(
    distributional_injustice_as_feature,
    'Is the concentration of intergenerational and geographically dispersed costs a bug (unintended inequity that should be corrected) or a feature (unavoidable consequence of how energy systems work, and thus absorbed into the framework)?',
    'Regulatory redesign: if frameworks are adopted that assign explicit weight to intergenerational equity (e.g., per-capita waste burden caps, waste-site ownership by all future stakeholders) or geographic equity (supralocal benefit-sharing, reparation funds, site-selection by affected communities), the distributional pattern can be rebalanced. The test is whether such redesigns are adopted or resisted.',
    'If treated as unavoidable feature, suppression of tail-risk and distributional concerns is structural and rises. If treated as bug requiring correction, alternative frameworks (comparative_risk_dominant with equity overlays, or catastrophic_tail_dominant rejecting expected-value math entirely) become available. This directly influences whether the constraint is tangled_rope (mixed coordination + asymmetric extraction) or snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_injustice_as_feature, preference, 'Whether inequitable distribution of consequences is inherent to expected-value analysis or remediable through policy redesign.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of tail-risk and intergenerational concerns primarily structural (regulatory exclusion, institutional barriers to alternative frameworks, funding incentives for expected-value studies) or internalized (community acceptance of expected-value framing as legitimate, identity-fusion of waste-site residents with sacrifice narratives)?',
    'Post-regulatory-change test: if a jurisdiction adopts precautionary or tail-risk frameworks for energy policy, would intergenerational and proximity-affected communities mobilize to enforce them, or would suppression persist? Structural suppression would dissolve; internalized suppression would persist. Interviews with stakeholders about their own risk framing would provide intermediate evidence.',
    'If structural, suppression can be reduced by changing regulations. If internalized, the constraint''s effective suppression is higher than the scalar measures suggest — resistance would persist even if formal institutional barriers were removed. Classification implications: higher effective suppression may shift the constraint toward snare (extraction via internalized acceptance) rather than tangled_rope (mixed coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative risk frameworks operates through institutional barriers or internalized community acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 1970, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(acce_tr_t2030, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2030, 0.28).
narrative_ontology:measurement(acce_tr_t2050, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2050, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1986, 0.48).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(acce_be_t2030, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2030, 0.62).
narrative_ontology:measurement(acce_be_t2050, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2050, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1986, 0.38).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(acce_su_t2030, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2030, 0.58).
narrative_ontology:measurement(acce_su_t2050, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2050, 0.58).
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1986, 0.45).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(acce_su_t2030, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2030, 0.64).
narrative_ontology:measurement(acce_su_t2050, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2050, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.2).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, nuclear_waste_repository_legitimacy).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, carbon_discounting_conventions).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel acceptable_risk_for_energy. The expected-value reading (this constraint) and the catastrophic_tail_dominant reading (separate constraint) and the comparative_risk_dominant reading (separate constraint) represent structurally distinct constraints with different beneficiary/victim structures, different suppression mechanisms, and different resistance patterns. They share a common kernel (the commitment to quantitative risk governance for nuclear energy) but instantiate it differently. The ε values differ substantially: expected-value reading shows moderate-high extraction (0.62) because mathematical discounting and probability weighting enable concentration of diffuse costs; tail-risk reading will show higher extraction and suppression because it rejects the mathematical operations that make costs negligible; comparative-risk reading will show different victim/beneficiary structure because it frames nuclear acceptability via comparison to alternatives rather than absolute threshold. They are not the same constraint viewed from different angles — they are genuinely distinct constraints with different logical structures. They are linked via network.affects_constraints to enable contamination propagation analysis: if the expected-value reading's legitimacy decays (evidence of geological prediction limits, for instance), the tail-risk reading's authority increases, changing which framework dominates policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
