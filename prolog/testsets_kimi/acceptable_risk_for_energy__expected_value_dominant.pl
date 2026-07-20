% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Risk Acceptability Framework for Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint is the expected-value-dominant reading of the
 *   acceptable_risk_for_energy kernel. Under this reading, energy technology
 *   acceptability is determined by annual expected costs and climate
 *   benefits, with rare events weighted by their probability-consequence
 *   product. The reading treats nuclear power as exiting the victim set when
 *   its expected value is favorable relative to alternatives, treats waste
 *   disposal as a solvable engineering challenge with bounded expected cost,
 *   and maintains low structural suppression of tail-risk discourse by
 *   incorporating tail events into the EV calculus rather than banning the
 *   framing. The constraint coordinates energy policy by providing a single
 *   quantitative decision metric, while asymmetrically extracting
 *   risk-bearing capacity from localized and future populations who do not
 *   capture the produced benefits.
 *
 * KEY AGENTS:
 *   - Energy regulators (agenda_setter, institutional/constrained) â administer the EV licensing framework
 *   - Nuclear industry (beneficiary, powerful/constrained) â captures regulatory legitimacy and revenue from favorable EV assessments
 *   - Climate mitigation coalition (beneficiary, organized/mobile) â gains low-carbon deployment pathway
 *   - Local risk-bearing communities (payer, powerless/trapped) â bear localized accident and siting risks
 *   - Future generations (payer, powerless/trapped) â bear millennial waste and deferred catastrophic risks
 *   - Tail-risk advocates (excluded, moderate/constrained) â excluded when their arguments resist quantification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.62).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.55).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Risk Acceptability Framework for Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'b76bfe81-6c07-40ac-8ac8-74257bf157b8').
narrative_ontology:cs_kernel_codification('b76bfe81-6c07-40ac-8ac8-74257bf157b8', formalized).
narrative_ontology:cs_authority_grounding('b76bfe81-6c07-40ac-8ac8-74257bf157b8', expertise).
narrative_ontology:cs_interpretation_layer_present('b76bfe81-6c07-40ac-8ac8-74257bf157b8').
narrative_ontology:cs_reading_relation('b76bfe81-6c07-40ac-8ac8-74257bf157b8', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('b76bfe81-6c07-40ac-8ac8-74257bf157b8', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('b76bfe81-6c07-40ac-8ac8-74257bf157b8', foundational, risk_commensurability).
narrative_ontology:cs_axiom_status(risk_commensurability, holdable).
narrative_ontology:cs_axiom_grounding('b76bfe81-6c07-40ac-8ac8-74257bf157b8', risk_commensurability, empirically_contingent).
narrative_ontology:cs_axiom('b76bfe81-6c07-40ac-8ac8-74257bf157b8', foundational, absolute_expected_value_threshold).
narrative_ontology:cs_axiom_status(absolute_expected_value_threshold, holdable).
narrative_ontology:cs_axiom_grounding('b76bfe81-6c07-40ac-8ac8-74257bf157b8', absolute_expected_value_threshold, instrumental).
narrative_ontology:cs_reference_frame('b76bfe81-6c07-40ac-8ac8-74257bf157b8', rational_expected_value_framework).
narrative_ontology:cs_drift_state('b76bfe81-6c07-40ac-8ac8-74257bf157b8', contemporary_climate_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b76bfe81-6c07-40ac-8ac8-74257bf157b8', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_coalition).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, local_risk_bearing_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the expected-value risk framework for energy licensing, sets discount rates and probability thresholds, and enforces compliance by requiring quantitative cost-benefit submissions from developers. They do not personally capture the extracted value but their institutional legitimacy depends on maintaining the analytical apparatus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits when the EV framework renders nuclear power acceptable by quantifying rare accident risks and waste costs as bounded expected values, enabling licensing and insurance structures that would be unobtainable under precautionary framing. Exit from the framework means exiting the regulatory market entirely.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_industry, beneficiary,
    powerful, biographical, constrained, global).

% Advocates for low-carbon baseload power and treats the EV framework as an analytical ally that justifies rapid nuclear deployment against climate catastrophe. They can shift advocacy to other technologies if the EV calculus turns unfavorable, giving them mobility the local risk communities lack.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_coalition, beneficiary,
    organized, generational, mobile, global).

% Live near reactor sites, waste transport routes, or proposed storage facilities. They bear the localized health and property-value risks that the EV framework treats as quantified expected costs, but they rarely capture compensatory benefits equivalent to the national/global energy production value. Geographic and economic immobility traps them in the siting decision.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, local_risk_bearing_communities, payer,
    powerless, biographical, trapped, local).

% Bear millennial-scale waste stewardship burdens and deferred catastrophic risks that the framework discounts to small expected present values. They have no voice in the regulatory proceedings and no exit from the inherited waste inventory or altered biosphere.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Argue that catastrophic low-probability events carry non-quantifiable ethical weight and that deep uncertainty makes probability estimation structurally unreliable. Their framings are admissible only to the extent they can be converted to probability-consequence products; non-commensurable arguments are excluded from formal licensing criteria.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_advocates, excluded,
    moderate, biographical, constrained, national).

narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single quantitative decision metric (annual expected cost and climate benefit) for comparing energy technologies, preventing regulatory paralysis from incommensurable risk framings and enabling consistent cross-technology resource allocation toward climate targets.
% TRANSFER_FUNCTION: Moves the burden of catastrophic tail-risk realization and long-term waste stewardship from present energy beneficiaries and the general public to localized host communities and future generations, while concentrating regulatory legitimacy, industrial revenue, and climate-policy credit in the present analytical and production apparatus.
% ABSENT_VOICES: Local communities lacking technical capacity to dispute probability assessments; future generations who cannot contest discount rates applied to millennial waste liabilities; advocates of non-commensurable protective or sacred values whose framings are procedurally inadmissible under quantification rules.
% DISAPPEARANCE_RATIONALE: If the expected-value framework vanished, energy licensing would lose its dominant decision criterion; nuclear projects would face re-evaluation under precautionary or comparative-risk standards, stranded-cost assumptions would shift, climate-target accounting would require revision, and regulatory bodies would need entirely new authorization methodologies.
% FOUNDING_PROBLEM: Mid-20th-century energy policy faced inconsistent risk standards across technologies, regulatory fragmentation by special interests, and political paralysis when attempting to rationally trade off climate benefits against localized hazards, producing either arbitrary technology selection or infrastructural stagnation.
% FOUNDING_PROBLEM_CORROBORATION: Energy economists and regulatory historians attest that standardization was needed to enable large-scale infrastructure decisions. Environmental justice scholars and some risk analysts contest that the 'paralysis' was actually warranted democratic deliberation, and that the EV framework was adopted to override local opposition rather than solve a coordination problem. Independent government accountability reports from outside the nuclear beneficiary set document both the standardization benefit and the distributive asymmetry.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the framework systematically moves hard-to-quantify tail risks and intergenerational waste burdens onto parties who cannot contest the probability or discount assumptions. Suppression (0.55) is moderate: the reading does not ban tail-risk discourse outright but structurally defangs it by requiring probability-conversion for admissibility, which progressively filters out non-quantifiable objections as the framework institutionalizes. Theater ratio (0.22) is low because the analytical apparatus is functionally central to licensing; most activity is genuine quantification rather than performative compliance. Accessibility collapse (0.58) reflects that once the EV frame is accepted, precautionary alternatives appear irrational. Resistance (0.52) is moderate and rising, driven by local opposition and post-Fukushima empirical challenges to tail-probability estimates.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as rational coordination that prevents arbitrary energy policy and enables climate action. The payer seats experience the identical constraint as a legitimization of risk dumping through technical quantification they lack resources to contest. The engine computes this divergence from the structural asymmetry in exit options (mobile vs trapped) and power (organized vs powerless), not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and climate mitigation coalition are structural beneficiaries (low d): the framework subsidizes their respective revenue and decarbonization goals by rendering nuclear licensable. Local communities and future generations are structural targets (high d): they absorb the realized risks and deferred costs that the EV calculus minimizes. Energy regulators sit near symmetric/administrative (moderate d): they enforce the transfer but do not personally capture the extracted surplus. Tail-risk advocates are excluded rather than governed; their exclusion is the mechanism by which the framework maintains its coordination boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve genuine coordination failure in energy risk assessment. That founding problem remains live in some form, preventing piton classification. However, the framework has accumulated extractive function by treating probability as an objective given rather than a contested assumption, thereby externalizing risks to non-beneficiaries. Classifying it as tangled_rope captures the genuine coordination utility while registering the asymmetric extraction, distinguishing it from both pure rope (which would lack victims) and snare (which would lack the real standardization function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_probability_quantifiability,
    'Can catastrophic tail risks (reactor meltdown cascades, spent fuel pool fires, repository breach) be assigned stable, unbiased probabilities, or does deep uncertainty and model dependence make the EV calculation structurally unreliable?',
    'Systematic retrospective audit of probabilistic risk assessments against observed event frequencies; comparison of independent elicitation studies for rare event probabilities.',
    'If tail probabilities are not reliably quantifiable, the risk_commensurability axiom is empirically undermined and the constraint shifts toward snare-like extraction by legitimizing projects on fictitious precision. If quantifiable, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_probability_quantifiability, empirical, 'Whether rare catastrophic risks admit objective probability weighting').

omega_variable(
    intergenerational_discount_rate,
    'What normative discount rate should apply to millennial-scale waste stewardship costs, and does the rate selected determine whether nuclear waste appears as a solvable engineering challenge or an intergenerational injustice?',
    'Deliberative democratic forums with future-representation mechanisms; ethical review of discounting conventions independent of regulatory capture.',
    'A near-zero discount rate would substantially raise expected waste costs and could reclassify nuclear as a payer under this framework; a market-rate discount rate sustains the current beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'Normative ambiguity in discounting future costs and risks').

omega_variable(
    kernel_reading_boundary,
    'Does the expected_value_dominant reading genuinely foreclose catastrophic_tail_dominant, or do hybrid regulatory frameworks exist that blend EV optimization with absolute catastrophic prohibitions?',
    'Comparative institutional analysis of energy regulatory regimes to identify whether any single framework maintains both EV determinacy and categorical tail-risk prohibitions without contradiction.',
    'If hybrid frameworks are coherent, the forecloses relation should be downgraded to influences, altering the kernel''s contamination propagation model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between EV-dominant and catastrophic-tail readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 8, 0.12).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 16, 0.14).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 24, 0.17).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 32, 0.2).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.15).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable_risk_for_energy kernel, decomposed per the Îµ-invariance principle from the colloquial label 'acceptable energy risk' into structurally distinct claims. The expected_value_dominant reading differs from catastrophic_tail_dominant and comparative_risk_dominant in its epsilon value, beneficiary structure, and empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
