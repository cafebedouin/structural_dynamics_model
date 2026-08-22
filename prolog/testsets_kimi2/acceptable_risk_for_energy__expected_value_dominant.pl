% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Expected-Value Dominant Energy Risk Acceptability Framework
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint is the expected_value_dominant reading of the
 *   acceptable_risk_for_energy kernel. It instantiates the policy framework
 *   under which annual expected costs and climate benefits determine the
 *   acceptability of energy technologies, rare events are weighted by
 *   probability-consequence product, and nuclear power exits the victim set
 *   whenever its expected value is favorable relative to alternatives.
 *   Sibling readings include catastrophic_tail_dominant (tail risks veto
 *   deployment regardless of expected value) and comparative_risk_dominant
 *   (risk acceptable only relative to competing energy risks). Under this
 *   reading, waste disposal is reframed as a solvable engineering challenge
 *   rather than an intergenerational moral burden, and suppression of
 *   tail-risk framings is comparatively low.
 *
 * KEY AGENTS:
 *   - Probabilistic Risk Establishment (agenda_setter / institutional / analytical) â sets methodological standards and certifies acceptability
 *   - Nuclear Energy Sector (beneficiary / powerful / mobile) â gains deployment clearance under favorable expected value
 *   - Proximate Risk Communities (payer / powerless / trapped) â bear realized tail risks discounted by annualization
 *   - Electricity Ratepayers (beneficiary / organized / constrained) â benefit from lower costs via risk externalization
 *   - Precautionary Advocates (excluded / moderate / constrained) â excluded from regulatory table by quantification requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.42).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.35).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Dominant Energy Risk Acceptability Framework").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'f2c5b1cd-a517-4d6e-8c49-86f67aed838a').
narrative_ontology:cs_kernel_codification('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', distributed).
narrative_ontology:cs_authority_grounding('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', expertise).
narrative_ontology:cs_interpretation_layer_present('f2c5b1cd-a517-4d6e-8c49-86f67aed838a').
narrative_ontology:cs_reading_relation('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', foundational, risk_commensurability_by_probability).
narrative_ontology:cs_axiom_status(risk_commensurability_by_probability, holdable).
narrative_ontology:cs_axiom_grounding('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', risk_commensurability_by_probability, empirically_contingent).
narrative_ontology:cs_axiom('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', foundational, tail_risk_subordination_to_annual_expectation).
narrative_ontology:cs_axiom_status(tail_risk_subordination_to_annual_expectation, holdable).
narrative_ontology:cs_axiom_grounding('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', tail_risk_subordination_to_annual_expectation, instrumental).
narrative_ontology:cs_reference_frame('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', actuarial_rationality_framework).
narrative_ontology:cs_drift_state('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', contemporary_energy_policy, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f2c5b1cd-a517-4d6e-8c49-86f67aed838a', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_sector).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, electricity_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, proximate_risk_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the institutional capacity for probabilistic risk assessment, sets methodological standards for converting accident frequencies and consequences into annualized expected costs, and certifies which energy projects meet the acceptability threshold. Its authority rests on actuarial expertise and regulatory mandate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, probabilistic_risk_establishment, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from a regulatory pathway that clears projects for deployment when probabilistic expected value is favorable, avoiding precautionary delays or absolute risk prohibitions that would block construction and financing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_energy_sector, beneficiary,
    powerful, biographical, mobile, national).

% Live near proposed or existing nuclear facilities and waste corridors. They bear the low-probability high-consequence tail risk that the expected-value framework treats as an acceptable annualized cost. Their objections based on catastrophic potential are discounted as statistically irrational unless translated into probability-consequence products they lack resources to produce.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, proximate_risk_communities, payer,
    powerless, biographical, trapped, local).

% Receive electricity from technologies that are deployable under expected-value regulation. They benefit from lower rates than if tail risks were fully internalized into project costs, though they do not individually choose the risk framework or the technology mix.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, electricity_ratepayers, beneficiary,
    organized, biographical, constrained, regional).

% Argue that some risks are not meaningfully commensurable via probability weighting and that irreversible catastrophic potential should veto projects regardless of expected value. They are structurally excluded from the regulatory table when the formal decision framework only admits quantified, annualized risk-cost comparisons.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, precautionary_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts disparate, incommensurable risks and benefits across energy technologies into a single decision metric (annualized expected cost), enabling centralized licensing and investment decisions that would otherwise deadlock under competing risk ontologies.
% TRANSFER_FUNCTION: Moves the realized burden of low-probability, high-consequence tail events from energy developers and general ratepayers to proximate communities and ecosystems, by translating those risks into probability-consequence products that are too small to block deployment on an annualized basis.
% ABSENT_VOICES: Precautionary advocates and proximate community members who treat catastrophic potential as veto-worthy regardless of probability weighting. They are formally present in public comment processes but structurally excluded when the decision framework only admits quantified, annualized risk-cost comparisons and treats non-probabilistic objections as irrational.
% DISAPPEARANCE_RATIONALE: If the expected-value framework vanished overnight, energy licensing proceedings would lose their primary decision metric. Regulatory agencies would face deadlock between incommensurable risk framings, project timelines would become unpredictable, and the current distribution of risk-bearing would unravel as non-quantified catastrophic objections gained standing equal to probabilistic assessments.
% FOUNDING_PROBLEM: Industrial societies needed a rational, comparable method for evaluating hazardous but socially beneficial technologies without being paralyzed by every conceivable catastrophic scenario.
% FOUNDING_PROBLEM_CORROBORATION: Policy historians and risk analysts outside the nuclear beneficiary set attest that genuine coordination failure around risk evaluation existed in the mid-twentieth century. Environmental justice scholars and disaster sociologists attest that the 'founding problem' was constructed to exclude qualitative and distributive objections that were always part of legitimate public decision-making. Both attestation sets are external to the beneficiary coalition.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the framework genuinely coordinates disparate risks into a comparable metric, but it systematically transfers tail-risk burden to geographically locked proximate communities who do not consent and are not compensated. Suppression (0.35) is low-to-moderate: tail-risk framings are not brutally suppressed, but they are structurally excluded from the regulatory table by the quantification requirement. Theater ratio (0.25) reflects moderate performative reassuranceâpost-Fukushima spikes in probabilistic reassurance theaterâbut the core analysis remains functional. Accessibility collapse (0.62) captures how alternatives like the precautionary principle appear irrational once the expected-value frame is accepted. Resistance (0.50) reflects ongoing community and civil-society pushback.
 *
 * PERSPECTIVAL GAP:
 *   The probabilistic risk establishment and nuclear sector experience the constraint as rational coordination that solves a genuine policy deadlock. Proximate communities experience the same constraint as the institutionalization of their uncompensated exposureâmathematics laundering risk imposition. The engine computes this divergence from the structural data: agenda-setters with analytical exit sit near the beneficiary pole, while trapped payers sit near the full-target pole.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear energy sector and electricity ratepayers are structural beneficiaries (low d): they collect deployment permission and lower costs without bearing the tail realization. The probabilistic risk establishment sits near the beneficiary end as well, though its gains are authority and institutional scope rather than direct monetary rents. Proximate risk communities are the primary targets (high d): they bear the physical consequences of the framework's discounting, are geographically trapped, and have no reciprocal claim on the benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The expected-value framework prevents mandatrophy mislabeling by preserving its genuine coordination function: without a shared metric, energy policy deadlocks. A snare reading would erase this function and treat the framework as pure cover for risk dumping. A rope reading would ignore the asymmetric transfer to trapped proximate communities. Tangled rope captures that the coordination and extraction are structurally coupled through the same probability-weighting machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the expected_value_dominant reading of the acceptable_risk_for_energy kernel. Would classification change if the catastrophic_tail_dominant reading were adopted as the governing framework?',
    'Compare the two constraints in the family; the kernel instantiates different victim sets, suppression levels, and coordination functions depending on which reading governs regulatory practice.',
    'Under the catastrophic reading, nuclear re-enters the victim set and the constraint becomes substantially more extractive or purely suppressive; under this reading, extraction is moderate and the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega locating this constraint within the acceptable_risk_for_energy kernel.').

omega_variable(
    tail_risk_probability_reliability,
    'Are the probability estimates for catastrophic nuclear events (core damage, waste breach) empirically reliable enough to justify expected-value optimization, or do they systematically underestimate tail risk due to model dependence and historical base-rate limitations?',
    'Retrospective validation of pre-event PRA estimates against actual incident frequencies; analysis of model uncertainty and common-cause failures excluded from standard probabilistic risk assessments.',
    'If estimates are unreliable, the foundational axiom of risk commensurability is challenged and the constraint shifts toward snare (coordination cover for risk imposition); if reliable, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_probability_reliability, empirical, 'Empirical uncertainty about the reliability of tail-risk probability estimates.').

omega_variable(
    waste_disposal_engineering_assumption,
    'Does treating waste disposal as a solvable engineering challenge rest on demonstrated technical solutions or on an unverified projection that transfers intergenerational burden?',
    'Long-term repository performance assessment independent of institutional continuity; archaeological-scale durability studies and future-society consent analysis.',
    'If the engineering assumption is unverified, the intergenerational public should join proximate communities in the victim set, raising extractiveness and potentially shifting classification toward higher asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_engineering_assumption, empirical, 'Uncertainty about whether waste disposal is a solved engineering problem or an intergenerational risk transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_ev_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acceptable_risk_ev_tr_t10, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 10, 0.14).
narrative_ontology:measurement(acceptable_risk_ev_tr_t20, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 20, 0.18).
narrative_ontology:measurement(acceptable_risk_ev_tr_t30, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 30, 0.22).
narrative_ontology:measurement(acceptable_risk_ev_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.3).
narrative_ontology:measurement(acceptable_risk_ev_tr_t50, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_ev_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(acceptable_risk_ev_be_t10, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(acceptable_risk_ev_be_t20, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(acceptable_risk_ev_be_t30, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(acceptable_risk_ev_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(acceptable_risk_ev_be_t50, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 50, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(acceptable_risk_for_energy__expected_value_dominant, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
