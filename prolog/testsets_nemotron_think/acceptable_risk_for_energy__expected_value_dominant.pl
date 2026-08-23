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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Expected-Value-Dominant Risk Acceptability for Energy Systems
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   The expected-value-dominant reading of acceptable risk for energy systems
 *   instantiates probabilistic risk assessment (PRA) as the governing
 *   constraint for nuclear licensing and energy policy comparison. It claims
 *   to be a neutral coordination tool (Rope) — a common metric enabling
 *   rational comparison across technologies. The authored metrics reveal
 *   moderate extraction (0.38) concentrated on tail-risk bearers whose
 *   catastrophic risks are probability-weighted into negligible annualized
 *   values, low suppression (0.22) because tail-risk framings remain legally
 *   and discursively available though structurally marginalized, and low
 *   theater (0.18) because the PRA machinery genuinely performs coordination
 *   work for regulators and climate policy. The constraint is a kernel
 *   reading: the same kernel 'acceptable_risk_for_energy' admits
 *   catastrophic_tail_dominant and comparative_risk_dominant readings that
 *   produce different victim sets and extraction profiles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.38).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.22).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Risk Acceptability for Energy Systems").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'ca058c5e-99e6-474f-861f-d1717c0b93ba').
narrative_ontology:cs_kernel_codification('ca058c5e-99e6-474f-861f-d1717c0b93ba', formalized).
narrative_ontology:cs_authority_grounding('ca058c5e-99e6-474f-861f-d1717c0b93ba', expertise).
narrative_ontology:cs_interpretation_layer_present('ca058c5e-99e6-474f-861f-d1717c0b93ba').
narrative_ontology:cs_reading_relation('ca058c5e-99e6-474f-861f-d1717c0b93ba', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('ca058c5e-99e6-474f-861f-d1717c0b93ba', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('ca058c5e-99e6-474f-861f-d1717c0b93ba', foundational, expected_value_commensurability).
narrative_ontology:cs_axiom_status(expected_value_commensurability, holdable).
narrative_ontology:cs_axiom_grounding('ca058c5e-99e6-474f-861f-d1717c0b93ba', expected_value_commensurability, empirically_contingent).
narrative_ontology:cs_axiom('ca058c5e-99e6-474f-861f-d1717c0b93ba', foundational, tail_events_not_privileged).
narrative_ontology:cs_axiom_status(tail_events_not_privileged, holdable).
narrative_ontology:cs_axiom_grounding('ca058c5e-99e6-474f-861f-d1717c0b93ba', tail_events_not_privileged, empirically_contingent).
narrative_ontology:cs_reference_frame('ca058c5e-99e6-474f-861f-d1717c0b93ba', probabilistic_risk_assessment_paradigm).
narrative_ontology:cs_drift_state('ca058c5e-99e6-474f-861f-d1717c0b93ba', post_fukushima_climate_urgency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca058c5e-99e6-474f-861f-d1717c0b93ba', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_makers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, regulatory_authorities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, tail_risk_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate nuclear plants under licensing frameworks that adopt expected-value risk metrics. When the framework yields favorable expected value (low annualized risk, high climate benefit), they gain regulatory approval and social license. Their exit options are constrained by capital intensity and site specificity — they cannot easily relocate or switch technologies.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators, beneficiary,
    organized, biographical, constrained, national).

% Nuclear regulatory commissions (NRC, ONR, ASN, etc.) set the risk-informed regulatory framework. They adopt probabilistic risk assessment (PRA) methodologies as the technical basis for licensing decisions. They benefit from a defensible, calculable decision procedure that withstands judicial review. They are not directly extracted from but administer the constraint.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% National and international climate policy bodies (IPCC, UNFCCC, national energy ministries) need a risk framework that can rank nuclear against fossil alternatives on commensurable terms. Expected-value analysis lets them count climate benefits as negative risk (avoided emissions), making nuclear favorable. They can shift frameworks if political winds change — mobile at the institutional level.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_makers, beneficiary,
    institutional, generational, mobile, global).

% Communities near nuclear facilities, downstream populations, and workers who bear the concentrated consequences of low-probability high-consequence events (radiological release, long-term contamination). Under expected-value weighting, their catastrophic tail risks are multiplied by very low probabilities, yielding small annualized numbers that disappear into the calculus. Exit is constrained by housing, employment, and community ties — they cannot easily leave the risk shadow.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_bearers, payer,
    moderate, biographical, constrained, regional).

% Bear the intergenerational burden of waste disposal and latent contamination risks that extend beyond any discounting horizon. They have no voice in current risk calculus and no exit — they are structurally trapped in the consequences of today's expected-value decisions. The framework treats waste as a solvable engineering challenge with finite cost, pushing the burden forward.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Environmental organizations, nuclear-critical scientists, and community groups who argue that catastrophic risks require special weighting (precautionary principle, irreversibility, intergenerational equity). They participate in public comment processes but the expected-value framework structurally marginalizes their framing — their inputs are heard but not integrated into the core calculus. They can shift venues (courts, legislatures, international forums) — mobile at the advocacy level.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_advocates, excluded,
    organized, biographical, mobile, national).

% Academic and institutional analysts (National Academies, IAEA safety standards committees, energy systems modelers) who study risk frameworks comparatively. They observe that expected-value dominance coexists with comparative and tail-risk frameworks in different jurisdictions and decision contexts. They hold no stake in the outcome but map the structural field.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, comparative_risk_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, calculable, technology-neutral metric (expected annual cost/risk) that allows regulators, investors, and policymakers to compare nuclear, fossil, and renewable options on commensurable terms — enabling portfolio decisions, licensing standards, and climate policy integration.
% TRANSFER_FUNCTION: Moves decision authority from qualitative safety judgments (defense-in-depth, precaution) to quantitative probabilistic risk assessment; shifts burden of proof from 'demonstrate absolute safety' to 'show expected value favorable vs. alternatives'; transfers tail-risk burden from operators/regulators to exposed communities by rendering low-probability catastrophic consequences as negligible annualized values.
% ABSENT_VOICES: Communities in the radiological plume pathway (tail-risk bearers) who would reject the probability-weighting of catastrophic consequences; future generations who bear waste and contamination burdens beyond any discount horizon; indigenous nations with intergenerational stewardship ethics that treat certain risks as categorically unacceptable regardless of probability.
% DISAPPEARANCE_RATIONALE: If expected-value dominance vanished overnight, nuclear licensing would revert to deterministic defense-in-depth standards (higher capital cost, slower deployment), climate policy would lose its primary quantitative argument for nuclear inclusion, and tail-risk frameworks (precautionary principle, ALARA) would regain regulatory primacy — the energy transition pathway would materially reorganize.
% FOUNDING_PROBLEM: The 1970s-80s crisis of nuclear licensing: deterministic safety standards produced unbounded costs and unpredictable schedules, while utilities and regulators needed a rational, comparable method to justify continued nuclear deployment alongside emerging climate concerns. WASH-1400 (1975) established probabilistic risk assessment as the technical solution.
% FOUNDING_PROBLEM_CORROBORATION: NRC and IAEA historical records corroborate the licensing crisis as the founding driver. Climate policy bodies (IPCC AR5/AR6, IEA Net Zero Roadmap) corroborate the climate-benefit integration as a living extension. Tail-risk critics (Nuclear Energy Agency 'Risk and Radiation' studies, environmental justice literature, National Academies 'Improving Risk Communication' reports) corroborate that the original problem (licensing paralysis) is substantially solved but the framework now serves a different function (climate justification) that the founding actors did not anticipate.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects the systematic transfer of tail-risk burden from operators/regulators to exposed communities via probability-weighting — the constraint extracts by rendering catastrophic consequences commensurable with routine risks. Suppression (0.22) is low because the framework does not ban tail-risk arguments; they are heard in hearings, modeled in sensitivity analyses, and available in court — but they do not move the core calculus. Theater (0.18) is low because PRA genuinely solves the coordination problem of technology-neutral licensing. Accessibility collapse (0.45) is moderate: alternative frameworks exist and are used in parallel (comparative risk in some jurisdictions, tail-risk in waste policy) but expected-value dominates the licensing gateway. Resistance (0.52) is moderate: tail-risk advocates maintain persistent institutional presence but have not displaced the core framework.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory_authorities seat, this constraint computes as Rope — a genuine coordination achievement that solved licensing paralysis. From the tail_risk_bearers seat, it computes as Tangled Rope or Snare — coordination for others, extraction for them. The engine computes this divergence from the structural data (beneficiaries, victims, exit_options, power). The claimed_type 'rope' reflects the authoring seat's structural reading; the engine's per-seat output will differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and climate policy makers are beneficiaries (d ~ 0.15-0.25): they gain a decision pathway that can yield favorable results. Regulatory authorities are agenda_setters (d ~ 0.3): they administer the constraint and gain legitimacy from its technical defensibility. Tail-risk bearers are payers (d ~ 0.8): they bear concentrated catastrophic consequences that the constraint renders invisible in its core metric. Future generations are excluded/trapped (d ~ 1.0): they have no voice and no exit. Tail-risk advocates are excluded/mobile (d ~ 0.6): they participate but cannot shift the core metric. Comparative risk analysts are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (licensing paralysis under deterministic standards) is substantially solved — PRA delivers predictable, comparable licensing. But the constraint has acquired a second function (climate policy justification) that the founding actors did not build it for. This functional drift creates mandatrophy risk: the constraint persists because it now serves climate policy, not because the original licensing problem requires it. The status 'contested' captures this — the founding problem is dead for licensing but live for climate integration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the expected-value-dominant reading represent a genuine technical consensus on risk commensurability, or does it function as a commitment-system kernel that structurally privileges nuclear by making climate benefits commensurable with radiological risks while rendering tail risks commensurable only through probability-weighting?',
    'Trace the genealogical path from WASH-1400 (licensing crisis) to IPCC AR6 (climate integration): if the same mathematical formalism serves both without modification, the kernel is technically stable; if climate-benefit integration required redefining ''risk'' to include negative risk (avoided emissions), the kernel has been stretched to serve a new function.',
    'If the kernel is stretched, the expected-value reading is a mandate-expansion (mandatrophy) rather than a stable coordination solution — the constraint''s extraction from tail-risk bearers is not the price of coordination but the price of climate justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the expected-value kernel is technically invariant across its licensing and climate-policy functions.').

omega_variable(
    tail_risk_suppression_mechanism,
    'Is the marginalization of tail-risk framings structural (the probability-weighting math inherently compresses catastrophic consequences) or internalized (tail-risk advocates accept the framework''s premises and only argue about parameter values)?',
    'Examine regulatory hearing transcripts and intervenor filings: do tail-risk advocates challenge the probability-weighting axiom itself, or only the input probabilities? If the former, suppression is structural; if the latter, advocates have internalized the frame.',
    'If structural, the constraint''s effective suppression is higher than the authored 0.22 — the suppression is baked into the calculus, not imposed externally. If internalized, the suppression metric captures the full picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_suppression_mechanism, empirical, 'Structural vs. internalized suppression of tail-risk framings within expected-value calculus.').

omega_variable(
    climate_radiological_commensurability,
    'Are avoided carbon emissions (climate benefit) and radiological risk (nuclear cost) genuinely commensurable on a single expected-value scale, or does the commensurability claim mask a category error that systematically favors nuclear by converting a global public good (climate) into a local risk offset?',
    'Compare the discount rates, spatial scales, and uncertainty distributions used for climate benefit quantification vs. radiological risk quantification in actual licensing PRAs. If they differ systematically, commensurability is a modeling choice, not a measurement fact.',
    'If commensurability is a modeling choice, the extraction from tail-risk bearers is not a side effect of coordination but the mechanism by which climate policy captures nuclear''s risk budget — the constraint is a transfer device, not a measurement device.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_radiological_commensurability, conceptual, 'Whether climate benefits and radiological risks are genuinely commensurable or artificially equated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 1975, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arfe_evd_tr_t1975, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(arfe_evd_tr_t1986, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1986, 0.12).
narrative_ontology:measurement(arfe_evd_tr_t1995, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(arfe_evd_tr_t2005, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(arfe_evd_tr_t2011, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2011, 0.25).
narrative_ontology:measurement(arfe_evd_tr_t2018, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(arfe_evd_tr_t2024, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(arfe_evd_be_t1975, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(arfe_evd_be_t1986, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1986, 0.28).
narrative_ontology:measurement(arfe_evd_be_t1995, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(arfe_evd_be_t2005, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2005, 0.31).
narrative_ontology:measurement(arfe_evd_be_t2011, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2011, 0.42).
narrative_ontology:measurement(arfe_evd_be_t2018, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(arfe_evd_be_t2024, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(arfe_evd_su_t1975, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(arfe_evd_su_t1986, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1986, 0.35).
narrative_ontology:measurement(arfe_evd_su_t1995, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(arfe_evd_su_t2005, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(arfe_evd_su_t2011, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2011, 0.3).
narrative_ontology:measurement(arfe_evd_su_t2018, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement(arfe_evd_su_t2024, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, nuclear_waste_disposal_framework).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_portfolio_standards).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, radiation_protection_standards_icrp).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, energy_justice_assessment_protocols).

% DUAL FORMULATION NOTE:
% Part of the acceptable_risk_for_energy kernel family. This reading (expected_value_dominant) coexists with catastrophic_tail_dominant and comparative_risk_dominant. The epsilon values differ: expected-value reading has moderate extraction (tail-risk bearers pay); catastrophic-tail reading has high extraction (nuclear industry pays via prohibition); comparative-risk reading has low extraction (all technologies assessed relative to each other).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, organized, 0.2).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, moderate, 0.75).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
