% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Dominant Reading of Nuclear Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint story captures the comparative_risk_dominant reading of
 *   the acceptable_risk_for_energy kernel: a policy and regulatory framework
 *   under which nuclear energy risks are deemed acceptable solely in relation
 *   to fossil fuel and climate catastrophe alternatives, with no absolute
 *   safety threshold. The framework is formalized in national regulatory
 *   guidelines and international atomic energy standards. It coordinates
 *   energy transition investment while asymmetrically imposing risks on host
 *   communities, future generations, and climate-vulnerable populations whose
 *   vulnerability is used to justify the comparative baseline.
 *
 * KEY AGENTS:
 *   - nuclear_policy_authority: Agenda setter (institutional/arbitrage) â sets comparative risk standards
 *   - nuclear_energy_sector: Primary beneficiary (powerful/mobile) â captures revenue from nuclear deployment
 *   - nuclear_host_communities: Primary target (powerless/trapped/local) â bears localized risk deemed acceptable by comparison
 *   - future_generations: Intergenerational target (powerless/trapped/civilizational) â inherits waste without consent
 *   - climate_vulnerable_populations: Structural target (powerless/trapped/global) â vulnerability appropriated to justify framework
 *   - environmental_justice_advocates: Analytical observer (organized/analytical) â documents distributional asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.66).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.64).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.66).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Dominant Reading of Nuclear Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, 'cda9f2de-fb2b-46b5-8164-f2e2a69b79f5').
narrative_ontology:cs_kernel_codification('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', formalized).
narrative_ontology:cs_authority_grounding('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', expertise).
narrative_ontology:cs_interpretation_layer_present('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5').
narrative_ontology:cs_reading_relation('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', foundational, energy_acceptability_requires_relative_harm_comparison).
narrative_ontology:cs_axiom_status(energy_acceptability_requires_relative_harm_comparison, holdable).
narrative_ontology:cs_axiom_grounding('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', energy_acceptability_requires_relative_harm_comparison, instrumental).
narrative_ontology:cs_axiom('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', foundational, present_climate_urgency_overrides_intergenerational_equity).
narrative_ontology:cs_axiom_status(present_climate_urgency_overrides_intergenerational_equity, holdable).
narrative_ontology:cs_axiom_grounding('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', present_climate_urgency_overrides_intergenerational_equity, deontological).
narrative_ontology:cs_reference_frame('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', fossil_baseline_risk_governance).
narrative_ontology:cs_drift_state('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', contemporary_climate_urgency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cda9f2de-fb2b-46b5-8164-f2e2a69b79f5', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of risk acceptability for nuclear energy by mandating comparative assessment against fossil fuel baselines. Defends the framework through regulatory guidelines, licensing criteria, and international safety standards. Can shift the evaluative baseline but derives institutional legitimacy from appearing scientifically neutral.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_policy_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Collects revenue and policy support from nuclear deployment enabled by the comparative risk frame. Does not set the risk standard but actively advocates for its maintenance. Can diversify into other energy sectors but currently profits from the nuclear-specific regulatory environment.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_sector, beneficiary,
    powerful, biographical, mobile, global).

% Live near reactors, waste storage, or mining facilities. Bear localized health, safety, and property value impacts of nuclear infrastructure. Their risk is deemed acceptable only by comparison to coal, not by absolute standards. Exit is economically and socially constrained; relocation is costly and severs community ties.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_host_communities, payer,
    powerless, generational, trapped, local).

% Will inherit long-lived radioactive waste and decommissioned facility liabilities without having consented to their creation. The comparative risk framework discounts their burdens relative to present climate mitigation benefits.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__comparative_risk_dominant, future_generations).

% Bear disproportionate climate impacts that the comparative risk framework cites to justify nuclear acceptability, yet receive limited direct protection from either the nuclear deployment or the adaptation funding the framework promises. Their vulnerability is used as a rhetorical counterweight to nuclear risk concerns while structural investment in their resilience remains inadequate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, global).

% Analyze and contest the comparative risk framework, arguing that it transfers harms from climate-vulnerable and host communities to benefit present energy consumers and industry. They document the distributional asymmetries the comparative baseline obscures.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, environmental_justice_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_energy_sector).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves energy policy deadlock by providing a decision rule that permits nuclear deployment when its risks are judged inferior to fossil fuel alternatives, thereby coordinating investment and regulatory approval around a common evaluative metric.
% TRANSFER_FUNCTION: Moves risk tolerance and investment capital from populations exposed to nuclear harms toward present energy production and climate-mitigation accounting, while transferring rhetorical legitimacy from climate-vulnerable populations to nuclear policy authorities.
% ABSENT_VOICES: Renewables-only advocates and precautionary-principle ethicists who reject both nuclear and fossil baselines are structurally excluded from comparative risk regulatory processes; indigenous land-rights holders affected by uranium mining and waste siting are often absent from the national-level risk comparisons that license projects on their territories.
% DISAPPEARANCE_RATIONALE: If the comparative risk constraint vanished overnight, nuclear licensing would stall absent an alternative decision framework, fossil fuel retirement timelines would destabilize, and energy policy would fragment into jurisdiction-specific absolute-risk standards â the global coordination function would collapse.
% FOUNDING_PROBLEM: Mid-20th century energy policy faced a deadlock between growing electricity demand, fossil fuel pollution, and public fear of nuclear accidents; regulators needed a standard that could license nuclear plants without requiring zero risk.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear policy authorities and industry attest the problem remains live because climate targets require baseload. Environmental justice advocates and some climate scientists attest the founding problem has shifted â the current crisis is not a shortage of acceptable risk frameworks but an excess of incumbent energy systems blocking distributed renewables; independent energy-systems research from outside the benefiting parties supports the shifted-function reading.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) reflects the systematic transfer of risk from concentrated beneficiary seats (industry, present consumers) to diffuse payer seats (host communities, future generations, climate-vulnerable populations) under cover of a scientific comparative method. Suppression (0.64) is structural: absolute-risk and precautionary alternatives are formally excluded from regulatory consideration in jurisdictions adopting comparative frameworks. Theater_ratio (0.42) captures the performative objectivity of probabilistic risk assessments that embed normative baseline choices. Accessibility_collapse (0.65) indicates that once the comparative frame is institutionalized, zero-risk or renewables-only alternatives become cognitively inaccessible in policy discourse. Resistance (0.58) comes from environmental justice movements and affected communities. The temporal series show gradual intensification as climate urgency has been invoked to harden the comparative frame against intergenerational objections.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (policy authority) experiences the constraint as a necessary scientific coordination mechanism; the payer seats (host communities, future generations, climate-vulnerable populations) experience it as an imposed risk transfer whose legitimacy depends on a baseline they did not choose. The engine will compute divergent per-seat classifications: the policy authority may read as rope or scaffold, while trapped payer seats compute as snare or tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_policy_authority sits near the beneficiary end because it derives institutional legitimacy and coordination capacity; nuclear_energy_sector is a direct beneficiary. Nuclear_host_communities, future_generations, and climate_vulnerable_populations are structural targets because the constraint specifically legitimates imposing risk upon them. Environmental_justice_advocates are analytical, neither collecting nor paying. No override is needed because structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the framework as pure extraction (snare) â there is a genuine coordination function in resolving energy policy deadlock and providing a low-carbon baseload pathway. It also prevents mislabeling as pure coordination (rope) â the asymmetric risk imposition on non-consenting populations and the active suppression of absolute-threshold alternatives demonstrate that coordination and extraction are structurally coupled. If the founding problem (energy policy deadlock) is dead but the arrangement persists, the scaffold-to-piton drift path would become relevant; current evidence suggests the founding problem is contested, keeping the constraint in tangled_rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_tail_alternative_structural_delta,
    'How would the stakeholder directionality and victim set change if the catastrophic_tail_dominant reading of this kernel were adopted?',
    'Comparative analysis of the same policy framework under catastrophic_tail premises: identify whether future generations and local risk-bearing populations would shift from moderate directionalities to full-target status.',
    'A catastrophic-tail reading would likely increase computed extractiveness by treating present-generation energy consumers as beneficiaries of risk deferral, and would reclassify intergenerational burdens from acceptable trade-off to unjustified harm transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_tail_alternative_structural_delta, conceptual, 'Sibling reading structural delta for catastrophic_tail_dominant').

omega_variable(
    expected_value_alternative_baseline,
    'Does the comparative risk reading extract additional legitimacy by shifting the evaluative baseline from zero-risk to fossil-fuel baseline, and would an expected_value_dominant reading revert to a standalone assessment?',
    'Cross-reading comparison of baseline assumptions in policy documents and regulatory impact assessments.',
    'If the baseline shift is the primary extraction mechanism, expected_value_dominant would partially neutralize it by requiring absolute risk accounting; this would reduce effective extraction for climate-vulnerable populations currently used as comparative justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expected_value_alternative_baseline, conceptual, 'Sibling reading structural delta for expected_value_dominant').

omega_variable(
    comparative_risk_suppression_ambiguity,
    'Is the suppression of absolute-threshold risk frameworks structural (regulatory exclusion) or internalized (public and expert acceptance of comparative heuristics as common sense)?',
    'Review of regulatory dockets and public comment periods to see whether absolute-threshold alternatives are structurally barred from consideration, versus appearing in discourse but dismissed as irrational.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint reproduces itself through cognitive capture of policymakers and publics even without formal prohibition of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_risk_suppression_ambiguity, empirical, 'Structural vs internalized suppression in comparative risk framing').

omega_variable(
    climate_vulnerable_victim_mechanism,
    'Are climate-vulnerable populations victims of this constraint because comparative risk framing diverts adaptation resources to nuclear infrastructure, or because their vulnerability is narratively appropriated to justify risk imposition on others?',
    'Empirical tracing of climate adaptation funding flows and energy policy justifications in jurisdictions adopting comparative nuclear risk frameworks.',
    'If resource diversion, extractiveness is direct; if narrative appropriation, extraction operates primarily through legitimacy transfer and the victim classification should be treated as representational rather than material.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_vulnerable_victim_mechanism, empirical, 'Material vs representational victimhood for climate-vulnerable populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cr_comparative_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cr_comparative_tr_t8, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 8, 0.28).
narrative_ontology:measurement(cr_comparative_tr_t16, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 16, 0.33).
narrative_ontology:measurement(cr_comparative_tr_t24, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 24, 0.38).
narrative_ontology:measurement(cr_comparative_tr_t32, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 32, 0.4).
narrative_ontology:measurement(cr_comparative_tr_t40, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cr_comparative_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cr_comparative_be_t8, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(cr_comparative_be_t16, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(cr_comparative_be_t24, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(cr_comparative_be_t32, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(cr_comparative_be_t40, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cr_comparative_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cr_comparative_su_t8, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(cr_comparative_su_t16, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(cr_comparative_su_t24, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(cr_comparative_su_t32, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(cr_comparative_su_t40, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 40, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
