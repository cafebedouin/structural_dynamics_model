% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test (Aggregation Doctrine)
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   This constraint instantiates the broad_effects_test reading of the
 *   commerce_clause_scope kernel. Under this reading, the Commerce Clause
 *   authorizes Congress to regulate any economic activity that, in the
 *   aggregate, substantially affects interstate commerce, including
 *   prohibition and comprehensive control of intrastate conduct. The doctrine
 *   originates in New Deal-era jurisprudence (NLRB v. Jones & Laughlin,
 *   Wickard v. Filburn) and underwrites modern federal civil rights,
 *   environmental, and economic regulation. It functions as a tangled rope:
 *   it solves genuine national collective-action problems and enables uniform
 *   standards, while simultaneously extracting sovereignty from state
 *   governments and local economic actors by subsuming their police powers
 *   under a cumulative-effects logic. The expansive victim set includes
 *   virtually all state-level policy experimentation and intrastate commerce.
 *   Sibling readings (narrow_originalist, intermediate_channels) treat the
 *   same constitutional text as imposing stricter limits; this reading treats
 *   those limits as functionally dissolved by economic interdependence.
 *
 * KEY AGENTS:
 *   - federal_regulators: Primary agenda-setter (institutional/analytical) â expand and enforce federal regulatory jurisdiction.
 *   - national_uniform_policy_groups: Primary beneficiary (organized/mobile) â capture uniform national standards via preemption.
 *   - civil_rights_enforcers: Secondary beneficiary (institutional/constrained) â rely on broad commerce power for jurisdictional hooks.
 *   - state_governments: Primary payer (institutional/constrained) â lose regulatory autonomy and police powers.
 *   - local_economic_actors: Secondary payer (moderate/constrained) â regulated based on aggregate effects despite intrastate character.
 *   - constitutional_originalists: Analytical observer (analytical/analytical) â document divergence from original meaning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.82).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.75).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.82).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test (Aggregation Doctrine)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '5652436f-f094-468f-9a3f-edd2d788eea5').
narrative_ontology:cs_kernel_codification('5652436f-f094-468f-9a3f-edd2d788eea5', formalized).
narrative_ontology:cs_authority_grounding('5652436f-f094-468f-9a3f-edd2d788eea5', lineage).
narrative_ontology:cs_interpretation_layer_present('5652436f-f094-468f-9a3f-edd2d788eea5').
narrative_ontology:cs_reading_relation('5652436f-f094-468f-9a3f-edd2d788eea5', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('5652436f-f094-468f-9a3f-edd2d788eea5', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('5652436f-f094-468f-9a3f-edd2d788eea5', foundational, aggregate_economic_effects_create_commerce_power).
narrative_ontology:cs_axiom_status(aggregate_economic_effects_create_commerce_power, holdable).
narrative_ontology:cs_axiom_grounding('5652436f-f094-468f-9a3f-edd2d788eea5', aggregate_economic_effects_create_commerce_power, conventional).
narrative_ontology:cs_axiom('5652436f-f094-468f-9a3f-edd2d788eea5', foundational, regulate_includes_prohibition_and_comprehensive_control).
narrative_ontology:cs_axiom_status(regulate_includes_prohibition_and_comprehensive_control, holdable).
narrative_ontology:cs_axiom_grounding('5652436f-f094-468f-9a3f-edd2d788eea5', regulate_includes_prohibition_and_comprehensive_control, conventional).
narrative_ontology:cs_reference_frame('5652436f-f094-468f-9a3f-edd2d788eea5', comprehensive_national_economic_authority).
narrative_ontology:cs_drift_state('5652436f-f094-468f-9a3f-edd2d788eea5', contemporary_federalism_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('5652436f-f094-468f-9a3f-edd2d788eea5', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_uniform_policy_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcers).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_actors).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, aggregation_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, substantial_effects_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and federal agencies that invoke the Commerce Clause to justify regulation of intrastate economic activity. They set the enforcement agenda, draft legislation with jurisdictional hooks based on aggregate effects, and defend federal power against state challenges.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Advocacy and industry organizations seeking uniform national standards. They benefit from federal preemption of state laws and lobby for broad Commerce Clause authority to avoid navigating fifty different regulatory regimes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_uniform_policy_groups, beneficiary,
    organized, generational, mobile, national).

% Federal actors and organizations relying on Commerce Clause authority to enforce anti-discrimination laws and voting rights in contexts where other federal powers are absent or contested. The broad effects test provides a jurisdictional foundation for national civil rights mandates.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcers, beneficiary,
    institutional, generational, constrained, national).

% State legislatures and executives whose police powers and policy experimentation space are preempted by federal statutes justified under the aggregation doctrine. They bear the loss of regulatory autonomy and the costs of compliance with federal mandates.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% Small businesses and intrastate producers whose activities are regulated or prohibited under federal law based on claimed aggregate effects on interstate commerce, despite limited or no direct participation in interstate markets.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_actors, payer,
    moderate, biographical, constrained, local).

% Legal scholars and jurists analyzing the original public meaning of the Commerce Clause. They document the structural expansion of federal power and argue that the broad effects test departs from the Constitution's text and founding-era understanding.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_originalists, observer,
    analytical, civilizational, analytical, national).

narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables uniform national regulation of economic activity to solve collective action problems, prevent race-to-the-bottom dynamics among states, and enforce civil rights standards across jurisdictions.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy autonomy from state governments and local economic actors to federal regulators and national interest groups, centralizing control over activities with claimed aggregate economic effects.
% ABSENT_VOICES: State sovereignty advocates and strict originalists are partially heard in judicial dissents but systematically outnumbered in legislative and administrative contexts where the broad effects test is invoked; local actors without an interstate footprint lack standing to challenge aggregation logic directly.
% DISAPPEARANCE_RATIONALE: If the broad effects test disappeared overnight, federal civil rights statutes, environmental laws, and much economic regulation would lose their primary constitutional hook; states would regain significant police powers; national interest groups would face a patchwork of state laws; the mobile software and services economy would need to renegotiate compliance across fifty distinct regimes.
% FOUNDING_PROBLEM: The Articles of Confederation failed in part because states erected trade barriers and pursued parochial economic policies, harming national economic union.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and originalist jurists attest the founding problem was preventing state-level protectionism and trade barriers. Federal regulators and national interest groups attest the problem has evolved to encompass national collective-action problems requiring centralized resolution. No party outside the benefiting set fully corroborates the aggregation doctrine as the originally intended solution; corroboration is split between the historical problem (dead) and the contemporary coordination function (live).
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the aggregation doctrine allows federal power to subsume virtually all state economic regulation, extracting sovereignty rather than merely coordinating cross-border trade. Suppression (0.75) is structurally enforced by judicial supremacy, federal preemption, and the absence of viable state exit. Theater ratio (0.40) reflects that genuine coordination functions existânational civil rights enforcement, environmental standards, market uniformityâbut a substantial fraction of Commerce Clause invocation today is constitutional theater masking a general federal police power. Accessibility collapse (0.70) is high because, once the aggregation doctrine is accepted, state-level alternatives are foreclosed as unconstitutional. Resistance (0.55) is moderate: states litigate continuously (Lopez, Morrison, Sebelius) but rarely prevail comprehensively.
 *
 * PERSPECTIVAL GAP:
 *   Federal regulators and national interest groups experience the constraint as necessary coordination for a national economy; state governments experience it as sovereignty usurpation. The divergence is structural: the same doctrine that enables civil rights enforcement nationwide simultaneously destroys state policy experimentation.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators, national uniform policy groups, and civil rights enforcers are structural beneficiaries (low d) because the constraint expands their authority or advances their policy goals. State governments and local economic actors are structural targets (high d) because the constraint extracts their autonomy and subjects them to federal control. The Supreme Court's interpretive role is analytical; its d is neutral/analytical. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâstate trade barriers under the Articles of Confederationâis contested as to whether it persists in a form justifying the current scope. The constraint avoids mislabeling as pure extraction because it continues to serve live coordination functions (civil rights, environment, collective-action problems). It avoids mislabeling as pure coordination because the victim set (state sovereignty) is expansive and the extraction is asymmetric. The metrics reflect this hybridity: high extraction paired with real coordination output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_doctrine_legitimacy,
    'Is the aggregation doctrine a natural implication of the Commerce Clause text, or a constructed expansion that supplants the original meaning?',
    'Historical-linguistic analysis of ''commerce'' and ''regulate'' at the founding; structural comparison with sibling readings'' predictive accuracy regarding constitutional design.',
    'If constructed, the constraint''s extraction is higher than a genuine coordination reading would admit; if natural, the current metrics may overstate asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_legitimacy, conceptual, 'Ambiguity between natural legal development and constructed expansion of federal power.').

omega_variable(
    economic_non_economic_boundary_stability,
    'Can the broad effects test maintain a stable boundary between economic and non-economic activity, or does it inevitably collapse into a general federal police power?',
    'Longitudinal tracking of Supreme Court jurisprudence (Lopez, Morrison, Raich, Sebelius) for the durability of limiting principles.',
    'If the boundary is unstable, the victim set expands indefinitely and the constraint trends toward snare; if stable, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_stability, empirical, 'Stability of the economic-non-economic limiting principle.').

omega_variable(
    kernel_foreclosure_or_coexistence,
    'Does the broad effects test reading logically foreclose the narrow originalist reading within a single constitutional framework, or do they coexist as live interpretive options?',
    'Engine computation from cs_structure axioms; analysis of whether a single jurist can coherently endorse both the aggregate-effects premise and the original trade-facilitation premise.',
    'If foreclosed, the kernel is structurally fractured between incompatible readings; if coexistent, classification varies by interpretive community.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_or_coexistence, conceptual, 'Structural relationship between broad effects and originalist sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_broad_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cc_broad_tr_t16, commerce_clause_scope__broad_effects_test, theater_ratio, 16, 0.3).
narrative_ontology:measurement(cc_broad_tr_t32, commerce_clause_scope__broad_effects_test, theater_ratio, 32, 0.45).
narrative_ontology:measurement(cc_broad_tr_t48, commerce_clause_scope__broad_effects_test, theater_ratio, 48, 0.5).
narrative_ontology:measurement(cc_broad_tr_t64, commerce_clause_scope__broad_effects_test, theater_ratio, 64, 0.45).
narrative_ontology:measurement(cc_broad_tr_t80, commerce_clause_scope__broad_effects_test, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(cc_broad_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cc_broad_be_t16, commerce_clause_scope__broad_effects_test, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(cc_broad_be_t32, commerce_clause_scope__broad_effects_test, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(cc_broad_be_t48, commerce_clause_scope__broad_effects_test, base_extractiveness, 48, 0.82).
narrative_ontology:measurement(cc_broad_be_t64, commerce_clause_scope__broad_effects_test, base_extractiveness, 64, 0.85).
narrative_ontology:measurement(cc_broad_be_t80, commerce_clause_scope__broad_effects_test, base_extractiveness, 80, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(cc_broad_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cc_broad_su_t16, commerce_clause_scope__broad_effects_test, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(cc_broad_su_t32, commerce_clause_scope__broad_effects_test, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(cc_broad_su_t48, commerce_clause_scope__broad_effects_test, suppression_requirement, 48, 0.75).
narrative_ontology:measurement(cc_broad_su_t64, commerce_clause_scope__broad_effects_test, suppression_requirement, 64, 0.78).
narrative_ontology:measurement(cc_broad_su_t80, commerce_clause_scope__broad_effects_test, suppression_requirement, 80, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel. The broad effects test, intermediate channels test, and narrow originalist test are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and classification types. They share a doctrinal label but not a constraint identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
