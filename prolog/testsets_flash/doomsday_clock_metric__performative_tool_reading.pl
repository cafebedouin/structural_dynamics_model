% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock as Performative Policy Tool
 *   domain: science_communication/risk_governance
 *
 * SUMMARY:
 *   This constraint describes the Doomsday Clock as a performative tool,
 *   where its setting is strategically chosen to maximize policy impact and
 *   mobilize collective action, rather than strictly reflecting an objective
 *   risk assessment. The Bulletin of Atomic Scientists, as the agenda-setter,
 *   benefits from the influence and attention generated, while the epistemic
 *   credibility of scientific communication and the broader scientific
 *   community bear the cost of this strategic manipulation. The claimed type
 *   is 'tangled_rope' because it still serves a coordination function
 *   (mobilizing action) but does so through an extractive mechanism
 *   (sacrificing epistemic rigor for impact).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.2).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '4175b4c8-e826-4d3a-9b91-66a592dd84b0').
narrative_ontology:cs_kernel_codification('4175b4c8-e826-4d3a-9b91-66a592dd84b0', formalized).
narrative_ontology:cs_authority_grounding('4175b4c8-e826-4d3a-9b91-66a592dd84b0', expertise).
narrative_ontology:cs_interpretation_layer_present('4175b4c8-e826-4d3a-9b91-66a592dd84b0').
narrative_ontology:cs_reading_relation('4175b4c8-e826-4d3a-9b91-66a592dd84b0', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('4175b4c8-e826-4d3a-9b91-66a592dd84b0', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('4175b4c8-e826-4d3a-9b91-66a592dd84b0', foundational, policy_impact_maximization_is_primary).
narrative_ontology:cs_axiom_status(policy_impact_maximization_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('4175b4c8-e826-4d3a-9b91-66a592dd84b0', policy_impact_maximization_is_primary, instrumental).
narrative_ontology:cs_axiom('4175b4c8-e826-4d3a-9b91-66a592dd84b0', secondary, epistemic_rigor_is_subordinate_to_action).
narrative_ontology:cs_axiom_status(epistemic_rigor_is_subordinate_to_action, holdable).
narrative_ontology:cs_axiom_grounding('4175b4c8-e826-4d3a-9b91-66a592dd84b0', epistemic_rigor_is_subordinate_to_action, instrumental).
narrative_ontology:cs_reference_frame('4175b4c8-e826-4d3a-9b91-66a592dd84b0', strategic_communication_framework).
narrative_ontology:cs_drift_state('4175b4c8-e826-4d3a-9b91-66a592dd84b0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4175b4c8-e826-4d3a-9b91-66a592dd84b0', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_advocates).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, public_mobilization_campaigns).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, general_public).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organization that sets and publicizes the Doomsday Clock. From this reading, they strategically adjust the clock to maximize public attention and policy impact, prioritizing mobilization over strict empirical fidelity. They benefit from the attention and influence this generates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Groups and individuals who use the Doomsday Clock's pronouncements to bolster their arguments for specific policy interventions (e.g., nuclear disarmament, climate action). They benefit from the heightened sense of urgency and public attention the clock generates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Leverage the clock's symbolic power to rally public support and participation in collective action initiatives. They gain momentum and visibility from the clock's dramatic pronouncements.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_mobilization_campaigns, beneficiary,
    organized, immediate, mobile, global).

% The general trustworthiness and reliability of scientific pronouncements, particularly on long-term risks. This is 'paid' through a gradual erosion of public trust when the clock's strategic adjustments are perceived as manipulative rather than objective.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_credibility, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).

% Individual scientists and research institutions who rely on public trust and perceived objectivity. They bear the cost of diminished credibility when the clock's strategic use blurs the lines between scientific assessment and advocacy, potentially undermining broader scientific authority.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_community, payer,
    organized, generational, constrained, global).

% Receives simplified, urgent messages about existential risks, which can spur engagement but also lead to alarm fatigue or cynicism if the messaging is perceived as overly strategic. They pay in terms of potential misinformed risk perception or eroded trust in expert warnings.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, general_public, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, general_public, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public attention and political will around existential threats by providing a simple, dramatic metric that signals urgency and the need for collective action.
% TRANSFER_FUNCTION: Transfers public attention and political capital towards specific policy agendas, from a general, diffuse concern about future risks.
% ABSENT_VOICES: Strict empiricists and methodologists who would argue for a more transparent, less manipulable metric, but whose concerns are often sidelined in favor of maximizing impact.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock vanished, a significant tool for mobilizing public and political attention on existential risks would be lost. While other mechanisms exist, the clock's unique symbolic power and media penetration would leave a void, requiring new strategies for risk communication and advocacy.
% FOUNDING_PROBLEM: The problem of communicating the urgency of nuclear proliferation and other global catastrophic risks to a broad public and policymakers in a way that compels action.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of Atomic Scientists attests the problem is live, citing ongoing nuclear threats and climate change. Policy advocates and public mobilization campaigns corroborate this, as they continue to rely on such tools to gain traction for their causes. The scientific community, while critical of the method, generally agrees on the existence of the underlying risks.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the strategic manipulation of the clock's setting extracts from epistemic credibility to benefit policy advocacy. Suppression is low (0.20) because there's no direct coercion, but rather a subtle pressure to conform to the narrative of urgency. Theater ratio is very high (0.80) as the primary function becomes the performance of urgency and impact, with the underlying 'measurement' aspect becoming largely theatrical. The increasing theater ratio over time reflects a growing emphasis on strategic communication over strict empirical reporting.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policy advocates, the clock is a highly effective 'rope' for coordinating action on critical issues. From the perspective of the scientific community, it operates more like a 'snare' that compromises scientific integrity for political ends. The engine's classification will capture this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of Atomic Scientists, policy advocates, and public mobilization campaigns are beneficiaries, as they gain influence and momentum from the clock's strategic use. Epistemic credibility and the scientific community are victims, as their long-term trustworthiness is eroded. The general public is both a beneficiary (receiving urgent warnings) and a payer (potentially misinformed or becoming cynical).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_vs_credibility_tradeoff,
    'What is the optimal balance between maximizing policy impact and maintaining epistemic credibility in communicating existential risks?',
    'Empirical studies on the long-term effects of strategic communication on public trust and policy outcomes, combined with normative deliberation on the ethical boundaries of scientific advocacy.',
    'If a high impact can be achieved without significant credibility loss, the extractiveness of this constraint would decrease. If the tradeoff is severe, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_vs_credibility_tradeoff, preference, 'The inherent tension between strategic communication and scientific integrity.').

omega_variable(
    alternative_mobilization_mechanisms,
    'Are there alternative, less epistemically extractive mechanisms for mobilizing collective action on existential risks?',
    'Development and evaluation of new science communication models that prioritize transparency and empirical rigor while still achieving policy impact.',
    'The existence of viable alternatives would reduce the perceived necessity of this constraint''s strategic manipulation, potentially reclassifying it towards a ''snare'' if the coordination function is found to be separable from the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_mobilization_mechanisms, empirical, 'Availability of less extractive methods for risk communication.').

omega_variable(
    framing_underdetermination_doomsday_clock,
    'Is the Doomsday Clock primarily an objective index, a performative tool, or an irreducible hybrid of both?',
    'Conceptual analysis and expert consensus on the nature of ''boundary objects'' in science communication, combined with empirical studies of how different audiences interpret the clock.',
    'If resolved towards the ''objective_index_reading'', this constraint would be reclassified as a ''mountain'' or ''rope'' with significantly lower extractiveness. If resolved towards the ''hybrid_legitimacy_reading'', it would acknowledge the entanglement but potentially reduce the ''theater_ratio'' of this specific reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_doomsday_clock, conceptual, 'Ambiguity in the fundamental nature and purpose of the Doomsday Clock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.3).
narrative_ontology:measurement(doom_tr_t1960, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1960, 0.45).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2000, 0.7).
narrative_ontology:measurement(doom_tr_t2010, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2010, 0.75).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2024, 0.8).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(doom_be_t1960, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(doom_be_t2010, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(doom_su_t1960, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2000, 0.17).
narrative_ontology:measurement(doom_su_t2010, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, global_nuclear_disarmament_treaties).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, climate_change_mitigation_policies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'doomsday_clock_metric' kernel, each representing a distinct structural claim about its operation and purpose. This 'performative_tool_reading' focuses on its strategic use for mobilization, distinct from an 'objective_index_reading' or a 'hybrid_legitimacy_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
