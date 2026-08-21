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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock as Performative Policy Tool
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint story models the Doomsday Clock metric from the
 *   perspective of a 'performative tool' reading. In this view, the clock's
 *   setting is strategically chosen by the Bulletin of the Atomic Scientists
 *   to maximize policy impact and mobilize collective action on existential
 *   risks, rather than serving as a purely objective index. The primary
 *   function is communication and activation, even if it entails a cost to
 *   epistemic credibility. The claimed type is Tangled Rope, reflecting a
 *   genuine coordination function (mobilizing action) coupled with asymmetric
 *   extraction (from epistemic credibility and nuanced understanding).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.7).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.75).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '9aefef46-4640-4552-a949-7a236496356f').
narrative_ontology:cs_kernel_codification('9aefef46-4640-4552-a949-7a236496356f', formalized).
narrative_ontology:cs_authority_grounding('9aefef46-4640-4552-a949-7a236496356f', extraction).
narrative_ontology:cs_interpretation_layer_present('9aefef46-4640-4552-a949-7a236496356f').
narrative_ontology:cs_reading_relation('9aefef46-4640-4552-a949-7a236496356f', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('9aefef46-4640-4552-a949-7a236496356f', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('9aefef46-4640-4552-a949-7a236496356f', foundational, policy_impact_is_primary_metric).
narrative_ontology:cs_axiom_status(policy_impact_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('9aefef46-4640-4552-a949-7a236496356f', policy_impact_is_primary_metric, instrumental).
narrative_ontology:cs_axiom('9aefef46-4640-4552-a949-7a236496356f', secondary, epistemic_purity_is_subordinate).
narrative_ontology:cs_axiom_status(epistemic_purity_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('9aefef46-4640-4552-a949-7a236496356f', epistemic_purity_is_subordinate, conventional).
narrative_ontology:cs_reference_frame('9aefef46-4640-4552-a949-7a236496356f', policy_impact_maximization).
narrative_ontology:cs_drift_state('9aefef46-4640-4552-a949-7a236496356f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9aefef46-4640-4552-a949-7a236496356f', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, risk_governance_institutions).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, media_outlets).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_community_members_valuing_objectivity).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organization responsible for setting and announcing the Doomsday Clock. In this reading, they strategically choose the clock's setting to maximize public and policy impact, prioritizing mobilization over strict empirical fidelity.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_the_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Utilize the Doomsday Clock's symbolic power and alarmist messaging to draw attention to existential risks and advocate for specific policy interventions. They benefit from the heightened urgency the clock generates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the public and political mobilization spurred by the clock, which can create a more receptive environment for their policy proposals and funding requests related to existential risk mitigation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, risk_governance_institutions, beneficiary,
    institutional, generational, constrained, global).

% Bear the cost of the clock's perceived lack of scientific rigor or strategic manipulation, which can undermine the broader credibility of science communication on complex issues. They may feel compelled to distance themselves or offer caveats.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_community_members_valuing_objectivity, payer,
    moderate, biographical, constrained, global).

% Receives a simplified, often alarmist, message about complex risks. While intended to mobilize, this can lead to desensitization over time or a distorted understanding of the underlying science, bearing the cost of reduced epistemic clarity.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_at_large, payer,
    powerless, immediate, constrained, global).

% An abstract concept representing the trustworthiness and reliability of scientific knowledge. It is 'victimized' when the clock's strategic use for impact is perceived to compromise its objective basis, leading to a loss of public trust in science communication.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_credibility, excluded,
    analytical, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).

% Benefit from the sensational and easily digestible narrative provided by the Doomsday Clock, which generates headlines and audience engagement. They amplify the clock's message, often without deep critical analysis of its methodology.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, media_outlets, beneficiary,
    organized, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes global public and political attention on critical existential risks (e.g., nuclear war, climate change) by providing a simple, urgent, and widely recognized symbolic metric.
% TRANSFER_FUNCTION: Transfers public attention, emotional urgency, and political will from a nuanced, complex understanding of global risks to a simplified, actionable call for policy change, from the scientific community to policy activists and institutions.
% ABSENT_VOICES: Scientists and ethicists who prioritize strict epistemic objectivity and fear the long-term erosion of scientific credibility due to strategic communication. They would argue for a more transparent and less performative approach to risk assessment.
% DISAPPEARANCE_RATIONALE: The Doomsday Clock is a globally recognized symbol for existential risk. Its disappearance would remove a powerful, established tool for framing public discourse and mobilizing collective action on these issues, requiring new, less effective mechanisms to fill the void. The communication landscape for global risks would fundamentally shift.
% FOUNDING_PROBLEM: To effectively communicate the unprecedented and existential threat of nuclear war to a broad public and political audience, translating complex scientific and geopolitical realities into an accessible and urgent warning.
% FOUNDING_PROBLEM_CORROBORATION: Policy makers, advocacy groups, and international organizations continue to cite the clock as a crucial tool for raising awareness and driving action on nuclear and climate risks. Independent analyses of public discourse confirm its ongoing role in shaping narratives around global threats, corroborating its continued utility as a performative tool.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) reflects the cost borne by epistemic credibility and the public's nuanced understanding of risk, as the clock's message is simplified for impact. Suppression (0.75) is high because alternative, more complex, or less alarmist interpretations of risk are actively downplayed or excluded to maintain the clock's singular, urgent message. The very high theater ratio (0.8) indicates that the primary function is performative communication and symbolic action, with the 'scientific index' aspect serving largely as a legitimizing frame. Resistance (0.6) comes from those in the scientific community who prioritize objectivity. Accessibility collapse (0.5) is moderate; while other communication channels exist, none possess the unique symbolic power of the clock.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policy activists, the clock is a highly effective Rope, a vital tool for coordination and mobilization. From the perspective of scientific purists, it operates as a Snare, extracting from scientific integrity for political ends. The engine's computation of per-seat classifications will highlight this divergence, which is central to the 'performative tool' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of the Atomic Scientists, policy activists, and media outlets are beneficiaries, gaining attention, political will, and engagement. Scientific community members valuing objectivity, the public at large (who may become desensitized or misinformed), and the abstract concept of epistemic credibility are the victims, bearing the costs of strategic simplification and potential overstatement. The directionality for the Bulletin is complex; while they are the agenda-setter, this reading implies they are also 'extracting' from credibility for a perceived greater good.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_cost_vs_policy_gain,
    'Is the observed cost to epistemic credibility a necessary and justifiable trade-off for the policy impact and mobilization achieved by the Doomsday Clock, or is it an avoidable side effect?',
    'Comparative analysis of alternative risk communication strategies: if strategies with higher epistemic fidelity achieve comparable policy impact, the cost is avoidable. If not, it may be necessary.',
    'If the cost is deemed necessary, the extraction might be re-evaluated as an inherent cost of effective communication in this domain. If avoidable, the extraction is pure rent-seeking from epistemic capital.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_cost_vs_policy_gain, conceptual, 'Assessing the necessity of epistemic cost for policy impact.').

omega_variable(
    long_term_desensitization_vs_mobilization,
    'Does the repeated alarm of the Doomsday Clock lead to long-term public desensitization and reduced efficacy, or does it maintain a baseline level of awareness and mobilization?',
    'Longitudinal studies of public perception, media analysis, and policy response following clock announcements over several decades, comparing initial impact to sustained engagement.',
    'If desensitization is dominant, the clock''s performative function degrades over time, potentially shifting its classification towards a Piton (atrophied function). If sustained mobilization, its Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_desensitization_vs_mobilization, empirical, 'Impact of repeated alarms on public response and policy efficacy.').

omega_variable(
    reading_identity_doomsday_clock_metric,
    'Is this constraint a genuine ''performative tool'' reading of the Doomsday Clock metric, or is it better understood as a ''hybrid legitimacy'' or ''objective index'' reading?',
    'Analysis of the Bulletin''s stated justifications, public reception, and policy outcomes: if strategic impact is consistently prioritized over empirical precision, this reading is reinforced. If scientific rigor or irreducible entanglement are emphasized, alternative readings gain weight.',
    'Reclassification to ''objective_index_reading'' would imply lower extraction and theater, higher suppression of non-empirical factors. Reclassification to ''hybrid_legitimacy_reading'' would acknowledge both functions, potentially leading to a more nuanced Tangled Rope or even Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_doomsday_clock_metric, conceptual, 'This constraint is one reading of the ''doomsday_clock_metric'' kernel, specifically the ''performative_tool_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(doom_tr_t15, doomsday_clock_metric__performative_tool_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__performative_tool_reading, theater_ratio, 30, 0.7).
narrative_ontology:measurement(doom_tr_t45, doomsday_clock_metric__performative_tool_reading, theater_ratio, 45, 0.75).
narrative_ontology:measurement(doom_tr_t60, doomsday_clock_metric__performative_tool_reading, theater_ratio, 60, 0.78).
narrative_ontology:measurement(doom_tr_t77, doomsday_clock_metric__performative_tool_reading, theater_ratio, 77, 0.8).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(doom_be_t15, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(doom_be_t45, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(doom_be_t60, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(doom_be_t77, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 77, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(doom_su_t15, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(doom_su_t45, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(doom_su_t60, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(doom_su_t77, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 77, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
