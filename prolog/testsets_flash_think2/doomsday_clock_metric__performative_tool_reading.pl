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
 *   domain: science_communication/risk_governance/normative_epistemology
 *
 * SUMMARY:
 *   This constraint describes the Doomsday Clock metric from the perspective
 *   of its function as a performative policy tool. Its setting is
 *   strategically chosen to maximize policy impact and mobilize collective
 *   action on global existential risks. While it serves a coordination
 *   function by focusing attention, it also extracts attention and political
 *   will through strategic framing, potentially at the cost of epistemic
 *   rigor and nuanced risk assessment. The high theater ratio reflects its
 *   primary role as a symbolic, communicative device.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.75).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.65).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/risk_governance/normative_epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '4d45a09e-845e-4b99-b689-f24e7fbd157d').
narrative_ontology:cs_kernel_codification('4d45a09e-845e-4b99-b689-f24e7fbd157d', formalized).
narrative_ontology:cs_authority_grounding('4d45a09e-845e-4b99-b689-f24e7fbd157d', lineage).
narrative_ontology:cs_interpretation_layer_present('4d45a09e-845e-4b99-b689-f24e7fbd157d').
narrative_ontology:cs_reading_relation('4d45a09e-845e-4b99-b689-f24e7fbd157d', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d45a09e-845e-4b99-b689-f24e7fbd157d', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('4d45a09e-845e-4b99-b689-f24e7fbd157d', foundational, policy_impact_maximization_is_primary).
narrative_ontology:cs_axiom_status(policy_impact_maximization_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('4d45a09e-845e-4b99-b689-f24e7fbd157d', policy_impact_maximization_is_primary, instrumental).
narrative_ontology:cs_axiom('4d45a09e-845e-4b99-b689-f24e7fbd157d', secondary, epistemic_rigor_is_secondary_to_action).
narrative_ontology:cs_axiom_status(epistemic_rigor_is_secondary_to_action, holdable).
narrative_ontology:cs_axiom_grounding('4d45a09e-845e-4b99-b689-f24e7fbd157d', epistemic_rigor_is_secondary_to_action, instrumental).
narrative_ontology:cs_reference_frame('4d45a09e-845e-4b99-b689-f24e7fbd157d', urgent_mobilization_imperative).
narrative_ontology:cs_drift_state('4d45a09e-845e-4b99-b689-f24e7fbd157d', contemporary_multi_threat_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d45a09e-845e-4b99-b689-f24e7fbd157d', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, risk_advocacy_organizations).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, nuanced_risk_assessments).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the Doomsday Clock, manages its public presentation, and defends its methodology. Prioritizes its role in public mobilization and policy impact over strict adherence to a purely objective index.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_the_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Leverage the clock's pronouncements to amplify their advocacy for specific policies related to nuclear weapons, climate change, and other global risks, benefiting from the generated urgency and attention.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the heightened public awareness and media attention generated by the clock, which aids their fundraising, outreach efforts, and ability to influence public opinion and policy.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, risk_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% Some members feel their epistemic standards are compromised by the clock's performative nature and strategic framing, bearing the cost of potential erosion of trust in scientific institutions. Others see value in its public engagement role.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_community, payer,
    organized, generational, constrained, global).

% Receives a simplified, often alarmist, view of complex existential risks, potentially leading to anxiety, cynicism, or a distorted understanding of the underlying science rather than informed action.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_discourse, payer,
    powerless, immediate, trapped, global).

% Academics and commentators who argue for more rigorous, transparent, and less emotionally charged methods of risk assessment. Their voices are often marginalized or reframed as undermining necessary action by the clock's media prominence.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_critics, excluded,
    moderate, biographical, mobile, global).

% Researchers who study the sociology of science, risk communication, and the impact of symbolic metrics like the Doomsday Clock on public perception and policy, analyzing its effects without direct participation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, analytical_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes public and political attention towards existential risks, fostering collective action on issues like nuclear proliferation, climate change, and emerging technologies by providing a clear, symbolic warning.
% TRANSFER_FUNCTION: Transfers public attention, media coverage, and political urgency towards specific policy agendas, from the general public and segments of the scientific community to policy activists and advocacy groups, often at the cost of nuanced understanding.
% ABSENT_VOICES: Epistemic purists who prioritize objective, dispassionate risk assessment over policy impact; they would argue for a more transparent, less alarmist methodology that separates scientific judgment from advocacy.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock and its symbolic power vanished overnight, a significant and widely recognized tool for mobilizing public and political attention on existential risks would disappear, requiring new mechanisms for collective action and risk communication to fill the void.
% FOUNDING_PROBLEM: The urgent need to communicate the existential threat of nuclear weapons to the public and policymakers in a compelling, easily understandable way during the Cold War, to spur action against proliferation.
% FOUNDING_PROBLEM_CORROBORATION: Policy makers, advocacy groups, and some segments of the public attest to its continued utility in raising awareness for contemporary threats like climate change and AI. Critics, however, question its scientific rigor and whether it still effectively addresses the original problem or has become a tool for other agendas.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects how the clock's strategic framing draws significant public and political attention, which can be channeled towards specific policy agendas. Suppression (0.65) is moderately high as the clock's prominence can marginalize alternative, more nuanced risk assessments that might not generate the same level of urgency. The very high theater ratio (0.80) indicates that its primary function is performative mobilization, with the 'objective index' aspect being secondary or a means to that end. Resistance (0.60) comes from critics who question its scientific methodology or its alarmist tendencies. The measurement series shows a trend of increasing extractiveness, theatricality, and suppression as the clock's mandate expands to cover more diverse and complex threats, requiring more active management of its narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the agenda-setter and beneficiaries, the clock is a vital tool for necessary mobilization. From the payer seats (scientific community, public discourse), it can be seen as a manipulative or overly simplistic instrument that compromises epistemic integrity for political ends. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of the Atomic Scientists, policy activists, and risk advocacy organizations are beneficiaries, as they gain attention and leverage for their agendas. The scientific community and public discourse are victims, as they bear the costs of potential epistemic compromise and simplified risk narratives. Epistemic critics are excluded, as their calls for more rigor are often sidelined in favor of the clock's impactful messaging.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_impact_vs_epistemic_trust,
    'Does the strategic manipulation of the clock''s setting for policy impact ultimately undermine long-term public trust in scientific institutions and risk assessments?',
    'Longitudinal studies of public trust in science correlated with perceived politicization of scientific bodies and the Doomsday Clock''s messaging.',
    'If trust erodes, the clock''s future efficacy as a mobilization tool diminishes, potentially leading to a Piton classification as its performative function loses its impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_impact_vs_epistemic_trust, empirical, 'Whether strategic framing for policy impact erodes epistemic trust.').

omega_variable(
    mobilization_efficacy_vs_alarmism,
    'To what extent does the clock''s performative function genuinely mobilize effective collective action, versus merely generating alarm or cynicism without concrete policy change?',
    'Policy analysis tracking clock settings against actual policy shifts, public engagement metrics, and behavioral changes, controlling for other influencing factors.',
    'If mobilization is low and alarmism high, the constraint''s coordination function is weaker than claimed, strengthening the extraction component and potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilization_efficacy_vs_alarmism, empirical, 'Assessing the true efficacy of the clock''s mobilization function.').

omega_variable(
    scientific_advocacy_boundary,
    'Where is the legitimate boundary between scientific assessment of risk and advocacy for policy action, and does the Doomsday Clock''s methodology cross it in this reading?',
    'Conceptual analysis and expert consensus on the ethics of science communication and advocacy in high-stakes domains, informed by case studies of similar boundary-crossing instances.',
    'If the boundary is deemed crossed, the clock''s legitimacy as a purely scientific instrument is further eroded, reinforcing its Snare-like qualities for the scientific community and potentially leading to greater resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scientific_advocacy_boundary, conceptual, 'Defining the ethical boundary between science and advocacy in risk communication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1990, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(doom_tr_t1998, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1998, 0.65).
narrative_ontology:measurement(doom_tr_t2007, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2007, 0.75).
narrative_ontology:measurement(doom_tr_t2016, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2016, 0.78).
narrative_ontology:measurement(doom_tr_t2025, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2025, 0.8).

% Extraction over time
narrative_ontology:measurement(doom_be_t1990, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(doom_be_t1998, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(doom_be_t2007, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2007, 0.65).
narrative_ontology:measurement(doom_be_t2016, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2016, 0.7).
narrative_ontology:measurement(doom_be_t2025, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1990, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(doom_su_t1998, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(doom_su_t2007, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement(doom_su_t2016, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(doom_su_t2025, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
