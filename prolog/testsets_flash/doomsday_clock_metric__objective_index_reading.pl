% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint models the Doomsday Clock as an 'objective index' that
 *   tracks measurable existential risk levels through expert synthesis of
 *   empirical indicators. This reading emphasizes the scientific authority
 *   and empirical grounding of the clock, downplaying or suppressing its
 *   performative and normative dimensions. It is a contested reading of the
 *   'doomsday_clock_metric' kernel, where the core disagreement lies in the
 *   extent to which the clock is a purely scientific instrument versus a tool
 *   for advocacy or a hybrid of both.
 *
 * KEY AGENTS:
 *   - scientific_authority: Primary beneficiary (institutional/constrained)
 *   - expert_institutions: Agenda-setter (organized/constrained)
 *   - democratic_accountability: Primary victim (powerless/trapped)
 *   - public_discourse: Secondary victim (moderate/constrained)
 *   - policy_makers: Secondary beneficiary (institutional/mobile)
 *   - critical_epistemologists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.75).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '74715e9c-9468-459a-b835-994ae3bc2525').
narrative_ontology:cs_kernel_codification('74715e9c-9468-459a-b835-994ae3bc2525', formalized).
narrative_ontology:cs_authority_grounding('74715e9c-9468-459a-b835-994ae3bc2525', expertise).
narrative_ontology:cs_interpretation_layer_present('74715e9c-9468-459a-b835-994ae3bc2525').
narrative_ontology:cs_reading_relation('74715e9c-9468-459a-b835-994ae3bc2525', doomsday_clock_metric__performative_tool_reading, forecloses).
narrative_ontology:cs_reading_relation('74715e9c-9468-459a-b835-994ae3bc2525', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_axiom('74715e9c-9468-459a-b835-994ae3bc2525', foundational, existential_risk_is_empirically_quantifiable).
narrative_ontology:cs_axiom_status(existential_risk_is_empirically_quantifiable, holdable).
narrative_ontology:cs_axiom_grounding('74715e9c-9468-459a-b835-994ae3bc2525', existential_risk_is_empirically_quantifiable, empirically_contingent).
narrative_ontology:cs_axiom('74715e9c-9468-459a-b835-994ae3bc2525', foundational, expert_synthesis_yields_objective_assessment).
narrative_ontology:cs_axiom_status(expert_synthesis_yields_objective_assessment, holdable).
narrative_ontology:cs_axiom_grounding('74715e9c-9468-459a-b835-994ae3bc2525', expert_synthesis_yields_objective_assessment, empirically_contingent).
narrative_ontology:cs_reference_frame('74715e9c-9468-459a-b835-994ae3bc2525', pure_scientific_index).
narrative_ontology:cs_drift_state('74715e9c-9468-459a-b835-994ae3bc2525', contemporary_science_communication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74715e9c-9468-459a-b835-994ae3bc2525', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_institutions).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, public_discourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the perception that the Doomsday Clock is a purely objective, empirically-driven assessment, reinforcing its epistemic authority in risk governance. It gains legitimacy and influence by presenting the clock as a scientific index.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_authority, beneficiary,
    institutional, generational, constrained, global).

% The primary actors responsible for setting the clock, synthesizing empirical indicators, and communicating the 'objective' risk level. They control the methodology and interpretation, reinforcing their role as indispensable arbiters of existential risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_institutions, agenda_setter,
    organized, biographical, constrained, global).

% Bears the cost of reduced public participation and oversight in defining and prioritizing existential risks. The 'objective index' framing suppresses open normative debate about risk thresholds and policy responses, centralizing decision-making power with experts.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_accountability, payer,
    powerless, generational, trapped, national).

% Suffers from the narrowing of legitimate discussion around existential risk to purely technical, empirical terms. Normative and ethical considerations, as well as alternative framings of risk, are marginalized, leading to a less robust and inclusive public debate.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, public_discourse, payer,
    moderate, biographical, constrained, global).

% Can leverage the 'objective' authority of the Doomsday Clock to justify policy decisions, particularly those requiring significant public buy-in or resource allocation for risk mitigation. They benefit from a seemingly neutral, scientific backing for their agendas.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers, beneficiary,
    institutional, immediate, mobile, national).

% Analyze the underlying assumptions and epistemic claims of the Doomsday Clock, particularly the tension between its presented objectivity and its inherent normative dimensions. They seek to expose the structural biases and power dynamics embedded in its 'objective index' reading.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, critical_epistemologists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global attention and expert consensus on the most pressing existential risks by providing a single, authoritative metric that synthesizes diverse empirical data into a comprehensible indicator.
% TRANSFER_FUNCTION: Transfers epistemic authority and influence over risk prioritization from broader public and democratic processes to a specialized body of scientific experts, in exchange for a seemingly objective and unified assessment of global threats.
% ABSENT_VOICES: Ethicists, philosophers of technology, and representatives of marginalized communities who might challenge the 'objective' framing of risk and advocate for more inclusive, normatively explicit approaches to risk assessment are largely excluded from the core setting process. They would argue for a more transparent and democratically accountable process for defining and responding to existential threats.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock, as an 'objective index,' vanished, the global discourse on existential risk would lose a central, authoritative (if contested) metric. Expert institutions would need to find new ways to synthesize and communicate risk, and the public would be left without a widely recognized, if flawed, indicator, potentially leading to fragmentation in attention and policy focus.
% FOUNDING_PROBLEM: The problem of communicating complex, interconnected global threats (like nuclear war) to a broad public and policymakers in a way that is both authoritative and actionable, without oversimplifying or sensationalizing.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists, the expert institutions involved, and many policymakers attest that the problem of communicating complex global threats remains live. Critical epistemologists, while challenging the 'objective index' solution, generally agree on the persistence of the underlying communication challenge, corroborating the problem's status from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates global attention on critical risks (a coordination function) but does so through an asymmetric extraction of epistemic authority from democratic processes and public discourse (asymmetric extraction). The 'objective index' framing requires active enforcement to suppress alternative, more normative or performative readings. Extractiveness (0.65) reflects the degree to which the expert interpretation monopolizes the risk narrative. Suppression (0.75) is high due to the active marginalization of non-empirical framings of risk. Theater ratio (0.20) is low, as this reading emphasizes the genuine scientific work involved, even if it serves a broader agenda.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of scientific authority and expert institutions, the clock is a necessary, objective tool for global risk assessment, providing a vital coordination function. From the perspective of democratic accountability and public discourse, it represents an opaque, expert-driven process that extracts the right to define and prioritize risks, suppressing broader normative debate. The engine's per-seat classification should reflect this divergence, with beneficiaries experiencing a Rope-like function and victims experiencing a Snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Scientific authority and expert institutions are beneficiaries (d=0.0-0.2) as they gain legitimacy and influence. Democratic accountability and public discourse are victims (d=0.8-1.0) as their role in risk definition is suppressed. Policymakers are also beneficiaries (d=0.2-0.4) as they can leverage the clock's authority. Critical epistemologists are analytical observers (d=0.5). The 'objective index' framing actively enforces this distribution of authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'objective index' reading prevents mislabeling the clock as a pure Snare by acknowledging its genuine coordination function in synthesizing complex risk data. However, it prevents mislabeling it as a pure Rope by highlighting the active suppression of alternative framings and the extraction of epistemic authority from democratic processes. The mandatrophy is not fully resolved because the founding problem (communicating complex threats) is still live, but the solution (an 'objective index') has accumulated extractive properties over time, shifting its function from pure coordination to a hybrid of coordination and expert-driven extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_empirical_boundary,
    'To what extent can existential risk assessment be purely empirical, and at what point do normative judgments become inseparable from the ''objective'' indicators?',
    'Detailed philosophical and epistemological analysis of the methodology, identifying specific points where expert judgment implicitly incorporates normative weighting or prioritization. Comparison with alternative risk frameworks that explicitly declare their normative foundations.',
    'If normative judgments are found to be deeply embedded and unacknowledged, the ''objective index'' claim collapses, reclassifying the constraint closer to a Snare or Tangled Rope with higher extraction and suppression of democratic input. If a clear empirical core can be isolated, the Rope-like coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_empirical_boundary, conceptual, 'Ambiguity in the boundary between empirical measurement and normative judgment in risk assessment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of normative framing structural (due to the inherent complexity of the science) or internalized (due to a cultural deference to scientific authority)?',
    'Analysis of public and policy responses to attempts at more normatively explicit risk communication. If resistance to normative framing persists even when structural barriers are lowered, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the public carries the suppression with them. If purely structural, interventions to simplify scientific communication might reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for normative framing.').

omega_variable(
    objective_vs_performative_intent,
    'Is the primary intent behind the ''objective index'' framing to accurately reflect risk, or to strategically leverage scientific authority for policy impact?',
    'Analysis of internal communications, historical archives, and expert testimonies regarding the explicit goals of the Doomsday Clock''s communication strategy. Comparison of the ''objective index'' reading''s impact on policy versus its accuracy in predicting outcomes.',
    'If strategic intent is dominant, the constraint shifts closer to the ''performative_tool_reading'' sibling, increasing its theater_ratio and potentially reclassifying it as a Snare or Piton if the coordination function is secondary to the performative extraction of attention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_vs_performative_intent, preference, 'Ambiguity in the primary intent of the ''objective index'' framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(doom_tr_t1960, doomsday_clock_metric__objective_index_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__objective_index_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__objective_index_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__objective_index_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(doom_be_t1960, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.5).
narrative_ontology:measurement(doom_su_t1960, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, global_nuclear_disarmament_treaties).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, climate_change_mitigation_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'doomsday_clock_metric' kernel. This 'objective_index_reading' emphasizes the scientific and empirical aspects, while 'performative_tool_reading' focuses on its strategic communication function, and 'hybrid_legitimacy_reading' acknowledges the entanglement of both. Each reading represents a distinct constraint with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
