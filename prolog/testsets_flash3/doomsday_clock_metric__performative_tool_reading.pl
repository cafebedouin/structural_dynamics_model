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
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint story models the Doomsday Clock's setting as a
 *   performative tool, strategically manipulated to maximize policy impact
 *   and mobilize collective action. It is one reading of the
 *   'doomsday_clock_metric' kernel, where the clock's primary function is
 *   seen as advocacy rather than objective measurement. The high theater
 *   ratio reflects the emphasis on symbolic action and public messaging over
 *   strict empirical fidelity. The claimed type is 'tangled_rope' because it
 *   genuinely coordinates attention on critical issues (a public good) but
 *   does so by extracting from epistemic credibility and public trust in
 *   science.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.65).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.4).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '9d580234-4374-49e5-8a5f-9509a4d2af19').
narrative_ontology:cs_kernel_codification('9d580234-4374-49e5-8a5f-9509a4d2af19', formalized).
narrative_ontology:cs_authority_grounding('9d580234-4374-49e5-8a5f-9509a4d2af19', extraction).
narrative_ontology:cs_interpretation_layer_present('9d580234-4374-49e5-8a5f-9509a4d2af19').
narrative_ontology:cs_reading_relation('9d580234-4374-49e5-8a5f-9509a4d2af19', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d580234-4374-49e5-8a5f-9509a4d2af19', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('9d580234-4374-49e5-8a5f-9509a4d2af19', foundational, policy_impact_trumps_epistemic_purity).
narrative_ontology:cs_axiom_status(policy_impact_trumps_epistemic_purity, holdable).
narrative_ontology:cs_axiom_grounding('9d580234-4374-49e5-8a5f-9509a4d2af19', policy_impact_trumps_epistemic_purity, instrumental).
narrative_ontology:cs_axiom('9d580234-4374-49e5-8a5f-9509a4d2af19', foundational, symbolic_urgency_mobilizes_action).
narrative_ontology:cs_axiom_status(symbolic_urgency_mobilizes_action, holdable).
narrative_ontology:cs_axiom_grounding('9d580234-4374-49e5-8a5f-9509a4d2af19', symbolic_urgency_mobilizes_action, empirically_contingent).
narrative_ontology:cs_reference_frame('9d580234-4374-49e5-8a5f-9509a4d2af19', cold_war_mobilization_paradigm).
narrative_ontology:cs_drift_state('9d580234-4374-49e5-8a5f-9509a4d2af19', contemporary_multi_threat_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9d580234-4374-49e5-8a5f-9509a4d2af19', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, risk_advocacy_organizations).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_trust_in_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organization that sets and publicizes the Doomsday Clock. From this reading, their primary goal is to influence policy and public opinion, using the clock's setting as a strategic lever rather than a purely objective measure. They benefit from the attention and policy impact generated.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Utilize the clock's setting as a powerful rhetorical device to mobilize support for their causes (e.g., nuclear disarmament, climate action). They benefit from the heightened sense of urgency and media attention the clock generates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Similar to policy activists, these organizations leverage the clock's pronouncements to raise awareness and secure funding for their work on existential risks. They benefit from the amplified public discourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, risk_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% The perceived trustworthiness and reliability of scientific pronouncements. In this reading, strategic manipulation of the clock's setting, even for good intentions, erodes the long-term credibility of scientific institutions and expert judgment, making future warnings less effective.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_credibility, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).

% The general public's confidence in scientific institutions and their ability to provide objective assessments. When the clock is perceived as a political tool, this trust is diminished, making the public more susceptible to misinformation and less likely to heed genuine scientific warnings.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_trust_in_science, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, public_trust_in_science).

% Observes the clock's setting and its impact. Some members may endorse its performative utility, while others express concern about its impact on scientific objectivity and public perception of science.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective attention and action on global catastrophic risks by providing a simple, evocative metric that signals urgency and mobilizes stakeholders towards policy change.
% TRANSFER_FUNCTION: Transfers public attention and political will towards specific policy agendas (e.g., nuclear disarmament, climate action) by strategically framing the severity of existential threats.
% ABSENT_VOICES: Those who prioritize strict epistemic objectivity in science communication are implicitly excluded from the clock-setting process, as their concerns about methodological purity are subordinated to the goal of policy impact. They would argue for a more transparent, empirically grounded metric.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock vanished, a significant rhetorical tool for policy activists and risk advocacy organizations would disappear. While other metrics exist, the clock's unique symbolic power and media resonance would be lost, requiring these groups to find new ways to galvanize public and political attention on existential risks.
% FOUNDING_PROBLEM: The problem of communicating the existential threat of nuclear war to a broad public and mobilizing political action during the Cold War.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists attests the problem is live, now expanded to include climate change and other global threats. Policy activists and risk advocacy organizations corroborate that the challenge of mobilizing action on these diffuse, long-term threats remains acute, making the clock's performative function still relevant.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because the strategic setting of the clock, while aiming for positive policy outcomes, 'extracts' from the long-term epistemic health of science by blurring the lines between advocacy and objective assessment. Suppression (0.40) is moderate; while there isn't direct coercion, alternative, purely objective risk metrics are implicitly suppressed in public discourse by the clock's dominant narrative. The theater ratio (0.70) is high, indicating that a significant portion of the clock's activity is performative, designed for public impact rather than precise scientific indexing. The trend shows increasing extractiveness and theatricality over time as the clock's role has broadened beyond its initial nuclear focus.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policy activists, the clock is a vital tool for necessary coordination and mobilization. From the perspective of those concerned with epistemic integrity, it represents a problematic extraction from scientific objectivity. The engine's classification will highlight this divergence, showing a beneficial 'rope' for activists and an extractive 'snare' for epistemic values.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of the Atomic Scientists, policy activists, and risk advocacy organizations are beneficiaries, gaining influence and attention. Epistemic credibility and public trust in science are the victims, bearing the cost of strategic communication. The scientific community acts as an observer, with some members benefiting from the public engagement while others are concerned about the methodological implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_cost_benefit_tradeoff,
    'Does the policy impact generated by the performative use of the Doomsday Clock outweigh the long-term erosion of epistemic credibility and public trust in science?',
    'Longitudinal studies tracking public perception of scientific authority in relation to high-profile advocacy-driven scientific communications, alongside counterfactual analysis of policy outcomes without such tools.',
    'If the epistemic costs are found to outweigh the benefits, the constraint''s extractiveness would be re-evaluated as even higher, potentially shifting its classification towards a pure snare. If benefits are clear, the extraction might be seen as a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_cost_benefit_tradeoff, preference, 'Assessing the normative trade-off between policy impact and epistemic integrity.').

omega_variable(
    alternative_communication_strategies,
    'Are there alternative science communication strategies that could achieve similar policy impact without compromising epistemic credibility?',
    'Experimental trials of different communication frameworks for existential risks, comparing their effectiveness in mobilizing action against their impact on public trust and scientific perception.',
    'If effective alternatives exist, the ''suppression'' metric for this constraint would be re-evaluated as higher, as the current method actively suppresses more epistemically sound approaches. This would further emphasize the extractive nature of the performative reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_communication_strategies, empirical, 'Exploring less epistemically costly ways to achieve policy impact.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the Doomsday Clock fundamentally an objective index, a performative tool, or an irreducible hybrid of both?',
    'Conceptual analysis and expert consensus on the inherent nature of ''boundary objects'' in science communication, and the extent to which they can be disentangled into purely epistemic or purely performative functions.',
    'If resolved towards a purely objective index, this ''performative_tool_reading'' would be foreclosed, and the ''objective_index_reading'' would gain dominance. If resolved as an irreducible hybrid, the ''hybrid_legitimacy_reading'' would be validated, and the structural tension between epistemic and performative goals would be explicitly recognized as inherent to the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the fundamental nature and purpose of the Doomsday Clock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(doom_tr_t15, doomsday_clock_metric__performative_tool_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__performative_tool_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(doom_tr_t45, doomsday_clock_metric__performative_tool_reading, theater_ratio, 45, 0.68).
narrative_ontology:measurement(doom_tr_t60, doomsday_clock_metric__performative_tool_reading, theater_ratio, 60, 0.69).
narrative_ontology:measurement(doom_tr_t75, doomsday_clock_metric__performative_tool_reading, theater_ratio, 75, 0.7).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(doom_be_t15, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(doom_be_t45, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(doom_be_t60, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(doom_be_t75, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 75, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(doom_su_t15, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(doom_su_t45, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement(doom_su_t60, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(doom_su_t75, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 75, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, global_nuclear_disarmament_treaties).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, international_climate_agreements).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
