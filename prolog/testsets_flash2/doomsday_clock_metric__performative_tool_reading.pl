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
 *   human_readable: Doomsday Clock Metric (Performative Tool Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the Doomsday Clock metric from the
 *   perspective of a 'performative tool,' where its setting is strategically
 *   chosen to maximize policy impact and mobilize collective action. This
 *   reading emphasizes the clock's role as a communication device,
 *   prioritizing its ability to generate urgency and attention over its
 *   strict empirical accuracy. The claimed type is Piton, reflecting a
 *   function that has largely atrophied into performance, where the primary
 *   'extraction' is from epistemic credibility for the benefit of policy
 *   activism.
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
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, piton).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Metric (Performative Tool Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '4fecb792-bc65-49f2-aa05-b3e60c93223b').
narrative_ontology:cs_kernel_codification('4fecb792-bc65-49f2-aa05-b3e60c93223b', formalized).
narrative_ontology:cs_authority_grounding('4fecb792-bc65-49f2-aa05-b3e60c93223b', lineage).
narrative_ontology:cs_interpretation_layer_present('4fecb792-bc65-49f2-aa05-b3e60c93223b').
narrative_ontology:cs_reading_relation('4fecb792-bc65-49f2-aa05-b3e60c93223b', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fecb792-bc65-49f2-aa05-b3e60c93223b', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('4fecb792-bc65-49f2-aa05-b3e60c93223b', foundational, policy_impact_maximization_is_primary).
narrative_ontology:cs_axiom_status(policy_impact_maximization_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('4fecb792-bc65-49f2-aa05-b3e60c93223b', policy_impact_maximization_is_primary, instrumental).
narrative_ontology:cs_axiom('4fecb792-bc65-49f2-aa05-b3e60c93223b', secondary, symbolic_urgency_drives_action).
narrative_ontology:cs_axiom_status(symbolic_urgency_drives_action, holdable).
narrative_ontology:cs_axiom_grounding('4fecb792-bc65-49f2-aa05-b3e60c93223b', symbolic_urgency_drives_action, empirically_contingent).
narrative_ontology:cs_reference_frame('4fecb792-bc65-49f2-aa05-b3e60c93223b', strategic_communication_framework).
narrative_ontology:cs_drift_state('4fecb792-bc65-49f2-aa05-b3e60c93223b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4fecb792-bc65-49f2-aa05-b3e60c93223b', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, public_attention_seekers).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body that sets the Doomsday Clock. Under this reading, they strategically adjust the clock to maximize public and policy impact, prioritizing mobilization over strict empirical indexing. They benefit from the attention and influence this generates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Utilize the clock's pronouncements as a powerful rhetorical tool to galvanize public opinion and pressure policymakers on issues like nuclear disarmament and climate change. They benefit from the amplified urgency the clock provides.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Any individual or group seeking to draw media attention to existential risks. The clock provides a ready-made, high-profile narrative hook for their advocacy efforts, benefiting from its performative impact.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_attention_seekers, beneficiary,
    moderate, immediate, mobile, global).

% The general trustworthiness and reliability of scientific pronouncements, particularly in risk assessment. Under this reading, the strategic manipulation of the clock erodes this credibility, making it harder for future scientific warnings to be taken seriously. It is a non-agent entity that bears a diffuse cost.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_credibility, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).

% A broad group of researchers and experts who rely on public trust in science. They bear the cost of diminished epistemic credibility when the clock is perceived as a political tool rather than an objective index, potentially undermining their own work.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_community, payer,
    organized, generational, constrained, global).

% Experts who prioritize rigorous, empirically-driven risk assessment. They would advocate for a more transparent, data-driven methodology for the clock, but their input is sidelined in favor of maximizing performative impact.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, objective_risk_analysts, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and policy attention around existential risks by providing a simple, memorable, and urgent symbolic metric.
% TRANSFER_FUNCTION: Transfers public attention and political urgency from other issues to existential risks, and transfers epistemic capital from scientific objectivity to policy advocacy.
% ABSENT_VOICES: Objective risk analysts and those who prioritize strict scientific indexing are excluded; they would argue for a more transparent, empirically-grounded methodology, but their concerns are secondary to the goal of mobilization.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock vanished, a significant tool for mobilizing public and policy attention on existential risks would be lost. Activists would need to find new rhetorical devices, and the Bulletin of Atomic Scientists would lose a major source of its influence, leading to a rearrangement of how these issues are communicated and acted upon.
% FOUNDING_PROBLEM: The problem of communicating the existential threat of nuclear weapons to a broad public and galvanizing action during the Cold War.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of Atomic Scientists attests the problem is live, citing ongoing nuclear threats and climate change. Policy activists corroborate this, as they continue to use the clock for mobilization. Objective risk analysts, however, contest the clock's efficacy and epistemic integrity in addressing the problem.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The high theater_ratio (0.80) reflects that the clock's primary function, under this reading, is performative communication rather than objective indexing. Extractiveness (0.65) is measured as the cost to epistemic credibility and the scientific community, which is 'extracted' to fuel policy activism. Suppression (0.20) is low because the constraint doesn't actively coerce; rather, it relies on public engagement and media amplification. Accessibility collapse (0.30) is low as alternative risk communication methods exist, but the clock's prominence makes them less visible. Resistance (0.10) is low, as direct resistance to the clock's existence is minimal, though its methodology is debated. The increasing extractiveness and theater ratio over time reflect a growing emphasis on its performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policy activists, the clock is a highly effective Rope, a vital tool for coordination and mobilization. From the perspective of the scientific community, it operates more like a Snare, extracting from their collective credibility for a purpose they may not fully endorse. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of Atomic Scientists, policy activists, and public attention seekers are beneficiaries, as the clock serves their goals of influence and mobilization. Epistemic credibility and the broader scientific community are victims, as the strategic use of the clock erodes trust in scientific pronouncements. Objective risk analysts are excluded, as their preferred methodology is not prioritized.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a Piton because its original mandate (objective warning) has largely atrophied, replaced by a performative function. No single party benefits enough to maintain it as a strictly objective index, and no party is hurt enough to force a fundamental change in its strategic use, leading to its persistence through institutional inertia and theatrical maintenance. The 'extraction' is diffuse (from epistemic credibility) and the 'beneficiaries' are diffuse (policy activism, public attention), fitting the Piton profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_vs_performative_function,
    'Is the Doomsday Clock primarily an objective index of risk or a performative tool for policy advocacy?',
    'Analysis of the Bulletin''s internal deliberations and public statements over time, comparing stated methodology with actual clock adjustments and their correlation with policy outcomes versus empirical risk changes.',
    'If primarily objective, the constraint would reclassify towards Rope or even Mountain (if truly reflecting natural limits). If primarily performative, the Piton classification is reinforced, highlighting the erosion of its original mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(objective_vs_performative_function, conceptual, 'Ambiguity in the clock''s core function: objective measurement versus strategic communication.').

omega_variable(
    epistemic_cost_quantification,
    'How precisely can the ''extraction'' from epistemic credibility be quantified and attributed to the clock''s performative use?',
    'Longitudinal studies of public trust in science, media analysis of how the clock is framed, and surveys of scientific community perceptions regarding the clock''s impact on their credibility.',
    'A clear quantification of significant epistemic cost would strengthen the Snare-like aspects of the Piton classification. If the cost is negligible, the constraint might lean more towards a benign Rope, albeit a theatrical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_cost_quantification, empirical, 'Measuring the impact of performative communication on scientific credibility.').

omega_variable(
    kernel_reading_difference,
    'What is the precise structural difference between this ''performative_tool_reading'' and the ''objective_index_reading'' of the Doomsday Clock kernel?',
    'Compare the declared axioms and reference frames of both readings. The ''performative_tool_reading'' prioritizes impact and mobilization, while the ''objective_index_reading'' prioritizes empirical accuracy and transparency.',
    'If the structural differences are minor, the kernel might be a single constraint with different observer perspectives. If the differences are foundational, it confirms the need for separate constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Distinguishing the core premises of different readings of the Doomsday Clock kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(doom_tr_t10, doomsday_clock_metric__performative_tool_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__performative_tool_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__performative_tool_reading, theater_ratio, 30, 0.72).
narrative_ontology:measurement(doom_tr_t40, doomsday_clock_metric__performative_tool_reading, theater_ratio, 40, 0.76).
narrative_ontology:measurement(doom_tr_t50, doomsday_clock_metric__performative_tool_reading, theater_ratio, 50, 0.78).
narrative_ontology:measurement(doom_tr_t60, doomsday_clock_metric__performative_tool_reading, theater_ratio, 60, 0.79).
narrative_ontology:measurement(doom_tr_t70, doomsday_clock_metric__performative_tool_reading, theater_ratio, 70, 0.8).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(doom_be_t10, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(doom_be_t40, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(doom_be_t50, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(doom_be_t60, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(doom_be_t70, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(doom_su_t10, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 30, 0.17).
narrative_ontology:measurement(doom_su_t40, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(doom_su_t50, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 50, 0.19).
narrative_ontology:measurement(doom_su_t60, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(doom_su_t70, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 70, 0.2).


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
