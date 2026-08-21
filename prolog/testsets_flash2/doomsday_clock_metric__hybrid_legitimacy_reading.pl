% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Metric: Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint describes the Doomsday Clock metric as embodying a hybrid
 *   legitimacy, blending scientific judgment with normative stakes in
 *   existential risk. It is a reading that acknowledges the irreducible
 *   entanglement of these two domains, where the clock's authority derives
 *   precisely from its ability to operate in this ambiguous space, rather
 *   than being a purely objective index or a mere performative tool. The
 *   constraint is claimed as a Rope because its primary function is
 *   coordination of attention and discourse, with relatively low extraction
 *   and suppression, but its hybrid nature introduces a degree of 'theater'
 *   as it navigates scientific and normative demands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.3).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.1).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Metric: Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '123c0c15-32c5-4017-be3a-0b55a563143b').
narrative_ontology:cs_kernel_codification('123c0c15-32c5-4017-be3a-0b55a563143b', formalized).
narrative_ontology:cs_authority_grounding('123c0c15-32c5-4017-be3a-0b55a563143b', lineage).
narrative_ontology:cs_interpretation_layer_present('123c0c15-32c5-4017-be3a-0b55a563143b').
narrative_ontology:cs_reading_relation('123c0c15-32c5-4017-be3a-0b55a563143b', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('123c0c15-32c5-4017-be3a-0b55a563143b', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('123c0c15-32c5-4017-be3a-0b55a563143b', foundational, risk_assessment_is_socio_technical).
narrative_ontology:cs_axiom_status(risk_assessment_is_socio_technical, holdable).
narrative_ontology:cs_axiom_grounding('123c0c15-32c5-4017-be3a-0b55a563143b', risk_assessment_is_socio_technical, empirically_contingent).
narrative_ontology:cs_axiom('123c0c15-32c5-4017-be3a-0b55a563143b', foundational, symbolic_communication_is_essential_for_x_risk).
narrative_ontology:cs_axiom_status(symbolic_communication_is_essential_for_x_risk, holdable).
narrative_ontology:cs_axiom_grounding('123c0c15-32c5-4017-be3a-0b55a563143b', symbolic_communication_is_essential_for_x_risk, instrumental).
narrative_ontology:cs_reference_frame('123c0c15-32c5-4017-be3a-0b55a563143b', integrated_scientific_normative_assessment).
narrative_ontology:cs_drift_state('123c0c15-32c5-4017-be3a-0b55a563143b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('123c0c15-32c5-4017-be3a-0b55a563143b', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, public_discourse_on_x_risk).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, risk_modelers).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, interdisciplinary_risk_assessment).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, public_engagement_in_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body responsible for setting and communicating the Doomsday Clock. They navigate the tension between scientific credibility and public impact, deliberately maintaining ambiguity about the clock's precise methodology to preserve its influence.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from a focal point for discussion on existential risks, even if the metric's exact nature is ambiguous. The clock provides a shared, high-profile reference for media and public engagement.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, public_discourse_on_x_risk, beneficiary,
    organized, biographical, mobile, global).

% Utilize the clock's pronouncements as a rhetorical tool to highlight urgency or justify policy initiatives related to nuclear disarmament, climate change, or other global threats. They benefit from its perceived authority without needing to scrutinize its precise scientific basis.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers, beneficiary,
    powerful, immediate, mobile, national).

% Observes the clock's setting with a mix of appreciation for its public awareness function and skepticism regarding its scientific rigor. Some members may critique its methodology, while others support its role in science communication.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_community, observer,
    institutional, generational, analytical, global).

% Bear the cost of the clock's ambiguity when attempting to integrate its pronouncements into more rigorous quantitative risk assessments. They struggle to reconcile its qualitative, hybrid nature with demands for precise, empirically grounded metrics.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, risk_modelers, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally recognized, high-profile symbolic metric that coordinates public and policy attention on existential risks, bridging scientific assessment with normative urgency.
% TRANSFER_FUNCTION: Transfers a sense of urgency and a focal point for discussion from the Bulletin's expert panel to global public discourse and policy agendas, without a direct financial transfer.
% ABSENT_VOICES: Purely quantitative risk modelers who demand a transparent, empirically falsifiable metric are often marginalized in the public discourse surrounding the clock, as their critiques can undermine its hybrid legitimacy.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock vanished, the global public discourse on existential risks would lose a powerful, widely recognized symbol and a consistent, high-profile platform for annual updates. While other risk assessments exist, none currently command the same level of public and media attention, leading to a rearrangement of how these risks are communicated and perceived.
% FOUNDING_PROBLEM: To communicate the immediate and existential danger of nuclear war to a global public and policy makers, translating complex scientific and geopolitical realities into an understandable, urgent symbol.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of Atomic Scientists, along with many international relations experts and climate scientists, attests that the founding problem of communicating existential risk (now expanded beyond nuclear war) remains critically live. Independent analyses of public awareness and policy engagement confirm the clock's ongoing role in this communication, even if its scientific precision is debated.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).
:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) because the 'cost' is primarily the ambiguity and lack of precise scientific accountability for those seeking it, rather than direct financial extraction. Suppression is low (0.1) as there's no active coercion to accept the clock's pronouncements, though its prominence can overshadow alternative metrics. Theater ratio is moderate (0.4) reflecting the deliberate performative aspect of its communication, which is essential to its function but not purely 'scientific'. Accessibility collapse is high (0.7) because few other mechanisms achieve the same global reach and symbolic power for existential risk communication. Resistance is low (0.2) because while some critique its methodology, few actively resist its existence or general message.
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin, public discourse, and policy makers largely experience this as a beneficial coordination mechanism. Risk modelers, however, experience the ambiguity as a 'cost' or 'extraction' in terms of intellectual rigor and the challenge of integrating it into their work. The engine should compute a more extractive classification for the risk modelers due to this structural friction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of Atomic Scientists, as the agenda-setter, benefits from the clock's influence (low d). Public discourse and policy makers are beneficiaries, gaining a focal point for attention and rhetoric. Risk modelers are payers, bearing the cost of methodological ambiguity when trying to integrate the clock's qualitative nature into quantitative frameworks. The scientific community acts as an observer, with varied perspectives on its utility.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid legitimacy reading prevents mislabeling the clock as pure extraction (Snare) by acknowledging its genuine coordination function in public discourse, even if its scientific rigor is debated. It also prevents mislabeling it as a pure Mountain by recognizing the active choices and normative judgments involved in its setting, rather than treating it as a purely objective, natural fact. The 'theater' is part of its function, not solely a sign of decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_vs_normative_weighting,
    'What is the precise weighting of scientific judgment versus normative stakes in the clock''s setting process, and how is this weighting justified?',
    'Internal documentation from the Bulletin detailing the decision-making process, or a formal external audit of the factors influencing clock adjustments.',
    'Greater transparency could either solidify its hybrid legitimacy by showing a reasoned balance, or expose an imbalance that shifts its classification towards a more performative (Snare) or purely scientific (Rope) reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_vs_normative_weighting, empirical, 'Ambiguity in the balance between scientific and normative inputs.').

omega_variable(
    accountability_for_ambiguity,
    'Does the deliberate ambiguity in the clock''s legitimacy create an accountability void, where neither scientific nor normative standards can fully hold it to account?',
    'Analysis of public and expert critiques, and the Bulletin''s responses, to determine if specific challenges to the clock''s methodology or impact are effectively addressed or deflected by its hybrid nature.',
    'If an accountability void is confirmed, the constraint''s effective extractiveness (from those seeking clear accountability) would be higher, potentially shifting its classification towards a Tangled Rope or Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_for_ambiguity, conceptual, 'Whether hybrid legitimacy leads to an accountability void.').

omega_variable(
    framing_under_determination,
    'Is the ''hybrid legitimacy'' framing the only defensible way to understand the Doomsday Clock, or would an ''objective index'' or ''performative tool'' framing be equally coherent?',
    'A meta-analysis of scholarly interpretations and public reception of the clock, assessing which framing best explains its persistence and impact across different contexts.',
    'If an alternative framing (e.g., ''objective index'') were adopted, the constraint would be reclassified as a Mountain (if truly objective) or a Snare (if claiming objectivity but being extractive). If a ''performative tool'' framing were adopted, it would likely be a Tangled Rope or Snare, depending on the beneficiaries of the performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative coherent framings of the Doomsday Clock''s nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(doom_tr_t15, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(doom_tr_t45, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(doom_tr_t60, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(doom_tr_t75, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(doom_be_t15, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(doom_be_t45, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 45, 0.28).
narrative_ontology:measurement(doom_be_t60, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(doom_be_t75, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 75, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(doom_su_t15, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(doom_su_t45, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 45, 0.09).
narrative_ontology:measurement(doom_su_t60, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(doom_su_t75, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 75, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, global_nuclear_disarmament_treaties).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, climate_change_policy_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
