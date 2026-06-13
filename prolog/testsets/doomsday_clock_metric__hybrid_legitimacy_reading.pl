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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock: Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint describes the Doomsday Clock from the 'hybrid legitimacy'
 *   perspective, where its authority derives from an irreducible entanglement
 *   of scientific judgment and normative advocacy. It is neither a purely
 *   objective index nor a mere performative tool, but intentionally occupies
 *   a liminal space. This reading highlights the coordination function of
 *   drawing attention to existential risks, but also the 'extraction' of
 *   clarity and accountability from public discourse and policy, as the basis
 *   for its pronouncements remains deliberately ambiguous. The claimed type
 *   is Tangled Rope because it genuinely coordinates attention
 *   (beneficiaries) but does so through an opaque mechanism that extracts
 *   clarity (victims) and requires active maintenance of its ambiguous
 *   status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.4).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.2).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock: Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '752b6dc4-e9c9-49bb-92be-948bf72e5d09').
narrative_ontology:cs_kernel_codification('752b6dc4-e9c9-49bb-92be-948bf72e5d09', formalized).
narrative_ontology:cs_authority_grounding('752b6dc4-e9c9-49bb-92be-948bf72e5d09', lineage).
narrative_ontology:cs_interpretation_layer_present('752b6dc4-e9c9-49bb-92be-948bf72e5d09').
narrative_ontology:cs_reading_relation('752b6dc4-e9c9-49bb-92be-948bf72e5d09', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('752b6dc4-e9c9-49bb-92be-948bf72e5d09', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('752b6dc4-e9c9-49bb-92be-948bf72e5d09', foundational, risk_communication_requires_hybridity).
narrative_ontology:cs_axiom_status(risk_communication_requires_hybridity, holdable).
narrative_ontology:cs_axiom_grounding('752b6dc4-e9c9-49bb-92be-948bf72e5d09', risk_communication_requires_hybridity, conventional).
narrative_ontology:cs_axiom('752b6dc4-e9c9-49bb-92be-948bf72e5d09', foundational, scientific_judgment_is_normatively_laden).
narrative_ontology:cs_axiom_status(scientific_judgment_is_normatively_laden, holdable).
narrative_ontology:cs_axiom_grounding('752b6dc4-e9c9-49bb-92be-948bf72e5d09', scientific_judgment_is_normatively_laden, deontological).
narrative_ontology:cs_reference_frame('752b6dc4-e9c9-49bb-92be-948bf72e5d09', entangled_judgment_and_advocacy).
narrative_ontology:cs_drift_state('752b6dc4-e9c9-49bb-92be-948bf72e5d09', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('752b6dc4-e9c9-49bb-92be-948bf72e5d09', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_the_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, public_discourse).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body that sets and maintains the Doomsday Clock. They benefit from the visibility and authority the Clock provides, but are constrained by the need to maintain credibility across scientific and policy communities. They actively enforce the hybrid framing, resisting attempts to reduce the Clock to a purely scientific index or a mere advocacy tool.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_the_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the Clock's ability to draw attention and funding to existential risk topics. They leverage the Clock's pronouncements to frame their research as urgent and relevant, without directly controlling its setting. Their benefit is primarily in agenda-setting and public awareness.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_researchers, beneficiary,
    organized, biographical, mobile, global).

% Bears the cost of ambiguity: the public struggles to interpret the Clock's meaning, oscillating between treating it as a scientific forecast and a call to action. This can lead to confusion, alarm fatigue, or misdirected efforts, as the underlying basis for the 'threat level' is never fully transparent.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, public_discourse, payer,
    powerless, immediate, trapped, global).

% Are pressured to respond to the Clock's warnings, but lack clear, actionable metrics or scientific consensus on which to base policy. They pay the cost of having to engage with a metric that is intentionally vague about its scientific grounding and normative implications, making it difficult to translate into concrete governance strategies.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers, payer,
    powerful, biographical, constrained, national).

% Many scientists are uncomfortable with the Clock's methodology, which blends empirical assessment with normative judgment, making it difficult to integrate into standard scientific discourse or peer review. They are excluded from the Clock's setting process and often find its hybrid nature problematic for scientific credibility.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_community, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global attention on existential risks by providing a single, widely recognized symbolic metric, fostering a shared (if ambiguous) understanding of urgency across diverse audiences.
% TRANSFER_FUNCTION: Transfers attention, urgency, and a sense of shared threat from the Bulletin's expert panel to global public discourse and policy agendas, without a clear, measurable transfer of resources or accountability.
% ABSENT_VOICES: The broader scientific community, particularly those focused on rigorous empirical risk assessment, are largely absent from the Clock's setting process. They would argue for greater transparency in methodology and a clearer distinction between scientific judgment and normative advocacy.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock vanished, the global conversation around existential risk would lose a central, iconic symbol. While other risk assessments exist, none hold the same cultural resonance or ability to instantly communicate a perceived threat level, leading to a fragmentation of public attention and a loss of a shared reference point for urgency.
% FOUNDING_PROBLEM: The problem of communicating the existential threat of nuclear war to a global public and galvanizing action, given the abstract and long-term nature of the risk.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists asserts the problem is live, citing ongoing nuclear proliferation and new threats like climate change and AI. Independent risk analysts and historians corroborate that the challenge of communicating complex, long-term, low-probability/high-impact risks remains a persistent problem, even if the specific threats have evolved.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).

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
 *   Extractiveness (0.4) is moderate: the Clock extracts clarity and accountability from public discourse, but its primary function is not direct resource transfer. Suppression (0.2) is low: there's no direct coercion, but the ambiguity itself suppresses clear, metric-driven debate. Theater ratio (0.6) is high: a significant portion of the Clock's operation involves maintaining its symbolic power and ambiguous status, rather than purely scientific assessment. The Clock's persistence relies on actively managing its hybrid identity, which requires enforcement against attempts to reduce it to a single, transparent function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bulletin, the hybrid nature is a necessary and effective way to communicate complex risks. From the perspective of the public and policy makers, it's a source of confusion and an accountability void. The engine should compute a coordination benefit for the agenda-setters and researchers, but an extractive cost for those trying to make sense of or act on the Clock's pronouncements.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of the Atomic Scientists (agenda_setter) and existential risk researchers (beneficiary) benefit from the Clock's ability to set agendas and draw attention. Public discourse and policy makers (victims/payers) bear the cost of ambiguity and lack of actionable metrics. The scientific community is largely excluded, as the Clock's hybrid nature makes it difficult to engage with on purely scientific terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling by acknowledging the genuine coordination function (drawing attention to risk) while also identifying the costs imposed by its deliberate ambiguity. It's not a pure Snare because there's a real, if imperfect, coordination benefit. It's not a pure Rope because the ambiguity itself creates an accountability void that extracts clarity from public and policy discourse. The 'mandate' to communicate risk is live, but the 'trophy' is the continued institutional authority derived from maintaining the ambiguity, rather than resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_vs_normative_weighting,
    'What is the precise weighting of scientific judgment versus normative stakes in the Clock''s setting process, and how does this weighting change over time?',
    'Internal documentation from the Bulletin, expert interviews, and content analysis of Clock statements over time, focusing on explicit justifications for changes.',
    'If the weighting is predominantly normative, the Clock''s scientific legitimacy is further eroded, pushing it closer to a pure performative tool. If a clear, consistent scientific methodology can be discerned, it would strengthen claims of objectivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_vs_normative_weighting, empirical, 'Ambiguity in the balance between scientific and normative inputs.').

omega_variable(
    accountability_void_vs_coordination_benefit,
    'Does the ambiguity inherent in the hybrid legitimacy reading primarily serve to coordinate attention on risk, or does it primarily create an accountability void for those who set the Clock?',
    'Longitudinal studies of public and policy response to Clock changes, assessing whether it leads to concrete, effective action or merely generates transient alarm without clear direction.',
    'If the accountability void is dominant, the constraint leans more towards a Snare, as the ''coordination'' is a cover for maintaining institutional authority without clear responsibility. If coordination is dominant, it leans more towards a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_void_vs_coordination_benefit, empirical, 'Whether ambiguity is a feature or a bug for risk communication.').

omega_variable(
    framing_under_determination_doomsday_clock,
    'Is the ''hybrid legitimacy'' framing the most defensible interpretation of the Doomsday Clock, or would an ''objective index'' or ''performative tool'' framing better capture its structural dynamics?',
    'Comparative analysis of the Clock''s historical impact and public reception under each framing, assessing which best explains observed outcomes and institutional behavior.',
    'Adopting an ''objective index'' framing would reclassify the Clock as a contested Mountain or Tangled Rope (if the metrics are found to be flawed). Adopting a ''performative tool'' framing would reclassify it as a Snare or Piton, depending on the degree of extraction and theatricality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_doomsday_clock, conceptual, 'Alternative framings of the Doomsday Clock''s core function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.4).
narrative_ontology:measurement(doom_tr_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.3).
narrative_ontology:measurement(doom_be_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(doom_su_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, global_nuclear_disarmament_treaties).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, climate_change_mitigation_targets).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, ai_safety_governance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'doomsday_clock_metric' kernel, alongside 'objective_index_reading' and 'performative_tool_reading'. Each reading represents a distinct structural claim about the Clock's function and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
