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
 *   human_readable: Doomsday Clock: Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint describes the Doomsday Clock as a mechanism that derives
 *   its legitimacy from a deliberate entanglement of scientific judgment and
 *   normative stakes. It is not a purely objective index, nor is it merely a
 *   performative tool; its power comes from the ambiguity of its hybrid
 *   nature. This reading acknowledges the clock's coordination function in
 *   raising awareness but also its potential for diffuse extraction through
 *   anxiety and a lack of clear accountability. The claimed type is 'piton'
 *   because its primary function (clear, actionable risk assessment) has
 *   atrophied, but it persists due to institutional inertia and its
 *   performative role in public discourse, extracting diffuse attention and
 *   anxiety without concentrated benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.35).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.15).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, piton).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock: Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, 'eb1d3428-9dec-4614-8192-16d4f73d9a42').
narrative_ontology:cs_kernel_codification('eb1d3428-9dec-4614-8192-16d4f73d9a42', implicit).
narrative_ontology:cs_authority_grounding('eb1d3428-9dec-4614-8192-16d4f73d9a42', lineage).
narrative_ontology:cs_interpretation_layer_present('eb1d3428-9dec-4614-8192-16d4f73d9a42').
narrative_ontology:cs_reading_relation('eb1d3428-9dec-4614-8192-16d4f73d9a42', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb1d3428-9dec-4614-8192-16d4f73d9a42', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('eb1d3428-9dec-4614-8192-16d4f73d9a42', foundational, risk_assessment_is_inherently_normative).
narrative_ontology:cs_axiom_status(risk_assessment_is_inherently_normative, holdable).
narrative_ontology:cs_axiom_grounding('eb1d3428-9dec-4614-8192-16d4f73d9a42', risk_assessment_is_inherently_normative, deontological).
narrative_ontology:cs_axiom('eb1d3428-9dec-4614-8192-16d4f73d9a42', foundational, public_engagement_requires_symbolic_synthesis).
narrative_ontology:cs_axiom_status(public_engagement_requires_symbolic_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('eb1d3428-9dec-4614-8192-16d4f73d9a42', public_engagement_requires_symbolic_synthesis, instrumental).
narrative_ontology:cs_reference_frame('eb1d3428-9dec-4614-8192-16d4f73d9a42', post_wwii_scientist_activism).
narrative_ontology:cs_drift_state('eb1d3428-9dec-4614-8192-16d4f73d9a42', contemporary_multi_risk_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eb1d3428-9dec-4614-8192-16d4f73d9a42', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_the_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, risk_governance_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, general_public).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, interdisciplinary_risk_assessment).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, public_engagement_in_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organization that sets and maintains the Doomsday Clock. They frame the clock as a blend of scientific assessment and a call to action, deliberately maintaining ambiguity about its precise metric. They benefit from the attention and influence the clock generates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_the_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Academics who study existential risk and its governance. They use the clock as a focal point for discussion and research, benefiting from its ability to draw public and policy attention to their field. They appreciate the nuanced, hybrid framing.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, risk_governance_scholars, beneficiary,
    organized, biographical, mobile, global).

% Government officials who must respond to public and expert concerns about existential risks. They bear the cost of needing to interpret and potentially act on the clock's pronouncements, often struggling with its lack of clear, actionable metrics. They are pressured to acknowledge its legitimacy without clear guidance.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers, payer,
    powerful, immediate, constrained, national).

% Receives the clock's message as a warning about global catastrophe. They bear the psychological cost of anxiety and the cognitive cost of trying to understand a metric that is intentionally ambiguous. Their ability to act is limited, making them passive recipients of the message.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, general_public, payer,
    powerless, biographical, trapped, global).

% Scientists and communicators who advocate for purely empirical, quantifiable metrics in risk assessment. They are excluded from the clock's setting process and would argue for a more transparent, data-driven methodology, finding the hybrid approach unscientific.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, hard_science_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public and policy attention on complex, long-term existential risks by providing a simple, symbolic indicator that blends scientific assessment with normative urgency.
% TRANSFER_FUNCTION: Transfers attention and a sense of urgency from the general public and policymakers towards the Bulletin of the Atomic Scientists and the issues they highlight, without a clear, quantifiable metric for accountability.
% ABSENT_VOICES: Hard science advocates and those demanding clear, falsifiable metrics are excluded; they would argue for a purely objective index, finding the hybrid approach to be an obfuscation of scientific rigor.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock vanished, a significant focal point for global existential risk discourse would disappear. While the underlying risks would remain, the mechanism for coordinating public and policy attention, however imperfect, would be lost, requiring new communication strategies to emerge.
% FOUNDING_PROBLEM: After World War II, scientists recognized the need to communicate the unprecedented existential threat of nuclear weapons to a global audience, blending scientific expertise with a moral imperative for action.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists attests the problem is live, citing ongoing nuclear threats and new risks like climate change and AI. Risk governance scholars and public opinion surveys corroborate the continued need for public engagement on these issues, even if the clock's method is debated.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.35) because the clock's ambiguity makes it difficult to hold anyone accountable for its pronouncements, leading to a diffuse cost of anxiety and unaddressed risk. Suppression is low (0.15) as there's no direct coercion, but the symbolic weight of the clock can subtly suppress alternative, more precise risk communication. Theater ratio is high (0.6) because a significant portion of its activity is performative, aimed at public engagement and symbolic warning, rather than precise, actionable scientific indexing. The clock's persistence is more about its role as a cultural artifact and a call to action than its direct utility as a scientific metric.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bulletin, the hybrid approach is a necessary and effective way to communicate complex risks. From the perspective of hard science advocates, it's an unscientific obfuscation. Policymakers experience it as a source of pressure without clear actionable intelligence. The engine's classification will highlight how this 'piton' operates differently across these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin of the Atomic Scientists and risk governance scholars are beneficiaries, gaining attention and legitimacy from the clock's operation. Policymakers and the general public are diffuse payers, bearing the costs of interpretation, anxiety, and the pressure to act without clear guidance. Hard science advocates are excluded, as their preferred mode of communication is incompatible with the clock's hybrid nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The clock's original mandate was to warn about nuclear war. While it has expanded to include other existential risks, its function as a precise, actionable scientific index has atrophied. It persists as a 'piton' because its symbolic power and ability to generate public discourse still serve a purpose for its administrators and beneficiaries, even if its direct utility as a scientific metric is low. The ambiguity of its hybrid legitimacy allows it to continue operating without clear accountability for its 'metric's' accuracy or impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_vs_normative_weighting,
    'What is the precise weighting of scientific judgment versus normative stakes in the clock''s setting process, and how does this weighting change over time?',
    'Detailed internal documentation of the Bulletin''s decision-making process, including transcripts of discussions and explicit criteria for adjustments, or a formal external audit of their methodology.',
    'If the normative weighting is found to be dominant, it would further support the ''performative_tool_reading'' and weaken claims of scientific objectivity, potentially reclassifying this as a more extractive ''snare'' if the ''scientific'' cover is deemed purely theatrical. If scientific weighting is dominant, it would push towards the ''objective_index_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_vs_normative_weighting, empirical, 'Ambiguity in the balance between scientific assessment and normative advocacy in the clock''s setting.').

omega_variable(
    accountability_void_or_coordination_benefit,
    'Does the clock''s ambiguity primarily create an accountability void (allowing the Bulletin to avoid scrutiny) or is it a necessary feature for coordinating attention on complex, uncertain risks?',
    'Comparative analysis of risk communication strategies: evaluate whether more precise, less ambiguous metrics achieve similar levels of public engagement and policy response for similar risks, or if the clock''s unique hybridity is genuinely more effective.',
    'If it''s primarily an accountability void, the extractiveness would be higher, potentially shifting the classification towards ''snare'' due to the uncompensated cost of anxiety. If it''s a necessary coordination benefit, the extractiveness would be lower, supporting a ''rope'' or ''scaffold'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_void_or_coordination_benefit, conceptual, 'Whether the clock''s ambiguity serves a genuine coordination function or primarily shields its administrators from accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.4).
narrative_ontology:measurement(doom_tr_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1960, 0.45).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1980, 0.5).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(doom_tr_t2010, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(doom_be_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(doom_be_t2010, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(doom_su_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1980, 0.13).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(doom_su_t2010, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, global_nuclear_disarmament_treaties).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, climate_change_mitigation_targets).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, ai_governance_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
