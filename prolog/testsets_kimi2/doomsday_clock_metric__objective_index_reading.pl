% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock Objective Index Reading
 *   domain: science communication / normative epistemology / risk governance
 *
 * SUMMARY:
 *   The Doomsday Clock is a contested kernel with multiple readings. This
 *   constraint instantiates the objective_index_reading: the claim that the
 *   Bulletin of the Atomic Scientists' annual clock-setting tracks measurable
 *   existential risk levels through expert synthesis of empirical indicators.
 *   Structurally, this reading asserts a natural-law-like epistemic authority
 *   â that risk is quantifiable and that only credentialed experts can
 *   validly synthesize it. In operation, the constraint actively suppresses
 *   normative framing: the clock presents value-laden indicator weightings
 *   (nuclear vs climate vs AI risk, present-generation vs future-generation
 *   harms) as objective empirical conclusions. Scientific authority benefits
 *   from a monopoly on interpretation, while democratic accountability bears
 *   the cost of excluded deliberation. The claim/metric independence is
 *   maintained: the reading claims objective coordination while the authored
 *   metrics describe asymmetric extraction through enforced expert monopoly.
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists expert panel (agenda-setter/beneficiary): convenes annually to synthesize risk indicators and publishes the clock position as objective empirical conclusion.
 *   - Democratic publics (payer): receive the clock as authoritative risk information but are excluded from indicator weighting and normative framing; their own risk priorities are subordinated.
 *   - Policy elites (beneficiary): use the clock in diplomatic and budgetary rhetoric as an objective warrant for urgency, offloading normative responsibility to scientific authority.
 *   - Alternative risk assessors (excluded): include civil society researchers, indigenous knowledge holders, and deliberative practitioners who would weight risks differently or reject single-index synthesis.
 *   - Science studies scholars (observer): document how normative assumptions enter expert synthesis and how the objective framing suppresses democratic contestation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.78).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock Objective Index Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science communication / normative epistemology / risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '65649167-70dd-4458-bd20-d5d69d1077be').
narrative_ontology:cs_kernel_codification('65649167-70dd-4458-bd20-d5d69d1077be', formalized).
narrative_ontology:cs_authority_grounding('65649167-70dd-4458-bd20-d5d69d1077be', expertise).
narrative_ontology:cs_interpretation_layer_present('65649167-70dd-4458-bd20-d5d69d1077be').
narrative_ontology:cs_reading_relation('65649167-70dd-4458-bd20-d5d69d1077be', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('65649167-70dd-4458-bd20-d5d69d1077be', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('65649167-70dd-4458-bd20-d5d69d1077be', foundational, existential_risk_quantifiable).
narrative_ontology:cs_axiom_status(existential_risk_quantifiable, holdable).
narrative_ontology:cs_axiom_grounding('65649167-70dd-4458-bd20-d5d69d1077be', existential_risk_quantifiable, empirically_contingent).
narrative_ontology:cs_axiom('65649167-70dd-4458-bd20-d5d69d1077be', foundational, expert_synthesis_supersedes_deliberation).
narrative_ontology:cs_axiom_status(expert_synthesis_supersedes_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('65649167-70dd-4458-bd20-d5d69d1077be', expert_synthesis_supersedes_deliberation, instrumental).
narrative_ontology:cs_reference_frame('65649167-70dd-4458-bd20-d5d69d1077be', objective_risk_quantification).
narrative_ontology:cs_drift_state('65649167-70dd-4458-bd20-d5d69d1077be', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65649167-70dd-4458-bd20-d5d69d1077be', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_expert_panel).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, policy_elites).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_publics).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, expert_risk_synthesis_superiority).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, existential_risk_communication_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes annually to synthesize nuclear, climate, biotech, and AI risk indicators into a unified clock position. Presents the result as empirical conclusions from objective analysis. Receives global media coverage, policy access, and epistemic authority from being the definitive risk arbiter. Members can exit the panel while retaining individual scientific credibility.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_expert_panel, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, bulletin_expert_panel, beneficiary).

% Receive the clock reading as authoritative risk information. Their own normative risk priorities and local knowledge are excluded from the synthesis. They cannot directly challenge indicator weightings or panel composition. The clock shapes their political anxiety and demands without their participation in its construction, and they cannot opt out of the global risk discourse it dominates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_publics, payer,
    powerless, civilizational, constrained, global).

% Use the clock in diplomatic speeches, treaty negotiations, and funding requests as an objective warrant for urgency. Benefit from the offload of normative responsibility to scientific authority, making policy choices appear compelled by empirical risk rather than political selection. Exit is constrained because abandoning the clock would require developing alternative risk legitimation strategies.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_elites, beneficiary,
    powerful, biographical, constrained, global).

% Include civil society risk researchers, indigenous knowledge holders, and deliberative practitioners who would weight risk indicators differently or reject single-index commensuration. They are excluded from the Bulletin's formal process and from the media coverage that treats the clock as the definitive risk statement. Their exclusion maintains the expert panel's interpretive monopoly.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, alternative_risk_assessors, excluded,
    moderate, biographical, constrained, global).

% Study the sociology of the clock-setting process, documenting how normative assumptions enter expert synthesis and how the objective framing suppresses democratic contestation. They observe the structural extraction of interpretive authority without participating in the constraint's operation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, bulletin_expert_panel).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synthesizing complex, multi-domain existential risk indicators â nuclear arsenals, climate trajectories, biotech capabilities, AI development â into a single communicable metric that publics and policymakers can apprehend without specialized technical expertise.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk prioritization from democratic deliberation and civil society assessment to a closed expert panel; moves public attention and policy urgency according to the expert panel's annual risk ranking.
% ABSENT_VOICES: Democratic publics whose local risk priorities differ from global expert synthesis; normative ethicists who would insist on transparent value-weighting; indigenous knowledge holders and alternative risk assessors who would use different indicators or reject single-index commensuration; critical scientists who dispute the feasibility of objective cross-domain risk comparison.
% DISAPPEARANCE_RATIONALE: Global risk communication would lose its most prominent coordinating symbol; policy elites would need alternative legitimation strategies for urgency; democratic publics might develop more pluralistic risk assessment practices; the Bulletin would revert to a specialist advocacy organization without transgressive epistemic authority.
% FOUNDING_PROBLEM: Communicating urgent nuclear risk to a non-specialist public and policymakers in the early Cold War, providing a shared reference point for an otherwise technically opaque strategic threat.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and Cold War diplomacy corroborate the original information problem. Democratic theorists and science communication scholars from outside the Bulletin attest that while risk communication remains necessary, the specific solution of expert monopoly on a single index has outlived its original context and now functions as much to concentrate authority as to inform.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is high (0.72) because the constraint transfers substantial interpretive authority from democratic deliberation to a closed expert panel, and policy choices are reframed as compelled by empirical risk rather than selected through politics. Suppression is higher (0.78) because the constraint's persistence depends on actively excluding alternative assessors and concealing the normative choices embedded in indicator weighting. Theater ratio is moderate (0.45): genuine empirical analysis occurs, but the 'minutes to midnight' metaphor and objectivity claims perform significant legitimizing work. Accessibility collapse (0.72) reflects that publics and policymakers rarely develop independent risk-assessment frameworks once the clock is accepted as the definitive index. Resistance (0.58) captures growing contestation from democratic theorists, populist movements, and critical scientists who challenge expert monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin expert-panel seat, the constraint is necessary coordination â synthesizing incomprehensible complexity into a single actionable metric that publics and policymakers can apprehend. From the democratic-public seat, the same structure is extraction: their risk priorities, local knowledge, and normative choices are excluded from the synthesis that governs their anxieties and policy environment. Policy elites sit between, receiving coordination benefits (ready-made urgency metric) while paying in democratic legitimacy (their choices appear externally compelled rather than politically accountable). The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin expert panel is the structural beneficiary and agenda-setter: it controls the synthesis process, collects epistemic rents in the form of media authority and policy access, and has arbitrage-grade exit (members retain individual scientific standing). Its directionality sits near the beneficiary end. Democratic publics are the structural payer: they bear the cost of lost interpretive agency, have no exit from the global risk discourse the clock dominates, and their directionality sits near the full-target end. Policy elites are net beneficiaries of coordination but diffuse payers in democratic accountability; their directionality sits in the mid-range. Alternative risk assessors are structurally excluded, not coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy and the tangled_rope gating requirements, this constraint could be misread as a simple rope (genuine risk communication) or a pure snare (propaganda). The tangled_rope classification is structurally gated on the coexistence of a genuine coordination function (expert synthesis does solve an information problem for complex, cross-domain risk) with asymmetric extraction (the synthesis is monopolized, normative choices are hidden, and democratic accountability is suppressed). The founding problem â communicating urgent nuclear risk in the early Cold War â was live in 1947; its contemporary status is contested because the clock now synthesizes incommensurable domains (nuclear, climate, biotech, AI) and the expert monopoly has intensified while the original information asymmetry has partly eroded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'This constraint is the objective_index_reading of kernel doomsday_clock_metric; siblings are performative_tool_reading and hybrid_legitimacy_reading. What does each sibling change structurally, and where is the disagreement located?',
    'Cross-reading comparison across the three instantiated constraints. If performative_tool_reading is accurate, the objective claim is legitimizing theater; if hybrid_legitimacy_reading is accurate, the objective claim falsely denies irreducible value-entanglement.',
    'If sibling readings are descriptively superior, this constraint''s objectivity claim functions as cover, extraction rises toward snare territory, and theater_ratio understates the performative component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Kernel reading contest and sibling structural deltas').

omega_variable(
    normative_framing_suppression,
    'Does the clock-setting process genuinely eliminate normative framing, or does it conceal normative choices behind empirical rhetoric?',
    'Discourse analysis of Bulletin announcement language and deliberation records; comparison of published indicator weightings against implicit value hierarchies (nuclear vs climate vs AI prioritization).',
    'If normative choices are concealed rather than absent, suppression is higher than structural barriers suggest, and the objective-index claim functions as legitimizing theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_framing_suppression, empirical, 'Whether normative choices are truly excluded or only hidden').

omega_variable(
    expert_monopoly_necessity,
    'Is expert monopoly on existential risk interpretation necessary due to cognitive complexity, or is it extractive capture of democratic deliberation?',
    'Comparative analysis of participatory risk assessment methods and deliberative forecasting against expert-only synthesis; measure predictive parity and public uptake.',
    'If participatory methods perform comparably, the expert monopoly is extractive; if they fail, the coordination function is genuine and extraction is partly coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expert_monopoly_necessity, empirical, 'Whether expert monopoly is structurally necessary or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_clock_obj_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(doomsday_clock_obj_tr_t13, doomsday_clock_metric__objective_index_reading, theater_ratio, 13, 0.15).
narrative_ontology:measurement(doomsday_clock_obj_tr_t26, doomsday_clock_metric__objective_index_reading, theater_ratio, 26, 0.2).
narrative_ontology:measurement(doomsday_clock_obj_tr_t39, doomsday_clock_metric__objective_index_reading, theater_ratio, 39, 0.28).
narrative_ontology:measurement(doomsday_clock_obj_tr_t52, doomsday_clock_metric__objective_index_reading, theater_ratio, 52, 0.35).
narrative_ontology:measurement(doomsday_clock_obj_tr_t65, doomsday_clock_metric__objective_index_reading, theater_ratio, 65, 0.42).
narrative_ontology:measurement(doomsday_clock_obj_tr_t78, doomsday_clock_metric__objective_index_reading, theater_ratio, 78, 0.45).

% Extraction over time
narrative_ontology:measurement(doomsday_clock_obj_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(doomsday_clock_obj_be_t13, doomsday_clock_metric__objective_index_reading, base_extractiveness, 13, 0.3).
narrative_ontology:measurement(doomsday_clock_obj_be_t26, doomsday_clock_metric__objective_index_reading, base_extractiveness, 26, 0.38).
narrative_ontology:measurement(doomsday_clock_obj_be_t39, doomsday_clock_metric__objective_index_reading, base_extractiveness, 39, 0.48).
narrative_ontology:measurement(doomsday_clock_obj_be_t52, doomsday_clock_metric__objective_index_reading, base_extractiveness, 52, 0.58).
narrative_ontology:measurement(doomsday_clock_obj_be_t65, doomsday_clock_metric__objective_index_reading, base_extractiveness, 65, 0.66).
narrative_ontology:measurement(doomsday_clock_obj_be_t78, doomsday_clock_metric__objective_index_reading, base_extractiveness, 78, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(doomsday_clock_obj_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(doomsday_clock_obj_su_t13, doomsday_clock_metric__objective_index_reading, suppression_requirement, 13, 0.35).
narrative_ontology:measurement(doomsday_clock_obj_su_t26, doomsday_clock_metric__objective_index_reading, suppression_requirement, 26, 0.45).
narrative_ontology:measurement(doomsday_clock_obj_su_t39, doomsday_clock_metric__objective_index_reading, suppression_requirement, 39, 0.55).
narrative_ontology:measurement(doomsday_clock_obj_su_t52, doomsday_clock_metric__objective_index_reading, suppression_requirement, 52, 0.65).
narrative_ontology:measurement(doomsday_clock_obj_su_t65, doomsday_clock_metric__objective_index_reading, suppression_requirement, 65, 0.73).
narrative_ontology:measurement(doomsday_clock_obj_su_t78, doomsday_clock_metric__objective_index_reading, suppression_requirement, 78, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Doomsday Clock' conflates three structurally distinct constraints: an objective index claim (this file), a performative mobilization claim, and a hybrid legitimacy claim. Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family through mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
