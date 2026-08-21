% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: AI Governance Legitimacy: Democratic Pluralist Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'democratic pluralist' reading of AI
 *   governance legitimacy, asserting that legitimate principles for AI emerge
 *   from inclusive public reason and the consent of the governed, rather than
 *   from a single authoritative tradition (religious or technocratic). It
 *   views the encyclical as one voice among many, contributing to a broader,
 *   ongoing deliberation. The constraint is classified as a Scaffold because
 *   it aims to build and establish the participatory infrastructure for this
 *   democratic process, with an implicit sunset on the initial 'building'
 *   phase.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.2).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "AI Governance Legitimacy: Democratic Pluralist Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '0aa6d63c-9708-41fe-a768-df9ecb33c0d5').
narrative_ontology:cs_kernel_codification('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', distributed).
narrative_ontology:cs_authority_grounding('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', practice).
narrative_ontology:cs_interpretation_layer_present('0aa6d63c-9708-41fe-a768-df9ecb33c0d5').
narrative_ontology:cs_reading_relation('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', foundational, legitimacy_from_consent).
narrative_ontology:cs_axiom_status(legitimacy_from_consent, holdable).
narrative_ontology:cs_axiom_grounding('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', legitimacy_from_consent, deontological).
narrative_ontology:cs_axiom('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', foundational, pluralism_of_values).
narrative_ontology:cs_axiom_status(pluralism_of_values, holdable).
narrative_ontology:cs_axiom_grounding('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', pluralism_of_values, conventional).
narrative_ontology:cs_reference_frame('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', enlightenment_public_reason).
narrative_ontology:cs_drift_state('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', contemporary_global_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0aa6d63c-9708-41fe-a768-df9ecb33c0d5', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, excluded_from_deliberation).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for establishing and maintaining the deliberative processes, ensuring transparency, and enacting policies that reflect public consent. They gain legitimacy and public trust from this approach.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Actively participate in and facilitate public deliberation, advocating for diverse interests and ensuring inclusive representation. They benefit from having their voices heard and influencing policy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Their values and concerns are explicitly sought and balanced in the deliberative process, protecting them from AI systems that might otherwise marginalize or harm them. They benefit from enhanced protection and representation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, constrained, local).

% Despite the stated goal of inclusion, some groups may still be systematically excluded due to structural barriers, lack of resources, or political marginalization, bearing the cost of non-representation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, excluded_from_deliberation, payer,
    powerless, immediate, trapped, local).

% Cannot participate in democratic deliberation and consent processes, and thus AI governance in their context lacks this form of legitimacy, potentially leading to systems that serve state control rather than public good.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, generational, trapped, national).

% Lose their prior de facto monopoly on defining AI ethics and governance, as their expertise is integrated into a broader deliberative framework rather than being the sole authority. They bear the cost of shared authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites, payer,
    powerful, biographical, mobile, global).

% Their interpretive monopoly on ethical principles for AI is explicitly denied, and their contributions are treated as one voice among many in public reason. They bear the cost of diminished unique authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, religious_authorities, payer,
    institutional, generational, constrained, global).

% Study the effectiveness and legitimacy of democratic deliberation in AI governance, assessing its inclusivity, transparency, and ability to produce just outcomes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To collectively establish legitimate and just principles for AI development and deployment across diverse societies and value systems, ensuring broad public buy-in and trust.
% TRANSFER_FUNCTION: Transfers interpretive authority from singular traditions (religious, technocratic) or market forces to inclusive public deliberation and democratically accountable institutions. It also transfers the burden of justification to those proposing AI policies.
% ABSENT_VOICES: Those systematically excluded from deliberative processes due to lack of resources, political oppression, or structural marginalization. Their absence undermines the very legitimacy the constraint seeks to establish.
% DISAPPEARANCE_RATIONALE: If the commitment to democratic deliberation and consent for AI governance vanished, the vacuum would likely be filled by technocratic, market-driven, or authoritarian approaches, leading to AI systems that lack broad public trust, entrench existing power imbalances, and fail to serve the common good as broadly defined.
% FOUNDING_PROBLEM: The risk of AI development proceeding without broad societal input, leading to systems that entrench existing inequalities or are designed by narrow interests, lacking public trust and legitimacy, and potentially causing widespread societal harm.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, ethicists, civil society groups, and international organizations widely attest to the ongoing challenge of ensuring democratic legitimacy and public trust in rapidly advancing technological domains like AI. Reports from UNESCO, the UN, and various national AI ethics commissions corroborate this problem.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderately (0.45) as establishing democratic processes can be resource-intensive and may initially impose costs on previously dominant actors, but it is modeled to decrease slightly over time (to 0.35) as the infrastructure matures and becomes more efficient in balancing diverse values. Suppression is low (0.20) because the core function is inclusion and open deliberation, not coercion. Theater ratio is low (0.10) as the emphasis is on genuine, transparent political processes rather than performative gestures. Accessibility collapse is low (0.20) because it actively seeks to open up alternatives and participation. Resistance is moderate (0.40) reflecting the inherent contestation and negotiation in democratic processes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of democratic institutions and civil society, this constraint is a vital Rope or Scaffold, building necessary coordination for legitimate governance. From the perspective of technocratic or religious authorities, it may be perceived as a Snare, extracting their traditional authority and imposing a burdensome, pluralistic process. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic institutions, civil society organizations, and minority rights holders are beneficiaries, gaining legitimacy, influence, and protection. Technocratic elites and religious authorities are payers, as they lose their prior interpretive monopolies and must engage in broader public reason. Groups systematically excluded from deliberation or living under authoritarian regimes are also payers, as they bear the cost of non-representation or lack of legitimate governance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Scaffold, implying a transitional phase to build participatory infrastructure. Its 'sunset' is the completion of this initial infrastructure, after which the ongoing governance process would ideally transition to a more stable Rope-like coordination. The low theater ratio and decreasing extractiveness over time suggest it is not drifting towards a Piton, but rather genuinely working towards its stated goal of establishing legitimate governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_democracy,
    'Does ''democratic deliberation'' refer to purely procedural legitimacy (fair process) or also substantive outcomes (just policies)?',
    'Analysis of judicial review standards and legislative outcomes: if courts or laws consistently prioritize substantive justice over procedural regularity, it leans towards substantive democracy.',
    'If purely procedural, the constraint''s effective extractiveness might be higher for minority groups whose substantive concerns are outvoted. If substantive, it implies stronger protections and lower extraction for vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_democracy, conceptual, 'Ambiguity in the definition of democratic legitimacy.').

omega_variable(
    capture_by_well_resourced_groups,
    'To what extent are ''inclusive public reason'' and ''transparent political processes'' susceptible to capture by well-resourced civil society organizations or other powerful interests?',
    'Empirical studies of lobbying, funding, and media influence in AI policy debates; analysis of who actually shapes the agenda and final decisions.',
    'If capture is substantial, the effective extractiveness for truly powerless groups would be higher, and the constraint might function more like a Tangled Rope or Snare for them, despite its democratic framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_by_well_resourced_groups, empirical, 'Risk of democratic processes being captured by powerful actors.').

omega_variable(
    pace_of_deliberation_vs_ai_development,
    'Can democratic deliberation and political processes realistically keep pace with the rapid development and deployment of AI technologies?',
    'Longitudinal studies comparing the speed of AI innovation cycles with the speed of policy development and implementation in democratic systems.',
    'If deliberation consistently lags, the constraint''s ability to genuinely shape AI governance is diminished, potentially leading to a higher theater ratio and a de facto shift towards technocratic or market-driven governance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pace_of_deliberation_vs_ai_development, empirical, 'Feasibility of democratic processes in a fast-moving technological domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(ai_g_tr_t40, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(ai_g_tr_t50, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(ai_g_be_t40, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(ai_g_be_t50, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(ai_g_su_t40, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(ai_g_su_t50, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
