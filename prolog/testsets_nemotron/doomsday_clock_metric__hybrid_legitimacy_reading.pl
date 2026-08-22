% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Doomsday Clock Metric — Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained since 1947 by the Bulletin of the Atomic
 *   Scientists, is widely treated as an objective index of existential risk.
 *   This reading — the hybrid_legitimacy_reading — holds that the Clock's
 *   *actual* operation is structurally hybrid: it coordinates global
 *   attention on existential risks (genuine coordination function) while
 *   simultaneously displacing democratic accountability for risk judgments
 *   onto an unaccountable expert body (asymmetric extraction). The Clock's
 *   deliberate ambiguity — neither a pure measurement nor a pure performance
 *   — is the source of its legitimacy; it draws authority from *appearing*
 *   scientific while exercising *normative* judgment. No clear
 *   beneficiary/victim structure exists in the conventional sense: the
 *   coordination benefit is real and widely shared, but the extraction is
 *   diffuse and structural — the public sphere pays in lost deliberative
 *   capacity, and democratic governance pays in displaced responsibility. The
 *   Bulletin itself is both agenda-setter and primary institutional
 *   beneficiary; policy elites and risk researchers are secondary
 *   beneficiaries who also bear costs (constraint by the Clock's framing).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.38).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.22).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Metric — Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, 'dbb52c43-7bb1-47c4-9e29-deda8abea4a7').
narrative_ontology:cs_kernel_codification('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', distributed).
narrative_ontology:cs_authority_grounding('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', practice).
narrative_ontology:cs_interpretation_layer_present('dbb52c43-7bb1-47c4-9e29-deda8abea4a7').
narrative_ontology:cs_reading_relation('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', foundational, ambiguity_as_legitimacy_source).
narrative_ontology:cs_axiom_status(ambiguity_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', ambiguity_as_legitimacy_source, conventional).
narrative_ontology:cs_axiom('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', foundational, expert_judgment_irreducibly_normative).
narrative_ontology:cs_axiom_status(expert_judgment_irreducibly_normative, holdable).
narrative_ontology:cs_axiom_grounding('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', expert_judgment_irreducibly_normative, deontological).
narrative_ontology:cs_reference_frame('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', scientist_public_awakening_symbol).
narrative_ontology:cs_drift_state('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', contemporary_governance_authorization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbb52c43-7bb1-47c4-9e29-deda8abea4a7', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policy_elites_seeking_legible_risk_signals).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, public_sphere_accountability).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, democratic_deliberation_on_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains editorial control over the Clock's setting through its Science and Security Board; the Clock is the Bulletin's primary institutional brand and fundraising vehicle; the setting process is deliberative but closed, with no formal external accountability mechanism.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, arbitrage, global).

% Use the Clock as a ready-made, authoritative risk signal that bypasses contested technical debates; the Clock's ambiguity lets them cite 'scientific consensus' without specifying which scientists or which evidence; they can adopt or ignore the signal as political convenience dictates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_elites_seeking_legible_risk_signals, beneficiary,
    powerful, biographical, mobile, national).

% Gain visibility and funding attention from the Clock's cultural prominence; simultaneously constrained by the Clock's framing of risk categories — research agendas that don't map to the Clock's minutes-to-midnight logic struggle for recognition; career advancement tracks the Clock's attention economy.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_community, payer).

% Receives a simplified, metaphorical risk signal that substitutes for substantive public deliberation on existential threats; the Clock's authority forecloses questions about *whose* judgments, *which* values, and *what* evidence; no mechanism exists for the public to contest or refine the setting.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, public_sphere_accountability, payer,
    powerless, generational, trapped, global).

% The Clock's pronouncements function as expert declarations that preempt democratic contestation of risk priorities; its scientific veneer makes political disagreement appear as 'science denial'; deliberative spaces shrink as the Clock becomes the shorthand for 'what experts think.'
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, democratic_deliberation_on_risk, payer,
    moderate, generational, constrained, global).

% Produce rigorous quantitative risk assessments that the Clock's qualitative, Board-driven process does not incorporate; their work is structurally invisible to the setting mechanism; they would challenge the Clock's epistemic monopoly but lack institutional access.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, independent_risk_scholars, excluded,
    moderate, biographical, constrained, global).

% Observes the Clock as a case study in how scientific authority is constructed and deployed in governance; sees the coordination function (shared risk vocabulary) and the extraction function (accountability displacement) operating simultaneously; no stake in the Clock's institutional success.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally recognized, conceptually accessible symbol that coordinates attention, discourse, and policy urgency around existential risks across scientific, political, and public domains — a shared reference point that did not exist before 1947.
% TRANSFER_FUNCTION: Transfers epistemic authority and agenda-setting power from democratic deliberation and quantitative risk analysis to a closed expert Board; transfers institutional legitimacy and funding attention to the Bulletin and research areas aligned with the Clock's framing; transfers the burden of justification from policymakers to 'the scientists.'
% ABSENT_VOICES: Independent quantitative risk analysts, affected communities in the Global South facing proximate existential threats (nuclear fallout zones, climate frontlines), and publics who would demand participatory risk governance — all are structurally excluded from the setting process and its legitimation.
% DISAPPEARANCE_RATIONALE: If the Clock vanished, the Bulletin would lose its central convening symbol; policymakers would lose a convenient authoritative citation; risk researchers would lose a dominant attention funnel; publics would lose a familiar (if misleading) risk heuristic; new coordination mechanisms would need to emerge — likely more fragmented, possibly more democratic, possibly less salient.
% FOUNDING_PROBLEM: Post-Hiroshima need for scientists to communicate nuclear danger to publics and policymakers without requiring technical literacy; the founding problem was *communication*, not *governance* — a symbol to wake people up, not a standing institution to authorize policy.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's own histories (e.g., 'The Doomsday Clock at 75') attest the founding problem was communicative; Kennette Benedict (former Bulletin ED) has written that the Clock was never meant to be a governance instrument; independent historians of science (e.g., Alex Wellerstein, Audra Wolfe) corroborate the communicative origin; the Bulletin's current leadership treats the governance function as a natural evolution, not a departure.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects the Clock's displacement of democratic deliberation and quantitative rigor — not a monetary rent but an epistemic rent. Suppression (0.22) is low in coercive terms (no one is forced to cite the Clock) but structurally significant: the Clock's cultural saturation makes alternative risk framings marginally harder to advance. Theater ratio (0.41) is substantial: the elaborate Board deliberation and minute-by-minute announcements perform scientific rigor while the actual setting reflects normative judgments about *which* risks matter and *how much*. Accessibility collapse (0.35) is moderate: alternative risk metrics exist (e.g., Global Challenges Foundation, quantitative x-risk estimates) but the Clock's brand dominance creates a gravitational center. Resistance (0.28) is present but fragmented: independent scholars critique the methodology, but no coordinated challenge to the Clock's epistemic authority has gained traction.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin's seat, the Clock is a rope: genuine coordination with minimal coercion. From the public sphere's seat, it is a snare: extraction without consent. From the risk researcher's seat, it is a tangled rope: real coordination benefit tangled with framing capture. The engine computes these per-seat classifications from the structural data; the claimed_type (tangled_rope) represents the authoring seat's structural judgment that the constraint *as a whole* instantiates both functions irreducibly.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin (agenda_setter, institutional, arbitrage exit) sits at d ≈ 0.15 — it controls the constraint and extracts institutional capital. Policy elites (beneficiary, powerful, mobile) sit at d ≈ 0.25 — they use the Clock instrumentally and can discard it. Existential risk researchers (beneficiary/payer, organized, constrained) sit at d ≈ 0.55 — they gain visibility but are constrained by the Clock's framing. Public sphere accountability (payer, powerless, trapped) sits at d ≈ 0.9 — it bears the structural cost with no exit. Democratic deliberation (victim, moderate, constrained) sits at d ≈ 0.75 — it is displaced but could theoretically reclaim space. Independent scholars (excluded, moderate, constrained) and the analytical observer (observer, analytical) sit outside the extraction gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communication of nuclear danger) is contested — the Bulletin says it persists; historians say it has been substantially solved by the existence of multiple risk communication channels. The Clock persists *as if* the founding problem were live, but its current function (governance authorization via expert symbol) differs structurally from its founding function (public awakening via metaphor). This is mandatrophy: the mandate has outlived its function, but the constraint persists by converting its original communicative legitimacy into ongoing governance legitimacy. The hybrid_legitimacy_reading captures this by treating the ambiguity *itself* as the constraint's operating principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the Clock''s governance legitimacy derived from its scientific credibility, its normative authority, or the deliberate ambiguity between them?',
    'Counterfactual: if the Bulletin explicitly declared the Clock a normative judgment (not a risk index), would policymakers still cite it with the same authority? If yes, legitimacy is normative; if no, legitimacy depends on the scientific veneer.',
    'If legitimacy requires the scientific veneer, the hybrid structure is extractive (the ambiguity is the mechanism). If legitimacy survives explicit normative framing, the coordination function may be genuine and the extraction incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether the Clock''s authority requires the science/norm ambiguity or survives its resolution.').

omega_variable(
    accountability_void_nature,
    'Is the displacement of democratic deliberation an inherent feature of *any* expert risk symbol, or a contingent feature of the Clock''s specific institutional design (closed Board, no appeal, brand monopoly)?',
    'Comparative analysis: do other expert risk symbols (IPCC summaries, WHO pandemic phases, IAEA nuclear safety ratings) produce similar accountability voids, or do their institutional designs (intergovernmental review, formal challenge processes) mitigate it?',
    'If inherent, the hybrid_legitimacy_reading applies to the *genre* of expert risk symbols; if contingent, the Clock''s extraction is a design choice, not a structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_void_nature, empirical, 'Whether the accountability extraction is generic to expert risk symbols or specific to the Clock.').

omega_variable(
    kernel_reading_relations,
    'How do the three readings of the doomsday_clock_metric kernel structurally relate — does the hybrid_legitimacy_reading foreclose, coexist with, or influence the objective_index_reading and performative_tool_reading?',
    'Analyze whether a single institutional framework could simultaneously hold the hybrid reading (ambiguity as legitimacy source) and the objective reading (Clock as measurement) — if the objective reading requires the Clock to *be* a measurement, the hybrid reading''s claim that ambiguity is essential forecloses it.',
    'If forecloses: the readings cannot coexist in one governance framework; if coexists_with: different actors hold different readings simultaneously; if influences: the hybrid reading''s prominence shapes the operating conditions for the other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between this reading and its siblings in the doomsday_clock_metric kernel.').

omega_variable(
    mandatrophy_corroboration_gap,
    'Is there any corroborating source *outside the Bulletin''s benefiting parties* that attests the founding problem (nuclear danger communication) remains live in its original form?',
    'Survey independent historians of science, nuclear risk communicators, and science policy scholars not affiliated with the Bulletin; assess whether they see the *original* communicative problem as unsolved or whether they see the Clock as solving a different problem now.',
    'If no external corroboration exists, the founding_problem_status = ''contested'' with ''no external corroboration'' is itself evidence of mandatrophy — the arrangement persists by self-certifying its own continued relevance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_corroboration_gap, empirical, 'Whether the Clock''s founding problem has external corroboration or only self-attestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcm_hl_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.18).
narrative_ontology:measurement(dcm_hl_tr_t1953, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1953, 0.25).
narrative_ontology:measurement(dcm_hl_tr_t1963, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1963, 0.22).
narrative_ontology:measurement(dcm_hl_tr_t1984, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1984, 0.32).
narrative_ontology:measurement(dcm_hl_tr_t1991, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(dcm_hl_tr_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2007, 0.36).
narrative_ontology:measurement(dcm_hl_tr_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(dcm_hl_tr_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(dcm_hl_tr_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(dcm_hl_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(dcm_hl_be_t1953, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1953, 0.22).
narrative_ontology:measurement(dcm_hl_be_t1963, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1963, 0.18).
narrative_ontology:measurement(dcm_hl_be_t1984, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1984, 0.28).
narrative_ontology:measurement(dcm_hl_be_t1991, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement(dcm_hl_be_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2007, 0.31).
narrative_ontology:measurement(dcm_hl_be_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(dcm_hl_be_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(dcm_hl_be_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dcm_hl_su_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1947, 0.08).
narrative_ontology:measurement(dcm_hl_su_t1953, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1953, 0.12).
narrative_ontology:measurement(dcm_hl_su_t1963, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1963, 0.1).
narrative_ontology:measurement(dcm_hl_su_t1984, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1984, 0.18).
narrative_ontology:measurement(dcm_hl_su_t1991, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1991, 0.15).
narrative_ontology:measurement(dcm_hl_su_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2007, 0.2).
narrative_ontology:measurement(dcm_hl_su_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2015, 0.21).
narrative_ontology:measurement(dcm_hl_su_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(dcm_hl_su_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__hybrid_legitimacy_reading, 0.03).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, global_catastrophic_risk_governance_framework).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, expert_authority_in_climate_policy).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three constraint stories: objective_index_reading (ε ≈ 0.15, claimed mountain/rope), performative_tool_reading (ε ≈ 0.55, claimed snare), and this hybrid_legitimacy_reading (ε ≈ 0.38, claimed tangled_rope). The hybrid reading occupies the structural middle: it acknowledges the coordination function the objective reading centers and the extraction the performative reading centers, but argues the *ambiguity itself* is the operating principle. The objective reading influences the hybrid (the Clock's scientific veneer enables the hybrid's legitimacy); the performative reading influences the hybrid (the Clock's policy impact motivates the Board's normative judgments); the hybrid influences both by exposing their partiality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, powerful, 0.25).
constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, organized, 0.55).
constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, powerless, 0.9).
constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
