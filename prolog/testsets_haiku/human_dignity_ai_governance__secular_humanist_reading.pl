% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_governance__secular_humanist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_governance__secular_humanist_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: human_dignity_ai_governance__secular_humanist_reading
 *   human_readable: Secular Humanist Dignity-Rights Framework for AI Governance
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   This constraint embodies the secular-humanist reading of the contested
 *   kernel 'human dignity grounds AI governance.' It asserts that AI systems
 *   must respect rights (privacy, autonomy, non-discrimination, due process)
 *   grounded in rational individual personhood and universal human rights
 *   doctrine (UDHR framework). This reading insists that AI governance should
 *   be determined through democratic deliberation, not by theological
 *   authority. Dignity is defended through law and rights-adjudication, not
 *   theology. The constraint sits in tension with three sibling readings: (1)
 *   the magisterial integralist reading, which grounds dignity in imago Dei
 *   and Church authority; (2) the pluralist pragmatic reading, which resists
 *   any single metaphysical ground and seeks negotiated overlapping
 *   consensus; (3) the techno-optimist reading, which sees dignity as
 *   enhanced through augmentation, not limited by rights-constraints. Each
 *   reading produces a different constraint on AI, with different
 *   beneficiaries, victims, extractiveness, and type. This story generates
 *   ONLY the secular-humanist reading and routes the inter-reading contest
 *   through omega variables per the committer-frame discipline.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_governance__secular_humanist_reading, 0.38).
domain_priors:suppression_score(human_dignity_ai_governance__secular_humanist_reading, 0.28).
domain_priors:theater_ratio(human_dignity_ai_governance__secular_humanist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_governance__secular_humanist_reading, rope).
narrative_ontology:human_readable(human_dignity_ai_governance__secular_humanist_reading, "Secular Humanist Dignity-Rights Framework for AI Governance").
narrative_ontology:topic_domain(human_dignity_ai_governance__secular_humanist_reading, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(human_dignity_ai_governance__secular_humanist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_governance__secular_humanist_reading, 'c34cec99-915f-4911-b4ba-1ccf65f2cd2e').
narrative_ontology:cs_kernel_codification('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', distributed).
narrative_ontology:cs_authority_grounding('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', distributed).
narrative_ontology:cs_reading_relation('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', human_dignity_ai_governance__magisterial_integralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', human_dignity_ai_governance__pluralist_pragmatic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', human_dignity_ai_governance__techno_optimist_reading, coexists_with).
narrative_ontology:cs_axiom('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', foundational, dignity_grounded_in_rational_autonomy).
narrative_ontology:cs_axiom_status(dignity_grounded_in_rational_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', dignity_grounded_in_rational_autonomy, deontological).
narrative_ontology:cs_axiom('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', foundational, democratic_deliberation_sole_legitimacy_source).
narrative_ontology:cs_axiom_status(democratic_deliberation_sole_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', democratic_deliberation_sole_legitimacy_source, conventional).
narrative_ontology:cs_reference_frame('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', secular_democratic_dignity_governance).
narrative_ontology:cs_drift_state('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', contemporary_ai_governance_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c34cec99-915f-4911-b4ba-1ccf65f2cd2e', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, rights_holders_globally).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, democratic_deliberative_institutions).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, legal_regulatory_bodies).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, excluded_voices_in_governance).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, religious_authority_institutions).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, communities_outside_liberal_secular_framework).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_governance__secular_humanist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(human_dignity_ai_governance__secular_humanist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_governance__secular_humanist_reading_tests).
:- end_tests(human_dignity_ai_governance__secular_humanist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint transfers authority from theological institutions to democratic ones, and enforces rights-based limits on AI developers. The transfer itself is an extraction from religious institutions' claim to unique governance authority. However, extractiveness is capped at moderate-low because (1) the constraint does not require a comprehensive worldview adoption—only behavioral compliance with rights standards; (2) it does not concentrate benefits in a single captured actor; (3) the democratic institutions that enforce it are structurally accountable to the rights-holders they protect. Suppression is low (0.28) because the constraint's enforcement relies primarily on legal transparency, due process, and appeal mechanisms rather than coercion. However, suppression is not negligible because (1) religious institutions are systematically excluded from the authority-setting frame; (2) communities outside secular-humanist anthropologies are suppressed in their inability to contest the dignity-ground itself; (3) the deliberative structure privileges secular epistemologies. Theater ratio is very low (0.12) because the rights-protection function is genuine and constantly exercised through courts, audits, and regulations—not performed for legitimacy. The measurements show extractiveness and suppression rising through year 15 (as regulatory regime solidifies and religious pushback emerges) then plateauing, consistent with a mature constraint whose basic structure is now stable and contested from the outside rather than evolving internally. The interval (0–40) spans early AI governance debates (0–10), regulatory establishment phase (10–25), and stabilization with ongoing sibling-reading contestation (25–40 projected).
 *
 * PERSPECTIVAL GAP:
 *   From the secular-humanist reading's own seat: the constraint is rope—genuine coordination that solves the pluralist-democracy problem of how to limit AI without imposing theology. From the magisterial integralist seat: the constraint is snare—it extracts authority from the Church and enforces a secular monopoly on dignity-language, foreclosing genuine theological governance. From the pluralist pragmatist seat: the constraint is tangled-rope with hidden exclusion—it claims to be secular-neutral but privileges rationalist epistemology, forcing theological voices into the choice between translation or silence. From the techno-optimist seat: the constraint is snare—it extracts from innovators and enforces a particular anthropology (autonomy-protection) that may conflict with enhancement and flourishing. The engine will compute these divergences from the structural data: the same institutional arrangement produces rope for rights-holders, snare for religious institutions, tangled-rope for pragmatists, and snare for techno-optimists. That divergence IS the evidence that this is a contested reading, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-holders globally sit near the beneficiary end (d near 0.1): the constraint protects them without requiring any particular belief system. Democratic deliberative institutions and legal bodies sit near beneficiary-with-power (d near 0.2): they gain authority over governance and can revise it through process. AI developers sit near the target end (d near 0.8): they bear compliance costs, engineering overhead, and constraints on deployment. Religious authorities sit at full-target (d = 1.0): they lose institutional authority over AI governance and are excluded from the authoritative frame. Excluded voices (subsistence communities, non-secular populations) sit in a complex position (d near 0.75): they are nominally protected by rights standards but pay a cost in epistemic injustice—their own dignity-framings are not recognized as legitimate grounds for governance standards. Directionality is derived from beneficiary/victim declarations and exit options: beneficiaries have mobile or arbitrage exit (can exit to other rights-respecting jurisdictions, can adjust); victims have constrained or identity-locked exit (religious institutions cannot un-do their theological commitments; excluded communities cannot access the deliberative table without abandoning their epistemology).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is live on its own terms: the founding problem (AI development without rights-clarity) is contested in status but still substantially present. Regulatory bodies have not achieved complete success in embedding rights-protection; harms continue; the deliberative process is ongoing. The constraint has not crossed the mandatrophy threshold where the founding problem is dead but the constraint persists by inertia. However, there is a fork: if one sibling reading (integralist, pragmatist, or techno-optimist) gains institutional dominance, this reading would become obsolete, and the constraint might persist as vestigial performance of secular-humanist language with integralist or techno-optimist substance. That is the mandatrophy-risk zone: continued flattery of 'democratic deliberation' while actual authority migrates to Church or to markets. The omega variable on enforcement asymmetry (Global South participation) traces that risk: if standards are set in wealthy democracies and imposed through trade power, the constraint becomes zombie—performing democratic legitimacy while extracting from the powerless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_vs_theological_dignity_grounding,
    'Is rational autonomy and equal moral status (UDHR frame) sufficient to ground all legitimate AI constraints, or does human dignity require theological or metaphysical foundations that secular rights language cannot capture?',
    'Empirical: do rights-based constraints (privacy, non-discrimination, due process) prevent documented harms and satisfy rights-holders across cultures? Conceptual: can a single metaphysical foundation ground dignity constraints for pluralist societies, or does secular rights language offer the only non-coercive path?',
    'If theological grounding is necessary, this constraint forecloses legitimate governance frameworks and should be reclassified as snare (enforced exclusion). If secular rights suffice, the constraint is legitimate rope. If both are defensible, the kernel is genuinely contested and all readings coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_vs_theological_dignity_grounding, conceptual, 'Whether secular rationalist dignity-grounding is epistemically sufficient or merely hegemonically imposed.').

omega_variable(
    democratic_deliberation_accessibility,
    'Are democratic deliberative institutions truly accessible to voices outside secular-humanist traditions, or does the structure of democratic deliberation itself privilege secular epistemologies and exclude theological authority claims as illegitimate?',
    'Institutional audit: how do deliberative bodies actually treat theological arguments? Are they heard and refuted on merits, or pre-emptively excluded as ''not policy arguments''? Do non-secular communities report genuine voice or performative inclusion?',
    'If democratic institutions structurally exclude theological voices, the suppression score should be higher (0.28 is likely underestimated). If inclusion is genuine but theological claims lose in fair debate, suppression is lower and the constraint is cleaner rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_deliberation_accessibility, empirical, 'Whether democratic deliberation genuinely accommodates theological perspectives or excludes them by procedural structure.').

omega_variable(
    rational_autonomy_universality_claim,
    'Is rational individual autonomy a truly universal dignity-ground, or is it a cultural-specific value of liberal-secular modernity that is being universalized through institutional power?',
    'Comparative investigation: how do non-liberal, non-secular anthropologies frame human dignity? Can AI rights-protections be framed in those terms, or must they be translated into autonomy-language? Do communities outside secular-humanist frames experience the constraint as protecting them or as imposing a foreign anthropology?',
    'If autonomy is culturally parochial, the victims category (excluded voices) should include communities for whom this dignity-frame is alien or colonizing. Extractiveness might be higher when accounting for epistemic injustice. If autonomy is defensibly universal, the constraint cleanly serves all rights-holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_autonomy_universality_claim, conceptual, 'Whether rational-individual autonomy is a universal dignity-ground or a liberal-particular value universalized through power.').

omega_variable(
    enforcement_asymmetry_north_south,
    'Do democratic deliberative institutions in wealthy liberal democracies set AI governance standards that are then imposed on Global South countries through trade, investment, and platform power, without corresponding participation?',
    'Institutional analysis: who sits at the table when AI governance standards are authored? Do Global South voices have equal vote-weight and agenda-setting power, or are they consulted after decisions are made? Does compliance with secular-humanist standards become a condition of market access?',
    'If enforcement is asymmetric, suppression is higher (excluded_voices bears suppression from powerful-institution gates), and the constraint moves toward snare or tangled-rope territory. If standard-setting is genuinely multilateral, suppression is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry_north_south, empirical, 'Whether rights-based AI governance standards are set in pluralist deliberation or imposed by wealthy democracies.').

omega_variable(
    reading_contest_underdetermination,
    'This constraint is one reading of the contested kernel ''human dignity grounds AI governance.'' The sibling readings (theological integralist, pragmatist, techno-optimist) all claim to be legitimate answers to the founding problem. What resolves which reading is the correct one?',
    'The readings differ on three axes: (1) What grounds dignity (theology vs. reason vs. pragmatism vs. enhancement capability)? (2) Who has authority to set standards (Church vs. democratic deliberation vs. negotiated consensus vs. markets)? (3) What counts as harm (violation of sacred personhood vs. violation of autonomy vs. exclusion from consensus vs. limitation of capability)? No empirical fact alone resolves this—the readings are grounded in different normative commitments. Resolution requires either: (a) winning a cultural-political contest (which reading''s advocates gain institutional power), (b) genuine theoretical synthesis (can multiple grounds cohere in one framework?), or (c) meta-level agreement to differ (pluralist acceptance that readings coexist).',
    'If this reading is correct, the other readings are either foreclosed, overridden, or permanently coexistent. The measurement of extractiveness depends on whether religious institutions are victims (if foreclosed) or competitors (if coexistent). The type classification depends on whether the constraint is enforced or negotiated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_underdetermination, conceptual, 'Whether the secular-humanist reading is the true account of dignity''s ground or one live option among incommensurable alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_governance__secular_humanist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(huma_tr_t5, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(huma_tr_t10, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(huma_tr_t15, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(huma_tr_t20, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(huma_tr_t25, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(huma_tr_t30, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(huma_tr_t35, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 35, 0.12).
narrative_ontology:measurement(huma_tr_t40, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(huma_be_t5, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(huma_be_t10, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(huma_be_t15, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(huma_be_t20, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(huma_be_t25, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(huma_be_t30, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(huma_be_t35, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(huma_be_t40, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(huma_su_t5, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 5, 0.21).
narrative_ontology:measurement(huma_su_t10, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(huma_su_t15, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(huma_su_t20, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(huma_su_t25, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(huma_su_t30, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(huma_su_t35, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 35, 0.28).
narrative_ontology:measurement(huma_su_t40, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_governance__secular_humanist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(human_dignity_ai_governance__secular_humanist_reading, 0.12).
narrative_ontology:affects_constraint(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance__magisterial_integralist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance__pluralist_pragmatic_reading).
narrative_ontology:affects_constraint(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance__techno_optimist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'human dignity grounds AI governance.' The kernel is a persisting commitment (the claim that human dignity must guide AI development) that different parties read differently. The secular-humanist reading asserts that dignity is grounded in rational autonomy and universal rights, defended through democratic deliberation and law. The sibling readings (magisterial integralist, pluralist pragmatic, techno optimist) instantiate different dignity-groundings and authority-structures. All four readings are live options in the contemporary contest; none has been logically foreclosed by the others. The readings coexist because they rest on different normative foundations (theology vs. reason vs. pragmatism vs. enhancement capability) that cannot be adjudicated by empirical fact alone. This story is the secular-humanist reading; each sibling is a separate constraint story. They affect each other through institutional competition and conceptual influence, not through logical entailment or refutation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
