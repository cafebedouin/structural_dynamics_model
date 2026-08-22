% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Work-Disincentive Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   income_support_commitment kernel. The reading claims unconditional income
 *   support operates as a work disincentive that atrophies recipient skills
 *   and increases state dependence, extracting from productive workers to
 *   subsidize non-participation. The constraint is claimed as tangled_rope:
 *   it retains a genuine coordination function (poverty insurance) but layers
 *   asymmetric extraction (work disincentive, bureaucratic self-preservation)
 *   atop it, requiring active enforcement (eligibility policing, work
 *   requirements, stigma maintenance). The ε referent is the standing
 *   arrangement — the actually existing welfare state — assessed by this
 *   reading's lights. The freedom_floor_reading and
 *   targeting_efficiency_reading are sibling constraints (other files) with
 *   different ε, different beneficiary/victim structures, different types.
 *
 * KEY AGENTS:
 *   - non_participating_recipients: Primary beneficiary (powerless/identity_locked) — receives transfer but skills atrophy, exit degrades
 *   - working_taxpayers: Primary victim (organized/constrained) — funds transfer, experiences reciprocity breach
 *   - skill_atrophied_poor: Victim (powerless/trapped) — collateral damage, human capital depreciates in system
 *   - administrative_bureaucracy: Agenda setter + beneficiary (institutional/arbitrage) — administers, benefits from persistence
 *   - policy_analysts_critics: Observer (analytical/analytical) — evaluates coordination vs extraction
 *   - excluded_workers: Excluded (moderate/mobile) — would object, structurally absent from conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.42).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.38).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '3964c3d2-a8af-47cf-b908-49fa0d3a7b53').
narrative_ontology:cs_kernel_codification('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', formalized).
narrative_ontology:cs_authority_grounding('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', lineage).
narrative_ontology:cs_interpretation_layer_present('3964c3d2-a8af-47cf-b908-49fa0d3a7b53').
narrative_ontology:cs_reading_relation('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', foundational, non_participation_is_moral_hazard).
narrative_ontology:cs_axiom_status(non_participation_is_moral_hazard, holdable).
narrative_ontology:cs_axiom_grounding('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', non_participation_is_moral_hazard, deontological).
narrative_ontology:cs_axiom('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', foundational, reciprocity_requires_labor_contribution).
narrative_ontology:cs_axiom_status(reciprocity_requires_labor_contribution, holdable).
narrative_ontology:cs_axiom_grounding('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', reciprocity_requires_labor_contribution, conventional).
narrative_ontology:cs_axiom('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', secondary, state_dependence_is_degradation).
narrative_ontology:cs_axiom_status(state_dependence_is_degradation, holdable).
narrative_ontology:cs_axiom_grounding('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', state_dependence_is_degradation, empirically_contingent).
narrative_ontology:cs_reference_frame('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', beveridge_insurance_principle).
narrative_ontology:cs_drift_state('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', contemporary_activation_paradigm, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3964c3d2-a8af-47cf-b908-49fa0d3a7b53', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, non_participating_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, administrative_bureaucracy).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophied_poor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, non_participating_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income without labor-market participation. The benefit substitutes for wage income, but long-term receipt erodes skills, professional networks, and labor-market attachment. Exit means accepting low-wage or unstable work that may leave them worse off net of benefits lost — a structural trap where the benefit itself degrades the capacity to leave it. They also bear the psychological cost of stigma and the political risk of benefit retrenchment.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, non_participating_recipients, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, non_participating_recipients, payer).

% Fund the transfer through progressive taxation. They experience the constraint as extraction from their labor product to support non-participation. Their exit options are limited: tax avoidance (constrained by enforcement), political voice (diluted), or emigration (high cost). The extraction is experienced as a breach of reciprocity — they work, others do not, and the state enforces the transfer.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Individuals who enter the support system during vulnerable periods (youth unemployment, disability onset, caregiving) and find their human capital depreciates while on benefits. The longer they remain, the less employable they become. The constraint does not actively suppress their exit — it creates a gradient where exit becomes progressively more costly. They are not the intended beneficiaries of the trap reading; they are its collateral damage.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophied_poor, payer,
    powerless, generational, trapped, national).

% Administers the income support system, defines eligibility, manages compliance, and controls the narrative of 'deservingness.' The bureaucracy benefits from a large, stable client base that justifies its budget, staffing, and institutional relevance. It has strong exit options — it can reform the system, reclassify recipients, or advocate for expansion. Its interest is in the persistence of the arrangement, not necessarily in the well-being of recipients.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, administrative_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, administrative_bureaucracy, beneficiary).

% Produce the evidence base on labor-supply elasticities, skill depreciation rates, and fiscal incidence. They do not bear the constraint's costs or collect its benefits directly. Their structural position is to evaluate whether the coordination function (poverty alleviation) is being subverted by the extraction function (work disincentive, bureaucratic self-preservation).
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, policy_analysts_critics, observer,
    analytical, civilizational, analytical, global).

% Low-wage workers who earn just above eligibility thresholds and receive no support, while seeing non-participants receive unconditional income. They would object to the arrangement if asked, but are structurally excluded from the policy conversation — their voice would challenge the moral economy of the trap reading. Their exit is mobility: they can change jobs, sectors, or locations, but cannot escape the tax incidence.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, excluded_workers, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a floor against absolute destitution for those unable to participate in the labor market (disability, caregiving, structural unemployment). Solves the coordination problem of societal risk-pooling against income loss.
% TRANSFER_FUNCTION: Moves resources from current workers (via taxation) to non-participating recipients. The transfer is universal in principle but targeted in practice by eligibility rules. The extraction is from labor income to non-labor income.
% ABSENT_VOICES: The skill_atrophied_poor and excluded_workers are structurally absent from the legislative bargain. The former are too dispersed and demoralized to organize; the latter are told their dissatisfaction is 'resentment' rather than a legitimate reciprocity claim. Both would challenge the trap reading's beneficiary/victim assignment if present.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, poverty rates would spike immediately, labor supply would increase at the margin (some recipients would enter low-wage work), but the skill-atrophied cohort would face catastrophic destitution without reintegration infrastructure. The administrative bureaucracy would lose its primary mandate. The fiscal transfer would cease, but the social crisis would reorganize politics around emergency relief.
% FOUNDING_PROBLEM: Post-war welfare states needed to insure citizens against income loss from unemployment, disability, and old age without the stigma and administrative burden of means-testing. The founding problem was destitution risk, not work disincentive.
% FOUNDING_PROBLEM_CORROBORATION: Original Beveridge Report (1942) and comparable founding documents frame the problem as 'want' (destitution), not idleness. Contemporary labor economists (e.g., Autor, Acemoglu) corroborate that the work-disincentive effect is real but modest at current benefit levels; the 'dependency trap' narrative is amplified by political actors who benefit from retrenchment. No independent corroboration exists for the claim that the *primary* function of the system is now work disincentive rather than poverty alleviation.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).
:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the transfer is real but not total; most recipients are not 'idle' by choice but structurally excluded from good work. Suppression (0.38) is moderate: the constraint does not physically prevent exit, but the benefit cliff, skill depreciation, and stigma create a soft trap. Theater ratio (0.28) reflects that work requirements, job-search mandates, and fraud detection increasingly perform 'activation' while the core transfer persists — the coordination function is real but the enforcement theater grows. Accessibility collapse (0.45) is moderate: alternatives (employment, self-employment, migration) exist but are degraded by the constraint's own operation. Resistance (0.52) is significant: political movements, legal challenges, and policy reforms continuously contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The non_participating_recipients experience the constraint as a lifeline with hidden costs (skill atrophy, stigma, exit trap) — their computed type may be scaffold or tangled_rope depending on whether they see the coordination function as genuine. The working_taxpayers experience it as snare-like extraction — they pay, others don't work, the state enforces it. The administrative_bureaucracy experiences it as rope (coordination) or piton (if they see the system as inertially maintained). The engine computes these per-seat classifications from power/exit/beneficiary declarations; the claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: non_participating_recipients (direct transfer recipients) and administrative_bureaucracy (institutional self-preservation). Victims declared: working_taxpayers (fiscal extraction) and skill_atrophied_poor (human capital destruction). The directionality derivation chains: recipients are identity_locked (benefit fused to survival, exit degrades capacity) → d near 1.0 (full target despite beneficiary label — the benefit traps). Taxpayers are organized but constrained exit → d ~0.7-0.8. Bureaucracy is institutional with arbitrage exit → d ~0.15 (beneficiary). Skill_atrophied_poor are trapped → d ~0.95. This creates the seat divergence the engine measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (destitution insurance) remains live, but the arrangement has accumulated extraction layers: work requirements that don't lead to good jobs, bureaucracy that grows faster than caseloads, stigma that serves political rhetoric. The mandate has not atrophied — the problem persists — but the *form* of the solution has been captured by extraction dynamics. This is tangled_rope, not piton: the coordination function is still needed and still operates, but the extraction is structural and active. The mandatrophy_resolved flag is false because the problem is not gone; the arrangement has not been replaced by a better coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_disincentive_magnitude,
    'What is the actual labor-supply elasticity of unconditional income support at current benefit levels — is the work disincentive large enough to constitute structural extraction, or marginal enough to be a coordination cost?',
    'Natural experiments (negative income tax trials, Alaska Permanent Fund, COVID expanded UI, universal basic income pilots) with rigorous labor-supply measurement, controlling for benefit cliffs and marginal tax rates.',
    'If elasticity is high (>0.3), the extraction claim is empirically grounded and the tangled_rope classification holds. If elasticity is near zero, the dependency_trap_reading is a political construct with low ε — the constraint would be rope or mountain from the recipient seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_disincentive_magnitude, empirical, 'Empirical magnitude of the work disincentive effect at current policy parameters.').

omega_variable(
    skill_atrophy_causality,
    'Does long-term benefit receipt *cause* skill atrophy and reduced employability, or does selection into long-term receipt reflect pre-existing low employability?',
    'Longitudinal studies with instrumental variables (policy discontinuities, random assignment to activation programs) separating causal effect of benefit duration from selection effects.',
    'If causal, the skill_atrophied_poor are genuine victims of the constraint''s operation (extraction via human capital destruction). If selection, they are a pre-existing population the constraint fails to help — the constraint is rope with implementation failure, not tangled_rope with structural extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_atrophy_causality, empirical, 'Causal direction of the skill atrophy / benefit duration relationship.').

omega_variable(
    bureaucratic_self_preservation,
    'Does the administrative bureaucracy actively resist reforms that would reduce caseloads or simplify eligibility, or is bureaucratic inertia a passive byproduct of institutional complexity?',
    'Process-tracing of reform attempts (welfare reform 1996, universal credit rollout, basic income proposals) measuring bureaucratic advocacy, rule-making discretion, and budget protection behavior.',
    'If active resistance, the bureaucracy is a strategic beneficiary (agenda_setter + beneficiary) and the extraction is intentional. If passive, the bureaucracy is a piton-like inertial maintainer and the extraction is structural but not agential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bureaucratic_self_preservation, conceptual, 'Whether bureaucratic self-interest is an active extraction mechanism or passive inertia.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the dependency_trap_reading foreclose or merely coexist with the freedom_floor_reading — is the disagreement about empirical magnitude (coexists) or about the normative status of non-participation (forecloses)?',
    'Map the logical structure of each reading''s axioms: if freedom_floor holds ''non-participation is a right'' and dependency_trap holds ''non-participation is a moral hazard,'' they foreclose within a single framework. If both hold ''non-participation has costs and benefits'' but weight them differently, they coexist.',
    'If forecloses, the kernel has a genuine fault line — no single institutional framework can satisfy both readings. If coexists_with, the kernel is a negotiated space where institutional design can balance the readings. This determines whether the constraint family requires structural separation or can be mediated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between dependency_trap and freedom_floor readings at the axiom level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_support_dependency_trap_tr_t1975, income_support_commitment__dependency_trap_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(income_support_dependency_trap_tr_t1985, income_support_commitment__dependency_trap_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(income_support_dependency_trap_tr_t1995, income_support_commitment__dependency_trap_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(income_support_dependency_trap_tr_t2005, income_support_commitment__dependency_trap_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(income_support_dependency_trap_tr_t2015, income_support_commitment__dependency_trap_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(income_support_dependency_trap_tr_t2025, income_support_commitment__dependency_trap_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(income_support_dependency_trap_be_t1975, income_support_commitment__dependency_trap_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(income_support_dependency_trap_be_t1985, income_support_commitment__dependency_trap_reading, base_extractiveness, 1985, 0.33).
narrative_ontology:measurement(income_support_dependency_trap_be_t1995, income_support_commitment__dependency_trap_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(income_support_dependency_trap_be_t2005, income_support_commitment__dependency_trap_reading, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement(income_support_dependency_trap_be_t2015, income_support_commitment__dependency_trap_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(income_support_dependency_trap_be_t2025, income_support_commitment__dependency_trap_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(income_support_dependency_trap_su_t1975, income_support_commitment__dependency_trap_reading, suppression_requirement, 1975, 0.22).
narrative_ontology:measurement(income_support_dependency_trap_su_t1985, income_support_commitment__dependency_trap_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(income_support_dependency_trap_su_t1995, income_support_commitment__dependency_trap_reading, suppression_requirement, 1995, 0.31).
narrative_ontology:measurement(income_support_dependency_trap_su_t2005, income_support_commitment__dependency_trap_reading, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(income_support_dependency_trap_su_t2015, income_support_commitment__dependency_trap_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(income_support_dependency_trap_su_t2025, income_support_commitment__dependency_trap_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.15).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, labor_market_activation_requirements).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, tax_progressivity_structure).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, administrative_bureaucracy_self_preservation).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel decomposes into three readings: dependency_trap (this story, tangled_rope, ε=0.42), freedom_floor (rope/mountain, low ε), targeting_efficiency (snare/tangled_rope, different victim structure). All three share the referent (the standing welfare arrangement) but instantiate different constraints with different ε, different beneficiary/victim assignments, different types. This reading's ε is moderate because it sees both coordination (poverty floor) and extraction (work disincentive, bureaucratic capture). The freedom_floor reading sees primarily coordination. The targeting_efficiency reading sees the universal design itself as the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, powerless, 0.92).
constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, organized, 0.75).
constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
