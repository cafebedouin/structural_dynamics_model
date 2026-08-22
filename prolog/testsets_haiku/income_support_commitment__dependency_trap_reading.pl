% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Work-Disincentive and Dependency Trap
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested
 *   income-support commitment kernel: the dependency-trap reading. Under this
 *   reading, unconditional income support is presented as solving a
 *   subsistence problem while operating as a mechanism that transfers
 *   extraction from working taxpayers to income-support recipients and,
 *   simultaneously, extracts from poor individuals by atrophying their human
 *   capital and locking them into non-participant identity. The reading is
 *   NOT neutral—it takes a structural position that extraction and
 *   skill-atrophy are the actual effects, even when the program is nominally
 *   framed as coordination and dignity. This story does not debate whether
 *   that reading is true; it models what the world looks like if the reading
 *   is adopted.
 *
 * KEY AGENTS:
 *   - unconditional_support_recipients_exiting_labor: Receive transfers; over time, identity-locked into non-participation as skills erode
 *   - working_taxpayers: Fund transfers through taxation; experience direct and indirect wage/price pressure from labor-supply shifts
 *   - poor_individuals_skill_atrophying: Initial beneficiaries of subsistence income; victims of skill erosion and human-capital degradation over time
 *   - policy_administrators: Set and defend the program; frame it as coordination and dignity
 *   - labor_market_participants_competing: Excluded; would demand conditionality or lower benefit levels if heard
 *   - human_capital_development_constituency: Observer seat; measures outcomes and trajectories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.58).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.42).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive and Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '638dcc1c-322f-4106-8c29-47c858f26ea6').
narrative_ontology:cs_kernel_codification('638dcc1c-322f-4106-8c29-47c858f26ea6', formalized).
narrative_ontology:cs_authority_grounding('638dcc1c-322f-4106-8c29-47c858f26ea6', extraction).
narrative_ontology:cs_interpretation_layer_present('638dcc1c-322f-4106-8c29-47c858f26ea6').
narrative_ontology:cs_reading_relation('638dcc1c-322f-4106-8c29-47c858f26ea6', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('638dcc1c-322f-4106-8c29-47c858f26ea6', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('638dcc1c-322f-4106-8c29-47c858f26ea6', foundational, unconditional_income_work_disincentive_necessary).
narrative_ontology:cs_axiom_status(unconditional_income_work_disincentive_necessary, holdable).
narrative_ontology:cs_axiom_grounding('638dcc1c-322f-4106-8c29-47c858f26ea6', unconditional_income_work_disincentive_necessary, empirically_contingent).
narrative_ontology:cs_axiom('638dcc1c-322f-4106-8c29-47c858f26ea6', foundational, skill_atrophy_locks_recipients_into_dependence).
narrative_ontology:cs_axiom_status(skill_atrophy_locks_recipients_into_dependence, holdable).
narrative_ontology:cs_axiom_grounding('638dcc1c-322f-4106-8c29-47c858f26ea6', skill_atrophy_locks_recipients_into_dependence, empirically_contingent).
narrative_ontology:cs_reference_frame('638dcc1c-322f-4106-8c29-47c858f26ea6', poverty_reduction_through_universal_transfer).
narrative_ontology:cs_drift_state('638dcc1c-322f-4106-8c29-47c858f26ea6', mature_implementation_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('638dcc1c-322f-4106-8c29-47c858f26ea6', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, unconditional_support_recipients_exiting_labor).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophying).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophying).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional cash transfers sufficient to meet subsistence needs without engaging in labor. The constraint transfers income in their direction without requiring participation in the tax base's productive cycle. Over time, as labor market participation declines and skills atrophy from non-use, their structural position solidifies: leaving the program means re-entering a labor market they have been absent from, which their atrophied skills make difficult. They are locked into the recipient role not by force but by the identity fusion (non-participant) and the opportunity cost of rebuilding skills.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, unconditional_support_recipients_exiting_labor, beneficiary,
    powerless, biographical, identity_locked, national).

% Fund the unconditional transfers through taxation. They bear the extraction directly as diverted income; they also bear it indirectly through wage pressure (if unconditional income changes labor supply, wages in non-tradeable sectors may fall as labor supply expands into lower-wage work, or remain flat while prices rise). Their exit option is constrained by the geographic scope of the tax regime and their immobility; exit via tax arbitrage (relocation) is possible but costly. They experience the constraint as a forced transfer from productive to non-productive population.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    moderate, biographical, constrained, national).

% May initially enter the unconditional support system by choice (to escape grinding poverty or unstable employment), but as they remain outside the labor market and their skills deteriorate, they become trapped in a low-skill, low-wage category even when (or if) they seek to re-enter labor. The constraint extracts from them by degrading their human capital and market position, even though the immediate transfer addresses their subsistence need. They are identity-locked by the erosion of marketable skills and the self-concept of 'non-worker' that unconditional income permits to crystallize.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophying, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophying, beneficiary).

% Design, implement, and administer the unconditional income program. They set the transfer level, determine eligibility (often universal or near-universal by design), and manage the cash flow. They justify the program as reducing poverty and supporting human dignity. From this seat, the constraint is presented as a coordination solution: solve the problem of subsistence income universally rather than through means-tested bureaucracy. Their exit is to restructure or terminate the program, which they can do unilaterally if political conditions permit.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, policy_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Workers and employers in sectors with elastic labor supply (service industries, low-wage employment) would have a strong voice in whether unconditional income is high enough to depress labor participation and wages in their sectors. They are excluded from the design decision because the program is typically framed as individual-centered (rights/dignity) rather than as a labor-market intervention. If consulted, they would argue for conditionality (work-requirement tie-ins) or lower transfer levels.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_market_participants_competing, excluded,
    organized, biographical, constrained, national).

% Economists, labor-policy advocates, and workforce-development specialists who measure skill formation, earnings trajectories, and labor-force participation. They observe the constraint from the seat of technical measurement: is the program delivering the claimed benefit of autonomy without the cost of skill erosion, or is erosion happening? They take data from the other seats' experiences and analyze trajectories.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, human_capital_development_constituency, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, policy_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unconditional income support solves the coordination problem of subsistence provision by centralizing redistribution: one universal program in place of fragmented means-tested bureaucracy, reducing stigma and transaction costs. Under this reading, the coordination is real but subordinate; the primary function that drives the constraint's persistence is extraction from the productive to the non-productive.
% TRANSFER_FUNCTION: Transfers cash income from the tax base (working population) to unconditional-support recipients, with side effects of skill atrophy and labor-market exit. The magnitude and structure of the transfer is not calibrated to the cost of the coordination function itself; the transfer magnitude reflects political choice and ideological commitment to universality, not service cost.
% ABSENT_VOICES: Labor-market participants in elastic-demand sectors (service, agriculture, light manufacturing) are structurally excluded from the design conversation. They would argue that unconditional income at a level high enough to address poverty disincentivizes their sector's labor supply and compresses wages. Employers in those sectors likewise would demand conditionality or lower benefit levels. Individual recipients whose skills are atrophying may not fully perceive or articulate the long-term opportunity cost until they attempt re-entry, so their voice at the design stage is not an informed dissent.
% DISAPPEARANCE_RATIONALE: If unconditional income support at current levels vanished overnight, labor-force participation would rise within months as non-workers returned to the labor market out of necessity; wage structure would adjust (upward pressure in labor-scarce sectors, downward in elastic sectors); individuals who had exited would face re-entry costs (retraining, certification, network rebuilding). The constraint's disappearance would reorganize the entire labor market and income distribution.
% FOUNDING_PROBLEM: Persistent poverty in advanced economies, and the inadequacy and stigma of means-tested welfare systems that trap recipients in low-benefit traps and require invasive eligibility verification.
% FOUNDING_PROBLEM_CORROBORATION: Policy advocates and some labor economists attest that inadequate welfare and poverty remain live problems; critics and conservative economists attest that the founding problem has been substantially addressed by existing targeted programs and that unconditional income is an over-broad response that creates new problems (dependency, skill atrophy, work disincentive) rather than solving the original one. The contest is carried in peer-reviewed labor economics literature and policy institutes outside the administration itself.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (initial design phase, program framed as anti-poverty) to 0.58 (mature implementation, labor-supply and skill effects accumulated). The measurement trajectory captures the diagnosis inherent in this reading: extractiveness is LOW at launch because the program is new and framed as redistribution, but RISES as non-participation accumulates and skill atrophy becomes measurable. Theater_ratio starts low (0.18) because genuine poverty-reduction coordination is present, but rises to 0.31 as administrative focus shifts toward managing non-workers and defending the transfer against labor-supply criticism. Suppression_requirement is MODERATE and stable (0.35-0.43) because the constraint does not depend on violent coercion—it operates through identity-locking and opportunity-cost structures. The rise in suppression from t0 to t18 reflects the increasing need to suppress both labor-market pressure (workers and employers demanding conditionality) and the internal contradiction (recipients' atrophied skills making re-entry impossible). The slight decline from t18 to t25 reflects projection uncertainty (if the program reaches policy crisis, suppression might modulate downward as the system enters contestation).
 *
 * PERSPECTIVAL GAP:
 *   The policy administrator seat experiences the constraint as coordination (universal provision, administrative simplicity, dignity). The working-taxpayer seat experiences it as extraction (income transfer, labor-supply compression). The skill-atrophying poor experience it as initial relief shifting into human-capital trap. The engine computes these divergences from the structural data (power, exit_options, role) and the directional atoms (beneficiary/payer declarations). The claim/metric independence rule applies: we claim tangled_rope (the expected type for coordination + extraction + enforcement) and we author metrics that describe the actual extractiveness trajectory; the engine measures their alignment or divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients exiting labor are structural beneficiaries (receive income transfer, low power, identity-locked—directionality near full-benefit end, d ≈ 0.1-0.2). Working taxpayers are structural payers (fund the transfer, constrained exit but organized power—directionality toward target end, d ≈ 0.7-0.8). Poor individuals whose skills atrophy are BOTH beneficiaries initially (income transfer) and payers over the interval (human-capital loss, labor-market disadvantage accumulates—directionality complex, starting near symmetric ~0.5 and drifting toward target ~0.7-0.8 as atrophy crystallizes). The constraint's active enforcement (suppression_requirement rises from 0.35 to 0.43) reflects the administrative burden of managing labor-supply exit and defending against labor-market pressure—the administration must actively suppress both external criticism (labor-market participants) and internal contradictions (recipients whose atrophied skills make program exit harder).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids conflating coordination with pure extraction by explicitly naming the dual structure: YES, unconditional income provides real coordination (universal provision, reduced stigma, simpler administration vs. means-tested alternatives). AND YES, the same mechanism operates as extraction (transfer from productive to non-productive, skill erosion, identity-locking). The tangled-rope classification holds both in tension—the rope part is genuine, the extraction part is also genuine, and they are sustained by the same enforcement structure (administration prevents splitting them, suppresses labor-market feedback, maintains the universality claim even as outcomes diverge). If mandatrophy were to occur, it would manifest as the founding problem (poverty, inadequate welfare) either disappearing (in which case the constraint persists without its original justification, becoming piton) or being transparently displaced by the new problem (skill atrophy, work disincentive), in which case the claim of tangled_rope fails and the constraint reclassifies as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_atrophy_mechanism_ambiguity,
    'Is the measured skill atrophy a direct causal effect of unconditional income (individuals rationally exit labor market, skills decay from disuse), or an artifact of selection (those who exit are already lower-skill or have lower preference for work, so observed skill decline reflects composition change rather than degradation)?',
    'Longitudinal wage-trajectory analysis comparing individuals who received unconditional income with matched control group; measurement of skill-specific decline (language, technical, interpersonal) vs. composition shift. Randomized controlled trials with skill measurement at baseline and follow-up.',
    'If atrophy is direct causal effect, the constraint''s extraction from individual human capital is real. If atrophy is selection effect, the extraction is from tax base to recipients, but the recipients'' skill loss reflects their pre-existing position, not the program''s mechanism—the classification shifts toward pure rope (coordination with transfer, no extraction from individuals'' capabilities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_mechanism_ambiguity, empirical, 'Whether skill decline is causal effect of non-participation or selection effect of who exits.').

omega_variable(
    labor_supply_elasticity_to_unconditional_income,
    'How elastic is labor supply to unconditional income? What percentage of recipients exit the labor force as a direct result of the transfer, vs. what percentage would have exited anyway (compositional effect) or remain in marginal attachment?',
    'Comparative analysis of labor-force participation before and after program introduction; quasi-experimental designs exploiting differential eligibility or benefit levels; survey and administrative data on work hours conditional on receipt.',
    'High elasticity (many people exit in direct response) makes extraction from the tax base large and the work-disincentive reading strong; low elasticity makes the extraction modest and the program closer to pure redistribution without substantial labor-market distortion. The measured extractiveness (0.58 at maturity) assumes moderate-to-high elasticity; lower elasticity would lower the ε value materially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_supply_elasticity_to_unconditional_income, empirical, 'Strength of work-disincentive effect in response to unconditional income level.').

omega_variable(
    beneficiary_class_homogeneity_assumption,
    'Are unconditional-income recipients a homogeneous group (idle non-workers subsidized by taxpayers), or a heterogeneous population including caregivers, disabled individuals, students, and those in between jobs, whose exit from formal labor is not irrational given their constraints?',
    'Demographic analysis of recipient population by age, ability, caregiving responsibilities, prior employment history, and stated reasons for non-participation. Qualitative research on recipients'' own framing of their relationship to the program.',
    'If recipients are heterogeneous, the victim/beneficiary structure becomes more ambiguous: caregivers and disabled individuals are solving real coordination problems (their care work is socially valuable but unpaid; unconditional income enables it) rather than extracting. The classification might shift toward rope with multiple coordination functions rather than tangled_rope. If recipients are predominantly able-bodied and work-capable, the extraction frame holds stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_class_homogeneity_assumption, empirical, 'Whether beneficiary population is homogeneous ''idle non-workers'' or heterogeneous mix with diverse participation constraints.').

omega_variable(
    reading_kernel_ambiguity,
    'Is the income-support commitment fundamentally about anti-poverty (the founding_problem in this reading), or about labor-market decommodification and enabling choice (the freedom_floor reading''s premise), or about optimal targeting of limited resources (the targeting_efficiency reading)? These map to different kernels: poverty-as-problem vs. commodification-as-problem vs. resource-scarcity-as-problem.',
    'Historical analysis of the commitment''s origin texts, policy debates, and administrative framing at establishment. Examination of which problem statement dominated the founding discourse and which has persisted across administrations.',
    'If anti-poverty is the true founding problem and poverty remains live, the dependency-trap reading''s claim that the founding problem is addressed is falsified, and the constraint looks closer to piton (persistent mechanism divorced from function). If decommodification is the true founding commitment, the dependency-trap reading misframes the constraint entirely—skill atrophy might be side effect, not the primary extraction mechanism; the classification could shift to rope (the extraction is a side effect, not the purpose). If resource-scarcity is the frame, the constraint might be tangled_rope from a different angle (transfer + rationing, not transfer + work-disincentive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Which problem (poverty, commodification, resource-scarcity) does the income-support commitment actually address? Reading-choice determines constraint boundaries.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.35-0.43) structural suppression (government actively defending the program against labor-market pressure and policy alternatives), or internalized suppression (recipients'' self-concept as non-workers, identity fusion with the status, internalized belief that re-entry is impossible)?',
    'Post-program-exit trajectory studies: if individuals who leave the program maintain suppression patterns (continued non-work, low labor-market engagement), suppression is substantially internalized. Policy debate and media framing analysis: is the program defended on principle (autonomy, dignity) or on pragmatics (too costly to shut down)? Both would indicate different suppression types.',
    'If structural, the suppression fades if the program ends and labor demand returns. If internalized, the suppression persists—individuals remain trapped in non-participant identity even after the program''s structural support is gone. Internalized suppression indicates the constraint''s extraction is deeper than immediate income transfer; the damage to human capital and self-conception is durable. This would support a higher effective_extraction valuation (χ scaling) over longer time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Is suppression of labor-market participation structural or internalized in recipients'' identity?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(inco_tr_t0, projected).
narrative_ontology:measurement(inco_tr_t3, income_support_commitment__dependency_trap_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(inco_tr_t3, observed).
narrative_ontology:measurement(inco_tr_t6, income_support_commitment__dependency_trap_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(inco_tr_t6, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__dependency_trap_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t18, income_support_commitment__dependency_trap_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(inco_tr_t18, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__dependency_trap_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(inco_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(inco_be_t0, projected).
narrative_ontology:measurement(inco_be_t3, income_support_commitment__dependency_trap_reading, base_extractiveness, 3, 0.47).
narrative_ontology:measurement_basis(inco_be_t3, observed).
narrative_ontology:measurement(inco_be_t6, income_support_commitment__dependency_trap_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(inco_be_t6, observed).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__dependency_trap_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t18, income_support_commitment__dependency_trap_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement_basis(inco_be_t18, observed).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__dependency_trap_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(inco_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(inco_su_t0, projected).
narrative_ontology:measurement(inco_su_t3, income_support_commitment__dependency_trap_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement_basis(inco_su_t3, observed).
narrative_ontology:measurement(inco_su_t6, income_support_commitment__dependency_trap_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(inco_su_t6, observed).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__dependency_trap_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(inco_su_t12, observed).
narrative_ontology:measurement(inco_su_t18, income_support_commitment__dependency_trap_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement_basis(inco_su_t18, observed).
narrative_ontology:measurement(inco_su_t25, income_support_commitment__dependency_trap_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(inco_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the income_support_commitment kernel family (three readings: dependency_trap_reading, freedom_floor_reading, targeting_efficiency_reading). All three share the same standing arrangement (unconditional cash transfers) but frame it differently, instantiating different ε values and victim/beneficiary structures. The dependency_trap reading positions the same arrangement as extractive (ε = 0.58, beneficiary-payer asymmetry); the freedom_floor reading would position it as coordination with dignity benefits (lower ε, symmetric or inverted directionality); the targeting_efficiency reading would reframe the kernel as over-broad and advocate calibration. Each story is a distinct constraint with distinct metrics and stakeholders; they are linked via this network field and related in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
