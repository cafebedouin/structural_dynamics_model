% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/labor_economics/social_policy
 *
 * SUMMARY:
 *   Unconditional income support, when layered over low-wage labor markets,
 *   can function as a subsidy to employers rather than liberation for
 *   workers. This reading holds that the arrangement: (1) solves a real
 *   coordination problem (maintaining labor supply to low-wage sectors), (2)
 *   transfers nominal income to workers, but (3) captures much of that
 *   transfer through wage suppression — employers reduce wages precisely
 *   because they know workers' subsistence is guaranteed by the state. The
 *   constraint operates as tangled_rope: genuine coordination (labor supply)
 *   accompanied by asymmetric extraction (wage suppression captured by
 *   employers). This is one reading of a contested kernel
 *   (income_support_conditionality) where different framings emphasize
 *   different structural dynamics. The claim/metric gap is intentional: the
 *   constraint is claimed as tangled_rope while the metrics are authored from
 *   what this reading describes as empirically true of operation.
 *
 * KEY AGENTS:
 *   - low_wage_workers: structural payers; subsistence dependent on unconditional income; wage-suppression victims
 *   - low_wage_employers: beneficiaries; can pay below-subsistence wages due to state subsidy; wage-setting power concentrated
 *   - state_fiscal_authority: agenda-setter; administers the support; secondarily benefits from reduced wage-inflation and maintained labor supply
 *   - taxpayers: diffuse payers; fund the program; mostly unaware of employer-capture mechanism
 *   - labor organizing movements: payers (collective leverage reduced); constrained exit
 *   - competing jurisdictions: excluded (cannot unilaterally break the race-to-the-bottom dynamic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.62).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/labor_economics/social_policy").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'c7837b57-7d43-40f0-814c-1be17cb43d8f').
narrative_ontology:cs_kernel_codification('c7837b57-7d43-40f0-814c-1be17cb43d8f', distributed).
narrative_ontology:cs_authority_grounding('c7837b57-7d43-40f0-814c-1be17cb43d8f', extraction).
narrative_ontology:cs_reading_relation('c7837b57-7d43-40f0-814c-1be17cb43d8f', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7837b57-7d43-40f0-814c-1be17cb43d8f', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('c7837b57-7d43-40f0-814c-1be17cb43d8f', foundational, support_captures_wages_through_employer_adjustment).
narrative_ontology:cs_axiom_status(support_captures_wages_through_employer_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('c7837b57-7d43-40f0-814c-1be17cb43d8f', support_captures_wages_through_employer_adjustment, empirically_contingent).
narrative_ontology:cs_axiom('c7837b57-7d43-40f0-814c-1be17cb43d8f', secondary, state_benefits_from_suppressed_wage_inflation).
narrative_ontology:cs_axiom_status(state_benefits_from_suppressed_wage_inflation, holdable).
narrative_ontology:cs_axiom_grounding('c7837b57-7d43-40f0-814c-1be17cb43d8f', state_benefits_from_suppressed_wage_inflation, instrumental).
narrative_ontology:cs_reference_frame('c7837b57-7d43-40f0-814c-1be17cb43d8f', unconditional_income_support_as_worker_liberation).
narrative_ontology:cs_drift_state('c7837b57-7d43-40f0-814c-1be17cb43d8f', contemporary_labor_market_stagnation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7837b57-7d43-40f0-814c-1be17cb43d8f', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, state_fiscal_authority).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, taxpayers_subsidizing_wages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, labor_organizing_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support that maintains their subsistence while accepting below-market wage employment. They experience the income support as enabling bare survival within the labor market; employers experience their acceptance of below-cost wages as subsidized labor. Their exit options are constrained: refusing work means losing the wage entirely while retaining only the unconditional payment (insufficient for non-poverty living); their identity as workers is fused with the constraint's operation — they are the demographic the support is nominally designed for, yet the support's structure allows their formal employment to be systematically underpaid.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, low_wage_workers, beneficiary).

% Systematically pay wages below the subsistence threshold they could not justify in an unsubsidized labor market, knowing workers will accept the arrangement because unconditional income fills the gap. They externalize the cost of worker reproduction to the state. Their ability to do so depends entirely on the income support's persistence — if unconditional payment is withdrawn, their labor costs would rise sharply or they would need to raise wages. They have exit options (relocate operations, mechanize, adjust business models) that low-wage workers lack.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    powerful, generational, mobile, national).

% Designs, funds, and administers the unconditional income support. Benefits by reducing direct wage-regulation and minimum-wage enforcement costs while maintaining labor-market participation (workers stay in employment rather than exiting to benefit dependency). Collects tax revenue that funds the program and is partly recirculated to employers indirectly through subsidy-enabled wage suppression that reduces inflation and maintains labor supply. Administers the program's rules and can adjust conditionality, rates, or targeting.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, state_fiscal_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, state_fiscal_authority, beneficiary).

% Fund the unconditional income support through taxation. They bear the fiscal cost of a program that, under this reading, functions as a transfer to employers rather than direct assistance to workers — the subsidy captures much of the nominal benefit. Their ability to exit is constrained by citizenship and the obligation to contribute; their awareness of the transfer dynamic varies widely (many do not recognize that subsidy is flowing to employers).
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers_subsidizing_wages, payer,
    organized, biographical, trapped, national).

% Face structural pressure from the arrangement: unionization becomes harder when income support absorbs wage pressure, workers have less collective leverage to demand higher wages (the safety net substitutes for wage floors), and employers can credibly threaten relocation without raising wages since the state will subsidize the remainder. Their power to raise wages through collective action is dampened by the existence of unconditional income support that allows workers to accept lower negotiated wages.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_organizing_movements, payer,
    moderate, generational, constrained, national).

% Advocate for unconditional income support on grounds that it decommodifies labor and enables exit from coercive work relationships. Under this reading, their intended purpose (freedom from labor-market discipline) is inverted: the support sustains coercive low-wage work by making subsistence-level employment tolerable. They experience this reading as a co-optation or subversion of their policy vision. Their analytical position allows them to critique the constraint without being trapped by its operation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, political_advocates_of_decommodification, observer,
    moderate, generational, analytical, national).

% Are excluded from the mechanism design: they cannot unilaterally adopt the same policy without competitive disadvantage (lower-wage jurisdictions attract low-wage employers seeking the most favorable subsidy-to-wage-suppression ratio; higher-wage jurisdictions face fiscal pressure to match or see employers relocate). International labor standards and capital mobility make this a structural exclusion — jurisdictions cannot easily exit the competitive dynamic the wage-subsidy arrangement creates.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, competing_jurisdictions, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real labor-supply problem: without some floor income, low-wage workers would exit the labor market entirely; unconditional support maintains a supply of workers to low-wage employers and keeps workers engaged with formal employment. It coordinates the state, employers, and workers around the maintenance of low-wage labor supply.
% TRANSFER_FUNCTION: Moves tax revenue from the broad taxpayer base to low-wage workers (nominally); under this reading's analysis, the actual transfer is to low-wage employers, who capture the support by adjusting wages downward — employers retain the subsidy as profit rather than passing it to workers as higher wages. Workers receive the unconditional payment but at the cost of accepting systematically lower market wages than they would negotiate without the support.
% ABSENT_VOICES: High-wage workers and unionized labor are structurally excluded from full participation: their wage-setting power is reduced by the state's wage-subsidy regime (why demand higher wages when the state will fill the gap for low-wage workers?). Workers in jurisdictions that do not offer unconditional support face a competitive disadvantage (their labor is more expensive to employers). Capital that has relocated to lower-wage jurisdictions does not participate in the domestic debate.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage employers would face immediate pressure to raise wages to subsistence levels or lose workers to non-participation; wage floors and/or collective bargaining power would likely rise sharply; labor organizing would regain leverage; the fiscal budget would shift from income support to other uses (or taxes would fall). The structure of low-wage labor markets would reorganize fundamentally — the arrangement's disappearance would rearrange the entire system.
% FOUNDING_PROBLEM: Low-wage workers could not afford subsistence on market wages alone; poverty persisted despite employment. Income support was created to solve the problem of worker destitution in the context of formal low-wage labor.
% FOUNDING_PROBLEM_CORROBORATION: Employers and state fiscal authorities attest the founding problem is still live and income support is necessary to maintain labor supply. Low-wage workers and labor economists analyzing wage data attest the founding problem has been masked rather than solved — wages have not risen to subsistence levels even with support in place, suggesting the support has been captured by employers. Comparative analysis from jurisdictions that have raised wages and reduced unconditional support (or withdrawn it) shows variable outcomes, with some showing labor-market adjustments and others showing persistent low-wage employment — corroboration from outside the benefiting parties is mixed.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the magnitude of captured wage subsidy: low-wage workers receive support but at the structural cost of accepting suppressed market wages. The support enables employers to externalize labor-reproduction costs to the state while capturing the subsidy through wage adjustment. Suppression at 0.62 reflects active enforcement: the arrangement persists only through continuous state administration of income support and through workers' identity-lock (they cannot easily refuse low-wage work without losing the wage income, and the identity of 'worker' is fused with the subsidy arrangement). Theater ratio at 0.41 is moderate: some genuine coordination function (labor supply is real), but increasing portions of state activity are devoted to maintaining the subsidy eligibility structure rather than verifying genuine need. Accessibility_collapse at 0.58: alternatives (refusal of low-wage work, unionization, exit to higher-wage jurisdictions) have partially collapsed — workers have fewer leverage points. Resistance at 0.72: substantial opposition from labor movements, workers' advocates, and competing-wage jurisdictions, but collective action is dampened by the support's existence. The measurement series tracks accumulating extractiveness and suppression over 25 years (an illustrative interval matching the policy tenure of many modern income-support regimes): as employers learn the subsidy persists, wage-suppression deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the state's agenda-setter position: the constraint appears as efficient anti-poverty policy (workers get income, employers get labor, fiscal cost is lower than direct wages). From low-wage workers' position: the constraint appears as coercive (they receive subsistence only by accepting systematically suppressed wages). From low-wage employers' position: the constraint appears as a sustained competitive advantage (they can undercut non-subsidizing competitors). From the labor organizing position: the constraint appears as a structural attack on collective power (the support replaces the need for wage negotiation). The engine computes these divergent classifications from the structural data (directionality, power asymmetries, exit options). The authored claim and the authored metrics reflect this reading's interpretation; the other readings (freedom_floor_reading, dependency_trap_reading) would author different metrics and different beneficiary/victim structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers are structural targets (d near 1.0): they receive nominal income but at the cost of suppressed market wages; their exit options are identity-locked (refusing work while retaining only subsistence-level support is not a viable exit; the identity of 'worker' is fused with accepting low wages). Low-wage employers are beneficiaries (d near 0.0): they pay suppressed wages enabled by the subsidy and have mobile exit options (relocate, mechanize, raise prices). The state fiscal authority is an agenda-setter capturing secondary benefit (d near 0.2): it administers the support and benefits from lower wage-inflation and maintained labor supply, but is not the primary extractor. Taxpayers are distributed payers (d varies: near-middle earners pay higher effective rates; high earners have arbitrage options; low-income taxpayers have identity_locked status similar to workers). The directionality computation should produce: workers d~0.75, employers d~0.15, state d~0.25, taxpayers d~0.60 (average, variable by income).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (worker destitution) and the constraint's persistence show potential mandatrophy: the founding problem is contested (is it solved? are wages rising? is the support adequate?), and the constraint's persistence depends on state administration, not on participant preference. Under this reading, if the founding problem were truly solved (wages rose to subsistence, poverty declined substantively), the constraint should be unnecessary — yet it persists and deepens. This suggests either: (1) the founding problem has not been solved and support is genuinely needed (supporting the constraint's mandate), or (2) the constraint's persistence serves other interests (employer subsidy, fiscal management, labor discipline) that are decoupled from the founding problem. The rising extraction and suppression measurements suggest (2): as the constraint matures, it appears to serve interests beyond the original mandate. Mandatrophy is not resolved but is present as a structural question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_adjustment_mechanism,
    'Do employers systematically reduce wages in response to the knowledge that workers'' subsistence is guaranteed by unconditional income support, or do wages adjust for other reasons (productivity, market competition, inflation)?',
    'Quasi-experimental analysis comparing wage dynamics in jurisdictions with/without unconditional support, controlling for economic conditions; employer interviews about wage-setting rationale; analysis of wage-setting behavior before/after the introduction of unconditional support in the same jurisdiction.',
    'If wage suppression is systematic and employer-attributed to the subsidy''s existence, the reading''s core claim is strongly supported and extraction classification is confirmed. If wages are set independently of subsidy knowledge, the extraction dynamic is weaker and the constraint''s type shifts toward rope. If wage suppression is offset by improved working conditions or benefits, extraction may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_adjustment_mechanism, empirical, 'Whether wage suppression is systematically caused by the support''s existence.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit status of low-wage workers a structural feature (they literally cannot refuse work and retain subsistence), or a cognitive/cultural phenomenon (they believe they must work even if subsistence is guaranteed)?',
    'Post-withdrawal behavioral analysis: if workers withdraw from low-wage labor when support is removed but not before (despite theoretical ability to do so), the lock is partly cognitive/cultural. Comparative analysis of workers in jurisdictions with guaranteed unconditional support vs. weak/absent support.',
    'If identity-lock is structural, suppression is higher and the constraint is more coercive. If identity-lock is cognitive/cultural, the constraint''s coerciveness is lower and workers have more effective agency. The distinction affects the classification''s power-dynamics component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether workers are structurally locked into low-wage labor or culturally bound by work identity.').

omega_variable(
    alternative_reading_possibility,
    'Could the same empirical facts (unconditional support + low-wage employment) be coherently framed as the freedom_floor_reading (support enabling exit from worst working conditions, workers choosing low-wage work freely) rather than the wage_subsidy_reading (support captured by employers, wages suppressed)?',
    'Hermeneutic analysis: can both readings survive the same empirical data? If yes, the difference is interpretive framework, not empirical fact. If the readings make divergent empirical predictions that can be tested, the readings are partially falsifiable (omega moves to conceptual+empirical hybrid).',
    'If both readings are empirically equivalent (same facts, different interpretation), the constraint type is underdetermined and the kernel genuinely contains incompatible readings. If the readings make different predictions that can be tested, empirical data can distinguish them. This affects whether mandatrophy is diagnosed as genuine ambiguity or as suppressed evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_possibility, conceptual, 'Whether this reading and the freedom_floor reading are empirically distinguishable or hermeneutically incommensurable.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.62) structural (workers have no real choice but to accept low wages given the income-support structure) or internalized (workers have choice but have internalized the belief that they must work, even at low wages)?',
    'Post-exit trajectory: if workers who exit the low-wage labor market (through support reduction, relocation, or deliberate refusal) show sustained suppression effects (belief they must work, difficulty refusing), suppression is partially internalized. If suppression dissolves after structural removal, it was structural. Comparative analysis of workers'' stated reasons for remaining in low-wage work.',
    'Structural suppression is an external barrier feature of the constraint; internalized suppression travels with the worker and persists even after exit. If substantially internalized, the constraint''s true suppressive power is higher than the structural measure (the target carries the suppression after leaving). If structural, removal of the constraint removes suppression immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural or internalized in low-wage workers.').

omega_variable(
    state_benefit_vs_employer_benefit,
    'Does the state genuinely benefit from the income-support regime (lower wage inflation, maintained labor supply, reduced direct wages it would otherwise need to pay), or does the state pay the full fiscal cost without secondary gain?',
    'Fiscal accounting: compare total public expenditure (unconditional support + foregone wage subsidies if the state employed the same workers) to private-sector wage savings under the regime. Compare inflation outcomes and labor-supply metrics with/without unconditional support, controlling for economic conditions.',
    'If the state captures secondary fiscal benefit, it is a true agenda-setter/secondary-beneficiary. If the state bears the full cost with no secondary benefit, the state''s role is purely administrative and the primary extraction is employer→worker (snare, not tangled_rope). This affects whether the constraint is truly tangled_rope (coordination + asymmetric extraction at multiple levels) or primarily snare with state as passive administrator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_benefit_vs_employer_benefit, empirical, 'Whether the state''s fiscal benefit from the regime is significant enough to sustain the agenda-setter claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(inco_tr_t3, income_support_conditionality__wage_subsidy_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(inco_tr_t6, income_support_conditionality__wage_subsidy_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(inco_tr_t18, income_support_conditionality__wage_subsidy_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__wage_subsidy_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(inco_be_t3, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(inco_be_t6, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(inco_be_t18, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(inco_su_t3, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(inco_su_t6, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(inco_su_t18, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 18, 0.61).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__wage_subsidy_reading, 0.18).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, minimum_wage_avoidance_through_subsidy).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, labor_market_wage_suppression_through_state_transfer).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the income_support_conditionality kernel. The kernel describes the standing political commitment to provide unconditional income support; different readings emphasize different structural dynamics: the wage_subsidy_reading (this constraint) emphasizes employer capture and wage suppression; the freedom_floor_reading emphasizes decommodification and positive freedom; the dependency_trap_reading emphasizes work-disincentive effects. Each reading produces a different constraint story with different beneficiary/victim structures, different ε values, and different claimed types. The three stories are linked via network.affects_constraints to document their kernel kinship. The sibling reading stories (freedom_floor, dependency_trap) are authored as separate JSON files with their own structural data and metrics. Empirical divergence between the readings' predictions can be resolved only by examining actual wage-setting behavior, workers' stated preferences, and comparative outcomes across jurisdictions with/without unconditional support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, powerless, 0.75).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, powerful, 0.12).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
