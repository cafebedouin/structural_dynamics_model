% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeted Income Support vs. Universal Distribution (Efficiency Reading)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel
 *   governing income support policy: the targeting-efficiency reading. The
 *   kernel is the commitment to 'income support for those in need,' and the
 *   question is whether 'need' should be assessed through means-testing and
 *   targeting, or distributed universally without assessment. This reading
 *   argues that targeting is efficient—resources reach those who need them
 *   most, preventing waste on those who do not need support. This claim is
 *   contested by the freedom-floor reading (unconditional support enables
 *   dignity and autonomy) and the dependency-trap reading (unconditional
 *   support should not be provided because it reduces work incentive). The
 *   constraint story generates the targeting-efficiency reading as a clean
 *   ε-invariant structure: high extractiveness (0.78), high suppression
 *   (0.71), moderate theater (0.42), claimed as snare. The deep-poverty
 *   recipients are both nominal beneficiaries (they receive targeted support)
 *   and actual victims (they lose $19,100 per household if UBI replaces
 *   targeted programs). The measurement series from 1965–2026 shows
 *   extractiveness and suppression rising over 60 years as programs
 *   multiplied, eligibility rules hardened, and the middle-income tax base
 *   consolidated its political support for targeted redistribution framed as
 *   'efficiency.'
 *
 * KEY AGENTS:
 *   - Deep-poverty recipients: trapped, powerless, receiving mean $31,100 in targeted benefits; would receive $12,000 under UBI funded by program cannibalization.
 *   - Program administrators: institutional agenda-setters controlling eligibility, verification, and rule-enforcement; have captured policy discourse through efficiency framing.
 *   - Middle-income tax base: organized beneficiaries who receive the political benefit of 'efficient' redistribution (visible outcomes, moral standing) without bearing the full cost of universalism.
 *   - UBI advocates: excluded from policy implementation; would argue targeting is surveillance and extraction, not efficiency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.78).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.71).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeted Income Support vs. Universal Distribution (Efficiency Reading)").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "economic/political/social").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'debe349f-22e2-4ff3-a922-c56739276eb9').
narrative_ontology:cs_kernel_codification('debe349f-22e2-4ff3-a922-c56739276eb9', fixed_text).
narrative_ontology:cs_authority_grounding('debe349f-22e2-4ff3-a922-c56739276eb9', extraction).
narrative_ontology:cs_interpretation_layer_present('debe349f-22e2-4ff3-a922-c56739276eb9').
narrative_ontology:cs_reading_relation('debe349f-22e2-4ff3-a922-c56739276eb9', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('debe349f-22e2-4ff3-a922-c56739276eb9', income_support_commitment__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('debe349f-22e2-4ff3-a922-c56739276eb9', foundational, demonstrated_need_efficiency_principle).
narrative_ontology:cs_axiom_status(demonstrated_need_efficiency_principle, holdable).
narrative_ontology:cs_axiom_grounding('debe349f-22e2-4ff3-a922-c56739276eb9', demonstrated_need_efficiency_principle, instrumental).
narrative_ontology:cs_axiom('debe349f-22e2-4ff3-a922-c56739276eb9', secondary, empirical_work_disincentive_prevention).
narrative_ontology:cs_axiom_status(empirical_work_disincentive_prevention, overridden).
narrative_ontology:cs_axiom_grounding('debe349f-22e2-4ff3-a922-c56739276eb9', empirical_work_disincentive_prevention, empirically_contingent).
narrative_ontology:cs_reference_frame('debe349f-22e2-4ff3-a922-c56739276eb9', fiscal_responsibility_via_demonstrated_need).
narrative_ontology:cs_drift_state('debe349f-22e2-4ff3-a922-c56739276eb9', contemporary_post_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('debe349f-22e2-4ff3-a922-c56739276eb9', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, program_administrators).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, middle_income_tax_base).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, deep_poverty_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, means_tested_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, deep_poverty_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, moderate_income_workers).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, fiscal_efficiency_doctrine).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, empirical_deservingness_assessment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Current recipients of targeted programs (SNAP, housing assistance, TANF, Medicaid) receiving mean benefits of $31,100 annually. Under a UBI scheme funded by cannibalizing these programs, they would receive a universal payment of approximately $12,000 annually, creating a net loss of $19,100 per household. They are trapped: unable to exit poverty without income support, unable to substitute one program form for another without legislative action, and politically powerless to block redistribution of their targeted resources to universal recipients. They nominally 'benefit' from universalism rhetoric but bear the actual extraction cost.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, deep_poverty_recipients, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, deep_poverty_recipients, beneficiary).

% Workers earning $35,000–$75,000 annually who bear both income taxes funding current targeted programs AND would bear UBI funding taxes. Under targeted programs, they subsidize the poorest. Under UBI funded by consolidating targeted programs, their tax burden might remain stable or rise, while they receive a universal payment insufficient for their needs and receive no targeted assistance. Their exit options are constrained: relocation to lower-tax jurisdictions is possible but costly; tax avoidance is available to some but not most.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, moderate_income_workers, payer,
    moderate, biographical, constrained, national).

% Federal, state, and local bureaucracies administering targeted programs (HHS, state welfare departments, local housing authorities). They argue that needs-based programs efficiently direct resources to those with the greatest need; they defend targeting mechanisms (asset tests, income verification, program-specific eligibility) as necessary for fiscal discipline and fraud prevention. They control the rules, set eligibility thresholds, and administer the verification machinery. A shift to UBI would eliminate their administrative domain and discretionary authority. They have institutional inertia and access to policy-making venues.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% The tax-paying class ($75,000–$150,000+) who fund social programs. Under targeted programs, their tax burden is justified by visible outcomes in the poorest communities and their ability to claim moral standing ('we help those most in need'). Under UBI, their tax burden would rise to universal levels, but they would receive the same nominal benefit as the poorest ($12,000 or similar). The constraint benefits them by allowing them to feel their taxes are efficiently used and democratically accountable; it maintains political consent for redistribution by proving it reaches the intended targets.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, middle_income_tax_base, beneficiary,
    organized, generational, mobile, national).

% Advocates for unconditional universal basic income who are excluded from the conversation by the targeting-efficiency reading's framing. They would argue that targeting creates surveillance, stigma, and administrative waste; that universalism is simpler, more dignified, and enables genuine freedom; that the 'efficiency' frame is cover for class-based deservingness judgments. They lack institutional standing in social-service bureaucracies and have limited influence over program design absent major legislative shifts.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_policy_advocates, excluded,
    moderate, biographical, constrained, national).

% Academic economists, policy analysts, and evaluation researchers who study program outcomes, cost-effectiveness, and behavioral responses. They provide evidence on whether targeted programs reach the poorest, whether universal programs create work disincentives, whether administrative overhead is large relative to benefits transferred, and whether means-testing causes stigma-driven non-take-up. Their findings are cited by both targeting and universalism advocates; they take no institutional position but provide the evidentiary terrain on which the constraint is contested.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, empirical_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, program_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Targets scarce public resources toward those with the highest need, avoiding waste on universal payments to those who do not need support. Solves the coordination problem of distinguishing genuine need from preference or circumstance, using means-testing and asset verification to allocate within a budget constraint.
% TRANSFER_FUNCTION: Transfers income from tax-paying workers to the poorest households, but specifically to those meeting needs-based criteria. Also transfers administrative discretion from democratic legislatures to bureaucratic program managers who set and enforce eligibility rules. Under the efficiency reading, this discretion is legitimate because it directs resources efficiently; under the universalism reading, it is illegitimate because it creates surveillance and stratification.
% ABSENT_VOICES: Advocates for universal basic income and unconditional support are excluded from policy implementation because they reject the core premise that need should be assessed and verified. They would argue that targeting itself is the extraction mechanism, and that universalism is not wasteful but liberatory. Non-participants in targeted programs (the 'non-take-up' population) who avoid means-tested assistance due to stigma or administrative burden are also absent, though their absence is itself evidence some claim the targeting constraint extracts.
% DISAPPEARANCE_RATIONALE: If the targeting requirement disappeared and income support became unconditional and universal, the budget would either expand (to maintain current recipients' benefit levels), contract (leaving current recipients worse off), or redistribute (cannibalizing targeted programs to fund universal payments). In all cases, the poorest households' material conditions would shift, labor supply incentives would change, and bureaucratic authority over eligibility would dissolve. The current arrangement's persistence depends on the targeting rule itself; removing it reorganizes the entire income-support sector.
% FOUNDING_PROBLEM: In the 1960s-1980s, concerns that unconditional welfare payments would create work disincentives and dependency, and that resources should be directed to those with the greatest demonstrated need rather than distributed universally. The founding problem was the claim that universal income support would be fiscally unsustainable and behaviorally harmful.
% FOUNDING_PROBLEM_CORROBORATION: The efficiency reading asserts the founding problem is still live: means-testing is necessary to prevent waste and work disincentive. However, policy researchers and international evidence contradict this: (1) Natural experiments and evaluations from the 1990s onward (negative income tax trials, EITC studies, universal child allowance outcomes in Canada and Germany) consistently show unconditional or nearly-unconditional support does not eliminate work participation. (2) Non-take-up rates in means-tested programs suggest stigma and administrative burden suppress uptake, reducing efficiency. (3) Countries with universal child allowances (Canada, Australia, much of Europe) maintain stable or higher labor-force participation rates than the U.S. The founding behavioral problem is empirically dead. Corroborating sources OUTSIDE the benefiting program-administrator class: World Bank poverty evaluations, OECD social policy reviews, academic meta-analyses of conditional vs. unconditional transfer programs, and longitudinal surveys of program leavers all support that the behavioral founding problem does not materialize in practice.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at present) because the constraint permits the middle-income tax base to determine eligibility for the poorest, collecting the political benefit of appearing to prevent waste (moral standing, electoral support) while the poorest bear the cost of verification, asset testing, and reduced benefits if UBI replaces targeting. The measurement series shows a 60-year trajectory of rising extraction: the founding problem (work disincentive from unconditional support) had modest empirical support in the 1960s (extractiveness 0.42), but as the evidence accumulated showing universal programs do not eliminate work participation, the constraint's persistence has relied increasingly on political and bureaucratic inertia rather than behavioral evidence. Suppression is high (0.71) because means-testing is actively enforced: eligibility verification, asset discovery, fraud investigation, and sanctioning for non-compliance are the machinery that sustains the constraint. Theater is moderate-low (0.42) because the efficiency narrative still holds some truth—administrative overhead is real—but a growing share of the rule set (asset caps, time limits) is justified by dependency-trap reasoning (work disincentive), not efficiency. The theater ratio rises across the interval as the efficiency frame becomes increasingly disconnected from the empirical evidence.
 *
 * PERSPECTIVAL GAP:
 *   The deep-poverty recipients and program administrators compute dramatically different types from this constraint. For recipients, the constraint is extractive and coercive: they bear suppression (verification, sanctions), lose options (cannot substitute universal for targeted), and are trapped (powerless to negotiate the rules). From their seat, this computes as snare. From the administrator seat, the constraint is genuine coordination: they solve a real problem (directing finite resources to greatest need), they manage the verification machinery to prevent fraud, and they defend the rule as rational and evidence-based. From their seat, this might compute as tangled_rope (coordination + extraction). The engine computes each seat's type from the structural data; the authored claim does not adjudicate. The gap is the point—the constraint looks like coordination to those running it and like extraction to those living under it.
 *
 * DIRECTIONALITY LOGIC:
 *   Deep-poverty recipients (powerless, trapped exit) are the targets: they bear suppression, lose alternatives, and are extracted from (d near 1.0). Program administrators (institutional power, arbitrage exit) are positioned as beneficiaries: they control the rule-set, face no exit cost (can always administer a different program), and benefit from the legitimacy the efficiency frame provides (d near 0.0). The middle-income tax base (organized power, mobile exit) is positioned as partial beneficiary: they receive political benefit (moral standing via visible targeting) but pay taxes that fund both the targeted programs and could fund UBI (d near 0.5, but pulled beneficiary-ward by the political benefit of appearing efficient and caring). The directionality overrides are not needed for this story because the structural derivation (power + exit + beneficiary/victim declarations) produces the right relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (work disincentive from unconditional support) was structurally live in the 1960s when behavioral evidence was scarce and welfare expansion was rapid. By the 1990s, multiple natural experiments and evaluations had shown that unconditional or nearly-unconditional support (negative income tax, earned income tax credit, universal child allowances in other countries) did not eliminate work participation. The problem is now dead in empirical terms: the behavioral premise is not supported by evidence. Yet the targeting constraint persists, stronger than ever (extractiveness rose from 0.42 to 0.78; suppression from 0.48 to 0.71). This is a textbook mandatrophy: the founding problem died, but the institutional structure and the political coalition supporting targeting have hardened. The constraint persists because it benefits the middle-income tax base and administrators, not because it solves the behavioral problem it was built to address. The theater ratio (0.42 at present) reflects this: the constraint is increasingly performative. It performs 'efficiency' and 'fiscal responsibility' to maintain political consent for redistribution, but the efficiency claim no longer rests on evidence. This constraint is a candidate for mandatrophy resolution: the rule should be revisited in light of the dead founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    administrative_cost_vs_targeting_benefit,
    'What is the actual administrative overhead of means-testing and asset verification relative to the ''waste'' prevented by excluding non-needy recipients?',
    'Comparative cost accounting: full-burden administrative cost of targeted programs (eligibility verification, fraud investigation, appeal processing, data systems) vs. estimated benefit transfers to ineligible non-poor under a universal scheme. International comparison of administrative costs in countries with universal vs. targeted programs.',
    'If administrative overhead exceeds the prevented waste, the targeting constraint is theater covering extraction rather than genuine efficiency. If overhead is low relative to waste prevented, the constraint''s efficiency claim is validated and the snare classification shifts toward tangled_rope (genuine coordination function with extraction overhead).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_cost_vs_targeting_benefit, empirical, 'Whether means-testing administrative cost justifies the targeting requirement.').

omega_variable(
    stigma_and_behavioral_suppression,
    'Is the measured suppression (0.71) structural—coercive eligibility rules and asset tests—or partially internalized through stigma and shame that persist after program exit?',
    'Longitudinal survey of program leavers: measuring psychological distress, health outcomes, labor-force participation, and self-worth 2–5 years after exit. If suppression persists despite program exit, it is internalized; if it resolves, suppression is structural only.',
    'If suppression is partially internalized, the constraint''s effective extraction is higher than the structural measure (0.71) suggests, and the snare classification is stronger. If purely structural, fixing the constraint requires only changing the rule, not remedying psychological damage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_and_behavioral_suppression, empirical, 'Whether suppression is structural (rules) or internalized (shame, learned helplessness).').

omega_variable(
    kernel_reading_boundary,
    'Is the constraint ONLY instantiating the targeting-efficiency reading of the income-support kernel, or is it simultaneously instantiating the dependency-trap reading through its design (work-testing, time limits, asset caps)?',
    'Structural analysis of program rules: compare explicit justification for asset tests and work requirements (which reading is cited?), and trace genealogy of rule adoption (which era, which policy advocates). Identify which axioms are active in current rule-making.',
    'If both readings are baked into the current rule set, the constraint instantiates BOTH (dependency_trap + targeting_efficiency) simultaneously, making it a dual-kernel constraint. If only targeting_efficiency is operative, the constraint is a single reading as this story claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this constraint instantiates one or multiple readings of the income-support kernel simultaneously.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the measured extractiveness (0.78) the cost of coordination (allocating resources to the neediest within a budget), or is it primarily the rent collected by administrators and the middle-income tax base for maintaining power over the poorest?',
    'Counterfactual analysis: design a hypothetical universal program with the same total budget, and compare: (a) material outcomes for the poorest (better, worse, or same), (b) administrative overhead, (c) behavioral outcomes (labor, health, dignity). If the poorest are materially worse off AND the administrative overhead is lower under universalism, the extracted surplus is pure rent; if outcomes are similar or worse for the poorest even with lower overhead, some of the measured extraction funds genuine coordination.',
    'If extraction is primarily rent-seeking, the snare classification is confirmed. If extraction is coordination cost, the constraint should be reclassified as tangled_rope (genuine coordination + asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether extraction is a coordination cost or pure rent-seeking power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 1965, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t1965, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement_basis(inco_tr_t1965, observed).
narrative_ontology:measurement(inco_tr_t1985, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(inco_tr_t1985, observed).
narrative_ontology:measurement(inco_tr_t2000, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(inco_tr_t2000, observed).
narrative_ontology:measurement(inco_tr_t2012, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2012, 0.39).
narrative_ontology:measurement_basis(inco_tr_t2012, observed).
narrative_ontology:measurement(inco_tr_t2020, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(inco_tr_t2020, observed).
narrative_ontology:measurement(inco_tr_t2026, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(inco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t1965, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement_basis(inco_be_t1965, observed).
narrative_ontology:measurement(inco_be_t1985, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement_basis(inco_be_t1985, observed).
narrative_ontology:measurement(inco_be_t2000, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement_basis(inco_be_t2000, observed).
narrative_ontology:measurement(inco_be_t2012, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2012, 0.71).
narrative_ontology:measurement_basis(inco_be_t2012, observed).
narrative_ontology:measurement(inco_be_t2020, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement_basis(inco_be_t2020, observed).
narrative_ontology:measurement(inco_be_t2026, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(inco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t1965, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement_basis(inco_su_t1965, observed).
narrative_ontology:measurement(inco_su_t1985, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement_basis(inco_su_t1985, observed).
narrative_ontology:measurement(inco_su_t2000, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement_basis(inco_su_t2000, observed).
narrative_ontology:measurement(inco_su_t2012, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement_basis(inco_su_t2012, observed).
narrative_ontology:measurement(inco_su_t2020, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(inco_su_t2020, observed).
narrative_ontology:measurement(inco_su_t2026, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(inco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__targeting_efficiency_reading, 0.18).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the income_support_commitment kernel. The sibling readings are: freedom_floor_reading (unconditional support enables autonomy and dignity), and dependency_trap_reading (unconditional support creates work disincentive). All three readings instantiate different ε values and different beneficiary/victim structures from the same underlying commitment to income support. The targeting-efficiency reading claims ε is high (0.78, snare) because targeting concentrates extraction on the poorest; the freedom-floor reading claims ε is moderate (coordination with administrative overhead); the dependency-trap reading claims ε is high (behavioral extraction via work disincentive). Each reading is a structurally distinct constraint and should be compiled as a separate story, linked via affects_constraints to show the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
