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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/labor/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the wage_subsidy_reading of the
 *   income_support_conditionality kernel: unconditional income support (UBI
 *   or basic income) enables low-wage employers to pay below-subsistence
 *   wages by knowing the public subsidy fills the gap. Workers receive
 *   support that keeps them from destitution but lose the bargaining leverage
 *   they would have if employers could not capture the subsidy as suppressed
 *   wages. The arrangement is tangled: a genuine coordination function
 *   (matching workers to necessary low-wage work without coercion) coexists
 *   with asymmetric extraction (employers capture part of the public transfer
 *   as lower wage costs). This reading contests the freedom_floor_reading
 *   (which holds that income support decommodifies labor and enables worker
 *   exit) and the dependency_trap_reading (which holds that support
 *   undermines work incentives). The wage_subsidy_reading asserts that
 *   support persists precisely because it subsidizes low-wage labor, not
 *   because it liberates workers or degrades their motivation.
 *
 * KEY AGENTS:
 *   - low_wage_employers: institutional agenda-setter and primary beneficiary; capture subsidy as suppressed wage costs
 *   - low_wage_workers: powerless victims; trapped between subsistence wages and precarious employment; receive support that does not increase bargaining power
 *   - high_productivity_firms: powerful payers; bear tax burden without capturing subsidy
 *   - state_fiscal_authority: institutional agenda-setter; sets support level and program rules
 *   - labor_bargaining_coalitions: organized payers and observers; contest whether support enables exit or institutionalizes low-wage labor
 *   - excluded_precariat: structurally absent; experience wage suppression without compensating subsidy; anchor floor downward
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.52).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/labor/social_policy").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '76e72980-e846-4a5a-b832-a2be0d3f5018').
narrative_ontology:cs_kernel_codification('76e72980-e846-4a5a-b832-a2be0d3f5018', formalized).
narrative_ontology:cs_authority_grounding('76e72980-e846-4a5a-b832-a2be0d3f5018', extraction).
narrative_ontology:cs_interpretation_layer_present('76e72980-e846-4a5a-b832-a2be0d3f5018').
narrative_ontology:cs_reading_relation('76e72980-e846-4a5a-b832-a2be0d3f5018', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('76e72980-e846-4a5a-b832-a2be0d3f5018', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('76e72980-e846-4a5a-b832-a2be0d3f5018', foundational, income_support_enables_employer_subsidy).
narrative_ontology:cs_axiom_status(income_support_enables_employer_subsidy, holdable).
narrative_ontology:cs_axiom_grounding('76e72980-e846-4a5a-b832-a2be0d3f5018', income_support_enables_employer_subsidy, empirically_contingent).
narrative_ontology:cs_axiom('76e72980-e846-4a5a-b832-a2be0d3f5018', secondary, wage_suppression_through_subsidy_capture_measurable).
narrative_ontology:cs_axiom_status(wage_suppression_through_subsidy_capture_measurable, holdable).
narrative_ontology:cs_axiom_grounding('76e72980-e846-4a5a-b832-a2be0d3f5018', wage_suppression_through_subsidy_capture_measurable, empirically_contingent).
narrative_ontology:cs_reference_frame('76e72980-e846-4a5a-b832-a2be0d3f5018', unconditional_income_support_as_worker_liberation).
narrative_ontology:cs_drift_state('76e72980-e846-4a5a-b832-a2be0d3f5018', contemporary_subsidy_capture_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('76e72980-e846-4a5a-b832-a2be0d3f5018', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, capital_intensive_firms).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, precariat_labor_force).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, high_productivity_firms).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, labor_bargaining_coalitions).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, labor_market_clearing_through_subsidy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Firms in retail, food service, hospitality, care, and agriculture that employ workers at wages below subsistence levels (defined as the amount needed for autonomous living without public assistance). The unconditional income support allows them to pay below this threshold, knowing that public funds will bridge the gap to subsistence. They advocate for generous unconditional support while resisting wage regulation or sectoral minimum wages. They control employment thresholds, scheduling, and compensation within their labor markets and have the institutional resources to lobby for support policy expansion. The subsidy capture is material and quantifiable: the difference between what workers would require in autonomous wages and what they accept with income support is the captured share.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, low_wage_employers, agenda_setter).

% Workers in low-wage employment (retail, food service, care, hospitality, agriculture, gig work) whose earned wages fall below subsistence even at full-time hours. They receive unconditional income support that prevents immediate destitution but does not increase their bargaining power relative to employers. They cannot easily exit to higher-wage sectors (lack credentials, face geographic immobility, or confront discrimination barriers). They are trapped between accepting low wages supplemented by public income, or earning nothing at all. The income support is available to them, but only conditional on remaining in low-wage employment; if they refuse work, support is typically terminated or reduced. Their wage options are constrained by the knowledge that employers can pay less because the subsidy is available.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, biographical, trapped, national).

% Firms in high-wage sectors (technology, finance, specialized manufacturing, professional services) that pay productivity-linked wages above subsistence and do not employ workers who depend on income support. They bear the tax burden funding the support program while not capturing its subsidy (their labor markets are not affected by income support and their wage-setting is driven by productivity and skill scarcity). They have the resources and market position to relocate operations if the tax burden becomes excessive, and their labor costs are constrained by competitive skill markets rather than by subsistence income. They experience the program as a transfer from them to low-wage employers, not as a coordination benefit.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, high_productivity_firms, payer,
    powerful, generational, mobile, global).

% The government agency responsible for setting the income support level, defining eligibility, administering distribution, and funding the program through taxation or reallocation of other spending. It sets the rules and can unilaterally change them. It does not itself collect the extracted value (that accrues to employers) but bears the program cost and manages the fiscal consequences of reduced tax revenue if wage suppression accumulates. It is positioned to make the support conditional (tie it to work requirements, wage growth, or sector-specific supplements) but faces political pressure from both employers (who benefit from unconditional support) and workers (who may fear conditions). Its choices about program design determine whether the support functions primarily as coordination, primarily as subsidy capture, or as a mix of both.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, state_fiscal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Unions, worker-organizing bodies, and labor-left political movements. They see unconditional income support as a double-edged tool: it can provide a floor that enables worker refusal of coercive work (freedom_floor reading), but it can equally serve as cover for wage suppression if employers are permitted to capture the subsidy. They advocate for support being paired with wage floors, sector-specific supplements, and conditionality on wage growth or union recognition. They are trapped between supporting a policy that could liberate workers and opposing a policy mechanism that is being used to suppress wages. Their exit options are constrained because their members work in the low-wage and precariat sectors most affected, and they face declining membership power as wages stagnate despite support.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_bargaining_coalitions, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, labor_bargaining_coalitions, observer).

% Researchers, economists, and intellectuals who analyze and advocate for unconditional income support from a freedom-focused perspective (left-libertarian, de-commodification, capabilities approach). They argue that unconditional income support enables workers to exercise positive freedom (to refuse coercive work, to negotiate better terms, to pursue non-market activities). They produce econometric evidence, philosophical frameworks, and policy proposals supporting this reading. They engage with the wage_subsidy_reading as a design problem (how to prevent capture) rather than as a fundamental critique of the policy idea. Their analytical position sits at the intersection of theory (what the support should enable) and evidence (what it actually enables in implementation).
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, policy_economists_freedom_reading, observer,
    analytical, biographical, analytical, global).

% Workers who fall outside income support program eligibility due to immigration status, lack of formal documentation, or administrative exclusion (street-involved persons, people with criminal records, others deemed ineligible by policy design). They experience wage suppression without access to the compensating income support, because employers can threaten them with additional wage cuts, deportation, or job termination. Their exclusion from the nominal program paradoxically intensifies the wage-suppression dynamic for all workers: they anchor the floor toward which other workers' wages are driven downward, because employers can credibly threaten to replace higher-wage workers with excluded workers willing to accept even lower wages. Their absence from policy conversations and their structural exclusion from the beneficiary frame are not incidental to how the constraint operates; they are central to it. If they were included in income support, the structure of low-wage labor markets would shift (employers could not credibly threaten replacement with non-supported workers), and the subsidy-capture mechanism would be partially interrupted.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, excluded_precariat, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides minimal income floor that enables labor-market participation in sectors where necessary work (care, sanitation, retail, basic services) would otherwise attract workers only through threat of destitution or complete market failure. Solves the coordination problem: matching workers with undesirable but socially necessary jobs at wages that are individually unsustainable but collectively acceptable with supplementary income.
% TRANSFER_FUNCTION: Moves public funds (from taxation, primarily of high-productivity firms) to income-poor workers, with a structural portion captured by low-wage employers as suppressed wage costs. A worker receiving $1,000 in monthly income support who would have required $1,200 in autonomous wages represents a $200 monthly capture by the employer as reduced wage bills. Aggregated across all low-wage workers and all low-wage employers in a jurisdiction, this captured transfer can be substantial (5-15% of total program spending in high-employment sectors).
% ABSENT_VOICES: Precariat and undocumented workers are structurally excluded from income support programs and do not participate in policy deliberation, yet their exclusion and the threat of replacement they represent to other workers is central to how the constraint operates. They would argue that the policy's real effect is to subsidize their own exclusion and to anchor the wage floor downward for all other workers. Subsistence-wage workers in jurisdictions without or with lower levels of income support are also absent from policy conversations, yet their labor-market competition with subsidized low-wage workers in support-heavy jurisdictions creates downward pressure on their wages. High-productivity-firm workers whose tax burden funds the program without receiving benefits are partially represented (through their firms' political advocacy) but their individual voices on the program's fairness are typically absent from deliberation.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared overnight, low-wage workers would face immediate destitution unless wages rose to fill the gap. Employers in low-wage sectors would either raise wages to attract and retain workers (reducing their subsidy-derived profit margin), or reduce workforce and hours (shrinking the supply of low-wage labor). Low-wage sectors would reorganize: some would offer higher wages and reduced hours, some would shrink or relocate, some would accelerate automation (invest in technology rather than pay higher wages). The fiscal shock would cascade to tax-dependent public services and the social safety net. The historical record in jurisdictions that reduced or eliminated income support shows precisely this pattern: either wages rise, employment shrinks, or both. The arrangement demonstrably persists because the subsidy flow to employers is material and they have institutional power to defend it; without the subsidy mechanism, that arrangement would visibly break and wages in low-wage sectors would move closer to what autonomous workers would require.
% FOUNDING_PROBLEM: Late-stage capitalist labor markets, particularly in developed economies with deindustrialization and shift to service sectors, produce large numbers of jobs that at competitive market equilibrium pay below subsistence. These jobs (care work, sanitation, retail, hospitality, food service, agriculture) are socially necessary and cannot be outsourced or automated away, but they attract workers only through coercion (threat of destitution) or structural traps (immobility, credential barriers, discrimination). Income support was introduced to provide a floor that enables workers to survive and participate in the labor market while reducing the pure coercive mechanism of threat of starvation.
% FOUNDING_PROBLEM_CORROBORATION: Employers and pro-market economists argue the founding problem is live: without income support, care and service sectors would face acute labor shortages and quality degradation because workers would not accept subsistence-level wages, leading to either wage inflation (which erodes employer margins) or market collapse in essential services. Labor economists and worker organizations argue the founding problem was real in early welfare-state formation (1960s-1990s, before gig economy and precarity expansion) but has been substantially outpaced by the discovered subsidy-capture function: workers are now kept in low-wage sectors not by need for the support but by precarity (zero-hours contracts, gig shifting, task-based hiring) that keeps them from accumulating bargaining power even with the support. The shift from unconditional to conditional support in some jurisdictions (tied to work requirements), and the empirical lack of wage growth in support-heavy sectors despite program expansion, evidences that the founding coordination problem has been either solved or superseded by the extraction function. Data corroboration: OECD studies on wage growth in low-wage sectors, sector-specific labor-market analysis showing wage stagnation despite income support program increases, comparative cross-national studies showing low-wage sectors in high-support jurisdictions do not show higher wages than comparable low-support jurisdictions (suggesting capture rather than pass-through to workers).
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.68) because the constraint captures a measurable share of public transfer (estimated 20-40% depending on sector and labor elasticity) as suppressed wages. Suppression is moderate (0.52) because the trap is not coercive in the classical sense — workers are not physically forced to accept low wages — but structural: the income support removes the binding constraint (immediate destitution) that would force wage negotiation upward. Theater is moderate (0.41) because the arrangement is defended both as genuine coordination (matching workers to necessary jobs) and as support for worker autonomy, even as empirical evidence accumulates that wages in sectors with high income support coverage have grown more slowly than in comparable high-productivity sectors. The measurement series shows extractiveness and theater rising over the interval (t=0 to t=40, years 0-40 of policy implementation), reflecting the discovered and elaborated subsidy-capture function: as the policy matures, employers learn to rely on income support and systematically adjust wage-setting downward, increasing the captured share. Accessibility collapse is measured at four levels: structural level shows highest collapse (0.62→0.65) because the policy framework makes public income support institutional fact; organizational level (0.55→0.58) shows employer-side institutionalization of the subsidy; class level (0.48→0.52) shows worker-class constraint developing as wages fail to rise; individual level (0.35→0.42) shows individual workers retaining some ability to shift between employers within the constrained set, but declining as sector-wide wage suppression converges. Stakes inflation shows inverse pattern at organizational level (0.32→0.28) because low-wage employers face declining stakes as their subsidy capture increases and becomes reliable; but stakes rise for workers (individual 0.58→0.62, class 0.61→0.65) as the gap between income support and autonomous wage income widens.
 *
 * PERSPECTIVAL GAP:
 *   From the low_wage_employers' seat, this is genuine coordination: income support solves the matching problem for necessary but undesirable work, allowing them to operate sustainable businesses at scale. They author the beneficiary reading. From the low_wage_workers' seat, the same structure operates as a wage-suppression mechanism: the income support is available to them only because they remain in low-wage employment, and it does not increase their bargaining leverage relative to employers. From the high_productivity_firms' seat, this is a transfer from them (as taxpayers) to low-wage employers (as subsidy captors) with workers as pass-through; they experience it as cost without coordination benefit. From the state's seat, it is an expenditure with uncertain multiplier effects: some portion directly reaches worker consumption, some portion is captured by employers as suppressed costs, some portion leaks into reduced tax revenue from wage suppression. From labor coalitions' seat, this is an institutional mechanism that undermines their primary source of power (scarcity of labor) by creating a non-market income floor that employers can capture as wage constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Low_wage_employers are full beneficiaries (d → 0.0): they set wages, capture the subsidy as lower labor costs, and have mobile exit options if wage regulation increases. Their power is institutional (they control employment terms in their sectors) and their exit is arbitrage (relocate labor-intensive operations if local wages rise). Low_wage_workers are full targets (d → 1.0): they are trapped (confined to low-wage sectors by skills, geography, and credential barriers), their time horizon is biographical (no intergenerational exit through education or capital accumulation), and they bear the extraction as suppressed wages relative to what would obtain in a labor market without income support. High_productivity_firms are moderate payers (d → 0.6-0.7): they bear the tax cost but do not capture subsidy and are not directly affected by low-wage sector dynamics; their exit is mobile (relocate if tax burden rises). The state sits at d → 0.5 (symmetric): it provides the coordination mechanism (income floor) and bears the cost, but also constrains employer power through program design (could make support conditional, could require wage minimums). Labor coalitions sit at d → 0.65-0.75 (targets): they bear the erosion of their bargaining power and the suppression of wages in solidarity sectors, with constrained exit (union members are locked into their trades and sectors by skill and credential).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: late-stage capitalist labor markets produce sectors where competitive equilibrium wages are below subsistence (care, retail, hospitality, sanitation). Income support was introduced to provide a floor that enables workers to survive and participate without coercion. However, the observed function has shifted: instead of unconditional support enabling workers to refuse coercive work and negotiate better terms, the support has been captured by employers as suppressed wage costs. The mandatrophy is NOT that the founding problem disappeared (it is contested), but that the institutional design, while solving the original coordination problem (matching workers to necessary jobs), has revealed a second structural problem: employer capture of the subsidy. This is the diagnostic signature of tangled rope: the coordination function is real and necessary, but the extraction function is equally real and captured by the most powerful parties. A Piton reading would assert that income support is now mostly theater (the real wage-suppression mechanism operates through precarity and gig-ification, not through the support program itself). The wage_subsidy_reading asserts that the support program is the central institutional mechanism enabling wage suppression, not a theatrical artifact of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_capture_mechanism,
    'How much of the income support transfer is actually captured by low-wage employers as suppressed wage costs, versus retained by workers as genuine income increase?',
    'Econometric analysis comparing wage growth in low-wage sectors with high income support coverage versus comparable low-support sectors; controlled-experiment jurisdictions that vary support levels and measure wage responses; long-panel tracking of individual worker wages before and after support implementation.',
    'If capture is near-zero (worker wages rise by near the full support amount), the constraint is Rope or Scaffold; if capture is substantial (50%+), the wage_subsidy_reading is confirmed and Tangled Rope classification is sustained; if capture approaches 100%, the constraint approaches pure Snare (support becomes invisible redistribution from high-productivity firms to low-wage employers, with workers no better off).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_capture_mechanism, empirical, 'Degree of employer capture of public income support as suppressed wages.').

omega_variable(
    exit_leverage_delta,
    'Does the income support increase or decrease low-wage workers'' exit leverage relative to employers? Specifically: would a worker accept a lower wage (or worse conditions) WITH income support than they would require WITHOUT it?',
    'Survey evidence on reservation wages and willingness-to-accept offers among workers with and without income support; quasi-experimental variation in support availability; behavioral economics analysis of outside options and threat points in wage bargaining.',
    'If support INCREASES exit leverage (workers demand higher wages or refuse worse conditions because they have a fallback income), the freedom_floor_reading is partially supported and extraction is limited. If support DECREASES leverage (workers accept lower wages because subsistence is guaranteed), the wage_subsidy_reading is confirmed. If effect is near-zero (workers'' bargaining position is unchanged), the support is purely redistributive without coordination or extraction function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_leverage_delta, empirical, 'Direction and magnitude of income support''s effect on worker exit leverage in wage bargaining.').

omega_variable(
    labor_market_counterfactual,
    'What would the low-wage sector labor market look like if unconditional income support were withdrawn but all other conditions (technology, globalization, capital availability) remained constant?',
    'Simulation modeling with labor-supply elasticity estimates; historical comparison to pre-support equilibrium in early-adopter jurisdictions; controlled withdrawal experiments (pilot programs ending support and tracking outcomes); comparative cross-national analysis of similar economies with and without income support programs.',
    'If counterfactual shows wage increases (employers must raise wages to attract labor), the support is currently suppressing wages and the wage_subsidy_reading is confirmed. If counterfactual shows labor-market collapse (insufficient workers at higher wages), the coordination function is real but the magnitude of subsidy capture is ambiguous. If counterfactual shows sector automation (employers invest in technology instead of raising wages), the constraint is shifting from wage suppression to technological displacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_counterfactual, empirical, 'Counterfactual equilibrium of low-wage sectors without income support.').

omega_variable(
    kernel_reading_contestation,
    'Is this a case where the freedom_floor_reading and the wage_subsidy_reading coexist across different parties (coexists_with relation), or does one reading logically foreclose the other within a single coherent framework?',
    'Examination of whether a single actor can consistently hold both readings (they can: ''income support is liberatory BUT we must prevent wage suppression''), or whether accepting one reading commits an actor to rejecting the other (freedom_floor requires that workers exercise exit leverage; wage_subsidy requires that workers do not, by assumption).',
    'If coexists_with is correct, both readings remain live policy positions and policy debate should focus on institutional design (how to prevent capture). If forecloses is correct, one reading''s core premise logically rules out the other''s, and apparent agreement on income support masks deep disagreement about what it does. This affects how policy consensus is evaluated and whether apparent compromise is genuine or unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether freedom_floor and wage_subsidy readings are logically compatible or mutually foreclosing.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) primarily structural (employers extract through institutional power and market control) or internalized (workers believe they deserve low wages, or accept suppression as inevitable given their position)?',
    'Qualitative research on worker beliefs about wage adequacy and entitlement; comparison of reservation wages and accepted wages when support is transparent versus opaque; post-exit trajectory analysis (if workers'' wage expectations rise after leaving low-wage sectors, suppression was partially internalized).',
    'If suppression is primarily structural, removing employer power (unionization, wage floors, benefit requirements) could increase wages directly. If suppression is internalized, workers would need cognitive reframing (recognition that low wages are not inevitable) before wage expectations rise. Mixed suppression suggests institutional redesign must address both structural barriers and internalized expectations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree to which measured suppression is structural versus internalized in worker consciousness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__wage_subsidy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(inco_tr_t25, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__wage_subsidy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(inco_tr_t30, observed).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__wage_subsidy_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(inco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(inco_be_t25, observed).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(inco_be_t30, observed).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(inco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement_basis(inco_su_t25, observed).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(inco_su_t30, observed).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(inco_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(inco_grid_01, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(inco_grid_02, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(class), 40, 0.52).
narrative_ontology:measurement(inco_grid_03, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(inco_grid_04, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(individual), 40, 0.42).
narrative_ontology:measurement(inco_grid_05, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(inco_grid_06, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(organizational), 40, 0.58).
narrative_ontology:measurement(inco_grid_07, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(inco_grid_08, income_support_conditionality__wage_subsidy_reading, accessibility_collapse(structural), 40, 0.65).
narrative_ontology:measurement(inco_grid_09, income_support_conditionality__wage_subsidy_reading, resistance(class), 0, 0.61).
narrative_ontology:measurement(inco_grid_10, income_support_conditionality__wage_subsidy_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(inco_grid_11, income_support_conditionality__wage_subsidy_reading, resistance(individual), 0, 0.38).
narrative_ontology:measurement(inco_grid_12, income_support_conditionality__wage_subsidy_reading, resistance(individual), 40, 0.35).
narrative_ontology:measurement(inco_grid_13, income_support_conditionality__wage_subsidy_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(inco_grid_14, income_support_conditionality__wage_subsidy_reading, resistance(organizational), 40, 0.68).
narrative_ontology:measurement(inco_grid_15, income_support_conditionality__wage_subsidy_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(inco_grid_16, income_support_conditionality__wage_subsidy_reading, resistance(structural), 40, 0.52).
narrative_ontology:measurement(inco_grid_17, income_support_conditionality__wage_subsidy_reading, stakes_inflation(class), 0, 0.61).
narrative_ontology:measurement(inco_grid_18, income_support_conditionality__wage_subsidy_reading, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(inco_grid_19, income_support_conditionality__wage_subsidy_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(inco_grid_20, income_support_conditionality__wage_subsidy_reading, stakes_inflation(individual), 40, 0.62).
narrative_ontology:measurement(inco_grid_21, income_support_conditionality__wage_subsidy_reading, stakes_inflation(organizational), 0, 0.32).
narrative_ontology:measurement(inco_grid_22, income_support_conditionality__wage_subsidy_reading, stakes_inflation(organizational), 40, 0.28).
narrative_ontology:measurement(inco_grid_23, income_support_conditionality__wage_subsidy_reading, stakes_inflation(structural), 0, 0.44).
narrative_ontology:measurement(inco_grid_24, income_support_conditionality__wage_subsidy_reading, stakes_inflation(structural), 40, 0.48).
narrative_ontology:measurement(inco_grid_25, income_support_conditionality__wage_subsidy_reading, suppression(class), 0, 0.52).
narrative_ontology:measurement(inco_grid_26, income_support_conditionality__wage_subsidy_reading, suppression(class), 40, 0.54).
narrative_ontology:measurement(inco_grid_27, income_support_conditionality__wage_subsidy_reading, suppression(individual), 0, 0.55).
narrative_ontology:measurement(inco_grid_28, income_support_conditionality__wage_subsidy_reading, suppression(individual), 40, 0.58).
narrative_ontology:measurement(inco_grid_29, income_support_conditionality__wage_subsidy_reading, suppression(organizational), 0, 0.18).
narrative_ontology:measurement(inco_grid_30, income_support_conditionality__wage_subsidy_reading, suppression(organizational), 40, 0.15).
narrative_ontology:measurement(inco_grid_31, income_support_conditionality__wage_subsidy_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(inco_grid_32, income_support_conditionality__wage_subsidy_reading, suppression(structural), 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__wage_subsidy_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, precariat_labor_gig_economy_subsidy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the income_support_conditionality kernel. The freedom_floor_reading frames the same policy as liberatory (decommodifies labor, enables exit); the dependency_trap_reading frames it as degrading (undermines motivation, creates dependency). The wage_subsidy_reading asserts that the policy's actual function depends on institutional design — it can serve liberation OR trap workers in low-wage subsidy depending on whether wage suppression is prevented. The three readings share the same kernel (unconditional income support exists and is universally available) but diverge on what that commitment accomplishes. Generate all three readings as separate constraint stories linked by network.affects_constraints to enable comparative analysis of how different policy communities read the same institutional fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, powerless, 0.92).
constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
