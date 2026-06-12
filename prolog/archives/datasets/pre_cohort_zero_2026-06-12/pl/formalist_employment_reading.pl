% ============================================================================
% CONSTRAINT STORY: formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_formalist_employment_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: formalist_employment_reading
 *   human_readable: Formalist Employment Boundary (Contract + Supervision Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The formalist employment boundary defines employment by formal contract
 *   terms and direct human supervision, excluding platform workers who are
 *   labeled 'independent contractors' despite algorithmic management and
 *   unilateral terms. This reading of the employment kernel emerged from
 *   20th-century labor law designed for stable firms and long-term employment
 *   relationships. It solves a genuine coordination problem: distinguishing
 *   employees (who receive minimum wage, overtime, benefits, wrongful
 *   termination protection, collective bargaining rights) from genuine
 *   independent contractors (who negotiate rates, control work methods, and
 *   operate across multiple clients). But the formal criteria are gameable:
 *   platforms use contractor agreements and algorithmic supervision (rather
 *   than human managers) to avoid employment classification while exercising
 *   functional control over workers. The constraint exhibits substantial
 *   extraction: platforms externalize labor costs to workers (who bear injury
 *   risk, income volatility, and lack of benefits) and to state insurance
 *   systems (which absorb emergency medical costs and means-tested benefits).
 *   The extraction has increased over the interval as platform work has
 *   scaled and algorithmic management has intensified. Theater ratio reflects
 *   the gap between the formal 'independence' narrative and the structural
 *   reality of algorithmic control and economic dependence.
 *
 * KEY AGENTS:
 *   - Platform Companies: Primary beneficiary (institutional/arbitrage) — capture marketplace rents while externalizing employment costs; can exit to favorable jurisdictions
 *   - Low-Bargaining-Power Platform Workers: Primary victim (powerless/trapped) — excluded from employment protections despite functional employee status; bear full downside risk
 *   - State Insurance Systems: Institutional victim (institutional/constrained) — absorb fiscal costs of uninsured platform workers via emergency services and means-tested benefits
 *   - Traditional Employees: Secondary victim (moderate/constrained) — face wage floor erosion from competition with unprotected platform labor
 *   - High-Skill Contractors: Secondary beneficiary (powerful/arbitrage) — genuine independent professionals who benefit from legal clarity and tax optimization
 *   - Labor Reform Coalition: Organized agents (organized/constrained) — building alternative classification frameworks with sunset logic
 *   - Analytical Observer: Sees both coordination (legitimate contractor distinction) and extraction (misclassification of dependent workers)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(formalist_employment_reading, 0.68).
domain_priors:suppression_score(formalist_employment_reading, 0.72).
domain_priors:theater_ratio(formalist_employment_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(formalist_employment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(formalist_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(formalist_employment_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(formalist_employment_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(formalist_employment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(formalist_employment_reading, "Formalist Employment Boundary (Contract + Supervision Reading)").
narrative_ontology:topic_domain(formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(formalist_employment_reading, 'a3e8bd07-a6e9-457e-b97e-a616156d651a').
narrative_ontology:cs_kernel_codification('a3e8bd07-a6e9-457e-b97e-a616156d651a', formalized).
narrative_ontology:cs_authority_grounding('a3e8bd07-a6e9-457e-b97e-a616156d651a', lineage).
narrative_ontology:cs_interpretation_layer_present('a3e8bd07-a6e9-457e-b97e-a616156d651a').
narrative_ontology:cs_reading_relation('a3e8bd07-a6e9-457e-b97e-a616156d651a', formalist_employment_reading__substantive_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3e8bd07-a6e9-457e-b97e-a616156d651a', formalist_employment_reading__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('a3e8bd07-a6e9-457e-b97e-a616156d651a', foundational, contract_label_determinative).
narrative_ontology:cs_axiom_status(contract_label_determinative, holdable).
narrative_ontology:cs_axiom_grounding('a3e8bd07-a6e9-457e-b97e-a616156d651a', contract_label_determinative, conventional).
narrative_ontology:cs_axiom('a3e8bd07-a6e9-457e-b97e-a616156d651a', foundational, supervision_requires_human_oversight).
narrative_ontology:cs_axiom_status(supervision_requires_human_oversight, holdable).
narrative_ontology:cs_axiom_grounding('a3e8bd07-a6e9-457e-b97e-a616156d651a', supervision_requires_human_oversight, empirically_contingent).
narrative_ontology:cs_axiom('a3e8bd07-a6e9-457e-b97e-a616156d651a', secondary, flexibility_equals_independence).
narrative_ontology:cs_axiom_status(flexibility_equals_independence, holdable).
narrative_ontology:cs_axiom_grounding('a3e8bd07-a6e9-457e-b97e-a616156d651a', flexibility_equals_independence, instrumental).
narrative_ontology:cs_reference_frame('a3e8bd07-a6e9-457e-b97e-a616156d651a', common_law_employment_doctrine).
narrative_ontology:cs_drift_state('a3e8bd07-a6e9-457e-b97e-a616156d651a', platform_economy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3e8bd07-a6e9-457e-b97e-a616156d651a', '2026-01-15T14:32:00Z').
narrative_ontology:cs_kernel_id(formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(formalist_employment_reading, high_skill_contractors).
narrative_ontology:constraint_victim(formalist_employment_reading, platform_workers_low_bargaining_power).
narrative_ontology:constraint_victim(formalist_employment_reading, state_insurance_systems).
narrative_ontology:constraint_victim(formalist_employment_reading, traditional_employees_wage_floor).
narrative_ontology:constraint_vindicates(formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(formalist_employment_reading, entrepreneurial_autonomy_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of the contractor relationship via unilateral terms of service and algorithmic management. Capture marketplace rents while externalizing employment costs (health insurance, retirement, unemployment risk, workers' compensation, training) to workers and state systems. Can exit to jurisdictions with favorable classification regimes or restructure operations if enforcement tightens. The formalist boundary enables asset-light scaling and global expansion.
narrative_ontology:constraint_stakeholder(formalist_employment_reading, platform_companies, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(formalist_employment_reading, platform_companies, beneficiary).

% Bear the costs of contractor classification: no minimum wage, no overtime, no unemployment insurance, no workers' compensation, no wrongful termination protection, no collective bargaining rights. Face algorithmic supervision (route assignment, performance scoring, deactivation authority) and unilateral terms with no negotiating power. Cannot exit to traditional employment (skills mismatch, credential barriers, geographic constraints) and cannot exit platform work (economic necessity, bills due, no savings buffer). The 'flexibility' narrative does not match their structural reality: they work when the algorithm assigns work, at rates the platform sets, under threat of deactivation.
narrative_ontology:constraint_stakeholder(formalist_employment_reading, platform_workers_low_bargaining_power, payer,
    powerless, biographical, trapped, national).

% Genuine independent professionals (software developers, consultants, designers) who negotiate rates, choose clients, control work methods, and prefer contractor status for tax optimization and schedule flexibility. The formalist boundary protects their autonomy and enables them to operate across multiple clients without employment entanglement. They have the bargaining power to capture the upside of independent contracting.
narrative_ontology:constraint_stakeholder(formalist_employment_reading, high_skill_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Absorb fiscal costs of platform worker exclusion from employment-based coverage: emergency room visits for uninsured gig workers, means-tested benefits for income-volatile households, long-term disability costs from uncompensated workplace injuries. The formalist boundary creates a coverage gap that the state fills via general revenue rather than employer contributions. Cannot exit the fiscal obligation (emergency services are legally mandated, means-tested benefits are politically entrenched) but also cannot compel platforms to bear employment costs under current classification.
narrative_ontology:constraint_stakeholder(formalist_employment_reading, state_insurance_systems, payer,
    institutional, generational, constrained, national).

% Face wage floor erosion from labor market competition with unprotected platform workers. Benefit from employment classification when it applies to them (minimum wage, overtime, benefits, wrongful termination protection) but are harmed when the formalist boundary allows platforms to undercut wage floors and working conditions via contractor misclassification. The constraint coordinates some labor standards while simultaneously enabling their erosion via competitive pressure.
narrative_ontology:constraint_stakeholder(formalist_employment_reading, traditional_employees_wage_floor, payer,
    moderate, biographical, constrained, national).

% Labor unions, worker advocacy groups, and progressive legislators building alternative classification frameworks: portable benefits legislation, sectoral bargaining, ABC tests that reclassify platform workers as employees. See the formalist boundary as a temporary misclassification that reform will correct. The sunset logic: the boundary was designed for a world of stable firms and long-term employment; it cannot survive the platform economy's scale without triggering political correction.
narrative_ontology:constraint_stakeholder(formalist_employment_reading, labor_reform_coalition, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The formalist employment boundary solves the problem of distinguishing employees (who receive minimum wage, overtime, benefits, wrongful termination protection, collective bargaining rights) from genuine independent contractors (who negotiate rates, control work methods, and operate across multiple clients). The boundary coordinates labor standards for covered workers and protects legitimate independent contracting for high-skill professionals.
% TRANSFER_FUNCTION: The arrangement transfers employment costs (health insurance, retirement, unemployment risk, workers' compensation, training) from platforms to workers and state insurance systems. It transfers marketplace rents from workers (who would capture more value as employees with bargaining rights) to platforms (who capture rents via cost externalization). It transfers schedule flexibility and tax optimization benefits to high-skill contractors who have the bargaining power to use them.
% ABSENT_VOICES: Low-bargaining-power platform workers are formally in the conversation (they sign contractor agreements) but have no negotiating power over terms. Their structural position (economic necessity, algorithmic supervision, unilateral terms) is not represented in the formalist test's design. The test was designed for a world of stable firms and long-term employment, not for algorithmic management and marketplace platforms. The workers' lack of voice is structural: they are atomized, geographically dispersed, and face retaliation risk (deactivation) for organizing.
% DISAPPEARANCE_RATIONALE: If the formalist employment boundary disappeared overnight, platforms would face immediate employment obligations: minimum wage, overtime, unemployment insurance contributions, workers' compensation premiums, wrongful termination liability, and collective bargaining duties. They would either (1) absorb the costs and raise prices, (2) automate more aggressively to reduce headcount, (3) exit jurisdictions with strict enforcement, or (4) restructure operations to minimize employee count. Workers would gain protections but might lose flexibility (platforms would impose schedules to justify employment costs). State insurance systems would see fiscal relief (employment-based coverage would replace means-tested benefits). Traditional employees would see wage floor stabilization (less competition from unprotected labor). High-skill contractors would lose tax optimization and autonomy (platforms might default to employment to avoid misclassification risk). The world rearranges substantially.
% FOUNDING_PROBLEM: The employment boundary was built to solve the problem of distinguishing employees (who need protection from employer power asymmetry) from independent business owners (who do not need protection because they have bargaining power and control). The founding problem in early 20th-century labor law: how to extend minimum wage, overtime, and collective bargaining rights to factory workers and service employees without entangling genuine independent contractors (doctors, lawyers, small business owners) in employment regulation.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and libertarian legal scholars argue the founding problem is still live: the distinction between employees and contractors remains necessary to protect entrepreneurial autonomy and avoid over-regulation. Labor advocates and progressive legal scholars argue the founding problem is dead: the platform economy has created a new category of workers (algorithmically managed, economically dependent, but formally independent) that the founding distinction does not fit. The contest is visible in litigation (ABC test cases, economic realities test cases), legislation (portable benefits bills, sectoral bargaining proposals), and academic discourse. Corroborating sources outside the beneficiary set: labor economists documenting income volatility and lack of benefits among platform workers; state insurance actuaries documenting fiscal externalities; courts applying economic realities tests that look past formal contract labels to functional control.
narrative_ontology:disappearance_verdict(formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(formalist_employment_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-BARGAINING-POWER PLATFORM WORKER (SNARE) — Trapped by economic necessity in algorithmically managed work with no employment protections. The formalist boundary excludes them from minimum wage, overtime, unemployment insurance, workers' compensation, and collective bargaining rights. Cannot exit to traditional employment (skills mismatch, credential barriers, geographic constraints) and cannot exit platform work (bills due, no savings buffer). Experiences maximum extraction: bears full downside risk (injury, demand fluctuation, algorithmic deactivation) with no upside protection. The 'independent contractor' label is pure cover — the coordination story (flexibility, autonomy) does not match their structural reality (algorithmic supervision, unilateral terms, no negotiating power).
constraint_indexing:constraint_classification(formalist_employment_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL EMPLOYEE (TANGLED ROPE) — Constrained by labor market competition from unprotected platform workers. Benefits from the employment classification system when it applies to them (minimum wage, overtime, benefits, wrongful termination protection) but is harmed when the formalist boundary allows platforms to undercut wage floors and working conditions via contractor misclassification. The constraint coordinates some labor standards while simultaneously enabling their erosion. Mixed experience: protection within the boundary, but the boundary's placement extracts from them via competitive pressure.
constraint_indexing:constraint_classification(formalist_employment_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM COMPANY (ROPE) — Primary beneficiary with arbitrage-grade exit options. The formalist boundary solves a genuine coordination problem: how to scale a marketplace without employment overhead. Experiences the constraint as pure coordination: the legal clarity of the contractor classification enables rapid scaling, asset-light business models, and global expansion. Can exit to jurisdictions with favorable classification regimes or restructure operations if enforcement tightens. Net beneficiary: extraction runs toward this agent. The constraint externalizes labor costs (health insurance, retirement, unemployment risk, training) to workers and state systems while capturing marketplace rents.
constraint_indexing:constraint_classification(formalist_employment_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE INSURANCE SYSTEM (TANGLED ROPE) — Constrained institutional victim. The formalist boundary coordinates employment-based social insurance (unemployment, workers' comp, disability) but creates a coverage gap when platform workers are excluded. The state bears fiscal costs: emergency room visits for uninsured gig workers, means-tested benefits for income-volatile households, long-term disability costs from uncompensated workplace injuries. Also experiences coordination function: the employment boundary does organize social insurance for covered workers. Mixed extraction: the system works for some, fails for others, and the state absorbs the externalized costs.
constraint_indexing:constraint_classification(formalist_employment_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-SKILL CONTRACTOR (ROPE) — Powerful agent with genuine arbitrage options. The formalist boundary enables legitimate independent contracting: specialized professionals (software developers, consultants, designers) who negotiate rates, choose clients, control work methods, and prefer contractor status for tax optimization and schedule flexibility. Experiences the constraint as coordination: the legal clarity allows them to operate across multiple clients without employment entanglement. Net beneficiary: the flexibility is real, the autonomy is real, and they have the bargaining power to capture the upside.
constraint_indexing:constraint_classification(formalist_employment_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR REFORM COALITION (SCAFFOLD) — Organized agents (labor unions, worker advocacy groups, progressive legislators) see the formalist boundary as a temporary misclassification that reform will correct. They are building alternative pathways: portable benefits legislation, sectoral bargaining frameworks, ABC tests that reclassify platform workers as employees. The constraint coordinates labor standards for now, but the coalition sees a sunset: as platform work becomes the dominant employment model, the formalist boundary will be legislatively overridden or judicially reinterpreted. The sunset logic: the boundary was designed for a world of stable firms and long-term employment; it cannot survive the platform economy's scale without triggering political correction.
constraint_indexing:constraint_classification(formalist_employment_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The formalist employment boundary coordinates some legitimate independent contracting (high-skill professionals with real autonomy) while enabling substantial extraction from low-bargaining-power platform workers who are functionally employees under algorithmic supervision. The constraint exhibits both coordination and extraction: it solves the real problem of distinguishing employees from contractors, but the formal criteria (contract label, supervision method) are gameable and systematically misapplied to workers with no meaningful independence. The analytical perspective sees the structural asymmetry: platforms capture the coordination benefit (legal clarity for scaling) while externalizing the extraction cost (unprotected labor, state-absorbed risk). This is the claimed type.
constraint_indexing:constraint_classification(formalist_employment_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(formalist_employment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(formalist_employment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(formalist_employment_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. Platforms capture marketplace rents while externalizing employment costs (health insurance, retirement, unemployment risk, workers' compensation, training) to workers and state systems. The extraction has increased over the interval as platform work has scaled from niche gig economy to dominant employment model in transportation, delivery, and service sectors. The base metric reflects the weighted average across all affected agents: high extraction from low-bargaining-power workers and state systems, moderate extraction from traditional employees via wage competition, low or negative extraction for platforms and high-skill contractors. Suppression (0.72): High. Workers face significant barriers to exit: economic necessity (bills due, no savings buffer), skills mismatch with traditional employment, credential barriers, geographic constraints, and algorithmic deactivation risk. Platforms actively suppress alternatives via terms-of-service restrictions on collective action, arbitration clauses blocking class litigation, and lobbying against reclassification legislation. Suppression has increased as platforms have consolidated market power and built enforcement infrastructure. Theater ratio (0.58): Moderate-high. The 'independent contractor' label and 'flexibility' narrative are substantially performative for low-bargaining-power workers who experience algorithmic supervision, unilateral terms, and no meaningful negotiating power. The theater is the gap between formal independence and functional dependence. Theater has increased as platforms have refined algorithmic management to maximize control while maintaining contractor classification. Accessibility collapse (0.35): Low-moderate. Alternatives to the formalist boundary are visible and contested: substantive employment tests (ABC test, economic realities test), portable benefits legislation, sectoral bargaining frameworks. The boundary is not experienced as inevitable — labor advocates, progressive legislators, and some courts actively challenge it. Resistance (0.71): High. The constraint meets substantial organized resistance from labor unions, worker advocacy groups, and reform coalitions. Platform workers themselves resist via organizing campaigns, wildcat strikes, and litigation. The resistance is a structural feature: the boundary's extraction is visible and contested, not naturalized.
 *
 * PERSPECTIVAL GAP:
 *   The platform company sees pure coordination (Rope): the formalist boundary enables scaling and marketplace efficiency. The high-skill contractor sees coordination (Rope): the boundary protects legitimate independent contracting. The low-bargaining-power platform worker sees pure extraction (Snare): the contractor label is cover for unprotected, algorithmically managed work with no exit. The traditional employee sees mixed coordination and extraction (Tangled Rope): protected within the boundary but harmed by its placement. The state insurance system sees mixed coordination and extraction (Tangled Rope): the boundary organizes social insurance for covered workers but creates a fiscal externality for excluded workers. The labor reform coalition sees a temporary problem with a sunset (Scaffold): the boundary will be legislatively corrected as platform work scales. The analytical observer sees tangled rope: genuine coordination for high-skill contractors, substantial extraction from low-bargaining-power workers, with platforms capturing the coordination benefit while externalizing the extraction cost. The gap reveals that the 'employment' category is not a natural kind but a contested boundary with different structural consequences for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are primary beneficiaries with arbitrage exit options — they experience low or negative effective extraction (the constraint subsidizes them by externalizing costs). High-skill contractors are secondary beneficiaries with arbitrage options — they experience low extraction (genuine flexibility and autonomy). Low-bargaining-power platform workers are primary victims with trapped exit options — they experience maximum extraction (excluded from protections, bear full risk, no exit path). State insurance systems are institutional victims with constrained exit — they experience substantial extraction (absorb fiscal costs of uninsured workers). Traditional employees are secondary victims with constrained exit — they experience moderate extraction (wage floor erosion from platform competition). The labor reform coalition is organized with constrained exit — they experience moderate extraction but see a sunset path. The analytical observer sees the structural asymmetry: coordination benefit flows to platforms and high-skill contractors, extraction cost flows to low-bargaining-power workers and state systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The formalist employment boundary exhibits mandatrophy risk: its original mandate (distinguishing employees from independent contractors in a world of stable firms and long-term employment) is increasingly mismatched to the platform economy's scale and algorithmic management. The constraint persists not because it solves the coordination problem well but because platforms benefit from its persistence and have the political power to defend it. The theater ratio (0.58) reflects this: the formal criteria (contract label, supervision method) are maintained as legitimating performance while the structural reality (algorithmic control, economic dependence, unilateral terms) diverges. The labor reform coalition's scaffold perspective captures the sunset logic: as platform work becomes the dominant employment model, the formalist boundary's extraction will trigger political correction. The mandatrophy is not yet resolved — the constraint is still active and enforced — but the organized resistance and visible alternatives indicate the boundary is contested rather than naturalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the employment boundary defined by formal contract criteria (this reading) or by substantive economic dependence and control (substantive_employment_reading)?',
    'Judicial interpretation in contested cases; legislative reform that adopts one framing over the other; empirical analysis of which test better predicts worker vulnerability and need for protection.',
    'If formalist reading prevails: platform workers remain excluded from employment protections, extraction continues. If substantive reading prevails: most platform workers reclassified as employees, platforms bear employment costs, extraction substantially reduced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Which reading of the employment kernel is structurally correct: formal contract criteria or substantive economic reality?').

omega_variable(
    algorithmic_supervision_threshold,
    'Does algorithmic management (route assignment, performance scoring, deactivation authority) constitute ''supervision'' under the formalist test, or does supervision require human oversight?',
    'Case law development on whether algorithmic control satisfies the supervision prong of employment tests; empirical comparison of worker autonomy under algorithmic vs human management.',
    'If algorithmic management counts as supervision: formalist test reclassifies many platform workers as employees, reducing extraction. If it does not: formalist boundary holds, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_supervision_threshold, empirical, 'Whether algorithmic management constitutes supervision under formalist employment criteria').

omega_variable(
    flexibility_autonomy_empirical_test,
    'Do platform workers with low bargaining power actually experience meaningful flexibility and autonomy, or is the flexibility narrative a cover story for precarity?',
    'Survey data on schedule control, income volatility, ability to refuse assignments, and subjective autonomy; comparison of stated preferences (flexibility) vs revealed preferences (behavior under economic pressure).',
    'If flexibility is real: coordination function is genuine, tangled_rope classification holds. If flexibility is illusory: coordination story is cover, snare classification from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flexibility_autonomy_empirical_test, empirical, 'Whether platform workers experience genuine flexibility or precarity disguised as autonomy').

omega_variable(
    state_fiscal_externality_magnitude,
    'What is the fiscal cost to state insurance systems of platform worker exclusion from employment-based coverage?',
    'Actuarial analysis of emergency medical costs, means-tested benefit utilization, and long-term disability claims attributable to uninsured platform work; comparison to employment-based insurance counterfactual.',
    'If fiscal externality is large: state is a major victim, extraction is higher than base metric suggests. If small: extraction is concentrated on workers, state impact is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_fiscal_externality_magnitude, empirical, 'Magnitude of fiscal externality to state insurance systems from contractor misclassification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(formalist_employment_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(formal_emp_tr_t0, formalist_employment_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(formal_emp_tr_t3, formalist_employment_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement(formal_emp_tr_t6, formalist_employment_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(formal_emp_tr_t9, formalist_employment_reading, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(formal_emp_be_t0, formalist_employment_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(formal_emp_be_t3, formalist_employment_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(formal_emp_be_t6, formalist_employment_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(formal_emp_be_t9, formalist_employment_reading, base_extractiveness, 9, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(formal_emp_su_t0, formalist_employment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(formal_emp_su_t3, formalist_employment_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(formal_emp_su_t6, formalist_employment_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(formal_emp_su_t9, formalist_employment_reading, suppression_requirement, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(formalist_employment_reading, resource_allocation).
narrative_ontology:affects_constraint(formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(formalist_employment_reading, hybrid_security_reading).
narrative_ontology:affects_constraint(formalist_employment_reading, minimum_wage_floor).
narrative_ontology:affects_constraint(formalist_employment_reading, collective_bargaining_access).

% DUAL FORMULATION NOTE:
% The formalist employment boundary is one reading of the employment_boundary kernel. The substantive_employment_reading and hybrid_security_reading are sibling constraints (other readings of the same kernel) with different ε values and different beneficiary/victim structures. All three readings are linked via network.affects_constraints. The formalist reading also affects downstream constraints (minimum_wage_floor, collective_bargaining_access) by determining who is covered.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
