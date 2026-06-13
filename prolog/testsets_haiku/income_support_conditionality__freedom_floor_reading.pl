% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/labor/social_policy
 *
 * SUMMARY:
 *   Under this reading, unconditional income support functions as a
 *   structural decommodification of labor power. By providing subsistence
 *   security independent of employment, the constraint removes the coercive
 *   leverage employers exert through starvation threat. Low-wage workers gain
 *   a genuine exit option — they can refuse unsafe work, subminimum wages, or
 *   coercive management without facing immediate destitution. The constraint
 *   is claimed as ROPE (coordination on exit options creating voluntary labor
 *   markets) with metrics reflecting low extractiveness (the constraint
 *   removes extraction from the employer domain), low suppression (no active
 *   coercion is required to maintain the floor — it is a transfer, not a
 *   enforcement mechanism), and low theater (the function is what it appears
 *   to be: subsistence security). This reading directly contests two
 *   siblings: the dependency_trap_reading (which claims the floor undermines
 *   incentives) and the wage_subsidy_reading (which claims it subsidizes
 *   employers). This is one ε-invariant constraint instantiating one reading;
 *   the siblings are separate constraint stories linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Low-wage workers: primary beneficiaries. Exit from coercive labor markets; negotiate from strength rather than desperation.
 *   - Precarious workers: secondary beneficiaries. Gain protection against wage theft and arbitrary termination; can refuse abusive conditions.
 *   - Care work performers: tertiary beneficiaries. Enable unpaid or low-wage care work as genuine choice, not economic necessity.
 *   - Employers seeking low-wage labor: primary payers. Lose coercive leverage; must compete on wages and conditions.
 *   - Public finance system: secondary payer. Bears transfer cost; mandatory but democratic and recurring.
 *   - Subsistence/informal workers: structurally excluded. Fall outside administrative reach despite needing it most.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.22).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.18).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/labor/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'e502730e-4305-43a3-8058-ddba8403b74d').
narrative_ontology:cs_kernel_codification('e502730e-4305-43a3-8058-ddba8403b74d', distributed).
narrative_ontology:cs_authority_grounding('e502730e-4305-43a3-8058-ddba8403b74d', distributed).
narrative_ontology:cs_reading_relation('e502730e-4305-43a3-8058-ddba8403b74d', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('e502730e-4305-43a3-8058-ddba8403b74d', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('e502730e-4305-43a3-8058-ddba8403b74d', foundational, labor_decommodification_via_exit_option).
narrative_ontology:cs_axiom_status(labor_decommodification_via_exit_option, holdable).
narrative_ontology:cs_axiom_grounding('e502730e-4305-43a3-8058-ddba8403b74d', labor_decommodification_via_exit_option, deontological).
narrative_ontology:cs_axiom('e502730e-4305-43a3-8058-ddba8403b74d', foundational, subsistence_security_enables_positive_freedom).
narrative_ontology:cs_axiom_status(subsistence_security_enables_positive_freedom, holdable).
narrative_ontology:cs_axiom_grounding('e502730e-4305-43a3-8058-ddba8403b74d', subsistence_security_enables_positive_freedom, instrumental).
narrative_ontology:cs_reference_frame('e502730e-4305-43a3-8058-ddba8403b74d', coercive_labor_market_baseline).
narrative_ontology:cs_drift_state('e502730e-4305-43a3-8058-ddba8403b74d', post_income_floor_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e502730e-4305-43a3-8058-ddba8403b74d', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, care_work_performers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_seeking_low_wage_labor).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, public_finance_system).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, positive_freedom_doctrine).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, decommodification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support that creates a genuine exit option from coercive employment: they can refuse jobs with unsafe conditions, subminimum wages, or coercive management practices without immediate destitution. The floor changes the negotiating position fundamentally — employers can no longer use immediate starvation as a disciplinary threat. They remain workers but as voluntary participants rather than necessity-driven captives.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, arbitrage, national).

% Gain protection against wage theft, unpredictable hours, and termination without cause — the income floor makes the cost of refusing abusive management bearable. They can search for stable work without accepting the first predatory offer. The support de-links survival from employer discretion.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, arbitrage, national).

% The support enables performing care work (childcare, elder care, community support) that markets systematically underprice or refuse to compensate. Without the floor, care work is either unpaid household labor or wage slavery in low-margin care industries. The floor decommodifies care: it becomes a genuine choice rather than economic necessity.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, care_work_performers, beneficiary,
    powerless, biographical, arbitrage, national).

% Lose the coercive leverage of threatened destitution: they can no longer discipline workers through starvation or force acceptance of wages below subsistence because workers have an exit option. They must compete on wages, conditions, and autonomy rather than desperation. The constraint removes a source of extraction from their available toolkit.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_seeking_low_wage_labor, payer,
    powerful, generational, constrained, national).

% Bears the cost of the unconditional transfer. This is a pure structural transfer: public resources fund the floor. The finance is mandatory (tax/debt), but the constraint itself does not enforce collection or constrain resistance — it is a democratic choice, recurring and contestable at each budget cycle.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, public_finance_system, payer,
    institutional, generational, constrained, national).

% Experience the constraint indirectly: their employers face tighter labor markets and upward wage pressure from the floor; some may pay higher taxes to fund the floor depending on the financing structure. They are not the primary beneficiaries or payers; their situation changes at the margin through labor-market spillovers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, high_wage_workers_and_professionals, observer,
    powerful, biographical, mobile, national).

% May fall outside the institutional reach of the income support system (undocumented status, rural isolation, informal-sector work that leaves no administrative trace). They would benefit if included but are structurally prevented from claiming the floor by administrative or legal barriers. Their exclusion is what the system's implementation boundary creates.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, subsistence_agriculture_and_informal_economy_participants, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared exit option (the income floor) that coordinates the labor market: workers can refuse coercive work; employers must compete on terms rather than desperation; wage offers reflect genuine productivity and preference, not threat.
% TRANSFER_FUNCTION: Transfers public resources (tax/debt revenue) to low-wage workers without work requirement, moving income from the tax-funding pool (broadly progressive or regressive depending on financing) to those without other adequate income sources.
% ABSENT_VOICES: Subsistence and informal-economy workers are structurally excluded by implementation boundaries (documentation, spatial coverage, registration requirements). Tax-burdened small-business owners and mid-tier employees who may experience wage pressure or tax incidence carry costs not volunteered into the conversation. Employers who depend on coercive labor (those whose model is extractive, not competitive) have no seat because the constraint's point is to disrupt their coercive lever.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, low-wage workers would lose their exit option and employers would regain coercive leverage. Wage pressure would collapse, work conditions would degrade, and workers would accept jobs only under desperation. The labor market would reorganize around necessity rather than voluntary participation. Care work would shrink further into unpaid household labor. The economy would rearrange to a lower-wage, higher-coercion equilibrium.
% FOUNDING_PROBLEM: Labor markets under commodity scarcity create coercive pressure: workers must accept any wage and condition because refusal means starvation. This is not a failure of individual work ethic but a structural fact of labor commodification without subsistence security. The problem is the absence of genuine exit options, not the absence of jobs.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists (Piketty, Wacquant, Roemer) document persistent wage suppression and coercive work conditions in low-wage labor markets. Worker testimony from strike histories, workplace safety investigations, and wage-theft litigation attests ongoing coercive conditions. Behavioral economics (Kahneman, Ariely) documents how scarcity and desperation impair decision-making and bargaining. These attestations come from outside the beneficiary advocacy set — they are independent empirical findings.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint removes extraction from the employer domain: coercive wage suppression is no longer available as a business model. The transfer (public → low-wage workers) is a net flow toward those without resources, not extraction of surplus from productive activity. Suppression is low (0.18) because maintaining the floor requires no active coercion — it is a standing transfer, contestable at each budget cycle, not enforced through surveillance or violence. Theater is low (0.12) because the constraint's stated function (provide subsistence security) is exactly its structural function; there is no gap between appearance and operation. The measurement trajectory is flat-to-gently-rising: as coverage and benefit levels stabilize, slight creep occurs from administrative friction (means-testing pressure, benefit erosion due to inflation), but the core function remains stable. The constraint is fundamentally steady-state coordination, not a degrading piton or accumulating snare.
 *
 * PERSPECTIVAL GAP:
 *   The worker seats and the employer seats compute divergent types. Workers perceive genuine coordination (they gain agency; the constraint enables their participation in voluntary labor markets). Employers perceive a cost imposition (they lose coercive leverage and must raise wages or automation). The engine computes this correctly from the structural data: beneficiaries gain d toward coordination; payers bear a cost but not extraction from them personally — the extraction they lose is extraction they were exerting on workers, which the constraint removes. The public finance system bears a transfer cost (fiscal extraction) but this is democratic and non-coercive (legitimate taxation for redistributive purpose). The constraint is structurally separable: it is rope for workers (voluntary labor market coordination) and cost-imposition for employers (loss of coercive leverage), not snare for either.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers: beneficiaries with powerless/trapped-exit baseline. The constraint converts trapped → arbitrage (exit becomes real). Directionality toward coordination (d near 0.2). Precarious workers: same. Care performers: same — the constraint enables unpaid care as viable choice. Employers: powerful but constrained to the national labor market; they lose the coercive firing lever (a source of extraction they wielded) but do not become victims. The transfer is real (they pay upward wage pressure) but it is not extraction from them; it is the removal of extraction they were exerting. Directionality: d toward 0.4–0.5 (constrained cost-bearer, not target). Public finance: institutional, constrained to revenue instruments; the transfer is its primary function, not extraction. Directionality: near-symmetric (d near 0.5) — the cost is structural but manageable; no identity-lock. No directionality overrides needed; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy collapse. The founding problem (coercive labor markets due to subsistence insecurity) is live: low-wage work remains structurally coercive across documented cases. The disappearance verdict (world_rearranges) aligns with the persistence fact — if the floor disappeared, labor market coercion would return, confirming the constraint's function is active. Theater is low and stable (no performative decay) because the constraint's visible function (transfer + security) is its real function. The beneficiaries (low-wage workers) remain identifiable and gain documented agency; this is not a zombie constraint maintained by inertia. The payers (employers, public finance) remain clear about what they pay and why (wage pressure from exit options, progressive transfer). No mandatrophy signature fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_incentive_versus_freedom_empirics,
    'Does unconditional income support reduce work participation, and if so, is the reduction coercive dependency or voluntary exit from coercive labor?',
    'Longitudinal studies of basic income pilots and unconditional transfer programs measuring (1) labor force exit, (2) wage negotiation outcomes, (3) self-reported work satisfaction and coercion; (4) exit destination (further education, care work, entrepreneurship, or purely leisure). Distinguish voluntary exit from involuntary job loss.',
    'If reduction is concentrated in involuntary job loss or downward wage pressure, the freedom_floor reading is false — the constraint is not enabling exit, it is causing destitution disguised as choice. If reduction is concentrated in voluntary exit from coercive jobs, movement toward higher wages, or reallocation to care/community work, the reading holds. If no significant labor reduction occurs, both readings are weakly supported; other mechanisms dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(work_incentive_versus_freedom_empirics, empirical, 'Whether measured labor-participation changes reflect coercive dependency or voluntary exit from coercion.').

omega_variable(
    employer_wage_response_vs_substitution,
    'When workers gain exit options through income support, do employers raise wages to compete (freedom reading), suppress wages while relying on the floor (subsidy reading), or substitute labor with automation (partial dependency reading)?',
    'Quasi-experimental data from regions with and without income support, controlling for automation trends, labor demand, and regional economic structure. Measure wage changes at different percentiles, job quality metrics (hours, benefits, safety), and labor force composition.',
    'Wage increases support the freedom reading (workers negotiate from strength). Wage stagnation or decline despite labor scarcity supports the subsidy reading (floor is subsidizing low-wage employment). High automation substitution weakens both readings. Heterogeneous outcomes (different industries, regions, skill levels respond differently) would support a nuanced understanding of the constraint as locally contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(employer_wage_response_vs_substitution, empirical, 'Whether the constraint generates competitive wage response or subsidizes low-wage employment.').

omega_variable(
    care_work_decommodification_empirics,
    'Does income support enable unpaid or low-wage care work to expand, or does labor-market pressure push care workers into higher-wage sectors, leaving care undersupplied?',
    'Data on care-sector employment, wages, and quality; surveys of care-work performers on whether the floor enabled them to enter/stay in care; comparison of care quality and availability before/after support implementation.',
    'Expansion of care-work supply at lower cost suggests decommodification (freedom reading). Contraction or quality decline suggests labor is drawn out of care toward higher-wage sectors, contradicting the decommodification claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_work_decommodification_empirics, empirical, 'Whether the constraint enables care-work decommodification or reallocates care workers toward wage employment.').

omega_variable(
    coercion_internalization_residue,
    'Among workers who gain exit options through the income floor, does coercive labor psychology persist (internalized disciplinary norms, self-blame for poverty, shame about non-employment) despite structural escape?',
    'Qualitative research (interviews, focus groups) with income-floor recipients tracking (1) self-reported coercion and freedom, (2) willingness to refuse work, (3) persistence of internalized deservingness narratives. Measure psychological coercion separately from structural coercion.',
    'High persistence of internalized coercion despite structural exit suggests suppression is not purely structural — the constraint must work on both external barriers and internalized narrative to fully decommodify. If internalized coercion persists, the effective suppression is higher than the structural measure; workers with exit options still refuse to use them due to guilt or shame. This would mean the freedom reading overstates the decommodification achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_internalization_residue, empirical, 'Whether the constraint achieves decommodification structurally or is partially negated by internalized coercive psychology.').

omega_variable(
    administrative_exclusion_boundary,
    'What fraction of the low-wage labor force falls outside the institutional reach of the income support system (undocumented, rural, informal-sector, unregistered), and does the system''s exclusionary boundary create a secondary coercive stratum?',
    'Audit of institutional coverage (documentation requirements, spatial reach, registration barriers); comparative analysis of work coercion and exit options inside versus outside the covered population.',
    'High exclusion with low coercion outside suggests the constraint is not decommodifying the full labor market — it is creating a tiered system where covered workers gain freedom and excluded workers bear intensified coercion. This would mean the freedom_floor reading is partially true (true for the included) and partially false (the excluded bear the cost). The constraint would be partially a rope (for the included) and partially a snare (for the excluded).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_exclusion_boundary, empirical, 'Whether administrative boundaries create secondary coercive strata outside the income-support system.').

omega_variable(
    kernel_reading_boundary_contention,
    'Is the disaggregation of the income_support_conditionality kernel into three separate readings (freedom_floor, dependency_trap, wage_subsidy) analytically sound, or do the three readings describe overlapping mechanisms that operate simultaneously in the same system?',
    'Empirical test of the three readings'' predictions within the same population or system: (1) measure work participation changes; (2) measure wage outcomes; (3) measure work coercion/voluntariness. If all three show their predicted directions (participation down, wages up, work more voluntary = freedom_floor true; participation down, wages stagnant/down = wage_subsidy true; participation down, wages down, work satisfaction down = dependency trap true), then the readings describe overlapping effects in a single constraint, not separate constraints. If only one reading''s predictions hold, the decomposition is valid.',
    'If the readings describe simultaneous overlapping effects, each is a partial truth about one constraint, not a separate ε-invariant constraint. The ε-invariance principle would require reconsidering whether the three should be separate files. If one reading''s predictions hold and the others do not, the decomposition is valid and the separate constraint files are justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_contention, conceptual, 'Whether the kernel decomposition into three readings reflects genuine constraint diversity or is an analyst''s framing choice without structural grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(inco_tr_t0, projected).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__freedom_floor_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(inco_tr_t25, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__freedom_floor_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(inco_tr_t30, observed).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__freedom_floor_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(inco_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(inco_be_t0, projected).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.19).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.21).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__freedom_floor_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement_basis(inco_be_t25, observed).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__freedom_floor_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(inco_be_t30, observed).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__freedom_floor_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement_basis(inco_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement_basis(inco_su_t0, projected).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.17).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__freedom_floor_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement_basis(inco_su_t25, observed).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__freedom_floor_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(inco_su_t30, observed).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__freedom_floor_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(inco_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.18).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_market_coercion_baseline).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, employer_firing_power_leverage).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel income_support_conditionality. The freedom_floor_reading (THIS FILE) claims unconditional income support decommodifies labor by removing the coercive threat of starvation, enabling workers to refuse coercive employment. The dependency_trap_reading (separate constraint file) claims the same policy undermines work incentives and creates behavioral dependency. The wage_subsidy_reading (separate constraint file) claims the floor subsidizes low-wage employers by allowing them to pay less while workers subsist on the transfer. All three share the institutional object (unconditional income support) but instantiate different constraints with different ε values, beneficiary/victim structures, and types. The three readings coexist as live positions in public discourse; the empirical evidence would determine which reading's structural claim is closest to reality. Link all three via network.affects_constraints so that corpus analysis can test the readings against each other within a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
