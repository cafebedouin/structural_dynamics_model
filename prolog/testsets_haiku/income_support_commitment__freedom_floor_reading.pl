% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   contested kernel income_support_commitment. It models unconditional
 *   income support as a genuine coordination mechanism that solves the
 *   collective-action problem of funding a survival-level floor while
 *   simultaneously decoupling labor supply from desperation, enabling workers
 *   to exit exploitative conditions and caregivers to pursue socially
 *   necessary work the market does not price. The reading is structurally
 *   distinct from two sibling readings: dependency_trap_reading (which argues
 *   unconditional support creates work disincentive and state dependence) and
 *   targeting_efficiency_reading (which concentrates support on demonstrated
 *   need rather than universalizing it). Each reading has a different ε,
 *   beneficiary set, and type classification; they are not variations on one
 *   constraint but separate constraints grounded in different empirical and
 *   normative premises.
 *
 * KEY AGENTS:
 *   - Caregivers (childcare, elder care, disability): primary beneficiary whose unpaid essential work is not priced by markets; identity-locked in care obligations without support.
 *   - Precarious workers (gig, seasonal, low-wage): beneficiary whose constrained exit options give employers unilateral wage-setting power; support shifts bargaining equilibrium.
 *   - Abuse survivors: beneficiary whose material dependence on abusers is the actual barrier to exit; income floor is exit precondition.
 *   - Artists and entrepreneurs: beneficiary whose creative/business work cannot fund itself during development; support enables genuine risk-taking.
 *   - Employers and organized labor market: payer whose effective wage-setting power is constrained by workers' improved exit capacity; support raises the effective wage floor.
 *   - State apparatus: agenda-setter that administers the system, sets transfer level, and coordinates taxation and distribution.
 *   - Political factions holding sibling readings: excluded from this reading's stakeholder set; named as structural alternatives in the kernel contest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '44c98a27-27a2-4d65-8fc1-18dc3060b1ef').
narrative_ontology:cs_kernel_codification('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', formalized).
narrative_ontology:cs_authority_grounding('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', distributed).
narrative_ontology:cs_reading_relation('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', foundational, autonomy_requires_material_exit_capacity).
narrative_ontology:cs_axiom_status(autonomy_requires_material_exit_capacity, holdable).
narrative_ontology:cs_axiom_grounding('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', autonomy_requires_material_exit_capacity, deontological).
narrative_ontology:cs_axiom('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', foundational, unpaid_care_work_is_socially_necessary).
narrative_ontology:cs_axiom_status(unpaid_care_work_is_socially_necessary, holdable).
narrative_ontology:cs_axiom_grounding('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', unpaid_care_work_is_socially_necessary, conventional).
narrative_ontology:cs_axiom('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', secondary, labor_supply_shifts_with_bargaining_power).
narrative_ontology:cs_axiom_status(labor_supply_shifts_with_bargaining_power, holdable).
narrative_ontology:cs_axiom_grounding('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', labor_supply_shifts_with_bargaining_power, empirically_contingent).
narrative_ontology:cs_reference_frame('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', universal_survival_floor_enables_autonomy).
narrative_ontology:cs_drift_state('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', contemporary_neoliberal_austerity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('44c98a27-27a2-4d65-8fc1-18dc3060b1ef', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, educational_pursuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers_labor_market).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide essential unpaid care work (childcare, elder care, disability support) that the market does not price and society treats as non-negotiable obligation. Without income support, must choose between care responsibilities and wage labor, or accept extreme poverty. Unconditional support decouples care provision from market demand, recognizing care as socially necessary work.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, identity_locked, national).

% Work in gig, seasonal, or low-wage unstable employment where individual bargaining power is minimal. Without income floor, must accept exploitative terms to survive; employer wage-setting power depends on worker desperation. Support enables refusal of abusive conditions and negotiation for better terms.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Trapped in economic dependence on abusers because leaving means destitution. Income support provides material basis for exit without requiring family reinvolvement, shelter system reliance, or return to abuser for economic survival. Floor is precondition for safety, not optional supplement.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, biographical, identity_locked, national).

% Pursue creative work or business development that generates no income during gestation phase. Without support, forced into wage work that crowds out time/energy for creative development or startup work. Support enables genuine risk-taking and skill development inaccessible to survival-mode workers.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, constrained, national).

% Pursue education, skill-building, or career transition requiring full-time study without immediate income. Low-income students especially cannot afford tuition + living costs from wage work. Support decouples education access from family wealth, enabling human capital investment across class boundaries.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, educational_pursuers, beneficiary,
    moderate, biographical, constrained, national).

% Finance unconditional income through taxation (corporate, income, or consumption taxes). The support floor eliminates employers' unilateral wage-setting power by expanding workers' exit options — workers can refuse exploitative terms because survival is decoupled from employment. This shifts bargaining power back toward labor, raising real wage floors.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers_labor_market, payer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, employers_labor_market, observer).

% Fund the income floor through general taxation. For lower-income taxpayers, the cost-benefit is often favorable (they receive the same floor). For high-income taxpayers, effective transfer is upward through the fiscal system. Constraint operationalizes a collective funding decision; cost bearers are identified by tax structure, not by the constraint itself.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Under means-tested regimes, subjected to stigma, surveillance, behavioral conditions, and poverty traps (clawback of benefits as income rises). If universal income support replaced means testing, they would gain dignity, privacy, and freedom from paternalistic controls. Their voice in this reading is muted — the reading does not address them as present stakeholders but structures them out.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, means_tested_welfare_dependents, excluded,
    powerless, biographical, trapped, national).

% Administers the income support system, sets transfer level, determines eligibility (universal vs. targeted), and establishes enforcement mechanisms. In this reading, the state's role is to fund and deliver coordination (pooled risk, universal coverage) and to constrain the market power that would otherwise force survival-mode labor.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for work-incentive design and conditional support, arguing unconditional transfers reduce labor supply and create state dependence. From the freedom_floor_reading position, this constituency is excluded from the conversation; it is articulated as the sibling dependency_trap_reading in the kernel contest.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, dependency_trap_reading_constituency, excluded,
    institutional, generational, mobile, national).

% Advocates for means-tested, targeted income support concentrated on demonstrated need. From the freedom_floor_reading position, this constituency is excluded; it is articulated as the sibling targeting_efficiency_reading in the kernel contest.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, targeting_efficiency_reading_constituency, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of funding a survival-level income floor (pooled risk, universal coverage, tax coordination) and decouples labor supply from survival need — enabling workers to exit exploitative conditions and enabling caregivers to pursue socially necessary work the market does not price but society depends on.
% TRANSFER_FUNCTION: Transfers income from employed workers and capital holders (via taxation) to individuals whose labor (paid or unpaid) does not generate survival income, or whose participation in paid labor is curtailed by care obligations, education pursuits, or exit from exploitation.
% ABSENT_VOICES: Means-tested welfare recipients (excluded by universality's design, treated as a solved problem), employers who benefit from desperation-driven wage suppression (outside the conversation in this reading), and the dependency_trap_reading and targeting_efficiency_reading communities (articulated as sibling readings, not present stakeholders).
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared overnight, labor markets would reorganize immediately around desperation: wage floors would collapse, caregiving would exit to family-only provision or disappear, abuse survivors would lose escape capacity, creative work and education would become luxury goods for the wealthy, and workers' bargaining power would vanish. The material preconditions for autonomy would evaporate.
% FOUNDING_PROBLEM: Markets do not price essential unpaid care work; wage labor coerces participation through survival threat; workers trapped in exploitative conditions cannot refuse them without risking destitution; caregiving responsibilities and creative/educational pursuits are crowded out by survival-mode wage work.
% FOUNDING_PROBLEM_CORROBORATION: Academic literature on care work economics (Folbre, Hochschild), labor historians documenting desperation-driven wage acceptance (Piven & Cloward), abuse researchers attesting exit barriers (National Domestic Violence Hotline), and independent economists modeling labor-supply elasticity and bargaining power shifts (Bivens, Rodrik) all corroborate the founding problem from outside the direct beneficiary communities. Policy jurisdictions implementing unconditional income pilots (Finland, Kenya, Kenya GiveDirectly, Stockton SEED) provide observational evidence of behavioral shifts.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint solves a genuine coordination problem (pooled risk, universal coverage) without creating asymmetric extraction — beneficiaries genuinely gain autonomy and dignity, not just rents. There are no victims in this reading (universality eliminates means-test stigma and surveillance). Suppression is minimal (0.12) because the constraint relies on voluntary participation and taxation, not coercion; the 'suppression' that exists is political friction from competing readings, not inherent to the arrangement itself. Theater ratio is very low (0.08) because the performance of the system tracks its actual function — income delivery, autonomy enablement, labor-market shift. The measurement series shows slight upward drift in all three metrics as the system faces political pressure and implementation complexity, but the core magnitudes remain stable and low, consistent with a genuine rope classification. All metrics are authored on the shared time grid so no backward injection occurs.
 *
 * PERSPECTIVAL GAP:
 *   The freedom_floor_reading describes a scenario where employers and organized labor see the constraint as raising wage floors and reducing their unilateral power (d toward target end, χ upward). Beneficiary seats (caregivers, precarious workers) experience it as liberation and dignity restoration (d toward beneficiary end, χ negative or near-zero). The state sees it as coordination infrastructure it maintains. The dependency_trap_reading and targeting_efficiency_reading constituencies would experience this same system as either harmful (undermining work motivation) or wasteful (supporting those not needing support), producing divergent d and χ values from the same structural arrangement. The engine computes these seat-level differences from the beneficiary/victim declarations and power atoms; this commentary explains why the divergence is structural and expected.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality is set by the explicit beneficiary declaration (caregivers, precarious workers, abuse survivors, artists, educational pursuers) combined with their improved exit options: support provides them genuine alternatives to desperation-driven labor. Employer directionality is set by constraint payership (taxes funding the system) and institutional power that is constrained by workers' improved bargaining position. Taxpayers sit on the payer axis but lower-income taxpayers are also beneficiaries (they receive the floor), so their net d is near symmetric; high-income taxpayers bear net transfer costs and have d shifted toward target. No directionality overrides are necessary; the structural data (beneficiary/victim + power + exit options) produces honest derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (markets don't price care, desperation suppresses wages, exit barriers trap workers) is live and observable. The constraint's function (enabling exit, funding care, shifting bargaining power) remains continuous with its founding problem — no mandatrophy. The constraint could degrade if: (1) political pressure to means-test it returned (moving toward dependency_trap or targeting_efficiency readings), (2) transfer levels fell below survival, or (3) enforcement shifted to behavioral conditions and surveillance. These would indicate drift toward snare or piton, not mandatrophy in the freedom_floor_reading itself. As authored, the constraint remains in coherent operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_reading_contest,
    'Do unconditional income transfers reduce labor supply (dependency_trap_reading hypothesis) or increase bargaining power and selective exit capacity without reducing overall work (freedom_floor_reading hypothesis)?',
    'Randomized controlled trials with sufficient follow-up time (3+ years) measuring labor participation, wage outcomes, and occupational mobility. Pilot programs: Finland 2017–2018, Kenya GiveDirectly 2016–2019, Stockton SEED 2019–2021. Mechanism: track separable outcomes (hours worked vs. job quality, wage acceptance vs. employment rate, creative/educational pursuit vs. wage labor exit).',
    'If labor supply falls significantly and wages do not rise, dependency_trap_reading gains empirical support and the freedom_floor_reading''s core claim (exit capacity improves bargaining) is undermined. If labor supply is stable or shifts toward higher-quality work and wages rise, freedom_floor_reading is empirically vindicated and dependency_trap_reading is falsified. Targeting_efficiency_reading would survive regardless — efficiency is about distribution, not supply elasticity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_supply_elasticity_reading_contest, empirical, 'Whether unconditional income reduces or reshapes labor supply; the empirical pivot for freedom_floor vs dependency_trap.').

omega_variable(
    care_work_valuation_and_market_pricing,
    'Is unpaid care work (childcare, elder care, disability support) genuinely non-priced by markets, or do alternative market arrangements exist and fail only due to undersupply of demand?',
    'Comparative analysis of care-work labor markets across jurisdictions: where care work is commodified (private childcare, for-profit elder care), do prices equilibrate at livable wages, or do they stabilize at poverty wages due to consumer inability to pay + care-giver desperation? Compare to professionalized care (nursing, therapy) where licensing and training investment raises wages. Empirical question: what market conditions would be required for unpaid care work to become priced care work at living wages?',
    'If care work IS genuinely non-priceable (asymmetric information, demand-side poverty, love''s labor dynamics), the founding problem of the freedom_floor_reading is structural and income support is necessary coordination. If care work IS priceable but merely undersupplied or underpaid due to bargaining asymmetry, alternative solutions (professional licensing, minimum care standards) might suffice without universal income support, weakening the reading''s claim to necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_work_valuation_and_market_pricing, empirical, 'Whether unpaid care work is structurally non-priceable or merely undersupplied/underpaid.').

omega_variable(
    universality_vs_targeting_dignity_tradeoff,
    'Does the means-testing required by targeting_efficiency_reading produce unavoidable stigma and surveillance harms, or can means-testing be administered with privacy and dignity maintained?',
    'Ethnographic and survey data from means-tested welfare recipients on stigma, behavioral compliance burden, and privacy intrusion. Comparison: universal programs (Social Security) vs. targeted programs (TANF, food stamps) on reported dignity and autonomy. Mechanism: track both direct stigma (perceived judgment) and structural stigma (administrative surveillance, benefit clawback design).',
    'If means-testing creates unavoidable dignity harm, targeting_efficiency_reading''s cost-benefit calculus must account for hidden costs of administration and psychological burden, possibly favoring universality. If means-testing can be redesigned to minimize stigma, targeting_efficiency_reading remains viable as an alternative reading. The core tradeoff — efficiency vs. dignity — remains unresolved empirically; this omega documents where the readings'' empirical premises diverge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_targeting_dignity_tradeoff, empirical, 'Whether means-testing''s administrative costs (dignity, surveillance, psychological burden) are unavoidable or redesignable.').

omega_variable(
    founding_problem_status_contested,
    'Is the founding problem (markets don''t price care, desperation suppresses wages, exit barriers trap workers) genuinely live, or has market evolution and labor-law development substantially solved it?',
    'Longitudinal data: care-work wages over time, labor-market desperation indicators (gig work growth, union decline, wage stagnation in low-income brackets), domestic violence shelter waiting lists and exit barriers, creative-work and educational-pursuit participation by class. If all indicators improve substantially, founding problem is dead; if stagnant or worsening, it is live. Contested status reflects partisan disagreement on what counts as ''solved''.',
    'If founding problem is dead, the constraint becomes mandatrophic or pitonized — it persists as administrative inertia or ideology rather than solving an active coordination problem. If contested, mandatrophy is contested (one reading says solved, another says live) — the constraint''s necessity is politically mediated. This resolves the R5 genealogy question: founding_problem_status determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_contested, empirical, 'Whether the founding problem (markets don''t price care, desperation suppresses wages) is live or dead.').

omega_variable(
    kernel_reading_underdetermination,
    'Do the three sibling readings (freedom_floor, dependency_trap, targeting_efficiency) partition the conceptual space exhaustively, or do additional readings exist that the framing has missed?',
    'Genealogical analysis of income support policy debates: are there distinct policy positions or normative framings that cannot be mapped to one of the three readings? Examples: (1) libertarian reading (income support violates property rights and state coercion), (2) communitarian reading (income support only works embedded in reciprocal obligation), (3) ecological reading (income support enables sustainability transitions away from consumption-maximizing work). If new readings emerge with distinct ε and beneficiary sets, the kernel is under-partitioned.',
    'If the three readings are exhaustive, the kernel contest is fully specified and the generative model is complete. If new readings exist, the kernel complexity is higher and the constraint family is larger. This does not change THIS constraint''s classification but affects how it relates to the broader income-support ecosystem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the three kernel readings exhaust the normative conceptual space or additional readings exist.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.12) entirely structural (political/institutional resistance to universality) or partially internalized (beneficiaries internalize dependency fears, caregivers internalize shame at claiming support, precarious workers internalize desperation as personal failure)?',
    'Post-implementation qualitative research on beneficiary self-conception and dignity shift: do beneficiaries'' subjective framing of support change when it is framed as universal right vs. means-tested welfare? Do caregivers'' sense of legitimacy and self-worth respond to recognition of care work? Do precarious workers'' bargaining confidence increase with income floor? Mechanism: compare beneficiary narratives in universal vs. targeted programs on internalized shame/legitimacy.',
    'If suppression is mostly structural, the measured 0.12 is accurate. If partially internalized, the true suppression experienced by beneficiaries is higher until internalized shame dissolves (which may take multiple years post-implementation). Internalization suggests beneficiaries carry suppression even when structural barriers are removed — the constraint''s true persistence lies in absorbed norms, not just institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural (institutional resistance) or partially internalized (beneficiary shame, desperation norm).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__freedom_floor_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__freedom_floor_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__freedom_floor_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(inco_tr_t35, income_support_commitment__freedom_floor_reading, theater_ratio, 35, 0.08).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__freedom_floor_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__freedom_floor_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__freedom_floor_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(inco_be_t35, income_support_commitment__freedom_floor_reading, base_extractiveness, 35, 0.18).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__freedom_floor_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__freedom_floor_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(inco_su_t25, income_support_commitment__freedom_floor_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(inco_su_t35, income_support_commitment__freedom_floor_reading, suppression_requirement, 35, 0.12).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel has three structurally distinct readings. This story (freedom_floor_reading) is the core universal/dignity framing, emphasizing autonomy and labor-market exit capacity. The sibling readings (dependency_trap_reading and targeting_efficiency_reading) decompose the contested kernel into separate constraints with different ε values, beneficiary sets, and empirical premises. This family requires all three stories linked via network.affects_constraints to capture the full kernel contest. The freedom_floor_reading influences both siblings by establishing the universality framing and dignity claim that they must either defend or counter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
