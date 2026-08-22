% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Labor Decommodification (Freedom Floor Reading)
 *   domain: political_economy/labor/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the FREEDOM_FLOOR_READING of the
 *   income_support_conditionality kernel. Under this reading, unconditional
 *   income support operates as a decommodification mechanism: it removes the
 *   coercive threat of destitution that forces workers to accept subsistence
 *   wages and degrading conditions. The reading contests the
 *   dependency_trap_reading (which frames UIS as creating incentive collapse)
 *   and the wage_subsidy_reading (which frames UIS as employer subsidy
 *   enabling wage suppression). The freedom-floor reading asserts that by
 *   guaranteeing survival independent of wage acceptance, UIS shifts the
 *   constraint from a coercive snare (desperate workers have no exit) to a
 *   coordination rope (all workers gain genuine choice). This is a committed
 *   position: employers lose coercive power; low-wage workers exit the victim
 *   set; the bargaining ecology reorganizes around choice rather than
 *   desperation. The constraint described here is what the world looks like
 *   IF this reading is correct about the structural effects of UIS.
 *
 * KEY AGENTS:
 *   - Low-wage workers: Primary beneficiaries — move from trapped to mobile exit options
 *   - Employers reliant on wage suppression: Enter victim set — lose coercive firing power and desperation-pricing advantage
 *   - Precarious and care workers: Beneficiaries — gain negotiating power and choice about work conditions
 *   - Public fiscal authority: Agenda-setter — administers the income floor; maintains universality and unconditionality
 *   - Political opponents of decommodification: Excluded — would contest the reading and defend wage-suppression economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.28).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Labor Decommodification (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/labor/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '2b064169-448f-4370-b817-17e39d58ca74').
narrative_ontology:cs_kernel_codification('2b064169-448f-4370-b817-17e39d58ca74', distributed).
narrative_ontology:cs_authority_grounding('2b064169-448f-4370-b817-17e39d58ca74', distributed).
narrative_ontology:cs_reading_relation('2b064169-448f-4370-b817-17e39d58ca74', income_support_conditionality__dependency_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('2b064169-448f-4370-b817-17e39d58ca74', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('2b064169-448f-4370-b817-17e39d58ca74', foundational, labor_decommodification_via_unconditional_income).
narrative_ontology:cs_axiom_status(labor_decommodification_via_unconditional_income, holdable).
narrative_ontology:cs_axiom_grounding('2b064169-448f-4370-b817-17e39d58ca74', labor_decommodification_via_unconditional_income, deontological).
narrative_ontology:cs_axiom('2b064169-448f-4370-b817-17e39d58ca74', foundational, worker_exit_power_eliminates_coercive_pricing).
narrative_ontology:cs_axiom_status(worker_exit_power_eliminates_coercive_pricing, holdable).
narrative_ontology:cs_axiom_grounding('2b064169-448f-4370-b817-17e39d58ca74', worker_exit_power_eliminates_coercive_pricing, empirically_contingent).
narrative_ontology:cs_axiom('2b064169-448f-4370-b817-17e39d58ca74', secondary, positive_freedom_requires_material_decommodification).
narrative_ontology:cs_axiom_status(positive_freedom_requires_material_decommodification, holdable).
narrative_ontology:cs_axiom_grounding('2b064169-448f-4370-b817-17e39d58ca74', positive_freedom_requires_material_decommodification, deontological).
narrative_ontology:cs_reference_frame('2b064169-448f-4370-b817-17e39d58ca74', decommodified_labor_market_with_universal_income_floor).
narrative_ontology:cs_drift_state('2b064169-448f-4370-b817-17e39d58ca74', political_contestation_present, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2b064169-448f-4370-b817-17e39d58ca74', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, care_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, community_members_reducing_coerced_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_reliant_on_wage_suppression).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, positive_freedom_doctrine).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, decommodification_principle).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, exit_option_equality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers earning subsistence or below can now refuse exploitative wages, coercive schedules, and degrading conditions without facing immediate destitution. The income floor provides genuine exit power: they can leave abusive employment, retrain, care for dependents, or negotiate from a position of reduced desperation. Their structural position shifts from trapped-or-constrained to mobile.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Gig workers, temp workers, and others with unstable employment gain a buffer against income volatility. They can refuse exploitative piece-rate assignments, unreasonable shift demands, and discriminatory treatment without risking homelessness. The income floor stabilizes their negotiating position.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, mobile, national).

% Domestic workers, childcare providers, elder-care workers, and other care laborers—often unpaid or underpaid—can now do this work by choice rather than necessity. The income floor decouples survival from wage acceptance, allowing care work to be valued and compensated fairly or performed voluntarily without coercion.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, care_workers, beneficiary,
    powerless, biographical, mobile, national).

% Broader society benefits from reduced coercive labor dynamics: workers have genuine choice in labor participation, which shifts social norms away from survival-driven desperation. Communities experience less labor trafficking, exploitation, and dignity harm when labor power is not commodified at the subsistence threshold.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, community_members_reducing_coerced_labor, beneficiary,
    moderate, biographical, mobile, national).

% Employers who have structured business models around access to desperate workers now face genuine wage pressure and worker choice. They can no longer rely on worker desperation to accept poverty wages, dangerous conditions, or arbitrary firing. Their extraction mechanism—coercive labor pricing via subsistence vulnerability—is dismantled. They must compete for workers on terms that reflect labor's actual value.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_reliant_on_wage_suppression, payer,
    organized, biographical, constrained, national).

% Workers with scarce skills and strong labor-market position are largely unaffected by the income floor—they already had genuine exit options. They observe as the structural position of powerless workers shifts toward something resembling their own negotiating freedom.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, high_skill_workers_with_exit_options, observer,
    powerful, biographical, arbitrage, national).

% The state or regional authority administers and funds the unconditional income floor. It sets the payment level, eligibility (typically universal or near-universal), and enforcement (typically automatic, not means-tested or behavioral-conditioned). The authority is implementing a decommodification policy.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, public_fiscal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Political parties, ideological movements, and employer coalitions that defend wage-suppression economics are structurally excluded from the policy design. They would argue that decommodification creates dependency and inflation; their voice would reframe the constraint as a snare rather than a rope. They are outside the beneficiary coalition that established and maintains the policy.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, political_opponents_of_decommodification, excluded,
    powerful, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common income floor below which no person falls, solving the collective-action problem of labor-market desperation: when workers face starvation, they underbid each other on wages and conditions, driving a race to the bottom that harms all workers and concentrates power in employers. The income floor coordinates all workers around a shared exit option, shifting the bargaining dynamic from desperation to dignity.
% TRANSFER_FUNCTION: Moves fiscal resources from the state (or from progressive taxation) to all residents at or below an income floor, creating a non-commodity income stream. The transfer is NOT from employers to workers (no direct employment relation), but from collective fiscal capacity to individuals, decoupling survival from wage negotiation.
% ABSENT_VOICES: Employers structured around low-wage extraction are excluded from the policy coalition that designed and maintains it. Political opponents—libertarian, neoclassical, or wage-competition-focused ideologies—would argue the policy creates dependency and inflation, but are kept outside the beneficiary coalition. Workers in other policy regimes (conditional welfare, wage-subsidy frameworks) would report different experiences and would challenge the freedom-floor framing as incomplete without other protections.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared overnight, labor markets would immediately revert to subsistence-driven desperation: workers would accept lower wages, worse conditions, and coercive employer behavior; employer power would reconcentrate; social norms would shift back toward survival-justified labor acceptance; care work would become unpaid or underpaid again; precarious workers would lose negotiating power. The entire bargaining ecology would reorganize around renewed labor commodification.
% FOUNDING_PROBLEM: Labor power is the only asset most people own; when labor markets clear at subsistence wages, workers are forced to sell that asset under coercive conditions (accept any wage or face homelessness). This commodification of labor at desperation prices is structurally extractive: employers capture the surplus of worker productivity minus subsistence, which maximizes employer power and minimizes worker freedom. The founding problem is the commodification of labor itself—treating labor as a pure market good subject to price floors set by survival necessity rather than as an expression of human capability and choice.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists document persistent wage suppression in low-skill sectors (Autor, Katz); labor-rights organizations document coercive conditions enabled by desperation (Human Rights Watch, ILO); workers themselves attest in testimony that fear of homelessness drives acceptance of exploitation. The problem is corroborated outside the beneficiary coalition by independent researchers and international labor standards bodies.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.28 end-point, 0.32 start) under this reading because the income floor is REMOVING extraction, not imposing it. The constraint's function is to eliminate the prior extraction mechanism (coercive labor pricing). Suppression is also LOW (0.15) because the policy operates by expanding choices (exit options), not by coercion. Theater is minimal (0.12) because the mechanism is straightforward and functional: pay the income floor, enforce universality, prevent means-testing creep. Accessibility collapse is moderate (0.42) because even with an income floor, workers still face real tradeoffs (opportunity cost of time, social pressure, skill requirements for preferred work) — the floor eliminates desperation but not all friction. Resistance is moderate (0.38) because employers and wage-suppression ideologies mount real opposition; the policy persists only if the beneficiary coalition maintains political will. The measurement trajectory shows slight downward drift in extractiveness (as the floor's decommodifying effect stabilizes) and slight upward drift in theater (as compliance and benefit-distribution bureaucracy develops). Measurements are projected (future-oriented) because this reading's full effects depend on policy implementation and sustained political support.
 *
 * PERSPECTIVAL GAP:
 *   The employer seat and the worker seat compute dramatically differently. From the employer's perspective using traditional labor-market analysis, the income floor raises labor costs and reduces their bargaining power (they experience the constraint as a floor imposed on their market freedom). From the low-wage worker's perspective, the income floor PROVIDES freedom — it eliminates the coercive threat of destitution and allows genuine choice. From the political-opponent seat, the policy appears as an inflationary, dependency-creating transfer; from the beneficiary seats, it appears as liberation from commodification. The engine computes these divergent perceptions from the structural data: employers and workers have opposite directionalities (d near 1.0 for employers forced to compete, d near 0.0 for workers gaining choice), which produce opposite type classifications from the same constraint structure. This divergence is NOT an error — it is the point: the same policy-mechanism produces fundamentally different constraint-types depending on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers: d = 0.0-0.15 (full beneficiary range). They move from trapped/identity-locked exit (desperation bounds them to any wage) to mobile exit (income floor provides genuine refusal power). The constraint benefits them directly and unconditionally. Precarious and care workers: similar, d near 0.1. Employers reliant on wage suppression: d = 0.85-1.0 (full target range). They lose their extraction mechanism (coercive labor pricing via desperation); they must compete on terms reflecting labor's actual value rather than subsistence minimum. Fiscal authority: d = 0.5 (symmetric). They administer a coordination mechanism that benefits workers but costs resources. High-skill workers: d = 0.5 (largely unaffected; already had choice). Political opponents: d = 0.7+ (they are constrained — their preferred policy regime is foreclosed). No directionality overrides are required; the structural derivation from beneficiary/victim declarations and exit-option shifts produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap because the founding problem (labor commodification at desperation prices) remains LIVE and the policy directly addresses it. The constraint's function (decommodification via income floor) has not atrophied or become theatrical — it is the mechanism itself. Theater ratio stays low because the policy mechanism is unambiguous: if the income floor exists and is unconditional, the decommodifying effect is automatic. The risk of mandatrophy enters only if political pressure reduces the income level below subsistence (means-testing creeps back in, or inflation erodes the floor's value without adjustment) — at that point, the constraint becomes performative (theater_ratio rises) without the decommodifying function. The six-questions mismatch test (founding_problem_status=live + disappearance_verdict=world_rearranges) supports the constraint as functionally essential, not theatrical. Mandatrophy would be signaled by rising theater_ratio + stable/rising suppression_requirement without corresponding functional change — not present in this trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_suppression_vs_increased_productivity,
    'When workers gain exit power via income support, do employers respond by suppressing wages to compensate for increased labor costs, or by investing in productivity to compete for workers on non-wage terms?',
    'Wage and productivity data from jurisdictions with long-standing UIS programs: do wages stagnate or rise? Do firms invest in working conditions, training, and technology? Do labor-market outcomes improve or degrade?',
    'If employers suppress wages, the policy partially offsets its own effect (wages stay low, just less coercively so) — the wage-subsidy reading gains empirical support. If employers invest in productivity, the freedom-floor reading holds — workers gain real choice AND wage gains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_suppression_vs_increased_productivity, empirical, 'Whether employer response to labor scarcity is wage suppression or productivity investment.').

omega_variable(
    behavioral_response_labor_supply,
    'When workers gain unconditional income support, does their labor supply decrease (dependency-trap reading) or shift to genuinely chosen work (freedom-floor reading)? What work do they choose?',
    'Labor-force participation, work-type transitions, and job-quality data from pilot programs: do participation rates decline or remain stable? Do workers exit coercive sectors and enter chosen sectors? Does unpaid care work increase?',
    'If labor supply declines substantially, the dependency-trap reading receives support — the constraint may function as snare rather than rope. If labor supply remains stable but composition shifts toward less-coercive work and more care work, the freedom-floor reading holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_response_labor_supply, empirical, 'Whether reduced wage coercion produces labor-supply collapse or work reallocation.').

omega_variable(
    inflation_and_real_floor,
    'Does the real purchasing power of the income floor erode over time due to inflation, political erosion, or means-testing creep? Does the policy maintain genuine decommodification, or does the floor sink back toward subsistence?',
    'Long-run tracking of real floor value, indexation practices, and political pressure for conditionality; comparison of nominal increases to wage and cost-of-living growth.',
    'If the floor erodes in real terms, the constraint''s decommodifying function atrophies — mandatrophy risk. Theater ratio rises (policy performs decommodification without substance) while extractiveness returns to prior levels. The constraint becomes piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_and_real_floor, empirical, 'Whether income floor maintains real decommodifying power or erodes toward performative status.').

omega_variable(
    kernel_reading_irresolvability,
    'Are the three readings of the income_support_conditionality kernel logically forecloses (mutually exclusive in a single framework), or merely coexisting policy positions that reflect different empirical beliefs?',
    'Clarify the underlying disagreement: is it a dispute about FACTS (what actually happens when UIS is implemented) or about DEFINITIONS (what counts as freedom, dependency, subsidy)? If empirical, the readings remain contestable and can coexist. If definitional, one reading forecloses others.',
    'If empirical, the three readings should be treated as rival hypotheses for a single kernel, all authored as separate constraint stories, empirically testable. If definitional, one reading forecloses the others in any coherent framework — the corpus should reflect that structure by marking one as overcoded/foreclosed (never authoring it as a live competitor). This omega is CONCEPTUAL rather than empirical because it concerns the nature of the disagreement itself, not a factual claim within any single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_irresolvability, conceptual, 'Whether the three income_support readings are empirically contestable or logically foreclosed.').

omega_variable(
    exit_option_measurement_under_partial_implementation,
    'If income support is implemented with barriers (means-testing, conditionality, eligibility gaps, below-subsistence amount), does the constraint still provide genuine exit power, or does partial implementation collapse the decommodifying mechanism?',
    'Comparative analysis of UIS programs on spectrum from unconditional/universal to conditional/means-tested: do exit options remain mobile under partial implementation? Do workers still refuse coercive wages, or does conditionality recreate desperation?',
    'If partial implementation preserves exit power, the freedom-floor reading is robust to implementation variation. If exit power collapses when implementation becomes conditional, the reading''s claim to decommodification is implementation-contingent — the constraint type may be snare (for conditionally-excluded workers) and rope (for included workers) simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_measurement_under_partial_implementation, empirical, 'Whether genuine decommodification requires fully unconditional and universal income support, or can survive partial/conditional implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(inco_tr_t0, projected).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(inco_tr_t5, projected).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(inco_tr_t10, projected).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(inco_tr_t15, projected).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(inco_tr_t20, projected).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__freedom_floor_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(inco_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(inco_be_t0, projected).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement_basis(inco_be_t5, projected).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(inco_be_t10, projected).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(inco_be_t15, projected).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(inco_be_t20, projected).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__freedom_floor_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(inco_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(inco_su_t0, projected).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.16).
narrative_ontology:measurement_basis(inco_su_t5, projected).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(inco_su_t10, projected).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement_basis(inco_su_t15, projected).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(inco_su_t20, projected).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__freedom_floor_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(inco_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_coercion_via_subsistence_threat).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, employer_power_over_worker_bargaining).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel income_support_conditionality. The kernel question is: 'What does unconditional income support DO to labor markets and worker freedom?' This reading (freedom-floor) claims it DECOMMODIFIES labor by removing the coercive threat of destitution. Sibling readings claim it creates DEPENDENCY (undermining incentives) or functions as EMPLOYER SUBSIDY (enabling wage suppression). The three readings have irreconcilable beneficiary/victim structures: freedom-floor removes employers from coercive power (employers enter victim set); dependency-trap frames the policy itself as creating victims; wage-subsidy frames workers as collateral victims of state-enabled wage suppression. The three readings should be authored as separate constraint stories, each with its own ε, beneficiary/victim structure, and type classification. The network links show dependencies: wage-subsidy reading may be empirically nested (a real outcome) within the freedom-floor mechanism if employers respond to labor scarcity by suppressing wages; dependency-trap reading contests the behavioral assumptions (workers may withdraw from labor supply) that freedom-floor assumes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
