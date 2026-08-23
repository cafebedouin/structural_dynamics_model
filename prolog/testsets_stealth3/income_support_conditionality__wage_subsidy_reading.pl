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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy (Wage-Incidence Reading)
 *   domain: political economy / social policy / labor economics
 *
 * SUMMARY:
 *   An unconditional income support scheme pays every resident a regular cash
 *   grant with no employment condition, funded from general taxation. This
 *   story instantiates ONE reading of that arrangement — the wage-subsidy
 *   reading — under which the floor operates inside market wage-setting:
 *   because recipients can survive without accepting any given offer,
 *   equilibrium wage offers drift down toward the floor, and a growing share
 *   of the transfer's value converts into employer margin while workers
 *   remain employed at below-subsistence wages topped up by the state.
 *   Epsilon's referent is the STANDING arrangement — unconditional income
 *   support operating inside market wage-setting — assessed by this reading's
 *   incidence-focused lights; the reading's endorsed alternative (a floor
 *   paired with bargaining institutions) is not the referent and contributes
 *   nothing to the scores. Per Rule 1, the sibling readings are not described
 *   inside this constraint's body: freedom_floor_reading and
 *   dependency_trap_reading are separate files with their own epsilon,
 *   beneficiary/victim structure, and classification, linked only through
 *   network.affects_constraints, and the disagreement between readings is
 *   routed to omega variables rather than averaged into any score. The
 *   claimed_type and the metrics are independent authored facts: this reading
 *   judges tangled_rope to be structurally true — a genuine
 *   subsistence-coordination function carrying an asymmetric employer-capture
 *   function through the same payment rail, requiring active political and
 *   administrative maintenance to hold — while the metric values describe the
 *   arrangement's observed operation, and any divergence between claim and
 *   computed per-seat types is exactly the data this corpus exists to take.
 *
 * KEY AGENTS:
 *   - - low_wage_employers: Primary beneficiary (powerful/arbitrage) — captures transferred value via downward wage adjustment; campaigns to cap the payment below living-wage levels
 *   - - low_wage_workers: Primary target (powerless/trapped) — subsistence maintained but wage growth suppressed; collective wage action thinned
 *   - - general_taxpayers: Secondary target (moderate/constrained) — fund the full transfer; net gains accrue elsewhere
 *   - - non_labor_recipients: Genuine beneficiary (powerless/trapped) — caregivers, students, disabled recipients outside wage labor; largest measured wellbeing gains
 *   - - social_protection_authority: Agenda setter (institutional/constrained) — sets payment level, administers disbursement, politically locked into continuance
 *   - - labor_unions: Excluded voice (organized/constrained) — would pair the floor with bargaining institutions; sidelined in pilot design
 *   - - labor_economists: Analytical observer (analytical/analytical) — measure incidence; collect no rents from either verdict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.71).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.55).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy (Wage-Incidence Reading)").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political economy / social policy / labor economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '20969a92-a09c-43f6-b25f-69c993debf94').
narrative_ontology:cs_kernel_codification('20969a92-a09c-43f6-b25f-69c993debf94', distributed).
narrative_ontology:cs_authority_grounding('20969a92-a09c-43f6-b25f-69c993debf94', distributed).
narrative_ontology:cs_reading_relation('20969a92-a09c-43f6-b25f-69c993debf94', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('20969a92-a09c-43f6-b25f-69c993debf94', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('20969a92-a09c-43f6-b25f-69c993debf94', foundational, transfer_incidence_capitalizes_into_wages).
narrative_ontology:cs_axiom_status(transfer_incidence_capitalizes_into_wages, holdable).
narrative_ontology:cs_axiom_grounding('20969a92-a09c-43f6-b25f-69c993debf94', transfer_incidence_capitalizes_into_wages, empirically_contingent).
narrative_ontology:cs_axiom('20969a92-a09c-43f6-b25f-69c993debf94', secondary, income_floor_weakens_wage_pressure).
narrative_ontology:cs_axiom_status(income_floor_weakens_wage_pressure, holdable).
narrative_ontology:cs_axiom_grounding('20969a92-a09c-43f6-b25f-69c993debf94', income_floor_weakens_wage_pressure, empirically_contingent).
narrative_ontology:cs_reference_frame('20969a92-a09c-43f6-b25f-69c993debf94', wage_integrated_subsistence_floor).
narrative_ontology:cs_drift_state('20969a92-a09c-43f6-b25f-69c993debf94', contemporary_pilot_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20969a92-a09c-43f6-b25f-69c993debf94', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, non_labor_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, flexible_low_wage_labor_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the annual payment level, runs enrollment and disbursement, and defends the program's budget line in every legislative cycle. Once payments began flowing, suspension became politically unthinkable for any sitting government, so the authority now manages and defends the arrangement rather than choosing among alternatives. Its administrative budget and staffing expand with the program.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, social_protection_authority, agenda_setter,
    institutional, generational, constrained, national).

% Staff warehouses, care homes, farms, fast-food chains, and delivery networks with hourly labor. Because applicants can survive on the unconditional payment, wage offers have drifted down toward the floor over the program's life, and vacancies fill anyway. Industry associations campaign to keep the payment capped below a living wage and oppose minimum-wage escalations that would reclaim the margin.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    powerful, biographical, arbitrage, national).

% Work hourly shifts in logistics, care, hospitality, and retail. The payment covers rent and groceries, so a paycheck tops up rather than sustains them, and successive contract renewals arrive with flatter wage offers. Refusing any particular job no longer means hunger, but leaving the sector means losing seniority, references, and local networks, and retraining paths sit far from regional labor markets. Collective wage action has thinned as the urgency behind it faded.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, trapped, regional).

% Fund the transfer through the general tax mix. Middle-income earners see the levy on their payslips; small businesses without low-wage payrolls carry proportionate shares. They receive the program's macroeconomic stability only indirectly while bearing its full fiscal cost, and they cannot decline the contribution short of emigration.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Caregivers of children and elders, students, people with disabilities, and others outside the wage market receive the same payment with no employment condition attached. For them the transfer is a direct income gain with no offsetting wage effect; household survey data attribute the largest measured wellbeing improvements to this group. Their livelihood depends on the program's continuance.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, non_labor_recipients, beneficiary,
    powerless, biographical, trapped, national).

% Built the wage gains that preceded the program and proposed pairing any floor with sectoral bargaining councils and wage-indexation rules. They were consulted late or not at all in pilot design, and their proposals for linking payment levels to negotiated wage floors did not enter the program architecture. Recruitment has weakened because the wage urgency the floor removed was their organizing hook.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Run incidence studies comparing pre- and post-program wage trajectories in saturated regions, model reservation-wage responses, and test whether announced wellbeing gains survive composition controls. Their findings feed both the defense and the critique of the arrangement, and they collect no rents from either outcome.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an unconditional subsistence floor decoupled from employment status: it stabilizes household consumption across job loss and hours volatility, eliminates benefit cliffs and means-testing bureaucracy, and sustains aggregate demand during labor-market slack.
% TRANSFER_FUNCTION: Moves taxed fiscal resources to every resident as unconditional cash. Within the labor market, a measurable share of that value subsequently moves from the public ledger into low-wage employers' margins through downward wage adjustment: the worker's grant partly converts into reduced payroll cost.
% ABSENT_VOICES: Organized labor — the seat that would insist the floor be tied to bargaining institutions and indexed to negotiated wages — sat largely outside pilot design and advocacy coalitions. Also absent: future cohorts who inherit the fiscal commitment, and job-guarantee proponents who would couple the floor to public employment creation rather than letting wage-setting absorb it.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, millions of households lose subsistence immediately; low-wage employers face sudden staffing collapse at prevailing wages and must raise wage offers steeply to refill shifts; consumption in recipient-heavy regions contracts; and the low-wage service economy built around the current wage bill reorganizes within months around higher labor costs.
% FOUNDING_PROBLEM: Material insecurity among people outside stable full-time employment, administered through stigmatizing means-tested programs whose benefit cliffs punished any attempt to work more.
% FOUNDING_PROBLEM_CORROBORATION: National statistical agencies' material-deprivation series and OECD at-risk-of-poverty data corroborate that the founding insecurity remains live. No source outside the benefiting parties attests that the present arrangement resolves the founding problem rather than managing it indefinitely; incidence researchers from outside the beneficiary set explicitly contest that it does.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.71: the incidence evidence from adjacent targeted-transfer regimes shows eligibility-linked cash reaching employers through wage adjustment, and the arrangement's mature form channels a majority of marginal transfer value into reduced payroll cost at the bottom of the wage distribution. Suppression is authored at 0.55 as a RAW STRUCTURAL PROPERTY — it is not scaled by power or scope; the engine owns all scaling. What the floor suppresses is not the formal right to refuse work (that survives by design) but the economic force of refusal: wage-pressure alternatives (collective bargaining escalations, statutory floor increases) lost their urgency and their organizing base, which is why suppression sits mid-range rather than high. Theater ratio is 0.31: real money reaches real households every cycle, so the core function is not performance, but a growing layer of empowerment-and-freedom marketing wraps the program while its wage-incidence effects accumulate underneath. Accessibility collapse is 0.45 — the alternative architectures (floor plus sectoral bargaining, living-wage-level statutory floors, job guarantees) remain partially constructible, which is characteristic of a hybrid arrangement rather than a totalizing one. Resistance is 0.50: union objections and fiscal-conservative repeal pressure meet entrenched employer defense of the arrangement, producing persistent but unresolved contest. The temporal series run on ONE shared grid (t=0..24 at 4-year steps) so every tracked metric is authored at every examined time point; trajectories are monotonically accumulating rather than cyclical — extraction ratchets upward as wage-setting equilibrates to the floor, with no oscillation phase to document, and the suppression_requirement series tracks the enforcement story specifically: light-touch administration early, hardening into anti-fraud machinery, mandatory enrollment infrastructure, and recurring legislative defense as the arrangement became load-bearing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the low_wage_employer seat, the arrangement is a subsidy it did not ask to be called one: applications arrive, wages settle where the market clears against a guaranteed floor, and every euro of the grant it captures looks like ordinary price adjustment — this seat experiences a coordination mechanism it benefits from without administering. From the low_wage_worker seat, the same payment rail reads as the thing that dissolved the wage urgency their grandparents' collective action won: subsistence without leverage, employment without progression. The non_labor_recipient seat experiences a third constraint entirely — a plain income gain with no labor-market offset at all. The taxpayer seat sees a fiscal transfer whose headline beneficiaries are not the advertised ones. The engine derives each seat's type from these structural positions; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. low_wage_employers are declared beneficiaries with arbitrage-grade exit (relocation, automation, restructuring pay mixes), placing them near the full-beneficiary end — effective extraction inverts toward subsidy for them, which is precisely the mechanism this reading asserts. low_wage_workers are declared victims, powerless, with trapped exit (sector-specific seniority, regional immobility, thin retraining paths), placing them near the full-target end; the floor removes the desperation that once powered their wage pressure, deepening rather than relieving their exposure. general_taxpayers are declared victims with constrained exit (taxation is not escapable short of emigration) at moderate power — high but not maximal targeting, since they receive macroeconomic stabilization back diffusely. non_labor_recipients are declared beneficiaries with trapped exit: trapping normally amplifies targeting, but for declared beneficiaries it pins them to a genuine subsidy, and the derivation reads the declaration first. labor_unions (excluded) and labor_economists (observer) carry no beneficiary/victim declarations and ride canonical fallbacks appropriate to their seats. ONE override is authored: the social_protection_authority is the sole institutional actor, declares neither beneficiary nor victim status, and would otherwise fall to a canonical fallback that misstates its position — it mildly profits from the arrangement through expanding administrative budget, staffing, and political credit for program stewardship, so d is overridden to 0.30 (mild-beneficiary side) rather than neutral. Suppression stays unscaled in all of this; only extractiveness is amplified by directionality and the national scope's verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — material insecurity outside stable employment — is still live (corroborated from outside the beneficiary set by deprivation statistics), so this is NOT a resolved-mandatrophy case: the arrangement has not outlived its mandate, it has ACCRETED a second function (carrying low-wage business models) onto a mandate it still partially serves. The tangled-rope classification is what prevents both mislabelings. Read as pure extraction, the analysis would erase the genuine floor function that measurably stabilizes non-labor households and consumption; read as pure coordination, it would launder the employer capture that the incidence record documents. Naming both sides — beneficiaries including low_wage_employers AND non_labor_recipients, victims including low_wage_workers AND general_taxpayers, enforcement requirement true — forces the hybrid verdict and keeps the capture component from hiding inside the poverty statistics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This file instantiates only the wage_subsidy_reading of kernel income_support_conditionality: which causal channel actually dominates the standing arrangement — wage-incidence capture (this reading), refusal-capacity expansion (freedom_floor_reading), or labor-force withdrawal (dependency_trap_reading)?',
    'Head-to-head comparison of wage-incidence estimates against reservation-wage and labor-supply evidence under universal coverage; the sibling stories carry their own epsilon and classification, and whichever mechanism the evidence favors governs which story describes the referent.',
    'If the capture channel dominates, this story''s epsilon and tangled-rope profile stand. If decommodification dominates instead, this reading''s constraint misdescribes the arrangement and the freedom-floor sibling supersedes it as the referent''s account; averaging across readings would corrupt all three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer-frame routing: one reading of a contested kernel; the disagreement lives between sibling files, not inside this one.').

omega_variable(
    capitalization_scale_threshold,
    'Is wage capitalization absent at pilot scale but dominant at universal scale — that is, does the employer-subsidy channel activate only once the floor covers an entire regional labor market?',
    'Staggered or regionally saturated rollouts with dose-response wage-trajectory analysis; compare wage drift in fully covered commuting zones against partially covered neighbors.',
    'If no threshold exists, this story''s extraction estimate is overstated and the capture mechanism is weaker than claimed. If the threshold is sharp, small-scale pilots systematically understate extraction and the historical measurement series understates the mature regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capitalization_scale_threshold, empirical, 'Scale-dependence of the wage-adjustment channel that grounds this reading''s epsilon.').

omega_variable(
    bargaining_complementarity_conditional,
    'Does the floor substitute for collective wage-setting where bargaining institutions are weak, but complement it where sectoral bargaining or wage councils are strong?',
    'Cross-national comparison of income-floor regimes nested inside strong bargaining systems versus floor-alone systems, controlling for coverage rates and union density.',
    'If complementarity holds, the extraction attributed to the arrangement is conditional on institutional context, pairing remedies become available, and the cost class of fixing drops; if substitution holds everywhere, the capture is intrinsic to the floor-under-market-wages configuration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bargaining_complementarity_conditional, empirical, 'Institutional-context contingency of the employer-capture mechanism.').

omega_variable(
    fiscal_incidence_regressivity,
    'Who actually bears the funding burden of the transfer — is the tax mix financing it progressive enough that general_taxpayers'' cost falls mainly on higher incomes, or does it regress onto the working poor?',
    'Distributional analysis of the financing mix (payroll levies, consumption taxes, income-tax shares) matched to household expenditure surveys.',
    'If financing regresses, the general_taxpayers seat overlaps the low_wage_workers seat and effective extraction concentrates on the same households twice; if progressive, the two victim seats remain structurally distinct and the extraction picture is as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_incidence_regressivity, empirical, 'Regressivity of the funding side determines whether the two payer seats are distinct populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__wage_subsidy_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(inco_tr_t4, observed).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(inco_tr_t8, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(inco_tr_t16, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(inco_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(inco_be_t4, observed).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(inco_be_t8, observed).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(inco_be_t16, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement_basis(inco_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t4, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(inco_su_t4, observed).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(inco_su_t8, observed).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement_basis(inco_su_t12, observed).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(inco_su_t16, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(inco_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the income_support_conditionality kernel into three sibling stories, one per reading: freedom_floor_reading (decommodification account), wage_subsidy_reading (this file; wage-incidence capture account), and dependency_trap_reading (labor-attachment erosion account). The colloquial label 'unconditional basic income' conflates three structurally distinct claims with different epsilon values, beneficiary/victim structures, and classifications; per the epsilon-invariance principle they are modeled as linked stories, not one story with a framing parameter. Upstream/downstream structure: freedom_floor_reading supplies the normative case cited in advocacy and pilot justification; wage_subsidy_reading assesses realized incidence and exerts downstream legitimacy pressure on the freedom-floor coalition's claims; dependency_trap_reading contests both on labor-attachment grounds. Each sibling file mirrors these links in its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__wage_subsidy_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
