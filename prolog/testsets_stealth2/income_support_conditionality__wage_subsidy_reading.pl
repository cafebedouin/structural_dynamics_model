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
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy (Wage-Subsidy Reading)
 *   domain: political economy/social policy/labor economics
 *
 * SUMMARY:
 *   An unconditional income transfer — paid to individuals regardless of work
 *   status — operates inside labor markets where employers set wages. This
 *   story instantiates ONE reading of the contested kernel
 *   income_support_conditionality: the wage-subsidy reading, which holds that
 *   because the transfer guarantees a subsistence top-up, employers can offer
 *   wages below what subsistence alone would force, and part of the public
 *   transfer is capitalized into lower wage offers. On this reading the
 *   arrangement coordinates genuine poverty relief (people outside the wage
 *   relation receive the grant at face value) while simultaneously routing
 *   part of labor's compensation to employers — a hybrid with identifiable
 *   coordinated parties and identifiable paying parties. The epsilon referent
 *   is the standing arrangement — the transfer as operated — assessed by this
 *   reading's own lights; the sibling readings (freedom_floor,
 *   dependency_trap) instantiate different constraints with different epsilon
 *   values and are linked, not averaged, here.
 *
 * KEY AGENTS:
 *   - low_wage_sector_employers: Primary beneficiary (organized/arbitrage) — collects suppressed wage bills funded by the public transfer; lobbies to preserve calibration
 *   - low_wage_workers: Primary target (powerless/trapped) — bears the wage adjustment; the transfer arrives pre-netted-out of compensation
 *   - general_taxpayers: Secondary target (moderate/constrained) — funds what wage bills would otherwise carry
 *   - transfer_administration_agency: Agenda setter (institutional/constrained) — administers the machinery and frames outcomes as poverty reduction
 *   - non_participating_recipients: Incidental beneficiary (powerless/trapped) — receives the grant at face value, no wage interaction
 *   - sectoral_bargaining_unions: Excluded voice (organized/constrained) — bargaining-power alternatives kept out of design venues
 *   - fiscal_policy_economists: Analytical observer — produces the incidence estimates every camp cites and none accepts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.7).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.62).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy (Wage-Subsidy Reading)").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political economy/social policy/labor economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'a815e841-887a-443c-afdf-62f7d36d9bd2').
narrative_ontology:cs_kernel_codification('a815e841-887a-443c-afdf-62f7d36d9bd2', formalized).
narrative_ontology:cs_authority_grounding('a815e841-887a-443c-afdf-62f7d36d9bd2', distributed).
narrative_ontology:cs_reading_relation('a815e841-887a-443c-afdf-62f7d36d9bd2', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a815e841-887a-443c-afdf-62f7d36d9bd2', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('a815e841-887a-443c-afdf-62f7d36d9bd2', foundational, transfer_incidence_captured_by_employers).
narrative_ontology:cs_axiom_status(transfer_incidence_captured_by_employers, holdable).
narrative_ontology:cs_axiom_grounding('a815e841-887a-443c-afdf-62f7d36d9bd2', transfer_incidence_captured_by_employers, empirically_contingent).
narrative_ontology:cs_axiom('a815e841-887a-443c-afdf-62f7d36d9bd2', secondary, public_transfer_externalizes_labor_reproduction_cost).
narrative_ontology:cs_axiom_status(public_transfer_externalizes_labor_reproduction_cost, holdable).
narrative_ontology:cs_axiom_grounding('a815e841-887a-443c-afdf-62f7d36d9bd2', public_transfer_externalizes_labor_reproduction_cost, instrumental).
narrative_ontology:cs_reference_frame('a815e841-887a-443c-afdf-62f7d36d9bd2', unconditional_transfer_low_wage_equilibrium).
narrative_ontology:cs_drift_state('a815e841-887a-443c-afdf-62f7d36d9bd2', contemporary_incidence_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a815e841-887a-443c-afdf-62f7d36d9bd2', '2026-06-12T09:00:00Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_sector_employers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, non_participating_recipients).
narrative_ontology:constraint_vindicates(income_support_conditionality__wage_subsidy_reading, market_wage_flexibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate businesses staffed heavily by low-wage labor: retail, food service, care work, logistics, warehousing. Set wage offers in labor markets where applicants hold a guaranteed transfer, pricing offers to what applicants will accept given that outside income. Fund trade associations that lobby for transfer generosity and against wage-floor increases. Can relocate facilities, automate roles, or restructure contracting if local labor costs rise.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_sector_employers, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, low_wage_sector_employers, agenda_setter).

% Work hourly jobs in retail, care, food service, and warehousing. Receive the unconditional transfer as part of household income; face wage offers that already price that transfer in. Rent and food costs exceed the transfer alone, so declining a wage offer means hardship, and moving regions or retraining carries costs they mostly cannot finance.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, trapped, national).

% Fund the transfer through income, payroll, and consumption taxes. Have no direct handle on how the money interacts with wage-setting; see the program line in budgets and the poverty statistics it moves. Can vote on governments that set transfer levels, but the chain between their vote and wage outcomes is long and opaque.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Runs enrollment, payment, income verification, and fraud controls for the transfer. Publishes take-up and poverty-impact figures. Its budget and headcount scale with the program; it defends program continuity in appropriations cycles and presents outcomes in terms of poverty reduction.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, transfer_administration_agency, agenda_setter,
    institutional, generational, constrained, national).

% People outside the wage relation — unpaid caregivers, people with disabilities, those between jobs — receive the transfer as direct income with no employer on the other side of the transaction to adjust prices against it. For them the grant arrives roughly at face value.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, non_participating_recipients, beneficiary,
    powerless, biographical, trapped, national).

% Organize for industry-wide wage agreements and wage-floor increases. Were not seated in the fiscal-policy processes where transfer design was settled; their proposals arrive as amendments after parameters are fixed. Represent a shrinking share of the private-sector workforce.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, sectoral_bargaining_unions, excluded,
    organized, generational, constrained, national).

% Estimate how transfers interact with wage-setting: incidence studies, labor-supply elasticities, monopsony pass-through models. Publish in journals and advise ministries; their estimates are cited by every camp in the policy dispute and settle nothing by themselves.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, fiscal_policy_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_sector_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a subsistence income floor reaching people regardless of employment status, replaces a patchwork of means-tested programs with a single payment, smooths household consumption across job spells, and stabilizes aggregate demand at the bottom of the income distribution.
% TRANSFER_FUNCTION: Moves purchasing power from the general tax base to low-income households. Inside wage-setting labor markets, part of that purchasing power is then re-priced: employers can offer lower wages because applicants hold the transfer, so a share of the public money finances employers' reduced wage bills rather than raising workers' total compensation.
% ABSENT_VOICES: Sectoral bargaining unions and wage-floor advocates were not seated where transfer parameters were set — fiscal ministries negotiated with employer associations and anti-poverty charities, with bargaining-power alternatives treated as out of scope. Future taxpayers who will carry the fiscal cost had no seat. Both would object that the arrangement's calibration embeds employer wage interests.
% DISAPPEARANCE_RATIONALE: If the transfer vanished overnight, wage offers in low-wage sectors would have to rise toward subsistence or labor supply in those sectors would contract sharply; millions of household budgets would break immediately; employers would reprice or shrink; the fiscal saving would be contested across rival uses. The low-wage labor market as currently priced depends on the transfer's existence.
% FOUNDING_PROBLEM: Guarantee subsistence and reduce extreme poverty through a simple, universal payment — replacing fragmented, stigmatizing, means-tested relief that missed people between programs.
% FOUNDING_PROBLEM_CORROBORATION: National statistical agencies' poverty and material-deprivation series and charitable-sector demand data (food banks, housing services) attest from outside the beneficiary set that subsistence insecurity persists. Employer associations notably do not attest the founding problem — their testimony concerns labor-cost flexibility, not poverty.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.70 because the reading's core mechanism — transfer dollars capitalized into lower wage offers — routes a large share of the public money to employers' margins while the official object of the spending is worker income. Suppression (0.62) is a raw structural property, unscaled by power or scope: the transfer is calibrated below subsistence, so declining a wage offer remains materially unavailable, and the arrangement defuses pressure for employer-funded subsistence. Theater (0.40): the anti-poverty delivery is real — non-participants receive the grant at face value — but a growing share of the program's public justification performs redistribution that the wage-incidence channel quietly reroutes. Accessibility_collapse (0.35) is low because alternatives stay visible and partly live: statutory wage floors, sectoral bargaining, and conditional programs all remain on the table even once the incidence dynamic is understood. Resistance (0.50) reflects sustained employer-lobby opposition to wage floors and union campaigns against the subsidy framing, with some jurisdictions raising minimum wages through the interval. The three metric series share one seven-point grid; trajectories are monotone (no oscillation, so no intermittent-reinforcement mechanism is claimed), and the endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the employer seat the transfer is a stable input that lowers the wage needed to staff a shift — coordination it did not build but readily prices against. From the worker seat the same payment arrives pre-hypothecated: the wage offer already nets it out, so the check feels like poverty relief while the compensation package stays flat. The administration seat experiences the arrangement as delivery logistics and poverty statistics. Non-participating recipients experience the promise kept at face value. The engine computes these divergent per-seat classifications from power, exit, and role data; the divergence between the employer's and the worker's computed types is the perspectival fact this story exists to register. Coalition note: individually powerless workers sit adjacent to organized unions currently excluded from design venues; a successful organizing wave would convert trapped exit into constrained-or-mobile exit and bend the extractiveness series downward — the measurement record would show it before any reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   Employers are declared beneficiaries with arbitrage-grade exit: the derivation places them near the beneficiary pole (low d), so effective extraction inverts toward subsidy for them. Workers are declared victims with trapped exit: near the full-target pole (high d), amplified. Taxpayers are victims with constrained exit: high d, moderately amplified, scaled by national scope. The administration agency sets and runs the arrangement — its seat reads as the coordination operator. Non-participating recipients are beneficiaries with trapped exit: subsidized, near-zero d. Excluded unions and analytical economists contribute no directional pull; the unions matter as the coalition that could restructure everyone else's exit options. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring this as a hybrid prevents two mislabels. Reading the arrangement as pure coordination ignores the wage-incidence channel that routes part of the transfer to employers; reading it as pure extraction ignores the real floor delivered to non-participants and the genuine consumption-smoothing function. The founding problem — subsistence insecurity — is still live, so no resolved-mandatrophy declaration is made; what has accreted is a second function (externalizing labor-reproduction costs onto the tax base), and that accretion is tracked by the rising base_extractiveness series rather than by a mandate-death claim. The theater series rises in parallel: as the second function grows, more of the program's public presentation defends the first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which structural characterization of the unconditional-transfer arrangement is correct: employer-subsidy capture (this reading), decommodification (freedom_floor_reading), or incentive erosion (dependency_trap_reading)?',
    'Comparative incidence and labor-supply evidence across transfer reforms; the readings predict different signs and magnitudes on wage adjustment and refusal behavior, so converging estimates select among them.',
    'Resolution toward freedom_floor removes employers from the beneficiary set and drops epsilon sharply; resolution toward dependency_trap adds skill-atrophy dynamics and shifts the victim set toward long-term recipients; confirmation of this reading fixes employers as beneficiaries and workers as payers with the current type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of the income_support_conditionality kernel; sibling readings would restructure the beneficiary/victim sets and the type.').

omega_variable(
    wage_incidence_share,
    'What fraction of the marginal transfer dollar is captured by employers through downward wage adjustment rather than retained by workers?',
    'Natural experiments — regional or temporal transfer expansions matched to wage-panel data; monopsony-model estimates of pass-through from outside options to offered wages.',
    'Capture below roughly 0.2 collapses this reading toward the freedom_floor constraint; capture above roughly 0.5 pushes the arrangement toward pure extraction with employers as the sole receipt seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_incidence_share, empirical, 'Magnitude of the employer-capture channel that defines this reading.').

omega_variable(
    subsistence_calibration_threshold,
    'Is the transfer calibrated above or below the subsistence threshold at which refusing a wage offer becomes materially possible?',
    'Compare transfer levels against local subsistence baskets and housing costs; observe refusal and search-duration behavior among recipients at different transfer levels.',
    'Below the threshold, labor-force attachment stays mandatory and the capture regime holds; above it, the arrangement migrates toward the decommodification structure the freedom_floor reading describes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsistence_calibration_threshold, empirical, 'Whether the transfer''s level leaves refusal power real or nominal.').

omega_variable(
    capture_vs_incidence_framing,
    'Is ''employer capture'' the correct frame, or is the wage adjustment ordinary tax incidence — statutory recipient differs from economic bearer, with no agent capturing anything?',
    'Conceptual analysis against a specified counterfactual: if the counterfactual is ''workers would otherwise command the transfer as wage premium,'' capture language is apt; if the counterfactual is ''no transfer at all,'' the adjustment is standard equilibrium incidence.',
    'Under the incidence framing, epsilon drops toward coordination-cost levels and the constraint reads as rope-like; under the capture framing, the authored epsilon stands. The framing choice changes classification without changing any measured quantity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_incidence_framing, conceptual, 'Framing under-determination: capture versus neutral incidence accounts of the same wage adjustment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__wage_subsidy_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(inco_tr_t25, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__wage_subsidy_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(inco_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(inco_be_t25, observed).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(inco_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(inco_su_t25, observed).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(inco_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, minimum_wage_legislation).

% DUAL FORMULATION NOTE:
% The colloquial label 'unconditional income support' covers structurally distinct claims yielding different epsilon values: whether the transfer decommodifies labor (freedom_floor_reading), erodes work incentives (dependency_trap_reading), or is capitalized into suppressed wages (this file). Per the epsilon-invariance principle these are separate constraints — separate epsilon, beneficiary/victim structure, and type — linked as one kernel family. This reading sits alongside its siblings citing the same empirical record; the wage-incidence channel is the element on which the family members diverge, and minimum_wage_legislation is coupled because wage floors and transfer calibration jointly determine the capture margin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
