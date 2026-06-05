% ============================================================================
% CONSTRAINT STORY: venture_capital_maturity_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venture_capital_maturity_mismatch, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: venture_capital_maturity_mismatch
 *   human_readable: Venture Capital Maturity Mismatch: Growth-Stage Extraction Under Early-Stage Structures
 *   domain: finance/venture_capital/corporate_governance
 *
 * SUMMARY:
 *   Venture capital maturity mismatch describes a structural constraint where
 *   early-stage governance mechanisms (founder control, investor
 *   anti-dilution, liquidation preferences) persist unchanged as companies
 *   scale into growth and late stages, creating systematic extraction from
 *   growth-stage employees and minority shareholders. The constraint operates
 *   through multiple interlocking mechanisms: (1) vesting schedules that trap
 *   employees in illiquid positions, (2) founder voting blocs that prevent
 *   governance evolution despite company maturity, (3) anti-dilution
 *   protections that shift down-round costs to employees and minority
 *   holders, (4) preferential liquidation cascades that treat early investors
 *   as creditors while treating employees as residual claimants. The
 *   constraint exhibits different classification types from different
 *   observer positions: pure extraction (Snare) for trapped employees, mixed
 *   coordination-extraction (Tangled Rope) for growth-stage founders, pure
 *   coordination (Rope) for early investors, institutional theater (Piton)
 *   for standardized Series A documents, and apparent natural law (Mountain)
 *   when viewed from a universal perspective that mistakenly treats venture
 *   governance as immutable. The theater ratio has increased over the
 *   measurement interval (0.35 to 0.65) reflecting growing performance of
 *   governance legitimacy despite degraded actual function—Series A standard
 *   documents are increasingly presented as 'investor protections' despite
 *   primarily protecting founders.
 *
 * KEY AGENTS:
 *   - Early-Stage Investors (Seed/Series A/B): Primary beneficiary (institutional/arbitrage) — capture anti-dilution protections, board control, and exit liquidity while founders remain locked in and employees remain trapped
 *   - Founders with Near-Term Exit Options: Secondary beneficiary (institutional/arbitrage) — maintain operational control, capture founder preference shares, can exit via acquisition or secondary sales while employees cannot
 *   - Growth-Stage Employees: Primary victim (powerless/trapped) — vest into illiquid equity, cannot exit without forfeiting unvested grants, have no governance voice or board representation, bear dilution costs of follow-on rounds
 *   - Late-Stage Minority Shareholders: Primary victim (powerless/trapped) — acquired via later-stage funding rounds, systematically subordinated to founder voting and early-investor preferences, face illiquidity and adverse selection in secondary markets
 *   - Mid-Stage Founders Without Near-Term Exits: Constrained actor (moderate/constrained) — benefit from capital but increasingly constrained by governance lock-in, can neither change control structure nor exit cleanly
 *   - Late-Stage Growth Investors (Series C/D/E): Constrained institutional actor (organized/constrained) — have board seats but limited voting power; see governance failures but cannot force change without triggering founder departure
 *   - Series A Document Templates (YC/SAFE): Institutional theater (institutional/arbitrage) — standardized legal documents that codify founder control while appearing neutral, persist through ecosystem inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venture_capital_maturity_mismatch, 0.58).
domain_priors:suppression_score(venture_capital_maturity_mismatch, 0.62).
domain_priors:theater_ratio(venture_capital_maturity_mismatch, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venture_capital_maturity_mismatch, extractiveness, 0.58).
narrative_ontology:constraint_metric(venture_capital_maturity_mismatch, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(venture_capital_maturity_mismatch, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venture_capital_maturity_mismatch, tangled_rope).
narrative_ontology:human_readable(venture_capital_maturity_mismatch, "Venture Capital Maturity Mismatch: Growth-Stage Extraction Under Early-Stage Structures").
narrative_ontology:topic_domain(venture_capital_maturity_mismatch, "finance/venture_capital/corporate_governance").

domain_priors:requires_active_enforcement(venture_capital_maturity_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venture_capital_maturity_mismatch, early_stage_investors).
narrative_ontology:constraint_beneficiary(venture_capital_maturity_mismatch, founders_with_exit_options).
narrative_ontology:constraint_victim(venture_capital_maturity_mismatch, growth_stage_employees).
narrative_ontology:constraint_victim(venture_capital_maturity_mismatch, late_stage_minority_shareholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GROWTH-STAGE EMPLOYEES (SNARE) — Trapped by vesting schedules, illiquidity, and the collective action problem of organizing against founders. Cannot exit without losing equity. Founder-controlled governance means employees have no mechanism to challenge extraction or demand board representation. Maximum suppression: founders control information flow, compensation decisions, and exit timing. Zero degrees of freedom.
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATE-STAGE MINORITY SHAREHOLDERS (SNARE) — Trapped by illiquidity, asymmetric information, and voting structures that preserve founder control across capital raises. Cannot exit without accepting pennies-on-the-dollar secondary sales. Dragged into increasingly extractive structures (preferred stock features, anti-dilution protections) that protect early investors at their expense. No exit mechanism.
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-STAGE FOUNDERS WITHOUT NEAR-TERM EXITS (TANGLED ROPE) — Experience the constraint as genuine coordination: capital infusion enables team expansion, product development, and market penetration. But increasingly constrained by founder-friendly but growth-hostile governance (founder voting blocs, liquidation preferences, anti-dilution protections) that make future raises harder and suppress external accountability. Mixed experience: genuine benefit from capital + growing extraction from governance lock-in. High cost to exit (must abandon company).
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EARLY-STAGE INVESTORS (ROPE) — Pure beneficiary. Experience the constraint as optimal coordination: founder control preserves alignment during execution phase, multi-stage capital raises maintain ongoing incentives, and anti-dilution protections guarantee their position. Can exit via secondary sales, recapitalizations, or M&A. Extraction flows toward this group; they see the governance structure as coordination.
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LATE-STAGE GROWTH INVESTORS (TANGLED ROPE) — Benefit from access to high-growth companies but increasingly constrained by founder control structures inherited from early stages. Receive privileged information (board seats) that creates coordination function (strategic input), but extraction mechanisms (founder voting, anti-dilution clauses) limit their ability to shape outcomes or exit cleanly. Trapped between needing founder execution and needing governance change. Moderate extraction but real coordination benefit.
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SERIES A GOVERNANCE TEMPLATES (PITON) — The standardized documents (YC documents, SAFE, Series A preferred stock terms) are largely performative: they codify founder control while appearing to create investor protections. Theater ratio high because the documents' stated purpose (investor protection, governance clarity) is subordinate to their actual function (founder control preservation). These templates persist through institutional inertia despite their obsolescence for growth-stage companies. Viewed from within the startup ecosystem, the documents are degraded — everyone knows they don't protect growth-stage investors or employees, but alternatives haven't fully displaced them.
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some degree of founder control and early-investor protection is inherent to venture capital: founders must have sufficient autonomy to execute, and early investors must have sufficient security to commit capital at high risk. This perspective sees the constraint as an immutable property of venture finance. However, the structural data contradicts the mountain classification — the engine identifies this as a false summit: the maturity mismatch is not inherent to venture capital but contingent on institutional failure to upgrade governance structures as companies scale.
constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venture_capital_maturity_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venture_capital_maturity_mismatch, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venture_capital_maturity_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venture_capital_maturity_mismatch, TR),
    TR >= 0.70.

:- end_tests(venture_capital_maturity_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Begins at 0.22 during pure seed/Series A phase when founder control genuinely enables execution with minimal extraction. Rises to 0.38 by Series C (growth stage) as founders maintain operational control despite company maturity requiring distributed decision-making. Reaches 0.58 by series E/F (late growth) as anti-dilution clauses, founder voting blocs, and liquidation preferences systematically subordinate growth-stage stakeholders. The rising trajectory reflects accumulation of extraction mechanisms rather than change in founder intent—governance structures designed for $2M seed rounds remain operative at $500M valuations. Suppression (0.62): High. Multiple suppression mechanisms: (1) vesting schedules create information asymmetry and exit barriers, (2) founder control of board prevents employee voice, (3) lack of mandatory employee board representation, (4) illiquidity prevents secondary market exit, (5) anti-dilution terms suppress minority shareholder exit options, (6) collective action problem prevents employee organizing. Theater ratio (0.65): Moderate-high and rising. Series A documents present themselves as 'investor protections' and 'governance clarity' despite their primary function being founder control preservation. Board meetings perform legitimacy (quarterly updates, strategic input) despite limited actual governance change when founders hold voting control. Anti-dilution clauses are presented as 'investor risk management' despite primarily enabling founder extraction in down rounds.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap reveals how institutional vocabulary ('investor protections,' 'founder alignment') masks extraction mechanisms. The early investor's Rope classification (genuine coordination) is structurally correct—they benefit from founder autonomy and can exit. The employee's Snare classification (pure extraction) is structurally correct—they are trapped and have no exit. The gap is not analytical error but genuine difference in how the constraint operates: the same governance structure (founder voting, anti-dilution) is a coordination mechanism from the beneficiary's perspective and an extraction mechanism from the victim's perspective. The constraint itself forces these perspectives to diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: Early investors (beneficiary + arbitrage exit) derive d ≈ 0.05, producing negative f(d) ≈ -0.12—extraction flows toward them. Growth-stage employees (victim + trapped exit) derive d ≈ 0.95, producing high f(d) ≈ 1.42—maximum experienced extraction. Mid-stage founders without exits (mixed + constrained) derive d ≈ 0.50, producing f(d) ≈ 0.65—moderate experienced extraction. Late-stage growth investors (organized + constrained exit) derive d ≈ 0.55, producing f(d) ≈ 0.75—above-average experienced extraction. The pipeline correctly identifies early investors as net beneficiaries despite explicit anti-dilution 'protections', and correctly identifies employees as trapped victims despite explicit equity grants. The directionality overrides are not needed—structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: The constraint resolves the mandatrophy by demonstrating that Tangled Rope (not pure Rope or pure Snare) is the correct classification at the structural level because the constraint genuinely coordinates capital deployment AND systematically extracts from growth-stage stakeholders. The classification prevents mislabeling: (1) Pure Rope would ignore the suppression and extraction mechanisms targeting employees and minority shareholders. (2) Pure Snare would overstate founder intent—early-stage founder control genuinely enables execution and is not parasitic. (3) Tangled Rope correctly captures that the constraint serves both a coordination function (getting capital to founders) and an extraction function (keeping early investors and founders rich at growth-stage stakeholders' expense). The false summit (Mountain) occurs when observers naturalize the governance structure as 'how venture capital works' rather than seeing it as contingent institutional choice. Mandate resolution: the constraint is not inherent to venture capital—alternative structures (mandatory governance evolution at Series C, employee board representation, pro-rata anti-dilution) exist and are used in other jurisdictions. The persistence of current structures reflects beneficiary power and institutional path dependence, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_control_necessity,
    'Is founder control structurally necessary for execution in early-stage ventures, or has it become a rent-extraction mechanism maintained by institutional inertia?',
    'Comparative analysis: outcomes in jurisdictions with mandatory governance upgrades at Series C+ vs US standard practices; founder retention and startup success rates under shared governance models',
    'If necessary: constraint reclassifies as Rope (coordination benefit justifies restrictions). If contingent: remains Snare/Tangled Rope — extraction mechanism masquerading as alignment incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_control_necessity, empirical, 'Whether founder control is structurally necessary or institutionally contingent').

omega_variable(
    growth_stage_maturity_threshold,
    'At what company maturity threshold should governance structures mandatory transition from founder-controlled to distributed (board majority, employee representation, minority shareholder rights)?',
    'Time-series analysis of company failures, acquisitions, and public exits; correlation between governance structure and growth trajectory; stakeholder exit satisfaction surveys',
    'If threshold < $50M ARR: many high-growth companies face premature governance conflict. If threshold > $100M ARR: extraction mechanisms persist through growth phase unchecked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_stage_maturity_threshold, empirical, 'Optimal governance transition maturity threshold').

omega_variable(
    anti_dilution_moral_hazard,
    'Do anti-dilution clauses (protecting early investors from down rounds) reduce or amplify founder rent-extraction in down-round scenarios?',
    'Case study analysis: down-round negotiations with/without anti-dilution; founder behavior and employee outcome comparison; secondary market pricing of anti-dilution-protected vs unprotected positions',
    'If anti-dilution amplifies extraction: reclassify anti-dilution structure as Snare component. If protective: reclassifies as legitimate governance safeguard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_dilution_moral_hazard, empirical, 'Whether anti-dilution clauses increase or decrease founder extraction incentives').

omega_variable(
    employee_coordination_alternative,
    'Could employee ownership stakes (equity grants with governance rights) substitute for founder voting blocs while maintaining execution coordination?',
    'Pilot programs in growth-stage startups with employee board representation; governance efficiency and execution speed comparison; employee exit satisfaction surveys',
    'If substitution works: constraint reclassifies as Scaffold with sunset (current founder control gives way to distributed governance). If substitution fails: founders have legitimate need for enhanced control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employee_coordination_alternative, empirical, 'Whether employee ownership can replace founder voting concentrations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venture_capital_maturity_mismatch, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcmm_tr_t0, venture_capital_maturity_mismatch, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vcmm_tr_t3, venture_capital_maturity_mismatch, theater_ratio, 3, 0.48).
narrative_ontology:measurement(vcmm_tr_t6, venture_capital_maturity_mismatch, theater_ratio, 6, 0.62).
narrative_ontology:measurement(vcmm_tr_t10, venture_capital_maturity_mismatch, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(vcmm_be_t0, venture_capital_maturity_mismatch, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vcmm_be_t3, venture_capital_maturity_mismatch, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(vcmm_be_t6, venture_capital_maturity_mismatch, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(vcmm_be_t10, venture_capital_maturity_mismatch, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venture_capital_maturity_mismatch, resource_allocation).
narrative_ontology:affects_constraint(venture_capital_maturity_mismatch, founder_vesting_lock_in).
narrative_ontology:affects_constraint(venture_capital_maturity_mismatch, employee_liquidity_event_dependency).
narrative_ontology:affects_constraint(venture_capital_maturity_mismatch, anti_dilution_down_round_extraction).

% DUAL FORMULATION NOTE:
% The venture capital maturity mismatch is a parent constraint affecting three domain-specific manifestations: (1) founder vesting lock-in (affects founder behavior in down rounds and exit decisions), (2) employee liquidity event dependency (affects employee retention and negotiation power), (3) anti-dilution down-round extraction (affects minority shareholder outcomes during funding crises). Each downstream constraint has different ε values reflecting the specific mechanisms, but all inherit the maturity mismatch as structural precondition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
