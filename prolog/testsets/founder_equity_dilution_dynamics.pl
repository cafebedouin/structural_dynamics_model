% ============================================================================
% CONSTRAINT STORY: founder_equity_dilution_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_founder_equity_dilution_dynamics, []).

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
 *   constraint_id: founder_equity_dilution_dynamics
 *   human_readable: Founder Equity Dilution Dynamics in Venture-Backed Startups
 *   domain: venture_capital/corporate_governance
 *
 * SUMMARY:
 *   Founder equity dilution in venture-backed startups creates a structural
 *   tension between the coordination benefit of capital (acceleration,
 *   validation, hiring) and the extraction dynamic embedded in venture's
 *   ownership models. This constraint exhibits a perspectival spectrum
 *   spanning pure coordination (VC beneficiary view), mixed
 *   coordination-extraction (founder perspective), pure extraction (trapped
 *   founder view), degraded institutional ritual (term sheet theater), and an
 *   aspirational exit pathway (alternative capital structures). The dilution
 *   dynamics have intensified over the interval (2020-2026) as competition
 *   for market share has increased capital pressure and as venture firms have
 *   consolidated power through standard terms, increasing theater ratio from
 *   founder-negotiated variation toward mechanical standardization. Base
 *   extractiveness has grown from 0.35 (when founders had more leverage) to
 *   0.58 (current) as information asymmetry about alternatives has increased
 *   and founder bargaining power has decreased relative to capital
 *   availability pressure.
 *
 * KEY AGENTS:
 *   - Bootstrapped Founder: Primary victim (powerless/trapped) — dependent on VC capital with no alternative, faces dilution as coercive necessity
 *   - Optioned Founder: Secondary victim (moderate/constrained) — has some alternatives but faces realistic exit costs; bears dilution burden alongside coordination benefits
 *   - Serial Founder: Organized victim with agency (powerful/mobile) — can negotiate better terms but still subject to structural asymmetries and sunk dilution costs
 *   - Venture Capital Firm: Primary beneficiary (institutional/arbitrage) — controls capital flow and uses dilution as governance and alignment mechanism; experiences constraint as coordination
 *   - Later-Stage Investors: Secondary beneficiary (institutional/arbitrage) — benefit from earlier founder dilution which enables their entry and governance participation
 *   - Regulatory Reform Coalition: Organized agents (organized/mobile) — building alternative capital structures (SAFE, revenue-share, secondary markets) to bypass traditional dilution
 *   - Standard Term Sheet: Institutional actor (institutional/arbitrage) — maintains mechanical dilution formulas through standardization theater and path dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(founder_equity_dilution_dynamics, 0.58).
domain_priors:suppression_score(founder_equity_dilution_dynamics, 0.65).
domain_priors:theater_ratio(founder_equity_dilution_dynamics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(founder_equity_dilution_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(founder_equity_dilution_dynamics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(founder_equity_dilution_dynamics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(founder_equity_dilution_dynamics, tangled_rope).
narrative_ontology:human_readable(founder_equity_dilution_dynamics, "Founder Equity Dilution Dynamics in Venture-Backed Startups").
narrative_ontology:topic_domain(founder_equity_dilution_dynamics, "venture_capital/corporate_governance").

domain_priors:requires_active_enforcement(founder_equity_dilution_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(founder_equity_dilution_dynamics, venture_capital_firms).
narrative_ontology:constraint_beneficiary(founder_equity_dilution_dynamics, later_stage_investors).
narrative_ontology:constraint_victim(founder_equity_dilution_dynamics, founder_wealth_claims).
narrative_ontology:constraint_victim(founder_equity_dilution_dynamics, founder_decision_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOOTSTRAPPED FOUNDER (SNARE) — Founder who has no alternative capital sources faces dilution as coercive extraction with no meaningful exit. Each funding round is presented as necessary for survival, suppressing perceived alternatives. Trapped both by capital dependence and by the temporal pressure of competitive markets. No arbitrage available — refusal of dilution results in failure.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPTIONED FOUNDER (TANGLED ROPE) — Founder with options to dilute (strategic partnership, alternative investors, slower growth) experiences the dilution constraint as mixed. Coordination function exists: venture funding genuinely accelerates reach, hiring, and market validation. But asymmetric extraction remains: founder's percentage ownership decreases while investor leverage increases. Constrained by realistic exit costs (switching investors carries risk, slower growth may fail to achieve market opportunity), not trapped.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VENTURE CAPITAL FIRM (ROPE) — For institutional investors with arbitrage (deploy capital to multiple startups, exit via IPO/acquisition, govern through board seats), the dilution constraint is pure coordination. Founder dilution aligns incentives across capital rounds, ensures follow-on investment commitment, and triggers replacement of underperforming founders. The VC experiences this as governance coordination, not extraction.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SERIAL FOUNDER (TANGLED ROPE) — Founder with track record and network can negotiate better terms, bring co-investors to reduce dilution per round, and switch between projects. More mobile than bootstrapped founder, but still subject to asymmetric information (VC knows the dilution playbook better), asymmetric bargaining power (VC controls capital flow in their geographic market), and structural lock-in (dilution has already happened in previous rounds, sunk cost). Some agency, significant extraction.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized founders (founder associations, pro-equity reform advocates) see the dilution constraint as a temporary institutional design problem with a sunset clause. SAFE notes, equity-lite funding models, revenue-share agreements, and secondary markets (allowing early-stage liquidity without dilution) represent exit pathways from the VC dilution paradigm. These mechanisms are building alternatives to traditional Series A dilution. The coalition has agency and perceives a maturation timeline (10-15 years) over which the constraint's function becomes optional.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: STANDARD VENTURE TERM SHEET (PITON) — The mechanical dilution formula in venture (1x liquidation preference, cumulative dividends, participating preferred, anti-dilution clauses) persists largely through institutional habit despite continuous critique. Fund managers know the terms are suboptimal for founder incentives but use them because (a) they protect VC downside, (b) competitors use them, (c) departing from norms creates due-diligence friction. The constraint is maintained by theatrical standardization: 'this is market standard' becomes a self-fulfilling prophecy. Theater ratio reflects that much of the dilution justification is post-hoc rationalization rather than optimal governance.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAPITAL CONSTRAINT VIEW (MOUNTAIN) — From a universal/civilizational perspective, some dilution is inherent to capital formation: new money must claim some claim on future value, and founder ownership percentages necessarily decrease when capital is raised. This perspective sees the dilution constraint as immutable law of capital markets. However, the structural data reveals this as a false summit: the dilution magnitude, structure (participating preferred vs non-), and use of anti-dilution clauses are all contingent institutional choices, not inevitable consequences of capital formation.
constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(founder_equity_dilution_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(founder_equity_dilution_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(founder_equity_dilution_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(founder_equity_dilution_dynamics, TR),
    TR >= 0.70.

:- end_tests(founder_equity_dilution_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Founder dilution captures a real economic value transfer (founder ownership percentage decreases while investor control increases) but the magnitude is contested. The 0.58 value reflects that the transfer is substantial but not total — founders retain negotiation options (terms vary by founder quality and market conditions), and dilution is accompanied by genuine capital benefits (growth capital, validation, hiring). The growth trajectory (0.35 → 0.58 over 6 years) reflects increased standardization: earlier periods had more variation in founder-VC bargaining power; current period sees more mechanical application of standard terms. Suppression (0.65): High. Founders face significant barriers to exit: capital is necessary for growth, alternative sources are not equally visible or accessible (information asymmetry), refusing dilution often means slower growth that still results in eventual acquisition at lower valuation (sunk cost suppression), and competitive pressure creates temporal urgency that overrides careful negotiation. Suppression is not absolute — some founders do negotiate better terms, some do bootstrap successfully, some do use alternative capital — but the default path is heavily suppressed toward accepting standard dilution. Theater ratio (0.48): Moderate. The justification for dilution focuses on alignment narrative ('we both win if we grow fast'), governance narrative ('I need board seats to protect my capital'), and market standard narrative ('this is what everyone does'). These narratives contain truth but also obscure the extraction: the founder's ownership percentage goes down regardless of company performance, the VC's control increases regardless of contribution, and term sheets show high variation by founder quality suggesting that 'market standard' is partly post-hoc justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates full perspectival spread across six classification types. The bootstrapped founder sees trapped extraction (Snare, chi ≈ 0.92). The optioned founder sees mixed coordination-extraction (Tangled Rope, chi ≈ 0.60). The serial founder sees constrained extraction with agency (Tangled Rope, chi ≈ 0.45). The VC sees pure coordination (Rope, chi ≈ -0.05). The term sheet sees degraded ritual (Piton, theater = 0.48). The reform coalition sees temporary problem with sunset (Scaffold, chi ≈ 0.25). The analytical observer sees an immutable law (Mountain) but this is a false summit — the observed dilution is institutional choice, not capital physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position relative to the dilution flow. Beneficiaries (VC firms) with arbitrage exit options have low d (0.05-0.20) — the constraint is a tool in their favor, extraction flows toward them. Victims (founders) with trapped exit options have high d (0.85-0.95) — the constraint extracts from them with minimal options. Optioned founders with constrained exit have moderate d (0.55-0.70) — some extraction but also some alternatives. Serial founders with mobile exit options have lower d (0.40-0.55) — more bargaining power reduces experienced extraction. The sigmoid f(d) maps these d values to experienced extractiveness chi, which varies by founder position. The piton perspective uses institutional d (0.40) because the term sheet itself is not an extracting agent but a degraded ritual maintained by institutional inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint is genuinely Tangled Rope at the analytical level: (1) coordination function is real — capital enables growth, hiring, market validation that founders cannot achieve bootstrapped; (2) asymmetric extraction is real — founder ownership percentage decreases, VC control increases, and the allocation of these benefits is not proportional to contribution; (3) active enforcement is real — term sheets are negotiated and updated; (4) the beneficiaries (VCs) and victims (founders) are clearly distinct groups. This is NOT a mislabeled Rope (if it were pure coordination, founders would not experience extraction suppression). This is NOT a mislabeled Snare (if it were pure extraction, the capital benefits would not be real and material). The constraint resolves mandatrophy by confirming the mixed nature: genuine coordination + asymmetric extraction = Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_alternative_capital_availability,
    'How much of founder suppression derives from genuine unavailability of alternative capital sources versus institutional habit/information asymmetry about alternatives?',
    'Comparison of founder outcomes across regulatory regimes with different restrictions on VC dilution structures; analysis of founder choice distribution when alternative capital sources (angel networks, corporate VC, revenue-based financing) are equally visible and accessible',
    'If primarily genuine scarcity: suppression value (0.65) is structurally accurate. If primarily information asymmetry: true suppression is lower and the constraint is more purely extractive (snare) than mixed (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_alternative_capital_availability, empirical, 'Whether founder suppression is from actual capital scarcity or information asymmetry').

omega_variable(
    dilution_alignment_vs_extraction,
    'Do ownership dilution mechanisms in venture actually align founder incentives with investor returns, or do they primarily enable investor leverage and founder replacement?',
    'Longitudinal analysis of founder retention rates, company performance, and investor returns by dilution structure (founder-friendly vs investor-friendly terms); isolation of incentive alignment effect from selection effect (better founders raise better terms)',
    'If alignment dominant: dilution is coordination mechanism (Rope from VC perspective, Tangled Rope from founder perspective). If leverage/replacement dominant: dilution is extraction mechanism (Snare from founder perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dilution_alignment_vs_extraction, empirical, 'Whether dilution aligns incentives or enables investor leverage').

omega_variable(
    secondary_market_viability_timeline,
    'How quickly will secondary markets and equity-lite funding models (SAFE, revenue-share, employee liquidity platforms) eliminate the necessity of massive single-round dilution?',
    'Tracking adoption rates of alternative funding structures; measurement of founder outcomes on SAFE-only vs Series A paths; analysis of secondary market liquidity development for early-stage founders',
    'If viable sunset by 2030: scaffold perspective is accurate and the constraint has a real exit path. If secondary markets remain marginal: scaffold is aspirational and the constraint persists as tangled rope / snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_viability_timeline, empirical, 'Timeline for alternative capital structures to replace Series A dilution').

omega_variable(
    anti_dilution_clause_necessity,
    'Are anti-dilution clauses genuinely necessary to protect investor downside, or do they primarily amplify founder dilution and create down-round extraction spirals?',
    'Comparative analysis of down-round outcomes and founder ownership trajectories in funds that use vs restrict anti-dilution provisions; measurement of company performance correlation with anti-dilution clause presence',
    'If necessary: clause structure is reasonable governance. If primarily amplifying: anti-dilution is a secondary extraction mechanism (piton theater) maintaining the constraint''s suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(anti_dilution_clause_necessity, empirical, 'Whether anti-dilution clauses are necessary governance or primary extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(founder_equity_dilution_dynamics, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(foun_tr_t0, founder_equity_dilution_dynamics, theater_ratio, 0, 0.38).
narrative_ontology:measurement(foun_tr_t3, founder_equity_dilution_dynamics, theater_ratio, 3, 0.44).
narrative_ontology:measurement(foun_tr_t6, founder_equity_dilution_dynamics, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(foun_be_t0, founder_equity_dilution_dynamics, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(foun_be_t3, founder_equity_dilution_dynamics, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(foun_be_t6, founder_equity_dilution_dynamics, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(founder_equity_dilution_dynamics, resource_allocation).
narrative_ontology:affects_constraint(founder_equity_dilution_dynamics, employee_equity_compensation).
narrative_ontology:affects_constraint(founder_equity_dilution_dynamics, founder_replacement_dynamics).
narrative_ontology:affects_constraint(founder_equity_dilution_dynamics, venture_fund_returns_asymmetry).

% DUAL FORMULATION NOTE:
% The founder equity dilution constraint is downstream of capital formation constraints but represents its own distinct structural claim. Upstream: founders need capital (capital scarcity). This story: dilution mechanics are used to allocate capital (resource allocation constraint). Downstream: these mechanics affect employee compensation structures and founder retention/replacement dynamics. Each story has different epsilon and different perspectives; all are linked by resource allocation coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
